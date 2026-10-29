capture file close f
log using /tmp/phase7b_4comp.log, replace text
use "dta/merge_counselor_outcomes.dta", clear

* Verify the 4-comp variables exist
foreach v in avg_quality_4c high_quality_avg_all cqt_resid_loo {
    capture confirm variable `v'
    if _rc == 0 di "  OK: `v'"
    else di "  MISSING: `v'"
}

* If high_quality_avg_all doesn't exist, build it
capture confirm variable high_quality_avg_all
if _rc != 0 {
    quietly summarize avg_quality_4c if treatment == 1, detail
    local p50 = r(p50)
    capture drop high_quality_avg_all
    gen high_quality_avg_all = (avg_quality_4c > `p50') if !missing(avg_quality_4c) & treatment == 1
}

global covariates ///
    i.age_group_bl sex_age_bl total_alive_bl ever_use_bl ///
    edu_primary_bl work_bl religion_r_bl i.area_bl ethnicity_r_bl ///
    preg_2016 curr_use_bl counselLP_bl
global covariates_no_preg ///
    i.age_group_bl sex_age_bl total_alive_bl ever_use_bl ///
    edu_primary_bl work_bl religion_r_bl i.area_bl ethnicity_r_bl ///
    curr_use_bl counselLP_bl

capture drop hq_X_preg_4c cqt_X_preg_4c
gen hq_X_preg_4c = high_quality_avg_all * preg_2016
gen cqt_X_preg_4c = cqt_resid_loo * preg_2016

local A_outs share_same_counselor total_sessions visit_rate_mo husband_present
local B_outs etp1_fp etp1_spacing etp1_preg etp1_partner etp1_childhealth etp1_breast
local C_outs admin_fp_any admin_inject_any n_admin_fp fp_inj_coverage_frac husband_knows_fp
local D_outs birth_within_12mo birth_within_24mo preg_within_12mo preg_within_24mo mistimed_preg_expost

postfile pf str25 outcome str8 panel ///
    double pool_b double pool_se double pool_p ///
    double pp_b double pp_se double pp_p ///
    double pr_b double pr_se double pr_p ///
    double pdiff double n double mn ///
    using /tmp/phase7b_results.dta, replace

* Panels A/B (continuity + topics): woman-level (one obs per woman), matching
* the main spec; these outcomes have zero within-woman variation across waves.
preserve
keep if treatment == 1
collapse (mean) `A_outs' `B_outs' ///
    high_quality_avg_all cqt_resid_loo hq_X_preg_4c cqt_X_preg_4c ///
    age_group_bl sex_age_bl total_alive_bl ever_use_bl edu_primary_bl work_bl ///
    religion_r_bl area_bl ethnicity_r_bl preg_2016 curr_use_bl counselLP_bl, by(caseid)
foreach panel in A B {
    local outs ``panel'_outs'
    foreach y of local outs {
        quietly summarize `y', meanonly
        local mn = r(mean)

        quietly capture noisily ivreg2 `y' (high_quality_avg_all = cqt_resid_loo) ///
            $covariates, robust
        local pb = _b[high_quality_avg_all]
        local pse = _se[high_quality_avg_all]
        local pp = 2*(1 - normal(abs(`pb'/`pse')))
        local pn = e(N)

        quietly capture noisily ivreg2 `y' (high_quality_avg_all hq_X_preg_4c = cqt_resid_loo cqt_X_preg_4c) ///
            $covariates, robust
        quietly lincom high_quality_avg_all
        local pp_b = r(estimate)
        local pp_se = r(se)
        local pp_p = r(p)
        quietly lincom high_quality_avg_all + hq_X_preg_4c
        local pr_b = r(estimate)
        local pr_se = r(se)
        local pr_p = r(p)
        quietly lincom hq_X_preg_4c
        local d_p = r(p)

        post pf ("`y'") ("`panel'") (`pb') (`pse') (`pp') (`pp_b') (`pp_se') (`pp_p') (`pr_b') (`pr_se') (`pr_p') (`d_p') (`pn') (`mn')
    }
}
restore

* Panels C/D (FP coverage + reproductive): endline year==2018 (one per woman).
foreach panel in C D {
    local outs ``panel'_outs'
    foreach y of local outs {
        quietly summarize `y' if year == 2018 & treatment == 1, meanonly
        local mn = r(mean)

        quietly capture noisily ivreg2 `y' (high_quality_avg_all = cqt_resid_loo) ///
            $covariates if year == 2018 & treatment == 1, robust
        local pb = _b[high_quality_avg_all]
        local pse = _se[high_quality_avg_all]
        local pp = 2*(1 - normal(abs(`pb'/`pse')))
        local pn = e(N)

        quietly capture noisily ivreg2 `y' (high_quality_avg_all hq_X_preg_4c = cqt_resid_loo cqt_X_preg_4c) ///
            $covariates if year == 2018 & treatment == 1, robust
        quietly lincom high_quality_avg_all
        local pp_b = r(estimate)
        local pp_se = r(se)
        local pp_p = r(p)
        quietly lincom high_quality_avg_all + hq_X_preg_4c
        local pr_b = r(estimate)
        local pr_se = r(se)
        local pr_p = r(p)
        quietly lincom hq_X_preg_4c
        local d_p = r(p)

        post pf ("`y'") ("`panel'") (`pb') (`pse') (`pp') (`pp_b') (`pp_se') (`pp_p') (`pr_b') (`pr_se') (`pr_p') (`d_p') (`pn') (`mn')
    }
}
postclose pf

preserve
use /tmp/phase7b_results.dta, clear
export delimited /tmp/phase7b_results.csv, replace
restore
log close
