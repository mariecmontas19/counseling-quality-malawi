capture file close f
log using /tmp/phase5_v2.log, replace text
use "dta/merge_counselor_outcomes.dta", clear

quietly summarize avg_quality_sattop if treatment == 1, detail
local p50 = r(p50)
capture drop high_quality_avg_sattop
gen high_quality_avg_sattop = (avg_quality_sattop > `p50') if !missing(avg_quality_sattop) & treatment == 1

global covariates ///
    i.age_group_bl sex_age_bl total_alive_bl ever_use_bl ///
    edu_primary_bl work_bl religion_r_bl i.area_bl ethnicity_r_bl ///
    preg_2016 curr_use_bl counselLP_bl
global covariates_no_preg ///
    i.age_group_bl sex_age_bl total_alive_bl ever_use_bl ///
    edu_primary_bl work_bl religion_r_bl i.area_bl ethnicity_r_bl ///
    curr_use_bl counselLP_bl

capture drop hq_X_preg cqt_X_preg
gen hq_X_preg = high_quality_avg_sattop * preg_2016
gen cqt_X_preg = cqt_resid_loo_sattop * preg_2016

local A_outs share_same_counselor total_sessions visit_rate_mo husband_present
local B_outs etp1_fp etp1_spacing etp1_preg etp1_partner etp1_childhealth etp1_breast
local C_outs admin_fp_any admin_inject_any n_admin_fp fp_inj_coverage_frac husband_knows_fp
local D_outs birth_within_24mo preg_within_12mo preg_within_24mo mistimed_preg_expost

* Timing-interval outcomes (for the new interval-hazard table)
local T_outs preg_0_6mo preg_6_12mo preg_12_18mo preg_18_24mo

postfile pf str25 outcome str8 panel ///
    double pool_b double pool_se double pool_p ///
    double pp_b double pp_se double pp_p ///
    double pr_b double pr_se double pr_p ///
    double pdiff double n double mn ///
    using /tmp/phase5_v2_results.dta, replace

* ===========================================================================
* Panels A and B (counseling continuity + topics): WOMAN-LEVEL.
* These outcomes and the endogenous/instrument are woman-level constants with
* ZERO within-woman variation across the panel waves, so the person-year panel
* is pure replication. We collapse to one observation per woman so the unit of
* analysis matches the woman-level IV and Panels C/D, and so heteroskedasticity-
* robust SEs are not understated by treating replicated woman-records as
* independent (referee comment 5, May 2026). Point estimates are essentially
* unchanged; SEs widen by ~the sqrt of the average waves-per-woman.
* ===========================================================================
preserve
keep if treatment == 1
collapse (mean) `A_outs' `B_outs' ///
    high_quality_avg_sattop cqt_resid_loo_sattop hq_X_preg cqt_X_preg ///
    age_group_bl sex_age_bl total_alive_bl ever_use_bl edu_primary_bl work_bl ///
    religion_r_bl area_bl ethnicity_r_bl preg_2016 curr_use_bl counselLP_bl, by(caseid)

foreach panel in A B {
    local outs ``panel'_outs'
    foreach y of local outs {
        quietly summarize `y', meanonly
        local mn = r(mean)

        quietly capture noisily ivreg2 `y' (high_quality_avg_sattop = cqt_resid_loo_sattop) ///
            $covariates, robust
        local pb = _b[high_quality_avg_sattop]
        local pse = _se[high_quality_avg_sattop]
        local pp = 2*(1 - normal(abs(`pb'/`pse')))
        local pn = e(N)

        quietly capture noisily ivreg2 `y' (high_quality_avg_sattop hq_X_preg = cqt_resid_loo_sattop cqt_X_preg) ///
            $covariates, robust
        quietly lincom high_quality_avg_sattop
        local pp_b = r(estimate)
        local pp_se = r(se)
        local pp_p = r(p)
        quietly lincom high_quality_avg_sattop + hq_X_preg
        local pr_b = r(estimate)
        local pr_se = r(se)
        local pr_p = r(p)
        quietly lincom hq_X_preg
        local d_p = r(p)

        post pf ("`y'") ("`panel'") (`pb') (`pse') (`pp') (`pp_b') (`pp_se') (`pp_p') (`pr_b') (`pr_se') (`pr_p') (`d_p') (`pn') (`mn')
    }
}
restore

* ===========================================================================
* Panels C and D (FP coverage + reproductive): endline year==2018, already one
* observation per woman.
* ===========================================================================
foreach panel in C D {
    local outs ``panel'_outs'
    foreach y of local outs {
        quietly summarize `y' if year == 2018 & treatment == 1, meanonly
        local mn = r(mean)

        quietly capture noisily ivreg2 `y' (high_quality_avg_sattop = cqt_resid_loo_sattop) ///
            $covariates if year == 2018 & treatment == 1, robust
        local pb = _b[high_quality_avg_sattop]
        local pse = _se[high_quality_avg_sattop]
        local pp = 2*(1 - normal(abs(`pb'/`pse')))
        local pn = e(N)

        quietly capture noisily ivreg2 `y' (high_quality_avg_sattop hq_X_preg = cqt_resid_loo_sattop cqt_X_preg) ///
            $covariates if year == 2018 & treatment == 1, robust
        quietly lincom high_quality_avg_sattop
        local pp_b = r(estimate)
        local pp_se = r(se)
        local pp_p = r(p)
        quietly lincom high_quality_avg_sattop + hq_X_preg
        local pr_b = r(estimate)
        local pr_se = r(se)
        local pr_p = r(p)
        quietly lincom hq_X_preg
        local d_p = r(p)

        post pf ("`y'") ("`panel'") (`pb') (`pse') (`pp') (`pp_b') (`pp_se') (`pp_p') (`pr_b') (`pr_se') (`pr_p') (`d_p') (`pn') (`mn')
    }
}
postclose pf

preserve
use /tmp/phase5_v2_results.dta, clear
export delimited /tmp/phase5_v2_results.csv, replace
restore

* ---------------------------------------------------------------------------
* Interval-hazard IV for Table 4 / timing figure.
* For each non-overlapping interval (0-6, 6-12, 12-18, 18-24 months), run the
* same pooled and het-by-preg IV as in Panel D. Save to /tmp/timing_intervals_results.csv.
* ---------------------------------------------------------------------------

postfile pf_t str20 outcome str10 interval ///
    double pool_b double pool_se double pool_p ///
    double pp_b double pp_se double pp_p ///
    double pr_b double pr_se double pr_p ///
    double pdiff double n double mn ///
    using /tmp/timing_intervals_results.dta, replace

foreach y of local T_outs {
    local intv = subinstr("`y'", "preg_", "", 1)

    quietly summarize `y' if year == 2018 & treatment == 1, meanonly
    local mn = r(mean)

    quietly capture noisily ivreg2 `y' (high_quality_avg_sattop = cqt_resid_loo_sattop) ///
        $covariates if year == 2018 & treatment == 1, robust
    local pb = _b[high_quality_avg_sattop]
    local pse = _se[high_quality_avg_sattop]
    local pp = 2*(1 - normal(abs(`pb'/`pse')))
    local pn = e(N)

    quietly capture noisily ivreg2 `y' (high_quality_avg_sattop hq_X_preg = cqt_resid_loo_sattop cqt_X_preg) ///
        $covariates if year == 2018 & treatment == 1, robust
    quietly lincom high_quality_avg_sattop
    local pp_b = r(estimate)
    local pp_se = r(se)
    local pp_p = r(p)
    quietly lincom high_quality_avg_sattop + hq_X_preg
    local pr_b = r(estimate)
    local pr_se = r(se)
    local pr_p = r(p)
    quietly lincom hq_X_preg
    local d_p = r(p)

    post pf_t ("`y'") ("`intv'") (`pb') (`pse') (`pp') (`pp_b') (`pp_se') (`pp_p') (`pr_b') (`pr_se') (`pr_p') (`d_p') (`pn') (`mn')
}
postclose pf_t

preserve
use /tmp/timing_intervals_results.dta, clear
export delimited /tmp/timing_intervals_results.csv, replace
restore

log close
