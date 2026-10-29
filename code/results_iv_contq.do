* ===========================================================================
* results_iv_contq.do
* Appendix robustness (August 2026, coauthor meeting): the main IV panels
* re-estimated with the CONTINUOUS first-session quality index
* (quality_index_first_sattop) as the endogenous variable, instead of the
* binary above-median cumulative-average indicator. Instrument, covariates,
* samples, and the interacted PP/Preg system are identical to
* results_main_iv.do. Coefficients read per unit of the first-session index
* (SD ~ 0.87).
*
* Output: /tmp/iv_contq.csv
* ===========================================================================

capture log close
log using "log/results_iv_contq.log", replace text

use "dta/merge_counselor_outcomes.dta", clear

global covariates ///
    i.age_group_bl sex_age_bl total_alive_bl ever_use_bl ///
    edu_primary_bl work_bl religion_r_bl i.area_bl ethnicity_r_bl ///
    preg_2016 curr_use_bl counselLP_bl

capture drop q1_X_preg cqt_X_preg
gen q1_X_preg  = quality_index_first_sattop * preg_2016
gen cqt_X_preg = cqt_resid_loo_sattop * preg_2016

local A_outs share_same_counselor total_sessions visit_rate_mo husband_present
local B_outs etp1_fp etp1_spacing etp1_preg etp1_partner etp1_childhealth etp1_breast
local C_outs admin_fp_any admin_inject_any n_admin_fp fp_inj_coverage_frac husband_knows_fp
local D_outs birth_within_24mo preg_within_12mo preg_within_24mo mistimed_preg_expost

postfile pf str25 outcome str8 panel ///
    double pool_b double pool_se double pool_p ///
    double pp_b double pp_se double pp_p ///
    double pr_b double pr_se double pr_p ///
    double pdiff double n double mn ///
    using /tmp/iv_contq.dta, replace

capture program drop run_contq
program define run_contq
    args y panel

    quietly summarize `y', meanonly
    local mn = r(mean)

    quietly capture noisily ivreg2 `y' ///
        (quality_index_first_sattop = cqt_resid_loo_sattop) $covariates, robust
    if _rc != 0 exit
    local pb  = _b[quality_index_first_sattop]
    local pse = _se[quality_index_first_sattop]
    local pp_ = 2*(1 - normal(abs(`pb'/`pse')))
    local pn  = e(N)

    quietly capture noisily ivreg2 `y' ///
        (quality_index_first_sattop q1_X_preg = cqt_resid_loo_sattop cqt_X_preg) ///
        $covariates, robust
    if _rc != 0 exit
    quietly lincom quality_index_first_sattop
    local pp_b  = r(estimate)
    local pp_se = r(se)
    local pp_p  = r(p)
    quietly lincom quality_index_first_sattop + q1_X_preg
    local pr_b  = r(estimate)
    local pr_se = r(se)
    local pr_p  = r(p)
    quietly lincom q1_X_preg
    local d_p = r(p)

    post pf ("`y'") ("`panel'") (`pb') (`pse') (`pp_') ///
        (`pp_b') (`pp_se') (`pp_p') (`pr_b') (`pr_se') (`pr_p') ///
        (`d_p') (`pn') (`mn')
end

* ---- Panels A & B: woman level ----
preserve
keep if treatment == 1
collapse (mean) `A_outs' `B_outs' ///
    quality_index_first_sattop cqt_resid_loo_sattop q1_X_preg cqt_X_preg ///
    age_group_bl sex_age_bl total_alive_bl ever_use_bl edu_primary_bl work_bl ///
    religion_r_bl area_bl ethnicity_r_bl preg_2016 curr_use_bl counselLP_bl, by(caseid)
foreach y of local A_outs {
    run_contq `y' A
}
foreach y of local B_outs {
    run_contq `y' B
}
restore

* ---- Panels C & D: endline ----
preserve
keep if treatment == 1 & year == 2018
foreach y of local C_outs {
    run_contq `y' C
}
foreach y of local D_outs {
    run_contq `y' D
}
restore

postclose pf

preserve
use /tmp/iv_contq.dta, clear
export delimited /tmp/iv_contq.csv, replace
restore

log close
