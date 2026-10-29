********************************************************************************
* results_robust_takeup.do
*
* Take-up controls robustness for the JHE submission appendix.
* Re-estimates every main IV coefficient (pooled, postpartum, pregnant) after
* adding `received_any_ancillary` as an additional covariate.  Addresses the
* "is the identifying variation really counseling, or is it transport
* vouchers / clinic reimbursement bundled with the intervention?" concern.
*
* Spec follows results_main_iv.do (May 19, 2026 corrected endog):
*   - Endogenous:   high_quality_avg_sattop  (woman-level mean sat+topics, above median)
*   - Instrument:   cqt_resid_loo_sattop
*   - Het:          $covariates includes preg_2016 (May 19 fix)
*   - Extra ctrl:   received_any_ancillary (transport OR reimbursement)
*   - Outcomes:     same 19-outcome 4-panel set as main IV
*
* OUTPUT:  /tmp/robust_takeup_results.csv
********************************************************************************

clear all
set more off
capture log close
capture file close f

if "$MFPS_ROOT" == "" global MFPS_ROOT "`c(pwd)'"  // project folder (holds Analysis/)
cd "${MFPS_ROOT}/Analysis"

log using "log/results_robust_takeup.log", replace text

use "dta/merge_counselor_outcomes.dta", clear

* Rebuild binary endog (per main script)
quietly summarize avg_quality_sattop if treatment == 1, detail
local p50 = r(p50)
capture drop high_quality_avg_sattop
gen high_quality_avg_sattop = (avg_quality_sattop > `p50') if !missing(avg_quality_sattop) & treatment == 1

global covariates ///
    i.age_group_bl sex_age_bl total_alive_bl ever_use_bl ///
    edu_primary_bl work_bl religion_r_bl i.area_bl ethnicity_r_bl ///
    preg_2016 curr_use_bl counselLP_bl

* Interaction terms for het spec
capture drop hq_X_preg cqt_X_preg
gen hq_X_preg = high_quality_avg_sattop * preg_2016
gen cqt_X_preg = cqt_resid_loo_sattop * preg_2016

* Verify required take-up indicator exists
capture confirm variable received_any_ancillary
if _rc != 0 {
    di as error "received_any_ancillary not found. Run build_takeup_indicators.do first."
    exit 198
}

local A_outs share_same_counselor total_sessions visit_rate_mo husband_present
local B_outs etp1_fp etp1_spacing etp1_preg etp1_partner etp1_childhealth etp1_breast
local C_outs admin_fp_any admin_inject_any n_admin_fp fp_inj_coverage_frac husband_knows_fp
local D_outs birth_within_24mo preg_within_12mo preg_within_24mo mistimed_preg_expost

postfile pf str25 outcome str8 panel ///
    double pool_b double pool_se double pool_p ///
    double pp_b double pp_se double pp_p ///
    double pr_b double pr_se double pr_p ///
    double pdiff double n double mn ///
    using /tmp/robust_takeup_results.dta, replace

* Panels A/B (continuity + topics): woman-level (one obs per woman), matching
* the main spec; zero within-woman variation across waves.
preserve
keep if treatment == 1
collapse (mean) `A_outs' `B_outs' ///
    high_quality_avg_sattop cqt_resid_loo_sattop hq_X_preg cqt_X_preg received_any_ancillary ///
    age_group_bl sex_age_bl total_alive_bl ever_use_bl edu_primary_bl work_bl ///
    religion_r_bl area_bl ethnicity_r_bl preg_2016 curr_use_bl counselLP_bl, by(caseid)
foreach panel in A B {
    local outs ``panel'_outs'
    foreach y of local outs {
        quietly summarize `y', meanonly
        local mn = r(mean)

        quietly capture noisily ivreg2 `y' (high_quality_avg_sattop = cqt_resid_loo_sattop) ///
            $covariates received_any_ancillary, robust
        local pb  = _b[high_quality_avg_sattop]
        local pse = _se[high_quality_avg_sattop]
        local pp  = 2*(1 - normal(abs(`pb'/`pse')))
        local pn  = e(N)

        quietly capture noisily ivreg2 `y' (high_quality_avg_sattop hq_X_preg = cqt_resid_loo_sattop cqt_X_preg) ///
            $covariates received_any_ancillary, robust
        quietly lincom high_quality_avg_sattop
        local pp_b  = r(estimate)
        local pp_se = r(se)
        local pp_p  = r(p)
        quietly lincom high_quality_avg_sattop + hq_X_preg
        local pr_b  = r(estimate)
        local pr_se = r(se)
        local pr_p  = r(p)
        quietly lincom hq_X_preg
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

        quietly capture noisily ivreg2 `y' (high_quality_avg_sattop = cqt_resid_loo_sattop) ///
            $covariates received_any_ancillary ///
            if year == 2018 & treatment == 1, robust
        local pb  = _b[high_quality_avg_sattop]
        local pse = _se[high_quality_avg_sattop]
        local pp  = 2*(1 - normal(abs(`pb'/`pse')))
        local pn  = e(N)

        quietly capture noisily ivreg2 `y' (high_quality_avg_sattop hq_X_preg = cqt_resid_loo_sattop cqt_X_preg) ///
            $covariates received_any_ancillary ///
            if year == 2018 & treatment == 1, robust
        quietly lincom high_quality_avg_sattop
        local pp_b  = r(estimate)
        local pp_se = r(se)
        local pp_p  = r(p)
        quietly lincom high_quality_avg_sattop + hq_X_preg
        local pr_b  = r(estimate)
        local pr_se = r(se)
        local pr_p  = r(p)
        quietly lincom hq_X_preg
        local d_p = r(p)

        post pf ("`y'") ("`panel'") (`pb') (`pse') (`pp') (`pp_b') (`pp_se') (`pp_p') (`pr_b') (`pr_se') (`pr_p') (`d_p') (`pn') (`mn')
    }
}
postclose pf

preserve
use /tmp/robust_takeup_results.dta, clear
export delimited /tmp/robust_takeup_results.csv, replace
restore

di as text _n "Saved /tmp/robust_takeup_results.csv"
log close
