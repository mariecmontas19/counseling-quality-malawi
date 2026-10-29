********************************************************************************
* results_takeup_balance.do
*
* BALANCE TEST for the JHE submission appendix: does the CQT instrument predict
* take-up of the bundled ancillary components (transport vouchers / clinic
* reimbursement)? If the CQT does not strongly predict take-up, the exclusion
* restriction holds without conditioning on these post-treatment ("bad control")
* variables; the companion Appendix tables (iv_robust_takeup_*) confirm the IV
* estimates are robust to controlling for take-up anyway.
*
* Spec (matches results_main_iv.do, May 19 2026 corrected endog):
*   - Instrument tested: cqt_resid_loo_sattop  (CURRENT primary, sat+topics)
*     scaled by its range -> cqt_scaled_sattop (units interpretable; the t/p is
*     scale-invariant, so significance is unaffected by the scaling).
*   - DVs: received_transport, received_reimbursement, received_any_ancillary
*   - Covariates: $covariates (same baseline set as the main IV)
*   - SE: vce(hc3); Sample: year==2018 & treatment==1
*
* OUTPUT: /tmp/cqt_takeup_balance.csv   (outcome,beta,se,p,n,meandv)
********************************************************************************

clear all
set more off
capture log close
capture file close fh

if "$MFPS_ROOT" == "" global MFPS_ROOT "`c(pwd)'"  // project folder (holds Analysis/)
cd "${MFPS_ROOT}/Analysis"

log using "log/results_takeup_balance.log", replace text

global covariates ///
    i.age_group_bl sex_age_bl total_alive_bl ever_use_bl ///
    edu_primary_bl work_bl religion_r_bl i.area_bl ethnicity_r_bl ///
    preg_2016 curr_use_bl counselLP_bl

use "dta/merge_counselor_outcomes.dta", clear

* Confirm the take-up indicators are present
foreach v in received_transport received_reimbursement received_any_ancillary {
    capture confirm variable `v'
    if _rc {
        di as error "`v' not found. Run build_takeup_indicators.do (called from cleaning_counseling)."
        exit 111
    }
}

* CQT (current primary sat+topics instrument), range-normalized
quietly summarize cqt_resid_loo_sattop if year == 2018 & treatment == 1, detail
local cqt_range = r(max) - r(min)
capture drop cqt_scaled_sattop
gen double cqt_scaled_sattop = cqt_resid_loo_sattop / `cqt_range'
label var cqt_scaled_sattop "CQT (sat+topics, range-normalized)"

di _n as text "============================================================"
di as text "  BALANCE TEST: does the CQT (sat+topics) predict ancillary take-up?"
di as text "  Instrument: cqt_resid_loo_sattop (scaled). SE: HC3. year==2018 & treatment==1"
di as text "============================================================"

file open fh using "/tmp/cqt_takeup_balance.csv", write replace
file write fh "outcome,beta,se,p,n,meandv"
foreach v in received_transport received_reimbursement received_any_ancillary {
    di _n as text "--- `v' regressed on CQT (sat+topics, scaled) + baseline covariates ---"
    quietly reg `v' cqt_scaled_sattop $covariates if year == 2018 & treatment == 1, vce(hc3)
    test cqt_scaled_sattop
    local b   = _b[cqt_scaled_sattop]
    local se  = _se[cqt_scaled_sattop]
    local p   = r(p)
    local n   = e(N)
    quietly summarize `v' if e(sample)
    local mdv = r(mean)
    di "  CQT_scaled coef = " %7.4f `b' "  (SE " %6.4f `se' "), p = " %6.4f `p' ///
       ", N = " `n' ", mean DV = " %6.4f `mdv'
    file write fh _n `"`v',`b',`se',`p',`n',`mdv'"'
}
file close fh
di _n as text "Saved: /tmp/cqt_takeup_balance.csv"

log close
