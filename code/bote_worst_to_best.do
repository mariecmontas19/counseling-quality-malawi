********************************************************************************
* bote_worst_to_best.do
*
* Back-of-the-envelope for the introduction (committee request): how much does
* family-planning uptake change moving from the lowest- to the highest-quality
* counselor? We use the REDUCED FORM of the headline FP-coverage outcome on the
* CQT instrument, scaled to the observed worst-to-best counselor range.
*
* worst->best effect = (reduced-form coefficient per unit of CQT)
*                      x (max - min of the counselor-level mean CQT)
*
* Verified result (year==2018, treatment==1, covariate-adjusted, robust):
*   CQT counselor-level range = 2.12
*   Any FP service:        per-unit b=0.0456 (p=0.038)  -> worst->best = +9.7 pp
*   Any injectable:        per-unit b=0.0447 (p=0.019)  -> worst->best = +9.5 pp
*   Injectable coverage:   per-unit b=0.0282 (p=0.006)  -> worst->best = +6.0 pp
*   Any-FP sample mean = 0.230, so +9.7 pp is a ~42% relative increase.
* The +9.7 pp / 23% figures are the ones cited in the introduction.
********************************************************************************
clear all
set more off
if "$MFPS_ROOT" == "" global MFPS_ROOT "`c(pwd)'"  // project folder (holds Analysis/)
if inlist("`c(os)'", "MacOSX", "Unix") {
    global base "${MFPS_ROOT}/Analysis"
}
cd "${base}"
cap log close
log using "log/bote_worst_to_best.log", replace text

use "dta/merge_counselor_outcomes.dta", clear
global covariates ///
    i.age_group_bl sex_age_bl total_alive_bl ever_use_bl ///
    edu_primary_bl work_bl religion_r_bl i.area_bl ethnicity_r_bl ///
    preg_2016 curr_use_bl counselLP_bl

* ---- counselor-level mean CQT, worst-to-best range ----
preserve
keep if year==2018 & treatment==1 & !missing(cqt_resid_loo_sattop)
collapse (mean) cqt_c=cqt_resid_loo_sattop, by(first_counselor)
quietly summarize cqt_c
local cqtrange = r(max) - r(min)
restore
di _n "CQT counselor-level range (worst->best) = " %6.3f `cqtrange'

* ---- reduced form per unit of CQT, scaled to the worst-to-best range ----
foreach y in admin_fp_any admin_inject_any fp_inj_coverage_frac {
    quietly summarize `y' if year==2018 & treatment==1, meanonly
    local mean = r(mean)
    quietly reg `y' cqt_resid_loo_sattop $covariates if year==2018 & treatment==1, robust
    local b  = _b[cqt_resid_loo_sattop]
    local p  = 2*ttail(e(df_r), abs(_b[cqt_resid_loo_sattop]/_se[cqt_resid_loo_sattop]))
    local wb = `b'*`cqtrange'
    di "`y': per-unit b=" %6.4f `b' " (p=" %5.3f `p' "); worst->best=" %5.3f `wb' ///
       " (= " %4.1f 100*`wb' " pp); mean=" %5.3f `mean' "; relative=" %4.1f 100*`wb'/`mean' "%"
}
log close
