********************************************************************************
* results_autonomy.do
*
* Test whether the husband_knows_fp PP/Preg flip extends to other autonomy
* outcomes (Section 5.3 of the JHE paper).
*
*   PP women: higher counseling quality -> MORE autonomy (less husband
*             involvement, more woman-only decisions)
*   Preg women: higher counseling quality -> MORE partner involvement
*             (counselor includes husband for post-delivery FP planning)
*
* Sample: year == 2018, treatment. Endogenous: high_quality_avg_sattop. IV:
* cqt_resid_loo_sattop. Outcomes: husband_knows_fp + Section 8 decision-
* making questions (Q823-Q826) + 3 other autonomy probes.
*
* Reads:  dta/merge_counselor_outcomes.dta
* Writes: /tmp/autonomy_iv_results.csv  -> consumed by build_latex_tables/build_autonomy.py
* Log:    log/results_autonomy.log
********************************************************************************

capture log close
log using log/results_autonomy.log, replace text

use "dta/merge_counselor_outcomes.dta", clear

* Build the sat+topics endogenous (same as main spec)
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
gen hq_X_preg  = high_quality_avg_sattop * preg_2016
gen cqt_X_preg = cqt_resid_loo_sattop * preg_2016

* Autonomy / household-power outcomes to test
local autonomy_outs ///
    husband_knows_fp ///
    discussed_fp_husband ///
    decision_contraception ///
    w8_w823_binary ///
    w8_w824_binary ///
    w8_w825_binary ///
    w8_w826_binary ///
    husband_wants_children ///
    same_number_children

di _n "=== Confirming variable existence and coverage ==="
foreach v of local autonomy_outs {
    capture confirm variable `v'
    if _rc == 0 {
        quietly count if year == 2018 & treatment == 1 & !missing(`v')
        quietly summarize `v' if year == 2018 & treatment == 1, meanonly
        di "  `v': N=" %4.0f r(N) "  mean=" %5.3f r(mean)
    }
    else di "  `v': NOT FOUND"
}

di _n "=== IV results: autonomy outcomes by pregnancy status ==="
di "  Format: Pooled β | PP β | Preg β | p(PP=Preg)"

postfile pf_aut str25 outcome ///
    double pool_b double pool_se double pool_p ///
    double pp_b double pp_se double pp_p ///
    double pr_b double pr_se double pr_p ///
    double pdiff double n double mn ///
    using /tmp/autonomy_iv_results.dta, replace

foreach y of local autonomy_outs {
    capture confirm variable `y'
    if _rc != 0 continue

    quietly summarize `y' if year == 2018 & treatment == 1, meanonly
    local mn = r(mean)
    if missing(`mn') continue

    * Pooled IV
    quietly capture noisily ivreg2 `y' (high_quality_avg_sattop = cqt_resid_loo_sattop) ///
        $covariates if year == 2018 & treatment == 1, robust
    if _rc != 0 continue
    local pb = _b[high_quality_avg_sattop]
    local pse = _se[high_quality_avg_sattop]
    local pp = 2*(1 - normal(abs(`pb'/`pse')))
    local pn = e(N)

    * Het-by-pregnancy IV
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

    post pf_aut ("`y'") (`pb') (`pse') (`pp') (`pp_b') (`pp_se') (`pp_p') (`pr_b') (`pr_se') (`pr_p') (`d_p') (`pn') (`mn')

    di "--- `y' (N=`pn', mean=`mn') ---"
    di "    Pool b=`pb'  se=`pse'  p=`pp'"
    di "    PP   b=`pp_b'  se=`pp_se'  p=`pp_p'"
    di "    Preg b=`pr_b'  se=`pr_se'  p=`pr_p'"
    di "    p(PP=Preg)=`d_p'"

    * Flag the flip pattern
    if (`pp_b' < 0 & `pr_b' > 0) | (`pp_b' > 0 & `pr_b' < 0) {
        di "    *** SIGN FLIP between PP and Preg ***"
    }
}
postclose pf_aut

preserve
use /tmp/autonomy_iv_results.dta, clear
export delimited /tmp/autonomy_iv_results.csv, replace
list outcome pool_b pp_b pr_b pdiff n mn, noobs
restore

log close
