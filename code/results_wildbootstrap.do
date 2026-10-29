********************************************************************************
* results_wildbootstrap.do
*
* Wild cluster bootstrap (WCB) inference, clustering on the six counselors, as a
* small-G robustness complement to the woman-level robust SEs and the leave-one-
* counselor-out exercise. Uses boottest (Roodman et al.) with WEBB 6-point
* weights, which give up to 6^6 = 46,656 distinct bootstrap statistics at G=6
* (vs. only 2^6 = 64 for Rademacher), the recommended choice for very few
* clusters (Cameron-Gelbach-Miller 2008; MacKinnon-Webb 2017).
*
* For each of the 19 outcomes we report the WCB p-value for the pooled effect and
* for the postpartum / pregnant subgroup effects, plus the PP=Preg difference.
* Subgroup effects are recovered as DIRECT coefficients via a fully-interacted
* (no-main-effect) reparameterization, hq_pp = HQ*(1-preg) and hq_pr = HQ*preg
* (instruments cqt_pp, cqt_pr), so boottest tests a single coefficient and the
* point estimates equal the PP/Preg effects in Table 5 exactly.
*
* Unit of analysis matches the main table: Panels A/B at the woman level, Panels
* C/D at endline (year==2018). Point estimates are identical to Table 5; only the
* inference (the WCB p-value) differs.
*
* INPUT:  dta/merge_counselor_outcomes.dta
* OUTPUT: /tmp/wcb_results.csv
********************************************************************************

clear all
set more off
set seed 20260618
capture log close
log using "log/results_wildbootstrap.log", replace text

capture which boottest
if _rc ssc install boottest, replace

local REPS = 9999

use "dta/merge_counselor_outcomes.dta", clear
keep if treatment == 1

quietly summarize avg_quality_sattop if treatment == 1, detail
capture drop high_quality_avg_sattop
gen high_quality_avg_sattop = (avg_quality_sattop > r(p50)) if !missing(avg_quality_sattop)

* Reparameterized endogenous (PP/Preg as direct coefficients) + instruments
gen double hq_pp  = high_quality_avg_sattop * (1 - preg_2016)
gen double hq_pr  = high_quality_avg_sattop * preg_2016
gen double cqt_pp = cqt_resid_loo_sattop * (1 - preg_2016)
gen double cqt_pr = cqt_resid_loo_sattop * preg_2016

global cov i.age_group_bl sex_age_bl total_alive_bl ever_use_bl edu_primary_bl ///
    work_bl religion_r_bl i.area_bl ethnicity_r_bl preg_2016 curr_use_bl counselLP_bl

local A_outs share_same_counselor total_sessions visit_rate_mo husband_present
local B_outs etp1_fp etp1_spacing etp1_preg etp1_partner etp1_childhealth etp1_breast
local C_outs admin_fp_any admin_inject_any n_admin_fp fp_inj_coverage_frac husband_knows_fp
local D_outs birth_within_24mo preg_within_12mo preg_within_24mo mistimed_preg_expost

postfile pf str30 outcome str4 panel ///
    double pool_b double pool_pwcb ///
    double pp_b double pp_pwcb ///
    double pr_b double pr_pwcb ///
    double diff_pwcb ///
    using "/tmp/wcb_results.dta", replace

* ---------- program that runs the WCB for one outcome on the data in memory ----------
* (assumes the correct estimation sample is the data currently in memory)
capture program drop _wcbrow
program define _wcbrow
    args y panel reps
    * Pooled
    quietly ivreg2 `y' (high_quality_avg_sattop = cqt_resid_loo_sattop) $cov, cluster(first_counselor)
    local pool_b = _b[high_quality_avg_sattop]
    quietly boottest high_quality_avg_sattop, weighttype(webb) reps(`reps') ///
        cluster(first_counselor) bootcluster(first_counselor) nograph
    local pool_p = r(p)
    * Het (reparameterized: direct PP / Preg coefficients)
    quietly ivreg2 `y' (hq_pp hq_pr = cqt_pp cqt_pr) $cov, cluster(first_counselor)
    local pp_b = _b[hq_pp]
    local pr_b = _b[hq_pr]
    quietly boottest hq_pp, weighttype(webb) reps(`reps') ///
        cluster(first_counselor) bootcluster(first_counselor) nograph
    local pp_p = r(p)
    quietly boottest hq_pr, weighttype(webb) reps(`reps') ///
        cluster(first_counselor) bootcluster(first_counselor) nograph
    local pr_p = r(p)
    quietly boottest hq_pp=hq_pr, weighttype(webb) reps(`reps') ///
        cluster(first_counselor) bootcluster(first_counselor) nograph
    local diff_p = r(p)
    post pf ("`y'") ("`panel'") (`pool_b') (`pool_p') (`pp_b') (`pp_p') ///
        (`pr_b') (`pr_p') (`diff_p')
    di as text "  `y' (`panel'): pool b=" %6.3f `pool_b' " p=" %5.3f `pool_p' ///
        " | PP b=" %6.3f `pp_b' " p=" %5.3f `pp_p' ///
        " | Preg b=" %6.3f `pr_b' " p=" %5.3f `pr_p' " | diff p=" %5.3f `diff_p'
end

* ---------- Panels A & B: woman level (collapse to one obs per woman) ----------
preserve
collapse (mean) `A_outs' `B_outs' high_quality_avg_sattop ///
    cqt_resid_loo_sattop hq_pp hq_pr cqt_pp cqt_pr ///
    age_group_bl sex_age_bl total_alive_bl ever_use_bl edu_primary_bl work_bl ///
    religion_r_bl area_bl ethnicity_r_bl preg_2016 curr_use_bl counselLP_bl ///
    (first) first_counselor, by(caseid)
di _n "===== Panels A & B (woman level) ====="
foreach y of local A_outs {
    _wcbrow `y' A `REPS'
}
foreach y of local B_outs {
    _wcbrow `y' B `REPS'
}
restore

* ---------- Panels C & D: endline cross-section (year==2018) ----------
preserve
keep if year == 2018
di _n "===== Panels C & D (endline) ====="
foreach y of local C_outs {
    _wcbrow `y' C `REPS'
}
foreach y of local D_outs {
    _wcbrow `y' D `REPS'
}
restore

postclose pf
preserve
use "/tmp/wcb_results.dta", clear
export delimited using "/tmp/wcb_results.csv", replace
list outcome panel pool_b pool_pwcb pp_b pp_pwcb pr_b pr_pwcb diff_pwcb, sepby(panel) noobs
restore
log close
