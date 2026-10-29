* ===========================================================================
* results_itt_rf.do
* ITT / reduced-form results table (August 2026 restructure: ITT shown FIRST
* in the Results section, per coauthor meeting 8/2026).
*
* For each of the 19 outcomes:
*   (1) Reduced form of Y on the LOO CQT instrument, SCALED to the observed
*       worst-to-best counselor range (so the coefficient reads as the effect
*       of moving from the lowest- to the highest-CQT counselor, matching the
*       intro back-of-envelope in bote_worst_to_best.do): pooled and
*       PP/Preg via the interacted reparameterization (direct subgroup
*       coefficients), heteroskedasticity-robust SEs.
*   (2) Wild-cluster bootstrap p-values (boottest, Webb weights, 9,999 reps,
*       clustering on the 6 counselors) for the same three coefficients and
*       the PP=Preg difference -- feeds the appendix WCB twin of the ITT
*       table. NOTE: the counselor-dummy joint F CANNOT be wild-cluster
*       bootstrapped (the dummies are the cluster identities), so it is
*       reported with conventional robust inference only.
*   (3) Joint F of the five counselor dummies (pure ITT of assignment),
*       robust, as in results_overid.do.
*
* Samples match the main IV table: A/B woman-level; C/D endline.
* Output: /tmp/itt_rf.csv
* ===========================================================================

capture log close
log using "log/results_itt_rf.log", replace text

set seed 20260805

capture which boottest
if _rc ssc install boottest, replace

use "dta/merge_counselor_outcomes.dta", clear

global covariates ///
    i.age_group_bl sex_age_bl total_alive_bl ever_use_bl ///
    edu_primary_bl work_bl religion_r_bl i.area_bl ethnicity_r_bl ///
    preg_2016 curr_use_bl counselLP_bl
global covariates_no_preg ///
    i.age_group_bl sex_age_bl total_alive_bl ever_use_bl ///
    edu_primary_bl work_bl religion_r_bl i.area_bl ethnicity_r_bl ///
    curr_use_bl counselLP_bl

* ---- worst-to-best counselor CQT range (as in bote_worst_to_best.do) ----
preserve
keep if treatment == 1 & !missing(first_counselor)
bysort caseid: keep if _n == 1
collapse (mean) cqt_c = cqt_resid_loo_sattop, by(first_counselor)
quietly summarize cqt_c
local cqtrange = r(max) - r(min)
restore
di _n as text "CQT counselor-level worst-to-best range = " %6.3f `cqtrange'

* ---- scaled instrument + interactions + counselor dummies ----
capture drop cqt_sc cqt_sc_pp cqt_sc_pr
gen cqt_sc    = cqt_resid_loo_sattop / `cqtrange'
gen cqt_sc_pp = cqt_sc * (1 - preg_2016)
gen cqt_sc_pr = cqt_sc * preg_2016
forvalues k = 1/6 {
    capture drop cd`k'
    gen cd`k' = (first_counselor == `k') if !missing(first_counselor)
}

local A_outs share_same_counselor total_sessions visit_rate_mo husband_present
local B_outs etp1_fp etp1_spacing etp1_preg etp1_partner etp1_childhealth etp1_breast
local C_outs admin_fp_any admin_inject_any n_admin_fp fp_inj_coverage_frac husband_knows_fp
local D_outs birth_within_24mo preg_within_12mo preg_within_24mo mistimed_preg_expost

postfile pf str25 outcome str8 panel ///
    double pool_b double pool_se double pool_p double pool_pwcb ///
    double pp_b double pp_se double pp_p double pp_pwcb ///
    double pr_b double pr_se double pr_p double pr_pwcb ///
    double pdiff double pdiff_wcb ///
    double dumF double dumFp double n double mn ///
    using /tmp/itt_rf.dta, replace

capture program drop run_itt
program define run_itt
    args y panel

    quietly summarize `y', meanonly
    local mn = r(mean)

    * --- (1) pooled RF on scaled CQT, robust ---
    quietly capture noisily reg `y' cqt_sc $covariates, robust
    if _rc != 0 exit
    local pb  = _b[cqt_sc]
    local pse = _se[cqt_sc]
    local pp_ = 2*(1 - normal(abs(`pb'/`pse')))
    local pn  = e(N)
    * WCB p for the pooled coefficient
    quietly boottest cqt_sc, weighttype(webb) reps(9999) ///
        cluster(first_counselor) bootcluster(first_counselor) nograph
    local pwcb = r(p)

    * --- (1b) subgroup RF (direct PP / Preg coefficients), robust ---
    quietly reg `y' cqt_sc_pp cqt_sc_pr preg_2016 $covariates_no_preg, robust
    local ppb  = _b[cqt_sc_pp]
    local ppse = _se[cqt_sc_pp]
    local ppp  = 2*(1 - normal(abs(`ppb'/`ppse')))
    local prb  = _b[cqt_sc_pr]
    local prse = _se[cqt_sc_pr]
    local prp  = 2*(1 - normal(abs(`prb'/`prse')))
    quietly test cqt_sc_pp = cqt_sc_pr
    local pd = r(p)
    * WCB p-values for the subgroup coefficients and the difference
    quietly boottest cqt_sc_pp, weighttype(webb) reps(9999) ///
        cluster(first_counselor) bootcluster(first_counselor) nograph
    local ppwcb = r(p)
    quietly boottest cqt_sc_pr, weighttype(webb) reps(9999) ///
        cluster(first_counselor) bootcluster(first_counselor) nograph
    local prwcb = r(p)
    quietly boottest cqt_sc_pp = cqt_sc_pr, weighttype(webb) reps(9999) ///
        cluster(first_counselor) bootcluster(first_counselor) nograph
    local pdwcb = r(p)

    * --- (3) joint F of the five counselor dummies, robust ---
    quietly reg `y' cd2 cd3 cd4 cd5 cd6 $covariates, robust
    quietly testparm cd2 cd3 cd4 cd5 cd6
    local dF  = r(F)
    local dFp = r(p)

    post pf ("`y'") ("`panel'") ///
        (`pb') (`pse') (`pp_') (`pwcb') ///
        (`ppb') (`ppse') (`ppp') (`ppwcb') ///
        (`prb') (`prse') (`prp') (`prwcb') ///
        (`pd') (`pdwcb') (`dF') (`dFp') (`pn') (`mn')
    di as text "  `y' (`panel'): pooled b=" %6.3f `pb' " p=" %5.3f `pp_' ///
        " pWCB=" %5.3f `pwcb' " | dummies F=" %5.2f `dF' " p=" %5.3f `dFp'
end

* ---- Panels A & B: woman level ----
preserve
keep if treatment == 1
collapse (mean) `A_outs' `B_outs' ///
    cqt_sc cqt_sc_pp cqt_sc_pr first_counselor cd1 cd2 cd3 cd4 cd5 cd6 ///
    age_group_bl sex_age_bl total_alive_bl ever_use_bl edu_primary_bl work_bl ///
    religion_r_bl area_bl ethnicity_r_bl preg_2016 curr_use_bl counselLP_bl, by(caseid)
foreach y of local A_outs {
    run_itt `y' A
}
foreach y of local B_outs {
    run_itt `y' B
}
restore

* ---- Panels C & D: endline ----
preserve
keep if treatment == 1 & year == 2018
foreach y of local C_outs {
    run_itt `y' C
}
foreach y of local D_outs {
    run_itt `y' D
}
restore

postclose pf

preserve
use /tmp/itt_rf.dta, clear
export delimited /tmp/itt_rf.csv, replace
restore

log close
