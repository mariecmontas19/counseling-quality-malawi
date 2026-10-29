********************************************************************************
* results_summary_index.do
*
* Per-panel summary-index robustness check (Kling-Liebman-Katz 2007; Anderson
* 2008). For each of the four panels we standardize every outcome to mean 0 / SD
* 1 over its estimation sample, average the (sign-aligned) standardized outcomes
* into a single panel index, and run the SAME 2SLS as the main table on the
* index. Testing four indices instead of nineteen outcomes controls multiple
* testing and gains power by averaging out idiosyncratic noise.
*
* Sign alignment: within each panel the outcomes already point in a common
* direction (Panel A = more counseling continuity/dose; Panel B = more topics
* covered; Panel C = more FP coverage/knowledge; Panel D = more subsequent
* fertility events), so the equal-weighted mean of the standardized outcomes is
* internally consistent without flipping any component. We verify this with the
* within-panel correlation matrix printed below.
*
* Unit of analysis matches the main table: Panels A/B at the woman level (woman
* mean across attended sessions), Panels C/D at endline (year==2018).
*
* INPUT:  dta/merge_counselor_outcomes.dta
* OUTPUT: /tmp/summary_index_results.csv
********************************************************************************

clear all
set more off
capture log close
log using "log/results_summary_index.log", replace text

use "dta/merge_counselor_outcomes.dta", clear
keep if treatment == 1

* Endogenous binary + scheduling-residualized quality residual (woman level)
quietly summarize avg_quality_sattop if treatment == 1, detail
local p50 = r(p50)
capture drop high_quality_avg_sattop
gen high_quality_avg_sattop = (avg_quality_sattop > `p50') if !missing(avg_quality_sattop)

global covariates ///
    i.age_group_bl sex_age_bl total_alive_bl ever_use_bl ///
    edu_primary_bl work_bl religion_r_bl i.area_bl ethnicity_r_bl ///
    preg_2016 curr_use_bl counselLP_bl

local A_outs share_same_counselor total_sessions visit_rate_mo husband_present
local B_outs etp1_fp etp1_spacing etp1_preg etp1_partner etp1_childhealth etp1_breast
local C_outs admin_fp_any admin_inject_any n_admin_fp fp_inj_coverage_frac husband_knows_fp
local D_outs birth_within_24mo preg_within_12mo preg_within_24mo mistimed_preg_expost
local AB_outs `A_outs' `B_outs'
local CD_outs `C_outs' `D_outs'

* ---- Woman-level dataset: A/B = woman mean, C/D = endline (year==2018) value ----
foreach y of local AB_outs {
    bysort caseid: egen double w_`y' = mean(`y')
}
foreach y of local CD_outs {
    bysort caseid: egen double w_`y' = max(cond(year == 2018, `y', .))
}
foreach v in high_quality_avg_sattop cqt_resid_loo_sattop preg_2016 ///
    age_group_bl sex_age_bl total_alive_bl ever_use_bl edu_primary_bl work_bl ///
    religion_r_bl area_bl ethnicity_r_bl curr_use_bl counselLP_bl {
    bysort caseid: egen double w_`v' = max(`v')
}
bysort caseid: keep if _n == 1
keep caseid w_*
rename w_* *

gen double hq_X_preg  = high_quality_avg_sattop * preg_2016
gen double cqt_X_preg = cqt_resid_loo_sattop * preg_2016

* ---- Standardize each outcome (mean 0, SD 1) over its non-missing sample ----
foreach y in `A_outs' `B_outs' `C_outs' `D_outs' {
    egen double z_`y' = std(`y')
}

* ---- Build the four panel indices (equal-weighted mean of standardized outcomes) ----
egen double idx_A = rowmean(z_share_same_counselor z_total_sessions z_visit_rate_mo z_husband_present)
egen double idx_B = rowmean(z_etp1_fp z_etp1_spacing z_etp1_preg z_etp1_partner z_etp1_childhealth z_etp1_breast)
egen double idx_C = rowmean(z_admin_fp_any z_admin_inject_any z_n_admin_fp z_fp_inj_coverage_frac z_husband_knows_fp)
egen double idx_D = rowmean(z_birth_within_24mo z_preg_within_12mo z_preg_within_24mo z_mistimed_preg_expost)

* ---- Diagnostic: within-panel correlations (confirm co-directionality) ----
di _n "=== Within-panel correlations (sign-alignment check) ==="
di "Panel A:"
quietly correlate z_share_same_counselor z_total_sessions z_visit_rate_mo z_husband_present
matrix list r(C)
di "Panel C:"
quietly correlate z_admin_fp_any z_admin_inject_any z_n_admin_fp z_fp_inj_coverage_frac z_husband_knows_fp
matrix list r(C)

* ---- IV on each index: pooled + interacted (PP / Preg / diff) ----
postfile pf str8 panel double n ///
    double pool_b double pool_se double pool_p ///
    double pp_b double pp_se double pp_p ///
    double pr_b double pr_se double pr_p ///
    double pdiff ///
    using "/tmp/summary_index_results.dta", replace

foreach P in A B C D {
    quietly ivreg2 idx_`P' (high_quality_avg_sattop = cqt_resid_loo_sattop) $covariates, robust
    local nP  = e(N)
    local pb  = _b[high_quality_avg_sattop]
    local pse = _se[high_quality_avg_sattop]
    local pp  = 2*(1 - normal(abs(`pb'/`pse')))

    quietly ivreg2 idx_`P' ///
        (high_quality_avg_sattop hq_X_preg = cqt_resid_loo_sattop cqt_X_preg) $covariates, robust
    quietly lincom high_quality_avg_sattop
    local ppb = r(estimate)
    local ppse = r(se)
    local ppp = r(p)
    quietly lincom high_quality_avg_sattop + hq_X_preg
    local prb = r(estimate)
    local prse = r(se)
    local prp = r(p)
    quietly lincom hq_X_preg
    local pdiff = r(p)

    post pf ("`P'") (`nP') (`pb') (`pse') (`pp') ///
        (`ppb') (`ppse') (`ppp') (`prb') (`prse') (`prp') (`pdiff')

    di as text "Panel `P' (N=`nP'): pooled=" %6.3f `pb' " (p=" %5.3f `pp' ")" ///
        "  PP=" %6.3f `ppb' " (p=" %5.3f `ppp' ")" ///
        "  Preg=" %6.3f `prb' " (p=" %5.3f `prp' ")  p_diff=" %5.3f `pdiff'
}
postclose pf

preserve
use "/tmp/summary_index_results.dta", clear
export delimited using "/tmp/summary_index_results.csv", replace
list, noobs
restore

log close
