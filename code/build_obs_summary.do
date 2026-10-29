********************************************************************************
* build_obs_summary.do
*
* Observation-count appendix table for the CURRENT analysis (replaces the old,
* outdated tab:obs_summary in "Longer paper with robustness/paper2_results.tex").
*
* For every outcome in the main RF/IV table (results_main_iv.do, Panels A-D):
*   eligible N  -- the regression universe before missingness
*                  (Panels A/B woman-level: treatment women, one row each;
*                   Panels C/D endline: treatment==1 & year==2018)
*   non-missing -- eligible obs with a valid outcome value
*   missing     -- eligible minus non-missing
*   cov/instr loss -- non-missing minus estimation N
*   estimation N -- e(N) from the ACTUAL pooled IV regression (the number the
*                   paper's tables use)
*   collected at BL? -- whether the variable has any value at the 2016 baseline
*
* Sample definitions and specification mirror results_main_iv.do exactly.
* OUTPUT: tables/obs_summary.csv
********************************************************************************
clear all
set more off
if "$MFPS_ROOT" == "" global MFPS_ROOT "`c(pwd)'"  // project folder (holds Analysis/)
if inlist("`c(os)'", "MacOSX", "Unix") {
    global base "${MFPS_ROOT}/Analysis"
}
cd "${base}"
cap log close
log using "log/build_obs_summary.log", replace text

use "dta/merge_counselor_outcomes.dta", clear

quietly summarize avg_quality_sattop if treatment == 1, detail
local p50 = r(p50)
capture drop high_quality_avg_sattop
gen high_quality_avg_sattop = (avg_quality_sattop > `p50') if !missing(avg_quality_sattop) & treatment == 1

global covariates ///
    i.age_group_bl sex_age_bl total_alive_bl ever_use_bl ///
    edu_primary_bl work_bl religion_r_bl i.area_bl ethnicity_r_bl ///
    preg_2016 curr_use_bl counselLP_bl

local A_outs share_same_counselor total_sessions visit_rate_mo husband_present
local B_outs etp1_fp etp1_spacing etp1_preg etp1_partner etp1_childhealth etp1_breast
local C_outs admin_fp_any admin_inject_any n_admin_fp fp_inj_coverage_frac husband_knows_fp
local D_outs birth_within_24mo preg_within_12mo preg_within_24mo mistimed_preg_expost

* baseline-collection flag (endline outcomes): any non-missing value at year==2016
* computed on the full panel BEFORE collapsing
foreach y of local C_outs {
    quietly count if year == 2016 & treatment == 1 & !missing(`y')
    local bl_`y' = cond(r(N) > 0, "Yes", "No")
}
foreach y of local D_outs {
    quietly count if year == 2016 & treatment == 1 & !missing(`y')
    local bl_`y' = cond(r(N) > 0, "Yes", "No")
}

file open f using "tables/obs_summary.csv", write replace
file write f "panel,rowkey,eligible,nonmiss,missing,covloss,estN,bl" _n

* ---- Panels A & B: WOMAN-LEVEL (collapse to one row per treatment woman) ----
preserve
keep if treatment == 1
collapse (mean) `A_outs' `B_outs' ///
    high_quality_avg_sattop cqt_resid_loo_sattop ///
    age_group_bl sex_age_bl total_alive_bl ever_use_bl edu_primary_bl work_bl ///
    religion_r_bl area_bl ethnicity_r_bl preg_2016 curr_use_bl counselLP_bl, by(caseid)
quietly count
local elig_wl = r(N)
foreach panel in A B {
    local outs ``panel'_outs'
    foreach y of local outs {
        quietly count if !missing(`y')
        local nonmiss = r(N)
        quietly capture noisily ivreg2 `y' (high_quality_avg_sattop = cqt_resid_loo_sattop) ///
            $covariates, robust
        local estN = e(N)
        local miss = `elig_wl' - `nonmiss'
        local covloss = `nonmiss' - `estN'
        file write f "`panel',`y',`elig_wl',`nonmiss',`miss',`covloss',`estN',NA" _n
    }
}
restore

* ---- Panels C & D: ENDLINE (treatment==1 & year==2018) ----
quietly count if year == 2018 & treatment == 1
local elig_end = r(N)
foreach panel in C D {
    local outs ``panel'_outs'
    foreach y of local outs {
        quietly count if year == 2018 & treatment == 1 & !missing(`y')
        local nonmiss = r(N)
        quietly capture noisily ivreg2 `y' (high_quality_avg_sattop = cqt_resid_loo_sattop) ///
            $covariates if year == 2018 & treatment == 1, robust
        local estN = e(N)
        local miss = `elig_end' - `nonmiss'
        local covloss = `nonmiss' - `estN'
        file write f "`panel',`y',`elig_end',`nonmiss',`miss',`covloss',`estN',`bl_`y''" _n
    }
}
file close f

di _n "==== eligible: woman-level=`elig_wl'  endline=`elig_end' ===="
type "tables/obs_summary.csv"
log close
