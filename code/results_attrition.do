********************************************************************************
* results_attrition.do
*
* PURPOSE: Compute attrition by counselor (baseline -> midline -> endline) under
*          the current sat+topics-engaged specification. Tests:
*            (i)  Differential attrition by counselor (F-test) BL -> ML
*            (ii) Differential attrition by counselor (F-test) BL -> EL
*            (iii) CQT -> endline attrition (controlled for covariates)
*            (iv) Covariate balance in endline sample (Fisher combined)
*
* INPUT:  dta/merge_counselor_outcomes.dta
* OUTPUT: /tmp/attrition_by_counselor.csv  (one row per counselor + Total row)
*         /tmp/attrition_tests.csv          (F-tests, CQT-attrition p-value)
*         log/attrition.log
*
* AUTHOR: Marie Montas
* DATE:   May 20, 2026
********************************************************************************

clear all
set more off

* --- Paths ---
if "$MFPS_ROOT" == "" global MFPS_ROOT "`c(pwd)'"  // project folder (holds Analysis/)
if inlist("`c(os)'", "MacOSX", "Unix") {
    global base    "${MFPS_ROOT}/Analysis"
    global dta     "${base}/dta/"
    global log     "${base}/log/"
}

cd "${base}"
capture log close
log using "${log}attrition.log", replace text

* ==============================================================================
* Load data and restrict to treatment-arm women with first-counselor assignment
* ==============================================================================
use "${dta}merge_counselor_outcomes.dta", clear

keep if treatment == 1 & !missing(first_counselor)

* Wave-presence indicators
bys caseid: egen any2016 = max(year == 2016)
bys caseid: egen any2017 = max(year == 2017)
bys caseid: egen any2018 = max(year == 2018)

* Collapse to one observation per woman
bys caseid: gen __tag = (_n == 1)
keep if __tag == 1
drop __tag

keep if any2016 == 1   // present at baseline (should be all 901)

* ==============================================================================
* Attrition indicators
* ==============================================================================
gen attrit_ml = (any2017 == 0)
gen attrit_el = (any2018 == 0)

label var attrit_ml "Attrited by midline (not present 2017)"
label var attrit_el "Attrited by endline (not present 2018)"

count
local N_total = r(N)
di as text _n "Universe: N = `N_total' treatment-arm women with first counselor"

* ==============================================================================
* F-tests for differential attrition by counselor
* ==============================================================================
di _n as text "============================================="
di as text "F-test: differential attrition BL -> ML"
di as text "============================================="
reg attrit_ml i.first_counselor, vce(hc3)
testparm i.first_counselor
local F_ml = r(F)
local p_ml = r(p)
local df1_ml = r(df)
local df2_ml = r(df_r)

di _n as text "============================================="
di as text "F-test: differential attrition BL -> EL"
di as text "============================================="
reg attrit_el i.first_counselor, vce(hc3)
testparm i.first_counselor
local F_el = r(F)
local p_el = r(p)
local df1_el = r(df)
local df2_el = r(df_r)

* ==============================================================================
* CQT -> endline attrition (controlled)
* ==============================================================================
global covariates "i.age_group_bl sex_age_bl total_alive_bl ever_use_bl edu_primary_bl work_bl religion_r_bl i.area_bl ethnicity_r_bl preg_2016 curr_use_bl counselLP_bl"

di _n as text "============================================="
di as text "CQT predicts endline attrition? (with covariates)"
di as text "============================================="
capture noisily reg attrit_el cqt_resid_loo_sattop ${covariates}, vce(hc3)
if _rc == 0 {
    test cqt_resid_loo_sattop
    local p_cqt_el = r(p)
    di as text "p-value (CQT on EL attrition, with covariates): `p_cqt_el'"
}
else {
    di as error "CQT regression failed; check that cqt_resid_loo_sattop exists in dataset"
    local p_cqt_el = .
}

* ==============================================================================
* By-counselor cell counts and attrition rates
* ==============================================================================
preserve

collapse (sum) N_baseline=any2016 N_midline=any2017 N_endline=any2018, by(first_counselor)

gen attrit_rate_ml = (N_baseline - N_midline) / N_baseline
gen attrit_rate_el = (N_baseline - N_endline) / N_baseline
gen retain_rate_ml = N_midline / N_baseline
gen retain_rate_el = N_endline / N_baseline

list, sep(0) noobs

* Add total row
tempfile bycoun
save `bycoun', replace
collapse (sum) N_baseline N_midline N_endline
gen first_counselor = 99   // sentinel "Total"
gen attrit_rate_ml = (N_baseline - N_midline) / N_baseline
gen attrit_rate_el = (N_baseline - N_endline) / N_baseline
gen retain_rate_ml = N_midline / N_baseline
gen retain_rate_el = N_endline / N_baseline
append using `bycoun'
sort first_counselor

export delimited using "/tmp/attrition_by_counselor.csv", replace
di as text "Saved: /tmp/attrition_by_counselor.csv"

list, sep(0) noobs

restore

* ==============================================================================
* Fisher combined test of covariate balance in the endline analytic sample
* ==============================================================================
* Reload + rebuild from scratch so we have all panel waves and covariates
preserve

use "${dta}merge_counselor_outcomes.dta", clear

* Restrict to treatment-arm women with a first-counselor assignment
keep if treatment == 1 & !missing(first_counselor)

* Endline subset (year=2018, with all baseline covariates non-missing)
keep if year == 2018

* Drop women missing any baseline covariate (i.e., the IV-analytic endline sample)
local covar_list "age_group_bl sex_age_bl total_alive_bl ever_use_bl edu_primary_bl work_bl religion_r_bl area_bl ethnicity_r_bl preg_2016 curr_use_bl counselLP_bl"
foreach v of local covar_list {
    drop if missing(`v')
}

count
local N_el_analytic = r(N)
di as text _n "Endline analytic sample (treatment + first_counselor + all 12 covariates non-missing): N = `N_el_analytic'"

* ------------------------------------------------------------------------------
* Joint covariate-balance F-tests (reported in Table 2 notes + Section 4.2 text):
* regress the instrument (CQT) and the cumulative quality measure on the full
* baseline control set; heteroskedasticity-robust joint F-test of the null that
* all covariate coefficients are zero. Common IV-analytic sample (N = 724).
* ------------------------------------------------------------------------------
global covbal "i.age_group_bl sex_age_bl total_alive_bl ever_use_bl edu_primary_bl work_bl religion_r_bl i.area_bl ethnicity_r_bl preg_2016 curr_use_bl counselLP_bl"
quietly regress cqt_resid_loo_sattop ${covbal} if !missing(avg_quality_sattop), vce(robust)
testparm i.age_group_bl sex_age_bl total_alive_bl ever_use_bl edu_primary_bl work_bl religion_r_bl i.area_bl ethnicity_r_bl preg_2016 curr_use_bl counselLP_bl
di as text _n "Joint covariate-balance F-tests (robust, IV-analytic sample):"
di as text "  CQT ~ covariates:         F(" r(df) "," r(df_r) ") = " %5.2f r(F) ", p = " %5.3f r(p) ", N = " e(N)
quietly regress avg_quality_sattop ${covbal} if !missing(cqt_resid_loo_sattop), vce(robust)
testparm i.age_group_bl sex_age_bl total_alive_bl ever_use_bl edu_primary_bl work_bl religion_r_bl i.area_bl ethnicity_r_bl preg_2016 curr_use_bl counselLP_bl
di as text "  cumulative quality ~ cov: F(" r(df) "," r(df_r) ") = " %5.2f r(F) ", p = " %5.3f r(p) ", N = " e(N)

* For each baseline covariate, regress on counselor dummies and extract p-value
* of the joint F-test of counselor dummies. Fisher-combine the p-values.
local sum_logp = 0
local k = 0
local pvals ""

foreach v of local covar_list {
    capture noisily {
        if "`v'" == "age_group_bl" | "`v'" == "area_bl" {
            reg `v' i.first_counselor, vce(hc3)
        }
        else {
            reg `v' i.first_counselor, vce(hc3)
        }
        testparm i.first_counselor
        local p_v = r(p)
        if !missing(`p_v') & `p_v' > 0 {
            local sum_logp = `sum_logp' + ln(`p_v')
            local k = `k' + 1
            local pvals "`pvals' `v'=`p_v'"
            di as text "  `v': F p-value = `p_v'"
        }
        else {
            di as text "  `v': p-value missing or zero, skipping"
        }
    }
}

local chi2_fisher = -2 * `sum_logp'
local df_fisher = 2 * `k'
local p_fisher = chi2tail(`df_fisher', `chi2_fisher')

di _n as text "============================================="
di as text "Fisher combined test (endline analytic sample):"
di as text "  Number of covariates: `k'"
di as text "  Chi-squared statistic: " %9.3f `chi2_fisher'
di as text "  Degrees of freedom:    `df_fisher'"
di as text "  Combined p-value:      " %9.4f `p_fisher'
di as text "  Per-covariate p-values:`pvals'"
di as text "============================================="

* Save to CSV
clear
set obs 1
gen str40 test = "Fisher combined balance (endline analytic)"
gen N_endline_analytic = `N_el_analytic'
gen n_covariates = `k'
gen chi2_fisher = `chi2_fisher'
gen df_fisher = `df_fisher'
gen p_fisher = `p_fisher'
export delimited using "/tmp/attrition_fisher.csv", replace
list, sep(0) noobs

restore

* ==============================================================================
* Export test statistics
* ==============================================================================
preserve
clear
set obs 3
gen str40 test = ""
gen double F = .
gen double p = .
gen double df1 = .
gen double df2 = .

replace test = "F-test diff attrition BL->ML"  in 1
replace F    = `F_ml'                          in 1
replace p    = `p_ml'                          in 1
replace df1  = `df1_ml'                        in 1
replace df2  = `df2_ml'                        in 1

replace test = "F-test diff attrition BL->EL"  in 2
replace F    = `F_el'                          in 2
replace p    = `p_el'                          in 2
replace df1  = `df1_el'                        in 2
replace df2  = `df2_el'                        in 2

replace test = "CQT->EL attrition (w/ cov)"    in 3
replace p    = `p_cqt_el'                      in 3

export delimited using "/tmp/attrition_tests.csv", replace
di as text "Saved: /tmp/attrition_tests.csv"
list, sep(0) noobs
restore

* ==============================================================================
* SAMPLE FUNNEL VERIFICATION (every N in Figure 1 + §3.1.6 Sample Construction)
* ==============================================================================
preserve

use "${dta}merge_counselor_outcomes.dta", clear

di _n as text "================================================================"
di as text "SAMPLE FUNNEL VERIFICATION"
di as text "================================================================"

* Step 1: total baseline (year=2016, anyone in dataset)
count if year == 2016
local N_baseline = r(N)
di as text "[1] Total baseline (year=2016 rows): `N_baseline'"

* Step 2: treatment vs control at baseline
count if year == 2016 & treatment == 1
local N_trt = r(N)
count if year == 2016 & treatment == 0
local N_ctl = r(N)
di as text "[2] Treatment arm: `N_trt'   |   Control arm: `N_ctl'   |   Sum: " %5.0f `N_trt' + `N_ctl'

* Step 3: PP vs Preg within treatment
count if year == 2016 & treatment == 1 & preg_2016 == 0
local N_pp = r(N)
count if year == 2016 & treatment == 1 & preg_2016 == 1
local N_preg = r(N)
di as text "[3] Within treatment: PP = `N_pp'   |   Preg = `N_preg'   |   Sum: " %5.0f `N_pp' + `N_preg'

* Step 4: treatment with first counselor (any year, since first_counselor is fixed)
bys caseid: egen __any_first = max(!missing(first_counselor))
count if year == 2016 & treatment == 1 & __any_first == 1
local N_first = r(N)
di as text "[4] Treatment with first-counselor assignment: `N_first'"
drop __any_first

* Step 5: by-counselor distribution at baseline
di _n as text "[5] By-counselor distribution at baseline:"
tab first_counselor if year == 2016 & treatment == 1, missing

* Step 6: midline retention (within the 901)
count if year == 2017 & treatment == 1 & !missing(first_counselor)
local N_ml = r(N)
di as text "[6] Midline within 901 (year=2017): `N_ml'"

* Step 7: endline retention (within the 901, no covariate restriction)
count if year == 2018 & treatment == 1 & !missing(first_counselor)
local N_el = r(N)
di as text "[7] Endline within 901 (year=2018): `N_el'"

* Step 8: IV-regression sample = treatment + first counselor + non-missing avg_quality_sattop + non-missing all 12 baseline covariates
* Take woman-level data (one row per woman, e.g., year=2016 to anchor)
keep if treatment == 1 & !missing(first_counselor)
keep if year == 2016

local covar_list "age_group_bl sex_age_bl total_alive_bl ever_use_bl edu_primary_bl work_bl religion_r_bl area_bl ethnicity_r_bl preg_2016 curr_use_bl counselLP_bl"

count
di as text "[8a] Treatment + first counselor at baseline: " r(N)

drop if missing(avg_quality_sattop)
count
local N_iv = r(N)
di as text "[8b] Above + non-missing avg_quality_sattop: `N_iv'"

foreach v of local covar_list {
    drop if missing(`v')
}
count
local N_iv_full = r(N)
di as text "[8c] Above + all 12 covariates non-missing (= IV-regression sample): `N_iv_full'"

* Save list of caseids in IV-regression sample
keep caseid
tempfile iv_sample
save `iv_sample', replace

* Step 9: IV-regression-sample × interviewed at endline = endline-analytic sample
use "${dta}merge_counselor_outcomes.dta", clear
keep if year == 2018
merge m:1 caseid using `iv_sample', keep(match) nogen
count
local N_el_analytic = r(N)
di as text "[9] IV-regression sample interviewed at endline (= §3.1.6 analytic-endline N): `N_el_analytic'"

di _n as text "================================================================"
di as text "SUMMARY for Figure 1 DAG verification:"
di as text "================================================================"
di as text "  Baseline total:         `N_baseline'   (Figure 1 shows: 2143)"
di as text "  Treatment arm:          `N_trt'   (Figure 1 shows: 1026)"
di as text "  Control arm:            `N_ctl'   (Figure 1 shows: 1117)"
di as text "  PP at baseline:         `N_pp'   (Figure 1 shows: 497)"
di as text "  Preg at baseline:       `N_preg'   (Figure 1 shows: 529)"
di as text "  First-counselor sample: `N_first'   (Figure 1 shows: 901)"
di as text "  IV-regression sample:   `N_iv_full'   (§3.1.6 says: 784)"
di as text "  Endline-analytic:       `N_el_analytic'   (§3.1.6 says: 724)"
di as text "================================================================"

* Save funnel to CSV
clear
set obs 8
gen str40 step = ""
gen N = .
gen str40 paper_says = ""

replace step = "Baseline total"            in 1
replace N    = `N_baseline'                in 1
replace paper_says = "2143"                in 1

replace step = "Treatment arm"             in 2
replace N    = `N_trt'                     in 2
replace paper_says = "1026"                in 2

replace step = "Control arm"               in 3
replace N    = `N_ctl'                     in 3
replace paper_says = "1117"                in 3

replace step = "PP at baseline (treatment)" in 4
replace N    = `N_pp'                      in 4
replace paper_says = "497"                 in 4

replace step = "Preg at baseline (treatment)" in 5
replace N    = `N_preg'                    in 5
replace paper_says = "529"                 in 5

replace step = "Treatment with first counselor" in 6
replace N    = `N_first'                   in 6
replace paper_says = "901"                 in 6

replace step = "IV-regression sample"      in 7
replace N    = `N_iv_full'                 in 7
replace paper_says = "784"                 in 7

replace step = "Endline-analytic sample"   in 8
replace N    = `N_el_analytic'             in 8
replace paper_says = "724"                 in 8

export delimited using "/tmp/sample_funnel.csv", replace
list, sep(0) noobs
di as text "Saved: /tmp/sample_funnel.csv"

restore

log close
di as text _n "DONE."
