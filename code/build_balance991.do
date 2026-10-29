********************************************************************************
* build_balance991.do
*
* ITT (intention-to-treat) balance table: baseline covariates by the woman's
* RANDOMLY ASSIGNED FIRST-VISIT counselor, over the 991 approached treatment
* women (those with a first-visit form), NOT conditional on attending a session.
*
* Counselor assignment comes from the first-visit tracking file
* (MFPS_W1_TRACK_FIRSTV_FULL: first_fv_id_2a/2b), which carries caseid directly
* and matches the analysis first_counselor 99.0% of the time among the counseled.
* The CQT instrument is assigned at the counselor level (each counselor's mean
* leave-one-out residualized first-session quality), since the endogenous
* realized quality is undefined for women who never attended a session.
*
* OUTPUT: tables/balance991.csv       (one row per covariate: means/SD by
*                                      counselor + Total + ANOVA F,p; plus an
*                                      "N" row)
*         tables/balance991_meta.csv  (chi-square area x counselor; CQT
*                                      orthogonality F-test on the ITT sample)
********************************************************************************
clear all
set more off
if "$MFPS_ROOT" == "" global MFPS_ROOT "`c(pwd)'"  // project folder (holds Analysis/)
if inlist("`c(os)'", "MacOSX", "Unix") {
    global base "${MFPS_ROOT}/Analysis"
}
cd "${base}"
cap log close
log using "log/build_balance991.log", replace text

global covariates "i.age_group_bl sex_age_bl total_alive_bl ever_use_bl edu_primary_bl work_bl religion_r_bl i.area_bl ethnicity_r_bl preg_2016 curr_use_bl counselLP_bl"

* ---- counselor-level CQT (mean leave-one-out residualized first-session quality) ----
use "dta/merge_counselor_outcomes.dta", clear
bysort caseid: keep if _n==1
keep if !missing(first_counselor)
preserve
keep caseid
gen byte attended901 = 1
tempfile a901
save `a901'
restore
collapse (mean) cqt_c = cqt_resid_loo_sattop, by(first_counselor)
drop if missing(first_counselor)
rename first_counselor fvc
tempfile cqtc
save `cqtc'

* ---- first-visit counselor per caseid (the randomized assignment) ----
use "dta/MFPS_W1_TRACK_FIRSTV_FULL.dta", clear
tostring first_fv_id_2a first_fv_id_2b, replace force
gen fvc = .
replace fvc=1 if first_fv_id_2a=="51"
replace fvc=2 if first_fv_id_2a=="52"
replace fvc=3 if first_fv_id_2a=="54"
replace fvc=4 if first_fv_id_2b=="55"
replace fvc=5 if first_fv_id_2b=="57"
replace fvc=6 if first_fv_id_2b=="58"
keep caseid fvc
drop if missing(fvc)
bysort caseid (fvc): keep if _n==1
tempfile fv
save `fv'

* ---- baseline covariates + treatment ----
use "dta/ANALYSIS_w1_w2_w3_2023-09-26.dta", clear
bysort caseid: keep if _n==1
keep caseid treatment age_group_bl sex_age_bl total_alive_bl ever_use_bl ///
     edu_primary_bl work_bl religion_r_bl area_bl ethnicity_r_bl preg_2016 ///
     curr_use_bl counselLP_bl
merge 1:1 caseid using `fv', keep(match) nogen
merge m:1 fvc using `cqtc', keep(match master) nogen
merge 1:1 caseid using `a901', keep(master match) nogen
replace attended901 = 0 if missing(attended901)
keep if treatment==1
keep if inrange(fvc,1,6)

di _n "==== ITT balance sample (approached, treatment, counselor 1-6) ===="
count
di "tab age_group_bl:"
tab age_group_bl, missing
di "tab area_bl:"
tab area_bl, missing

* ---- dummies matching the published table rows ----
* age_group_bl coding is ROTATED (verified against raw age_bl min/max per code,
* July 2026): 0 = 31/35, 1 = <=20, 2 = 21-25, 3 = 26-30. The pre-July-2026
* version of this script assumed 0 = <=20, which shifted every age row of the
* published balance table by one position (David Canning's comment).
gen age1 = age_group_bl==1
gen age2 = age_group_bl==2
gen age3 = age_group_bl==3
gen age4 = age_group_bl==0
foreach a in 7 8 23 24 50 51 56 {
    gen area`a' = area_bl==`a'
}

local rowvars age1 age2 age3 age4 sex_age_bl total_alive_bl ever_use_bl ///
    edu_primary_bl work_bl religion_r_bl area7 area8 area23 area24 area50 ///
    area51 area56 ethnicity_r_bl preg_2016 curr_use_bl counselLP_bl

* ---- per-covariate means/SD by counselor + ANOVA ----
file open f using "tables/balance991.csv", write replace
file write f "rowkey,m1,m2,m3,m4,m5,m6,sd1,sd2,sd3,sd4,sd5,sd6,mtot,sdtot,F,p" _n
foreach v of local rowvars {
    forvalues c = 1/6 {
        quietly summarize `v' if fvc==`c'
        local m`c' = r(mean)
        local s`c' = r(sd)
    }
    quietly summarize `v'
    local mt = r(mean)
    local st = r(sd)
    quietly oneway `v' fvc
    local F  = r(F)
    local pp = Ftail(r(df_m), r(df_r), r(F))
    local out "`v'"
    forvalues c = 1/6 {
        local val = string(`m`c'', "%5.2f")
        local out "`out',`val'"
    }
    forvalues c = 1/6 {
        local val = string(`s`c'', "%5.2f")
        local out "`out',`val'"
    }
    local vmt = string(`mt', "%5.2f")
    local vst = string(`st', "%5.2f")
    local vF  = string(`F',  "%5.2f")
    local vp  = string(`pp', "%5.2f")
    local out "`out',`vmt',`vst',`vF',`vp'"
    file write f "`out'" _n
}
* N by counselor
local out "N"
forvalues c = 1/6 {
    quietly count if fvc==`c'
    local out "`out',`r(N)'"
}
forvalues c = 1/6 {
    local out "`out',"
}
quietly count
local out "`out',`r(N)',,,"
file write f "`out'" _n
file close f

* ---- meta: chi-square (area x counselor) and CQT orthogonality on ITT sample ----
quietly tab area_bl fvc, chi2
local achi = string(r(chi2), "%5.2f")
local adf  = (r(r)-1)*(r(c)-1)
local ap   = string(r(p), "%5.3f")
quietly reg cqt_c $covariates, vce(hc3)
testparm i.age_group_bl sex_age_bl total_alive_bl ever_use_bl edu_primary_bl ///
    work_bl religion_r_bl i.area_bl ethnicity_r_bl preg_2016 curr_use_bl counselLP_bl
local oF  = string(r(F), "%5.2f")
local od1 = r(df)
local od2 = r(df_r)
local op  = string(r(p), "%5.3f")
local oN  = e(N)
* selection check: does the CQT predict attendance (982 -> 901)?
quietly summarize attended901
local atrate = string(r(mean), "%5.3f")
quietly reg attended901 cqt_c, vce(hc3)
local atb = string(_b[cqt_c], "%6.3f")
local att = _b[cqt_c]/_se[cqt_c]
local atp = string(2*ttail(e(df_r), abs(`att')), "%5.3f")
local atN = e(N)
file open m using "tables/balance991_meta.csv", write replace
file write m "stat,value" _n
file write m "area_chi2,`achi'" _n
file write m "area_chi2_df,`adf'" _n
file write m "area_chi2_p,`ap'" _n
file write m "cqt_orth_F,`oF'" _n
file write m "cqt_orth_df1,`od1'" _n
file write m "cqt_orth_df2,`od2'" _n
file write m "cqt_orth_p,`op'" _n
file write m "cqt_orth_N,`oN'" _n
file write m "attend_rate,`atrate'" _n
file write m "attend_cqt_b,`atb'" _n
file write m "attend_cqt_p,`atp'" _n
file write m "attend_cqt_N,`atN'" _n
file close m

di _n "==== meta ===="
type "tables/balance991_meta.csv"
log close
