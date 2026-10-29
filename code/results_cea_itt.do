********************************************************************************
* results_cea_itt.do
*
* Cost-effectiveness input: the program's CAUSAL effect on contraceptive use.
*
* The paper's own identification is WITHIN the treatment arm (counseling-quality
* IV), so it cannot recover a treatment-vs-control effect. For the cost-per-
* additional-user denominator we therefore use the intention-to-treat (ITT)
* contrast from the parent RCT, exactly as estimated in Karra, Maggio & Canning
* (2022, PNAS, "The causal effect of a family planning intervention on women's
* contraceptive use and birth spacing"), Table 2 top panel. This is the full
* INTERVENTION BUNDLE effect (counseling + transport + reimbursement + field-
* manager accompaniment); the CEA section states that scope explicitly.
*
* Replicates the PNAS DO_3_MAIN_RESULTS.do specification (endline, year==2018):
*   reg curr_use_inj_adj  <baseline> treatment <covariates>, robust
*   reg more_eff_method   <baseline> treatment <covariates>, robust
* and also the unadjusted (col-1) spec, then writes the treatment coefficients
* to a CSV that build_cea_costs.py consumes.
*
* INPUT:  ../Replication package PNAS paper/dataverse_files/MFPS_PNAS_MAIN_ITT_DATA.dta
* OUTPUT: CEA/outputs/cea_itt.csv
********************************************************************************

clear all
set more off

if "$MFPS_ROOT" == "" global MFPS_ROOT "`c(pwd)'"  // project folder (holds Analysis/)
if inlist("`c(os)'", "MacOSX", "Unix") {
    global base "${MFPS_ROOT}/Analysis"
}
global pnas  "${base}/../Replication package PNAS paper/dataverse_files"
global ceaout "${base}/CEA/outputs"
cap mkdir "${ceaout}"

cap log close
log using "${base}/log/results_cea_itt.log", replace text

use "${pnas}/MFPS_PNAS_MAIN_ITT_DATA.dta", clear
xtset caseid_n year

global covariates "i.area_bl i.age_group_bl sex_age_bl total_alive_bl ever_use_bl edu_primary_bl work_bl religion_r_bl ethnicity_r_bl"

tempfile ittdta
tempname pf
postfile `pf' str20 outcome str10 spec double(b se pval n cmean) using "`ittdta'", replace

* helper: control endline mean for an outcome
foreach y in curr_use_inj_adj more_eff_method {

    quietly summarize `y' if year==2018 & treatment==0
    local cm = r(mean)

    * --- unadjusted (PNAS col 1) ---
    reg `y' treatment if year==2018, robust
    local b  = _b[treatment]
    local se = _se[treatment]
    local p  = 2*ttail(e(df_r), abs(_b[treatment]/_se[treatment]))
    post `pf' ("`y'") ("unadj") (`b') (`se') (`p') (e(N)) (`cm')

    * --- adjusted (PNAS col 2, preferred): baseline value + covariates ---
    reg `y' `y'_bl treatment $covariates if year==2018, robust
    local b  = _b[treatment]
    local se = _se[treatment]
    local p  = 2*ttail(e(df_r), abs(_b[treatment]/_se[treatment]))
    post `pf' ("`y'") ("adj") (`b') (`se') (`p') (e(N)) (`cm')

    di as result "`y': unadj/adj treatment effects posted (control endline mean = " %5.3f `cm' ")"
}

postclose `pf'

* write a proper CSV and echo it for the log
use "`ittdta'", clear
export delimited using "${ceaout}/cea_itt.csv", replace
list, clean noobs

log close
