********************************************************************************
* build_takeup_indicators.do
*
* Construct woman-level binary indicators for take-up of the two ancillary
* MFPS intervention components (transport voucher and clinic reimbursement),
* and merge them onto the main analysis dataset.
*
* INPUTS:
*   - Analysis/dta/merge_counselor_outcomes.dta (woman-year panel)
*   - Analysis/dta/Intervention Components Data/RAW DATA/MFPS_W1_TRACK_TRANSP_FULL.dta
*       (211 distinct caseids; transport voucher transactions)
*   - Analysis/dta/Intervention Components Data/clinic_reimbursement_log.csv
*       (parsed from CLINIC REIMBURSEMENT LOG DEIDENTIFIED - 7-19-16.xlsx
*        via Python; uses PROGRAM ID, ~210 distinct women)
*   - Analysis/dta/Intervention Components Data/program_id_caseid_map.dta
*       (master mapping: 999 PROGRAM_ID -> caseid pairs; built from FIRSTV +
*        TRANSP + COUNSE + FPCARE + AMEND1)
*
* OUTPUTS (variables added to merge_counselor_outcomes.dta):
*   received_transport       : binary (1 if appeared in TRANSP; 0 otherwise
*                              among treatment; missing for control)
*   received_reimbursement   : binary (1 if appeared in clinic reimbursement
*                              Excel for an FP-related transaction;
*                              0 otherwise among treatment; missing for control)
*   n_transport_trips        : count of distinct transport transactions
*   n_reimbursement_visits   : count of distinct FP reimbursement transactions
*   received_any_ancillary   : OR of the two binary indicators
*
* REIMBURSEMENT FIX (2026-05-04): the previous FPCARE.dta source had only 12
* women; the deidentified clinic Excel log has 210, which matches Mahesh's
* understanding. We now use the Excel as the authoritative source.
*
* This dofile is called from cleaning_counseling_02-10-2026.do near the end,
* AFTER merge_counselor_outcomes.dta has been built.
********************************************************************************

clear all
set more off
capture log close

if "$MFPS_ROOT" == "" global MFPS_ROOT "`c(pwd)'"  // project folder (holds Analysis/)
cd "${MFPS_ROOT}/Analysis"

global INTDIR    "dta/Intervention Components Data"
global RAWDIR    "${INTDIR}/RAW DATA"
global LOGDIR    "log"

log using "${LOGDIR}/build_takeup_indicators.log", replace text

* ============================================================
* PART 1: Build received_transport (woman-level)
* ============================================================
use "${RAWDIR}/MFPS_W1_TRACK_TRANSP_FULL.dta", clear
keep caseid trans_case_woman_pid
* Each row is one transport trip. Collapse to one row per caseid with count.
gen byte _one = 1
collapse (sum) n_transport_trips = _one (firstnm) trans_case_woman_pid, by(caseid)
gen byte received_transport = 1
label var received_transport     "1 = received transport voucher (any trip), 0 otherwise"
label var n_transport_trips      "Number of transport voucher trips"
keep caseid received_transport n_transport_trips
tempfile transp_indicator
save `transp_indicator'
di as text "TRANSP indicator: " _N " distinct women"

* ============================================================
* PART 2: Build received_reimbursement (woman-level) from Excel CSV
* ============================================================
import delimited "${INTDIR}/clinic_reimbursement_log.csv", clear varnames(1) ///
    case(preserve) bindquote(strict) maxquotedrows(unlimited) stringcols(_all)
rename PROGRAMID program_id
rename FPSERVICE fp_service
* Some rows have empty/blank program_id (spacers) - drop them
keep if !missing(program_id) & program_id != ""

* ----------------------------------------------------------------------------
* HAND-CORRECTIONS for 3 PROGRAM IDs that did not merge to the master roster
* due to data-entry typos in the Excel log:
*   "1064"   (no letter prefix)  -> N1064  (FMID 2, Consultation & Injectables;
*                                            disambiguated by FMID + service
*                                            profile of N1064 in same log)
*   "1136"   (no letter prefix)  -> S1136  (only 1136-suffix ID with FMID 2 /
*                                            injectable service in this log)
*   "N11868" (5-digit)           -> N1186  (extra trailing "8"; N1868 doesn't
*                                            exist in master roster, N1186 does)
* ----------------------------------------------------------------------------
replace program_id = "N1064" if program_id == "1064"
replace program_id = "S1136" if program_id == "1136"
replace program_id = "N1186" if program_id == "N11868"
* Restrict to FP-related services (matches Mahesh's intent: "FP care reimbursement")
gen byte _is_fp = (lower(fp_service) == "yes")
di as text "Reimbursement Excel rows total: " _N
qui count if _is_fp
di as text "Reimbursement Excel rows FP-related: " r(N)
qui count if !_is_fp
di as text "Reimbursement Excel rows non-FP / blank: " r(N)
keep if _is_fp == 1
gen byte _one = 1
collapse (sum) n_reimbursement_visits = _one, by(program_id)
qui count
di as text "Distinct PROGRAM IDs with FP reimbursement: " r(N)

* Merge to caseid using master mapping
merge 1:1 program_id using "${INTDIR}/program_id_caseid_map.dta", ///
    keep(master match) generate(_m_map)
qui count if _m_map == 1
di as text "Reimbursement PROGRAM IDs that didn't merge: " r(N)
qui count if _m_map == 3
di as text "Reimbursement PROGRAM IDs that merged: " r(N)
* Diagnostic: which IDs failed?
list program_id n_reimbursement_visits if _m_map == 1, clean noobs
keep if _m_map == 3
keep caseid n_reimbursement_visits
gen byte received_reimbursement = 1
label var received_reimbursement "1 = received clinic reimbursement (FP service), 0 otherwise"
label var n_reimbursement_visits "Number of clinic FP reimbursement transactions"
duplicates report caseid
* Sanity check: should be exactly one row per caseid (since we collapsed by program_id and program_id maps 1-1 to caseid)
tempfile reimburse_indicator
save `reimburse_indicator'
di as text "REIMBURSEMENT indicator: " _N " distinct women"

* ============================================================
* PART 3: Merge both indicators onto the main analysis dataset
* ============================================================
use "dta/merge_counselor_outcomes.dta", clear
di as text "Main dataset: " _N " woman-year rows"

merge m:1 caseid using `transp_indicator', generate(_m_transp) keep(master match)
merge m:1 caseid using `reimburse_indicator', generate(_m_reimb) keep(master match)

* Code 0 for treatment women not appearing in either tracking dataset;
* missing for control women (since they were never offered)
foreach v in received_transport received_reimbursement {
    replace `v' = 0 if missing(`v') & treatment == 1
    replace `v' = .  if missing(`v') & treatment != 1
}
foreach v in n_transport_trips n_reimbursement_visits {
    replace `v' = 0 if missing(`v') & treatment == 1
    replace `v' = .  if missing(`v') & treatment != 1
}

* Combined "received any ancillary" indicator
gen received_any_ancillary = max(received_transport, received_reimbursement)
label var received_any_ancillary "1 = received transport OR reimbursement, 0 otherwise"

drop _m_transp _m_reimb

* Save
save "dta/merge_counselor_outcomes.dta", replace

* ============================================================
* PART 4: Verification tabulations
* ============================================================
di _n as text "============================================================"
di as text "VERIFICATION: take-up rates"
di as text "============================================================"

di _n as text "By treatment arm (any year, deduplicated by caseid):"
preserve
    bys caseid: keep if _n == 1
    tab received_transport     treatment, missing
    tab received_reimbursement treatment, missing
    tab received_any_ancillary treatment, missing
restore

di _n as text "Among year==2018 endline treatment women:"
preserve
    keep if year == 2018 & treatment == 1
    summarize received_transport received_reimbursement received_any_ancillary
    summarize n_transport_trips n_reimbursement_visits if received_transport == 1 | received_reimbursement == 1
    di "Distinct women with received_transport == 1: "
    qui count if received_transport == 1
    di "  " r(N)
    qui count if received_reimbursement == 1
    di "Distinct women with received_reimbursement == 1: "
    di "  " r(N)
restore

log close
di as text "Done: take-up indicators saved to merge_counselor_outcomes.dta"
