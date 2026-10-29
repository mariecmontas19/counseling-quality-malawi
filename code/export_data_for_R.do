********************************************************************************
* EXPORT DATA FOR R GRAPHS
* Combines: export_graph_data.do + export_binscatter_data.do
*
* For R graphs: Sections 12, 13, and 15 of graphs_02-12-2026.R
*
* INPUT:  dta/merge_counselor_outcomes.dta
* OUTPUT: tables/dosage_data.csv
*         tables/content_funnel_data.csv
*         tables/content_funnel_pp_preg.csv
*         tables/binscatter_data.csv
*
* CREATED: Marie Montas, 25 Feb 2026
* UPDATED: Marie Montas, 2 Mar 2026  (merged two export files)
********************************************************************************

clear all
set more off

if "$MFPS_ROOT" == "" global MFPS_ROOT "`c(pwd)'"  // project folder (holds Analysis/)
if inlist("`c(os)'", "MacOSX", "Unix") {
    local base_path "${MFPS_ROOT}/Analysis"
}

capture log close
log using "`base_path'/log/export_data_for_R.log", replace text

* =============================================================================
* PART 1: DOSAGE DATA — CQT terciles x pregnancy status
* =============================================================================

use "`base_path'/dta/merge_counselor_outcomes.dta", clear

* Create cqt_scaled (same as analysis dofile)
quietly summarize cqt_resid_loo if year == 2018 & treatment == 1
local cqt_range = r(max) - r(min)
gen double cqt_scaled = cqt_resid_loo / `cqt_range'

* Treatment group only, one row per woman
keep if treatment == 1
bysort caseid (year): keep if _n == 1

* Create CQT terciles
xtile cqt_tercile = cqt_scaled, nq(3)

* Collapse to get mean sessions by tercile x pregnancy status
collapse (mean) mean_sessions=total_sessions ///
         (sd) sd_sessions=total_sessions ///
         (count) n=total_sessions, ///
         by(cqt_tercile preg_2016)

* Compute standard error
gen se_sessions = sd_sessions / sqrt(n)

* Drop if missing grouping variables
drop if missing(cqt_tercile) | missing(preg_2016)

* Export
export delimited using "`base_path'/tables/dosage_data.csv", replace

di "Exported dosage_data.csv: `=_N' rows"

* =============================================================================
* PART 2: CONTENT FUNNEL DATA — session count x pregnancy status
* =============================================================================

clear
use "`base_path'/dta/merge_counselor_outcomes.dta", clear

* Treatment group only, one row per woman
keep if treatment == 1
bysort caseid (year): keep if _n == 1

* Create session bins: <=3, 4-5, 6+
gen sess_bin = .
replace sess_bin = 1 if total_sessions >= 1 & total_sessions <= 3
replace sess_bin = 2 if total_sessions >= 4 & total_sessions <= 5
replace sess_bin = 3 if total_sessions >= 6 & !missing(total_sessions)

* Collapse: mean P(ever discussed spacing) by session bin x pregnancy
collapse (mean) pct_ever_spacing=ever_topic_spacing ///
         (count) n=ever_topic_spacing, ///
         by(sess_bin preg_2016)

* Drop if missing grouping variables
drop if missing(sess_bin) | missing(preg_2016)

* Export
export delimited using "`base_path'/tables/content_funnel_data.csv", replace

di "Exported content_funnel_data.csv: `=_N' rows"

* =============================================================================
* PART 3: CONTENT FUNNEL — PP only, split by next_preg_2018
* =============================================================================

clear
use "`base_path'/dta/merge_counselor_outcomes.dta", clear

* Treatment group only, one row per woman
keep if treatment == 1
bysort caseid (year): keep if _n == 1

* Restrict to postpartum women only
keep if preg_2016 == 0

* Create session bins: <=3, 4-5, 6+ (matching Panel A)
gen sess_bin = .
replace sess_bin = 1 if total_sessions >= 1 & total_sessions <= 3
replace sess_bin = 2 if total_sessions >= 4 & total_sessions <= 5
replace sess_bin = 3 if total_sessions >= 6 & !missing(total_sessions)

* Collapse by session bin x next_preg_2018
collapse (mean) pct_ever_spacing=ever_topic_spacing ///
         (count) n=ever_topic_spacing, ///
         by(sess_bin next_preg_2018)

drop if missing(sess_bin) | missing(next_preg_2018)

export delimited using "`base_path'/tables/content_funnel_pp_preg.csv", replace

di "Exported content_funnel_pp_preg.csv: `=_N' rows"

* =============================================================================
* PART 4: BINSCATTER DATA — CQT vs Unintended Pregnancy
* =============================================================================

clear
use "`base_path'/dta/merge_counselor_outcomes.dta", clear

* Create cqt_scaled (same as analysis dofile)
quietly summarize cqt_resid_loo if year == 2018 & treatment == 1
local cqt_range = r(max) - r(min)
gen double cqt_scaled = cqt_resid_loo / `cqt_range'

* Restrict to treatment group, endline only
keep if year == 2018 & treatment == 1

* Keep only needed variables
keep caseid cqt_scaled unintended_preg_expost preg_2016

* Drop observations with missing key variables
drop if missing(cqt_scaled) | missing(unintended_preg_expost)

* Export to CSV
export delimited using "`base_path'/tables/binscatter_data.csv", replace

di "Exported binscatter_data.csv: `=_N' observations"

di _n
di "=== All data exports for R complete ==="

log close
