********************************************************************************
* export_km_data.do
*
* Export the data needed by timing_km_plot.R to draw Kaplan-Meier-style
* survival curves of next pregnancy by counselor quality (Figure 6 in the JHE
* paper). Produces /tmp/km_data.csv with one row per endline treatment woman.
*
* Reads:  dta/merge_counselor_outcomes.dta
* Writes: /tmp/km_data.csv
* Log:    log/export_km_data.log
********************************************************************************

capture log close
log using log/export_km_data.log, replace text

use "dta/merge_counselor_outcomes.dta", clear

* Build the binary endogenous (same construction as in results scripts)
quietly summarize avg_quality_sattop if treatment == 1, detail
local p50 = r(p50)
capture drop high_quality_avg_sattop
gen high_quality_avg_sattop = (avg_quality_sattop > `p50') if !missing(avg_quality_sattop) & treatment == 1

keep if year == 2018 & treatment == 1
keep caseid preg_2016 termflag months_to_next_preg next_preg_2018 ///
     high_quality_avg_sattop avg_quality_sattop
* June 2026 (referee comment 12): include ALL subsequent pregnancies, i.e., do
* NOT drop women whose subsequent pregnancy terminated. A termination is itself a
* pregnancy event, and conditioning on it is post-treatment selection. The KM
* curves below therefore use the same all-pregnancy at-risk population as the
* Panel D pregnancy regressions.
keep if !missing(high_quality_avg_sattop)
export delimited /tmp/km_data.csv, replace nolabel

log close
