********************************************************************************
* build_subgroup_means.do
*
* Produce the three subgroup-mean CSVs that the LaTeX table builders consume
* for the "Mean DV (PP)", "Mean DV (Preg)", and timing-mechanism mean rows.
*
* Reads:  dta/merge_counselor_outcomes.dta
* Writes: /tmp/subgroup_means_table9_10.csv  (consumed by build_main_iv.py)
*         /tmp/subgroup_means_table11.csv    (consumed by build_timing.py)
*         /tmp/subgroup_means_intervals.csv  (consumed by build_timing_intervals.py)
* Log:    log/build_subgroup_means.log
********************************************************************************

capture log close
log using log/build_subgroup_means.log, replace text

use "dta/merge_counselor_outcomes.dta", clear

* ---- Table 9/10 (main IV): means by PP/Preg for the 19 main outcomes ----
* Panels A/B (continuity + topics) are estimated at the woman level (one obs per
* woman; see results_main_iv.do), so their mean DVs are woman-level too.
* Panels C/D (FP coverage + reproductive) are endline (year==2018) one-per-woman.
local outsAB ///
    share_same_counselor total_sessions visit_rate_mo husband_present ///
    etp1_fp etp1_spacing etp1_preg etp1_partner etp1_childhealth etp1_breast
local outsCD ///
    admin_fp_any admin_inject_any n_admin_fp fp_inj_coverage_frac husband_knows_fp ///
    birth_within_24mo preg_within_12mo preg_within_24mo mistimed_preg_expost

capture file close f
file open f using /tmp/subgroup_means_table9_10.csv, write replace
file write f "outcome,mn_pp,mn_preg" _n

* Panels C/D: endline means by subgroup
foreach y of local outsCD {
    quietly summarize `y' if year == 2018 & treatment == 1 & preg_2016 == 0, meanonly
    local m_pp = r(mean)
    quietly summarize `y' if year == 2018 & treatment == 1 & preg_2016 == 1, meanonly
    local m_pr = r(mean)
    file write f "`y',`m_pp',`m_pr'" _n
}

* Panels A/B: woman-level means (collapse to one obs per woman) by subgroup
preserve
keep if treatment == 1
collapse (mean) `outsAB' preg_2016, by(caseid)
foreach y of local outsAB {
    quietly summarize `y' if preg_2016 == 0, meanonly
    local m_pp = r(mean)
    quietly summarize `y' if preg_2016 == 1, meanonly
    local m_pr = r(mean)
    file write f "`y',`m_pp',`m_pr'" _n
}
restore
file close f
di "Wrote /tmp/subgroup_means_table9_10.csv"

* ---- Table 11 (timing mechanism): means by recent-PP/older-PP, 3rd-tri/earlier-Preg ----
local outs11 admin_fp_any admin_inject_any n_admin_fp fp_inj_coverage_frac fp_inj_protected_day birth_within_24mo preg_within_24mo

capture file close f
file open f using /tmp/subgroup_means_table11.csv, write replace
file write f "outcome,recent_pp,older_pp,third_tri_preg,earlier_preg" _n

foreach y of local outs11 {
    quietly summarize `y' if year == 2018 & treatment == 1 & preg_2016 == 0 & pp_late == 0, meanonly
    local m_rec_pp = r(mean)
    quietly summarize `y' if year == 2018 & treatment == 1 & preg_2016 == 0 & pp_late == 1, meanonly
    local m_old_pp = r(mean)
    quietly summarize `y' if year == 2018 & treatment == 1 & preg_2016 == 1 & preg_late == 0, meanonly
    local m_3rd_pr = r(mean)
    quietly summarize `y' if year == 2018 & treatment == 1 & preg_2016 == 1 & preg_late == 1, meanonly
    local m_erl_pr = r(mean)
    file write f "`y',`m_rec_pp',`m_old_pp',`m_3rd_pr',`m_erl_pr'" _n
}
file close f
di "Wrote /tmp/subgroup_means_table11.csv"

* ---- Timing intervals: means for each interval outcome ----
capture file close f
file open f using /tmp/subgroup_means_intervals.csv, write replace
file write f "outcome,mn_pool,mn_pp,mn_preg" _n
foreach y in preg_0_6mo preg_6_12mo preg_12_18mo preg_18_24mo {
    quietly summarize `y' if year == 2018 & treatment == 1, meanonly
    local m_pl = r(mean)
    quietly summarize `y' if year == 2018 & treatment == 1 & preg_2016 == 0, meanonly
    local m_pp = r(mean)
    quietly summarize `y' if year == 2018 & treatment == 1 & preg_2016 == 1, meanonly
    local m_pr = r(mean)
    file write f "`y',`m_pl',`m_pp',`m_pr'" _n
}
file close f
di "Wrote /tmp/subgroup_means_intervals.csv"

log close
