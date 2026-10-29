********************************************************************************
* build_topic_coverage.do
*
* Authoritative, reproducible computation of the counselor topic-coverage table
* (Appendix Table A5, table6_topic_coverage.tex) and the heatmap data source
* (counselor_topic_analysis.csv). Computes every cell from the session-level
* counseling-tracking data so the table is fully reproducible from the pipeline.
*
* SAMPLE (verified to give N = 3,898 sessions, matching the published N row):
*   - map couns_fv_id codes to counselor IDs 1-6 (drop ID 7 and missing)
*   - keep sessions with a valid, positive recorded session length
*     (drop missing OR zero length; this is the filter that yields 3,898)
*
* Per session, topic k is "covered" if couns_track_<code> == "1" (codes
* 10311-10319 = methods, pregnancy, spacing, side effects, services, STI,
* partner, child health, breastfeeding). Per counselor we report: the share of
* sessions covering each topic; mean topics/session; topic breadth (distinct
* topics ever covered); mean session minutes; and a NORMALIZED Herfindahl index
* HHI = sum_k (s_k / sum_j s_j)^2 (bounded in (0,1]; higher = more concentrated).
* An "Overall" row (couns_id = 0) holds session-weighted means across all 3,898
* sessions.
*
* INPUT:  dta/MFPS_W1_TRACK_COUNSE_FULL.dta
* OUTPUT: tables/counselor_topic_analysis.csv   (one row per counselor + Overall)
********************************************************************************

clear all
set more off
capture log close
log using "log/build_topic_coverage.log", replace text

use "dta/MFPS_W1_TRACK_COUNSE_FULL.dta", clear

gen couns_id = .
replace couns_id = 1 if couns_fv_id_2a == "51"
replace couns_id = 2 if couns_fv_id_2a == "52"
replace couns_id = 3 if couns_fv_id_2a == "54"
replace couns_id = 4 if couns_fv_id_2b == "55"
replace couns_id = 5 if couns_fv_id_2b == "57"
replace couns_id = 6 if couns_fv_id_2b == "58"
replace couns_id = 7 if couns_fv_id_2b == "59"
drop if couns_id == 7
drop if missing(couns_id)

rename couns_track_107 length_session
replace length_session = trim(length_session)
split length_session, parse(":") gen(ls_)
destring ls_1 ls_2, replace force
gen double session_total_min = ls_1 + ls_2/60 if !missing(ls_1, ls_2)
drop if missing(session_total_min) | session_total_min == 0

count
di "N sessions (should be 3,898) = " r(N)
assert r(N) == 3898

local names methods preg spacing sidefx services sti partner child breast
local codes 10311 10312 10313 10314 10315 10316 10317 10318 10319
local i = 0
foreach nm of local names {
    local ++i
    local t : word `i' of `codes'
    gen byte t_`nm' = (couns_track_`t' == "1")
}
egen n_topics_session = rowtotal(t_methods t_preg t_spacing t_sidefx t_services t_sti t_partner t_child t_breast)

* Per-counselor aggregates
foreach nm of local names {
    bysort couns_id: egen share_t_`nm' = mean(t_`nm')
    bysort couns_id: egen _ever_`nm'   = max(t_`nm')
}
egen topic_breadth = rowtotal(_ever_methods _ever_preg _ever_spacing _ever_sidefx _ever_services _ever_sti _ever_partner _ever_child _ever_breast)
bysort couns_id: egen avg_n_topics_couns = mean(n_topics_session)
bysort couns_id: egen mean_minutes = mean(session_total_min)
bysort couns_id: egen n_sessions = count(session_total_min)

* Normalized Herfindahl index = sum (s_k / sum_j s_j)^2
gen double _stot = 0
foreach nm of local names {
    replace _stot = _stot + share_t_`nm'
}
gen double topic_herfindahl = 0
foreach nm of local names {
    replace topic_herfindahl = topic_herfindahl + (share_t_`nm'/_stot)^2
}

preserve
    collapse (first) share_t_methods share_t_preg share_t_spacing share_t_sidefx ///
        share_t_services share_t_sti share_t_partner share_t_child share_t_breast ///
        avg_n_topics_couns topic_breadth mean_minutes topic_herfindahl n_sessions, ///
        by(couns_id)
    tempfile percouns
    save `percouns'
restore

* Overall row (couns_id = 0): session-weighted means across all sessions
preserve
    collapse (mean) share_t_methods=t_methods share_t_preg=t_preg ///
        share_t_spacing=t_spacing share_t_sidefx=t_sidefx share_t_services=t_services ///
        share_t_sti=t_sti share_t_partner=t_partner share_t_child=t_child ///
        share_t_breast=t_breast avg_n_topics_couns=n_topics_session ///
        mean_minutes=session_total_min (count) n_sessions=n_topics_session
    gen couns_id = 0
    gen topic_breadth = .
    gen topic_herfindahl = .
    append using `percouns'
    sort couns_id
    order couns_id share_t_methods share_t_preg share_t_spacing share_t_sidefx ///
        share_t_services share_t_sti share_t_partner share_t_child share_t_breast ///
        avg_n_topics_couns topic_breadth mean_minutes topic_herfindahl n_sessions
    list, clean noobs
    export delimited using "tables/counselor_topic_analysis.csv", replace
    di _n "Saved: tables/counselor_topic_analysis.csv"
restore

log close
