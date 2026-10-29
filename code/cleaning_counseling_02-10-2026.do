********************************************************************************
* CLEANING DOFILE: COUNSELING DATA
* Paper 2 - Malawi: Impact of counseling quality on FP and reproductive outcomes
********************************************************************************
* CREATED:  Marie Montas, 11 July 2025
* UPDATED:  Marie Montas, Feb 11 2026
*
* PURPOSE:  Cleans the counseling tracking data from the MFPS RCT, constructs
*           counseling quality measures, counselor assignment and continuity
*           variables, and the CQT (Counselor Quality Propensity) instrument
*           using a leave-one-out residualized estimator.
*
* INPUTS:   1. ANALYSIS_w1_w2_w3_2023-09-26.dta  (3-wave survey panel)
*           2. MFPS_W1_TRACK_COUNSE_FULL.dta      (session-level counseling data)
*
* OUTPUTS:  1. date_of_interviews.dta              (interview dates, wide format)
*           2. counseling_collapse_data.dta         (collapsed to caseid x year)
*           3. merge_counselor_outcomes.dta         (final analysis dataset)
*
* STRUCTURE:
*   Part 1: Setup and paths
*   Part 2: Extract interview dates from survey panel
*   Part 3: Clean counseling tracking data (dates, times, visit order)
*   Part 4: Counselor assignment, continuity, and session spacing
*   Part 5: Counseling topics and pregnancy/FP status at first visit
*   Part 6: Counseling quality measures (components + indices)
*   Part 7: Construct CQT instrument (residualized LOO) — before merge
*   Part 8: Collapse to caseid x year and merge with survey panel
*
* NOTES:
*   - Counselor 7 is excluded from the analysis sample (see Part 6).
*   - The CQT instrument is constructed BEFORE the merge/collapse (Part 6)
*     so that counselor-level analyses can be run pre-merge.
*   - Scheduling FEs (first_couns_dow/month/hour) are created in Part 5e
*     for use in the CQT residualization in Part 6.
********************************************************************************

clear all
set more off

********************************************************************************
* PART 1: SETUP AND PATHS
********************************************************************************

if "$MFPS_ROOT" == "" global MFPS_ROOT "`c(pwd)'"  // project folder (holds Analysis/)
if inlist("`c(os)'", "MacOSX", "Unix") {
    global dta "${MFPS_ROOT}/Analysis/dta/"
    global dofile "${MFPS_ROOT}/Analysis/dofile/"
    global tables "${MFPS_ROOT}/Analysis/tables/"
    global figures "${MFPS_ROOT}/Analysis/figures/"
    global log "${MFPS_ROOT}/Analysis/log/"
}

capture log close
log using "${log}cleaning_counseling_02-10-2026.log", replace text




********************************************************************************
* PART 2: EXTRACT INTERVIEW DATES FROM SURVEY PANEL
* Purpose: Get baseline/midline/endline interview dates per woman (wide format).
*          These dates are needed to assign counseling sessions to survey waves.
********************************************************************************

use "${dta}ANALYSIS_w1_w2_w3_2023-09-26.dta", clear

* Parse numeric date variable (YYYYMMDD) into Stata daily date
gen str8 str_date = string(w1_wid_3, "%08.0f")
gen date_int = date(str_date, "YMD")
format date_int %td
drop str_date

keep caseid year w1_wid_3 date_int

* Reshape to wide: one row per woman with date_int2016, date_int2017, date_int2018
reshape wide w1_wid_3 date_int, i(caseid) j(year)

save "${dta}/date_of_interviews.dta", replace


********************************************************************************
* PART 3: CLEAN COUNSELING TRACKING DATA
* Purpose: Load session-level data, parse dates/times, create visit ordering,
*          assign sessions to survey waves, and compute time since first session.
********************************************************************************

use "${dta}/MFPS_W1_TRACK_COUNSE_FULL.dta", clear

* Merge interview dates to assign counseling sessions to survey waves
merge m:1 caseid using "${dta}/date_of_interviews.dta"
keep if _merge == 3
drop _merge

*----- 3a. Counseling date and time -----*

* Parse counseling date (string YYYY-MM-DD -> Stata daily date)
gen counseling_date = date(couns_track_101a, "YMD")
format counseling_date %td
drop if missing(counseling_date)

* Parse counseling time (string HH:MM:SS -> Stata clock for HH:MM display)
gen str8 time_hms = substr(trim(couns_track_101b), 1, 8)
gen double counseling_time = clock("01jan1960 " + time_hms, "DMYhms")
format counseling_time %tcHH:MM
label var counseling_time "Counseling time (HH:MM)"
drop time_hms

*----- 3b. Visit ordering and survey wave assignment -----*

* Sequential visit number within each woman (ordered by date)
bys caseid (counseling_date): gen visit = _n
tab visit

* Assign counseling sessions to survey waves based on interview dates:
*   interval=2: session falls between baseline (2016) and midline (2017)
*   interval=3: session falls between midline (2017) and endline (2018)
*   Sessions after endline are dropped (should not exist in this study).
gen interval = 2
replace interval = 3 if counseling_date > date_int2017 & counseling_date <= date_int2018
drop if counseling_date > date_int2018 & !missing(date_int2018)

* Year variable for merging with survey panel (2017=BL-to-ML, 2018=ML-to-EL)
gen year = cond(interval == 3, 2018, 2017)
label var year "Survey wave: baseline to midline (2017), midline to endline (2018)"
tab year

*----- 3c. Time since first counseling session -----*

* First session date per woman
bys caseid (counseling_date): gen first_session_date = counseling_date[1]
format first_session_date %td

* Months from first session to 2018 endline interview
gen double months_since_first_2018 = ///
    (year(date_int2018) - year(first_session_date)) * 12 ///
    + (month(date_int2018) - month(first_session_date))

* Collapse to woman-level constant
bys caseid: egen months_since_first_final = max(months_since_first_2018)
label var months_since_first_final "Months between first counseling session and 2018 interview"

* Drop intermediate variable (no longer needed)
drop months_since_first_2018


********************************************************************************
* PART 4: COUNSELOR ASSIGNMENT, CONTINUITY, AND SESSION SPACING
* Purpose: Identify each woman's first counselor, compute measures of counselor
*          continuity (share of visits with same counselor), and session spacing
*          (gaps between visits, visit frequency).
********************************************************************************

*----- 4a. Counselor ID -----*

* Recode string counselor IDs to numeric (1-7)
* Note: IDs 51-54 come from couns_fv_id_2a; IDs 55-59 from couns_fv_id_2b
gen couns_id = .
replace couns_id = 1 if couns_fv_id_2a == "51"
replace couns_id = 2 if couns_fv_id_2a == "52"
replace couns_id = 3 if couns_fv_id_2a == "54"
replace couns_id = 4 if couns_fv_id_2b == "55"
replace couns_id = 5 if couns_fv_id_2b == "57"
replace couns_id = 6 if couns_fv_id_2b == "58"
replace couns_id = 7 if couns_fv_id_2b == "59"
tab couns_id, missing

*----- 4b. First counselor and continuity measures -----*

sort caseid counseling_date

* Identify counselor at woman's first visit
by caseid (counseling_date): gen first_counselor = couns_id[1]
label var first_counselor "Counselor at first visit"

* Indicator: this visit is with the same counselor as the first visit
gen same_counselor = (couns_id == first_counselor) if !missing(couns_id, first_counselor)

* Woman-level continuity measures
bys caseid: egen visits_same_counselor = total(same_counselor)
bys caseid: egen total_sessions       = total(!missing(counseling_date))

gen share_same_counselor = visits_same_counselor / total_sessions if total_sessions > 0
gen same_counselor_dummy = (visits_same_counselor == total_sessions) ///
    if !missing(visits_same_counselor, total_sessions)

label var visits_same_counselor "Number of visits with first counselor"
label var total_sessions        "Total number of counseling sessions"
label var share_same_counselor  "Share of sessions with first counselor"
label var same_counselor_dummy  "Stayed with same counselor (1=Yes)"

label define same_lbl 0 "Switched counselors" 1 "Stayed with same counselor"
label values same_counselor_dummy same_lbl

* Average sessions per counselor (counselor-level workload measure)
bysort first_counselor caseid: gen _tag_patient = (_n == 1)
bysort first_counselor: egen _n_patients = total(_tag_patient)
bysort first_counselor: gen _n_sessions = _N
gen avg_sessions_per_couns = _n_sessions / _n_patients
label var avg_sessions_per_couns "Average number of sessions per counselor"
drop _tag_patient _n_patients _n_sessions

* Quick check
sum visits_same_counselor total_sessions share_same_counselor avg_sessions_per_couns

*----- 4c. Session spacing and frequency -----*
*
* KEY DESIGN NOTE: All spacing variables are WOMAN-LEVEL CONSTANTS computed
* across all sessions regardless of survey wave. They measure how frequently
* a woman met with her counselor over the full study period.
*
* Variables created:
*   gap_days             - session-level: days since previous session
*   avg_days_between     - woman-level: mean days between consecutive sessions
*   median_days_between  - woman-level: median days between consecutive sessions
*   sd_gap_days          - woman-level: SD of inter-session gaps (regularity)
*   followup_days        - woman-level: total days from first to last session
*   visit_rate_mo        - woman-level: sessions per month over follow-up
*   days_to_second       - woman-level: days from 1st to 2nd session (early return)

* Gap in days between consecutive sessions (session-level)
* Data is already sorted by caseid counseling_date; gaps are always >= 0
bysort caseid (counseling_date): gen gap_days = counseling_date - counseling_date[_n-1] if _n > 1
label var gap_days "Days since previous counseling session"

* Woman-level mean and median of inter-session gaps
* These are computed once across ALL sessions per woman, then carried forward
bys caseid: egen avg_days_between   = mean(gap_days)
bys caseid: egen median_days_between = median(gap_days)

label var avg_days_between    "Mean days between counseling sessions"
label var median_days_between "Median days between counseling sessions"

* Follow-up window: days from first to last counseling session
bysort caseid (counseling_date): ///
    gen followup_days = counseling_date[_N] - counseling_date[1]
label var followup_days "Days from first to last counseling session"

* Visit rate: sessions per month over the observed follow-up window
gen visit_rate_mo = (total_sessions - 1) / (followup_days / 30) if followup_days > 0
label var visit_rate_mo "Sessions per month over follow-up window"

* Days from 1st to 2nd session (woman-level constant: early engagement measure)
bys caseid (counseling_date): gen _d2 = counseling_date[2] - counseling_date[1]
bys caseid: egen days_to_second = max(_d2)
drop _d2
label var days_to_second "Days from 1st to 2nd counseling session"

* Set missing for women with too few sessions for meaningful measures
replace avg_days_between    = . if total_sessions < 2
replace median_days_between = . if total_sessions < 2
replace visit_rate_mo       = . if total_sessions < 2
replace days_to_second      = . if total_sessions < 2

********************************************************************************
* PART 5: COUNSELING TOPICS AND PREGNANCY/FP STATUS AT FIRST VISIT
* Purpose: Create binary indicators for each counseling topic discussed
*          (aggregated per woman-year), and extract pregnancy and FP use
*          status at the first counseling visit.
********************************************************************************

*----- 5a-pre. Session-level topic counts (before per-year aggregation) -----*

* Parse all 9 topic strings → binary indicators at session level
forvalues j = 10311/10319 {
    gen byte _tbin_`j' = (couns_track_`j' == "1")
}
egen _n_topics_session = rowtotal(_tbin_10311-_tbin_10319)
label var _n_topics_session "Number of topics requested this session"

* First-session topic count (woman-level constant)
bys caseid (visit): gen n_topics_first_session = _n_topics_session[1]
label var n_topics_first_session "Number of topics requested at first session"
bys first_counselor: sum n_topics_first_session

* Average topic count across all sessions
bys caseid: egen n_topics_avg_all = mean(_n_topics_session)
label var n_topics_avg_all "Mean topics requested per session (all sessions)"
tab n_topics_avg_all

drop _tbin_*
* Keep _n_topics_session for the 4-component per-session quality index
* (built in Part 6 below). Drop after that.

*----- 5a. Counseling topics discussed (per woman-year) -----*

* Topics are string variables couns_track_10311-10319; recode to binary
* and aggregate to woman-year level (1 if topic discussed in ANY session)
local topics   "10311 10312 10313 10314 10315 10316 10317 10318 10319"
local names    "methods preg spacing sidefx services sti partner child breast"
local labels   `" "Info on particular methods" "Info on pregnancy" "Info on birth spacing" "Info on side effects" "Info on service availability" "Info on STIs" "Info on partner engagement" "Info on child health" "Info on breastfeeding" "'

local n_topics : word count `topics'
forvalues i = 1/`n_topics' {
    local t  : word `i' of `topics'
    local nm : word `i' of `names'
    local lb : word `i' of `labels'
    gen _tmp = (couns_track_`t' == "1")
    bys caseid year: egen topic_`nm' = max(_tmp)
    label var topic_`nm' "`lb'"
    drop _tmp
}

*----- 5a-post. New continuity and topic outcomes -----*

* only_one_session: binary (total_sessions == 1)  [total_sessions exists from Part 4b]
capture drop only_one_session
gen only_one_session = (total_sessions == 1)
label var only_one_session "Had only one counseling session"

* had_second_session: complement
capture drop had_second_session
gen had_second_session = (total_sessions >= 2)
label var had_second_session "Had 2+ counseling sessions"

* ever_topic_fp: FP info requested in ANY session ever
* FP = methods (10311) OR side effects (10314) OR service availability (10315)
capture drop _fp_session
gen _fp_session = (couns_track_10311 == "1" | ///
                   couns_track_10314 == "1" | ///
                   couns_track_10315 == "1")
bys caseid: egen ever_topic_fp = max(_fp_session)
label var ever_topic_fp "FP info requested in any session (ever)"
drop _fp_session

* ever_topic_spacing: birth spacing info requested in ANY session ever
capture drop _spacing_session
gen _spacing_session = (couns_track_10313 == "1")
bys caseid: egen ever_topic_spacing = max(_spacing_session)
label var ever_topic_spacing "Birth spacing info requested in any session (ever)"
drop _spacing_session

* ever_topic_preg: pregnancy info requested in ANY session ever
capture drop _preg_session
gen _preg_session = (couns_track_10312 == "1")
bys caseid: egen ever_topic_preg = max(_preg_session)
label var ever_topic_preg "Pregnancy info requested in any session (ever)"
drop _preg_session

* ever_topic_partner: partner engagement info requested in ANY session ever
capture drop _partner_session
gen _partner_session = (couns_track_10317 == "1")
bys caseid: egen ever_topic_partner = max(_partner_session)
label var ever_topic_partner "Partner engagement info requested in any session (ever)"
drop _partner_session

* ever_topic_childhealth: child health info requested in ANY session ever (10318 only)
capture drop _childhealth_session
gen _childhealth_session = (couns_track_10318 == "1")
bys caseid: egen ever_topic_childhealth = max(_childhealth_session)
label var ever_topic_childhealth "Child health info requested in any session (ever)"
drop _childhealth_session

* ever_topic_breast: breastfeeding info requested in ANY session ever (10319 only)
capture drop _breast_session
gen _breast_session = (couns_track_10319 == "1")
bys caseid: egen ever_topic_breast = max(_breast_session)
label var ever_topic_breast "Breastfeeding info requested in any session (ever)"
drop _breast_session

* Keep combined version for backward compatibility (not used in analysis globals)
capture drop _child_session
gen _child_session = (couns_track_10318 == "1" | couns_track_10319 == "1")
bys caseid: egen ever_topic_child = max(_child_session)
label var ever_topic_child "Child health OR breastfeeding info (combined, deprecated)"
drop _child_session

* ever_topic_sti: STI info requested in ANY session ever
capture drop _sti_session
gen _sti_session = (couns_track_10316 == "1")
bys caseid: egen ever_topic_sti = max(_sti_session)
label var ever_topic_sti "STI info requested in any session (ever)"
drop _sti_session

*----- 5a-post (DAC revision). Post-first-session topic outcomes -----*
* Reconstruction excluding session 1 to break mechanical correlation with the
* CQT instrument's quality index, which uses n_topics_first_session as one of
* its 4 components. These etp1_* outcomes use only sessions 2+ for the per-woman
* "ever requested" indicator.
*
* Naming: etp1_X = "Ever Topic Post-1st-session: X". Short prefix used to
* satisfy Stata's 32-character estimate-name limit when combined with regression
* prefixes (ivd6_, rfml_, etc.) in the analysis dofile.

* etp1_fp: FP info (10311 OR 10314 OR 10315), sessions 2+ only
capture drop _fp_post1
gen _fp_post1 = (visit > 1) & ( ///
        couns_track_10311 == "1" | ///
        couns_track_10314 == "1" | ///
        couns_track_10315 == "1")
bys caseid: egen etp1_fp = max(_fp_post1)
label var etp1_fp "FP info requested in session 2+ (ever) [post-first-session]"
drop _fp_post1

* etp1_spacing
capture drop _spacing_post1
gen _spacing_post1 = (visit > 1) & (couns_track_10313 == "1")
bys caseid: egen etp1_spacing = max(_spacing_post1)
label var etp1_spacing "Birth spacing info requested in session 2+ (ever) [post-first-session]"
drop _spacing_post1

* etp1_preg
capture drop _preg_post1
gen _preg_post1 = (visit > 1) & (couns_track_10312 == "1")
bys caseid: egen etp1_preg = max(_preg_post1)
label var etp1_preg "Pregnancy info requested in session 2+ (ever) [post-first-session]"
drop _preg_post1

* etp1_partner
capture drop _partner_post1
gen _partner_post1 = (visit > 1) & (couns_track_10317 == "1")
bys caseid: egen etp1_partner = max(_partner_post1)
label var etp1_partner "Partner engagement info requested in session 2+ (ever) [post-first-session]"
drop _partner_post1

* etp1_childhealth
capture drop _childhealth_post1
gen _childhealth_post1 = (visit > 1) & (couns_track_10318 == "1")
bys caseid: egen etp1_childhealth = max(_childhealth_post1)
label var etp1_childhealth "Child health info requested in session 2+ (ever) [post-first-session]"
drop _childhealth_post1

* etp1_breast
capture drop _breast_post1
gen _breast_post1 = (visit > 1) & (couns_track_10319 == "1")
bys caseid: egen etp1_breast = max(_breast_post1)
label var etp1_breast "Breastfeeding info requested in session 2+ (ever) [post-first-session]"
drop _breast_post1

* etp1_sti
capture drop _sti_post1
gen _sti_post1 = (visit > 1) & (couns_track_10316 == "1")
bys caseid: egen etp1_sti = max(_sti_post1)
label var etp1_sti "STI info requested in session 2+ (ever) [post-first-session]"
drop _sti_post1

*----- 5b. Pregnancy and FP status at first counseling visit -----*

* Pregnancy status (couns_track_104: 1=yes, 2=no)
rename couns_track_104 pregnant_raw
destring pregnant_raw, gen(pregnant_n) force
replace pregnant_n = 0 if pregnant_n == 2
bys caseid (visit): egen pregnant_first = max(cond(visit==1, pregnant_n, .))
label var pregnant_n     "Pregnant at visit (0/1)"
label var pregnant_first "Pregnant at first counseling visit (0/1)"

* Family planning use (couns_track_105: 1=yes, 2=no)
rename couns_track_105 fp_use_raw
destring fp_use_raw, gen(fp_use_n) force
replace fp_use_n = 0 if fp_use_n == 2
bys caseid (visit): egen fp_use_first = max(cond(visit==1, fp_use_n, .))
label var fp_use_n     "Using FP at visit (0/1)"
label var fp_use_first "Using FP at first counseling visit (0/1)"

********************************************************************************
* PART 6: COUNSELING QUALITY MEASURES
* Purpose: Clean quality components (questions answered, satisfaction, session
*          length), extract first-session values, construct summary-weighted
*          quality indices (first-session and average across all sessions).
********************************************************************************

*----- 6a. Quality components: clean and extract first-session values -----*

* Questions answered (binary: all questions answered = 1)
rename couns_track_108 questions_answered
destring questions_answered, gen(questions_answered_n) force
replace questions_answered_n = 0 if questions_answered_n == 2
bys caseid (visit): egen answered_first_all = max(cond(visit==1, questions_answered_n, .))
label var questions_answered_n "All questions answered (0/1)"
label var answered_first_all   "All questions answered at first session"

* Counseling satisfaction (ordinal: 1=Not, 2=Somewhat, 3=Satisfied, 4=Very)
rename couns_track_110 counseling_satisfaction
destring counseling_satisfaction, gen(counseling_satisfaction_n) force
bys caseid (visit): egen satisfaction_first_all = max(cond(visit==1, counseling_satisfaction_n, .))
label var counseling_satisfaction_n "Counseling satisfaction (1-4)"
label var satisfaction_first_all    "Counseling satisfaction at first session"

* Session length in minutes (parsed from MM:SS string)
rename couns_track_107 length_session
replace length_session = trim(length_session)
split length_session, parse(":") gen(ls_)
destring ls_1 ls_2, replace force
gen session_total_min = ls_1 + ls_2/60 if !missing(ls_1, ls_2)
drop ls_1 ls_2
drop if session_total_min == 0

bys caseid (visit): egen session_first_all = max(cond(visit==1, session_total_min, .))
label var session_total_min  "Counseling session length (minutes)"
label var session_first_all  "Session length at first session (minutes)"

*----- 6b. Total exposure to first counselor (in minutes) -----*

* Ensure first_counselor is filled for all rows within a woman
* Use [1] (first row after sorting) to propagate the correct first counselor
bysort caseid (counseling_date): replace first_counselor = first_counselor[1] ///
    if missing(first_counselor)

gen _minutes_same = cond(couns_id == first_counselor, session_total_min, 0)
bys caseid: egen minutes_same_counselor = total(_minutes_same)
label var minutes_same_counselor "Total minutes with same counselor (all visits)"
drop _minutes_same

*----- 6c. Quality indices -----*

* First-session quality index: equal-weighted standardized average of
* 4 components: satisfaction (1-4), topics requested (0-9),
* session length (minutes), and questions answered (0/1).
* NOTE: Q103 (topics) asks what women WANT to discuss, not what was
* discussed. The massive between-counselor variation (eta^2=0.90)
* confirms it still captures counselor behavior/administration style.
* All 4 components standardized to mean 0, SD 1, then averaged.
bys caseid (visit): gen byte _first_visit = (_n == 1)
foreach v in satisfaction_first_all n_topics_first_session session_first_all answered_first_all {
    quietly summarize `v' if _first_visit == 1
    gen _z_`v' = (`v' - r(mean)) / r(sd)
}
egen quality_index_first = rowmean( ///
    _z_satisfaction_first_all _z_n_topics_first_session ///
    _z_session_first_all _z_answered_first_all)
label var quality_index_first "Quality Index - First Session (4-component, equal-weighted)"
drop _first_visit _z_satisfaction_first_all _z_n_topics_first_session ///
    _z_session_first_all _z_answered_first_all

* Winsorize quality_index_first at 1st and 99th percentiles
* Caps extreme values to reduce influence of outliers on CQT and HQ classification
quietly summarize quality_index_first, detail
local p1  = r(p1)
local p99 = r(p99)
replace quality_index_first = `p1'  if quality_index_first < `p1'  & !missing(quality_index_first)
replace quality_index_first = `p99' if quality_index_first > `p99' & !missing(quality_index_first)
di "Winsorized quality_index_first at p1=" `p1' " and p99=" `p99'

* Visit-level quality index (same 3 components, measured at each session)
swindex questions_answered_n counseling_satisfaction_n session_total_min, ///
    generate(quality_index_visit)
label var quality_index_visit "Quality Index at each counseling visit"

* Average quality across ALL sessions per woman
bys caseid: egen avg_quality_all_sessions = mean(quality_index_visit)
egen avg_quality_std = std(avg_quality_all_sessions)

* Winsorize at 1st and 99th percentiles to limit outlier influence
winsor2 avg_quality_all_sessions, cuts(1 99) suffix(_trim)
label var avg_quality_all_sessions_trim "Average Counseling Quality - All Sessions (winsorized)"

* ----- 6c.bis. 4-component per-session quality index (DAC revision May 2026) --
* Mahesh requested switching primary endogenous variable from first-session
* to all-session quality. For consistency with the 4-component first-session
* index (sat + topics + duration + answered), we build a 4-component
* per-session index using the same components, with z-scores standardized
* on the first-visit means/SDs (so the all-session and first-session indices
* share the same scale).
bys caseid (visit): gen byte _is_first_4c = (_n == 1)
foreach v in counseling_satisfaction_n _n_topics_session session_total_min questions_answered_n {
    quietly summarize `v' if _is_first_4c == 1
    capture drop _z_4c_`v'
    gen _z_4c_`v' = (`v' - r(mean)) / r(sd) if r(sd) > 0
}
egen quality_index_visit_4c = rowmean( ///
    _z_4c_counseling_satisfaction_n _z_4c__n_topics_session ///
    _z_4c_session_total_min _z_4c_questions_answered_n)
label var quality_index_visit_4c "4-component per-session quality (sat+topics+dur+ans, std on 1st-visit scale)"

* Winsorize at p1/p99 (matching first-session winsorization)
quietly summarize quality_index_visit_4c, detail
local p1_v4c  = r(p1)
local p99_v4c = r(p99)
replace quality_index_visit_4c = `p1_v4c'  if quality_index_visit_4c < `p1_v4c'  & !missing(quality_index_visit_4c)
replace quality_index_visit_4c = `p99_v4c' if quality_index_visit_4c > `p99_v4c' & !missing(quality_index_visit_4c)
di "Winsorized quality_index_visit_4c at p1=" `p1_v4c' " and p99=" `p99_v4c'

* Woman-level mean (across all sessions for that woman)
bys caseid: egen avg_quality_4c = mean(quality_index_visit_4c)
label var avg_quality_4c "Mean 4-component quality across all sessions per woman"

* ----- 6c.ter. Satisfaction-only quality (PRIMARY May 2026) -----
* Following Cohen + Karra feedback (2026-05-07): use patient satisfaction (1-4)
* alone as the main quality measure. Satisfaction has the highest between-
* counselor variation (eta^2=0.599) among the four candidate components and is
* directly interpretable. No standardization or winsorization needed: the 1-4
* ordinal scale is already bounded. Topics is retained for the sat+topics
* robustness pair below; duration and questions answered are dropped from the
* main spec because their between-counselor eta^2 are 0.004 and 0.033.
*
* `quality_index_first_sat` is just satisfaction at the first visit (verbatim).
* `avg_quality_sat` is the woman-level mean across sessions (continuous, 1-4).

capture drop quality_index_first_sat
gen quality_index_first_sat = satisfaction_first_all
label var quality_index_first_sat "First-session satisfaction (1-4) [PRIMARY quality measure]"

capture drop avg_quality_sat
bys caseid: egen avg_quality_sat = mean(counseling_satisfaction_n)
label var avg_quality_sat "Mean satisfaction across all sessions per woman (1-4) [PRIMARY]"

* ----- 6c.quat. Satisfaction + topics-ENGAGED quality (PRIMARY, May 2026) -----
* The "engaged" topics component gates the topic count on whether the
* counselor answered all the woman's questions at that session. The gating
* parallels the construction of patient-centred counseling sub-indices in
* Wagner et al. (2025, EJ) and serves as an engagement check on top of the
* simple topic count: a session in which the counselor did not answer all
* questions does not deliver substantive topic content even if the checklist
* was nominally covered. The two components (satisfaction, engaged topics)
* are z-scored on first-visit means/SDs and winsorized at p1/p99 before
* averaging, matching the 4-comp construction.

* Per-session topics-engaged: topic count * indicator that all Qs answered
capture drop _topics_eng_session
gen _topics_eng_session = _n_topics_session * (questions_answered_n == 1) ///
    if !missing(_n_topics_session, questions_answered_n)
label var _topics_eng_session "Per-session topics requested (gated on answered=1)"

* First-session topics-engaged (woman-level: value at first session)
capture drop n_topics_eng_first
gen n_topics_eng_first = n_topics_first_session * (answered_first_all == 1) ///
    if !missing(n_topics_first_session, answered_first_all)
label var n_topics_eng_first "First-session topics requested (gated on answered=1)"

* First-session sat+topics-engaged index (z-scored on first-visit, winsorized)
foreach v in satisfaction_first_all n_topics_eng_first {
    quietly summarize `v' if _is_first_4c == 1
    capture drop _z_st_`v'
    gen _z_st_`v' = (`v' - r(mean)) / r(sd) if r(sd) > 0
}
capture drop quality_index_first_sattop
egen quality_index_first_sattop = rowmean( ///
    _z_st_satisfaction_first_all _z_st_n_topics_eng_first)
label var quality_index_first_sattop "First-session sat+topics-engaged quality (PRIMARY)"

quietly summarize quality_index_first_sattop, detail
local p1_st  = r(p1)
local p99_st = r(p99)
replace quality_index_first_sattop = `p1_st'  if quality_index_first_sattop < `p1_st'  & !missing(quality_index_first_sattop)
replace quality_index_first_sattop = `p99_st' if quality_index_first_sattop > `p99_st' & !missing(quality_index_first_sattop)
di "Winsorized quality_index_first_sattop at p1=" `p1_st' " and p99=" `p99_st'

* Per-session sat+topics-engaged index (z-scored on first-visit scale)
* Use short alias names (_zV1, _zV2) for the per-session z-scored vars to
* stay within Stata's 32-char variable-name limit.
quietly summarize counseling_satisfaction_n if _is_first_4c == 1
capture drop _zV1
gen _zV1 = (counseling_satisfaction_n - r(mean)) / r(sd) if r(sd) > 0

capture confirm variable _topics_eng_session
local has_topics_eng = (_rc == 0)
if `has_topics_eng' {
    quietly summarize _topics_eng_session if _is_first_4c == 1
    capture drop _zV2
    gen _zV2 = (_topics_eng_session - r(mean)) / r(sd) if r(sd) > 0
}

capture drop quality_index_visit_sattop
if `has_topics_eng' {
    egen quality_index_visit_sattop = rowmean(_zV1 _zV2)
    label var quality_index_visit_sattop "Per-session sat+topics-engaged quality (PRIMARY)"

    quietly summarize quality_index_visit_sattop, detail
    local p1_stv  = r(p1)
    local p99_stv = r(p99)
    replace quality_index_visit_sattop = `p1_stv'  if quality_index_visit_sattop < `p1_stv'  & !missing(quality_index_visit_sattop)
    replace quality_index_visit_sattop = `p99_stv' if quality_index_visit_sattop > `p99_stv' & !missing(quality_index_visit_sattop)

    capture drop avg_quality_sattop
    bys caseid: egen avg_quality_sattop = mean(quality_index_visit_sattop)
    label var avg_quality_sattop "Mean sat+topics-engaged quality across all sessions per woman (PRIMARY)"
}

capture drop _zV1 _zV2
drop _is_first_4c _z_4c_* _z_st_* _n_topics_session

*----- 6d. Quality diagnostics -----*

* Flag very low average quality women for inspection
gen low_avgQ = (avg_quality_all_sessions < -2)
tab low_avgQ
summarize questions_answered_n counseling_satisfaction_n session_total_min if low_avgQ == 1
summarize questions_answered_n counseling_satisfaction_n session_total_min if low_avgQ == 0
list caseid year first_counselor quality_index_visit ///
     questions_answered_n counseling_satisfaction_n session_total_min ///
     if low_avgQ == 1, sepby(first_counselor)
drop low_avgQ

* Component distributions
tab questions_answered_n, missing
tab counseling_satisfaction_n, missing
summarize session_total_min, detail

*----- 6e. First-visit scheduling variables (needed for CQT residualization) --*
* These extract day-of-week, month, and hour from the first counseling session.
* They are used as fixed effects when residualizing quality to construct the CQT.
* Must be created BEFORE Part 6 (CQT construction).

bys caseid (counseling_date): gen first_couns_dow      = dow(counseling_date[1])
bys caseid (counseling_date): gen first_couns_month    = month(counseling_date[1])
bys caseid (counseling_date): gen first_couns_hour     = hh(counseling_time[1])
bys caseid (counseling_date): gen first_couns_date_val = counseling_date[1]
format first_couns_date_val %td

label var first_couns_dow      "Day of week of first counseling session"
label var first_couns_month    "Month of first counseling session"
label var first_couns_hour     "Hour of day of first counseling session"
label var first_couns_date_val "Date of first counseling session"


********************************************************************************
* PART 7: CONSTRUCT CQT INSTRUMENT (Counselor Quality Propensity)
*
* Identification strategy: We exploit quasi-random assignment of women to one
* of six counselors at their first visit. The CQT is a leave-one-out (LOO)
* estimator of counselor quality tendency, following the judge/examiner design
* literature (e.g., Stevenson 2018, Arnold et al. 2018).
*
* Step 1: Residualize first-session quality on scheduling fixed effects
*         (day-of-week, month, hour of first visit). This removes predictable
*         quality variation due to timing. Only the regression residuals are
*         used -- SEs are irrelevant at this step since we only need residuals,
*         not inference.
*
* Step 2: For each woman i assigned to counselor j, compute:
*         CQT_i = (sum of residuals for all OTHER women with j) / (N_j - 1)
*         This provides exogenous variation in expected counseling quality.
*
* Note: Counselor 7 is excluded because they had very few patients, making
*       the LOO estimator unreliable for that counselor.
********************************************************************************

* Drop counselor 7 before constructing the instrument
drop if first_counselor == 7

* Tag exactly one row per woman (first year in panel)
bysort caseid (year): gen byte _tag_woman = (_n == 1)

* Define the residualization sample: tagged rows with non-missing key variables
gen byte _insample = _tag_woman == 1 ///
    & !missing(quality_index_first, first_counselor, ///
               first_couns_dow, first_couns_month, first_couns_hour)

* Step 1: Residualize first-session quality
reg quality_index_first i.first_couns_dow i.first_couns_month i.first_couns_hour if _insample, vce(robust)
predict double qresid_first if _insample, resid
label var qresid_first "Residualized first-session quality (scheduling FEs)"

* Step 2: Leave-one-out CQT
bysort first_counselor: egen double _sum_resid = total(qresid_first * _insample)
bysort first_counselor: egen double _n_women   = total(_insample)

gen double cqt_resid_loo = ///
    (_sum_resid - qresid_first) / (_n_women - 1) ///
    if _insample & _n_women > 1
label var cqt_resid_loo "CQT (residualized LOO mean first-session quality)"

* Carry CQT to all rows/years within each woman
bysort caseid: egen double _CQT_all = max(cqt_resid_loo)
replace cqt_resid_loo = _CQT_all

* Diagnostic: verify CQT varies across counselors
tab first_counselor, sum(cqt_resid_loo)

* ----- Additional CQT instruments (May 2026 spec change) ----------------------
* Build CQT for the satisfaction-only main spec and the sat+topics robustness,
* using the SAME residualization-then-LOO procedure but with the new first-
* session quality measures.
foreach idx in sat sattop {
    di _n as text "==== Building cqt_resid_loo_`idx' ===="

    capture drop _insample_`idx'
    gen byte _insample_`idx' = _tag_woman == 1 ///
        & !missing(quality_index_first_`idx', first_counselor, ///
                   first_couns_dow, first_couns_month, first_couns_hour)

    * Step 1: Residualize first-session quality on scheduling FEs
    capture drop qresid_first_`idx'
    quietly reg quality_index_first_`idx' i.first_couns_dow i.first_couns_month i.first_couns_hour ///
        if _insample_`idx', vce(robust)
    predict double qresid_first_`idx' if _insample_`idx', resid

    * Step 2: Leave-one-out CQT
    capture drop _sum_resid_`idx' _n_women_`idx'
    bysort first_counselor: egen double _sum_resid_`idx' = total(qresid_first_`idx' * _insample_`idx')
    bysort first_counselor: egen double _n_women_`idx'   = total(_insample_`idx')

    capture drop cqt_resid_loo_`idx'
    gen double cqt_resid_loo_`idx' = ///
        (_sum_resid_`idx' - qresid_first_`idx') / (_n_women_`idx' - 1) ///
        if _insample_`idx' & _n_women_`idx' > 1

    * Carry to all rows for that woman
    capture drop _CQT_all_`idx'
    bysort caseid: egen double _CQT_all_`idx' = max(cqt_resid_loo_`idx')
    replace cqt_resid_loo_`idx' = _CQT_all_`idx'

    drop _insample_`idx' _sum_resid_`idx' _n_women_`idx' _CQT_all_`idx' qresid_first_`idx'
}

label var cqt_resid_loo_sat    "CQT (LOO residualized) satisfaction [PRIMARY May 2026]"
label var cqt_resid_loo_sattop "CQT (LOO residualized) sat+topics [ROBUSTNESS May 2026]"

di _n as text "==== CQT diagnostics by counselor ===="
di "cqt_resid_loo (4-comp):"
tab first_counselor, sum(cqt_resid_loo)
di "cqt_resid_loo_sat (satisfaction only):"
tab first_counselor, sum(cqt_resid_loo_sat)
di "cqt_resid_loo_sattop (sat+topics):"
tab first_counselor, sum(cqt_resid_loo_sattop)

* Clean up temporary variables
drop _tag_woman _insample _sum_resid _n_women _CQT_all qresid_first


********************************************************************************
* PART 7b: PRIVACY AND HOUSEHOLD PRESENCE VARIABLES
* Source: Tracking form Q112-Q114 (post-session questions)
*   Q112: "Was anyone else present in the household during the counselling session?"
*   Q113: "Who else was present?" (husband=1, child=2, relative=3, friend=4, neighbor=5)
*   Q114: "Did the woman's husband/partner participate in the counselling session?"
********************************************************************************

* Convert string responses to binary indicators
capture drop anyone_present
gen anyone_present = (couns_track_112 == "1") ///
    if couns_track_112 != "---" & !missing(couns_track_112) & couns_track_112 != ""
label var anyone_present "Anyone else present during session (0/1)"

capture drop husband_present
gen husband_present = (couns_track_1131 == "1") ///
    if !missing(anyone_present)
replace husband_present = 0 if anyone_present == 0
label var husband_present "Husband/partner present during session"

capture drop husband_participated
gen husband_participated = (couns_track_114 == "1") ///
    if couns_track_114 != "---" & !missing(couns_track_114) & couns_track_114 != ""
replace husband_participated = 0 if anyone_present == 0
label var husband_participated "Husband participated in session"

* First-session versions (for heterogeneity analysis)
bys caseid (visit): gen anyone_present_first = anyone_present[1]
bys caseid (visit): gen husband_present_first = husband_present[1]
bys caseid (visit): gen husband_participated_first = husband_participated[1]
label var anyone_present_first "Anyone present at first session"
label var husband_present_first "Husband present at first session"
label var husband_participated_first "Husband participated at first session"

* Diagnostics
tab anyone_present_first, missing
tab husband_present_first if anyone_present_first == 1, missing
tab husband_participated_first if anyone_present_first == 1, missing


********************************************************************************
* PART 8: COLLAPSE TO CASEID x YEAR AND MERGE WITH SURVEY PANEL
* Purpose: Collapse session-level data to one row per woman-year, then merge
*          with the main analysis dataset to combine counseling measures with
*          survey outcomes and baseline covariates.
********************************************************************************

*----- 8a. Endline observation count (before collapse) -----*

bys caseid: gen _tag = (_n == 1)
bys caseid: egen _has_endline = max(!missing(date_int2018))
count if _has_endline == 1 & _tag == 1
* Expected: 742
drop _tag _has_endline

* Drop intermediate variables that should not enter the collapse
drop first_session_date followup_days

*----- 8b. Collapse to caseid x year -----*

collapse ///
    (first) days_to_second avg_days_between median_days_between visit_rate_mo ///
            topic_methods topic_preg topic_spacing topic_sidefx topic_services ///
            topic_sti topic_partner topic_child topic_breast quality_index_first ///
            first_counselor total_sessions counseling_date couns_id ///
            pregnant_first fp_use_first session_first_all ///
            avg_sessions_per_couns share_same_counselor ///
            avg_quality_std avg_quality_all_sessions_trim ///
            satisfaction_first_all answered_first_all ///
            visits_same_counselor minutes_same_counselor same_counselor_dummy ///
            months_since_first_final date_int2016 date_int2017 date_int2018 ///
            interval first_couns_dow first_couns_month first_couns_hour ///
            first_couns_date_val cqt_resid_loo ///
            only_one_session had_second_session ///
            ever_topic_fp ever_topic_spacing ever_topic_preg ///
            ever_topic_partner ever_topic_child ever_topic_sti ///
            ever_topic_childhealth ever_topic_breast ///
            etp1_fp etp1_spacing etp1_preg ///
            etp1_partner etp1_childhealth etp1_breast etp1_sti ///
            n_topics_first_session n_topics_avg_all ///
            anyone_present_first husband_present_first husband_participated_first ///
            avg_quality_4c ///
            quality_index_first_sat avg_quality_sat ///
            quality_index_first_sattop avg_quality_sattop ///
            n_topics_eng_first ///
            cqt_resid_loo_sat cqt_resid_loo_sattop ///
    (mean)  quality_index_visit quality_index_visit_4c gap_days anyone_present ///
    , by(caseid year)

save "${dta}/counseling_collapse_data.dta", replace

tab year
duplicates report caseid

*----- 8c. Merge with survey panel data -----*

merge 1:1 caseid year using "${dta}ANALYSIS_w1_w2_w3_2023-09-26.dta"

* NOTE: We keep ALL observations (treatment + control) in the merged dataset.
* Sample restriction to treatment==1 is applied at runtime in analysis do-files.
* This preserves the full sample for any future analyses that may need controls.

* Label key variables from both datasets
label var first_counselor              "Counselor"
label var answered_first_all           "All questions answered"
label var quality_index_first          "Quality Index First Session"
label var pregnant_first               "Pregnant at First Counseling"
label var fp_use_first                 "FP use at First Counseling"
label var session_first_all            "Length counseling session"
label var satisfaction_first_all       "Counseling satisfaction"
label var avg_sessions_per_couns       "Average number of visits per counselor"
label var visits_same_counselor        "Number of visits with the same counselor"
label var preg_2016                    "Pregnant at Baseline"
label var curr_use_bl                  "FP use at Baseline"
label var quality_index_visit          "Mean Quality Index at each counseling visit"
label var avg_quality_all_sessions_trim "Average Counseling Quality (All Sessions)"
label var share_same_counselor         "Share of sessions with first counselor"
label var months_since_first_final     "Months between first counseling session and 2018 interview"
label var avg_days_between             "Mean days between counseling sessions"
label var median_days_between          "Median days between counseling sessions"
label var visit_rate_mo                "Sessions per month over follow-up window"
label var days_to_second               "Days from 1st to 2nd counseling session"

* Rename and label survey panel variables used in analysis
rename w3_trt_10 satisfaction_survey
label var satisfaction_survey    "Counseling satisfaction (from surveys)"
label var implant                "Implant use"
label var ever_use_bl            "FP ever use - BL"
label var more_eff_method        "Long-Acting Method"
label var next_preg_2018         "Pregnancy since index birth"
label var next_birth_2018        "Birth since index birth"
label var curr_use_inj_adj_bl    "Injectable use BL"
label var area_bl                "Neighborhood (area code)"
label var work_bl                "Work status"
label var sex_age_bl             "Age at FSR"
label var curr_use_inj_adj       "Current use of FP"
label var implant_bl             "Implant - BL"
label var total_alive_bl         "Number of children alive"
label var age_group_bl           "Age group"
label var edu_primary_bl         "Education level"
label var religion_r_bl          "Religion"
label var ethnicity_r_bl         "Ethnicity"

sort caseid year
order caseid year

*---------------------------------------------------------------
* 8e. Fill counseling-derived vars across all years within caseid
* Purpose: The merge produces missing values for 2016 (baseline) rows
* because counseling data only exists for 2017/2018. Forward-fill and
* backward-fill so that woman-level constants (e.g., first_counselor,
* quality_index_first) are available on all rows.
*---------------------------------------------------------------

local vars days_to_second avg_days_between median_days_between visit_rate_mo ///
           topic_methods topic_preg topic_spacing topic_sidefx topic_services ///
           topic_sti topic_partner topic_child topic_breast quality_index_first ///
           first_counselor total_sessions counseling_date couns_id ///
           pregnant_first fp_use_first session_first_all ///
           avg_sessions_per_couns share_same_counselor ///
           avg_quality_std avg_quality_all_sessions_trim ///
           satisfaction_first_all answered_first_all ///
           visits_same_counselor minutes_same_counselor same_counselor_dummy ///
           months_since_first_final date_int2016 date_int2017 date_int2018 ///
           interval first_couns_dow first_couns_month first_couns_hour ///
           first_couns_date_val quality_index_visit cqt_resid_loo ///
           only_one_session had_second_session ///
           ever_topic_fp ever_topic_spacing ever_topic_preg ///
           ever_topic_partner ever_topic_child ever_topic_sti ///
           ever_topic_childhealth ever_topic_breast ///
           etp1_fp etp1_spacing etp1_preg ///
           etp1_partner etp1_childhealth etp1_breast etp1_sti ///
           n_topics_first_session n_topics_avg_all ///
           quality_index_first_sat avg_quality_sat ///
           quality_index_first_sattop avg_quality_sattop ///
           cqt_resid_loo_sat cqt_resid_loo_sattop

* Forward fill: carry earlier known value to later missing years
foreach v of local vars {
    bysort caseid (year): replace `v' = `v'[_n-1] if missing(`v') & _n > 1
}

* Backward fill: carry later known value to earlier missing years
gsort caseid -year
foreach v of local vars {
    by caseid: replace `v' = `v'[_n-1] if missing(`v') & _n > 1
}

* Restore natural ordering
sort caseid year

********************************************************************************
* SAVE FINAL DATASET
********************************************************************************

save "${dta}/merge_counselor_outcomes.dta", replace

log close

********************************************************************************
* POST-PROCESSING: Build take-up indicators for ancillary intervention components
* (transport voucher and clinic reimbursement). Adds received_transport,
* received_reimbursement, received_any_ancillary, n_transport_trips, and
* n_reimbursement_visits as woman-level constants on every panel row.
* See dofile/build_takeup_indicators.do for details.
********************************************************************************
do "${dofile}build_takeup_indicators.do"
