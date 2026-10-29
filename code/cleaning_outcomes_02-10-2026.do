****************************************************************************************
* CLEANING DOFILE: OUTCOMES DATA
* Paper: Causal Evidence of Counseling Quality on FP/Reproductive Outcomes
*        (Judge-Design IV, Malawi RCT)
*
* CREATED:  Marie Montas, 11 July 2025
* UPDATED:  Marie Montas, 11 Feb 2026
*
* INPUTS:   ${dta}/merge_counselor_outcomes.dta
* OUTPUTS:  ${dta}/merge_counselor_outcomes.dta  (overwritten)
*
* STRUCTURE:
*   1. Setup and paths
*   2. Fertility preferences (empowerment)
*   3. Pregnancy and birth outcomes (comprehensive)
*   4. Unintended pregnancy measures (ex-post and ex-ante)
*   5. Change in children alive (roster-based robustness)
*   6. Diagnostics: old vs new pregnancy measures
*   7. Other empowerment and FP outcomes
*   7b. Bargaining power / decision-making (Q823-Q826)
*   8. Contraception duration and adoption
*   9. FP knowledge and pregnancy attitudes
*  10. Health utilization, breastfeeding, sexual activity
*  11. Misc outcomes / FP source / distance
*  12. Admin FP from reimbursement log + coverage + baseline timing (May 2026)
*  13. Save
****************************************************************************************

clear all
set more off

*===============================================================================
* 1. SETUP AND PATHS
*===============================================================================

if "$MFPS_ROOT" == "" global MFPS_ROOT "`c(pwd)'"  // project folder (holds Analysis/)
if inlist("`c(os)'", "MacOSX", "Unix") {
    global dta    "${MFPS_ROOT}/Analysis/dta/"
    global dofile "${MFPS_ROOT}/Analysis/dofile/"
    global tables "${MFPS_ROOT}/Analysis/tables/"
    global figures "${MFPS_ROOT}/Analysis/figures/"
    global log    "${MFPS_ROOT}/Analysis/log/"
}

capture log close
log using "${log}cleaning_outcomes_02-10-2026.log", replace text

use "${dta}/merge_counselor_outcomes.dta", clear

* --- Forward-fill woman-level quality constants across all panel years ---
* Some woman-level quality variables are built at the session level and only
* end up populated for panel years where sessions occurred (typically 2017,
* 2018), leaving year=2016 rows missing. Propagate to all panel years here.
* (See May 2026 fix; previously only avg_quality_sat/sattop were propagated.)
foreach v in avg_quality_4c high_quality_avg_all ///
             avg_quality_sat high_quality_avg_sat ///
             avg_quality_sattop high_quality_avg_sattop ///
             cqt_resid_loo cqt_resid_loo_sat cqt_resid_loo_sattop ///
             quality_index_first quality_index_first_sat quality_index_first_sattop ///
             first_counselor counselLP_bl {
    capture confirm variable `v'
    if !_rc {
        tempvar _ff
        bys caseid: egen `_ff' = max(`v')
        replace `v' = `_ff' if missing(`v')
        drop `_ff'
    }
}

* --- Drop variables that will be recreated below (idempotent re-runs) ---
* Section 2: Fertility preferences
capture drop fertility_pref fertility_pref_bl
* Section 3: Pregnancy and birth
capture drop preg_now preg_loss n_preg_losses any_preg_comprehensive
capture drop months_to_next_birth months_to_next_preg
capture drop birth_within_12mo birth_within_24mo
capture drop preg_within_12mo preg_within_24mo
capture drop preg_0_6mo preg_6_12mo preg_12_18mo preg_18_24mo
* Section 4: Unintended pregnancy
capture drop unwanted_recent_birth mistimed_recent_birth unwanted_nomore_birth
capture drop unwanted_current_preg mistimed_current_preg unwanted_nomore_current_preg
capture drop unintended_preg_expost mistimed_preg_expost unwanted_nomore_expost
capture drop no_want_child_bl any_preg_followup unintended_preg_exante
capture drop _max_unint_expost _max_mist _max_unm _max_unint_exante
* Section 5: Change in children
capture drop delta_children had_new_child
* Section 7: Empowerment
capture drop husband_wants_children same_number_children
capture drop husband_knows_fp decision_contraception discussed_fp_husband
* Section 7b: Bargaining
foreach v in w8_w823_binary w8_w824_binary w8_w825_binary w8_w826_binary {
    capture drop `v'
}
* Section 8: Contraception duration + new horizon outcomes
capture drop fp_start_date baseline_date fp_start_effective
capture drop interview_date months_fp_use_post
capture drop baseline_user continued_fp new_adopter
capture drop months_to_fp_start new_adopter_1yr new_adopter_2yr
capture drop larc_uptake_1yr larc_uptake_2yr _larc_2017 _larc_2018
* Section 9: FP knowledge
capture drop preg_likely_nofp preg_likely_fp preg_feel_positive
* Section 6: Diagnostic variable
capture drop difference_up
* Section 10: Health/breastfeeding
capture drop breastfed

*===============================================================================
* 2. FERTILITY PREFERENCES (EMPOWERMENT)
*===============================================================================

* --- Want more children: non-pregnant women (w704) ---
capture rename w7_w704 fertility_pref_nonpregnant
replace fertility_pref_nonpregnant = 0 ///
    if inlist(fertility_pref_nonpregnant, 2, 3, 88)
label var fertility_pref_nonpregnant "Want more children - Non-pregnant"

* --- Want more children: pregnant women (w703) ---
capture rename w7_w703 fertility_pref_pregnant
replace fertility_pref_pregnant = 0 ///
    if inlist(fertility_pref_pregnant, 2, 88)
label var fertility_pref_pregnant "Want more children - Pregnant"

* --- Combined fertility preference (binary) ---
* NOTE: fertility_pref already exists in the dataset from the original panel.
*       We drop it first, then recreate with the combined measure.
capture drop fertility_pref
gen fertility_pref = 1 if fertility_pref_nonpregnant == 1 | fertility_pref_pregnant == 1
replace fertility_pref = 0 if fertility_pref_nonpregnant == 0 | fertility_pref_pregnant == 0
label var fertility_pref "Wants more children (combined)"

* --- Baseline fertility preference (for heterogeneity analysis) ---
* Carry baseline value forward to all waves to avoid post-treatment bias.
* Use cond() to avoid the zero/missing ambiguity: for non-2016 rows, cond()
* returns missing (not 0), so max() ignores them correctly.
capture drop _fertpref_bl_temp fertility_pref_bl
bys caseid: egen _fertpref_bl_temp = max(cond(year == 2016, fertility_pref, .))
gen fertility_pref_bl = _fertpref_bl_temp
drop _fertpref_bl_temp
label var fertility_pref_bl "Wants more children at baseline (2016)"

*===============================================================================
* 3. PREGNANCY AND BIRTH OUTCOMES (COMPREHENSIVE)
* The dataset already contains comprehensive pregnancy/birth variables from the
* birth history roster (Survey Section 2, Q201-Q224) and pregnancy calendar.
* These were constructed by the original study team and used in Karra et al.
* (PNAS 2022). We use them as our primary outcomes.
*
* VARIABLES USED:
*   next_preg_2018  = Any new pregnancy since index birth, before 2018 endline
*                     (from birth history + pregnancy calendar; captures ALL
*                      pregnancies including those that ended before interview)
*   next_birth_2018 = Any new live birth since index birth, before 2018 endline
*   w2_w226         = Currently pregnant at interview (midline/endline)
*   w2_w228         = Wanted current pregnancy? (1=yes, 2=no) [asked if pregnant]
*   w2_w229         = Wanted later or no more? (1=later, 2=no more) [if w228=2]
*   w4_w405         = Wanted pregnancy with last child? [asked for most recent
*                     birth, captures COMPLETED pregnancies, not just current]
*   w4_w406         = Wanted later or no more? (1=later, 2=no more)
*   w2_w230         = Ever had miscarriage/abortion/stillbirth since last birth
*   wantanother_bl  = Wanted another child at baseline (1=yes)
*   fertility_pref  = Wants more children (binary, all waves)
*===============================================================================

* --- OUTCOME 1: Any subsequent pregnancy (comprehensive) ---
* Uses next_preg_2018 from birth history roster + pregnancy calendar.
* Captures ALL pregnancies -- ongoing, delivered, or terminated --
* not just those coinciding with the interview date.
label var next_preg_2018 "Any pregnancy since index birth (before 2018 endline)"
label var next_preg_2017 "Any pregnancy since index birth (before 2017 midline)"
tab next_preg_2018 if year == 2018, missing

* --- OUTCOME 2: Any subsequent live birth ---
* Uses next_birth_2018 from the birth history roster.
* Captures all live births between baseline and endline.
label var next_birth_2018 "Any live birth since index birth (before 2018 endline)"
label var next_birth_2017 "Any live birth since index birth (before 2017 midline)"
tab next_birth_2018 if year == 2018, missing

* --- OUTCOME 2b: Short-term birth (within 12 mo and 24 mo of baseline) ---
* New short-term reproductive outcomes for the May 2026 spec change. Built from
* `next_birth_date` (Stata internal date, days since 1jan1960) and the woman's
* baseline interview date (`date_int2016`).
*
* `next_birth_date` is non-missing only for women who had a recorded next birth
* (~22 women out of 757 endline). Women with `next_birth_2018 == 0` have no
* recorded next birth, so they are observed 0 for both 12-mo and 24-mo (they
* have ~24 mo of follow-up by endline).
*
* Censoring rule: a woman is observed only if `next_birth_2018` is non-missing.
* Since baseline is mid-2016 and endline is mid-2018, all observed women have
* at least 24 months of follow-up.

* ----- Anchor: months are measured FROM INDEX BIRTHDATE (matches PNAS) -------
* This is the standard birth-spacing measure used in Karra et al. (2022, PNAS).
* `index_birth_date` is the date of the woman's index reproductive event:
*   - Postpartum women: youngest child's birth date at 2016 baseline
*   - Pregnant women: delivery date of the baseline pregnancy (occurred 2016/17)
* By construction (Muqi Guo's survival pipeline), `next_birth_date` and
* `next_preg_date` exclude the index event itself.

capture drop months_to_next_birth
gen double months_to_next_birth = (next_birth_date - index_birth_date) / 30.4375 ///
    if !missing(next_birth_date) & !missing(index_birth_date)
label var months_to_next_birth "Months from index birth to next live birth"

* birth_within_12mo:
*   - 1 if next birth occurred within 12 months of index birth
*   - 0 if no next birth was observed (next_birth_2018 == 0)
*   - 0 if next birth occurred > 12 months after index birth
*   - MISSING if next_birth_2018 == 1 but index_birth_date is missing
*   - MISSING if next_birth_2018 itself is missing (censored attriter)

capture drop birth_within_12mo
gen byte birth_within_12mo = 0 if next_birth_2018 == 0
replace  birth_within_12mo = 1 if !missing(months_to_next_birth) & months_to_next_birth <= 12
replace  birth_within_12mo = 0 if !missing(months_to_next_birth) & months_to_next_birth >  12
label var birth_within_12mo "Live birth within 12 months of index birth"

capture drop birth_within_24mo
gen byte birth_within_24mo = 0 if next_birth_2018 == 0
replace  birth_within_24mo = 1 if !missing(months_to_next_birth) & months_to_next_birth <= 24
replace  birth_within_24mo = 0 if !missing(months_to_next_birth) & months_to_next_birth >  24
label var birth_within_24mo "Live birth within 24 months of index birth"

* --- Parallel pregnancy outcomes (May 2026 spec, anchored at index birth) -----
capture drop months_to_next_preg
gen double months_to_next_preg = (next_preg_date - index_birth_date) / 30.4375 ///
    if !missing(next_preg_date) & !missing(index_birth_date)
label var months_to_next_preg "Months from index birth to next pregnancy (matches PNAS)"

capture drop preg_within_12mo
gen byte preg_within_12mo = 0 if next_preg_2018 == 0
replace  preg_within_12mo = 1 if !missing(months_to_next_preg) & months_to_next_preg <= 12
replace  preg_within_12mo = 0 if !missing(months_to_next_preg) & months_to_next_preg >  12
label var preg_within_12mo "Subsequent pregnancy within 12 months of index birth"

capture drop preg_within_24mo
gen byte preg_within_24mo = 0 if next_preg_2018 == 0
replace  preg_within_24mo = 1 if !missing(months_to_next_preg) & months_to_next_preg <= 24
replace  preg_within_24mo = 0 if !missing(months_to_next_preg) & months_to_next_preg >  24
label var preg_within_24mo "Subsequent pregnancy within 24 months of index birth (matches PNAS event_next_preg_24)"

* --- Non-overlapping pregnancy-timing intervals for the interval-hazard IV ---
* (Replaces the earlier preg_within_6mo single-cutoff variable. Each indicator
* equals 1 iff the woman's first next pregnancy fell in that month range from
* index birth, and 0 otherwise. Together they decompose preg_within_24mo into
* four non-overlapping windows so an IV at each interval gives the time-varying
* causal effect of high-quality counseling on pregnancy timing.)

capture drop preg_0_6mo preg_6_12mo preg_12_18mo preg_18_24mo

gen byte preg_0_6mo   = 0 if next_preg_2018 == 0
replace  preg_0_6mo   = 1 if !missing(months_to_next_preg) & months_to_next_preg >   0 & months_to_next_preg <=  6
replace  preg_0_6mo   = 0 if !missing(months_to_next_preg) & (months_to_next_preg <=  0 | months_to_next_preg >  6)
label var preg_0_6mo   "Next pregnancy in months 0--6 from index birth"

gen byte preg_6_12mo  = 0 if next_preg_2018 == 0
replace  preg_6_12mo  = 1 if !missing(months_to_next_preg) & months_to_next_preg >   6 & months_to_next_preg <= 12
replace  preg_6_12mo  = 0 if !missing(months_to_next_preg) & (months_to_next_preg <=  6 | months_to_next_preg > 12)
label var preg_6_12mo  "Next pregnancy in months 6--12 from index birth"

gen byte preg_12_18mo = 0 if next_preg_2018 == 0
replace  preg_12_18mo = 1 if !missing(months_to_next_preg) & months_to_next_preg >  12 & months_to_next_preg <= 18
replace  preg_12_18mo = 0 if !missing(months_to_next_preg) & (months_to_next_preg <= 12 | months_to_next_preg > 18)
label var preg_12_18mo "Next pregnancy in months 12--18 from index birth"

gen byte preg_18_24mo = 0 if next_preg_2018 == 0
replace  preg_18_24mo = 1 if !missing(months_to_next_preg) & months_to_next_preg >  18 & months_to_next_preg <= 24
replace  preg_18_24mo = 0 if !missing(months_to_next_preg) & (months_to_next_preg <= 18 | months_to_next_preg > 24)
label var preg_18_24mo "Next pregnancy in months 18--24 from index birth"

* ----- June 2026 (referee comment 12): INCLUDE ALL SUBSEQUENT PREGNANCIES -------
* Previously we set pregnancy outcomes to MISSING for women with a recorded
* pregnancy termination (`termflag == 1`: miscarriage, abortion, or stillbirth),
* following Karra et al. (2022, PNAS) Table 2 Panel E. We no longer do this.
* Dropping women on the basis of a termination conditions the IV sample on a
* POST-TREATMENT event (whether a pregnancy terminates can itself be affected by
* counseling quality), and a termination is itself a subsequent pregnancy event,
* so excluding it is inconsistent with an "any subsequent pregnancy" outcome.
* The pregnancy outcomes therefore count ALL subsequent pregnancies, including
* those that terminated, on the same at-risk population as the birth outcomes.
* (Verified: including these women moves the IV estimates by <= 0.003 and flips
* no significance category.) mistimed_preg_expost was never termflag-filtered.

di _n as text "==== Pregnancy outcomes (ALL subsequent pregnancies, no termflag filter) ===="
foreach v in next_preg_2018 preg_within_12mo preg_within_24mo ///
             preg_0_6mo preg_6_12mo preg_12_18mo preg_18_24mo {
    quietly count if treatment == 1 & year == 2018 & !missing(`v')
    local n_obs = r(N)
    quietly count if treatment == 1 & year == 2018 & `v' == 1
    local n_one = r(N)
    quietly summarize `v' if treatment == 1 & year == 2018
    di "  `v' (treatment, year=2018): N=" `n_obs' ", events=" `n_one' ", mean=" %5.3f r(mean)
}

* Forward/backward fill so all woman-level outcomes are constant across years
foreach v in months_to_next_birth birth_within_12mo birth_within_24mo ///
             months_to_next_preg  preg_within_12mo  preg_within_24mo ///
             preg_0_6mo preg_6_12mo preg_12_18mo preg_18_24mo {
    bys caseid (year): replace `v' = `v'[_n-1] if missing(`v') & _n > 1
    gsort caseid -year
    bys caseid: replace `v' = `v'[_n-1] if missing(`v') & _n > 1
    sort caseid year
}

di _n as text "==== Short-term birth + pregnancy diagnostics (treatment, year=2018) ===="
foreach v in next_birth_2018 birth_within_12mo birth_within_24mo ///
             next_preg_2018 preg_within_12mo preg_within_24mo {
    quietly count if treatment == 1 & year == 2018 & !missing(`v')
    local n_obs = r(N)
    quietly count if treatment == 1 & year == 2018 & `v' == 1
    local n_one = r(N)
    quietly summarize `v' if treatment == 1 & year == 2018
    di "  `v': N=" `n_obs' ", events=" `n_one' ", mean=" %5.3f r(mean)
}

di _n "Cross-tab: birth_within_24mo vs next_birth_2018 (treatment, year=2018):"
tab birth_within_24mo next_birth_2018 if treatment == 1 & year == 2018, missing
di _n "Cross-tab: preg_within_24mo vs next_preg_2018 (treatment, year=2018):"
tab preg_within_24mo next_preg_2018 if treatment == 1 & year == 2018, missing

* --- OUTCOME 3: Currently pregnant at interview (comparison measure) ---
* This is the OLD measure -- useful as a secondary check but misses
* pregnancies that ended before interview.
capture rename w2_w226 pregnant_at_interview
capture drop preg_now
gen preg_now = (pregnant_at_interview == 1) if inlist(year, 2017, 2018)
label var preg_now "Pregnant at interview (midline/endline)"
tab preg_now year, missing

* --- OUTCOME 4: Pregnancy loss (miscarriage, abortion, stillbirth) ---
* Survey Q230: "Since [youngest child/last year], have you ever had
* a pregnancy that miscarried, was aborted, or ended in a stillbirth?"
* Captures between-wave pregnancy losses invisible to "pregnant now" or "live birth".
capture drop preg_loss
gen preg_loss = (w2_w230 == 1) if !missing(w2_w230)
label var preg_loss "Had miscarriage/abortion/stillbirth since last birth"
tab preg_loss year, missing

* Number of pregnancy losses (intensity measure)
capture drop n_preg_losses
gen n_preg_losses = w2_w230a if !missing(w2_w230a) & w2_w230 == 1
replace n_preg_losses = 0 if w2_w230 == 2
label var n_preg_losses "Number of pregnancy losses since last birth"

* --- OUTCOME 5: Any pregnancy, comprehensive (births + current + losses) ---
* Combines all sources to capture ALL conceptions between baseline and endline.
capture drop any_preg_comprehensive
gen any_preg_comprehensive = (next_preg_2018 == 1 | preg_loss == 1) ///
    if year == 2018
replace any_preg_comprehensive = 0 ///
    if any_preg_comprehensive == . & year == 2018 & !missing(next_preg_2018)
label var any_preg_comprehensive ///
    "Any pregnancy since index (births + current + losses)"
tab any_preg_comprehensive if year == 2018, missing

*===============================================================================
* 4. UNINTENDED PREGNANCY MEASURES
*===============================================================================

*---------------------------------------------------------------
* METHOD 1 (EX POST): Wantedness of most recent birth/pregnancy
*
* Two sources of wantedness information:
*   (A) w4_w405: "When you got pregnant with [last child], did you want
*       to get pregnant at that time?" -- asked about the MOST RECENT
*       BIRTH, capturing completed pregnancies (Section 4, Q405)
*   (B) w2_w228: "When you got pregnant, did you want to get pregnant
*       at that time?" -- asked only if CURRENTLY PREGNANT (Section 2, Q228)
*
* Using (A) as primary because it captures pregnancies that already
* resulted in a birth (not just those ongoing at interview).
* (B) supplements for women pregnant at interview who haven't delivered.
*---------------------------------------------------------------

* A) Wantedness of most recent birth (w4_w405): 1=yes, 2=no
capture drop unwanted_recent_birth
gen unwanted_recent_birth = (w4_w405 == 2) if !missing(w4_w405)
label var unwanted_recent_birth "Most recent birth was unwanted (Q405)"

* Sub-classify: wanted later vs. no more (w4_w406): 1=later, 2=no more
capture drop mistimed_recent_birth
gen mistimed_recent_birth = (w4_w406 == 1) ///
    if !missing(w4_w406) & unwanted_recent_birth == 1
capture drop unwanted_nomore_birth
gen unwanted_nomore_birth = (w4_w406 == 2) ///
    if !missing(w4_w406) & unwanted_recent_birth == 1
label var mistimed_recent_birth "Most recent birth: wanted later (mistimed)"
label var unwanted_nomore_birth "Most recent birth: wanted no more children"

* B) Wantedness of current pregnancy (w2_w228): 1=yes, 2=no
capture drop unwanted_current_preg
gen unwanted_current_preg = (w2_w228 == 2) if !missing(w2_w228)
label var unwanted_current_preg "Current pregnancy was unwanted (Q228)"

* Sub-classify current pregnancy: wanted later vs. no more (w2_w229): 1=later, 2=no more
capture drop mistimed_current_preg
gen mistimed_current_preg = (w2_w229 == 1) ///
    if !missing(w2_w229) & unwanted_current_preg == 1
capture drop unwanted_nomore_current_preg
gen unwanted_nomore_current_preg = (w2_w229 == 2) ///
    if !missing(w2_w229) & unwanted_current_preg == 1
label var mistimed_current_preg "Current pregnancy: wanted later (mistimed)"
label var unwanted_nomore_current_preg "Current pregnancy: wanted no more children"

* C) Combined ex-post unintended pregnancy:
*    Unintended = had a NEW pregnancy (next_preg_2018==1) AND that pregnancy
*    was unwanted (either via birth wantedness or current pregnancy wantedness).
*    NOTE: next_preg_2018 already captures only NEW pregnancies since the index
*    birth, so baseline-pregnant women are correctly handled — their index
*    pregnancy is not counted. They CAN have a new subsequent pregnancy.
*    Everyone in the treatment sample should get a 0 or 1.
capture drop unintended_preg_expost
gen unintended_preg_expost = .

* Women who had a birth and report it was unwanted
replace unintended_preg_expost = 1 ///
    if next_birth_2018 == 1 & unwanted_recent_birth == 1

* Women currently pregnant who didn't want it
replace unintended_preg_expost = 1 ///
    if preg_now == 1 & unwanted_current_preg == 1

* Women who had a pregnancy but it was wanted
replace unintended_preg_expost = 0 ///
    if next_preg_2018 == 1 & unintended_preg_expost == . ///
    & (unwanted_recent_birth == 0 | unwanted_current_preg == 0)

* Women with pregnancy but no wantedness data available → code as 0 (conservative)
replace unintended_preg_expost = 0 ///
    if next_preg_2018 == 1 & unintended_preg_expost == .

* Women with no new pregnancy during follow-up → 0
replace unintended_preg_expost = 0 ///
    if next_preg_2018 == 0 & preg_now != 1

* Carry to woman-level constant (spread across panel years)
bys caseid: egen _max_unint_expost = max(unintended_preg_expost)
replace unintended_preg_expost = _max_unint_expost
drop _max_unint_expost

label var unintended_preg_expost "Unintended pregnancy (ex post)"
tab unintended_preg_expost year, missing

*---------------------------------------------------------------
* MISTIMED PREGNANCY (EX POST): wanted later
* Sub-measure of unintended_preg_expost where reason = "wanted later"
* Uses w4_w406 (births) and w2_w229 (current pregnancies)
*---------------------------------------------------------------
capture drop mistimed_preg_expost
gen mistimed_preg_expost = .

* Births reported as mistimed (wanted later)
replace mistimed_preg_expost = 1 ///
    if next_birth_2018 == 1 & unwanted_recent_birth == 1 & w4_w406 == 1

* Current pregnancies reported as mistimed (wanted later)
replace mistimed_preg_expost = 1 ///
    if preg_now == 1 & unwanted_current_preg == 1 & w2_w229 == 1

* Unwanted but "no more" (not mistimed) → 0
replace mistimed_preg_expost = 0 ///
    if next_birth_2018 == 1 & unwanted_recent_birth == 1 & w4_w406 == 2
replace mistimed_preg_expost = 0 ///
    if preg_now == 1 & unwanted_current_preg == 1 & w2_w229 == 2

* Wanted pregnancy or no pregnancy → 0
replace mistimed_preg_expost = 0 ///
    if next_preg_2018 == 1 & unintended_preg_expost == 0
replace mistimed_preg_expost = 0 ///
    if next_preg_2018 == 0 & preg_now != 1

* Women with pregnancy but no wantedness/sub-classification data → code as 0
replace mistimed_preg_expost = 0 ///
    if next_preg_2018 == 1 & mistimed_preg_expost == .

* Spread across panel years
bys caseid: egen _max_mist = max(mistimed_preg_expost)
replace mistimed_preg_expost = _max_mist
drop _max_mist

label var mistimed_preg_expost "Mistimed pregnancy — wanted later (ex post)"
tab mistimed_preg_expost year, missing

*---------------------------------------------------------------
* UNWANTED PREGNANCY (EX POST): wanted no more children
* Sub-measure of unintended_preg_expost where reason = "no more"
* Uses w4_w406 (births) and w2_w229 (current pregnancies)
*---------------------------------------------------------------
capture drop unwanted_nomore_expost
gen unwanted_nomore_expost = .

* Births reported as unwanted — no more
replace unwanted_nomore_expost = 1 ///
    if next_birth_2018 == 1 & unwanted_recent_birth == 1 & w4_w406 == 2

* Current pregnancies reported as unwanted — no more
replace unwanted_nomore_expost = 1 ///
    if preg_now == 1 & unwanted_current_preg == 1 & w2_w229 == 2

* Unwanted but "later" (mistimed, not no-more) → 0
replace unwanted_nomore_expost = 0 ///
    if next_birth_2018 == 1 & unwanted_recent_birth == 1 & w4_w406 == 1
replace unwanted_nomore_expost = 0 ///
    if preg_now == 1 & unwanted_current_preg == 1 & w2_w229 == 1

* Wanted pregnancy or no pregnancy → 0
replace unwanted_nomore_expost = 0 ///
    if next_preg_2018 == 1 & unintended_preg_expost == 0
replace unwanted_nomore_expost = 0 ///
    if next_preg_2018 == 0 & preg_now != 1

* Women with pregnancy but no wantedness/sub-classification data → code as 0
replace unwanted_nomore_expost = 0 ///
    if next_preg_2018 == 1 & unwanted_nomore_expost == .

* Spread across panel years
bys caseid: egen _max_unm = max(unwanted_nomore_expost)
replace unwanted_nomore_expost = _max_unm
drop _max_unm

label var unwanted_nomore_expost "Unwanted pregnancy — no more children (ex post)"
tab unwanted_nomore_expost year, missing

*---------------------------------------------------------------
* METHOD 2 (EX ANTE): Baseline preference + any subsequent pregnancy
*
* Logic: A woman who stated at baseline that she did NOT want more
* children, but who subsequently became pregnant (any pregnancy,
* not just "pregnant at interview"), experienced an unintended pregnancy.
*
* Uses next_preg_2018 (comprehensive) instead of preg_now (point-in-time).
*---------------------------------------------------------------

* Baseline: did NOT want another child
* wantanother_bl: 1=wants another (from original dataset)
* fertility_pref at year==2016: 1=wants more, 0=does not want more
capture drop no_want_child_bl
bys caseid: egen no_want_child_bl = max( ///
    (wantanother_bl == 0 | fertility_pref == 0) & year == 2016)
label var no_want_child_bl "Baseline: did not want another child"

* Any pregnancy during follow-up (comprehensive, not point-in-time)
capture drop any_preg_followup
bys caseid: egen any_preg_followup = max(next_preg_2018 == 1)
replace any_preg_followup = 0 if missing(any_preg_followup)
label var any_preg_followup "Any pregnancy during follow-up (comprehensive)"

* Ex-ante unintended = didn't want more children + got pregnant again
* NOTE: any_preg_followup uses next_preg_2018 which captures only NEW
* pregnancies since the index birth. Baseline-pregnant women who get
* pregnant AGAIN are correctly captured. Those who don't get 0.
capture drop unintended_preg_exante
gen unintended_preg_exante = (no_want_child_bl == 1 & any_preg_followup == 1)
label var unintended_preg_exante "Unintended pregnancy (ex ante)"

* Carry to woman-level constant
bys caseid: egen _max_unint_exante = max(unintended_preg_exante)
replace unintended_preg_exante = _max_unint_exante
drop _max_unint_exante

tab unintended_preg_exante year, missing

*===============================================================================
* 5. CHANGE IN CHILDREN ALIVE (ROSTER-BASED ROBUSTNESS) -- LEGACY (May 19, 2026)
*
* Compares total children alive at endline vs baseline to capture net fertility
* change. This section produces `delta_children` and `had_new_child`, neither
* of which is referenced by any active result script. Wrapped in `if 0` so it
* does not execute. Kept in the file for future reactivation if needed.
*===============================================================================

if 0 {

gen delta_children = w2_total_children_alive - total_alive_bl ///
    if year == 2018 & !missing(w2_total_children_alive, total_alive_bl)
label var delta_children "Change in children alive (endline - baseline)"

gen had_new_child = (delta_children > 0) if year == 2018
label var had_new_child "Had at least one new child (endline - baseline > 0)"
tab had_new_child if year == 2018, missing

}

*===============================================================================
* 6. DIAGNOSTICS: OLD VS NEW PREGNANCY MEASURES -- LEGACY (May 19, 2026)
*
* Diagnostic tabs and t-tests only; produces no new variables. Wrapped in
* `if 0` so it does not execute.
*===============================================================================

if 0 {

di _n "=== Comparison: next_preg_2018 vs preg_now (at endline) ==="
tab next_preg_2018 preg_now if year == 2018, missing

di _n "=== Pregnancies missed by point-in-time measure ==="
di "Women with next_preg_2018==1 but preg_now==0 (pregnancy already resolved):"
count if next_preg_2018 == 1 & preg_now == 0 & year == 2018

di _n "=== Pregnancy losses ==="
tab preg_loss if year == 2018, missing

di _n "=== Unintended pregnancy: ex-post vs ex-ante ==="
tab unintended_preg_expost unintended_preg_exante if year == 2018, missing

di _n "=== Verification: unintended preg by baseline pregnancy status ==="
di "Ex post by baseline pregnancy status (all should have 0/1, not missing):"
tab preg_2016 unintended_preg_expost if year == 2018, missing
di _n "Ex ante by baseline pregnancy status:"
tab preg_2016 unintended_preg_exante if year == 2018, missing
di _n "Sample sizes for unintended preg measures (endline):"
count if !missing(unintended_preg_expost) & year == 2018
count if !missing(unintended_preg_exante) & year == 2018

g difference_up = unintended_preg_exante - unintended_preg_expost
ttest unintended_preg_expost == unintended_preg_exante if year == 2018
reg difference_up treatment if year == 2018, robust
drop difference_up

}

*===============================================================================
* 7. OTHER EMPOWERMENT AND FP OUTCOMES
*===============================================================================

* --- Desired number of children ---
capture rename w7_w712b quant_children
label var quant_children "Desired number of children"

* --- Husband wants more children ---
gen husband_wants_children = 1 if w7_w718f == 1 | w7_w718g == 1
replace husband_wants_children = 0 ///
    if inlist(w7_w718f, 2) | inlist(w7_w718g, 2, 3, 88)
label var husband_wants_children "Husband wants more children"

* --- Husband wants same number of children ---
gen same_number_children = 1 if w7_w720 == 1
replace same_number_children = 0 if inlist(w7_w720, 2, 3, 88)
label var same_number_children "HH same number children"

* --- Husband knows about FP use ---
gen husband_knows_fp = .
replace husband_knows_fp = 1 if w7_w718a == 1
replace husband_knows_fp = 0 if inlist(w7_w718a, 2, 88, 99)
label var husband_knows_fp "Husband knows about FP use"

* --- Contraception decision-making ---
gen decision_contraception = .
replace decision_contraception = 1 if w7_w718b == 1
replace decision_contraception = 0 if inlist(w7_w718b, 2, 3, 4)
label var decision_contraception "FP decision: respondent decides (1=yes)"

* --- Ever discussed FP with husband ---
gen discussed_fp_husband = .
replace discussed_fp_husband = 1 if w7_w718c2 == 1
replace discussed_fp_husband = 0 if inlist(w7_w718c2, 2, 88)
label var discussed_fp_husband "Ever discussed FP with husband"

* --- Decision-making / bargaining power (Section 8, Q823-Q826) ---
* Woman is involved in the decision (alone or jointly with husband)
* Coding: 1=woman alone, 2=husband alone, 3=jointly
* Binary: 1 if woman involved (alone=1 or joint=3), 0 if husband alone (=2)
foreach v in w8_w823 w8_w824 w8_w825 w8_w826 {
    capture drop `v'_binary
    gen `v'_binary = (`v' == 1 | `v' == 3) if !missing(`v') & `v' <= 3
}
label var w8_w823_binary "Involved in healthcare decisions"
label var w8_w824_binary "Involved in major HH purchases"
label var w8_w825_binary "Involved in daily purchases"
label var w8_w826_binary "Involved in family visit decisions"

*===============================================================================
* 8. CONTRACEPTION DURATION AND ADOPTION -- LEGACY (May 19, 2026)
*
* Builds fp_start_date, new_adopter_*, larc_uptake_*, months_fp_use_post,
* fp_duration_days, etc. None of these are referenced by any active result
* script in the JHE pipeline. Wrapped in `if 0` so it does not execute.
*===============================================================================

if 0 {

* --- Months of contraception use since intervention ---
gen fp_start_date = ym(w3_w308a_a, w3_w308a_m)
format fp_start_date %tm
label var fp_start_date "Start date of current FP"

gen baseline_date = mofd(date_int2016)
format baseline_date %tm

* Cap start date at baseline (so we measure post-intervention persistence)
gen fp_start_effective = fp_start_date
replace fp_start_effective = baseline_date if fp_start_date < baseline_date
format fp_start_effective %tm
label var fp_start_effective "FP start date (capped at baseline)"

gen interview_date = mofd(date_int2018)
format interview_date %tm

gen months_fp_use_post = interview_date - fp_start_effective
replace months_fp_use_post = 0 if missing(fp_start_effective)
replace months_fp_use_post = 0 if months_fp_use_post < 0
label var months_fp_use_post "Months of continuous FP use since baseline"

* --- Robustness: baseline FP users vs new adopters ---
* "Because some women report having started their current contraceptive
*  method before baseline, we redefine the duration outcome to measure
*  months of continuous use since the intervention began. For women already
*  using contraception at baseline, we cap the start date at baseline, so
*  the outcome captures post-intervention persistence rather than lifetime use."

gen baseline_user = fp_start_date < baseline_date
label var baseline_user "Was using FP before baseline"

gen continued_fp = (months_fp_use_post > 0) if baseline_user == 1
label var continued_fp "Continued FP use (among baseline users)"

gen new_adopter = fp_start_date >= baseline_date
label var new_adopter "Started FP after baseline"

* ----- May 2026 spec: new_adopter at 1-year and 2-year horizons -----
* fp_start_date and baseline_date are in monthly format (Stata %tm), so the
* difference is in months.
*
* new_adopter_1yr: started FP within 12 months of baseline
* new_adopter_2yr: started FP within 24 months of baseline
*
* Censoring/non-adoption logic: women who never started FP (fp_start_date
* missing) or were already on FP at baseline (fp_start_date < baseline_date)
* get coded as 0. Missing only if baseline_date itself is missing.

capture drop months_to_fp_start
gen double months_to_fp_start = fp_start_date - baseline_date ///
    if !missing(fp_start_date) & !missing(baseline_date)
label var months_to_fp_start "Months from baseline to start of current FP method"

* ----- Per user request (May 2026) -----
* new_adopter_1yr = 1 if woman started FP use between baseline and midline
*                   (was not using at baseline AND was using at midline)
* new_adopter_2yr = 1 if woman started FP use between baseline and endline
*                   (was not using at baseline AND was using at midline OR
*                   endline)
*
* Uses curr_use panel data (curr_use at each wave) and curr_use_bl for the
* baseline status. AVOIDS the fp_start_date missing-handling bug in the
* original `new_adopter` variable (Stata treats `. >= num` as TRUE, which
* wrongly counted women not currently on FP as "new adopters").
*
* Limitation: women who started AND stopped FP entirely between waves are
* coded as 0 (we don't observe their use). This is conservative.

capture drop _cu_2017 _cu_2018
bys caseid: egen _cu_2017 = max(cond(year == 2017, curr_use, .))
bys caseid: egen _cu_2018 = max(cond(year == 2018, curr_use, .))

capture drop new_adopter_1yr
gen byte new_adopter_1yr = (curr_use_bl == 0 & _cu_2017 == 1) ///
    if !missing(curr_use_bl) & !missing(_cu_2017)
label var new_adopter_1yr "Started FP between baseline and midline (curr_use_bl=0 AND curr_use_2017=1)"

capture drop new_adopter_2yr
gen byte new_adopter_2yr = (curr_use_bl == 0 & (_cu_2017 == 1 | _cu_2018 == 1)) ///
    if !missing(curr_use_bl) & (!missing(_cu_2017) | !missing(_cu_2018))
label var new_adopter_2yr "Started FP between baseline and endline (curr_use_bl=0 AND curr_use_2017=1 OR curr_use_2018=1)"

drop _cu_2017 _cu_2018

* Forward/backward fill so the woman-level outcomes are constant across years
foreach v in months_to_fp_start new_adopter_1yr new_adopter_2yr {
    bys caseid (year): replace `v' = `v'[_n-1] if missing(`v') & _n > 1
    gsort caseid -year
    bys caseid: replace `v' = `v'[_n-1] if missing(`v') & _n > 1
    sort caseid year
}

di _n as text "==== new_adopter by horizon (treatment, year=2018) ===="
foreach v in new_adopter new_adopter_1yr new_adopter_2yr {
    quietly summarize `v' if treatment == 1 & year == 2018
    di "  `v': N=" r(N) ", mean=" %5.3f r(mean)
}

}

*===============================================================================
* 9. FP KNOWLEDGE AND PREGNANCY ATTITUDES -- LEGACY (May 19, 2026)
*
* Builds space_knowledge, preg_likely_nofp, preg_likely_fp, preg_feel_positive.
* Referenced only in deprecated forest-plot code paths of graphs_02-12-2026.R;
* not in any results script. Wrapped in `if 0` so it does not execute.
*===============================================================================

if 0 {

* --- Perceived pregnancy likelihood if NOT using FP ---
gen preg_likely_nofp = .
replace preg_likely_nofp = 1 if inlist(w7_w721, 4, 5)
replace preg_likely_nofp = 0 if inlist(w7_w721, 1, 2, 3)
label var preg_likely_nofp "Perceived pregnancy likely if not using FP"

* --- Perceived pregnancy likelihood if using FP ---
gen preg_likely_fp = .
replace preg_likely_fp = 1 if inlist(w7_w723, 4, 5)
replace preg_likely_fp = 0 if inlist(w7_w723, 1, 2, 3)
label var preg_likely_fp "Perceived pregnancy likely if using FP"

* --- Positive feelings about potential pregnancy ---
gen preg_feel_positive = .
replace preg_feel_positive = 1 if inlist(w7_w725, 5, 6, 7)
replace preg_feel_positive = 0 if inlist(w7_w725, 1, 2, 3, 4)
label var preg_feel_positive "Positive feelings about pregnancy (1 = high)"

}

*===============================================================================
* 10. HEALTH UTILIZATION, BREASTFEEDING, SEXUAL ACTIVITY -- LEGACY
*
* Builds breastfed, sex_since_birth, and related variables. None are
* referenced by any active result script. Wrapped in `if 0`.
*===============================================================================

if 0 {

* --- Care-seeking ---
capture rename w3_w327 seek_care
replace seek_care = 0 if seek_care == 2
label var seek_care "Visited a care facility"

* --- Breastfeeding indicator ---
* Coded 1 if any breastfeeding duration reported (hours or days)
gen breastfed = 0
replace breastfed = 1 if !missing(w4_w455_d) | !missing(w4_w455_h)
capture label drop breastfed_lbl
label define breastfed_lbl 0 "No" 1 "Yes"
label values breastfed breastfed_lbl
label var breastfed "Ever breastfed"

* --- Sexual intercourse since birth ---
capture rename w4_w451 sex_since_birth
replace sex_since_birth = 0 if sex_since_birth == 2
label var sex_since_birth "Had sexual intercourse since birth"

}

*===============================================================================
* 11. NEW OUTCOMES: METHOD CHOICE, SIDE EFFECTS, MENTAL HEALTH, ACCESS -- LEGACY
*     Added March 2026 (paper strengthening audit)
*
* Builds fp_method_larc, fp_method_injectable, fp_method_modern,
* side_effects_informed/managed, depression_binary, phq9_score,
* fp_source_public. None are referenced by any active result script.
* Wrapped in `if 0`.
*===============================================================================

if 0 {

* --- A1: FP Method Choice / Method Mix ---
* The dataset already contains pre-constructed method variables from earlier
* cleaning stages: injectable, implant, modern_method_use (N=782 at 2018).
* We create fp_method_larc as a new composite (IUD is rare, so LARC ≈ implant).
* The raw w3_w304* flags do NOT exist in the merged dataset.

* LARC = implant (IUD is very rare in this sample)
capture drop fp_method_larc
capture gen fp_method_larc = implant if !missing(implant)
capture label var fp_method_larc "Using LARC (implant/IUD)"

* fp_method_injectable: alias for existing `injectable` variable
capture drop fp_method_injectable
capture gen fp_method_injectable = injectable if !missing(injectable)
capture label var fp_method_injectable "Using injectable"

* fp_method_modern: alias for existing `modern_method_use`
capture drop fp_method_modern
capture gen fp_method_modern = modern_method_use if !missing(modern_method_use)
capture label var fp_method_modern "Using modern FP method"

* --- LARC uptake at 1-year and 2-year horizons (May 2026 spec) -----------------
* Definition: woman is using LARC (implant/IUD) at the relevant survey wave AND
* started her current FP method after baseline (i.e., she's a new adopter who
* chose LARC). Woman-level outcomes built by extracting the year-specific value.

* Extract year-specific LARC indicators
capture drop _larc_2017 _larc_2018
bys caseid: egen _larc_2017 = max(cond(year == 2017 & !missing(fp_method_larc), fp_method_larc, .))
bys caseid: egen _larc_2018 = max(cond(year == 2018 & !missing(fp_method_larc), fp_method_larc, .))

capture drop larc_uptake_1yr
gen byte larc_uptake_1yr = (new_adopter_1yr == 1 & _larc_2017 == 1) ///
    if !missing(new_adopter_1yr) & !missing(_larc_2017)
label var larc_uptake_1yr "LARC uptake within 12 months of baseline (new adopter + LARC at 2017)"

capture drop larc_uptake_2yr
gen byte larc_uptake_2yr = (new_adopter_2yr == 1 & _larc_2018 == 1) ///
    if !missing(new_adopter_2yr) & !missing(_larc_2018)
label var larc_uptake_2yr "LARC uptake within 24 months of baseline (new adopter + LARC at 2018)"

drop _larc_2017 _larc_2018

di _n as text "==== LARC uptake by horizon (treatment, year=2018) ===="
foreach v in fp_method_larc larc_uptake_1yr larc_uptake_2yr {
    quietly count if treatment == 1 & year == 2018 & !missing(`v')
    local n_obs = r(N)
    quietly count if treatment == 1 & year == 2018 & `v' == 1
    local n_one = r(N)
    quietly summarize `v' if treatment == 1 & year == 2018
    di "  `v': N=" `n_obs' ", events=" `n_one' ", mean=" %5.3f r(mean)
}

di _n "=== A1: Method choice ==="
capture noisily tab fp_method_larc if year == 2018 & treatment == 1
capture noisily tab fp_method_injectable if year == 2018 & treatment == 1
capture noisily tab fp_method_modern if year == 2018 & treatment == 1

* --- A2: Side Effects Counseling ---
* Q317: Told about side effects before starting (1=yes, 2=no)
* Q318: Told health worker about side effects (1=yes, 2=no)
* Q319: Told what to do about side effects (1=yes, 2=no)
capture drop side_effects_informed
capture gen side_effects_informed = (w3_w317 == 1) if !missing(w3_w317)
capture label var side_effects_informed "Told about side effects before starting method"

capture drop side_effects_managed
capture gen side_effects_managed = (w3_w319 == 1) if !missing(w3_w319)
capture label var side_effects_managed "Told what to do about side effects"

di _n "=== A2: Side effects counseling ==="
capture tab side_effects_informed if year == 2018, missing
capture tab side_effects_managed if year == 2018, missing

* --- A3: Postpartum Depression (PHQ-9) ---
* Q471_1 through Q471_9: 9-item PHQ-9 (0=not at all, 1=several days,
*   2=more than half days, 3=nearly every day)
* Q472: functional impairment (1-4 scale)
* PHQ-9 cutoff: >= 10 = moderate depression

capture drop phq9_score
capture gen phq9_score = w4_w471_1 + w4_w471_2 + w4_w471_3 + ///
    w4_w471_4 + w4_w471_5 + w4_w471_6 + ///
    w4_w471_7 + w4_w471_8 + w4_w471_9
capture label var phq9_score "PHQ-9 depression score (0-27)"

* Forward-fill PHQ-9: W4 module was collected at midline (2017) for most women.
* Only 18 treatment women have data at year==2018. Since PHQ-9 is a person-level
* measure (not meaningfully time-varying over 1 year), carry the midline value
* forward to endline for women missing at 2018.
capture {
    bysort caseid (year): egen double _phq9_fill = max(phq9_score)
    replace phq9_score = _phq9_fill if missing(phq9_score) & !missing(_phq9_fill)
    drop _phq9_fill
}

capture drop depression_binary
capture gen depression_binary = (phq9_score >= 10) if !missing(phq9_score)
capture label var depression_binary "Moderate+ depression (PHQ-9 >= 10)"

di _n "=== A3: PHQ-9 Depression ==="
capture noisily count if !missing(phq9_score) & year == 2018 & treatment == 1
capture sum phq9_score if year == 2018, detail
capture tab depression_binary if year == 2018, missing

* --- A4: Reasons for Not Using FP ---
* Q709 items: reasons not contracepting (multiple response)
* Key barriers grouped into categories
* NOTE: These variables may not exist in merged dataset — capture handles this
capture drop barrier_side_effects
capture gen barrier_side_effects = (w7_w709_o == 1 | w7_w709_p1 == 1) ///
    if !missing(curr_use) & curr_use == 0
capture label var barrier_side_effects "Barrier: fear of side effects (non-users)"

capture drop barrier_opposition
capture gen barrier_opposition = (w7_w709_i == 1 | w7_w709_j == 1 | ///
    w7_w709_k == 1 | w7_w709_l == 1) if !missing(curr_use) & curr_use == 0
capture label var barrier_opposition "Barrier: opposition (self/husband/religious)"

capture drop barrier_access
capture gen barrier_access = (w7_w709_n == 1 | w7_w709_p2 == 1 | ///
    w7_w709_q == 1) if !missing(curr_use) & curr_use == 0
capture label var barrier_access "Barrier: access/knowledge/cost (non-users)"

di _n "=== A4: Reasons for non-use ==="
capture tab barrier_side_effects if year == 2018, missing
capture tab barrier_opposition if year == 2018, missing
capture tab barrier_access if year == 2018, missing

* --- A7: FP Source / Provider ---
* Q323: source of current FP method
* Q323A: distance to provider (km)
* Q323B: travel time (minutes)
capture drop fp_source_public
capture gen fp_source_public = inlist(w3_w323, 1, 2, 3, 4, 5, 6, 7) ///
    if !missing(w3_w323) & curr_use == 1
capture label var fp_source_public "FP from public sector source"

capture drop fp_distance_km
capture clonevar fp_distance_km = w3_w323a
capture replace fp_distance_km = . if fp_distance_km > 100 | fp_distance_km < 0
capture label var fp_distance_km "Distance to FP provider (km)"

capture drop fp_travel_time
capture clonevar fp_travel_time = w3_w323b
capture replace fp_travel_time = . if fp_travel_time > 500 | fp_travel_time < 0
capture label var fp_travel_time "Travel time to FP provider (minutes)"

di _n "=== A7: FP source ==="
capture tab fp_source_public if year == 2018, missing
capture sum fp_distance_km if year == 2018, detail
capture sum fp_travel_time if year == 2018, detail

di _n "=== New variable construction complete ==="
di "Variables that show 0 obs likely don't exist in the merged dataset"
di "and will not be available for analysis."

}

*===============================================================================
* 12. ADMIN FP FROM REIMBURSEMENT LOG + COVERAGE + BASELINE TIMING
*    (Added May 2026 — moves /tmp/build_admin_fp.do, /tmp/build_counts.do,
*    /tmp/coverage2.do into the main cleaning pipeline)
*    Variables built here:
*      Admin FP:    admin_fp_any, admin_inject_any, admin_larc_any, admin_pills_any
*      Counts:      n_admin_fp, n_admin_inject, n_admin_larc
*      Dates:       first_admin_fp_date, last_admin_fp_date, admin_engagement_days
*      Coverage:    days_index_to_endline, fp_coverage_frac,
*                   fp_inj_protected_days, fp_inj_coverage_frac
*      Timing:      baseline_to_index_days, months_pp_at_baseline,
*                   months_preg_at_baseline, pp_late, preg_late
*===============================================================================

di _n "=== Section 12: Admin FP, coverage, and baseline timing ==="

* --- Save current data to a tempfile so we can return to it after building admin vars ---
tempfile main_data
save `main_data'

* --- STEP 1: Build per-woman admin FP measures from reimbursement log ---
import delimited "${dta}/Intervention Components Data/clinic_reimbursement_log.csv", ///
    clear varnames(1) stringcols(_all)
gen byte _is_fp = (lower(fpservice) == "yes")

* Hand-correct 3 PROGRAM ID typos
replace programid = "N1064"  if programid == "1064"
replace programid = "S1136"  if programid == "1136"
replace programid = "N1186"  if programid == "N11868"

* Categorize service type
gen byte svc_larc = regexm(lower(service_type), "implant|jadelle|iud|sterilization|sterilisation")
gen byte svc_inject = regexm(lower(service_type), "inject")
gen byte svc_pills = regexm(lower(service_type), "pills") & !svc_larc & !svc_inject

* Service date
gen svc_date = date(date, "DMY", 2050)
format svc_date %td

* Build any-service binary indicators (kept as "_b" to allow both max and sum aggregation)
gen byte _is_fp_b      = _is_fp
gen byte svc_larc_b    = svc_larc
gen byte svc_inject_b  = svc_inject

* --- STEP 2: Per-woman aggregates among FP-service rows ---
preserve
keep if _is_fp == 1
collapse (sum) n_admin_fp     = _is_fp_b ///
         (sum) n_admin_inject = svc_inject_b ///
         (sum) n_admin_larc   = svc_larc_b ///
         (max) admin_fp_any   = _is_fp ///
         (max) admin_inject_any = svc_inject ///
         (max) admin_larc_any = svc_larc ///
         (max) admin_pills_any = svc_pills ///
         (min) first_admin_fp_date = svc_date ///
         (max) last_admin_fp_date  = svc_date, by(programid)
format first_admin_fp_date last_admin_fp_date %td
gen admin_engagement_days = last_admin_fp_date - first_admin_fp_date

* Merge to caseid via PROGRAM_ID ↔ caseid mapping
rename programid program_id
merge 1:1 program_id using "${dta}/Intervention Components Data/program_id_caseid_map.dta", ///
    keep(master match) gen(_m_pidmap)
keep if _m_pidmap == 3
keep caseid n_admin_fp n_admin_inject n_admin_larc ///
     admin_fp_any admin_inject_any admin_larc_any admin_pills_any ///
     first_admin_fp_date last_admin_fp_date admin_engagement_days

tempfile admin_fp
save `admin_fp'
restore

* --- STEP 3: Reload main data and merge admin vars in ---
use `main_data', clear

* Drop pre-existing copies (idempotent)
foreach v in admin_fp_any admin_inject_any admin_larc_any admin_pills_any ///
             n_admin_fp n_admin_inject n_admin_larc ///
             first_admin_fp_date last_admin_fp_date admin_engagement_days ///
             days_index_to_endline fp_coverage_frac ///
             fp_inj_protected_days fp_inj_coverage_frac ///
             baseline_to_index_days months_pp_at_baseline months_preg_at_baseline ///
             pp_late preg_late {
    capture drop `v'
}

* Drop any pre-existing merge-tracking variables to keep the merge idempotent
foreach mv in _m_admin _m_pidmap _m_counts _m_takeup _merge {
    capture drop `mv'
}
merge m:1 caseid using `admin_fp', keep(master match) gen(_m_admin)

* For treatment women not in admin log, fill 0s on count/binary admin vars
foreach v in admin_fp_any admin_inject_any admin_larc_any admin_pills_any ///
             n_admin_fp n_admin_inject n_admin_larc admin_engagement_days {
    replace `v' = 0 if missing(`v') & treatment == 1
}
drop _m_admin

* --- STEP 4: Coverage variables ---
* Days from index_birth_date to endline interview (post-index fertile window)
gen days_index_to_endline = date_int2018 - index_birth_date

* Engagement-window coverage fraction
gen fp_coverage_frac = admin_engagement_days / days_index_to_endline ///
    if !missing(days_index_to_endline) & !missing(admin_engagement_days)
replace fp_coverage_frac = 0 if fp_coverage_frac < 0
replace fp_coverage_frac = 1 if fp_coverage_frac > 1 & !missing(fp_coverage_frac)

* Approximate days of injectable protection (each injectable ≈ 90 days, capped at follow-up window)
gen fp_inj_protected_days = n_admin_inject * 90 if !missing(n_admin_inject)
replace fp_inj_protected_days = days_index_to_endline ///
    if fp_inj_protected_days > days_index_to_endline & !missing(days_index_to_endline) & !missing(fp_inj_protected_days)
gen fp_inj_coverage_frac = fp_inj_protected_days / days_index_to_endline ///
    if !missing(days_index_to_endline)

* --- STEP 5: Baseline timing variables ---
* CRITICAL: use date_int2016 (%td daily) NOT baseline_date (%tm monthly) for daily date math.
gen baseline_to_index_days = index_birth_date - date_int2016 ///
    if !missing(index_birth_date, date_int2016)
gen months_pp_at_baseline   = -baseline_to_index_days / 30.4375 if preg_2016 == 0
gen months_preg_at_baseline =  baseline_to_index_days / 30.4375 if preg_2016 == 1

* Median splits within group (using year=2018 sample for split decision; same value for all panel years)
quietly summarize months_pp_at_baseline ///
    if preg_2016 == 0 & treatment == 1 & year == 2018, detail
local pp_med = r(p50)
gen byte pp_late = (months_pp_at_baseline > `pp_med') ///
    if preg_2016 == 0 & !missing(months_pp_at_baseline)

quietly summarize months_preg_at_baseline ///
    if preg_2016 == 1 & treatment == 1 & year == 2018, detail
local preg_med = r(p50)
gen byte preg_late = (months_preg_at_baseline > `preg_med') ///
    if preg_2016 == 1 & !missing(months_preg_at_baseline)

* --- Labels ---
label var admin_fp_any           "Admin: any FP service via reimbursement (binary)"
label var admin_inject_any       "Admin: any injectable service via reimbursement"
label var admin_larc_any         "Admin: any LARC service via reimbursement"
label var admin_pills_any        "Admin: any pills service via reimbursement"
label var n_admin_fp             "Admin: # FP services received"
label var n_admin_inject         "Admin: # injectable services received"
label var n_admin_larc           "Admin: # LARC services received"
label var first_admin_fp_date    "Admin: date of first FP service in reimbursement log"
label var last_admin_fp_date     "Admin: date of last FP service in reimbursement log"
label var admin_engagement_days  "Admin: days from first to last FP service (=0 for non-engagers)"
label var days_index_to_endline  "Days from index_birth_date to date_int2018"
label var fp_coverage_frac       "Engagement window / post-index follow-up days"
label var fp_inj_protected_days  "Approx days of injectable protection (n*90, capped at follow-up window)"
label var fp_inj_coverage_frac   "Injectable protection days / post-index follow-up window"
label var baseline_to_index_days "Days from baseline interview to index birth (negative for PP, positive for Preg)"
label var months_pp_at_baseline  "Months postpartum at baseline (PP only)"
label var months_preg_at_baseline "Months until delivery at baseline (Preg only)"
label var pp_late                "PP with >2.9 mo postpartum at baseline (older infant)"
label var preg_late              "Preg with >2.6 mo to delivery at baseline (earlier in pregnancy)"

* --- Verification ---
di _n "==== Verification of Section 12 builds (year=2018, treatment=1) ===="
foreach v in admin_fp_any admin_inject_any n_admin_fp fp_inj_coverage_frac ///
             fp_inj_protected_days days_index_to_endline ///
             months_pp_at_baseline months_preg_at_baseline {
    quietly summarize `v' if year == 2018 & treatment == 1
    di "  `v': N=" r(N) "  mean=" %7.3f r(mean) "  min=" %7.3f r(min) "  max=" %7.3f r(max)
}

*===============================================================================
* 13. SAVE
*===============================================================================

save "${dta}/merge_counselor_outcomes.dta", replace

di _n as text "=== cleaning_outcomes complete ==="
di as text "Output: ${dta}/merge_counselor_outcomes.dta"

log close
