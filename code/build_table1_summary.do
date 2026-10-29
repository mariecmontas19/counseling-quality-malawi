********************************************************************************
* build_table1_summary.do
*
* PURPOSE: Build Table 1 of the JHE paper - descriptive statistics for the
*          analytic sample (N = 901 treatment + first counselor), reporting
*          Mean and SD for the full sample, the pregnant-at-baseline subgroup,
*          and the postpartum-at-baseline subgroup.
*
* INPUT:  dta/merge_counselor_outcomes.dta
* OUTPUT: /tmp/table1_summary.csv  (rows = variables, columns = Total Mean/SD,
*                                    Pregnant Mean/SD, PP Mean/SD, N_total)
*
* AUTHOR: Marie Montas
* DATE:   May 20, 2026
********************************************************************************

clear all
set more off

if "$MFPS_ROOT" == "" global MFPS_ROOT "`c(pwd)'"  // project folder (holds Analysis/)
local dta "${MFPS_ROOT}/Analysis/dta"

* ==============================================================================
* Load and restrict to treatment + first counselor universe (N = 901)
* ==============================================================================
use "`dta'/merge_counselor_outcomes.dta", clear
keep if treatment == 1 & !missing(first_counselor)

* Build high_quality_avg_sattop (binary endogenous variable)
capture drop high_quality_avg_sattop
quietly summarize avg_quality_sattop if treatment == 1, detail
local p50 = r(p50)
gen high_quality_avg_sattop = (avg_quality_sattop > `p50') if !missing(avg_quality_sattop) & treatment == 1

* Build age-group dummies. Value labels (verified by tabulating with labels):
*   age_group_bl == 0 -> 31/35
*   age_group_bl == 1 -> <=20
*   age_group_bl == 2 -> 21-25
*   age_group_bl == 3 -> 26-30
capture drop age_le20 age_21_25 age_26_30 age_31_35
gen age_le20  = (age_group_bl == 1) if !missing(age_group_bl)
gen age_21_25 = (age_group_bl == 2) if !missing(age_group_bl)
gen age_26_30 = (age_group_bl == 3) if !missing(age_group_bl)
gen age_31_35 = (age_group_bl == 0) if !missing(age_group_bl)

* Religion: binary Christian indicator (verified: 1 = Christian, 0 = Not Christian)
capture drop religion_christian
gen religion_christian = (religion_r_bl == 1) if !missing(religion_r_bl)

* Ethnicity: binary Chewa indicator (verified: 1 = Chewa, 0 = Not Chewa)
capture drop ethnicity_chewa
gen ethnicity_chewa = (ethnicity_r_bl == 1) if !missing(ethnicity_r_bl)

* Build a woman-level husband_present (max across all panel years, since the
* variable is missing at year=2016 and populated at year=2017/2018).
capture drop husband_present_wl
bys caseid: egen husband_present_wl = max(husband_present)

* ==============================================================================
* BLOCK 1: Variables measured at baseline or forward-filled (woman-level
* snapshot at year=2016, N=901)
* ==============================================================================
preserve

keep if year == 2016

local block1_vars age_le20 age_21_25 age_26_30 age_31_35 ///
    sex_age_bl total_alive_bl ever_use_bl edu_primary_bl work_bl ///
    religion_christian ethnicity_chewa curr_use_bl counselLP_bl ///
    avg_quality_sattop cqt_resid_loo_sattop high_quality_avg_sattop ///
    share_same_counselor total_sessions visit_rate_mo husband_present_wl ///
    etp1_fp etp1_spacing etp1_preg etp1_partner etp1_childhealth etp1_breast

tempname B1
postfile `B1' str40 variable double total_mean double total_sd double total_n ///
    double preg_mean double preg_sd double preg_n ///
    double pp_mean double pp_sd double pp_n ///
    using /tmp/table1_block1.dta, replace

foreach v of local block1_vars {
    capture confirm variable `v'
    if _rc != 0 {
        di as error "Skipping `v' (not found)"
        continue
    }
    quietly summarize `v'
    local tm = r(mean)
    local ts = r(sd)
    local tn = r(N)
    quietly summarize `v' if preg_2016 == 1
    local prm = r(mean)
    local prs = r(sd)
    local prn = r(N)
    quietly summarize `v' if preg_2016 == 0
    local ppm = r(mean)
    local pps = r(sd)
    local ppn = r(N)
    post `B1' ("`v'") (`tm') (`ts') (`tn') (`prm') (`prs') (`prn') (`ppm') (`pps') (`ppn')
}
postclose `B1'

use /tmp/table1_block1.dta, clear
gen block = 1
tempfile b1
save `b1', replace

restore

* ==============================================================================
* BLOCK 2: Endline outcomes (year=2018, N depends on per-outcome missingness)
* ==============================================================================
preserve

keep if year == 2018

local block2_vars admin_fp_any admin_inject_any n_admin_fp fp_inj_coverage_frac husband_knows_fp ///
    birth_within_24mo preg_within_12mo preg_within_24mo mistimed_preg_expost

tempname B2
postfile `B2' str40 variable double total_mean double total_sd double total_n ///
    double preg_mean double preg_sd double preg_n ///
    double pp_mean double pp_sd double pp_n ///
    using /tmp/table1_block2.dta, replace

foreach v of local block2_vars {
    capture confirm variable `v'
    if _rc != 0 {
        di as error "Skipping `v' (not found)"
        continue
    }
    quietly summarize `v'
    local tm = r(mean)
    local ts = r(sd)
    local tn = r(N)
    quietly summarize `v' if preg_2016 == 1
    local prm = r(mean)
    local prs = r(sd)
    local prn = r(N)
    quietly summarize `v' if preg_2016 == 0
    local ppm = r(mean)
    local pps = r(sd)
    local ppn = r(N)
    post `B2' ("`v'") (`tm') (`ts') (`tn') (`prm') (`prs') (`prn') (`ppm') (`pps') (`ppn')
}
postclose `B2'

use /tmp/table1_block2.dta, clear
gen block = 2
tempfile b2
save `b2', replace

restore

* ==============================================================================
* Combine and export
* ==============================================================================
use `b1', clear
append using `b2'

export delimited using /tmp/table1_summary.csv, replace

list variable total_mean total_sd total_n preg_mean preg_sd preg_n pp_mean pp_sd pp_n block, sep(0) noobs

di _n "Saved: /tmp/table1_summary.csv"
