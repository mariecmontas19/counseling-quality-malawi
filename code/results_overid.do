* ===========================================================================
* results_overid.do
* New identification section (July 2026, per David Canning's comments):
*   1. Reduced-form ITT: joint F-test of the five counselor dummies in
*      Y = X + counselor dummies, per outcome. Pure ITT.
*   2. Overidentified 2SLS: continuous FIRST-SESSION quality q instrumented
*      by the five counselor dummies; Hansen J overidentification test.
*   3. Just-identified 2SLS: one counselor contrast at a time (each dummy
*      1{counselor==k} as the sole instrument, k = 1..6), with the
*      per-contrast first-stage F.
*
* Endogenous variable: quality_index_first_sattop (continuous first-session
* satisfaction + engaged-topics index) -- NOT the median-split cumulative
* average used in the old main tables. No scheduling residualization: the
* instruments are the raw randomly assigned first-counselor dummies.
*
* Samples match the main IV table: Panels A/B woman-level (one obs per
* woman); Panels C/D endline (year==2018, treatment==1).
*
* Outputs: /tmp/overid_rf.csv, /tmp/overid_iv.csv, /tmp/overid_ji.csv
* ===========================================================================

capture log close
log using "log/results_overid.log", replace text

use "dta/merge_counselor_outcomes.dta", clear

global covariates ///
    i.age_group_bl sex_age_bl total_alive_bl ever_use_bl ///
    edu_primary_bl work_bl religion_r_bl i.area_bl ethnicity_r_bl ///
    preg_2016 curr_use_bl counselLP_bl

* Counselor dummies (first randomly assigned counselor, 1..6)
forvalues k = 1/6 {
    capture drop cd`k'
    gen cd`k' = (first_counselor == `k') if !missing(first_counselor)
}

local A_outs share_same_counselor total_sessions visit_rate_mo husband_present
local B_outs etp1_fp etp1_spacing etp1_preg etp1_partner etp1_childhealth etp1_breast
local C_outs admin_fp_any admin_inject_any n_admin_fp fp_inj_coverage_frac husband_knows_fp
local D_outs birth_within_24mo preg_within_12mo preg_within_24mo mistimed_preg_expost

* ---------------------------------------------------------------------------
* Postfiles
* ---------------------------------------------------------------------------
* (1) Reduced-form ITT: joint F of the 5 dummies + per-counselor contrasts vs C1
postfile pf_rf str25 outcome str8 panel ///
    double rf_F double rf_p double n double mn ///
    double b2 double se2 double b3 double se3 double b4 double se4 ///
    double b5 double se5 double b6 double se6 ///
    using /tmp/overid_rf.dta, replace

* (2) Overidentified IV: q_first instrumented by 5 dummies + Hansen J
postfile pf_iv str25 outcome str8 panel ///
    double b double se double p double n ///
    double kpF double jstat double jdf double jp ///
    using /tmp/overid_iv.dta, replace

* (3) Just-identified IV: one dummy at a time
postfile pf_ji str25 outcome str8 panel double k ///
    double b double se double p double n double fsF ///
    using /tmp/overid_ji.dta, replace

* ---------------------------------------------------------------------------
* Estimation program (runs on whatever sample is in memory)
* ---------------------------------------------------------------------------
capture program drop run_overid
program define run_overid
    args y panel

    quietly summarize `y', meanonly
    local mn = r(mean)

    * --- (1) Reduced-form ITT: Y on counselor dummies + X ---
    quietly capture noisily reg `y' cd2 cd3 cd4 cd5 cd6 $covariates, robust
    if _rc == 0 {
        local rn = e(N)
        quietly testparm cd2 cd3 cd4 cd5 cd6
        local rf_F = r(F)
        local rf_p = r(p)
        forvalues k = 2/6 {
            local b`k'  = _b[cd`k']
            local se`k' = _se[cd`k']
        }
        post pf_rf ("`y'") ("`panel'") (`rf_F') (`rf_p') (`rn') (`mn') ///
            (`b2') (`se2') (`b3') (`se3') (`b4') (`se4') (`b5') (`se5') (`b6') (`se6')
    }

    * --- (2) Overidentified 2SLS: q_first on 5 dummies, Hansen J ---
    quietly capture noisily ivreg2 `y' ///
        (quality_index_first_sattop = cd2 cd3 cd4 cd5 cd6) $covariates, robust
    if _rc == 0 {
        local ib  = _b[quality_index_first_sattop]
        local ise = _se[quality_index_first_sattop]
        local ip  = 2*(1 - normal(abs(`ib'/`ise')))
        local in  = e(N)
        local kpF = e(widstat)
        local js  = e(j)
        local jdf = e(jdf)
        local jp  = e(jp)
        post pf_iv ("`y'") ("`panel'") (`ib') (`ise') (`ip') (`in') ///
            (`kpF') (`js') (`jdf') (`jp')
    }

    * --- (3) Just-identified: each counselor-k-vs-rest dummy alone ---
    forvalues k = 1/6 {
        quietly capture noisily ivreg2 `y' ///
            (quality_index_first_sattop = cd`k') $covariates, robust
        if _rc == 0 {
            local jb  = _b[quality_index_first_sattop]
            local jse = _se[quality_index_first_sattop]
            local jip = 2*(1 - normal(abs(`jb'/`jse')))
            local jn  = e(N)
            local jF  = e(widstat)
            post pf_ji ("`y'") ("`panel'") (`k') (`jb') (`jse') (`jip') (`jn') (`jF')
        }
    }
end

* ===========================================================================
* Panels A and B: WOMAN-LEVEL (one observation per woman), as in
* results_main_iv.do. All collapsed variables are woman-level constants.
* ===========================================================================
preserve
keep if treatment == 1
collapse (mean) `A_outs' `B_outs' ///
    quality_index_first_sattop first_counselor cd1 cd2 cd3 cd4 cd5 cd6 ///
    age_group_bl sex_age_bl total_alive_bl ever_use_bl edu_primary_bl work_bl ///
    religion_r_bl area_bl ethnicity_r_bl preg_2016 curr_use_bl counselLP_bl, by(caseid)

foreach y of local A_outs {
    run_overid `y' A
}
foreach y of local B_outs {
    run_overid `y' B
}
restore

* ===========================================================================
* Panels C and D: endline (year==2018, treatment==1), one obs per woman
* ===========================================================================
preserve
keep if treatment == 1 & year == 2018

foreach y of local C_outs {
    run_overid `y' C
}
foreach y of local D_outs {
    run_overid `y' D
}
restore

postclose pf_rf
postclose pf_iv
postclose pf_ji

preserve
use /tmp/overid_rf.dta, clear
export delimited /tmp/overid_rf.csv, replace
use /tmp/overid_iv.dta, clear
export delimited /tmp/overid_iv.csv, replace
use /tmp/overid_ji.dta, clear
export delimited /tmp/overid_ji.csv, replace
restore

log close
