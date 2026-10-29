capture file close f
log using /tmp/phase7a_dropcouns.log, replace text
use "dta/merge_counselor_outcomes.dta", clear

quietly summarize avg_quality_sattop if treatment == 1, detail
local p50 = r(p50)
capture drop high_quality_avg_sattop
gen high_quality_avg_sattop = (avg_quality_sattop > `p50') if !missing(avg_quality_sattop) & treatment == 1

global covariates ///
    i.age_group_bl sex_age_bl total_alive_bl ever_use_bl ///
    edu_primary_bl work_bl religion_r_bl i.area_bl ethnicity_r_bl ///
    preg_2016 curr_use_bl counselLP_bl
global covariates_no_preg ///
    i.age_group_bl sex_age_bl total_alive_bl ever_use_bl ///
    edu_primary_bl work_bl religion_r_bl i.area_bl ethnicity_r_bl ///
    curr_use_bl counselLP_bl

capture drop hq_X_preg cqt_X_preg
gen hq_X_preg = high_quality_avg_sattop * preg_2016
gen cqt_X_preg = cqt_resid_loo_sattop * preg_2016

* Verify counselor variable
di _n "==== Counselor distribution ===="
tab first_counselor if treatment == 1 & year == 2018

* FULL 19-outcome set (4 panels) — extended May 18, 2026 for the JHE appendix LOO table
*   Panel A (continuity, 4): total_sessions, share_same_counselor, visit_rate_mo, husband_present
*   Panel B (topics, 6):    etp1_fp, etp1_spacing, etp1_preg, etp1_partner, etp1_childhealth, etp1_breast
*   Panel C (FP coverage, 4): admin_fp_any, admin_inject_any, n_admin_fp, fp_inj_coverage_frac
*   Panel D (reproductive, 5): birth_within_12mo, birth_within_24mo, preg_within_12mo, preg_within_24mo, mistimed_preg_expost
local headline_outs total_sessions share_same_counselor visit_rate_mo husband_present etp1_fp etp1_spacing etp1_preg etp1_partner etp1_childhealth etp1_breast admin_fp_any admin_inject_any n_admin_fp fp_inj_coverage_frac birth_within_12mo birth_within_24mo preg_within_12mo preg_within_24mo mistimed_preg_expost

* Postfile: store β + p for each outcome × spec × subgroup
postfile pf str25 outcome str8 spec str8 subgroup double b double se double p double n ///
    using /tmp/phase7a_results.dta, replace

* Split the headline outcomes by panel: A/B (continuity + topics) are woman-level
* (one obs per woman; zero within-woman variation across waves), C/D (FP coverage
* + reproductive) are endline (year==2018, already one per woman).
local AB_outs total_sessions share_same_counselor visit_rate_mo husband_present etp1_fp etp1_spacing etp1_preg etp1_partner etp1_childhealth etp1_breast
local CD_outs admin_fp_any admin_inject_any n_admin_fp fp_inj_coverage_frac birth_within_12mo birth_within_24mo preg_within_12mo preg_within_24mo mistimed_preg_expost

* ===== Panels A/B: woman-level collapse (retain first_counselor for the drops) =====
preserve
keep if treatment == 1
collapse (mean) `AB_outs' high_quality_avg_sattop cqt_resid_loo_sattop hq_X_preg cqt_X_preg ///
    age_group_bl sex_age_bl total_alive_bl ever_use_bl edu_primary_bl work_bl ///
    religion_r_bl area_bl ethnicity_r_bl preg_2016 curr_use_bl counselLP_bl ///
    (firstnm) first_counselor, by(caseid)
foreach y of local AB_outs {
    quietly capture noisily ivreg2 `y' (high_quality_avg_sattop = cqt_resid_loo_sattop) ///
        $covariates, robust
    if _rc == 0 {
        local b = _b[high_quality_avg_sattop]
        local se = _se[high_quality_avg_sattop]
        local pv = 2*(1 - normal(abs(`b'/`se')))
        local n = e(N)
        post pf ("`y'") ("Main") ("Pool") (`b') (`se') (`pv') (`n')
    }
    quietly capture noisily ivreg2 `y' (high_quality_avg_sattop hq_X_preg = cqt_resid_loo_sattop cqt_X_preg) ///
        $covariates, robust
    if _rc == 0 {
        quietly lincom high_quality_avg_sattop
        post pf ("`y'") ("Main") ("PP") (r(estimate)) (r(se)) (r(p)) (r(N))
        quietly lincom high_quality_avg_sattop + hq_X_preg
        post pf ("`y'") ("Main") ("Preg") (r(estimate)) (r(se)) (r(p)) (r(N))
    }
    forvalues c = 1/6 {
        quietly capture noisily ivreg2 `y' (high_quality_avg_sattop = cqt_resid_loo_sattop) ///
            $covariates if first_counselor != `c', robust
        if _rc == 0 {
            local b = _b[high_quality_avg_sattop]
            local se = _se[high_quality_avg_sattop]
            local pv = 2*(1 - normal(abs(`b'/`se')))
            local n = e(N)
            post pf ("`y'") ("dC`c'") ("Pool") (`b') (`se') (`pv') (`n')
        }
        quietly capture noisily ivreg2 `y' (high_quality_avg_sattop hq_X_preg = cqt_resid_loo_sattop cqt_X_preg) ///
            $covariates if first_counselor != `c', robust
        if _rc == 0 {
            quietly lincom high_quality_avg_sattop
            post pf ("`y'") ("dC`c'") ("PP") (r(estimate)) (r(se)) (r(p)) (r(N))
            quietly lincom high_quality_avg_sattop + hq_X_preg
            post pf ("`y'") ("dC`c'") ("Preg") (r(estimate)) (r(se)) (r(p)) (r(N))
        }
    }
}
restore

* ===== Panels C/D: endline year==2018 =====
foreach y of local CD_outs {
    quietly capture noisily ivreg2 `y' (high_quality_avg_sattop = cqt_resid_loo_sattop) ///
        $covariates if year == 2018 & treatment == 1, robust
    if _rc == 0 {
        local b = _b[high_quality_avg_sattop]
        local se = _se[high_quality_avg_sattop]
        local pv = 2*(1 - normal(abs(`b'/`se')))
        local n = e(N)
        post pf ("`y'") ("Main") ("Pool") (`b') (`se') (`pv') (`n')
    }
    quietly capture noisily ivreg2 `y' (high_quality_avg_sattop hq_X_preg = cqt_resid_loo_sattop cqt_X_preg) ///
        $covariates if year == 2018 & treatment == 1, robust
    if _rc == 0 {
        quietly lincom high_quality_avg_sattop
        post pf ("`y'") ("Main") ("PP") (r(estimate)) (r(se)) (r(p)) (r(N))
        quietly lincom high_quality_avg_sattop + hq_X_preg
        post pf ("`y'") ("Main") ("Preg") (r(estimate)) (r(se)) (r(p)) (r(N))
    }
    forvalues c = 1/6 {
        quietly capture noisily ivreg2 `y' (high_quality_avg_sattop = cqt_resid_loo_sattop) ///
            $covariates if year == 2018 & treatment == 1 & first_counselor != `c', robust
        if _rc == 0 {
            local b = _b[high_quality_avg_sattop]
            local se = _se[high_quality_avg_sattop]
            local pv = 2*(1 - normal(abs(`b'/`se')))
            local n = e(N)
            post pf ("`y'") ("dC`c'") ("Pool") (`b') (`se') (`pv') (`n')
        }
        quietly capture noisily ivreg2 `y' (high_quality_avg_sattop hq_X_preg = cqt_resid_loo_sattop cqt_X_preg) ///
            $covariates if year == 2018 & treatment == 1 & first_counselor != `c', robust
        if _rc == 0 {
            quietly lincom high_quality_avg_sattop
            post pf ("`y'") ("dC`c'") ("PP") (r(estimate)) (r(se)) (r(p)) (r(N))
            quietly lincom high_quality_avg_sattop + hq_X_preg
            post pf ("`y'") ("dC`c'") ("Preg") (r(estimate)) (r(se)) (r(p)) (r(N))
        }
    }
}
postclose pf

preserve
use /tmp/phase7a_results.dta, clear
export delimited /tmp/phase7a_results.csv, replace
list outcome spec subgroup b p, sepby(outcome) noobs
restore
log close
