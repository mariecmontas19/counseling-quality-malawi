capture file close f
log using /tmp/phase6b_timing.log, replace text
use "dta/merge_counselor_outcomes.dta", clear

quietly summarize avg_quality_sattop if treatment == 1, detail
local p50 = r(p50)
capture drop high_quality_avg_sattop
gen high_quality_avg_sattop = (avg_quality_sattop > `p50') if !missing(avg_quality_sattop) & treatment == 1

global covariates_no_preg ///
    i.age_group_bl sex_age_bl total_alive_bl ever_use_bl ///
    edu_primary_bl work_bl religion_r_bl i.area_bl ethnicity_r_bl ///
    curr_use_bl counselLP_bl

postfile pf str20 outcome str8 panel byte grp double b double se double p double n double hq_mn double lq_mn ///
    using /tmp/phase6b_results.dta, replace

* Panel A: PP timing split (recent ≤2.9 mo postpartum vs older)
foreach y in admin_fp_any admin_inject_any n_admin_fp fp_inj_coverage_frac fp_inj_protected_days birth_within_24mo {
    foreach grp in 0 1 {
        quietly capture noisily ivreg2 `y' (high_quality_avg_sattop = cqt_resid_loo_sattop) ///
            $covariates_no_preg if year == 2018 & treatment == 1 & preg_2016 == 0 & pp_late == `grp', robust
        local b = _b[high_quality_avg_sattop]
        local se = _se[high_quality_avg_sattop]
        local pv = 2*(1 - normal(abs(`b'/`se')))
        local n = e(N)
        quietly summarize `y' if year == 2018 & treatment == 1 & preg_2016 == 0 & pp_late == `grp' & high_quality_avg_sattop == 1, meanonly
        local hq = r(mean)
        quietly summarize `y' if year == 2018 & treatment == 1 & preg_2016 == 0 & pp_late == `grp' & high_quality_avg_sattop == 0, meanonly
        local lq = r(mean)
        post pf ("`y'") ("PP") (`grp') (`b') (`se') (`pv') (`n') (`hq') (`lq')
    }
}

* Panel B: Preg timing split (3rd-tri ≤2.6 mo to delivery vs earlier)
foreach y in admin_fp_any admin_inject_any n_admin_fp fp_inj_coverage_frac fp_inj_protected_days preg_within_24mo {
    foreach grp in 0 1 {
        quietly capture noisily ivreg2 `y' (high_quality_avg_sattop = cqt_resid_loo_sattop) ///
            $covariates_no_preg if year == 2018 & treatment == 1 & preg_2016 == 1 & preg_late == `grp', robust
        local b = _b[high_quality_avg_sattop]
        local se = _se[high_quality_avg_sattop]
        local pv = 2*(1 - normal(abs(`b'/`se')))
        local n = e(N)
        quietly summarize `y' if year == 2018 & treatment == 1 & preg_2016 == 1 & preg_late == `grp' & high_quality_avg_sattop == 1, meanonly
        local hq = r(mean)
        quietly summarize `y' if year == 2018 & treatment == 1 & preg_2016 == 1 & preg_late == `grp' & high_quality_avg_sattop == 0, meanonly
        local lq = r(mean)
        post pf ("`y'") ("Preg") (`grp') (`b') (`se') (`pv') (`n') (`hq') (`lq')
    }
}
postclose pf

preserve
use /tmp/phase6b_results.dta, clear
export delimited /tmp/phase6b_results.csv, replace
restore

log close
