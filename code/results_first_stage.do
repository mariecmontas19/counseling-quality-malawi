capture file close f
log using /tmp/firststage4.log, replace text
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

local cols 1 2 3 4
local lab1 "Unadj Pooled"
local lab2 "Adj Pooled"
local lab3 "Adj PP"
local lab4 "Adj Preg"

* Pre-compute everything
foreach c of local cols {
    if `c' == 1 {
        local cov ""
        local subs "if year == 2018 & treatment == 1"
    }
    if `c' == 2 {
        local cov "$covariates"
        local subs "if year == 2018 & treatment == 1"
    }
    if `c' == 3 {
        local cov "$covariates_no_preg"
        local subs "if year == 2018 & treatment == 1 & preg_2016 == 0"
    }
    if `c' == 4 {
        local cov "$covariates_no_preg"
        local subs "if year == 2018 & treatment == 1 & preg_2016 == 1"
    }
    
    * First stage: regression of D on Z (and controls) on the maximum analytic
    * sample. We do NOT condition on any specific outcome here, since the first
    * stage characterizes the instrument's relevance independently of Y.
    quietly reg high_quality_avg_sattop cqt_resid_loo_sattop `cov' `subs', robust
    local b`c'  = _b[cqt_resid_loo_sattop]
    local se`c' = _se[cqt_resid_loo_sattop]
    local n`c'  = e(N)
    local r2_`c' = e(r2)
    local p`c' = 2*(1 - normal(abs(`b`c''/`se`c'')))

    * KP rk Wald F, KP rk LM chi2, Cragg-Donald F: computed from ivreg2 using
    * admin_fp_any as the outcome (no additional sample restriction beyond
    * `subs' and the covariate non-missingness, since admin_fp_any is defined
    * for all endline-present treatment-arm women).
    quietly ivreg2 admin_fp_any (high_quality_avg_sattop = cqt_resid_loo_sattop) `cov' `subs', robust
    local KPF`c' = e(rkf)
    local KPLM`c' = e(idstat)
    local CDF`c' = e(cdf)
    
    local s`c' = ""
    if `p`c'' < 0.10 local s`c' "*"
    if `p`c'' < 0.05 local s`c' "**"
    if `p`c'' < 0.01 local s`c' "***"
}

file open f using /tmp/firststage_table.txt, write replace
file write f "FIRST STAGE — Endog: high_quality_avg_sattop on Instrument: cqt_resid_loo_sattop" _n _n
file write f "Statistic                     |  (1) Unadj Pool |  (2) Adj Pool  |  (3) Adj PP   |  (4) Adj Preg" _n
file write f "------------------------------+-----------------+----------------+---------------+----------------" _n

local row_b "CQT (sat+topics)"
local row_se "  Robust SE"
local row_klm "Kleibergen-Paap rk LM chi2"
local row_kpf "Kleibergen-Paap rk Wald F"
local row_cdf "Cragg-Donald F"
local row_r2 "R-squared"
local row_n "N"

file write f "`row_b'              "
foreach c of local cols {
    file write f "| " %8.3f (`b`c'') "`s`c''       "
}
file write f _n

file write f "`row_se'                  "
foreach c of local cols {
    file write f "| (" %5.3f (`se`c'') ")         "
}
file write f _n _n

file write f "`row_klm'    "
foreach c of local cols {
    file write f "| " %9.2f (`KPLM`c'') "       "
}
file write f _n

file write f "`row_kpf'     "
foreach c of local cols {
    file write f "| " %9.2f (`KPF`c'') "       "
}
file write f _n

file write f "`row_cdf'                "
foreach c of local cols {
    file write f "| " %9.2f (`CDF`c'') "       "
}
file write f _n _n

file write f "`row_r2'                     "
foreach c of local cols {
    file write f "| " %9.3f (`r2_`c'') "       "
}
file write f _n

file write f "`row_n'                             "
foreach c of local cols {
    file write f "| " %5.0f (`n`c'') "           "
}
file write f _n _n

file write f "Notes: HC1 heteroskedasticity-robust SE. Stock-Yogo 10% maximal-IV-size CV = 16.38." _n
file write f "Stars: * p<0.10, ** p<0.05, *** p<0.01." _n
file close f
log close
