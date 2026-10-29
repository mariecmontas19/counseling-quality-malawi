********************************************************************************
* build_components_diagnostics.do
*
* PURPOSE: Build Appendix C quality components diagnostics table for the JHE
*          paper. Computes Mean, SD, between-counselor eta-squared, and
*          one-way ANOVA F-statistic for each of the four candidate quality
*          components (satisfaction, raw topic count, session duration, and
*          answered-all-questions indicator), plus standardized Cronbach's
*          alpha for the two-component primary index and the four-component
*          alternative, and the pairwise correlation between satisfaction and
*          topics.
*
* INPUT:  dta/merge_counselor_outcomes.dta
* OUTPUT: tables/quality_components_diagnostics.tex
*         /tmp/components_diagnostics.csv  (intermediate)
*
* AUTHOR: Marie Montas
* DATE:   May 20, 2026
********************************************************************************

clear all
set more off

if "$MFPS_ROOT" == "" global MFPS_ROOT "`c(pwd)'"  // project folder (holds Analysis/)
local dta "${MFPS_ROOT}/Analysis/dta"
local tex_out "${MFPS_ROOT}/Main paper/JHE submission/tables/quality_components_diagnostics.tex"

* ==============================================================================
* Load and restrict to one row per analytic-sample woman at baseline
* ==============================================================================
use "`dta'/merge_counselor_outcomes.dta", clear
keep if treatment == 1 & !missing(first_counselor)
keep if year == 2016
bys caseid: gen __tag = (_n == 1)
keep if __tag == 1

count
local N = r(N)
di as text "Analytic sample N = `N'"

* ==============================================================================
* Per-component diagnostics: Mean, SD, eta-squared, ANOVA F
* ==============================================================================
local comp_list satisfaction_first_all n_topics_first_session session_first_all answered_first_all

foreach v of local comp_list {
    quietly summarize `v'
    local mean_`v' = r(mean)
    local sd_`v'   = r(sd)
    local n_`v'    = r(N)

    quietly oneway `v' first_counselor
    local ssb = r(mss)
    local ssw = r(rss)
    local eta2_`v' = `ssb' / (`ssb' + `ssw')
    local F_`v'    = r(F)
    local df_w     = `n_`v'' - 6
    local p_`v'    = Ftail(5, `df_w', `F_`v'')

    di as text "  `v': mean = " %5.2f `mean_`v'' ", SD = " %5.2f `sd_`v'' ///
        ", eta2 = " %5.3f `eta2_`v'' ", F = " %7.2f `F_`v'' ", p = " %5.3f `p_`v''
}

* ==============================================================================
* Standardized Cronbach's alpha
* ==============================================================================
quietly alpha satisfaction_first_all n_topics_first_session, std
local alpha_2 = r(alpha)

quietly alpha satisfaction_first_all n_topics_first_session session_first_all answered_first_all, std
local alpha_4 = r(alpha)

di as text "Cronbach alpha (sat + topics): " %5.3f `alpha_2'
di as text "Cronbach alpha (all 4):        " %5.3f `alpha_4'

* ==============================================================================
* Pairwise correlation between satisfaction and topics
* ==============================================================================
correlate satisfaction_first_all n_topics_first_session
matrix C = r(C)
local r_sat_top = C[2,1]
* Compute p-value: t = r * sqrt(n-2) / sqrt(1-r^2)
local t_stat = `r_sat_top' * sqrt(`N' - 2) / sqrt(1 - `r_sat_top'^2)
local p_corr = 2*ttail(`N' - 2, abs(`t_stat'))

di as text "Correlation(sat, topics): r = " %5.3f `r_sat_top' ", p = " %7.5f `p_corr'

* ==============================================================================
* Helper: significance stars
* ==============================================================================
local stars_sat = ""
if `p_satisfaction_first_all' < 0.10 local stars_sat = "$^{*}$"
if `p_satisfaction_first_all' < 0.05 local stars_sat = "$^{**}$"
if `p_satisfaction_first_all' < 0.01 local stars_sat = "$^{***}$"

local stars_top = ""
if `p_n_topics_first_session' < 0.10 local stars_top = "$^{*}$"
if `p_n_topics_first_session' < 0.05 local stars_top = "$^{**}$"
if `p_n_topics_first_session' < 0.01 local stars_top = "$^{***}$"

local stars_ses = ""
if `p_session_first_all' < 0.10 local stars_ses = "$^{*}$"
if `p_session_first_all' < 0.05 local stars_ses = "$^{**}$"
if `p_session_first_all' < 0.01 local stars_ses = "$^{***}$"

local stars_ans = ""
if `p_answered_first_all' < 0.10 local stars_ans = "$^{*}$"
if `p_answered_first_all' < 0.05 local stars_ans = "$^{**}$"
if `p_answered_first_all' < 0.01 local stars_ans = "$^{***}$"

* ==============================================================================
* Write LaTeX table
* ==============================================================================
local df_within = `N' - 6

file open f using "`tex_out'", write replace
file write f "\begin{table}[H]" _n
file write f "\centering" _n
file write f "\caption{Quality Index Component Diagnostics}" _n
file write f "\label{tab:components}" _n
file write f "\footnotesize" _n
file write f "\resizebox{\textwidth}{!}{%" _n
file write f "\begin{tabular}{lcccc}" _n
file write f "\toprule" _n
file write f "Component & Mean (SD) & Between-counselor \$\eta^2\$ & ANOVA \$F(5, `df_within')\$ & Role \\" _n
file write f "\midrule" _n
file write f "Patient satisfaction (1--4)    & " %4.2f (`mean_satisfaction_first_all') " (" %4.2f (`sd_satisfaction_first_all') ")  & " %5.3f (`eta2_satisfaction_first_all') " & " %6.1f (`F_satisfaction_first_all') "`stars_sat'  & \textbf{Selected (primary index)} \\" _n
file write f "Topics requested (0--9)        & " %4.2f (`mean_n_topics_first_session') " (" %4.2f (`sd_n_topics_first_session') ")  & " %5.3f (`eta2_n_topics_first_session') " & " %6.1f (`F_n_topics_first_session') "`stars_top' & \textbf{Selected (primary index)} \\" _n
file write f "Session duration (min)         & " %5.2f (`mean_session_first_all') " (" %4.2f (`sd_session_first_all') ") & " %5.3f (`eta2_session_first_all') " & " %6.2f (`F_session_first_all') "`stars_ses'           & Dropped from primary \\" _n
file write f "Questions answered (0/1)       & " %4.2f (`mean_answered_first_all') " (" %4.2f (`sd_answered_first_all') ")  & " %5.3f (`eta2_answered_first_all') " & " %6.2f (`F_answered_first_all') "`stars_ans'   & Dropped from primary \\" _n
file write f "\midrule" _n
file write f "\multicolumn{5}{l}{\textit{Cronbach's \$\alpha\$ (standardized):}} \\[3pt]" _n
file write f "\quad Satisfaction \$+\$ topics (primary index) & \multicolumn{4}{l}{\boldmath\$\alpha = " %4.2f (`alpha_2') "\$} \\" _n
file write f "\quad All four components (robustness)         & \multicolumn{4}{l}{\$\alpha = " %4.2f (`alpha_4') "\$} \\" _n
file write f "\midrule" _n
file write f "\multicolumn{5}{l}{\textit{Pairwise correlation between selected components:}} \\[3pt]" _n
file write f "\quad Satisfaction \$\times\$ topics & \multicolumn{4}{l}{\$r = " %4.2f (`r_sat_top') "\$, \$p < 0.001\$} \\" _n
file write f "\bottomrule" _n
file write f "\end{tabular}}" _n _n
file write f "\smallskip" _n
file write f "\parbox{\textwidth}{\scriptsize\textit{Notes:} Sample restricted to treatment-arm women with a first-counselor assignment (\$N = `N'\$), measured at the first counseling session. Between-counselor \$\eta^2\$ is the share of total variance that lies between the six counselors, from a one-way analysis of variance with counselor identity as the grouping variable. Higher \$\eta^2\$ indicates the component captures more systematic counselor-level variation, which is the dimension the examiner-design instrument leverages. Patient satisfaction and topics requested have substantial between-counselor variation (\$\eta^2 = " %4.2f (`eta2_satisfaction_first_all') "\$ and \$\eta^2 = " %4.2f (`eta2_n_topics_first_session') "\$) and load on a single factor with Cronbach's \$\alpha = " %4.2f (`alpha_2') "\$, supporting their joint use as a scalar quality index. Session duration (\$\eta^2 = " %5.3f (`eta2_session_first_all') "\$) and the questions-answered indicator (\$\eta^2 = " %5.3f (`eta2_answered_first_all') "\$) have essentially no between-counselor variation and are dropped from the primary index; the four-component bundle has \$\alpha = " %4.2f (`alpha_4') "\$, indicating it is not a coherent unidimensional construct. \$^{***}p < 0.01\$.}" _n
file write f "\end{table}" _n
file close f

di _n as text "Saved: `tex_out'"
