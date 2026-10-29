********************************************************************************
* MASTER DO-FILE
* Paper 2: The Causal Effects of Counseling Quality on FP and Reproductive
*          Outcomes (Judge-Design IV, Malawi RCT)
*
* AUTHOR:  Marie Montas
* UPDATED: June 16, 2026
*
* PURPOSE: Reproduces every table, figure, and number in the JHE submission
*          (Main paper/JHE submission/manuscript_CQ_jhe.tex) from raw data.
*
* PREREQUISITES:
*   1. Raw data in dta/:  ANALYSIS_w1_w2_w3_2023-09-26.dta
*                         MFPS_W1_TRACK_COUNSE_FULL.dta
*                         Intervention Components Data/clinic_reimbursement_log.csv
*                         Intervention Components Data/MFPS_W1_TRACK_TRANSP_FULL.dta
*   2. Stata packages:    estout, coefplot, ivreg2, boottest, winsor2
*   3. Python 3 (csv standard library only — no extra packages)
*   4. R (for figures):   haven, dplyr, tidyr, ggplot2, patchwork, broom,
*                         scales, car, readr
*
* PIPELINE (top to bottom; ~10 min total for Stages 1-3):
*
*   STAGE 1: Data cleaning
*     1a. cleaning_counseling_02-10-2026.do  -> dta/merge_counselor_outcomes.dta
*         Builds CQT instrument + per-session and woman-level quality indices
*         using the sat+topics-engaged construction (topics gated on
*         questions answered at each session). Calls build_takeup_indicators.do.
*     1b. cleaning_outcomes_02-10-2026.do
*         Forward-fills woman-level constants and constructs all outcome
*         variables (Panels A-D, autonomy, timing intervals).
*
*   STAGE 2: Results scripts (one per paper table block)
*     2a. results_first_stage.do        -> /tmp/firststage_table.txt
*     2b. results_main_iv.do            -> /tmp/phase5_v2_results.csv
*                                          /tmp/timing_intervals_results.csv
*     2c. results_timing_mechanism.do   -> /tmp/phase6b_results.csv
*     2d. results_robust_dropcouns.do   -> /tmp/phase7a_results.csv (LOO)
*     2e. results_robust_4comp.do       -> /tmp/phase5_*_results.csv (4-comp)
*     2f. results_autonomy.do           -> /tmp/autonomy_iv_results.csv
*     2g. build_subgroup_means.do       -> /tmp/subgroup_means_*.csv
*     2h. results_robust_takeup.do      -> /tmp/robust_takeup_results.csv
*     2h2. results_summary_index.do     -> /tmp/summary_index_results.csv
*          (per-panel Anderson-2008/KLK-2007 summary-index robustness)
*     2h3. results_wildbootstrap.do     -> /tmp/wcb_results.csv
*          (wild-cluster bootstrap, Webb weights, clustering on the 6 counselors)
*     2i. results_takeup_balance.do     -> /tmp/cqt_takeup_balance.csv
*     2j. results_attrition.do          -> /tmp/attrition_by_counselor.csv,
*                                          /tmp/attrition_tests.csv
*     2j3. build_obs_summary.do        -> tables/obs_summary.csv
*          (observation counts by outcome for the main IV table; estimation N
*           matches the N rows in tab:iv_main; feeds appendix tab:obs_summary
*           via build_obs_summary.py)
*     2j2. build_balance991.do         -> tables/balance991.csv, tables/balance991_meta.csv
*          (ITT/991 balance: baseline covariates by RANDOMLY ASSIGNED first-visit
*           counselor, CQT orthogonality F, area chi2, CQT-on-attendance selection
*           check; feeds Table 1 tables/table2_balance.tex via build_balance991.py)
*     2k. build_table1_summary.do       -> /tmp/table1_summary.csv
*     2l. build_components_diagnostics.do -> writes the JHE
*         tables/quality_components_diagnostics.tex DIRECTLY (no Python builder)
*     2m. build_topic_coverage.do        -> tables/counselor_topic_analysis.csv
*         (session-level topic coverage by counselor, N=3,898; feeds Table A5
*          via build_topic_coverage.py and the appendix heatmap figure)
*
*   STAGE 3: LaTeX tables (Python builders consume /tmp/*.csv from Stage 2)
*     python3 dofile/build_latex_tables/build_first_stage.py
*     python3 dofile/build_latex_tables/build_main_iv.py
*     python3 dofile/build_latex_tables/build_timing.py
*     python3 dofile/build_latex_tables/build_timing_intervals.py
*     python3 dofile/build_latex_tables/build_robust_dropcouns.py
*     python3 dofile/build_latex_tables/build_autonomy.py
*     python3 dofile/build_latex_tables/build_robust_4comp.py
*     python3 dofile/build_latex_tables/build_robust_takeup.py
*     python3 dofile/build_latex_tables/build_summary_index.py
*     python3 dofile/build_latex_tables/build_wildbootstrap.py
*     python3 dofile/build_latex_tables/build_takeup_balance.py
*     python3 dofile/build_latex_tables/build_balance991.py  (Table 1, ITT balance)
*     python3 dofile/build_latex_tables/build_obs_summary.py  (appendix tab:obs_summary)
*     python3 dofile/build_latex_tables/build_topic_coverage.py  (Table A5)
*     python3 dofile/build_latex_tables/build_overid.py  (new identification section: tab:overid_rf/_iv/_ji)
*     python3 dofile/build_latex_tables/build_itt_rf.py  (ITT-first results: tab:itt_rf + WCB twin tab:itt_rf_wcb)
*     python3 dofile/build_latex_tables/build_iv_contq.py  (appendix continuous-q IV: tab:iv_contq)
*     NOTE: two manuscript tables have no automated builder and are
*     hand-assembled from the Stage 2 CSVs:
*       tables/attrition.tex            <- /tmp/attrition_by_counselor.csv (2j)
*       tables/table1_summary_stats.tex <- /tmp/table1_summary.csv         (2k)
*
*   STAGE 4: Export data for R + run R figure scripts (run separately)
*     do "${dofile}export_data_for_R.do"
*     do "${dofile}export_km_data.do"
*     Rscript dofile/graphs_02-12-2026.R           (balance, descriptive)
*     Rscript dofile/het_graph_examples.R          (forest plots, Figs 3-4)
*     Rscript dofile/timing_km_plot.R              (KM survival, Fig 6)
*     Rscript dofile/timing_intervals_plot.R       (interval coefplot, Fig 7)
*     Rscript dofile/drop_counselor_coefplot.R     (LOO coefplot, Fig 5)
*     Rscript dofile/appendix_topic_heatmap.R      (topic-coverage heatmap, App. fig)
*         reads tables/counselor_topic_analysis.csv (matches table6 Panel A)
*
*   (Randomization inference was tried but is NOT used: RI on the 2SLS coefficient
*    is uninformative at G=6 because the permuted instrument is weak. The script is
*    archived at dofile/Archive/superseded_2026-06-18/ri_outcomes_02-10-2026.do.
*    The wild-cluster bootstrap below is the small-G complement that the paper uses.)
*
* LOGS: All Stata logs are written to log/. Old/stray logs are in log/archive/.
********************************************************************************

clear all
set more off
set maxvar 10000

timer clear
timer on 1

* --- Paths ---
if "$MFPS_ROOT" == "" global MFPS_ROOT "`c(pwd)'"  // project folder (holds Analysis/)
if inlist("`c(os)'", "MacOSX", "Unix") {
    global base    "${MFPS_ROOT}/Analysis"
    global dta     "${base}/dta/"
    global dofile  "${base}/dofile/"
    global tables  "${base}/tables/"
    global figures "${base}/figures/"
    global log     "${base}/log/"
}

cd "${base}"
capture log close
log using "${log}master.log", replace text

di _n as text "================================================================"
di as text "  MASTER DO-FILE: Paper 2 - Counseling Quality (JHE submission)"
di as text "  Started: `c(current_date)' `c(current_time)'"
di as text "================================================================"

* ==============================================================================
* STAGE 1: DATA CLEANING
* ==============================================================================
di _n as text ">>> Stage 1a: Cleaning counseling data + building CQT..."
do "${dofile}cleaning_counseling_02-10-2026.do"

di _n as text ">>> Stage 1b: Cleaning outcome variables..."
do "${dofile}cleaning_outcomes_02-10-2026.do"

* ==============================================================================
* STAGE 2: RESULTS SCRIPTS
* ==============================================================================
di _n as text ">>> Stage 2a: First stage..."
do "${dofile}results_first_stage.do"

di _n as text ">>> Stage 2b: Main IV + timing intervals..."
do "${dofile}results_main_iv.do"

di _n as text ">>> Stage 2c: Timing mechanism..."
do "${dofile}results_timing_mechanism.do"

di _n as text ">>> Stage 2d: Leave-one-counselor-out (19-outcome appendix)..."
do "${dofile}results_robust_dropcouns.do"

di _n as text ">>> Stage 2e: 4-component robustness..."
do "${dofile}results_robust_4comp.do"

di _n as text ">>> Stage 2f: Autonomy spillover..."
do "${dofile}results_autonomy.do"

di _n as text ">>> Stage 2g: Subgroup means..."
do "${dofile}build_subgroup_means.do"

di _n as text ">>> Stage 2h: Take-up-control robustness..."
do "${dofile}results_robust_takeup.do"

di _n as text ">>> Stage 2h2: Per-panel summary-index robustness..."
do "${dofile}results_summary_index.do"

di _n as text ">>> Stage 2h3: Wild-cluster bootstrap (Webb weights, 6 counselors)..."
do "${dofile}results_wildbootstrap.do"

di _n as text ">>> Stage 2i: CQT-on-take-up balance test..."
do "${dofile}results_takeup_balance.do"

di _n as text ">>> Stage 2j: Attrition by counselor..."
do "${dofile}results_attrition.do"

di _n as text ">>> Stage 2k: Table 1 summary statistics..."
do "${dofile}build_table1_summary.do"

di _n as text ">>> Stage 2l: Quality-index component diagnostics (writes .tex directly)..."
do "${dofile}build_components_diagnostics.do"

di _n as text ">>> Stage 2m: Topic coverage by counselor (N=3,898; -> CSV for Table A5 + heatmap)..."
do "${dofile}build_topic_coverage.do"

di _n as text ">>> Stage 2m2: Overidentification tests (RF joint F, Hansen J, just-identified; July 2026)..."
*  Continuous first-session q instrumented by the five counselor dummies.
*  Writes /tmp/overid_{rf,iv,ji}.csv (consumed by build_latex_tables/build_overid.py).
do "${dofile}results_overid.do"

di _n as text ">>> Stage 2m3: ITT reduced-form table + WCB twin (August 2026 restructure)..."
*  RF on worst-to-best-scaled CQT (pooled/PP/Preg) + counselor-dummy joint F +
*  wild-cluster-bootstrap p-values. Writes /tmp/itt_rf.csv (-> build_itt_rf.py).
do "${dofile}results_itt_rf.do"

di _n as text ">>> Stage 2m4: Continuous first-session-q appendix IV (August 2026)..."
*  Main IV panels with continuous first-session q endogenous, CQT instrument.
*  Writes /tmp/iv_contq.csv (-> build_iv_contq.py).
do "${dofile}results_iv_contq.do"

di _n as text ">>> Stage 2n: Cost-effectiveness causal input (PNAS treatment-vs-control ITT)..."
*  Reads the PNAS replication data (one level up from Analysis/) and writes
*  CEA/outputs/cea_itt.csv (the original-trial ITT effect on contraceptive use,
*  consumed by build_cea_costs.py for the cost-per-additional-user reference metric).
do "${dofile}results_cea_itt.do"

* ==============================================================================
* STAGE 3: LATEX TABLES (Python builders)
* ==============================================================================
* The Python builders are deterministic, take seconds, and read from /tmp/.
* They are intentionally run from the shell rather than from Stata to keep
* logs clean and make it easy to rebuild a single table after editing.
di _n as text ">>> Stage 3: Run the nine Python builders from the shell:"
di as text "    python3 dofile/build_latex_tables/build_first_stage.py"
di as text "    python3 dofile/build_latex_tables/build_main_iv.py"
di as text "    python3 dofile/build_latex_tables/build_timing.py"
di as text "    python3 dofile/build_latex_tables/build_timing_intervals.py"
di as text "    python3 dofile/build_latex_tables/build_robust_dropcouns.py"
di as text "    python3 dofile/build_latex_tables/build_autonomy.py"
di as text "    python3 dofile/build_latex_tables/build_robust_4comp.py"
di as text "    python3 dofile/build_latex_tables/build_robust_takeup.py"
di as text "    python3 dofile/build_latex_tables/build_takeup_balance.py"
di as text "    python3 dofile/build_cea_costs.py            (CEA cost/CYP/throughput -> CEA/outputs/*.csv)"
di as text "    python3 dofile/build_latex_tables/build_cea_combined.py    (Table tab:cea, Panels A+B)"
di _n as text "    (build_components_diagnostics.do in Stage 2l already wrote its .tex.)"
di as text "    (attrition.tex and table1_summary_stats.tex have NO automated builder;"
di as text "     assemble them by hand from /tmp/attrition_by_counselor.csv and"
di as text "     /tmp/table1_summary.csv.)"

* ==============================================================================
* STAGE 4: DATA EXPORTS FOR R FIGURES
* ==============================================================================
di _n as text ">>> Stage 4: Exporting data for R figures..."
do "${dofile}export_data_for_R.do"
do "${dofile}export_km_data.do"

di _n as text ">>> Stage 4 follow-up: Run the five R scripts from the shell:"
di as text "    Rscript dofile/graphs_02-12-2026.R          (balance + descriptive)"
di as text "    Rscript dofile/het_graph_examples.R         (forest plots Figs 3-4)"
di as text "    Rscript dofile/timing_km_plot.R             (KM survival Fig 6)"
di as text "    Rscript dofile/timing_intervals_plot.R      (interval coefplot Fig 7)"
di as text "    Rscript dofile/drop_counselor_coefplot.R    (LOO coefplot Fig 5)"
di as text "    Rscript dofile/appendix_topic_heatmap.R     (topic-coverage heatmap, appendix)"

* ==============================================================================
* STAGE 5 (OPTIONAL): RANDOMIZATION INFERENCE
* ==============================================================================
* WARNING: ~2 hours with K=5000 permutations. Uncomment to include in the run.
* do "${dofile}ri_outcomes_02-10-2026.do"

* ==============================================================================
* DONE
* ==============================================================================
timer off 1
timer list 1

di _n as text "================================================================"
di as text "  MASTER DO-FILE COMPLETE"
di as text "  Finished: `c(current_date)' `c(current_time)'"
di as text "================================================================"

log close
