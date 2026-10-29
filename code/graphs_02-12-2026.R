################################################################################
# GRAPHS: Paper 2 - Counseling Quality on FP/Reproductive Outcomes
# Malawi RCT Judge-Design IV
#
# CREATED:  Marie Montas, 12 Feb 2026
# INPUTS:   dta/merge_counselor_outcomes.dta (after cleaning dofiles run)
# OUTPUTS:  figures/*.pdf (publication-quality graphs)
#
# STRUCTURE:
#   1. Setup and data load
#   2. Balance coefplot (Panel A: covariates, Panel B: outcomes)
#   3. Quality components by counselor (box + bar)
#   4. Quality index by counselor (scatter + CI)
#   5. CQT instrument + quality index combined panel
#   6. First stage scatter
#   7. Forest plot: IV main effects (two-panel: continuity + standard)
#   8. Forest plot: Reduced form effects (two-panel: continuity + standard)
#   9. Heterogeneity graphs: pregnancy status
#  10. Heterogeneity graphs: fertility preference
#  11. Linearity diagnostics: binned scatter + LOESS
#  12. Binned scatter: CQT vs unintended pregnancy
#  13. Session dosage by CQT x pregnancy status
#  14. Counselor topic heatmap
#  15. Content exposure funnel
################################################################################

# ==============================================================================
# 1. SETUP
# ==============================================================================

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(broom)
library(scales)
library(car)

# Paths
base_path <- file.path(
  path.expand("~"),
  "Library/CloudStorage/Dropbox/PhD Harvard/Dissertation",
  "Malawi paper/Counseling Qual MFPS paper/Analysis"
)
dta_path     <- file.path(base_path, "dta")
figures_path <- file.path(
  path.expand("~"),
  "Library/CloudStorage/Dropbox/PhD Harvard/Dissertation",
  "Malawi paper/Counseling Qual MFPS paper/Main paper/Preliminary results/figures"
)
tables_path  <- file.path(base_path, "tables")

dir.create(figures_path, showWarnings = FALSE, recursive = TRUE)

# Theme
theme_paper <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey92"),
    panel.grid.major.x = element_blank(),
    axis.line.x = element_line(color = "grey40"),
    axis.ticks.x = element_line(color = "grey40"),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    strip.text = element_text(face = "bold")
  )
theme_set(theme_paper)

# Load data
df <- read_dta(file.path(dta_path, "merge_counselor_outcomes.dta"))

# Create baseline fertility preference (same logic as Stata)
df <- df %>%
  group_by(caseid) %>%
  mutate(
    fertility_pref_bl = max(ifelse(year == 2016 & !is.na(fertility_pref),
                                   fertility_pref, NA_real_), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(fertility_pref_bl = ifelse(is.infinite(fertility_pref_bl),
                                     NA_real_, fertility_pref_bl))

# Restrict to treatment group only (mirrors Stata analysis)
df <- df %>% filter(treatment == 1)

# Convert categorical variables to factors for balance figure
df <- df %>% mutate(
  age_group_bl   = factor(age_group_bl),
  edu_primary_bl = factor(edu_primary_bl),
  religion_r_bl  = factor(religion_r_bl),
  ethnicity_r_bl = factor(ethnicity_r_bl)
)

# --- NEW QUALITY INDEX: 4-component equal-weighted standardized average ---
# Components: satisfaction, topics requested, session duration, questions answered
# (replaces old 3-component index; topics was added as 4th component)

# Tag one obs per woman (mirrors Stata _tag)
df <- df %>%
  group_by(caseid) %>%
  mutate(.tag = row_number() == 1) %>%
  ungroup()

# Z-score each component using treatment women means/SDs (first obs only)
sat_mean  <- mean(df$satisfaction_first_all[df$.tag], na.rm = TRUE)
sat_sd    <- sd(df$satisfaction_first_all[df$.tag], na.rm = TRUE)
top_mean  <- mean(df$n_topics_first_session[df$.tag], na.rm = TRUE)
top_sd    <- sd(df$n_topics_first_session[df$.tag], na.rm = TRUE)
dur_mean  <- mean(df$session_first_all[df$.tag], na.rm = TRUE)
dur_sd    <- sd(df$session_first_all[df$.tag], na.rm = TRUE)
ans_mean  <- mean(df$answered_first_all[df$.tag], na.rm = TRUE)
ans_sd    <- sd(df$answered_first_all[df$.tag], na.rm = TRUE)

df <- df %>% mutate(
  z_sat      = (satisfaction_first_all - sat_mean) / sat_sd,
  z_topics   = (n_topics_first_session - top_mean) / top_sd,
  z_duration = (session_first_all - dur_mean) / dur_sd,
  z_answered = (answered_first_all - ans_mean) / ans_sd,
  # PRIMARY (May 2026): sat + topics only; duration and answered are dropped
  quality_index_new = (z_sat + z_topics) / 2
)

cat("New quality index (sat + topics): sat mean =", round(sat_mean, 3),
    ", topics mean =", round(top_mean, 3),
    ", duration mean =", round(dur_mean, 3),
    ", answered mean =", round(ans_mean, 3), "\n")

# Residualize on scheduling FEs, then compute LOO CQT
df_resid <- df %>%
  filter(.tag,
         !is.na(quality_index_new),
         !is.na(first_counselor),
         !is.na(first_couns_dow),
         !is.na(first_couns_month),
         !is.na(first_couns_hour))

fe_fit <- lm(quality_index_new ~ factor(first_couns_dow) +
               factor(first_couns_month) + factor(first_couns_hour),
             data = df_resid)
df_resid$qresid <- residuals(fe_fit)

# LOO CQT by counselor
df_resid <- df_resid %>%
  group_by(first_counselor) %>%
  mutate(
    sum_resid = sum(qresid),
    n_couns   = n(),
    cqt_new   = (sum_resid - qresid) / (n_couns - 1)
  ) %>%
  ungroup() %>%
  select(caseid, cqt_new)

# Merge CQT back (carries to all years via caseid join)
df <- df %>%
  left_join(df_resid, by = "caseid")

cat("CQT constructed: N =", sum(!is.na(df$cqt_new[df$.tag])), "\n")

# Binary endogenous: above-median first-session quality (new index)
med_quality_new <- median(df$quality_index_new[df$.tag], na.rm = TRUE)
df <- df %>% mutate(
  high_quality_new = ifelse(!is.na(quality_index_new),
                            as.integer(quality_index_new > med_quality_new),
                            NA_integer_)
)

# Keep old high_quality_first variable for backward-compat (forest plots, het)
med_quality <- median(df$quality_index_first, na.rm = TRUE)
df <- df %>% mutate(
  high_quality_first = ifelse(!is.na(quality_index_first),
                              as.integer(quality_index_first > med_quality), NA_integer_)
)

# May 2026 update: PRIMARY endogenous variable is now `high_quality_avg_sattop`
# (above-median woman-level mean of sat+topics per-session quality). The Stata
# *results* scripts construct it internally from avg_quality_sattop rather than
# exporting it, so we build it here (above the treatment-sample median) for the
# descriptive figures below. `df` is already restricted to treatment women.
df <- df %>% mutate(
  high_quality_avg_sattop = ifelse(!is.na(avg_quality_sattop),
                                   as.integer(avg_quality_sattop >
                                              median(avg_quality_sattop, na.rm = TRUE)),
                                   NA_integer_)
)
cat("high_quality_avg_sattop (from Stata): mean =",
    round(mean(df$high_quality_avg_sattop, na.rm = TRUE), 3),
    ", N(1) =", sum(df$high_quality_avg_sattop == 1, na.rm = TRUE), "\n")

cat("high_quality_new: median =", round(med_quality_new, 3),
    ", N(1) =", sum(df$high_quality_new == 1, na.rm = TRUE), "\n")

# Convenience: endline subset
df18 <- df %>% filter(year == 2018)
df16 <- df %>% filter(year == 2016)

# Outcome labels
# UPDATED 2026-05-09: Final 19-outcome 4-panel set under sat+topics primary
# spec. Panel A = Reproductive, Panel B = FP coverage, Panel C = Continuity,
# Panel D = Topics. Legacy variables retained at the bottom for back-compat.
outcome_labels <- c(
  # Panel A: Reproductive (4)
  birth_within_24mo        = "Birth within 24 months",
  preg_within_12mo         = "Pregnancy within 12 months",
  preg_within_24mo         = "Pregnancy within 24 months",
  mistimed_preg_expost     = "Mistimed pregnancy",
  # Panel B: FP coverage (5)
  admin_fp_any             = "Any FP service",
  admin_inject_any         = "Any injectable",
  n_admin_fp               = "Number of FP services",
  fp_inj_coverage_frac     = "Injectable coverage",
  husband_knows_fp         = "Husband knows FP",
  # Panel C: Continuity (4)
  total_sessions           = "Total sessions",
  share_same_counselor     = "Same counselor share",
  visit_rate_mo            = "Counseling sessions per month",
  husband_present          = "Husband present",
  # Panel D: Topics post-1st-session (6)
  etp1_fp                  = "FP info",
  etp1_spacing             = "Birth spacing",
  etp1_preg                = "Pregnancy info",
  etp1_partner             = "Partner engagement",
  etp1_childhealth         = "Child health",
  etp1_breast              = "Breastfeeding",
  # Legacy / robustness (kept for figures that still reference them)
  fertility_pref_bl        = "Wants more children (BL)"
)

# Covariate names (no FE notation)
covariate_vars <- c("age_group_bl", "sex_age_bl", "total_alive_bl",
                     "ever_use_bl", "edu_primary_bl", "work_bl",
                     "religion_r_bl", "ethnicity_r_bl", "preg_2016",
                     "curr_use_bl", "counselLP_bl")

covariate_labels <- c(
  age_group_bl1   = "Age: <=20",
  age_group_bl2   = "Age: 21-25",
  age_group_bl3   = "Age: 26-30",
  sex_age_bl      = "Age at sex debut",
  total_alive_bl  = "Children alive",
  ever_use_bl     = "Ever used FP",
  edu_primary_bl1 = "Educ: Primary or less",
  work_bl         = "Working",
  religion_r_bl1  = "Religion: Christian",
  ethnicity_r_bl1 = "Ethnicity: Chewa",
  preg_2016       = "Pregnant at BL",
  curr_use_bl     = "FP use at BL",
  counselLP_bl    = "Counseled last preg"
)

# Outcomes available at baseline (full sample, no conditional vars)
# husband_knows_fp, decision_contraception only ~192 obs (conditional on FP use)
# discussed_fp_husband = 0 obs at BL
# Birth outcomes are post-treatment, cannot be used for balance
balouts_bl <- c("curr_use_bl", "fertility_pref_bl",
                "preg_likely_nofp", "preg_feel_positive",
                "same_number_children")

# Main outcomes for analysis
outcomes_main <- names(outcome_labels)

cat("Data loaded:", nrow(df), "rows,", nrow(df18), "endline obs\n")

# ==============================================================================
# 2. VALIDATION: RANDOM COUNSELOR ASSIGNMENT (Panel A + B)
# ==============================================================================
# Mirrors judge-design validation:
#   Panel A: baseline characteristics -> endogenous variable
#            (avg_quality_sattop: woman-level average quality across all sessions)
#   Panel B: baseline characteristics -> instrument
#            (cqt_resid_loo_sattop: LOO residualized CQT instrument)
# If assignment is random, Panel A may show predictors (quality is
# endogenous) but Panel B should show all near-zero (instrument is exogenous).
#
# Only the substantive controls from the IV regression covariate set appear
# here. Baseline outcomes (e.g., fertility_pref_bl, preg_likely_nofp) are NOT
# in the regression covariate set and are therefore excluded from the figure.

# Only the substantive controls; no baseline outcomes
all_baseline_vars   <- covariate_vars
all_baseline_labels <- covariate_labels

# Panel A: Average woman-level quality (across all attended sessions) ~ baseline covariates
fmla_quality <- as.formula(paste("avg_quality_sattop ~",
                                  paste(all_baseline_vars, collapse = " + ")))
fit_quality <- lm(fmla_quality, data = df18)
tidy_quality <- tidy(fit_quality, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(label = all_baseline_labels[term],
         label = ifelse(is.na(label), term, label))

# Joint F-test: all baseline covariates
ftest_qual <- summary(fit_quality)$fstatistic
fval_qual  <- ftest_qual[1]
pval_qual  <- pf(ftest_qual[1], ftest_qual[2], ftest_qual[3], lower.tail = FALSE)

# Panel B: CQT instrument ~ baseline covariates
fmla_cqt <- as.formula(paste("cqt_resid_loo_sattop ~",
                              paste(all_baseline_vars, collapse = " + ")))
fit_cqt <- lm(fmla_cqt, data = df18)
tidy_cqt <- tidy(fit_cqt, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(label = all_baseline_labels[term],
         label = ifelse(is.na(label), term, label))

# Joint F-test: all baseline covariates
ftest_cqt <- summary(fit_cqt)$fstatistic
fval_cqt  <- ftest_cqt[1]
pval_cqt  <- pf(ftest_cqt[1], ftest_cqt[2], ftest_cqt[3], lower.tail = FALSE)

# Y-axis order: control variables grouped logically (bottom-to-top in ggplot)
var_display_order <- c(
  "FP use at BL", "Counseled last preg", "Pregnant at BL",
  "Ethnicity: Chewa",
  "Religion: Christian",
  "Working",
  "Educ: Primary or less",
  "Ever used FP", "Children alive", "Age at sex debut",
  "Age: 26-30", "Age: 21-25", "Age: <=20"
)

tidy_quality <- tidy_quality %>% mutate(
  label = factor(label, levels = var_display_order)
) %>% filter(!is.na(label))
tidy_cqt <- tidy_cqt %>% mutate(
  label = factor(label, levels = var_display_order)
) %>% filter(!is.na(label))

# Panel A plot
p_val_quality <- ggplot(tidy_quality, aes(x = estimate, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2, color = "firebrick", linewidth = 0.6) +
  geom_point(color = "grey40", size = 2) +
  scale_x_continuous(breaks = seq(-0.5, 0.5, by = 0.1),
                     limits = c(-0.5, 0.5), oob = scales::squish) +
  labs(x = "Coefficient estimate", y = NULL,
       title = "Panel A: Average Quality Across All Sessions",
       subtitle = sprintf("Joint F = %.2f, p = %.3f",
                           fval_qual, pval_qual)) +
  theme(plot.margin = margin(5, 15, 5, 5))

# Panel B plot
p_val_cqt <- ggplot(tidy_cqt, aes(x = estimate, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2, color = "navy", linewidth = 0.6) +
  geom_point(color = "grey40", size = 2) +
  scale_x_continuous(breaks = seq(-0.5, 0.5, by = 0.1),
                     limits = c(-0.5, 0.5), oob = scales::squish) +
  labs(x = "Coefficient estimate", y = NULL,
       title = "Panel B: Counselor Quality Tendency (CQT)",
       subtitle = sprintf("Joint F = %.2f, p = %.3f",
                           fval_cqt, pval_cqt)) +
  theme(plot.margin = margin(5, 15, 5, 5))

p_validation <- p_val_quality + p_val_cqt +
  plot_annotation(
    title = "Balance: Baseline Controls vs. Average Quality and CQT Instrument"
  )

ggsave(file.path(figures_path, "balance_instrument_baseline.pdf"),
       p_validation, width = 13, height = 7, dpi = 300)
cat("Saved: balance_instrument_baseline.pdf\n")

# ==============================================================================
# 3. QUALITY COMPONENTS BY COUNSELOR (box + bar)
# ==============================================================================

# Bar chart helper with CI
bar_ci_plot <- function(data, varname, title_str, fill_color = "steelblue",
                        subtitle_str = NULL, y_label = "Mean") {
  data %>%
    filter(!is.na(.data[[varname]]), !is.na(first_counselor)) %>%
    group_by(first_counselor) %>%
    summarise(
      mean_val = mean(.data[[varname]], na.rm = TRUE),
      se_val   = sd(.data[[varname]], na.rm = TRUE) / sqrt(n()),
      n        = n(),
      .groups  = "drop"
    ) %>%
    mutate(
      tcrt = qt(0.975, n - 1),
      lci  = mean_val - tcrt * se_val,
      uci  = mean_val + tcrt * se_val,
      counselor = factor(first_counselor)
    ) %>%
    ggplot(aes(x = counselor, y = mean_val)) +
    geom_col(fill = fill_color, alpha = 0.7, width = 0.6) +
    geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.2) +
    labs(x = "First Counselor", y = y_label, title = title_str,
         subtitle = subtitle_str)
}

# Panel A: Patient satisfaction (bar + CI)
p_bar_sat <- bar_ci_plot(df, "satisfaction_first_all",
                         expression(bold("A. Patient Satisfaction")),
                         fill_color = "navy",
                         subtitle_str = expression(paste("Between-counselor ", eta^2, " = 0.60")),
                         y_label = "Mean Satisfaction (1-4)")

# Panel B: Topics discussed (bar + CI) — mean of n_topics_first_session by counselor
p_bar_topics <- bar_ci_plot(df, "n_topics_first_session",
                            expression(bold("B. Topics Discussed")),
                            fill_color = "firebrick",
                            subtitle_str = expression(paste("Between-counselor ", eta^2, " = 0.90")),
                            y_label = "Mean Topics (0-9)")

# Panel C: Session duration (boxplot)
p_box_session <- df %>%
  filter(!is.na(session_first_all), !is.na(first_counselor)) %>%
  mutate(counselor = factor(first_counselor)) %>%
  ggplot(aes(x = counselor, y = session_first_all)) +
  geom_boxplot(fill = "grey85", outlier.size = 0.8) +
  labs(x = "First Counselor", y = "Duration (minutes)",
       title = expression(bold("C. Session Duration")),
       subtitle = expression(paste("Between-counselor ", eta^2, " = 0.004")))

# Panel D: Questions answered (bar + CI)
p_bar_ans <- bar_ci_plot(df, "answered_first_all",
                         expression(bold("D. Questions Answered")),
                         fill_color = "darkorange",
                         subtitle_str = "98.6% = 1, near-constant",
                         y_label = "Share Answered")

p_quality_comp <- (p_bar_sat + p_bar_topics) / (p_box_session + p_bar_ans) +
  plot_annotation(
    title = "Distribution of Quality Components by First Assigned Counselor",
    caption = "Treatment women, first counseling session. All four components included in quality index (equal-weighted standardized average)."
  )

ggsave(file.path(figures_path, "quality_components_couns_combined.pdf"),
       p_quality_comp, width = 12, height = 9, dpi = 300)
cat("Saved: quality_components_couns_combined.pdf\n")

# ==============================================================================
# 4. QUALITY INDEX BY COUNSELOR (scatter + CI)
# ==============================================================================

qi_summary <- df %>%
  filter(!is.na(quality_index_new), !is.na(first_counselor)) %>%
  group_by(first_counselor) %>%
  summarise(
    mean_qi = mean(quality_index_new, na.rm = TRUE),
    se_qi   = sd(quality_index_new, na.rm = TRUE) / sqrt(n()),
    n       = n(),
    .groups = "drop"
  ) %>%
  mutate(
    tcrt = qt(0.975, n - 1),
    lci  = mean_qi - tcrt * se_qi,
    uci  = mean_qi + tcrt * se_qi
  )

p_qi_standalone <- ggplot(qi_summary, aes(x = first_counselor, y = mean_qi)) +
  geom_point(size = 3, color = "grey40") +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.2,
                color = "navy", linewidth = 0.6) +
  labs(x = "First Counselor", y = "Mean Quality Index (First Session)",
       title = "Quality Index at First Session by Counselor",
       subtitle = "Equal-weighted standardized average: satisfaction + topics + duration + answered") +
  scale_x_continuous(breaks = qi_summary$first_counselor)

ggsave(file.path(figures_path, "quality_index_by_counselor.pdf"),
       p_qi_standalone, width = 7, height = 5, dpi = 300)
cat("Saved: quality_index_by_counselor.pdf\n")

# ==============================================================================
# 5. CQT INSTRUMENT + AVERAGE QUALITY INDEX BY COUNSELOR (paper Figure 2)
# Both panels use the sat+topics-engaged construction (topics gated on the
# counselor answering all questions). Panel A: CQT at first session. Panel B:
# woman-level average quality across all attended sessions.
# ==============================================================================

# Treatment-women panel for plotting
df_plot <- df %>%
  filter(treatment == 1, !is.na(first_counselor))

# Panel A: CQT (first-session sat+topics-engaged), per counselor
# The plotted height is the counselor's mean residualized first-session quality
# (which equals the within-counselor mean of the leave-one-out CQT values). The
# CI, however, must reflect uncertainty in that counselor-level TENDENCY, so it
# is built from the UNDERLYING residualized first-session quality scores, not
# from the dispersion of the leave-one-out values (whose within-counselor
# variance is mechanically (N_c - 1)^-2 times that of the underlying scores).
# We recover each woman's underlying residualized score from the identity
#   Q_ic = N_c * Qbar_c - (N_c - 1) * CQT_ic ,  where Qbar_c = mean_i(CQT_ic),
# then form the standard error of the counselor mean as sd(Q_ic)/sqrt(N_c).
cqt_summary <- df_plot %>%
  filter(!is.na(cqt_resid_loo_sattop)) %>%
  group_by(first_counselor) %>%
  mutate(
    n_c     = n(),
    qbar_c  = mean(cqt_resid_loo_sattop, na.rm = TRUE),
    q_resid = n_c * qbar_c - (n_c - 1) * cqt_resid_loo_sattop
  ) %>%
  summarise(
    mean_val = mean(cqt_resid_loo_sattop, na.rm = TRUE),   # = Qbar_c (height unchanged)
    se_val   = sd(q_resid, na.rm = TRUE) / sqrt(n()),       # SE of the counselor mean
    n        = n(),
    .groups  = "drop"
  ) %>%
  mutate(
    tcrt = qt(0.975, n - 1),
    lci  = mean_val - tcrt * se_val,
    uci  = mean_val + tcrt * se_val
  )

p_cqt <- ggplot(cqt_summary, aes(x = first_counselor, y = mean_val)) +
  geom_point(size = 3, color = "grey40") +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.2,
                color = "navy", linewidth = 0.6) +
  labs(x = "First Counselor", y = "Mean CQT (LOO residualized)",
       title = "Panel A: CQT at First Session") +
  scale_x_continuous(breaks = cqt_summary$first_counselor)

# Panel B: Average quality index across all attended sessions, per counselor
avgq_summary <- df_plot %>%
  filter(!is.na(avg_quality_sattop)) %>%
  group_by(first_counselor) %>%
  summarise(
    mean_val = mean(avg_quality_sattop, na.rm = TRUE),
    se_val   = sd(avg_quality_sattop,   na.rm = TRUE) / sqrt(n()),
    n        = n(),
    .groups  = "drop"
  ) %>%
  mutate(
    tcrt = qt(0.975, n - 1),
    lci  = mean_val - tcrt * se_val,
    uci  = mean_val + tcrt * se_val
  )

p_qi_panel <- ggplot(avgq_summary, aes(x = first_counselor, y = mean_val)) +
  geom_point(size = 3, color = "grey40") +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.2,
                color = "navy", linewidth = 0.6) +
  labs(x = "First Counselor", y = "Mean Average Quality Index",
       title = "Panel B: Average Quality Index Across Sessions") +
  scale_x_continuous(breaks = avgq_summary$first_counselor)

# Cross-counselor slope: counselor-level mean(avg quality) on mean(CQT)
counsel_means <- df_plot %>%
  filter(!is.na(cqt_resid_loo_sattop), !is.na(avg_quality_sattop)) %>%
  group_by(first_counselor) %>%
  summarise(mean_cqt  = mean(cqt_resid_loo_sattop),
            mean_avgq = mean(avg_quality_sattop),
            .groups   = "drop")
slope_fit <- lm(mean_avgq ~ mean_cqt, data = counsel_means)
slope_val <- coef(slope_fit)["mean_cqt"]
slope_p   <- summary(slope_fit)$coefficients["mean_cqt", "Pr(>|t|)"]

p_instrument_quality <- p_cqt + p_qi_panel +
  plot_annotation(
    title = "CQT (first session) and Average Quality Index by Counselor",
    subtitle = sprintf(
      "Both built from sat + topics conditional on questions answered = 1.  Cross-counselor slope = %.2f (p = %.3f)",
      slope_val, slope_p),
    caption = "CQT = LOO residualized first-session quality, after removing scheduling fixed effects."
  )

ggsave(file.path(figures_path, "instrument_and_quality_by_counselor.pdf"),
       p_instrument_quality, width = 12, height = 5, dpi = 300)
cat("Saved: instrument_and_quality_by_counselor.pdf\n")

# ==============================================================================
# 6. FIRST STAGE: COUNSELOR-LEVEL MEANS PLOT
# ==============================================================================

# --- Binary first stage coefficients: high_quality_avg_sattop on cqt_resid_loo_sattop ---
# May 2026 update: PRIMARY endogenous is now high_quality_avg_sattop (above-median
# woman-level 4-comp all-session quality), built in Stata. We use cqt_resid_loo_sattop
# (the Stata LOO instrument) here too for the first stage scatter, to match the
# table values in tab:first_stage.
fs_bin_unadj <- lm(high_quality_avg_sattop ~ cqt_resid_loo_sattop, data = df18)
b_unadj <- coef(fs_bin_unadj)["cqt_resid_loo_sattop"]

covars_formula <- paste(c("cqt_resid_loo_sattop", covariate_vars), collapse = " + ")
fs_bin_adj <- lm(as.formula(paste("high_quality_avg_sattop ~", covars_formula)),
                 data = df18)
b_adj <- coef(fs_bin_adj)["cqt_resid_loo_sattop"]

# --- Counselor-level means for the plot ---
couns_means <- df18 %>%
  filter(!is.na(high_quality_avg_sattop), !is.na(cqt_resid_loo_sattop)) %>%
  group_by(first_counselor) %>%
  summarise(
    mean_cqt = mean(cqt_resid_loo_sattop, na.rm = TRUE),
    mean_hq  = mean(high_quality_avg_sattop, na.rm = TRUE),
    se_hq    = sd(high_quality_avg_sattop, na.rm = TRUE) / sqrt(n()),
    n        = n(),
    .groups  = "drop"
  ) %>%
  arrange(mean_cqt) %>%
  mutate(counselor_label = paste0("C", first_counselor, "\n(n=", n, ")"))

# --- Counselor-level first stage plot (binary outcome) ---
p_firststage <- ggplot(couns_means, aes(x = mean_cqt, y = mean_hq)) +
  geom_smooth(method = "lm", se = TRUE, color = "navy", fill = "navy",
              alpha = 0.15, linewidth = 1) +
  geom_errorbar(aes(ymin = mean_hq - 1.96 * se_hq,
                    ymax = mean_hq + 1.96 * se_hq),
                width = 0.015, color = "firebrick", linewidth = 0.6) +
  geom_point(aes(size = n), color = "firebrick", shape = 16) +
  geom_text(aes(label = counselor_label),
            vjust = -1.8, size = 3, color = "grey30") +
  scale_size_continuous(range = c(3, 7), guide = "none") +
  scale_x_continuous(limits = c(min(couns_means$mean_cqt) - 0.08,
                                max(couns_means$mean_cqt) + 0.08),
                     breaks = seq(-0.4, 1.2, by = 0.2)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.10, 1.05)) +
  annotate("text", x = min(couns_means$mean_cqt) + 0.02,
           y = 1.00, hjust = 0, size = 3.5, color = "navy",
           label = sprintf("Unadjusted b = %.3f***\nAdjusted b = %.3f***\nKP F = %.0f / %.0f",
                           b_unadj, b_adj,
                           summary(fs_bin_unadj)$fstatistic[1],
                           summary(fs_bin_adj)$fstatistic[1])) +
  labs(x = "Mean CQT (Leave-One-Out, Residualized)",
       y = "Share with Above-Median All-Session Quality",
       title = "First Stage: CQT Predicts Above-Median All-Session Quality",
       subtitle = "Each point = one counselor. Size proportional to N. Line = OLS fit across counselor means.") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "grey40"))

ggsave(file.path(figures_path, "first_stage_scatter.pdf"),
       p_firststage, width = 8, height = 6, dpi = 300)
cat("Saved: first_stage_scatter.pdf\n")

if (FALSE) {  # ---- SUPERSEDED SECTIONS 7-16 (custom forest plots, het graphs, diagnostics) ----
# These figures are NOT used in the current manuscript: the heterogeneity forest
# plots are produced by het_graph_examples.R and the main/robustness tables by the
# Python builders. Several of these sections also reference outcome variables that
# were removed during the spec evolution (next_birth_2018, new_adopter,
# unintended_preg_*, space_knowledge, preg_likely_nofp). Disabled so the script runs
# end-to-end and regenerates only the three figures the paper uses from this file
# (quality_components_couns_combined, instrument_and_quality_by_counselor, and the
# quality_component_correlations figure in Section 17 below).

# ==============================================================================
# 7. FOREST PLOT: IV MAIN EFFECTS — TWO-PANEL (reads from Stata CSV)
# ==============================================================================

# Read IV results exported by Stata (ensures coefficients match tables exactly)
iv_csv_path <- file.path(tables_path, "IV_main_results.csv")
cat("Reading IV results from:", iv_csv_path, "\n")
# Clean label mapping for continuity/topic outcomes (Stata exports raw names)
continuity_label_map <- c(
  "(first) total_sessions"           = "Total sessions",
  "(first) visit_rate_mo"            = "Sessions per month",
  "(first) anyone_present"           = "Anyone present during sessions",
  "(first) etp1_fp"                  = "FP info (post-1st)",
  "(first) etp1_spacing"             = "Birth spacing (post-1st)",
  "(first) etp1_preg"                = "Pregnancy (post-1st)",
  "(first) etp1_partner"             = "Partner engagement (post-1st)",
  "(first) etp1_childhealth"         = "Child health (post-1st)",
  "(first) etp1_breast"              = "Breastfeeding (post-1st)",
  "(first) etp1_sti"                 = "STI info (post-1st)"
)

# Clean verbose CSV labels to match LaTeX table labels
csv_label_map <- c(
  "Any live birth since index birth (before 2018 endline)" = "Live birth",
  "Unintended pregnancy (ex post)"  = "Unintended preg (ex post)",
  "Unintended pregnancy (ex ante)"  = "Unintended preg (ex ante)",
  "FP use at Baseline"              = "Current FP use",
  "Months of continuous FP use since baseline" = "Months FP use",
  "Husband knows about FP use"      = "Husband knows FP",
  "Perceived pregnancy likely if not using FP" = "Perceived preg risk",
  "Involved in healthcare decisions" = "Healthcare decisions",
  "Involved in major HH purchases"  = "Major HH purchases",
  "Share of sessions with first counselor" = "Share same counselor",
  "Sessions per month over follow-up window" = "Sessions per month",
  "Anyone else present during session (0/1)" = "Anyone present during sessions",
  "Spacing knowledge"               = "Spacing knowledge"
)

# Outcomes that appear in the paper tables (RF pooled / IV pooled).
# UPDATED 2026-05-09 to the final 19-outcome 4-panel set under the sat+topics
# primary spec.
table_outcomes <- c(
  # Panel A: Reproductive (4)
  "birth_within_24mo",
  "preg_within_12mo", "preg_within_24mo", "mistimed_preg_expost",
  # Panel B: FP coverage (5)
  "admin_fp_any", "admin_inject_any", "n_admin_fp",
  "fp_inj_coverage_frac", "husband_knows_fp",
  # Panel C: Continuity (4)
  "total_sessions", "share_same_counselor", "visit_rate_mo",
  "husband_present",
  # Panel D: Topics post-1st-session (6)
  "etp1_fp", "etp1_spacing", "etp1_preg",
  "etp1_partner", "etp1_childhealth", "etp1_breast"
)

# ==============================================================================
# HELPER: Read, filter, and label CSV results
# ==============================================================================
read_and_label <- function(csv_path) {
  read.csv(csv_path, stringsAsFactors = FALSE) %>%
    filter(!is.na(estimate), outcome %in% table_outcomes) %>%
    mutate(
      label = ifelse(label %in% names(continuity_label_map),
                     continuity_label_map[label], label),
      label = ifelse(label %in% names(csv_label_map),
                     csv_label_map[label], label),
      label = case_when(
        outcome == "birth_within_24mo"     ~ "Birth within 24 months",
        outcome == "preg_within_12mo"      ~ "Pregnancy within 12 months",
        outcome == "preg_within_24mo"      ~ "Pregnancy within 24 months",
        outcome == "mistimed_preg_expost"  ~ "Mistimed pregnancy",
        outcome == "admin_fp_any"          ~ "Any FP service",
        outcome == "admin_inject_any"      ~ "Any injectable",
        outcome == "n_admin_fp"            ~ "Number of FP services",
        outcome == "fp_inj_coverage_frac"  ~ "Injectable coverage",
        outcome == "husband_knows_fp"      ~ "Husband knows FP",
        outcome == "total_sessions"        ~ "Total sessions",
        outcome == "share_same_counselor"  ~ "Same counselor share",
        outcome == "visit_rate_mo"         ~ "Couns. sessions per month",
        outcome == "husband_present"       ~ "Husband present",
        outcome == "etp1_fp"               ~ "FP info",
        outcome == "etp1_spacing"          ~ "Birth spacing",
        outcome == "etp1_preg"             ~ "Pregnancy info",
        outcome == "etp1_partner"          ~ "Partner engagement",
        outcome == "etp1_childhealth"      ~ "Child health",
        outcome == "etp1_breast"           ~ "Breastfeeding",
        TRUE ~ label
      ),
      lci = estimate - 1.96 * se,
      uci = estimate + 1.96 * se,
      sig = case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ ""),
      panel = case_when(
        outcome %in% c("birth_within_24mo",
                        "preg_within_12mo", "preg_within_24mo",
                        "mistimed_preg_expost")
          ~ "A: Reproductive outcomes",
        outcome %in% c("admin_fp_any", "admin_inject_any", "n_admin_fp",
                        "fp_inj_coverage_frac", "husband_knows_fp")
          ~ "B: FP coverage",
        outcome %in% c("total_sessions", "share_same_counselor",
                        "visit_rate_mo", "husband_present")
          ~ "C: Counseling continuity",
        outcome %in% c("etp1_fp", "etp1_spacing", "etp1_preg",
                        "etp1_partner", "etp1_childhealth", "etp1_breast")
          ~ "D: Counseling topics"
      )
    )
}

# Panel-specific axis limits
forest_xlims <- list(
  "A: Reproductive outcomes"  = c(-0.10, 0.20),
  "B: FP coverage"            = c(-0.20, 1.20),
  "C: Counseling continuity"  = c(-1.00, 0.10),
  "D: Counseling topics"      = c(-0.30, 1.50)
)

# Ordered label lists within each panel (top to bottom display order)
panel_label_order <- list(
  "A: Reproductive outcomes" = c(
    "Birth within 24 months",
    "Pregnancy within 12 months", "Pregnancy within 24 months",
    "Mistimed pregnancy"),
  "B: FP coverage" = c(
    "Any FP service", "Any injectable", "Number of FP services",
    "Injectable coverage", "Husband knows FP"),
  "C: Counseling continuity" = c(
    "Total sessions", "Same counselor share",
    "Couns. sessions per month", "Husband present"),
  "D: Counseling topics" = c(
    "FP info", "Birth spacing", "Pregnancy info",
    "Partner engagement", "Child health", "Breastfeeding")
)

# Build a single 4-panel forest plot
make_forest_4panel <- function(df, xlab_text, point_color = "grey30", ci_color = "grey50") {
  panels <- lapply(names(forest_xlims), function(pname) {
    d <- df %>% filter(panel == pname) %>%
      mutate(label = factor(label, levels = rev(panel_label_order[[pname]])))

    xlim <- forest_xlims[[pname]]

    ggplot(d, aes(x = estimate, y = label)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
      geom_errorbarh(aes(xmin = lci, xmax = uci), height = 0.25,
                     linewidth = 0.5, color = ci_color) +
      geom_point(color = point_color, size = 2.5) +
      geom_text(aes(label = sig), hjust = -0.3, vjust = 0.35, size = 2.8,
                fontface = "bold", color = point_color) +
      coord_cartesian(xlim = xlim, clip = "off") +
      labs(x = NULL, y = NULL, title = pname) +
      theme(
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 7),
        plot.title = element_text(size = 10, face = "bold"),
        panel.grid.major.x = element_line(color = "grey92"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(5, 20, 2, 5)
      )
  })

  # Stack all 4 panels with proportional heights matching outcome counts:
  # A=5, B=5, C=4, D=6
  panels[[1]] / panels[[2]] / panels[[3]] / panels[[4]] +
    plot_layout(ncol = 1, heights = c(5, 5, 4, 6))
}

# ==============================================================================
# 7. FOREST PLOT: IV MAIN EFFECTS — FOUR-PANEL (reads from Stata CSV)
# ==============================================================================
iv_df <- read_and_label(iv_csv_path)

# STYLE 1: Stacked 4-panel (like het graphs)
p_forest_iv_style1 <- make_forest_4panel(iv_df, point_color = "navy", ci_color = "grey50") +
  plot_annotation(
    title = "Forest Plot: 2SLS IV Estimates",
    subtitle = "Endogenous: above-median sat+topics quality | Instrument: leave-one-out CQT (sat+topics)",
    theme = theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, color = "grey30")
    )
  )

ggsave(file.path(figures_path, "forest_plot_IV_main_style1.pdf"),
       p_forest_iv_style1, width = 8, height = 12, dpi = 300)
cat("Saved: forest_plot_IV_main_style1.pdf\n")

# STYLE 2: 2x2 grid
make_forest_2x2 <- function(df, point_color = "navy", ci_color = "grey50") {
  panels <- lapply(names(forest_xlims), function(pname) {
    d <- df %>% filter(panel == pname) %>%
      mutate(label = factor(label, levels = rev(panel_label_order[[pname]])))
    xlim <- forest_xlims[[pname]]
    ggplot(d, aes(x = estimate, y = label)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
      geom_errorbarh(aes(xmin = lci, xmax = uci), height = 0.25,
                     linewidth = 0.5, color = ci_color) +
      geom_point(color = point_color, size = 2.5) +
      geom_text(aes(label = sig), hjust = -0.3, vjust = 0.35, size = 2.8,
                fontface = "bold", color = point_color) +
      coord_cartesian(xlim = xlim, clip = "off") +
      labs(x = NULL, y = NULL, title = pname) +
      theme(
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 7),
        plot.title = element_text(size = 10, face = "bold"),
        panel.grid.major.x = element_line(color = "grey92"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(5, 15, 5, 5)
      )
  })
  (panels[[1]] + panels[[2]]) / (panels[[3]] + panels[[4]])
}

p_forest_iv_style2 <- make_forest_2x2(iv_df) +
  plot_annotation(
    title = "Forest Plot: 2SLS IV Estimates",
    subtitle = "Endogenous: above-median first-session quality | Instrument: CQT (LOO)",
    theme = theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, color = "grey30")
    )
  )

ggsave(file.path(figures_path, "forest_plot_IV_main_style2.pdf"),
       p_forest_iv_style2, width = 13, height = 9, dpi = 300)
cat("Saved: forest_plot_IV_main_style2.pdf\n")

# Keep the winning style as the main file (default: style 1)
# Use style 2 (2x2 grid) as the main forest plot
file.copy(file.path(figures_path, "forest_plot_IV_main_style2.pdf"),
          file.path(figures_path, "forest_plot_IV_main.pdf"), overwrite = TRUE)

# ==============================================================================
# 8. FOREST PLOT: REDUCED FORM EFFECTS — FOUR-PANEL (reads from Stata CSV)
# ==============================================================================

rf_csv_path <- file.path(tables_path, "RF_main_results.csv")
cat("Reading RF results from:", rf_csv_path, "\n")
rf_df <- read_and_label(rf_csv_path)

# Style 1: stacked
p_forest_rf_style1 <- make_forest_4panel(rf_df, point_color = "firebrick", ci_color = "grey50") +
  plot_annotation(
    title = "Forest Plot: Reduced Form Estimates",
    subtitle = "CQT scaled: 1 unit = full instrument range (low to high quality)",
    theme = theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, color = "grey30")
    )
  )

ggsave(file.path(figures_path, "forest_plot_RF_style1.pdf"),
       p_forest_rf_style1, width = 8, height = 12, dpi = 300)
cat("Saved: forest_plot_RF_style1.pdf\n")

# Style 2: 2x2
p_forest_rf_style2 <- make_forest_2x2(rf_df, point_color = "firebrick") +
  plot_annotation(
    title = "Forest Plot: Reduced Form Estimates",
    subtitle = "CQT scaled: 1 unit = full instrument range (low to high quality)",
    theme = theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, color = "grey30")
    )
  )

ggsave(file.path(figures_path, "forest_plot_RF_style2.pdf"),
       p_forest_rf_style2, width = 13, height = 9, dpi = 300)
cat("Saved: forest_plot_RF_style2.pdf\n")

# Keep winning style as main file (default: style 1)
# Use style 2 (2x2 grid) as the main forest plot
file.copy(file.path(figures_path, "forest_plot_RF_style2.pdf"),
          file.path(figures_path, "forest_plot_RF.pdf"), overwrite = TRUE)


# ==============================================================================
# 9. HETEROGENEITY GRAPHS: PREGNANCY STATUS
# ==============================================================================

# Load AER for ivreg (needed by het sections)
library(AER)

# Covariates formula (includes curr_use_bl as baseline covariate)
cov_rhs <- paste(covariate_vars, collapse = " + ")

het_outcomes_short <- c("next_birth_2018",
                        "unintended_preg_expost", "unintended_preg_exante",
                        "new_adopter",
                        "husband_knows_fp", "space_knowledge",
                        "preg_likely_nofp",
                        "share_same_counselor",
                        "total_sessions")

# Covariates excluding preg_2016
cov_no_preg <- setdiff(covariate_vars, "preg_2016")
cov_no_preg_rhs <- paste(cov_no_preg, collapse = " + ")

# PRIMARY endogenous (May 2026): high_quality_avg_sattop (already in dataset)
# Falls back to constructing a copy here for the year==2018 subset
df18$high_quality_avg_sattop <- ifelse(!is.na(df18$avg_quality_sattop) & df18$treatment == 1,
                                    as.integer(df18$avg_quality_sattop >
                                               median(df18$avg_quality_sattop[df18$treatment == 1],
                                                      na.rm = TRUE)),
                                    NA)

het_preg_results <- lapply(het_outcomes_short, function(y) {
  d <- df18 %>%
    filter(!is.na(.data[[y]]), !is.na(high_quality_avg_sattop),
           !is.na(cqt_resid_loo_sattop), !is.na(preg_2016)) %>%
    mutate(
      qualityXpreg = high_quality_avg_sattop * preg_2016,
      cqtXpreg     = cqt_resid_loo_sattop * preg_2016
    )
  fmla <- as.formula(sprintf(
    "%s ~ high_quality_avg_sattop + qualityXpreg + preg_2016 + %s | cqt_resid_loo_sattop + cqtXpreg + preg_2016 + %s",
    y, cov_no_preg_rhs, cov_no_preg_rhs
  ))
  tryCatch({
    fit <- ivreg(fmla, data = d)
    s   <- summary(fit)
    b_base <- s$coefficients["high_quality_avg_sattop", "Estimate"]
    se_base <- s$coefficients["high_quality_avg_sattop", "Std. Error"]
    b_int  <- s$coefficients["qualityXpreg", "Estimate"]
    se_int <- s$coefficients["qualityXpreg", "Std. Error"]
    data.frame(
      outcome = y,
      label   = outcome_labels[y],
      group   = c("Postpartum", "Interaction (Pregnant)"),
      estimate = c(b_base, b_int),
      se       = c(se_base, se_int),
      stringsAsFactors = FALSE
    )
  }, error = function(e) NULL)
})

het_preg_df <- bind_rows(het_preg_results) %>%
  mutate(
    lci = estimate - 1.96 * se,
    uci = estimate + 1.96 * se
  )

het_preg_plots <- lapply(unique(het_preg_df$label), function(lbl) {
  d <- het_preg_df %>% filter(label == lbl)
  ggplot(d, aes(x = group, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
    geom_col(fill = "grey80", width = 0.5) +
    geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.15, color = "navy") +
    labs(x = NULL, y = "Coefficient", title = lbl) +
    theme(axis.text.x = element_text(size = 8))
})

p_het_preg <- wrap_plots(het_preg_plots, ncol = 3) +
  plot_annotation(title = "IV Heterogeneity by Pregnancy Status at Baseline")

ggsave(file.path(figures_path, "IV_het_pregnancy_combined.pdf"),
       p_het_preg, width = 14, height = 10, dpi = 300)
cat("Saved: IV_het_pregnancy_combined.pdf\n")

# ==============================================================================
# 10. HETEROGENEITY GRAPHS: FERTILITY PREFERENCE
# ==============================================================================

het_fert_results <- lapply(het_outcomes_short, function(y) {
  d <- df18 %>%
    filter(!is.na(.data[[y]]), !is.na(high_quality_avg_sattop),
           !is.na(cqt_resid_loo_sattop), !is.na(fertility_pref_bl)) %>%
    mutate(
      qualityXfp = high_quality_avg_sattop * fertility_pref_bl,
      cqtXfp     = cqt_resid_loo_sattop * fertility_pref_bl
    )
  fmla <- as.formula(sprintf(
    "%s ~ high_quality_avg_sattop + qualityXfp + fertility_pref_bl + %s | cqt_resid_loo_sattop + cqtXfp + fertility_pref_bl + %s",
    y, cov_rhs, cov_rhs
  ))
  tryCatch({
    fit <- ivreg(fmla, data = d)
    s   <- summary(fit)
    b_base <- s$coefficients["high_quality_avg_sattop", "Estimate"]
    se_base <- s$coefficients["high_quality_avg_sattop", "Std. Error"]
    b_int  <- s$coefficients["qualityXfp", "Estimate"]
    se_int <- s$coefficients["qualityXfp", "Std. Error"]
    data.frame(
      outcome = y,
      label   = outcome_labels[y],
      group   = c("No more children", "Interaction (Wants more)"),
      estimate = c(b_base, b_int),
      se       = c(se_base, se_int)
    )
  }, error = function(e) NULL)
})

het_fert_df <- bind_rows(het_fert_results) %>%
  mutate(
    lci = estimate - 1.96 * se,
    uci = estimate + 1.96 * se
  )

het_fert_plots <- lapply(unique(het_fert_df$label), function(lbl) {
  d <- het_fert_df %>% filter(label == lbl)
  ggplot(d, aes(x = group, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
    geom_col(fill = "grey80", width = 0.5) +
    geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.15, color = "navy") +
    labs(x = NULL, y = "Coefficient", title = lbl) +
    theme(axis.text.x = element_text(size = 8))
})

p_het_fert <- wrap_plots(het_fert_plots, ncol = 3) +
  plot_annotation(title = "IV Heterogeneity by Fertility Preference at Baseline")

ggsave(file.path(figures_path, "IV_het_fertpref_combined.pdf"),
       p_het_fert, width = 14, height = 10, dpi = 300)
cat("Saved: IV_het_fertpref_combined.pdf\n")

# ==============================================================================
# 10b. HETEROGENEITY GRAPHS: PRIVACY (ANYONE PRESENT AT FIRST SESSION)
# ==============================================================================

cat("\n--- Section 10b: Privacy Heterogeneity ---\n")

het_privacy_outcomes <- c("next_birth_2018",
                          "unintended_preg_expost", "unintended_preg_exante",
                          "new_adopter",
                          "husband_knows_fp", "space_knowledge",
                          "preg_likely_nofp",
                          "share_same_counselor",
                          "total_sessions")

het_privacy_results <- lapply(het_privacy_outcomes, function(y) {
  d <- df18 %>%
    filter(!is.na(.data[[y]]), !is.na(high_quality_avg_sattop),
           !is.na(cqt_resid_loo_sattop), !is.na(anyone_present_first)) %>%
    mutate(
      qualityXpriv = high_quality_avg_sattop * anyone_present_first,
      cqtXpriv     = cqt_resid_loo_sattop * anyone_present_first
    )
  fmla <- as.formula(sprintf(
    "%s ~ high_quality_avg_sattop + qualityXpriv + anyone_present_first + %s | cqt_resid_loo_sattop + cqtXpriv + anyone_present_first + %s",
    y, cov_rhs, cov_rhs
  ))
  tryCatch({
    fit <- ivreg(fmla, data = d)
    s   <- summary(fit)
    b_base <- s$coefficients["high_quality_avg_sattop", "Estimate"]
    se_base <- s$coefficients["high_quality_avg_sattop", "Std. Error"]
    b_int  <- s$coefficients["qualityXpriv", "Estimate"]
    se_int <- s$coefficients["qualityXpriv", "Std. Error"]
    data.frame(
      outcome = y,
      label   = outcome_labels[y],
      group   = c("Private session", "Interaction (Someone present)"),
      estimate = c(b_base, b_int),
      se       = c(se_base, se_int),
      stringsAsFactors = FALSE
    )
  }, error = function(e) NULL)
})

het_privacy_df <- bind_rows(het_privacy_results) %>%
  mutate(
    lci = estimate - 1.96 * se,
    uci = estimate + 1.96 * se
  )

het_privacy_plots <- lapply(unique(het_privacy_df$label), function(lbl) {
  d <- het_privacy_df %>% filter(label == lbl)
  ggplot(d, aes(x = group, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
    geom_col(fill = "grey80", width = 0.5) +
    geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.15, color = "navy") +
    labs(x = NULL, y = "Coefficient", title = lbl) +
    theme(axis.text.x = element_text(size = 7, angle = 15, hjust = 1))
})

p_het_privacy <- wrap_plots(het_privacy_plots, ncol = 3) +
  plot_annotation(title = "IV Heterogeneity by Privacy During First Counseling Session")

ggsave(file.path(figures_path, "IV_het_privacy_combined.pdf"),
       p_het_privacy, width = 14, height = 10, dpi = 300)
cat("Saved: IV_het_privacy_combined.pdf\n")

# ============================================================================
# 11. LINEARITY DIAGNOSTICS: BINNED SCATTER + LOESS FOR ALL OUTCOMES
# ============================================================================
# Reads binned scatter data from Stata and produces a multi-panel figure
# showing the residualized relationship between CQT and each outcome.
# Each panel shows quintile bin means with a linear fit + LOESS smooth.

cat("\n--- Section 11: Linearity Diagnostics ---\n")

binscatter_path <- file.path(tables_path, "linearity_binscatter.csv")
if (file.exists(binscatter_path)) {

  bin_df <- read.csv(binscatter_path, stringsAsFactors = FALSE)

  # Outcome category mapping for grouping
  outcome_category <- c(
    "Live birth" = "Reproductive",
    "Unintended preg (ex post)" = "Reproductive",
    "Unintended preg (ex ante)" = "Reproductive",
    "Mistimed (wanted later)" = "Reproductive",
    "Current FP use" = "FP Use",
    "Months FP use" = "FP Use",
    "Husband knows FP" = "FP Use",
    "Preg risk perceived (no FP)" = "FP Knowledge",
    "Spacing knowledge" = "FP Knowledge",
    "Healthcare decisions" = "Bargaining",
    "Major HH purchases" = "Bargaining",
    "Share same counselor" = "Continuity",
    "Total sessions" = "Continuity",
    "Avg days between sessions" = "Continuity",
    "Visits with same counselor" = "Continuity",
    "Only one session" = "Continuity",
    "Ever requested FP" = "Topics",
    "Ever requested spacing" = "Topics",
    "Ever requested pregnancy" = "Topics",
    "Ever requested partner" = "Topics",
    "Ever requested child health" = "Topics",
    "Ever requested STI" = "Topics"
  )

  # Clean up labels to match
  bin_df <- bin_df %>%
    mutate(
      label_clean = case_when(
        grepl("live birth", label, ignore.case = TRUE) ~ "Live birth",
        grepl("ex post", label, ignore.case = TRUE) ~ "Unintended preg (ex post)",
        grepl("ex ante", label, ignore.case = TRUE) ~ "Unintended preg (ex ante)",
        grepl("Mistimed|wanted later", label, ignore.case = TRUE) ~ "Mistimed (wanted later)",
        grepl("Months|months_fp", label, ignore.case = TRUE) ~ "Months FP use",
        grepl("Husband knows", label, ignore.case = TRUE) ~ "Husband knows FP",
        grepl("likely|risk|preg_likely", label, ignore.case = TRUE) ~ "Preg risk perceived (no FP)",
        grepl("Healthcare|w823", label, ignore.case = TRUE) ~ "Healthcare decisions",
        grepl("Major|w824", label, ignore.case = TRUE) ~ "Major HH purchases",
        grepl("Share|share_same", label, ignore.case = TRUE) ~ "Share same counselor",
        grepl("Total sessions|total_sessions", label, ignore.case = TRUE) ~ "Total sessions",
        grepl("Avg days|avg_days|Mean days", label, ignore.case = TRUE) ~ "Avg days between sessions",
        grepl("visits.*same|Visits.*same", label, ignore.case = TRUE) ~ "Visits with same counselor",
        TRUE ~ label
      ),
      category = outcome_category[label_clean]
    )

  # Create individual panels
  unique_outcomes <- unique(bin_df$outcome)

  linearity_plots <- lapply(unique_outcomes, function(oc) {
    d <- bin_df %>% filter(outcome == oc)
    lbl <- d$label_clean[1]
    cat_color <- switch(d$category[1],
                        "Reproductive" = "navy",
                        "FP Use" = "darkgreen",
                        "Empowerment" = "darkorange",
                        "FP Knowledge" = "purple",
                        "Bargaining" = "brown",
                        "Continuity" = "darkred",
                        "Topics" = "darkcyan",
                        "grey40")

    ggplot(d, aes(x = cqt_bin_mean, y = y_bin_mean)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.3) +
      geom_smooth(method = "lm", se = FALSE, color = "grey60",
                  linetype = "dashed", linewidth = 0.5) +
      geom_smooth(method = "loess", se = TRUE, color = cat_color,
                  fill = cat_color, alpha = 0.15, linewidth = 0.8) +
      geom_point(size = 2.5, color = cat_color) +
      labs(x = "CQT (residualized, quintile mean)",
           y = "Outcome (residualized, quintile mean)",
           title = lbl) +
      theme(plot.title = element_text(size = 9, face = "bold"),
            axis.title = element_text(size = 7),
            axis.text = element_text(size = 6))
  })

  # Combine into multi-panel figure
  p_linearity <- wrap_plots(linearity_plots, ncol = 4) +
    plot_annotation(
      title = "Linearity Diagnostics: Binned Scatter (CQT vs Outcomes)",
      subtitle = "Quintile means of residualized CQT and outcomes | Dashed = linear fit, Solid = LOESS smooth",
      theme = theme(plot.title = element_text(size = 14, face = "bold"),
                    plot.subtitle = element_text(size = 10))
    )

  ggsave(file.path(figures_path, "linearity_diagnostics_all.pdf"),
         p_linearity, width = 16, height = 20, dpi = 300)
  cat("Saved: linearity_diagnostics_all.pdf\n")

  # Also create a focused version for reproductive outcomes only
  repro_outcomes_lin <- c("next_birth_2018",
                          "unintended_preg_expost", "unintended_preg_exante")

  repro_plots <- lapply(repro_outcomes_lin, function(oc) {
    d <- bin_df %>% filter(outcome == oc)
    lbl <- d$label_clean[1]

    ggplot(d, aes(x = cqt_bin_mean, y = y_bin_mean)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.3) +
      geom_smooth(method = "lm", se = FALSE, color = "grey60",
                  linetype = "dashed", linewidth = 0.7) +
      geom_smooth(method = "loess", se = TRUE, color = "navy",
                  fill = "navy", alpha = 0.15, linewidth = 1) +
      geom_point(size = 3.5, color = "navy") +
      labs(x = "CQT (residualized, quintile mean)",
           y = "Outcome (residualized, quintile mean)",
           title = lbl) +
      theme(plot.title = element_text(size = 11, face = "bold"),
            axis.title = element_text(size = 9))
  })

  p_linearity_repro <- wrap_plots(repro_plots, ncol = 2) +
    plot_annotation(
      title = "Linearity Check: Reproductive Outcomes",
      subtitle = "Quintile bin means | Dashed = linear, Solid = LOESS",
      theme = theme(plot.title = element_text(size = 13, face = "bold"))
    )

  ggsave(file.path(figures_path, "linearity_diagnostics_repro.pdf"),
         p_linearity_repro, width = 10, height = 8, dpi = 300)
  cat("Saved: linearity_diagnostics_repro.pdf\n")

} else {
  cat("WARNING: linearity_binscatter.csv not found. Run Stata first.\n")
}

# ==============================================================================
# 12. BINNED SCATTER: CQT vs UNINTENDED PREGNANCY
# ==============================================================================
# Reads binscatter_data.csv exported by Stata (export_binscatter_data.do)
# Columns: caseid, cqt_scaled, unintended_preg_expost, preg_2016

cat("\n--- Section 12: Binned Scatter (CQT vs Unintended Pregnancy) — Two Panels ---\n")

binscatter_cqt_path <- file.path(tables_path, "binscatter_data.csv")
if (file.exists(binscatter_cqt_path)) {

  bs_df <- read.csv(binscatter_cqt_path, stringsAsFactors = FALSE) %>%
    filter(!is.na(cqt_scaled), !is.na(unintended_preg_expost), !is.na(preg_2016)) %>%
    mutate(preg_label = ifelse(preg_2016 == 1, "Pregnant at Baseline", "Postpartum at Baseline"))

  # ---- Panel A: Postpartum women ----
  bs_pp <- bs_df %>% filter(preg_2016 == 0)
  bs_pp <- bs_pp %>% mutate(cqt_bin = ntile(cqt_scaled, 15))
  bs_pp_binned <- bs_pp %>%
    group_by(cqt_bin) %>%
    summarise(cqt_mean = mean(cqt_scaled, na.rm = TRUE),
              y_mean   = mean(unintended_preg_expost, na.rm = TRUE),
              n = n(), .groups = "drop")

  fit_pp <- lm(unintended_preg_expost ~ cqt_scaled, data = bs_pp)
  coef_pp <- coef(fit_pp)["cqt_scaled"]
  pval_pp <- summary(fit_pp)$coefficients["cqt_scaled", "Pr(>|t|)"]

  p_pp <- ggplot(bs_pp_binned, aes(x = cqt_mean, y = y_mean)) +
    geom_point(size = 3, color = "firebrick", alpha = 0.8) +
    geom_smooth(data = bs_pp, aes(x = cqt_scaled, y = unintended_preg_expost),
                method = "lm", se = TRUE, color = "firebrick",
                fill = "firebrick", alpha = 0.1, linewidth = 0.8) +
    annotate("text",
             x = min(bs_pp_binned$cqt_mean) * 1.05,
             y = max(bs_pp_binned$y_mean) * 0.95,
             label = "2SLS base = 0.192**\n95% CI: [0.042, 0.342]",
             hjust = 0, vjust = 1, size = 3.2, fontface = "italic",
             color = "firebrick4") +
    labs(x = "CQT (scaled, leave-one-out)",
         y = "P(Unintended Pregnancy, ex post)",
         title = "Panel A: Postpartum Women",
         subtitle = sprintf("b = %.4f, p = %.2f | N = %d",
                            coef_pp, pval_pp, nrow(bs_pp))) +
    theme(plot.margin = margin(10, 15, 5, 10))

  # ---- Panel B: Pregnant women ----
  bs_preg <- bs_df %>% filter(preg_2016 == 1)
  bs_preg <- bs_preg %>% mutate(cqt_bin = ntile(cqt_scaled, 15))
  bs_preg_binned <- bs_preg %>%
    group_by(cqt_bin) %>%
    summarise(cqt_mean = mean(cqt_scaled, na.rm = TRUE),
              y_mean   = mean(unintended_preg_expost, na.rm = TRUE),
              n = n(), .groups = "drop")

  fit_preg <- lm(unintended_preg_expost ~ cqt_scaled, data = bs_preg)
  coef_preg <- coef(fit_preg)["cqt_scaled"]
  pval_preg <- summary(fit_preg)$coefficients["cqt_scaled", "Pr(>|t|)"]

  p_preg <- ggplot(bs_preg_binned, aes(x = cqt_mean, y = y_mean)) +
    geom_point(size = 3, color = "steelblue", alpha = 0.8) +
    geom_smooth(data = bs_preg, aes(x = cqt_scaled, y = unintended_preg_expost),
                method = "lm", se = TRUE, color = "steelblue",
                fill = "steelblue", alpha = 0.1, linewidth = 0.8) +
    annotate("text",
             x = max(bs_preg_binned$cqt_mean) * 0.65,
             y = max(bs_preg_binned$y_mean) * 0.95,
             label = "2SLS interaction = -0.226***\n95% CI: [-0.394, -0.058]",
             hjust = 0, vjust = 1, size = 3.2, fontface = "italic",
             color = "steelblue4") +
    labs(x = "CQT (scaled, leave-one-out)",
         y = "P(Unintended Pregnancy, ex post)",
         title = "Panel B: Pregnant Women",
         subtitle = sprintf("b = %.4f, p = %.2f | 2SLS base+int = -0.034 (n.s.) | N = %d",
                            coef_preg, pval_preg, nrow(bs_preg))) +
    theme(plot.margin = margin(5, 15, 10, 10))

  # Combine with patchwork
  p_binscatter_2panel <- p_pp / p_preg +
    plot_annotation(
      title = "CQT and Unintended Pregnancy by Pregnancy Status",
      subtitle = "2SLS het: PP base = 0.192**, Pregnant interaction = -0.226*** | Binned scatter with linear fit"
    )

  ggsave(file.path(figures_path, "binscatter_cqt_unintended.pdf"),
         p_binscatter_2panel, width = 9, height = 10, dpi = 300)
  cat("Saved: binscatter_cqt_unintended.pdf (two-panel)\n")

} else {
  cat("WARNING: binscatter_data.csv not found. Run export_binscatter_data.do first.\n")
}

# ==============================================================================
# 13. SESSION DOSAGE BY CQT x PREGNANCY STATUS
# ==============================================================================
# Reads dosage_data.csv exported by Stata (export_graph_data.do)
# Columns: cqt_tercile, preg_2016, mean_sessions, se_sessions, n

cat("\n--- Section 13: Session Dosage by CQT x Pregnancy Status ---\n")

dosage_path <- file.path(tables_path, "dosage_data.csv")
if (file.exists(dosage_path)) {

  dos_df <- read.csv(dosage_path, stringsAsFactors = FALSE) %>%
    filter(!is.na(cqt_tercile), !is.na(preg_2016)) %>%
    mutate(
      tercile_label = factor(cqt_tercile,
                             levels = c(1, 2, 3),
                             labels = c("Low", "Medium", "High")),
      preg_label    = factor(preg_2016,
                             levels = c(0, 1),
                             labels = c("Postpartum", "Pregnant"))
    )

  p_dosage <- ggplot(dos_df, aes(x = tercile_label, y = mean_sessions,
                                  fill = preg_label)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +
    geom_errorbar(aes(ymin = mean_sessions - se_sessions,
                      ymax = mean_sessions + se_sessions),
                  position = position_dodge(width = 0.7),
                  width = 0.2, linewidth = 0.5) +
    scale_fill_manual(values = c("Postpartum" = "#F08080", "Pregnant" = "#2E8B57"),
                      name = "Pregnancy Status") +
    labs(x = "CQT Tercile",
         y = "Mean Total Sessions",
         title = "Session Engagement by CQT and Pregnancy Status",
         subtitle = "Mean total sessions by CQT tercile") +
    theme(legend.position = "top")

  ggsave(file.path(figures_path, "session_dosage_cqt_preg.pdf"),
         p_dosage, width = 8, height = 6, dpi = 300)
  cat("Saved: session_dosage_cqt_preg.pdf\n")

} else {
  cat("WARNING: dosage_data.csv not found. Run export_graph_data.do first.\n")
}

# ==============================================================================
# 14. COUNSELOR TOPIC HEATMAP
# ==============================================================================
# Reads counselor_topic_analysis.csv (already exists from Stata)
# Columns: couns_id, share_t_methods, ..., share_t_breast, mean_quality

cat("\n--- Section 14: Counselor Topic Heatmap ---\n")

topic_path <- file.path(tables_path, "counselor_topic_analysis.csv")
if (file.exists(topic_path)) {

  topic_df <- read.csv(topic_path, stringsAsFactors = FALSE)

  # Order counselors by mean_quality (ascending: lowest quality = C1)
  topic_df <- topic_df %>%
    arrange(mean_quality) %>%
    mutate(counselor_rank = paste0("C", row_number()))

  # Select topic share columns and reshape to long format
  topic_cols <- c("share_t_methods", "share_t_preg", "share_t_spacing",
                  "share_t_sidefx", "share_t_services", "share_t_sti",
                  "share_t_partner", "share_t_child", "share_t_breast")

  topic_labels <- c(
    share_t_methods  = "FP Methods",
    share_t_preg     = "Pregnancy",
    share_t_spacing  = "Birth Spacing",
    share_t_sidefx   = "Side Effects",
    share_t_services = "FP Services",
    share_t_sti      = "STIs",
    share_t_partner  = "Partner Comm.",
    share_t_child    = "Child Health",
    share_t_breast   = "Breastfeeding"
  )

  topic_long <- topic_df %>%
    select(counselor_rank, all_of(topic_cols)) %>%
    pivot_longer(cols = all_of(topic_cols),
                 names_to = "topic_var", values_to = "share") %>%
    mutate(
      topic_label = topic_labels[topic_var],
      topic_label = factor(topic_label, levels = rev(topic_labels)),
      counselor_rank = factor(counselor_rank,
                              levels = paste0("C", 1:nrow(topic_df)))
    )

  p_heatmap <- ggplot(topic_long, aes(x = counselor_rank, y = topic_label,
                                       fill = share)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.2f", share)),
              size = 3, color = ifelse(topic_long$share > 0.5, "white", "black")) +
    scale_fill_gradient(low = "white", high = "navy",
                        limits = c(0, 1), name = "Share") +
    labs(x = "Counselor (ordered by quality index)",
         y = NULL,
         title = NULL,
         subtitle = NULL) +
    theme(panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_text(size = 13, face = "bold", color = "black"),
          axis.text.x = element_text(size = 11, face = "bold", color = "black"))

  ggsave(file.path(figures_path, "counselor_topic_heatmap.pdf"),
         p_heatmap, width = 9, height = 6, dpi = 300)
  cat("Saved: counselor_topic_heatmap.pdf\n")

} else {
  cat("WARNING: counselor_topic_analysis.csv not found.\n")
}

# ==============================================================================
# 15. CONTENT EXPOSURE FUNNEL — Dot plot with 95% CIs
# ==============================================================================
# Both panels use session bins: <=3, 4-5, 6+
# Dot size proportional to N; whiskers show 95% CI for proportions

cat("\n--- Section 15: Content Exposure Funnel (Two Panels) ---\n")

# Helper: add 95% CI for a proportion (Wald interval)
add_prop_ci <- function(df, p_col = "pct_ever_spacing", n_col = "n") {
  df %>% mutate(
    ci_low  = pmax(0, .data[[p_col]] - 1.96 * sqrt(.data[[p_col]] * (1 - .data[[p_col]]) / .data[[n_col]])),
    ci_high = pmin(1, .data[[p_col]] + 1.96 * sqrt(.data[[p_col]] * (1 - .data[[p_col]]) / .data[[n_col]]))
  )
}

bin_labels <- c("1" = "1-3", "2" = "4-5", "3" = "6+")

funnel_path <- file.path(tables_path, "content_funnel_data.csv")
funnel_pp_path <- file.path(tables_path, "content_funnel_pp_preg.csv")

if (file.exists(funnel_path)) {

  # --- Panel A: All women, by pregnancy status at baseline ---
  fun_df <- read.csv(funnel_path, stringsAsFactors = FALSE) %>%
    filter(!is.na(sess_bin), !is.na(preg_2016)) %>%
    add_prop_ci() %>%
    mutate(
      session_label = factor(bin_labels[as.character(sess_bin)],
                             levels = c("1-3", "4-5", "6+")),
      preg_label    = factor(preg_2016,
                             levels = c(0, 1),
                             labels = c("Postpartum", "Pregnant"))
    )

  overall_mean_a <- weighted.mean(fun_df$pct_ever_spacing, fun_df$n, na.rm = TRUE)

  p_funnel_a <- ggplot(fun_df, aes(x = session_label, y = pct_ever_spacing,
                                    color = preg_label, group = preg_label)) +
    geom_hline(yintercept = overall_mean_a, linetype = "dashed",
               color = "grey50", linewidth = 0.5) +
    annotate("text", x = "1-3", y = overall_mean_a + 0.03,
             label = sprintf("Overall mean = %.0f%%", overall_mean_a * 100),
             size = 3, color = "grey40", hjust = 0) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                  width = 0.15, linewidth = 0.6,
                  position = position_dodge(width = 0.3)) +
    geom_point(aes(size = n),
               position = position_dodge(width = 0.3)) +
    geom_text(aes(label = n), size = 2.5, vjust = -1.5,
              position = position_dodge(width = 0.3), show.legend = FALSE) +
    scale_size_continuous(range = c(2.5, 5), name = "N", guide = "none") +
    scale_color_manual(values = c("Postpartum" = "#F08080", "Pregnant" = "#2E8B57"),
                       name = "Baseline Status") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1)) +
    labs(x = "Number of Sessions",
         y = "P(Ever Discussed Birth Spacing)",
         title = "Panel A: All Women by Pregnancy Status at Baseline") +
    theme(legend.position = "top")

  # --- Panel B: Postpartum women only, by subsequent pregnancy ---
  if (file.exists(funnel_pp_path)) {

    fun_pp_df <- read.csv(funnel_pp_path, stringsAsFactors = FALSE) %>%
      filter(!is.na(sess_bin), !is.na(next_preg_2018)) %>%
      add_prop_ci() %>%
      mutate(
        session_label = factor(bin_labels[as.character(sess_bin)],
                               levels = c("1-3", "4-5", "6+")),
        preg_outcome  = factor(next_preg_2018,
                               levels = c("no", "yes"),
                               labels = c("Does not get pregnant", "Gets pregnant again"))
      )

    overall_mean_b <- weighted.mean(fun_pp_df$pct_ever_spacing, fun_pp_df$n, na.rm = TRUE)

    p_funnel_b <- ggplot(fun_pp_df, aes(x = session_label, y = pct_ever_spacing,
                                         color = preg_outcome, group = preg_outcome)) +
      geom_hline(yintercept = overall_mean_b, linetype = "dashed",
                 color = "grey50", linewidth = 0.5) +
      annotate("text", x = "1-3", y = overall_mean_b + 0.03,
               label = sprintf("Overall mean = %.0f%%", overall_mean_b * 100),
               size = 3, color = "grey40", hjust = 0) +
      geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                    width = 0.15, linewidth = 0.6,
                    position = position_dodge(width = 0.3)) +
      geom_point(aes(size = n),
                 position = position_dodge(width = 0.3)) +
      geom_text(aes(label = n,
                    vjust = ifelse(preg_outcome == "Does not get pregnant", -1.5, 2.5)),
                size = 2.5,
                position = position_dodge(width = 0.3), show.legend = FALSE) +
      scale_size_continuous(range = c(2, 5), name = "N", guide = "none") +
      scale_color_manual(values = c("Does not get pregnant" = "#4682B4",
                                     "Gets pregnant again" = "#CD5C5C"),
                         name = "Pregnancy Outcome") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                         limits = c(0, 1)) +
      labs(x = "Number of Sessions",
           y = "P(Ever Discussed Birth Spacing)",
           title = "Panel B: Postpartum Women by Subsequent Pregnancy") +
      theme(legend.position = "top")

    # Combine panels stacked
    p_funnel_combined <- p_funnel_a / p_funnel_b

    ggsave(file.path(figures_path, "content_exposure_funnel.pdf"),
           p_funnel_combined, width = 8, height = 10, dpi = 300)
    cat("Saved: content_exposure_funnel.pdf (two panels)\n")

  } else {
    ggsave(file.path(figures_path, "content_exposure_funnel.pdf"),
           p_funnel_a, width = 8, height = 6, dpi = 300)
    cat("Saved: content_exposure_funnel.pdf (Panel A only; Panel B data not found)\n")
  }

} else {
  cat("WARNING: content_funnel_data.csv not found. Run export_graph_data.do first.\n")
}

# ==============================================================================
# 16. CQT vs TOPIC COVERAGE — LOLLIPOP CHART
# ==============================================================================

cat("\n--- Section 16: CQT vs Topic Coverage (Lollipop) ---\n")

topic_cov_path <- file.path(tables_path, "counselor_topic_analysis.csv")
if (file.exists(topic_cov_path)) {

  topic_df <- read.csv(topic_cov_path, stringsAsFactors = FALSE) %>%
    arrange(mean_quality) %>%
    mutate(counselor_label = paste0("C", couns_id))

  # Fit linear model for annotation
  fit_topic <- lm(avg_n_topics_couns ~ mean_quality, data = topic_df)
  r2_topic <- summary(fit_topic)$r.squared

  p_lollipop <- ggplot(topic_df, aes(x = mean_quality, y = avg_n_topics_couns)) +
    # Lollipop stems
    geom_segment(aes(xend = mean_quality, y = 0, yend = avg_n_topics_couns,
                     color = mean_quality), linewidth = 1.3, alpha = 0.85) +
    # Lollipop dots (size proportional to topics)
    geom_point(aes(color = mean_quality, size = avg_n_topics_couns), alpha = 0.9) +
    # Linear fit with CI
    geom_smooth(method = "lm", se = TRUE, color = "grey40",
                linetype = "dashed", linewidth = 0.7, alpha = 0.15) +
    # Color gradient: red (low CQT) to blue (high CQT)
    scale_color_gradient(low = "#D7263D", high = "#1B4965",
                         name = "CQT Score",
                         guide = guide_colorbar(barwidth = 8, barheight = 0.5,
                                                title.position = "top")) +
    # Dot size
    scale_size_continuous(range = c(5, 12), guide = "none") +
    # Counselor labels
    geom_text(aes(label = counselor_label), vjust = -1.8,
              fontface = "bold", size = 3.5, color = "grey30") +
    # R-squared annotation
    annotate("text", x = min(topic_df$mean_quality) + 0.05,
             y = max(topic_df$avg_n_topics_couns) * 0.95,
             label = sprintf("R\u00B2 = %.2f", r2_topic),
             size = 4, color = "grey40", fontface = "italic", hjust = 0) +
    # Axis labels
    labs(x = "Counselor Quality Tendency (CQT)",
         y = "Mean Topics per Session") +
    scale_y_continuous(limits = c(0, max(topic_df$avg_n_topics_couns) * 1.15),
                       breaks = seq(0, 8, 1)) +
    theme_paper +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3))

  ggsave(file.path(figures_path, "cqt_topic_coverage_lollipop.pdf"),
         p_lollipop, width = 7, height = 6, dpi = 300)
  cat("Saved: cqt_topic_coverage_lollipop.pdf\n")

} else {
  cat("WARNING: counselor_topic_analysis.csv not found.\n")
}
}  # ---- end SUPERSEDED SECTIONS 7-16 ----

# ==============================================================================
# 17. QUALITY COMPONENT CORRELATIONS (after Figure 2 in paper)
# ==============================================================================

cat("\n--- Section 17: Quality Component Correlations (all 4 index components, 6 panels) ---\n")

# Filter to treatment group, first session only
comp_df <- df %>%
  filter(.tag) %>%
  filter(!is.na(satisfaction_first_all),
         !is.na(n_topics_first_session),
         !is.na(session_first_all),
         !is.na(answered_first_all))

n_obs <- nrow(comp_df)

# Helper: create one scatter panel with r annotation
corr_panel <- function(data, xvar, yvar, xlab, ylab,
                       jitter_x = 0, jitter_y = 0,
                       annot_x = NULL, annot_y = NULL) {
  r_val <- cor(data[[xvar]], data[[yvar]], use = "complete.obs")
  p_val <- cor.test(data[[xvar]], data[[yvar]])$p.value
  stars <- ifelse(p_val < 0.001, "***", ifelse(p_val < 0.01, "**",
           ifelse(p_val < 0.05, "*", "")))
  label_txt <- sprintf("r = %.3f%s", r_val, stars)

  # Default annotation position: top center
  if (is.null(annot_x)) annot_x <- mean(range(data[[xvar]], na.rm = TRUE))
  if (is.null(annot_y)) annot_y <- max(data[[yvar]], na.rm = TRUE) * 0.98

  ggplot(data, aes(x = .data[[xvar]], y = .data[[yvar]])) +
    geom_jitter(alpha = 0.15, width = jitter_x, height = jitter_y,
                size = 1, color = "steelblue") +
    geom_smooth(method = "lm", se = TRUE, color = "firebrick", linewidth = 0.8) +
    annotate("text", x = annot_x, y = annot_y,
             label = label_txt,
             hjust = 0.5, size = 3.5, fontface = "italic") +
    labs(x = xlab, y = ylab) +
    theme_paper
}

# Row 1: Satisfaction vs others
p_c1 <- corr_panel(comp_df, "satisfaction_first_all", "n_topics_first_session",
                    "Patient satisfaction (1-4)", "Topics discussed (0-9)",
                    jitter_x = 0.1, jitter_y = 0.2)

p_c2 <- corr_panel(comp_df, "satisfaction_first_all", "session_first_all",
                    "Patient satisfaction (1-4)", "Session duration (min)",
                    jitter_x = 0.1)

p_c3 <- corr_panel(comp_df, "satisfaction_first_all", "answered_first_all",
                    "Patient satisfaction (1-4)", "Questions answered (0/1)",
                    jitter_x = 0.1, jitter_y = 0.03)

# Row 2: Topics vs others, Duration vs Answered
p_c4 <- corr_panel(comp_df, "n_topics_first_session", "session_first_all",
                    "Topics discussed (0-9)", "Session duration (min)",
                    jitter_x = 0.2)

p_c5 <- corr_panel(comp_df, "n_topics_first_session", "answered_first_all",
                    "Topics discussed (0-9)", "Questions answered (0/1)",
                    jitter_x = 0.2, jitter_y = 0.03)

p_c6 <- corr_panel(comp_df, "session_first_all", "answered_first_all",
                    "Session duration (min)", "Questions answered (0/1)",
                    jitter_y = 0.03)

# Combine: 2 rows x 3 columns
p_quality_corr <- (p_c1 + p_c2 + p_c3) / (p_c4 + p_c5 + p_c6) +
  plot_annotation(
    title = "Pairwise Correlations: Quality Index Components",
    subtitle = sprintf("N = %d treatment women, first counseling session | *** p<0.001, ** p<0.01, * p<0.05", n_obs)
  )

ggsave(file.path(figures_path, "quality_component_correlations.pdf"),
       p_quality_corr, width = 14, height = 9, dpi = 300)
cat("Saved: quality_component_correlations.pdf\n")

if (FALSE) {  # ---- SUPERSEDED diagnostic figures (histogram, monotonicity, falsification, leave-one-out) — not used in the manuscript ----

# ==============================================================================
# 18. CQT HISTOGRAM
# ==============================================================================
cat("\n--- 18. CQT Histogram ---\n")

cqt_data <- df %>%
  filter(year == 2016, treatment == 1, !is.na(cqt_resid_loo_sattop))

p_cqt_hist <- ggplot(cqt_data, aes(x = cqt_resid_loo_sattop)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.85) +
  geom_vline(xintercept = median(cqt_data$cqt_resid_loo_sattop, na.rm = TRUE),
             linetype = "dashed", color = "firebrick", linewidth = 0.8) +
  annotate("text",
           x = median(cqt_data$cqt_resid_loo_sattop, na.rm = TRUE),
           y = Inf, vjust = 1.5, hjust = -0.1,
           label = sprintf("Median = %.3f", median(cqt_data$cqt_resid_loo_sattop, na.rm = TRUE)),
           size = 3.5, fontface = "italic", color = "firebrick") +
  annotate("text", x = Inf, y = Inf, vjust = 1.5, hjust = 1.1,
           label = sprintf("N = %d\nRange: [%.2f, %.2f]",
                           nrow(cqt_data),
                           min(cqt_data$cqt_resid_loo_sattop, na.rm = TRUE),
                           max(cqt_data$cqt_resid_loo_sattop, na.rm = TRUE)),
           size = 3, color = "grey40") +
  labs(x = "CQT Instrument (leave-one-out residualized)",
       y = "Number of women",
       title = "Distribution of Counselor Quality Tendency (CQT)") +
  theme_paper

ggsave(file.path(figures_path, "cqt_histogram.pdf"),
       p_cqt_hist, width = 7, height = 5, dpi = 300)
cat("Saved: cqt_histogram.pdf\n")


# ==============================================================================
# 19. MONOTONICITY VERIFICATION
# ==============================================================================
cat("\n--- 19. Monotonicity Verification ---\n")

# Compute share above-median quality by counselor, ordered by CQT rank
mono_data <- df %>%
  filter(year == 2016, treatment == 1, !is.na(first_counselor), !is.na(cqt_new)) %>%
  group_by(first_counselor) %>%
  summarise(
    mean_cqt = mean(cqt_new, na.rm = TRUE),
    share_above_median = mean(high_quality_new, na.rm = TRUE),
    n = n(),
    se = sqrt(share_above_median * (1 - share_above_median) / n),
    .groups = "drop"
  ) %>%
  arrange(mean_cqt) %>%
  mutate(cqt_rank = row_number(),
         counselor_label = paste0("C", first_counselor))

p_mono <- ggplot(mono_data, aes(x = reorder(counselor_label, cqt_rank), y = share_above_median)) +
  geom_col(fill = "steelblue", alpha = 0.8, width = 0.6) +
  geom_errorbar(aes(ymin = pmax(0, share_above_median - 1.96 * se),
                     ymax = pmin(1, share_above_median + 1.96 * se)),
                width = 0.2, color = "grey30") +
  geom_text(aes(label = sprintf("%.0f%%", share_above_median * 100)),
            vjust = -0.8, size = 3.5) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1.05)) +
  labs(x = "Counselor (ordered by CQT rank, low to high)",
       y = "Share receiving above-median quality",
       title = "Monotonicity: Share Above-Median Quality by CQT Rank") +
  theme_paper

ggsave(file.path(figures_path, "monotonicity_plot.pdf"),
       p_mono, width = 7, height = 5, dpi = 300)
cat("Saved: monotonicity_plot.pdf\n")


# ==============================================================================
# 20. FALSIFICATION COEFFICIENT PLOT
# ==============================================================================
cat("\n--- 20. Falsification Coefficient Plot ---\n")

# Check if falsification CSV exists from Stata; if not, run regressions in R
falsification_file <- file.path(tables_path, "falsification_baseline.csv")

if (file.exists(falsification_file)) {
  fals_df <- read.csv(falsification_file, stringsAsFactors = FALSE)
} else {
  # Run falsification tests in R: regress each baseline outcome on CQT
  bl_outcomes <- c("fertility_pref_bl", "preg_likely_nofp",
                    "positive_preg_feelings_bl", "same_children_bl")

  bl_labels <- c("Fertility preference",
                  "Pregnancy likely w/o FP",
                  "Positive pregnancy feelings",
                  "Same # children wanted")

  fals_results <- list()
  bl_data <- df %>% filter(year == 2016, treatment == 1, !is.na(cqt_resid_loo_sattop))

  for (i in seq_along(bl_outcomes)) {
    var <- bl_outcomes[i]
    if (var %in% names(bl_data)) {
      fmla <- as.formula(paste0(var, " ~ cqt_resid_loo_sattop"))
      fit <- lm(fmla, data = bl_data)
      tidy_fit <- tidy(fit) %>% filter(term == "cqt_resid_loo_sattop")
      fals_results[[i]] <- data.frame(
        outcome = bl_labels[i],
        coef = tidy_fit$estimate,
        se = tidy_fit$std.error,
        ci_lo = tidy_fit$estimate - 1.96 * tidy_fit$std.error,
        ci_hi = tidy_fit$estimate + 1.96 * tidy_fit$std.error,
        pval = tidy_fit$p.value
      )
    }
  }
  fals_df <- bind_rows(fals_results)
}

if (nrow(fals_df) > 0) {
  p_falsification <- ggplot(fals_df, aes(x = coef, y = reorder(outcome, coef))) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0.2, color = "steelblue") +
    geom_point(size = 3, color = "navy") +
    labs(x = "Coefficient on CQT instrument",
         y = "",
         title = "Falsification: CQT on Baseline Outcomes") +
    theme_paper +
    theme(axis.text.y = element_text(size = 10))

  ggsave(file.path(figures_path, "falsification_baseline.pdf"),
         p_falsification, width = 8, height = 5, dpi = 300)
  cat("Saved: falsification_baseline.pdf\n")
} else {
  cat("WARNING: No falsification results available. Run Stata code first.\n")
}


# ==============================================================================
# 21. LEAVE-ONE-COUNSELOR-OUT FOREST PLOT
# ==============================================================================
cat("\n--- 21. Leave-One-Counselor-Out ---\n")

loo_file <- file.path(tables_path, "leave_one_out_counselor.csv")

if (file.exists(loo_file)) {
  loo_df <- read.csv(loo_file, stringsAsFactors = FALSE)

  p_loo <- ggplot(loo_df, aes(x = coef, y = reorder(label, row_order))) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0.25, color = "steelblue") +
    geom_point(size = 3, color = "navy") +
    labs(x = "2SLS coefficient on unintended pregnancy (ex post)",
         y = "",
         title = "Leave-One-Counselor-Out Sensitivity") +
    theme_paper +
    theme(axis.text.y = element_text(size = 10))

  ggsave(file.path(figures_path, "leave_one_out_counselor.pdf"),
         p_loo, width = 9, height = 5, dpi = 300)
  cat("Saved: leave_one_out_counselor.pdf\n")
} else {
  cat("WARNING: leave_one_out_counselor.csv not found. Run Stata robustness code first.\n")
}
}  # ---- end SUPERSEDED diagnostic figures ----


cat("\n=== All R graphs complete ===\n")
