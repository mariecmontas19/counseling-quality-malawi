################################################################################
# HETEROGENEOUS EFFECTS FIGURE
# Publication-quality grouped coefficient plot: IV het-preg results
# 2SLS estimates by pregnancy status at baseline
# Stars-only at dots + p-values for subgroup difference on right margin
# Two PDFs: (A+B) outcomes, (C+D) mechanisms
################################################################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# Paths
figures_path <- file.path(
  path.expand("~"),
  "Library/CloudStorage/Dropbox/PhD Harvard/Dissertation",
  "Malawi paper/Counseling Qual MFPS paper/Main paper/Preliminary results/figures"
)

# Theme matching existing paper style
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

# Colors
col_pp   <- "#1B2A4A"   # dark navy blue for postpartum
col_preg <- "#D46A00"   # dark orange for pregnant

# ==============================================================================
# DATA: IV het-preg coefficients (from analysis_outcomes_02-10-2026.do)
# ==============================================================================

# Display metadata only (outcome -> panel + axis label). The NUMBERS (PP/Preg
# 2SLS estimates, robust SEs, and the p(PP=Preg) subgroup-difference p-value) are
# read below directly from the same results CSV that the LaTeX table builder
# consumes (/tmp/phase5_v2_results.csv, written by results_main_iv.do), so the
# figure can never drift from Table 5. Refactored June 2026 (referee comment 13).
het_meta <- tribble(
  ~outcome, ~label, ~panel,
  "total_sessions",        "Total sessions",                  "A: Counseling continuity",
  "share_same_counselor",  "Same counselor\nshare",           "A: Counseling continuity",
  "visit_rate_mo",         "Counseling sessions\nper month",  "A: Counseling continuity",
  "husband_present",       "Husband\npresent",                "A: Counseling continuity",
  "etp1_fp",               "FP info",                         "B: Counseling topics",
  "etp1_spacing",          "Birth spacing",                   "B: Counseling topics",
  "etp1_preg",             "Pregnancy info",                  "B: Counseling topics",
  "etp1_partner",          "Partner engagement",              "B: Counseling topics",
  "etp1_childhealth",      "Child health",                    "B: Counseling topics",
  "etp1_breast",           "Breastfeeding",                   "B: Counseling topics",
  "admin_fp_any",          "Any FP service",                  "C: FP coverage",
  "admin_inject_any",      "Any injectable",                  "C: FP coverage",
  "fp_inj_coverage_frac",  "Injectable\ncoverage",            "C: FP coverage",
  "husband_knows_fp",      "Husband knows FP",                "C: FP coverage",
  "birth_within_24mo",     "Birth within\n24 months",         "D: Reproductive outcomes",
  "preg_within_12mo",      "Pregnancy within\n12 months",     "D: Reproductive outcomes",
  "preg_within_24mo",      "Pregnancy within\n24 months",     "D: Reproductive outcomes",
  "mistimed_preg_expost",  "Mistimed pregnancy",              "D: Reproductive outcomes",
)

iv_csv_path <- "/tmp/phase5_v2_results.csv"
if (!file.exists(iv_csv_path)) {
  stop("phase5_v2_results.csv not found. Run results_main_iv.do before this figure script.")
}
iv_csv <- read.csv(iv_csv_path, stringsAsFactors = FALSE)

# Map the CSV columns onto the names the plotting code expects and join by outcome.
het_data <- het_meta %>%
  left_join(
    iv_csv %>%
      transmute(outcome,
                pp_b   = pp_b,  pp_se   = pp_se,
                preg_b = pr_b,  preg_se = pr_se,
                p_diff = pdiff),
    by = "outcome"
  )

# Guard: every plotted outcome must be present in the results CSV, so the figure
# stays in lockstep with the table.
missing_outcomes <- het_data$outcome[is.na(het_data$pp_b)]
if (length(missing_outcomes) > 0) {
  stop("Outcomes missing from phase5_v2_results.csv: ",
       paste(missing_outcomes, collapse = ", "))
}

# Reshape to long format — build PP and Preg rows separately, then bind
het_pp <- het_data %>%
  transmute(outcome, label, panel, p_diff,
            group = "Postpartum at BL",
            estimate = pp_b, se = pp_se)
het_preg <- het_data %>%
  transmute(outcome, label, panel, p_diff,
            group = "Pregnant at BL",
            estimate = preg_b, se = preg_se)
het_long <- bind_rows(het_pp, het_preg) %>%
  mutate(
    lci = estimate - 1.96 * se,
    uci = estimate + 1.96 * se,
    p = 2 * pnorm(-abs(estimate / se)),
    sig = case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")
  )

# Order outcomes within panels (reverse for horizontal plot)
het_long <- het_long %>%
  mutate(label = factor(label, levels = rev(unique(het_data$label))))


# ==============================================================================
# GROUPED HORIZONTAL FOREST PLOT
# Stars only at each dot; p_diff on right margin
# ==============================================================================

# X-axis limits per panel
panel_xlims <- list(
  "A: Counseling continuity"  = c(-1.00, 0.20),
  "B: Counseling topics"      = c(-0.30, 1.60),
  "C: FP coverage"            = c(-0.20, 0.45),
  "D: Reproductive outcomes"  = c(-0.20, 0.30)
)

make_panel <- function(panel_name, data) {
  d <- data %>% filter(panel == panel_name)

  # p-diff annotation with stars for all outcomes
  pdiff_labels <- d %>%
    filter(group == "Postpartum at BL") %>%
    mutate(
      pdiff_stars = case_when(
        is.na(p_diff) ~ "",
        p_diff < 0.01 ~ "***",
        p_diff < 0.05 ~ "**",
        p_diff < 0.10 ~ "*",
        TRUE ~ ""
      ),
      pdiff_text = case_when(
        is.na(p_diff) ~ "(preg only)",
        p_diff < 0.001 ~ paste0("p<0.001", pdiff_stars),
        TRUE ~ sprintf("p=%.3f%s", p_diff, pdiff_stars)
      )
    )

  pd <- position_dodge(width = 0.5)

  p <- ggplot(d, aes(x = estimate, y = label, color = group, shape = group)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
    geom_linerange(aes(xmin = lci, xmax = uci),
                   linewidth = 0.7, position = pd) +
    geom_point(size = 3.5, position = pd) +
    # Significance stars only — placed just right of each dot
    geom_text(aes(label = sig), position = pd,
              hjust = -0.3, vjust = 0.35, size = 4.5,
              show.legend = FALSE, fontface = "bold") +
    # p-diff annotations on right margin (difference test)
    geom_text(data = pdiff_labels,
              aes(x = Inf, y = label, label = pdiff_text),
              hjust = 1.05, size = 3.5, color = "grey40", inherit.aes = FALSE) +
    scale_color_manual(values = c("Postpartum at BL" = col_pp,
                                  "Pregnant at BL"   = col_preg)) +
    scale_shape_manual(values = c("Postpartum at BL" = 16,
                                  "Pregnant at BL"   = 17)) +
    labs(x = NULL, y = NULL,
         title = panel_name,
         color = NULL, shape = NULL) +
    # 0.1-by-0.1 axis: labeled tick + gridline at every 0.1
    scale_x_continuous(breaks = seq(-3, 3, by = 0.1)) +
    theme(
      legend.position = "bottom",
      legend.key.size = unit(0.4, "cm"),
      legend.text = element_text(size = 12),
      plot.title = element_text(size = 15, face = "bold"),
      axis.text.y = element_text(size = 13, face = "bold"),
      axis.text.x = element_text(size = 8),
      panel.grid.major.x = element_line(color = "grey88", linewidth = 0.3),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    )

  # Apply fixed x limits if specified; clip = "off" for margin labels
  xlim <- panel_xlims[[panel_name]]
  if (!is.null(xlim)) {
    p <- p + coord_cartesian(xlim = xlim, clip = "off")
  } else {
    p <- p + coord_cartesian(clip = "off")
  }

  p
}

panels <- lapply(unique(het_data$panel), make_panel, data = het_long)
# `panels` now has indices 1..4 corresponding to panels A, B, C, D in that order,
# which after the renumbering = continuity, topics, FP coverage, reproductive.


# ==============================================================================
# PER-PANEL EXPORTS (for the ASHEcon talk: one panel per slide)
# Each saved as a standalone single-panel PDF into the JHE submission figures
# folder, with its own legend and significance caption.
# ==============================================================================

jhe_figures_path <- file.path(
  path.expand("~"),
  "Library/CloudStorage/Dropbox/PhD Harvard/Dissertation",
  "Malawi paper/Counseling Qual MFPS paper/Main paper/JHE submission/figures"
)

panel_files   <- c("panelA_continuity.pdf", "panelB_topics.pdf",
                   "panelC_fpcoverage.pdf", "panelD_reproductive.pdf")
panel_heights <- c(4.8, 6.2, 5.6, 4.8)   # taller for the 6-row topics panel

for (i in seq_along(panels)) {
  p_single <- panels[[i]] +
    plot_annotation(
      caption = "*p<0.10, **p<0.05, ***p<0.01. 2SLS estimates with 95% CIs.",
      theme = theme(
        legend.position = "bottom",
        plot.caption = element_text(size = 11, color = "grey40", hjust = 0.5)
      )
    )
  ggsave(file.path(jhe_figures_path, panel_files[i]),
         p_single, width = 9, height = panel_heights[i], dpi = 300)
  cat("Saved:", panel_files[i], "\n")
}


# ==============================================================================
# Figure 6 (het_preg_mechanisms.pdf): Panels A + B = Continuity + Topics
# Matches Table 9 in the paper.
# ==============================================================================

p_mechanisms <- panels[[1]] + panels[[2]] +
  plot_layout(ncol = 1, guides = "collect") +
  plot_annotation(
    caption = "*p<0.10, **p<0.05, ***p<0.01. 2SLS estimates with 95% CIs.",
    theme = theme(
      legend.position = "bottom",
      plot.caption = element_text(size = 11, color = "grey40", hjust = 0.5)
    )
  )

ggsave(file.path(figures_path, "het_preg_mechanisms.pdf"),
       p_mechanisms, width = 10, height = 11, dpi = 300)
ggsave(file.path(jhe_figures_path, "het_preg_mechanisms.pdf"),
       p_mechanisms, width = 10, height = 11, dpi = 300)
cat("Saved: het_preg_mechanisms.pdf (Panels A + B = Continuity + Topics)\n")


# ==============================================================================
# Figure 7 (het_preg_outcomes.pdf): Panels C + D = FP coverage + Reproductive
# Matches Table 10 in the paper.
# ==============================================================================

p_outcomes <- panels[[3]] + panels[[4]] +
  plot_layout(ncol = 1, guides = "collect") +
  plot_annotation(
    caption = "*p<0.10, **p<0.05, ***p<0.01. 2SLS estimates with 95% CIs.",
    theme = theme(
      legend.position = "bottom",
      plot.caption = element_text(size = 11, color = "grey40", hjust = 0.5)
    )
  )

ggsave(file.path(figures_path, "het_preg_outcomes.pdf"),
       p_outcomes, width = 10, height = 11, dpi = 300)
ggsave(file.path(jhe_figures_path, "het_preg_outcomes.pdf"),
       p_outcomes, width = 10, height = 11, dpi = 300)
cat("Saved: het_preg_outcomes.pdf (Panels C + D = FP coverage + Reproductive)\n")
