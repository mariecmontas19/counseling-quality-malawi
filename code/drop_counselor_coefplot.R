#!/usr/bin/env Rscript
# drop_counselor_coefplot.R
#
# Leave-one-counselor-out (LOO) coefficient plot for the JHE paper, Section 5.5.
#
# Layout: 2 rows x 3 facets.
#   Top row (Panel C, STABLE outcomes):
#     - admin_fp_any         "Any FP service"
#     - n_admin_fp           "Number of FP services"
#     - fp_inj_coverage_frac "Injectable coverage"
#   Bottom row (Panel D, C6-DRIVEN outcomes):
#     - birth_within_24mo    "Birth within 24 mo"
#     - preg_within_24mo     "Pregnancy within 24 mo"
#     - mistimed_preg_expost "Mistimed pregnancy"
#
# Within each facet: 7 x-positions (Main, Drop C1, ..., Drop C6); 3 dodged colored
# points per position (Pooled grey, PP navy, Preg orange); 95% CI bars; horizontal
# dashed reference line at 0; significance stars.
#
# Reads:  /tmp/phase7a_results.csv  (produced by dofile/results_robust_dropcouns.do)
# Writes: figures/drop_counselor_coefplot.pdf

suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(readr)
})

repo_root <- Sys.getenv("MFPS_ROOT", unset = getwd())
analysis_dir <- file.path(repo_root, "Analysis")
figures_path <- file.path(analysis_dir, "figures")

# ---- Load ----------------------------------------------------------------

df <- read_csv("/tmp/phase7a_results.csv", show_col_types = FALSE)

stopifnot(nrow(df) > 0)
stopifnot(all(c("outcome", "spec", "subgroup", "b", "se", "p") %in% names(df)))

# ---- Select the 6 outcomes for the main-text coefplot --------------------

keep_outs <- c(
    "admin_fp_any", "n_admin_fp", "fp_inj_coverage_frac",  # Panel C (stable)
    "birth_within_24mo", "preg_within_24mo", "mistimed_preg_expost"  # Panel D (C6-driven)
)

out_labels <- c(
    admin_fp_any         = "Any FP service",
    n_admin_fp           = "Number of FP services",
    fp_inj_coverage_frac = "Injectable coverage",
    birth_within_24mo    = "Birth within 24 mo",
    preg_within_24mo     = "Pregnancy within 24 mo",
    mistimed_preg_expost = "Mistimed pregnancy"
)

panel_labels <- c(
    admin_fp_any         = "Panel C: FP coverage (stable under LOO)",
    n_admin_fp           = "Panel C: FP coverage (stable under LOO)",
    fp_inj_coverage_frac = "Panel C: FP coverage (stable under LOO)",
    birth_within_24mo    = "Panel D: Reproductive (C6-driven)",
    preg_within_24mo     = "Panel D: Reproductive (C6-driven)",
    mistimed_preg_expost = "Panel D: Reproductive (C6-driven)"
)

# ---- Build long-format dataset -------------------------------------------

long <- df %>%
    filter(outcome %in% keep_outs) %>%
    mutate(
        outcome_lab = factor(out_labels[outcome],
                             levels = unname(out_labels[keep_outs])),
        panel_lab   = factor(panel_labels[outcome],
                             levels = c("Panel C: FP coverage (stable under LOO)",
                                        "Panel D: Reproductive (C6-driven)")),
        spec_lab    = factor(case_when(
            spec == "Main" ~ "Main",
            spec == "dC1"  ~ "Drop C1",
            spec == "dC2"  ~ "Drop C2",
            spec == "dC3"  ~ "Drop C3",
            spec == "dC4"  ~ "Drop C4",
            spec == "dC5"  ~ "Drop C5",
            spec == "dC6"  ~ "Drop C6",
            TRUE ~ NA_character_
        ), levels = c("Main", "Drop C1", "Drop C2", "Drop C3",
                      "Drop C4", "Drop C5", "Drop C6")),
        group_lab = factor(case_when(
            subgroup == "Pool" ~ "Pooled",
            subgroup == "PP"   ~ "Postpartum",
            subgroup == "Preg" ~ "Pregnant",
            TRUE ~ NA_character_
        ), levels = c("Pooled", "Postpartum", "Pregnant")),
        lci = b - 1.96 * se,
        uci = b + 1.96 * se,
        sig = case_when(p < 0.01 ~ "***",
                        p < 0.05 ~ "**",
                        p < 0.10 ~ "*",
                        TRUE ~ "")
    ) %>%
    filter(!is.na(spec_lab), !is.na(group_lab), !is.na(outcome_lab))

# Diagnostic: confirm 6 outcomes x 7 specs x 3 subgroups = 126 rows expected
cat("Long-format dataset:\n")
cat("  rows:", nrow(long), "(expected 126 = 6 outcomes x 7 specs x 3 subgroups)\n")
cat("  unique outcomes:", length(unique(long$outcome)), "\n")
cat("  unique specs:", length(unique(long$spec_lab)), "\n")
cat("  unique subgroups:", length(unique(long$group_lab)), "\n")

# ---- Plot ----------------------------------------------------------------

group_colors <- c("Pooled"     = "grey40",
                  "Postpartum" = "#1B2A4A",
                  "Pregnant"   = "#D46A00")

p <- ggplot(long, aes(x = spec_lab, y = b, color = group_lab, group = group_lab)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
    geom_errorbar(aes(ymin = lci, ymax = uci),
                  width = 0.18, linewidth = 0.45,
                  position = position_dodge(width = 0.55)) +
    geom_point(size = 2.2,
               position = position_dodge(width = 0.55)) +
    geom_text(aes(label = sig, y = uci),
              vjust = -0.35, size = 2.7, fontface = "bold",
              position = position_dodge(width = 0.55),
              show.legend = FALSE) +
    facet_wrap(~ outcome_lab, nrow = 2, scales = "free_y") +
    scale_color_manual(values = group_colors, name = NULL) +
    labs(
        title = "Leave-One-Counselor-Out IV Estimates",
        subtitle = "Top row: FP-coverage outcomes (Panel C). Bottom row: reproductive outcomes (Panel D).",
        x = NULL,
        y = "2SLS coefficient",
        caption = "*p<0.10, **p<0.05, ***p<0.01.  Bars show 95% confidence intervals."
    ) +
    theme_minimal(base_size = 10) +
    theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 10, color = "grey30"),
        plot.caption = element_text(size = 8, color = "grey40", hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.text = element_text(face = "bold", size = 9),
        axis.text.x = element_text(angle = 35, hjust = 1, size = 8)
    )

ggsave(file.path(figures_path, "drop_counselor_coefplot.pdf"),
       p, width = 11, height = 6.5, dpi = 300)

cat("Saved: figures/drop_counselor_coefplot.pdf\n")
