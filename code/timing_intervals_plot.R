#!/usr/bin/env Rscript
# timing_intervals_plot.R
#
# Coefficient plot for the interval-hazard IV: one panel for postpartum, one for
# pregnant-at-baseline, plotting the 2SLS effect of higher counseling quality on
# the probability of a pregnancy event in each non-overlapping interval (0-6,
# 6-12, 12-18, 18-24 months since index birth) with 95% CIs.
#
# Reads:  /tmp/timing_intervals_results.csv  (from results_main_iv.do)
# Writes: figures/timing_intervals_coefplot.pdf

suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(patchwork)
    library(readr)
})

repo_root <- Sys.getenv("MFPS_ROOT", unset = getwd())
analysis_dir <- file.path(repo_root, "Analysis")
figures_path <- file.path(analysis_dir, "figures")

# ---- Load IV results ------------------------------------------------------

df <- read_csv("/tmp/timing_intervals_results.csv", show_col_types = FALSE) %>%
    mutate(interval = factor(interval,
                             levels = c("0_6mo","6_12mo","12_18mo","18_24mo"),
                             labels = c("0–6 mo","6–12 mo","12–18 mo","18–24 mo")))

# Long-format: one row per (interval, subgroup) with estimate + 95% CI + sig
long <- bind_rows(
    df %>% transmute(interval, group = "Pooled",
                     b = pool_b, se = pool_se, p = pool_p),
    df %>% transmute(interval, group = "Postpartum (preg=0)",
                     b = pp_b, se = pp_se, p = pp_p),
    df %>% transmute(interval, group = "Pregnant (preg=1)",
                     b = pr_b, se = pr_se, p = pr_p)
) %>%
    mutate(
        lci = b - 1.96 * se,
        uci = b + 1.96 * se,
        sig = case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ ""),
        group = factor(group,
                       levels = c("Pooled","Postpartum (preg=0)","Pregnant (preg=1)"))
    )

# ---- Plot ------------------------------------------------------------------

# Color scheme matches het_graph_examples.R: navy = PP, orange = Preg, grey = Pool
group_colors <- c("Pooled" = "grey40",
                  "Postpartum (preg=0)" = "#1B2A4A",
                  "Pregnant (preg=1)"   = "#D46A00")

p <- ggplot(long, aes(x = interval, y = b, color = group, group = group)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
    geom_errorbar(aes(ymin = lci, ymax = uci),
                  width = 0.18, linewidth = 0.55,
                  position = position_dodge(width = 0.55)) +
    geom_point(size = 2.6,
               position = position_dodge(width = 0.55)) +
    geom_text(aes(label = sig, y = uci),
              vjust = -0.4, size = 3.4, fontface = "bold",
              position = position_dodge(width = 0.55),
              show.legend = FALSE) +
    scale_color_manual(values = group_colors, name = NULL) +
    labs(
        title = "Interval-Hazard IV: Effect of Higher Counseling Quality on Pregnancy Occurrence",
        subtitle = "Non-overlapping intervals from index birth date (excluding miscarriages, abortions, stillbirths)",
        x = "Months since index birth",
        y = "2SLS coefficient (pp change in pregnancy probability)",
        caption = "*p<0.10, **p<0.05, ***p<0.01.  Bars show 95% confidence intervals."
    ) +
    theme_minimal(base_size = 11) +
    theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 10, color = "grey30"),
        plot.caption = element_text(color = "grey40", hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
    )

ggsave(file.path(figures_path, "timing_intervals_coefplot.pdf"),
       p, width = 8, height = 5, dpi = 300)
cat("Saved: figures/timing_intervals_coefplot.pdf\n")
