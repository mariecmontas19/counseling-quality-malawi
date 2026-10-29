#!/usr/bin/env Rscript
# timing_km_plot.R
#
# Two-panel Kaplan-Meier-style cumulative-incidence plot of next pregnancy by
# months since index birth, separately for postpartum and pregnant-at-baseline
# subgroups. In each panel, two lines compare women who saw an above-median-
# quality counselor (sat+topics) to those who saw a below-median-quality one.
#
# Matches the structural style of Karra et al. (2022, PNAS) Figure 2, but with
# (i) two panels (one per pregnancy-status subgroup) instead of one, and
# (ii) the within-treatment "high-quality vs low-quality" comparison in place
# of the original control vs treatment.
#
# Reads:  /tmp/km_data.csv  (exported by /tmp/export_km_data.do)
# Writes: figures/timing_km_2panel.pdf

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

# ---- Load --------------------------------------------------------------
df <- read_csv("/tmp/km_data.csv", show_col_types = FALSE) %>%
    filter(!is.na(high_quality_avg_sattop), !is.na(preg_2016)) %>%
    mutate(
        # `event_month` = month of next pregnancy if it occurred within 24 mo;
        # otherwise censor at month 24 (we observe whether each woman had a
        # next pregnancy event by endline, so the binary censoring is at 24).
        event_month = case_when(
            next_preg_2018 == 1 & months_to_next_preg <= 24 ~ months_to_next_preg,
            TRUE ~ NA_real_
        ),
        event_observed = !is.na(event_month),
        subgroup = factor(preg_2016, levels = c(0, 1),
                          labels = c("Postpartum at baseline",
                                     "Pregnant at baseline")),
        quality = factor(high_quality_avg_sattop, levels = c(0, 1),
                         labels = c("LQ counselor (below median)",
                                    "HQ counselor (above median)"))
    )

cat("Sample sizes by subgroup x quality:\n")
print(df %>% count(subgroup, quality))

# ---- Compute cumulative incidence at each integer month ---------------
# Pointwise 95% CIs via normal approximation on the proportion:
#   SE = sqrt(p * (1 - p) / n);  band = p ± 1.96 * SE
# (Greenwood would be more standard for KM but the binary-censoring-at-24 setup
# makes the simple proportion SE more transparent.)
months <- 0:24
ci_grid <- expand_grid(subgroup = levels(df$subgroup),
                       quality  = levels(df$quality),
                       month    = months) %>%
    rowwise() %>%
    mutate(
        n_group    = sum(df$subgroup == subgroup & df$quality == quality, na.rm = TRUE),
        n_event_by = sum(df$subgroup == subgroup & df$quality == quality &
                         df$event_observed & df$event_month <= month, na.rm = TRUE),
        cum_inc    = ifelse(n_group > 0, n_event_by / n_group, NA_real_),
        surv       = 1 - cum_inc,
        se         = ifelse(n_group > 0,
                            sqrt(surv * (1 - surv) / n_group),
                            NA_real_),
        surv_lo    = pmax(0, surv - 1.96 * se),
        surv_hi    = pmin(1, surv + 1.96 * se)
    ) %>%
    ungroup()

# ---- Number-at-risk table (PNAS-style) --------------------------------
# At month t, "at risk" = women who have not yet experienced a pregnancy event
# by month t. (No competing censoring before 24 months in this binary setup.)
# We also report cumulative events by month t in parentheses.
risk_months <- seq(0, 24, 4)
risk_grid <- expand_grid(subgroup = levels(df$subgroup),
                         quality  = levels(df$quality),
                         month    = risk_months) %>%
    rowwise() %>%
    mutate(
        n_at_risk  = sum(df$subgroup == subgroup & df$quality == quality &
                         (!df$event_observed | df$event_month > month),
                         na.rm = TRUE),
        n_events_by = sum(df$subgroup == subgroup & df$quality == quality &
                          df$event_observed & df$event_month <= month,
                          na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
        quality_short = ifelse(grepl("HQ", quality), "HQ counselor", "LQ counselor"),
        label_txt     = paste0(n_at_risk, " (", n_events_by, ")")
    )

# ---- Plot --------------------------------------------------------------
# y-axis: P(not having a short-term pregnancy) by month t. Starting at 1 and
# declining as women experience a next pregnancy. Zoomed to 0.90-1.00 because
# events are rare (~5% by month 24). Shaded bands = 95% pointwise CIs.

make_km_panel <- function(sg) {
    d <- ci_grid %>% filter(subgroup == sg)
    ggplot(d, aes(x = month, y = surv, color = quality, fill = quality,
                  linetype = quality)) +
        geom_ribbon(aes(ymin = surv_lo, ymax = surv_hi),
                    alpha = 0.18, color = NA,
                    show.legend = FALSE) +
        geom_step(linewidth = 0.9) +
        geom_vline(xintercept = c(4, 8, 12, 16, 20),
                   linetype = "dotted", color = "grey80") +
        scale_color_manual(values = c(
            "LQ counselor (below median)" = "#B0B0B0",
            "HQ counselor (above median)" = "#1B2A4A"
        )) +
        scale_fill_manual(values = c(
            "LQ counselor (below median)" = "#B0B0B0",
            "HQ counselor (above median)" = "#1B2A4A"
        )) +
        scale_linetype_manual(values = c(
            "LQ counselor (below median)" = "22",
            "HQ counselor (above median)" = "solid"
        )) +
        scale_x_continuous(breaks = seq(0, 24, 4),
                           limits = c(-1.5, 25.5),
                           expand = c(0, 0)) +
        scale_y_continuous(limits = c(0.90, 1.001),
                           breaks = seq(0.90, 1.00, 0.02)) +
        labs(
            title = sg,
            x = "Months since index birth",
            y = "P(not having a pregnancy)",
            color = NULL, linetype = NULL
        ) +
        theme_minimal(base_size = 11) +
        theme(
            legend.position = "bottom",
            plot.title = element_text(face = "bold", size = 11),
            panel.grid.minor = element_blank(),
            axis.title.x = element_text(margin = margin(t = 4))
        )
}

make_risk_panel <- function(sg) {
    r <- risk_grid %>% filter(subgroup == sg) %>%
        mutate(quality_short = factor(quality_short,
                                      levels = c("HQ counselor","LQ counselor")))
    ggplot(r, aes(x = month, y = quality_short,
                  label = label_txt, color = quality_short)) +
        geom_text(size = 3.0, fontface = "plain") +
        scale_color_manual(values = c("HQ counselor" = "#1B2A4A",
                                      "LQ counselor" = "#6B6B6B"),
                           guide = "none") +
        scale_x_continuous(breaks = seq(0, 24, 4),
                           limits = c(-1.5, 25.5),
                           expand = c(0, 0)) +
        scale_y_discrete(limits = rev) +
        coord_cartesian(clip = "off") +
        labs(
            title = "Number at risk (cumulative number of events)",
            x = NULL,
            y = NULL
        ) +
        theme_minimal(base_size = 10) +
        theme(
            plot.title = element_text(face = "italic", size = 9, color = "grey30"),
            panel.grid = element_blank(),
            axis.text.y = element_text(size = 8, color = "grey20"),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
        )
}

p_pp     <- make_km_panel("Postpartum at baseline")
r_pp     <- make_risk_panel("Postpartum at baseline")
p_preg   <- make_km_panel("Pregnant at baseline")
r_preg   <- make_risk_panel("Pregnant at baseline")

p <- (p_pp / r_pp / p_preg / r_preg) +
    plot_layout(ncol = 1, heights = c(3, 0.55, 3, 0.55), guides = "collect") +
    plot_annotation(
        title = "Cumulative survival from next pregnancy by counselor quality",
        subtitle = paste0(
            "Probability of not having a next pregnancy within 24 months of index birth.\n",
            "HQ counselor = above-median satisfaction + topics quality index; LQ counselor = below-median."
        ),
        caption = paste0(
            "Shaded bands show 95% pointwise confidence intervals. Number-at-risk row reports women still\n",
            "pregnancy-free at each 4-month mark, with cumulative events in parentheses."
        ),
        theme = theme(
            legend.position = "bottom",
            plot.title = element_text(face = "bold", size = 13),
            plot.subtitle = element_text(size = 10, color = "grey30",
                                         margin = margin(b = 10)),
            plot.caption = element_text(size = 9, color = "grey40", hjust = 0.5)
        )
    )

ggsave(file.path(figures_path, "timing_km_2panel.pdf"),
       p, width = 9, height = 9, dpi = 300)
cat("Saved: figures/timing_km_2panel.pdf\n")
