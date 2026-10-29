# ==============================================================================
# appendix_topic_heatmap.R
#
# Appendix figure: topic-coverage concentration by counselor. Each cell is the
# share of that counselor's sessions in which the topic was discussed. Counselors
# are labeled by their actual IDs (C1-C6) so the figure is a direct visual
# companion to the topic-coverage table (Appendix Table A5,
# table6_topic_coverage.tex) -- the cell values reproduce that table's Panel A.
# Counselor 6 has the highest counseling-quality tendency.
#
# INPUT:  tables/counselor_topic_analysis.csv  (per-counselor topic shares;
#         verified to match table6_topic_coverage.tex Panel A exactly)
# OUTPUT: figures/counselor_topic_heatmap.pdf   (Analysis copy)
#         <JHE>/figures/counselor_topic_heatmap.pdf
# ==============================================================================

suppressMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
})

base    <- Sys.getenv("MFPS_ROOT", unset = getwd())
csv_in  <- file.path(base, "Analysis", "tables", "counselor_topic_analysis.csv")
out1    <- file.path(base, "Analysis", "figures", "counselor_topic_heatmap.pdf")
out2    <- file.path(base, "Main paper", "JHE submission", "figures", "counselor_topic_heatmap.pdf")

stopifnot(file.exists(csv_in))
df <- read.csv(csv_in, stringsAsFactors = FALSE)
cat("Rows (counselors):", nrow(df), "\n")

# Topic columns in the same order as Appendix Table A5, with matching labels.
topic_cols <- c("share_t_methods", "share_t_preg", "share_t_spacing",
                "share_t_sidefx", "share_t_services", "share_t_sti",
                "share_t_partner", "share_t_child", "share_t_breast")
topic_labels <- c(
  share_t_methods  = "FP methods",
  share_t_preg     = "Pregnancy",
  share_t_spacing  = "Birth spacing",
  share_t_sidefx   = "Side effects",
  share_t_services = "FP services",
  share_t_sti      = "STI prevention",
  share_t_partner  = "Partner involvement",
  share_t_child    = "Child health",
  share_t_breast   = "Breastfeeding"
)

stopifnot(all(topic_cols %in% names(df)))

# Drop the Overall row (couns_id == 0); keep actual counselor IDs C1-C6,
# ordered by ID to match Table A5 columns.
df <- df %>% filter(couns_id != 0) %>% arrange(couns_id) %>%
  mutate(counselor = paste0("C", couns_id))

long <- df %>%
  select(counselor, all_of(topic_cols)) %>%
  pivot_longer(cols = all_of(topic_cols), names_to = "topic_var", values_to = "share") %>%
  mutate(
    topic = factor(topic_labels[topic_var], levels = rev(unname(topic_labels))),
    counselor = factor(counselor, levels = paste0("C", sort(df$couns_id)))
  )

# Verification: print the cell values so they can be checked against Table A5.
cat("\n=== cell shares (should match table6_topic_coverage.tex Panel A) ===\n")
print(long %>%
        mutate(share = round(share, 3)) %>%
        select(counselor, topic, share) %>%
        tidyr::pivot_wider(names_from = counselor, values_from = share) %>%
        as.data.frame(), row.names = FALSE)

p <- ggplot(long, aes(x = counselor, y = topic, fill = share)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", share),
                color = share > 0.5), size = 3.2, show.legend = FALSE) +
  scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "black")) +
  scale_fill_gradient(low = "white", high = "navy", limits = c(0, 1), name = "Share") +
  labs(x = "Counselor", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 11, face = "bold", color = "black"),
        axis.text.x = element_text(size = 11, face = "bold", color = "black"),
        legend.position = "right")

ggsave(out1, p, width = 8, height = 5.2, dpi = 300)
ggsave(out2, p, width = 8, height = 5.2, dpi = 300)
cat("\nSaved:", out1, "\n      ", out2, "\n")
