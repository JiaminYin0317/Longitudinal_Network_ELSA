#### Longitudinal Network Analysis: Loneliness, Depressive Symptoms, and Cognition ####

# ---- 0. Setup ----
rm(list = ls())

packages <- c(
  "dplyr", "tidyverse", "Hmisc", "psych", "qgraph", "bootnet",
  "networktools", "CliquePercolation", "NetworkComparisonTest",
  "magick", "janitor", "labelled", "zoo", "tidyverse", "stringr",
  "purrr", "readr", "psychonetrics", "glmnet", "ggplot2", "haven",
  "tseries", "mgcv", "patchwork"
)

# install.packages(setdiff(packages, rownames(installed.packages())))

lapply(packages, library, character.only = TRUE)

set.seed(1234)

root <- "/Users/jiaminyin/Library/CloudStorage/OneDrive-UniversityofRochester/Research/ELSA_proj/Network_Analysis/"
# Set working directory
setwd("/Users/jiaminyin/Library/CloudStorage/OneDrive-UniversityofRochester/Research/ELSA_proj/Network_Analysis/output")

final_pruned <- readRDS("fit_pruned_adj.rds") # Decide to use the model searched one

# ---- 1. Panel GVAR model fit ----
fit_indices <- fit(final_pruned)

panel_gvar_fit_table <- fit_indices %>%
  filter(
    Measure %in% c(
      "nobs",
      "npar",
      "df",
      "chisq",
      "pvalue",
      "cfi",
      "tli",
      "rmsea",
      "rmsea.ci.lower",
      "rmsea.ci.upper",
      "srmr"
    )
  ) %>%
  mutate(
    Measure = recode(
      Measure,
      nobs = "Observations",
      npar = "Number of parameters",
      df = "Degrees of freedom",
      chisq = "Chi-square",
      pvalue = "p-value",
      cfi = "CFI",
      tli = "TLI",
      rmsea = "RMSEA",
      rmsea.ci.lower = "RMSEA 95% CI lower",
      rmsea.ci.upper = "RMSEA 95% CI upper",
      srmr = "SRMR"
    ),
    Value = case_when(
      Measure == "p-value" & as.numeric(Value) < 0.001 ~ "<0.001",
      Measure %in% c("CFI", "TLI", "RMSEA", "RMSEA 95% CI lower",
                     "RMSEA 95% CI upper", "SRMR") ~ sprintf("%.4f", as.numeric(Value)),
      Measure %in% c("Chi-square") ~ sprintf("%.2f", as.numeric(Value)),
      TRUE ~ as.character(round(as.numeric(Value), 0))
    )
  )

## Save model results
write.csv(
  panel_gvar_fit_table,
  "panelGVAR_fit_indices_adj.csv",
  row.names = FALSE
)


# ---- 2. Node labels, groups, colors, and communities ----

node_labels <- c(
  "LON1", "LON2", "LON3",
  "DEP1", "DEP2", "DEP3", "DEP4", "DEP5", "DEP6", "DEP7",
  "COG1", "COG2", "COG3"
)

node_construct <- c(
  rep("Loneliness", 3),
  rep("Depressive symptoms", 7),
  rep("Cognition", 3)
)

names(node_construct) <- node_labels

node_groups <- list(
  Loneliness = 1:3,
  Depression = 4:10,
  Cognition = 11:13
)

construct_lookup <- tibble(
  Node = node_labels,
  Construct = node_construct
)

node_colors <- c(
  rep("#E69F00", 3),
  rep("#4C78A8", 7),
  rep("#005A4A", 3)
)

communities <- c(
  rep("Loneliness", 3),
  rep("Depression", 7),
  rep("Cognition", 3)
)

# stopifnot(length(node_labels) == ncol(network_dat))
# stopifnot(length(communities) == ncol(network_dat))

legend_labels <- c(
  "Loneliness",
  "LON1  Lack of companionship",
  "LON2  Feeling left out",
  "LON3  Feeling isolated",
  "",
  "Depressive symptoms",
  "DEP1  Depressed mood",
  "DEP2  Greater effort",
  "DEP3  Restless sleep",
  "DEP4  Reduced happiness",
  "DEP5  Sadness",
  "DEP6  Difficulty getting going",
  "DEP7  Reduced enjoyment of life",
  "",
  "Cognition",
  "COG1  Immediate word recall",
  "COG2  Delayed word recall",
  "COG3  Verbal fluency"
)

legend_pch <- c(NA, rep(16, 3), 
                NA, 
                NA, rep(16, 7), 
                NA,
                NA, rep(16, 3))
legend_col <- c(
  "transparent",rep("#E69F00", 3),
  "transparent",
  "transparent",rep("#4C78A8", 7),
  "transparent",
  "transparent",rep("#005A4A", 3)
)

# ---- 3. Create network graphs ----

set.seed(123)

common_layout <- matrix(
  c(
    # x,     y
    
    # Loneliness
    -0.62, -0.02,   # LON1
    -0.05,  0.62,   # LON2
    0.88, -0.08,   # LON3
    
    # Depressive symptoms
    -1.30,  0.28,   # DEP1
    -1.00, -0.38,   # DEP2
    -0.35,  1.18,   # DEP3
    -1.05,  0.85,   # DEP4
    1.12,  1.18,   # DEP5
    1.08, -0.70,   # DEP6
    1.55,  0.45,   # DEP7
    
    # Cognition
    -0.95, -0.92,   # COG1
    -0.18, -1.12,   # COG2
    1.05, -1.22    # COG3
  ),
  ncol = 2,
  byrow = TRUE
)

rownames(common_layout) <- node_labels

# Temporal network: directed lag-1 effects
temporal_mat <- getmatrix(final_pruned, "beta")
rownames(temporal_mat) <- node_labels
colnames(temporal_mat) <- node_labels

temporal_cross_mat <- temporal_mat

for (i in seq_len(nrow(temporal_cross_mat))) {
  for (j in seq_len(ncol(temporal_cross_mat))) {
    if (node_construct[rownames(temporal_cross_mat)[i]] ==
        node_construct[colnames(temporal_cross_mat)[j]]) {
      temporal_cross_mat[i, j] <- 0
    }
  }
}

temporal_cross_plot_mat <- t(temporal_cross_mat)

png("fig_long_temporal_network_adj.png", width = 6000, height = 3600, res = 300)

# Split figure into network panel and legend panel
layout(matrix(c(1, 2), nrow = 1), widths = c(3.2, 1.15))

# -- Panel 1: Network
par(mar = c(1.5, 1.5, 2, 0.5))

qgraph(
  temporal_cross_plot_mat,
  
  # Layout
  layout = common_layout,
  
  # Nodes
  labels = node_labels,
  groups = node_groups,
  color = node_colors,
  vsize = 8,
  label.cex = 1.05,
  
  # Directed temporal edges
  directed = TRUE,
  arrows = TRUE,
  asize = 4,
  
  # Edge colors
  posCol = "#4D4D4D",
  negCol = "#8E4585",
  
  # Reduce visual heaviness
  # fade = FALSE,
  # esize = 5,
  
  # Separate reciprocal edges
  # curve = 0.35,
  # parallelEdge = TRUE,
  
  # Keep clean
  legend = FALSE,
  minimum = 0,
  edge.labels = FALSE,
  title = ""
)

# -- Panel 2: Legend
par(mar = c(2, 0, 5, 1))
plot.new()

legend(
  x = 0,
  y = 1,
  legend = legend_labels,
  pch = legend_pch,
  col = legend_col,
  pt.cex = 2.2,
  cex = 1.25,
  bty = "n",
  y.intersp = 1.35,
  x.intersp = 0.8
)

legend(
  x = 0,
  y = 0.3,
  legend = c(
    "Edge sign",
    "Positive association",
    "Negative association"
  ),
  col = c(
    "transparent",
    "#4D4D4D",
    "#8E4585"
  ),
  lty = c(NA, 1, 1),
  lwd = c(NA, 4, 4),
  cex = 1.25,
  bty = "n",
  y.intersp = 1.35
)

dev.off()

# Contemporaneous network: same-wave partial correlations
contemp_mat <- getmatrix(final_pruned, "omega_zeta_within")
rownames(contemp_mat) <- node_labels
colnames(contemp_mat) <- node_labels

contemp_cross_mat <- contemp_mat

for (i in seq_len(nrow(contemp_cross_mat))) {
  for (j in seq_len(ncol(contemp_cross_mat))) {
    if (node_construct[rownames(contemp_cross_mat)[i]] ==
        node_construct[colnames(contemp_cross_mat)[j]]) {
      contemp_cross_mat[i, j] <- 0
    }
  }
}

contemp_cross_plot_mat <- t(contemp_cross_mat)

png("fig_long_contemp_network_adj.png", width = 6000, height = 3600, res = 300)

# Split figure into network panel and legend panel
layout(matrix(c(1, 2), nrow = 1), widths = c(2.6, 1))

# -- Panel 1: Network
par(mar = c(2, 2, 5, 1))

qgraph(
  contemp_cross_plot_mat,
  # Layout
  layout = common_layout,
  
  # Nodes
  labels = node_labels,
  groups = node_groups,
  color = node_colors,
  vsize = 8,
  label.cex = 1.05,
  
  # Directed temporal edges
  directed = FALSE,
  arrows = TRUE,
  asize = 4,
  
  # Edge colors
  posCol = "#4D4D4D",
  negCol = "#8E4585",
  
  # Reduce visual heaviness
  # fade = FALSE,
  # esize = 5,
  
  # Separate reciprocal edges
  # curve = 0.35,
  # parallelEdge = TRUE,
  
  # Keep clean
  legend = FALSE,
  minimum = 0,
  edge.labels = FALSE,
  title = ""
)

# -- Panel 2: Legend
par(mar = c(2, 0, 5, 1))
plot.new()

legend(
  x = 0,
  y = 1,
  legend = legend_labels,
  pch = legend_pch,
  col = legend_col,
  pt.cex = 2.2,
  cex = 1.25,
  bty = "n",
  y.intersp = 1.35,
  x.intersp = 0.8
)

legend(
  x = 0,
  y = 0.3,
  legend = c(
    "Edge sign",
    "Positive association",
    "Negative association"
  ),
  col = c(
    "transparent",
    "#4D4D4D",
    "#8E4585"
  ),
  lty = c(NA, 1, 1),
  lwd = c(NA, 4, 4),
  cex = 1.25,
  bty = "n",
  y.intersp = 1.35
)

dev.off()

# Between-person network: stable between-person associations
between_mat <- getmatrix(final_pruned, "omega_zeta_between")
rownames(between_mat) <- node_labels
colnames(between_mat) <- node_labels

between_cross_mat <- between_mat

for (i in seq_len(nrow(between_cross_mat))) {
  for (j in seq_len(ncol(between_cross_mat))) {
    if (node_construct[i] == node_construct[j]) {
      between_cross_mat[i, j] <- 0
    }
  }
}

png("fig_long_between_network_adj.png", width = 6000, height = 3600, res = 300)

# Split figure into network panel and legend panel
layout(matrix(c(1, 2), nrow = 1), widths = c(2.6, 1))

# -- Panel 1: Network
par(mar = c(2, 2, 5, 1))

qgraph(
  between_mat,
  # Layout
  layout = common_layout,
  
  # Nodes
  labels = node_labels,
  groups = node_groups,
  color = node_colors,
  vsize = 8,
  label.cex = 1.05,
  
  # Directed temporal edges
  directed = FALSE,
  arrows = TRUE,
  asize = 4,
  
  # Edge colors
  posCol = "#4D4D4D",
  negCol = "#8E4585",
  
  # Reduce visual heaviness
  # fade = FALSE,
  # esize = 5,
  
  # Separate reciprocal edges
  # curve = 0.35,
  # parallelEdge = TRUE,
  
  # Keep clean
  legend = FALSE,
  minimum = 0,
  edge.labels = FALSE,
  title = ""
)

# -- Panel 2: Legend
par(mar = c(2, 0, 5, 1))
plot.new()

legend(
  x = 0,
  y = 1,
  legend = legend_labels,
  pch = legend_pch,
  col = legend_col,
  pt.cex = 2.2,
  cex = 1.25,
  bty = "n",
  y.intersp = 1.35,
  x.intersp = 0.8
)

dev.off()

# ---- 6. All edges ----
param_table <- parameters(final_pruned)

# Temporal edges
temporal_edges_cross <- param_table %>%
  filter(matrix == "beta") %>%
  mutate(
    From = node_labels[col],
    To   = node_labels[row],
    From_construct = node_construct[From],
    To_construct   = node_construct[To],
    Edge_type = ifelse(
      From_construct == To_construct,
      "Within-domain",
      "Cross-domain"
    )
  ) %>%
  filter(
    Edge_type == "Cross-domain" #, est != 0
  ) %>%
  mutate(
    Direction = paste0(From, " \u2192 ", To),
    Construct_path = paste0(From_construct, " \u2192 ", To_construct),
    abs_est = abs(est),
    p_display = case_when(
      is.na(p) ~ NA_character_,
      p < 0.001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", p)
    )
  ) %>%
  arrange(desc(abs_est)) %>%
  select(
    Direction,
    Construct_path,
    From_construct,
    From,
    To_construct,
    To,
    Estimate = est,
    SE = se,
    p = p_display,
    abs_est
  )
write.csv(temporal_edges_cross, "tbl_temporal_edges_cross_adj.csv", row.names = FALSE)

temporal_edges_within <- param_table %>%
  filter(matrix == "beta") %>%
  mutate(
    From = node_labels[col],
    To   = node_labels[row],
    From_construct = node_construct[From],
    To_construct   = node_construct[To],
    Edge_type = ifelse(
      From_construct == To_construct,
      "Within-domain",
      "Cross-domain"
    )
  ) %>%
  filter(
    Edge_type == "Within-domain" #, est != 0
  ) %>%
  mutate(
    Direction = paste0(From, " \u2192 ", To),
    Construct_path = paste0(From_construct, " \u2192 ", To_construct),
    abs_est = abs(est),
    p_display = case_when(
      is.na(p) ~ NA_character_,
      p < 0.001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", p)
    )
  ) %>%
  arrange(Direction,From,To) %>%
  select(
    Direction,
    Construct_path,
    From_construct,
    From,
    To_construct,
    To,
    Estimate = est,
    SE = se,
    p = p_display,
    abs_est
  )
write.csv(temporal_edges_within, "tbl_temporal_edges_within_adj.csv", row.names = FALSE)

# Contemporaneous edges
contemp_edges_cross <- param_table %>%
  filter(str_detect(matrix, "omega_zeta_within")) %>%
  mutate(
    From = node_labels[col],
    To   = node_labels[row],
    From_construct = node_construct[From],
    To_construct   = node_construct[To],
    Edge_type = ifelse(
      From_construct == To_construct,
      "Within-domain",
      "Cross-domain"
    )
  ) %>%
  filter(
    Edge_type == "Cross-domain" #, est != 0
  ) %>%
  mutate(
    Direction = paste0(From, " \u2192 ", To),
    Construct_path = paste0(From_construct, " \u2192 ", To_construct),
    abs_est = abs(est),
    p_display = case_when(
      is.na(p) ~ NA_character_,
      p < 0.001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", p)
    )
  ) %>%
  arrange(desc(abs_est)) %>%
  select(
    Direction,
    Construct_path,
    From_construct,
    From,
    To_construct,
    To,
    Estimate = est,
    SE = se,
    p = p_display,
    abs_est
  )
write.csv(contemp_edges_cross, "tbl_contemp_edges_cross_adj.csv", row.names = FALSE)

contemp_edges_within <- param_table %>%
  filter(matrix == "omega_zeta_within") %>%
  mutate(
    From = node_labels[col],
    To   = node_labels[row],
    From_construct = node_construct[From],
    To_construct   = node_construct[To],
    Edge_type = ifelse(
      From_construct == To_construct,
      "Within-domain",
      "Cross-domain"
    )
  ) %>%
  filter(
    Edge_type == "Within-domain" #, est != 0
  ) %>%
  mutate(
    Direction = paste0(From, " \u2192 ", To),
    Construct_path = paste0(From_construct, " \u2192 ", To_construct),
    abs_est = abs(est),
    p_display = case_when(
      is.na(p) ~ NA_character_,
      p < 0.001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", p)
    )
  ) %>%
  arrange(Direction,From,To) %>%
  select(
    Direction,
    Construct_path,
    From_construct,
    From,
    To_construct,
    To,
    Estimate = est,
    SE = se,
    p = p_display,
    abs_est
  )
write.csv(contemp_edges_within, "tbl_contemp_edges_within_adj.csv", row.names = FALSE)

# Between-person edges
between_edges_cross <- param_table %>%
  filter(str_detect(matrix, "omega_zeta_between")) %>%
  mutate(
    From = node_labels[col],
    To   = node_labels[row],
    From_construct = node_construct[From],
    To_construct   = node_construct[To],
    Edge_type = ifelse(
      From_construct == To_construct,
      "Within-domain",
      "Cross-domain"
    )
  ) %>%
  filter(
    Edge_type == "Cross-domain" #, est != 0
  ) %>%
  mutate(
    Direction = paste0(From, " \u2192 ", To),
    Construct_path = paste0(From_construct, " \u2192 ", To_construct),
    abs_est = abs(est),
    p_display = case_when(
      is.na(p) ~ NA_character_,
      p < 0.001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", p)
    )
  ) %>%
  arrange(desc(abs_est)) %>%
  select(
    Direction,
    Construct_path,
    From_construct,
    From,
    To_construct,
    To,
    Estimate = est,
    SE = se,
    p = p_display,
    abs_est
  )
write.csv(between_edges_cross, "tbl_between_edges_cross_adj.csv", row.names = FALSE)

between_edges_within <- param_table %>%
  filter(str_detect(matrix, "omega_zeta_between")) %>%
  mutate(
    From = node_labels[col],
    To   = node_labels[row],
    From_construct = node_construct[From],
    To_construct   = node_construct[To],
    Edge_type = ifelse(
      From_construct == To_construct,
      "Within-domain",
      "Cross-domain"
    )
  ) %>%
  filter(
    Edge_type == "Within-domain" #, est != 0
  ) %>%
  mutate(
    Direction = paste0(From, " \u2192 ", To),
    Construct_path = paste0(From_construct, " \u2192 ", To_construct),
    abs_est = abs(est),
    p_display = case_when(
      is.na(p) ~ NA_character_,
      p < 0.001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", p)
    )
  ) %>%
  arrange(desc(abs_est)) %>%
  select(
    Direction,
    Construct_path,
    From_construct,
    From,
    To_construct,
    To,
    Estimate = est,
    SE = se,
    p = p_display,
    abs_est
  )
write.csv(between_edges_within, "tbl_between_edges_within_adj.csv", row.names = FALSE)

# ---- 7. Create bridge and centrality graphs ----
# Bridge
bridge_temporal <- bridge(
  temporal_mat,
  communities = node_construct
)

bridge_contemp <- bridge(
  contemp_mat,
  communities = node_labels
)

p_temporal <- plot(
  bridge_temporal,
  include = c(
    "Bridge Strength",
    "Bridge Expected Influence (1-step)",
    "Bridge Expected Influence (2-step)"
  )
) +
  ggtitle("Temporal Network") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

p_contemp <- plot(
  bridge_contemp,
  include = c(
    "Bridge Strength",
    "Bridge Expected Influence (1-step)",
    "Bridge Expected Influence (2-step)"
  )
) +
  ggtitle("Contemporaneous Network") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

bridge_plot_combined <- p_temporal / p_contemp +
  plot_layout(ncol = 1)

ggsave(
  "fig_bridge_adj.png",
  bridge_plot_combined,
  width = 9,
  height = 12,
  units = "in",
  dpi = 300
)

# Centrality
temporal_plot_mat <- t(temporal_mat)

rownames(temporal_plot_mat) <- node_labels
colnames(temporal_plot_mat) <- node_labels

temporal_qgraph <- qgraph(
  temporal_mat,
  directed = TRUE,
  labels = node_labels,
  DoNotPlot = TRUE
)

contemp_qgraph <- qgraph(
  contemp_mat,
  directed = FALSE,
  labels = node_labels,
  DoNotPlot = TRUE
)

p_centrality_temporal <- centralityPlot(
  temporal_qgraph,
  include = c(
    "OutStrength",
    "InStrength",
    "OutExpectedInfluence",
    "InExpectedInfluence"
  ),
  scale = "raw"
) +
  ggtitle("Temporal Network") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

p_centrality_contemp <- centralityPlot(
  contemp_qgraph,
  include = c(
    "Strength",
    "ExpectedInfluence"
  ),
  scale = "raw"
) +
  ggtitle("Contemporaneous Network") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

centrality_plot_combined <- p_centrality_temporal / p_centrality_contemp +
  plot_layout(ncol = 1)

ggsave(
  "fig_centrality_adj.png",
  centrality_plot_combined,
  width = 9,
  height = 12,
  units = "in",
  dpi = 300
)

# Between-person 
bridge_between <- bridge(
  between_mat,
  communities = node_labels
)

between_qgraph <- qgraph(
  between_mat,
  directed = FALSE,
  labels = node_labels,
  DoNotPlot = TRUE
)

p_between <- plot(
  bridge_between,
  include = c(
    "Bridge Strength",
    "Bridge Expected Influence (1-step)",
    "Bridge Expected Influence (2-step)"
  )
) +
  ggtitle("Between-person Network") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

p_between_contemp <- centralityPlot(
  between_qgraph,
  include = c(
    "Strength",
    "ExpectedInfluence"
  ),
  scale = "raw"
) +
  ggtitle("Between-person Network") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

between_plot_combined <- p_between / p_between_contemp +
  plot_layout(ncol = 1)

ggsave(
  "fig_between_adj.png",
  between_plot_combined,
  width = 9,
  height = 12,
  units = "in",
  dpi = 300
)
