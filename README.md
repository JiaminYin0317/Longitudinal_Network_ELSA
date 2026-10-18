Longitudinal Network Analysis of Loneliness, Depressive Symptoms, and Cognition in ELSA
Overview
This project examines longitudinal relationships among loneliness, depressive symptoms, and cognitive function using data from the English Longitudinal Study of Ageing (ELSA).
A Panel Graphical Vector Autoregressive (Panel GVAR) model is used to distinguish three components:
- Temporal associations: directed lagged associations from a node at wave t to a node at wave t + 1.
- Contemporaneous associations: conditional associations among nodes within the same measurement occasion after accounting for temporal effects.
- Between-person associations: conditional associations among stable between-person differences across the observation period.
The primary analyses focus on the temporal and contemporaneous networks and on cross-domain connections among loneliness, depressive symptoms, and cognition.
Data
Data are drawn from the English Longitudinal Study of Ageing (ELSA), an ongoing nationally representative longitudinal study of adults aged 50 years and older living in England.
The analysis uses repeated observations across available study waves. Respondents were eligible for the longitudinal network analysis if they had complete information on all network indicators in at least three waves.
The three-wave requirement determines eligibility for the analytic sample. Full-information maximum likelihood (FIML) is subsequently used in the Panel GVAR so that available observations can contribute to model estimation even when respondents do not contribute data at every study wave.
Network Nodes
The network contains 13 nodes representing three domains.
Loneliness
Node	Variable	Description
LON1	ucla_companion	Lack of companionship
LON2	ucla_leftout	Feeling left out
LON3	ucla_isolated	Feeling isolated


Depressive Symptoms
Node	Variable	Description
DEP1	cesd_depressed	Depressed mood
DEP2	cesd_effort	Greater effort
DEP3	cesd_sleep	Restless sleep
DEP4	cesd_happy	Reduced happiness
DEP5	cesd_sad	Sadness
DEP6	cesd_going	Difficulty getting going
DEP7	cesd_enjoy	Reduced enjoyment of life


The CES-D loneliness item is excluded to reduce conceptual overlap with the loneliness measures. Positive-affect CES-D items are reverse-coded so that higher values consistently indicate greater depressive symptom severity.
Cognitive Function
Node	Variable	Description
COG1	immwdrc	Immediate word recall
COG2	dldwdrc	Delayed word recall
COG3	verbflcy	Verbal fluency


All network indicators are standardized before model estimation to place variables measured on different numerical scales on a common metric.
Key Node Definitions
node_labels <- c(
  "LON1", "LON2", "LON3",
  "DEP1", "DEP2", "DEP3", "DEP4", "DEP5", "DEP6", "DEP7",
  "COG1", "COG2", "COG3"
)

nodes <- c(
  "lon1", "lon2", "lon3",
  "dep1", "dep2", "dep3", "dep4", "dep5", "dep6", "dep7",
  "cog1", "cog2", "cog3"
)

node_construct <- c(
  rep("Loneliness", 3),
  rep("Depressive symptoms", 7),
  rep("Cognition", 3)
)

names(node_construct) <- node_labels
Data Preparation
The longitudinal data are reshaped to wide format, with one row per respondent and separate columns for each node at each study wave.
network_wide <- network_dat %>%
  pivot_wider(
    id_cols = idauniq,
    names_from = wave,
    values_from = all_of(nodes),
    names_glue = "{.value}_w{wave}"
  )
The variable matrix used by psychonetrics::panelgvar() has rows corresponding to the 13 network nodes and columns corresponding to measurement waves.
waves <- sort(unique(network_dat$wave))

vars_mat <- outer(
  nodes,
  waves,
  FUN = function(node, wave) {
    paste0(node, "_w", wave)
  }
)

rownames(vars_mat) <- nodes
colnames(vars_mat) <- paste0("wave", waves)
Primary Panel GVAR Model
The primary model is estimated using the psychonetrics package.
fit_full <- panelgvar(
  data = network_wide,
  vars = vars_mat,
  estimator = "FIML",
  missing = "fiml",
  beta = "full",
  within_latent = "ggm",
  between_latent = "ggm"
)

fit_full <- runmodel(fit_full)
Temporal Component
beta = "full" estimates directed lag-1 associations among all nodes.
For the temporal coefficient matrix:
- Columns = predictor/from-node at wave t
- Rows = outcome/to-node at wave t + 1
Thus:
beta[row, column]
represents:
column at wave t -> row at wave t + 1
This orientation is important when creating temporal edge tables.
Contemporaneous Component
within_latent = "ggm"
specifies the contemporaneous structure as a Gaussian graphical model (GGM). Edges represent conditional associations among within-person residuals at the same measurement occasion after accounting for temporal effects and the other nodes in the contemporaneous network.
Between-Person Component
between_latent = "ggm"
specifies the between-person structure as a GGM. Edges represent conditional associations among stable between-person differences after accounting for the other nodes in that network component.
Why Use a GGM?
GGMs are used because the substantive goal is to characterize conditional network structure rather than marginal correlations alone. A GGM edge reflects the association between two nodes after conditioning on the remaining nodes in the respective network.
The standardized ordinal, binary, and continuous indicators are treated as continuous within the Gaussian modeling framework. This represents an approximation for ordinal and binary indicators and is examined in sensitivity analyses using alternative dependence-structure parameterizations.
Missing Data and FIML
Full-information maximum likelihood (FIML) is used to handle incomplete observations across waves.
estimator = "FIML"
missing = "fiml"
FIML uses all available observed data rather than requiring respondents to have complete data at every measurement occasion. This reduces sample loss compared with listwise deletion and provides valid maximum-likelihood estimates under the assumption that data are missing at random conditional on variables included in the model.
The requirement of at least three complete waves determines eligibility for the analytic sample, whereas FIML handles incomplete measurement occasions among eligible respondents.
Model Pruning
After estimating the full model, statistically unsupported edges are recursively removed.
fit_pruned <- prune(
  fit_full,
  alpha = 0.05,
  recursive = TRUE
)

fit_pruned <- runmodel(fit_pruned)

saveRDS(fit_pruned, "fit_pruned.rds")
Figures and parameter tables are based on the final pruned model.
The full model is treated as an estimation step, whereas substantive interpretation focuses on parameters retained in the final model.
Model Fit
Model fit is evaluated using:
- Chi-square
- Comparative Fit Index (CFI)
- Tucker-Lewis Index (TLI)
- Root Mean Square Error of Approximation (RMSEA)
- Standardized Root Mean Square Residual (SRMR)
The final primary model demonstrated good overall fit, with approximately:
- CFI = 0.94
- TLI = 0.94
- RMSEA = 0.012
- SRMR = 0.038
The chi-square test was statistically significant, which is not unexpected given the large sample size and model complexity.
Extracting Network Matrices
temporal_mat <- getmatrix(final_pruned, "beta")
contemp_mat  <- getmatrix(final_pruned, "omega_zeta_within")
between_mat  <- getmatrix(final_pruned, "omega_zeta_between")

rownames(temporal_mat) <- node_labels
colnames(temporal_mat) <- node_labels

rownames(contemp_mat) <- node_labels
colnames(contemp_mat) <- node_labels

rownames(between_mat) <- node_labels
colnames(between_mat) <- node_labels
Temporal Network Orientation
In the psychonetrics temporal beta matrix:
column = From node at wave t
row    = To node at wave t + 1
Therefore, temporal tables should use:
From = node_labels[col]
To   = node_labels[row]
For plotting with qgraph, transpose the temporal matrix so that arrow direction is displayed correctly:
temporal_plot_mat <- t(temporal_mat)
This keeps the graphical direction consistent with:
From node at wave t -> To node at wave t + 1
Cross-Domain Networks
For selected figures and tables, within-domain edges are set to zero so that only connections across loneliness, depressive symptoms, and cognition are shown.
temporal_cross_mat <- temporal_mat

for (i in seq_len(nrow(temporal_cross_mat))) {
  for (j in seq_len(ncol(temporal_cross_mat))) {
    if (node_construct[i] == node_construct[j]) {
      temporal_cross_mat[i, j] <- 0
    }
  }
}
Cross-domain temporal edges include:
- loneliness -> depressive symptoms
- depressive symptoms -> loneliness
- loneliness -> cognition
- cognition -> loneliness
- depressive symptoms -> cognition
- cognition -> depressive symptoms
Autoregressive and within-domain edges remain part of the estimated model but may be omitted from figures for clarity.
Network Visualization
Network figures use consistent node labels, colors, and layouts across analyses.
Node Colors
- Loneliness: orange
- Depressive symptoms: blue
- Cognition: green
node_colors <- c(
  rep("#E69F00", 3),
  rep("#56B4E9", 7),
  rep("#009E73", 3)
)
Edge Colors
- Positive associations: gray
- Negative associations: purple
For temporal networks, arrowheads indicate the direction of the lagged association.
qgraph(
  t(temporal_cross_mat),
  layout = common_layout,
  labels = node_labels,
  groups = node_groups,
  color = node_colors,
  legend = FALSE,
  directed = TRUE,
  edge.labels = FALSE,
  posCol = "gray50",
  negCol = "purple"
)
Edge thickness represents the absolute magnitude of the estimated association.
Temporal Parameter Tables
Temporal parameters are presented as directed edges:
From node at wave t -> To node at wave t + 1
For the full temporal parameter table:
params <- parameters(final_pruned)

temporal_edge_table <- params %>%
  filter(matrix == "beta") %>%
  mutate(
    From = node_labels[col],
    To = node_labels[row],
    From_construct = node_construct[From],
    To_construct = node_construct[To],
    Direction = paste0(From, " -> ", To),
    Construct_pathway = paste0(
      From_construct,
      " -> ",
      To_construct
    ),
    Edge_type = case_when(
      From == To ~ "Autoregressive",
      From_construct == To_construct ~ "Within-domain",
      TRUE ~ "Cross-domain"
    ),
    CI_lower = est - 1.96 * se,
    CI_upper = est + 1.96 * se,
    abs_est = abs(est)
  ) %>%
  filter(
    !is.na(est),
    est != 0
  ) %>%
  arrange(desc(abs_est))
The main manuscript focuses on the strongest retained cross-domain temporal edges. Full retained temporal parameters are reported in supplementary materials.
Bridge Centrality
Bridge centrality is calculated using loneliness, depressive symptoms, and cognition as communities.
The primary bridge measures are:
- Bridge Expected Influence (1-step): signed sum of direct cross-community connections.
- Bridge Strength: sum of the absolute magnitude of direct cross-community connections.
Bridge Expected Influence (2-step) may be examined as a supplementary measure.
For example:
bridge_temporal <- bridge(
  temporal_mat,
  communities = node_construct
)
Bridge metrics are estimated separately for the temporal and contemporaneous networks.
Covariate-Adjusted Sensitivity Analysis
A sensitivity analysis adjusts the network indicators for baseline demographic, socioeconomic, and health characteristics.
Baseline covariates may include:
- age
- gender
- partner status
- educational qualification
- household income
- general health
- long-term limiting illness
Baseline covariates are defined using each respondent's first observed wave in the analytic sample.
baseline_covariates <- network_dat %>%
  arrange(idauniq, wave) %>%
  group_by(idauniq) %>%
  summarise(
    age_bl = first(age),
    gender_bl = first(gender),
    partner_bl = first(partner),
    edqual_bl = first(edqual_h),
    totinc_bl = first(totinc_q),
    hegenh_bl = first(hegenh),
    limitill_bl = first(limitill),
    .groups = "drop"
  ) %>%
  filter(
    if_all(
      -idauniq,
      ~ !is.na(.x)
    )
  )
Each network indicator is regressed on the baseline covariates. The residuals are then standardized and used as adjusted network indicators.
network_dat_adj <- network_dat %>%
  left_join(
    baseline_covariates,
    by = "idauniq"
  )

nodes_adj <- paste0(nodes, "_adj")

for (i in seq_along(nodes)) {

  node_i <- nodes[i]
  node_adj_i <- nodes_adj[i]

  model_i <- lm(
    as.formula(
      paste0(
        node_i,
        " ~ age_bl + gender_bl + partner_bl + ",
        "edqual_bl + totinc_bl + hegenh_bl + limitill_bl"
      )
    ),
    data = network_dat_adj,
    na.action = na.exclude
  )

  network_dat_adj[[node_adj_i]] <-
    as.numeric(scale(residuals(model_i)))
}
The adjusted Panel GVAR is estimated using the same model specification and pruning procedure as the primary analysis.
Alternative Dependence-Structure Specifications
The primary model specifies the contemporaneous and between-person structures as GGMs:
within_latent = "ggm"
between_latent = "ggm"
Sensitivity analyses assess whether findings depend on this parameterization.
Covariance Specification
fit_cov <- panelgvar(
  data = network_wide,
  vars = vars_mat,
  estimator = "FIML",
  missing = "fiml",
  beta = "full",
  within_latent = "cov",
  between_latent = "cov"
)
The covariance specification models the contemporaneous and between-person dependence structures directly as covariance matrices rather than conditional-association networks.
Precision-Matrix Specification
fit_prec <- panelgvar(
  data = network_wide,
  vars = vars_mat,
  estimator = "FIML",
  missing = "fiml",
  beta = "full",
  within_latent = "prec",
  between_latent = "prec"
)
The precision specification directly parameterizes the inverse covariance matrix.
The covariance specification produced fit comparable to the primary GGM model, whereas the precision-matrix specification showed poorer fit across several indices.
These sensitivity analyses evaluate robustness to the parameterization of the contemporaneous and between-person dependence structures. They do not eliminate the approximation involved in treating ordinal and binary indicators as continuous within a Gaussian modeling framework.
Stationarity
The Panel GVAR assumes a stationary dynamic structure, such that temporal and contemporaneous network parameters are constrained to remain constant across measurement occasions.
A fully unconstrained 13-node multiwave RI-CLPM was explored as a potential formal assessment of stationarity. However, the model was computationally unstable because of the large number of wave-specific parameters and did not produce an admissible solution.
Stationarity is therefore treated as a modeling assumption. Temporal and contemporaneous parameters from the Panel GVAR should be interpreted as average associations across the observation period.
A supplementary sensitivity analysis can estimate the same Panel GVAR separately over earlier and later study periods and compare the major network patterns.
The early-versus-late comparison should focus on:
- direction of temporal associations
- magnitude of temporal associations
- contemporaneous network structure
- cross-domain associations
- bridge-node patterns
This provides a robustness assessment rather than a formal test of stationarity.
Early- Versus Late-Period Sensitivity Analysis
The same Panel GVAR can be estimated separately over earlier and later portions of follow-up using identical model specifications.
Example:
early_waves <- c(2, 3, 4, 5)
late_waves  <- c(7, 8, 9, 10, 11)

network_early <- network_dat %>%
  filter(wave %in% early_waves)

network_late <- network_dat %>%
  filter(wave %in% late_waves)
Corresponding temporal edge estimates can then be compared across periods.
The primary goal is not to compare model-fit indices between the two periods, but to assess whether corresponding network parameters are similar in direction and magnitude.
Main Outputs
Tables
1. Characteristics of the analytic sample by ELSA wave
2. Panel GVAR model-fit statistics
3. Retained cross-domain temporal edge parameters
4. Full temporal parameter table
5. Contemporaneous edge parameters
6. Between-person edge parameters
7. Sensitivity-analysis model-fit statistics
Figures
1. Temporal network
2. Contemporaneous network
3. Bridge centrality
4. Covariate-adjusted temporal network
5. Alternative-specification sensitivity analyses
6. Early- and late-period network comparisons, where applicable
Interpretation
Temporal edges represent directed longitudinal conditional associations.
For example:
LON2 -> DEP1
indicates that feeling left out at wave t is associated with depressed mood at wave t + 1, conditional on the other temporal predictors and the modeled contemporaneous and between-person structures.
Temporal associations should not be interpreted as causal effects.
Contemporaneous edges represent conditional same-wave associations after accounting for temporal effects.
Between-person edges describe stable differences between individuals rather than within-person dynamics.
Important Methodological Considerations
1. The Panel GVAR assumes stationarity of temporal and contemporaneous network parameters across waves.
2. The observational design does not permit causal inference.
3. Residual and time-varying confounding may remain.
4. Several loneliness and depressive symptom indicators are ordinal or binary but are treated as continuous within the Gaussian modeling framework.
5. Standardization places variables on a common numerical scale but does not make ordinal or binary indicators normally distributed.
6. FIML relies on a missing-at-random assumption conditional on variables included in the model.
7. Network edges represent conditional statistical associations rather than direct causal effects.
8. Edge selection through pruning introduces uncertainty, so interpretation should emphasize robust overall patterns rather than isolated small edges.
9. Stationarity could not be formally assessed using a fully unconstrained 13-node model because of computational instability.
Software
Analyses are conducted in R using packages including:
psychonetrics
qgraph
networktools
dplyr
tidyr
ggplot2
patchwork
The longitudinal network models are estimated using:
psychonetrics::panelgvar()
Reproducibility
Set a random seed before procedures involving stochastic operations:
set.seed(123)
Save computationally intensive model objects after estimation:
saveRDS(fit_pruned, "fit_pruned.rds")
Reload saved models using:
fit_pruned <- readRDS("fit_pruned.rds")
This ensures that downstream figures and tables are generated from the same estimated model and avoids unnecessary model re-estimation.
Key Analysis Objects
Object	Description
network_dat	Long-format analytic network dataset
network_wide	Wide-format dataset used for Panel GVAR
nodes	Names of the 13 network variables
node_labels	Display labels LON1-COG3
node_construct	Domain assignment for each node
vars_mat	Node x wave variable matrix
fit_full	Initial full Panel GVAR
fit_pruned / final_pruned	Final pruned Panel GVAR
temporal_mat	Temporal beta matrix
contemp_mat	Contemporaneous GGM matrix
between_mat	Between-person GGM matrix
common_layout	Shared coordinates used for network figures


Citation
Analyses use data from the English Longitudinal Study of Ageing (ELSA). The appropriate ELSA dataset citation and funding acknowledgement should be included according to the requirements of the specific ELSA data release used in the project.
