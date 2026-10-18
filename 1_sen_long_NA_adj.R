#### Longitudinal Network Analysis: Loneliness, Depressive Symptoms, and Cognition ####

# ---- 0. Setup ----
rm(list = ls())

packages <- c(
  "dplyr", "tidyverse", "Hmisc", "psych", "qgraph", "bootnet",
  "networktools", "CliquePercolation", "NetworkComparisonTest",
  "magick", "janitor", "labelled", "zoo", "tidyverse", "stringr",
  "purrr", "readr", "psychonetrics", "glmnet", "ggplot2", "haven",
  "tseries", "mgcv"
)

# install.packages(setdiff(packages, rownames(installed.packages())))

lapply(packages, library, character.only = TRUE)

set.seed(1234)

root <- "/Users/jiaminyin/Library/CloudStorage/OneDrive-UniversityofRochester/Research/ELSA_proj/Network_Analysis/"
# Set working directory
setwd("/Users/jiaminyin/Library/CloudStorage/OneDrive-UniversityofRochester/Research/ELSA_proj/Network_Analysis/output")

# ---- 1. Data Preprocessing ----
dat_long <- read.csv(file.path(root, "data", "network_xsec.csv"), header = TRUE)

dat_long <- dat_long %>%
  filter(core_mem == 1)
n_distinct(dat_long$idauniq) # 19672

dat_long <- dat_long %>%
  mutate(
    indager = ifelse(indager >90, 90, indager),
    edqual_h = case_when(
      highest_edqual %in% c(1, 2) ~ 2,    # Other / No formal qualification
      highest_edqual %in% c(3, 4, 5) ~ 1, # High school education or equivalent
      highest_edqual %in% c(6, 7) ~ 0,    # Higher education
      TRUE ~ NA_real_
    )
  ) %>% 
  filter(
    complete.cases(
      ucla_companion, ucla_leftout, ucla_isolated,
      cesd_depressed, cesd_effort, cesd_sleep,
      cesd_sad, cesd_happy, cesd_going, cesd_enjoy,
      immwdrc, dldwdrc, verbflcy
    )
  ) 

n_distinct(dat_long$idauniq) # 15848

dat_long <- dat_long %>%
  mutate(
    indager = ifelse(indager >90, 90, indager)
  ) %>% 
  filter(indager >= 50) %>%   # age range was chosen based on sample sizes 
  group_by(idauniq) %>%
  filter(n() >= 3) %>%        # Respondents need to have at least 3 non-missing assessments 
  ungroup()
n_distinct(dat_long$idauniq) # 9075

## Standardize variables 
network_dat <- dat_long %>%
  transmute(
    idauniq = idauniq,
    wave = wave,
    age = indager,
    gender = gender,
    partner = partner,
    edqual_h = edqual_h,
    totinc_bu_s = totinc_bu_s,
    hegenh = hegenh,
    limitill = limitill,
    
    lon1 = ucla_companion,
    lon2 = ucla_leftout,
    lon3 = ucla_isolated,
    
    dep1 = cesd_depressed,
    dep2 = cesd_effort,
    dep3 = cesd_sleep,
    dep4 = cesd_happy,
    dep5 = cesd_sad,
    dep6 = cesd_going,
    dep7 = cesd_enjoy,
    
    cog1 = immwdrc,
    cog2 = dldwdrc,
    cog3 = verbflcy
  ) %>%
  arrange(idauniq, age, wave)

first_nonmissing <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA)
  } else {
    return(x[1])
  }
}

baseline_covariates <- network_dat %>%
  arrange(idauniq, wave) %>%
  group_by(wave) %>%
  mutate(
    temp_totinc_q = ntile(totinc_bu_s, 3),
    totinc_q = case_when(
      temp_totinc_q == 1 ~ 2, # Low
      temp_totinc_q == 2 ~ 1, # Med
      temp_totinc_q == 3 ~ 0, # High
      TRUE ~ NA_real_)
  ) %>% 
  ungroup() %>%
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
  filter(complete.cases(across(-idauniq))) 
  

nodes <- c(
  "lon1", "lon2", "lon3",
  "dep1", "dep2", "dep3", "dep4", "dep5", "dep6", "dep7",
  "cog1", "cog2", "cog3"
)

k <- length(nodes)

network_dat <- network_dat %>%
  right_join(baseline_covariates, by = "idauniq") %>%
  mutate(
    age_bl      = as.numeric(age_bl),
    gender_bl   = as.factor(gender_bl),
    partner_bl  = as.factor(partner_bl),
    edqual_bl   = as.factor(edqual_bl),
    totinc_bl   = as.factor(totinc_bl),
    hegenh_bl   = as.factor(hegenh_bl),
    limitill_bl = as.factor(limitill_bl)
  )
n_distinct(network_dat$idauniq) # 8087

# Names for residualized/adjusted nodes
nodes_adj <- paste0(nodes, "_adj")

# Regress each node on baseline covariates
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
    data = network_dat,
    na.action = na.exclude
  )
  
  # Standardize residuals to mean = 0, SD = 1
  network_dat[[node_adj_i]] <-
    as.numeric(scale(residuals(model_i)))
}

# ---- 3. Estimate full panel GVAR ----
## Reshape data to wide format
network_wide <- network_dat %>%
  pivot_wider(
    id_cols = idauniq,
    names_from = wave,
    values_from = all_of(nodes_adj),
    names_glue = "{.value}_w{wave}"
  )

# get wave values
waves <- sort(unique(network_dat$wave))

# create design matrix: rows = nodes, columns = waves
vars_mat <- outer(nodes_adj, waves, paste0)
vars_mat <- matrix(
  paste0(rep(nodes_adj, each = length(waves)), "_w", rep(waves, times = length(nodes_adj))),
  nrow = length(nodes_adj),
  ncol = length(waves),
  byrow = TRUE
)

rownames(vars_mat) <- nodes_adj
colnames(vars_mat) <- paste0("wave", waves)

vars_mat

## The PanelGVAR model
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

## First remove clearly nonsignificant edges
fit_pruned <- prune(
  fit_full,
  alpha = 0.05,
  recursive = TRUE
)

fit_pruned <- runmodel(fit_pruned)
# Common fit indices
fit_indices <- fit(fit_pruned)
fit_indices %>%
  filter(Measure %in% c(
    "chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"
  )) %>%
  pivot_wider(names_from = Measure, values_from = Value)

## Save model results
write.csv(
  fit_indices,
  "panelGVAR_fit_pruned_indices_adj.csv",
  row.names = FALSE
)
saveRDS(fit_pruned, "fit_pruned_adj.rds") # Simple pruned

## Then use BIC search starting from the simpler model
fit_search <- modelsearch(
  fit_pruned,
  criterion = "BIC",
  prunealpha = 0.05,
  addalpha = 0.05
)

fit_search <- runmodel(fit_search)

# Common fit indices
fit_indices <- fit(fit_search)
fit_indices %>%
  filter(Measure %in% c(
    "chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"
  )) %>%
  pivot_wider(names_from = Measure, values_from = Value)

## Save model results
write.csv(
  fit_indices,
  "panelGVAR_fit_indices_adj.csv",
  row.names = FALSE
)

saveRDS(fit_search, "fit_search_adj.rds") # Model searched based on BIC

