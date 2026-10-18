# Longitudinal Network Analysis of Loneliness, Depressive Symptoms, and Cognition in ELSA

## Overview

This project examines the longitudinal relationships among loneliness, depressive symptoms, and cognitive function in older adults using data from the English Longitudinal Study of Ageing (ELSA).

A Panel Graphical Vector Autoregressive (Panel GVAR) model is used to distinguish three types of associations:

- **Temporal associations:** directed lagged associations from a node at wave *t* to a node at wave *t + 1*.
- **Contemporaneous associations:** conditional associations among nodes within the same measurement occasion after accounting for temporal effects.
- **Between-person associations:** conditional associations among stable between-person differences across the observation period.

The primary analyses focus on the temporal and contemporaneous networks and on cross-domain connections among loneliness, depressive symptoms, and cognition.

---

## Data

Data are drawn from the English Longitudinal Study of Ageing (ELSA), a nationally representative longitudinal study of adults aged 50 years and older living in England.

The analysis uses repeated observations from Waves 2–5 and Waves 7–11.

### Analytic sample

Participants were eligible for the longitudinal network analysis if they had complete information on all network indicators at a minimum of three study waves.

The three-wave requirement determines eligibility for the analytic sample. Full-information maximum likelihood (FIML) is subsequently used in the Panel GVAR model so that available observations can contribute to model estimation when participants do not contribute data at every study wave.

---

## Network nodes

The network contains 13 nodes representing three domains.

### Loneliness

| Node | Variable | Description |
|---|---|---|
| LON1 | `ucla_companion` | Lack of companionship |
| LON2 | `ucla_leftout` | Feeling left out |
| LON3 | `ucla_isolated` | Feeling isolated |

### Depressive symptoms

| Node | Variable | Description |
|---|---|---|
| DEP1 | `cesd_depressed` | Depressed mood |
| DEP2 | `cesd_effort` | Greater effort |
| DEP3 | `cesd_sleep` | Restless sleep |
| DEP4 | `cesd_happy` | Reduced happiness |
| DEP5 | `cesd_sad` | Sadness |
| DEP6 | `cesd_going` | Difficulty getting going |
| DEP7 | `cesd_enjoy` | Reduced enjoyment of life |

The CES-D loneliness item is excluded to reduce conceptual overlap with the loneliness measures. Positive-affect items are reverse-coded so that higher values consistently represent greater depressive symptom severity.

### Cognitive function

| Node | Variable | Description |
|---|---|---|
| COG1 | `immwdrc` | Immediate word recall |
| COG2 | `dldwdrc` | Delayed word recall |
| COG3 | `verbflcy` | Verbal fluency |

All network indicators are standardized before model estimation to place variables measured on different numerical scales on a common metric.

---

## Analysis workflow

### 1. Data preparation

The longitudinal data are converted to wide format, with one row per respondent and separate columns for each node at each study wave.

The variable matrix used by `psychonetrics::panelgvar()` has:

- rows corresponding to the 13 network nodes;
- columns corresponding to measurement waves.

Example:

```r
vars_mat <- outer(
  nodes,
  waves,
  FUN = function(node, wave) {
    paste0(node, "_w", wave)
  }
)
