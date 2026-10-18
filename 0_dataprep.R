# ---- 0. Packages ----
library(tidyverse)
library(stringr)
library(purrr)
library(haven)     # if .dta
library(readr)     # if .csv/.tsv
library(janitor)
library(labelled)
library(zoo)

rm(list = ls())
setwd("/Users/jiaminyin/Library/CloudStorage/OneDrive-UniversityofRochester/Research/ELSA_proj/Network_Analysis/data")
list.files()

# Read in data 
core <- read.csv("/Users/jiaminyin/Library/CloudStorage/OneDrive-UniversityofRochester/Research/ELSA_proj/data/core_all.csv", header = TRUE)
core_cond <- read.csv("/Users/jiaminyin/Library/CloudStorage/OneDrive-UniversityofRochester/Research/ELSA_proj/data/core_cond_all.csv", header = TRUE)

core_all <- core %>%
  left_join(core_cond, by = "idauniq")

# ---- 1. Basic cleaning & recoding ----
# Categorize total income 
core_all <- core_all %>%
  group_by(year) %>%
  mutate(
    temp_totinc_q = ntile(totinc_bu_s, 3),
    totinc_q = case_when(
      temp_totinc_q == 1 ~ 2,
      temp_totinc_q == 2 ~ 1,
      temp_totinc_q == 3 ~ 0,
      TRUE ~ NA_real_)
  ) %>%
  ungroup() 

core_all <- core_all %>%
  # sort by person and wave so lag/fill behave as expected
  arrange(idauniq, wave) %>%
  group_by(idauniq) %>%
  
  mutate(adrd_trt = ifelse(is.na(wave_adrd), 0, 1),
         cond_trt = as.integer(is.na(wave_adrd) &  
                                 if_any(c(wave_lung, wave_arthritis, wave_oa_ra, wave_parkinson), ~ !is.na(.x))),
         wave_cond = pmin(wave_lung, wave_arthritis, wave_oa_ra, wave_parkinson, na.rm = TRUE),
         wave_cond = ifelse(is.infinite(wave_cond), NA_integer_, as.integer(wave_cond)),
  ) %>%
  
  # fill highest completed education year (fqend) down within person
  mutate(fqend = ifelse(
    is.na(fqend),
    zoo::na.locf(fqend, na.rm = FALSE),   # fill forward
    fqend
  )) %>%
  
  # fill highest completed education level (edqual) down within person
  mutate(edqual = ifelse(
    is.na(edqual),
    zoo::na.locf(edqual, na.rm = FALSE),   # fill forward
    edqual
  )) %>%
  
  # fill sex upward (so sex is filled wherever there is any info)
  mutate(indsex = ifelse(idauniq ==150730, 2, 
                         ifelse(idauniq ==150856, 1, indsex))) %>%
  
  # fill ethnicity both forward and backward within person
  mutate(fqethnr = ifelse(
    is.na(fqethnr),
    zoo::na.locf(fqethnr, na.rm = FALSE),   # fill forward
    fqethnr
  )) %>%
  mutate(fqethnr = ifelse(
    is.na(fqethnr),
    zoo::na.locf(fqethnr, fromLast = TRUE, na.rm = FALSE),  # fill backward
    fqethnr
  )) %>%
  
  # then fill fqend upward (so fqend is filled wherever there is any info)
  mutate(fqend = ifelse(
    is.na(fqend),
    zoo::na.locf(fqend, fromLast = TRUE, na.rm = FALSE),  # fill backward
    fqend
  )) %>%
  mutate(fqend = ifelse(
    is.na(fqend),
    zoo::na.locf(fqend, fromLast = TRUE, na.rm = FALSE),  # fill backward
    fqend
  )) %>%
  
  # then fill income quartile upward (so fqend is filled wherever there is any info)
  mutate(totinc_q = ifelse(
    is.na(totinc_q),
    zoo::na.locf(totinc_q, na.rm = FALSE),  # fill forward
    totinc_q
  )) %>%
  
  mutate(
    # fix inconsistent sex: if household says male (dhsex==1) 
    # but individual record says female (indsex==2), set individual to male
    indsex  = ifelse(!is.na(indsex) & !is.na(dhsex) & indsex == 2 & dhsex == 1, 1, indsex),
    
    # impute age: carry last non-missing age forward and add 2 years per wave gap
    # (assumes 2-year intervals between waves)
    indager = zoo::na.locf(indager, na.rm = FALSE) + 
      2 * (wave - lag(wave, default = first(wave))),
    
    # Recode CES-D items: original coding 1 = symptom present, 2 = symptom absent
    # → recode 1 → 1, 2 → 0, NA stays NA
    psceda = ifelse(is.na(psceda), NA, ifelse(psceda == 2, 0, psceda)),
    pscedb = ifelse(is.na(pscedb), NA, ifelse(pscedb == 2, 0, pscedb)),
    pscedc = ifelse(is.na(pscedc), NA, ifelse(pscedc == 2, 0, pscedc)),         
    pscedd = ifelse(pscedd == 1, 0, ifelse(pscedd == 2, 1, NA)),
    pscede = ifelse(is.na(pscede), NA, ifelse(pscede == 2, 0, pscede)),
    pscedf = ifelse(pscedf == 1, 0, ifelse(pscedf == 2, 1, NA)),
    pscedg = ifelse(is.na(pscedg), NA, ifelse(pscedg == 2, 0, pscedg)),
    pscedh = ifelse(is.na(pscedh), NA, ifelse(pscedh == 2, 0, pscedh)),
    
    # Recode marital quality items: original scale 1 - 4
    # → recode 1 → 0, 2 → 1, 3 → 2, 4 → 3, NA stays NA
    # reverse code negative items
    scptra = scptra - 1,
    scptrb = scptrb - 1,
    scptrc = scptrc - 1,
    scptrd = scptrd - 1,
    scptre = scptre - 1,
    scptrf = scptrf - 1,
    scptrg = scptrg - 1,
    
    # Recode cognitive orientation items: 1 = correct, 2 = incorrect → 1/0
    cfdatd = ifelse(is.na(cfdatd), NA, ifelse(cfdatd == 2, 0, cfdatd)),
    cfdatm = ifelse(is.na(cfdatm), NA, ifelse(cfdatm == 2, 0, cfdatm)),
    cfdaty = ifelse(is.na(cfdaty), NA, ifelse(cfdaty == 2, 0, cfdaty)),
    cfday  = ifelse(is.na(cfday),  NA, ifelse(cfday  == 2, 0, cfday))
  ) %>%  
  ungroup() 

core_all <- core_all %>%
  group_by(idauniq) %>%
  mutate(
    # education levels
    highest_edqual = {
      x <- as.numeric(edqual)
      
      if (all(is.na(x))) {
        NA_real_
      } else {
        min(x, na.rm = TRUE)
      }
    }
  ) %>%
  ungroup() %>%
  mutate(
    # rename age
    age = indager,
    
    # gender: 1 = male, 0 = female (assuming indsex: 1=male,2=female)
    gender = ifelse(indsex == 2, 0, indsex),
    
    # education years proxy from fqend (completion age)
    eduyrs = fqend,
    
    # race: 0 = White (fqethnr==1), 1 = non-White (fqethnr==2), else NA
    race = ifelse(fqethnr == 1, 0, 
                  ifelse(fqethnr == 2, 1, NA)),
    
    # partnered indicator: 0 = has partner in HH, 1 = single
    # couple==3 used as "Neither"; 1/2 = Married / Cohabit
    partner = ifelse(couple == 3, 1, 
                     ifelse(couple == 1 | couple == 2, 0, NA)),
    
    # employment status: 0 = employed, 1 = not employed, NA otherwise
    # (assuming wpdes==1 = in paid work)
    employstatus = ifelse(wpdes == 1, 0, 
                          ifelse(wpdes > 1, 1, NA)),
    
    # self-rated health: 0 = good/very good/excellent (1–3), 1 = fair/poor (4–5)
    srh = ifelse(is.na(hegenh), NA,
                 ifelse(hegenh <= 3, 0, 1)),
    
    # limiting longstanding illness:
    # heill==2 → no limiting illness (0)
    # heill==1 & helim==1 → limiting illness (1)
    # heill==1 & helim!=1 → no limiting illness (0)
    limitill = ifelse(is.na(heill), NA, 
                      ifelse(heill == 2, 0,
                             ifelse(helim == 1, 1, 0))),
    
    # ADL score: sum of ADL limitation indicators (0/1) across ADL items
    adl_scr = rowSums(across(c(headldr, headlwa, headlba, headlea, headlbe, headlwc)),
                      na.rm = TRUE),
    
    # Social contact score
    cntct_scr = rowSums(across(c(scchdg, scchdh, scchdj, scfamg, scfamh, scfamj,
                                 scfrdg, scfrdh, scfrdj)), na.rm = TRUE),
    
    # friend network: 0 = has friends, 1 = no close friends
    friend = ifelse(scfrd == 1, 0, ifelse(scfrd == 2, 1, NA)),
    
    # friend contact frequency: 0 = at least monthly, 1 = less than monthly
    frd_meet = ifelse(is.na(scfrdg), NA, 
                      ifelse(scfrdg <= 3, 0, 1)), # 0: ≥once/month; 1: <once/month
    
    frd_talk = ifelse(is.na(scfrdh), NA, 
                      ifelse(scfrdh <= 3, 0, 1)), # 0: ≥once/month; 1: <once/month
    
    frd_write = ifelse(is.na(scfrdj), NA, 
                       ifelse(scfrdj <= 3, 0, 1)), # 0: ≥once/month; 1: <once/month
    
    # Loneliness score: sum of UCLA-3 items (higher = more lonely)
    loneliness = rowSums(across(c(scfeela, scfeelb, scfeelc)), na.rm = FALSE),
    
    # Marital quality 
    marqual = rowSums(across(c(scptra, scptrb, scptrc, scptrd, scptre, scptrf,
                               scptrg)), na.rm = FALSE),
    
    # Depression symptom count: sum of recoded CES-D items (0/1, higher = more symptoms)
    dep_ced = rowSums(across(c(psceda, pscedb, pscedc, pscedd, pscede, 
                               pscedf, pscedg, pscedh)), na.rm = FALSE),
    dep_ced7 = rowSums(across(c(psceda, pscedb, pscedc, pscedd, 
                                pscedf, pscedg, pscedh)), na.rm = FALSE),
    
    # Memory score: sum of immediate + delayed recall (or other memory items)
    memory = rowSums(across(c(cflisen, cflisd)), na.rm = FALSE),
    immwdrc = cflisen,
    dldwdrc = cflisd,
    
    # Verbal fluency: animal naming score
    verbflcy = cfani,
    
    # Orientation score: sum of correct date/orientation items
    orient = rowSums(across(c(cfdatd, cfdatm, cfdaty, cfday)), na.rm = FALSE)
  )

core_all <- core_all %>%
  rename(ucla_companion = scfeela, ucla_leftout = scfeelb, ucla_isolated = scfeelc, 
         cesd_depressed = psceda, cesd_effort = pscedb, cesd_sleep = pscedc,
         cesd_happy = pscedd, cesd_lonely = pscede, cesd_enjoy = pscedf, 
         cesd_sad = pscedg, cesd_going = pscedh) 

# Save
write_csv(core_all, "/Users/jiaminyin/Library/CloudStorage/OneDrive-UniversityofRochester/Research/ELSA_proj/Network_Analysis/data/network_xsec.csv")
