# ---- 0. Packages ----
library(tidyverse)
library(stringr)
library(purrr)
library(haven)     # if .dta
library(readr)     # if .csv/.tsv
library(janitor)
library(labelled)

rm(list = ls())
# setwd("path/to/raw/data")
list.files()

# Read in data 
load_core_wave <- function(wave_num, path_prefix = "wave_", path_suffix = "_core_data.tab") {
  # Construct file name
  file_name <- paste0(path_prefix, wave_num, path_suffix)
  
  # Skip if file does not exist
  if (!file.exists(file_name)) {
    message("File not found: ", file_name, " — skipping.")
    return(NULL)
  }
  
  message("Reading ", file_name, " ...")
  
  # Read the tab file
  dat <- read.table(file_name, header = TRUE, sep = "\t", quote = "", fill = TRUE,
                    stringsAsFactors = FALSE)
  
  # Convert all column names to lowercase
  dat <- dat %>% 
    rename_with(tolower) %>% 
    mutate(wave=wave_num,
           year=2000+wave_num*2)
  
  # Dynamically create object name, e.g., core_w2
  obj_name <- paste0("core_w", wave_num)
  assign(obj_name, dat, envir = .GlobalEnv)
  
  return(dat)
}

for (w in 1:11) {
  load_core_wave(w)
}

load_finance <- function(wave_num, path_prefix = "wave_", path_suffix = "_financial_derived_variables.tab") {
  # Construct file name
  file_name <- paste0(path_prefix, wave_num, path_suffix)
  
  # Skip if file does not exist
  if (!file.exists(file_name)) {
    message("File not found: ", file_name, " — skipping.")
    return(NULL)
  }
  
  message("Reading ", file_name, " ...")
  
  # Read the tab file
  dat <- read.table(file_name, header = TRUE, sep = "\t", quote = "", fill = TRUE,
                    stringsAsFactors = FALSE)
  
  # Convert all column names to lowercase
  dat <- dat %>% 
    rename_with(tolower) %>% 
    mutate(wave=wave_num,
           year=2000+wave_num*2) %>%
    select("idauniq","tothass_bu_i","tothass_bu_f","tothass_bu_t","totinc_bu_s",
           "totinc_bu_f","totinc_bu_t","eqtotinc_bu_s","eqtotinc_bu_f","eqtotinc_bu_t")
  
  # Dynamically create object name, e.g., core_w2
  obj_name <- paste0("core_w", wave_num, "_finance")
  assign(obj_name, dat, envir = .GlobalEnv)
  
  return(dat)
}

for (w in 1:11) {
  load_finance(w)
}

finstat_map <- c(
  "C1CM"  = 1,
  "C1CP"  = 2,
  "C1SM"  = 3,
  "C1YP"  = 4,
  "C1NP1" = 5,
  "C1NP2" = 6,
  "C3CM"  = 7,
  "C3CP"  = 8,
  "C3SM"  = 9,
  "C3YP"  = 10,
  "C3OP"  = 11,
  "C3NP3" = 12,
  "C1NP3" = 13
)

core_w1_selected <- core_w1 %>%
  left_join(core_w1_finance, by="idauniq") %>% 
  mutate(finstatw1 = as.integer(finstat_map[finstat]),
         couple = ifelse(dimar %in% c(2,3), 3,
                         ifelse(dimar %in% c(1,4,5,6), 1, NA))) %>%
  select("idauniq","couple","cpid","hhresp","perid","hhtot","wave","year",
         "finstatw1",starts_with("idahhw"),"indoc",ends_with("hhout"),
         # ---- Demographics ----
         "indager","dhsex","indsex","dimar","fqend",ends_with("edqual"),"fqethnr",
         # ---- SES ----
         "wpdes","wpwlyy","wpwll","wpwlu","wpwle","wpwlr",
         "tothass_bu_i","tothass_bu_f","tothass_bu_t","totinc_bu_s","totinc_bu_f",
         "totinc_bu_t","eqtotinc_bu_s","eqtotinc_bu_f","eqtotinc_bu_t",
         # ---- Health ----
         "hegenh","heill",starts_with("he"),
         # ---- Depression ----
         "psceda","pscedb","pscedc","pscedd","pscede","pscedf","pscedg","pscedh",
         # ---- Loneliness ----
         #"scfeela","scfeelb","scfeelc","scfeeld",starts_with("sc"),
         # ---- Cognition  ----
         "cflisen","cflisd","cfdatd","cfdatm","cfdaty","cfday","cfdscr","cfani",
         "cfimp",starts_with("cf")) %>%
  rename(persno=perid,idahhw=idahhw1)

core_w2_selected <- core_w2 %>%
  left_join(core_w1 %>% select("idauniq","edqual"), by="idauniq") %>% 
  left_join(core_w2_finance, by="idauniq") %>% 
  mutate(finstatw2 = as.integer(finstat_map[finstat])) %>%
  select("idauniq","couple","cpid","hhresp","persno","sampsta","hhtot","wave","year",
         "finstatw2",starts_with("idahhw"),ends_with("indout"),ends_with("hhout"),
         # ---- Demographics ----
         "indager","dhsex","indsex","dimar","fqend",ends_with("edqual"),"fqethnr",
         # ---- SES ----
         "wpdes","wpwlyy","wpwlyyl","wpwlyyu","wpwlyye","wpwlyyr",
         "tothass_bu_i","tothass_bu_f","tothass_bu_t","totinc_bu_s","totinc_bu_f",
         "totinc_bu_t","eqtotinc_bu_s","eqtotinc_bu_f","eqtotinc_bu_t",
         # ---- Health ----
         "hehelf","heill",starts_with("he"),
         # ---- Depression ----
         "psceda","pscedb","pscedc","pscedd","pscede","pscedf","pscedg","pscedh",
         "totpsc",starts_with("ps"),
         # ---- Loneliness ----
         "scfeela","scfeelb","scfeelc","scfeeld",starts_with("sc"),
         # ---- Cognition  ----
         "cflisen","cflisd","cfdatd","cfdatm","cfdaty","cfday","cfdscr","cfani",
         "cfimp",starts_with("cf")) %>%
  rename(hegenh=hehelf,idahhw=idahhw2)

core_w3_selected <- core_w3 %>%
  left_join(core_w3_finance, by="idauniq") %>% 
  mutate(finstatw3 = as.integer(finstat_map[finstat])) %>%
  select("idauniq","couple","cpid","hhresp","perid","sampsta","hhtot","wave","year",
         "finstatw3",starts_with("idahhw"),ends_with("indout"),ends_with("hhout"),
         # ---- Demographics ----
         "indager","dhsex","indsex","dimar","fqend",ends_with("edqual"),"fqethnr",
         # ---- SES ----
         "wpdes","wpwlyy","wpwlyyl","wpwlyyu","wpwlyye","wpwlyyr",
         "tothass_bu_i","tothass_bu_f","tothass_bu_t","totinc_bu_s","totinc_bu_f",
         "totinc_bu_t","eqtotinc_bu_s","eqtotinc_bu_f","eqtotinc_bu_t",
         # ---- Health ----
         "hegenh","heill",starts_with("he"),
         # ---- Depression ----
         "psceda","pscedb","pscedc","pscedd","pscede","pscedf","pscedg","pscedh",
         "totalpsc",starts_with("ps"),
         # ---- Loneliness ----
         "scfeela","scfeelb","scfeelc","scfeeld",starts_with("sc"),
         # ---- Cognition  ----
         "cflisen","cflisd","cfdatd","cfdatm","cfdaty","cfday","cfdscr","cfani",
         "cfimp",starts_with("cf")) %>%
  rename(persno=perid, totpsc=totalpsc,idahhw=idahhw3,edqual=w3edqual)

core_w4_selected <- core_w4 %>%
  left_join(core_w4_finance, by="idauniq") %>% 
  mutate(sampsta=ifelse(samptyp=="CM", 1, 
                        ifelse(samptyp=="CP", 2, 
                               ifelse(samptyp=="NP", 3, 
                                      ifelse(samptyp=="YP", 4, 
                                             ifelse(samptyp=="OP", 5,
                                                    ifelse(samptyp=="SM", 6, -1))))))) %>%  
  select("idauniq","couple","cpid","hhresp","perid","samptyp","hhtot","wave","year",
         starts_with("finstat"),starts_with("idahhw"),ends_with("indout"),ends_with("hhout"),
         "outindw4","outhhw4",
         # ---- Demographics ----
         "indager","dhsex","indsex","dimar","fqend",ends_with("edqual"),"fqethnr",
         # ---- SES ----
         "wpdes","wpwlyy","wpwlyyl","wpwlyyu","wpwlyye","wpwlyyr",
         "tothass_bu_i","tothass_bu_f","tothass_bu_t","totinc_bu_s","totinc_bu_f",
         "totinc_bu_t","eqtotinc_bu_s","eqtotinc_bu_f","eqtotinc_bu_t",
         # ---- Health ----
         "hehelf","heill",starts_with("he"),
         # ---- Depression ----
         "psceda","pscedb","pscedc","pscedd","pscede","pscedf","pscedg","pscedh",
         starts_with("ps"),
         # ---- Loneliness ----
         "scfeela","scfeelb","scfeelc","scfeeld",starts_with("sc"),
         # ---- Cognition  ----
         "cflisen","cflisd","cfdatd","cfdatm","cfdaty","cfday","cfdscr","cfani",
         "cfimp",starts_with("cf")) %>%
  rename(persno=perid, hegenh=hehelf, w4indout=outindw4, w4hhout=outhhw4,
         idahhw=idahhw4,edqual=w4edqual)

core_w5_selected <- core_w5 %>%
  left_join(core_w5_finance, by="idauniq") %>% 
  mutate(sampsta=ifelse(samptyp=="CM", 1, 
                        ifelse(samptyp=="CP", 2, 
                               ifelse(samptyp=="NP", 3, 
                                      ifelse(samptyp=="YP", 4, 
                                             ifelse(samptyp=="OP", 5,
                                                    ifelse(samptyp=="SM", 6, -1))))))) %>%
  mutate(w5fqend = pmax(fqend, fffqend, na.rm = TRUE) ) %>%
  select("idauniq","couple","cpid","hhresp","perid","sampsta","hhtot","wave","year",
         starts_with("finstat"),starts_with("idahhw"),ends_with("indout"),ends_with("hhout"),
         # ---- Demographics ----
         "indager","dhsex","indsex","dimar","w5fqend",ends_with("edqual"),"fqethnr",
         # ---- SES ----
         "wpdes","wpwlyy","wpwlyyl","wpwlyyu","wpwlyye","wpwlyyr",
         "tothass_bu_i","tothass_bu_f","tothass_bu_t","totinc_bu_s","totinc_bu_f",
         "totinc_bu_t","eqtotinc_bu_s","eqtotinc_bu_f","eqtotinc_bu_t",
         # ---- Health ----
         "hehelf","heill",starts_with("he"),
         # ---- Depression ----
         "psceda","pscedb","pscedc","pscedd","pscede","pscedf","pscedg","pscedh",
         starts_with("ps"),
         # ---- Loneliness ----
         "scfeela","scfeelb","scfeelc","scfeeld",starts_with("sc"),
         # ---- Cognition  ----
         "cflisen","cflisd","cfdatd","cfdatm","cfdaty","cfday","cfdscr","cfani",
         "cfimp",starts_with("cf")) %>%
  rename(persno=perid, hegenh=hehelf,idahhw=idahhw5,edqual=w5edqual,fqend=w5fqend)

core_w6_selected <- core_w6 %>%
  left_join(core_w6_finance, by="idauniq") %>% 
  mutate(w6fqend = pmax(fqend, fffqend, na.rm = TRUE) ) %>%
  select("idauniq","couple","cpid","hhresp","perid","samptyp","hhtot","wave","year",
         starts_with("finstat"),starts_with("idahhw"),ends_with("indout"),ends_with("hhout"),
         # ---- Demographics ----
         "indager","dhsex","indsex","dimar","w6fqend",ends_with("edqual"),"fqethnr",
         # ---- SES ----
         "wpdes","wpwlyy","wpwlyyl","wpwlyyu","wpwlyye","wpwlyyr",
         "tothass_bu_i","tothass_bu_f","tothass_bu_t","totinc_bu_s","totinc_bu_f",
         "totinc_bu_t","eqtotinc_bu_s","eqtotinc_bu_f","eqtotinc_bu_t",
         # ---- Health ----
         "hehelf","heill",starts_with("he"),
         # ---- Depression ----
         "psceda","pscedb","pscedc","pscedd","pscede","pscedf","pscedg","pscedh",
         starts_with("ps"),
         # ---- Loneliness ----
         "scfeela","scfeelb","scfeelc","scfeeld",starts_with("sc"),
         # ---- Cognition  ----
         "cflisen","cflisd","cfdatd","cfdatm","cfdaty","cfday","cfdscr",
         "cfimp",starts_with("cf")) %>%
  rename(persno=perid, hegenh=hehelf, sampsta=samptyp,idahhw=idahhw6,fqend=w6fqend)

core_w7_selected <- core_w7 %>%
  left_join(core_w7_finance, by="idauniq") %>% 
  mutate(w7fqend = pmax(fqend, fffqend, na.rm = TRUE) ) %>%
  select("idauniq","couple","cpid","hhresp","perid","samptyp","hhtot","wave","year",
         starts_with("finstat"),starts_with("idahhw"),ends_with("indout"),ends_with("hhout"),
         # ---- Demographics ----
         "indager","dhsex","indsex","dimar","w7fqend",ends_with("edqual"),"fqethnr",
         # ---- SES ----
         "wpdes","wpwlyy","wpwlyyl","wpwlyyu","wpwlyye","wpwlyyr",
         "tothass_bu_i","tothass_bu_f","tothass_bu_t","totinc_bu_s","totinc_bu_f",
         "totinc_bu_t","eqtotinc_bu_s","eqtotinc_bu_f","eqtotinc_bu_t",
         # ---- Health ----
         "hehelf","heill",starts_with("he"),
         # ---- Depression ----
         "psceda","pscedb","pscedc","pscedd","pscede","pscedf","pscedg","pscedh",
         starts_with("ps"),
         # ---- Loneliness ----
         "scfeela","scfeelb","scfeelc","scfeeld",starts_with("sc"),
         # ---- Cognition  ----
         "cflisen","cflisd","cfdatd","cfdatm","cfdaty","cfday","cfdscr","cfani",
         "cfimp",starts_with("cf")) %>%
  rename(persno=perid, hegenh=hehelf, sampsta=samptyp,idahhw=idahhw7,
         fqend=w7fqend)

core_w8_selected <- core_w8 %>%
  left_join(core_w8_finance, by="idauniq") %>% 
  select("idauniq","couple","cpid","hhresp","perid","samptyp","hhtot","wave","year",
         starts_with("finstat"),starts_with("idahhw"),ends_with("indout"),ends_with("hhout"),
         # ---- Demographics ----
         "indager","indsex","dimarr","fqendm",ends_with("edqual"),"fqethnmr",
         # ---- SES ----
         "wpdes","wpwlyy","wpwlyyl","wpwlyyu","wpwlyye","wpwlyyr",
         "tothass_bu_i","tothass_bu_f","tothass_bu_t","totinc_bu_s","totinc_bu_f",
         "totinc_bu_t","eqtotinc_bu_s","eqtotinc_bu_f","eqtotinc_bu_t",
         # ---- Health ----
         "hehelf","heill",starts_with("he"),
         # ---- Depression ----
         "psceda","pscedb","pscedc","pscedd","pscede","pscedf","pscedg","pscedh",
         starts_with("ps"),
         # ---- Loneliness ----
         "scfeela","scfeelb","scfeelc","scfeeld",starts_with("sc"),
         # ---- Cognition  ----
         "cflisen","cflisd","cfdatd","cfdatm","cfdaty","cfday","cfdscr","cfani",
         "cfimp",starts_with("cf")) %>%
  rename(persno=perid, hegenh=hehelf, sampsta=samptyp,idahhw=idahhw8,
         fqethnr=fqethnmr,fqend=fqendm)

core_w9_selected <- core_w9 %>%
  left_join(core_w9_finance, by="idauniq") %>% 
  select("idauniq","couple","cpid","hhresp","perid","samptyp","hhtot","wave","year",
         starts_with("finstat"),starts_with("idahhw"),ends_with("indout"),ends_with("hhout"),
         # ---- Demographics ----
         "indager","indsex","dimarr","fqendm",ends_with("edqual"),"fqethnmr",
         # ---- SES ----
         "wpdes","wpwlyy","wpwlyyl","wpwlyyu","wpwlyye","wpwlyyr",
         "tothass_bu_i","tothass_bu_f","tothass_bu_t","totinc_bu_s","totinc_bu_f",
         "totinc_bu_t","eqtotinc_bu_s","eqtotinc_bu_f","eqtotinc_bu_t",
         # ---- Health ----
         "hehelf","heill",starts_with("he"),
         # ---- Depression ----
         "psceda","pscedb","pscedc","pscedd","pscede","pscedf","pscedg","pscedh",
         starts_with("ps"),
         # ---- Loneliness ----
         "scfeela","scfeelb","scfeelc","scfeeld",starts_with("sc"),
         # ---- Cognition  ----
         "cflisen","cflisd","cfdatd","cfdatm","cfdaty","cfday","cfdscr","cfani",
         "cfimp",starts_with("cf")) %>%
  rename(persno=perid, hegenh=hehelf, sampsta=samptyp,idahhw=idahhw9,
         fqethnr=fqethnmr,fqend=fqendm)

core_w10_selected <- core_w10 %>%
  left_join(core_w10_finance, by="idauniq") %>% 
  select("idauniq","couple","cpid","hhresp","perid","corepartner","hhtot","wave","year",
         starts_with("finstat"),starts_with("idahhw"),ends_with("indout"),ends_with("hhout"),
         "corepartner",
         # ---- Demographics ----
         "indager","indsex","dimarr","fqendm",ends_with("edqual"),"fqethnmr",
         # ---- SES ----
         "wpdes","wpwlyy","wpwlyyl","wpwlyyu","wpwlyye","wpwlyyr",
         "tothass_bu_i","tothass_bu_f","tothass_bu_t","totinc_bu_s","totinc_bu_f",
         "totinc_bu_t","eqtotinc_bu_s","eqtotinc_bu_f","eqtotinc_bu_t",
         # ---- Health ----
         "hehelf","heill",starts_with("he"),
         # ---- Depression ----
         "psceda","pscedb","pscedc","pscedd","pscede","pscedf","pscedg","pscedh",
         starts_with("ps"),
         # ---- Loneliness ----
         "scfeela","scfeelb","scfeelc","scfeeld",starts_with("sc"),
         # ---- Cognition  ----
         "cflisen","cflisd","cfdatd","cfdatm","cfdaty","cfday","cfdscr","cfani",
         "cfimp",starts_with("cf")) %>%
  rename(persno=perid, hegenh=hehelf,idahhw=idahhw10,fqethnr=fqethnmr,fqend=fqendm)

core_w11_selected <- core_w11 %>%
  select("idauniq","couple","cpid","hhresp","perid","corepartner","hhtot","wave","year",
         starts_with("finstat"),starts_with("idahhw"),ends_with("indout"),ends_with("hhout"),
         "corepartner",
         # ---- Demographics ----
         "indager","indsex","dimarr","fqendm",ends_with("edqual"),"fqethnmr",
         # ---- SES ----
         "wpdes","wpwlyy","wpwlyyl","wpwlyyu","wpwlyye","wpwlyyr",
         # ---- Health ----
         "hehelf","heill",starts_with("he"),
         # ---- Depression ----
         "psceda","pscedb","pscedc","pscedd","pscede","pscedf","pscedg","pscedh",
         starts_with("ps"),
         # ---- Loneliness ----
         "scfeela","scfeelb","scfeelc","scfeeld",starts_with("sc"),
         # ---- Cognition  ----
         "cflisen","cflisd","cfdatd","cfdatm","cfdaty","cfday","cfdscr","cfani",
         "cfimp",starts_with("cf")) %>%
  rename(persno=perid, hegenh=hehelf,idahhw=idahhw11,fqethnr=fqethnmr,fqend=fqendm)

# Find all data frames whose names start with "core_w"
objs <- ls(pattern = "^core_w\\d+_selected$")
core_list <- mget(objs, inherits = TRUE)
waves <- as.integer(stringr::str_extract(objs, "\\d+"))
core_list <- core_list[order(waves)]

# Append all datasets, automatically aligning columns by name
core_all <- dplyr::bind_rows(core_list)

set_na_if_neg_all <- function(data) {
  data %>%
    mutate(across(where(is.numeric), ~ ifelse(.x < 0, NA, .x)))
}

core_all <- set_na_if_neg_all(core_all) 

keep_vars <- c(
  "idauniq","couple","cpid","hhresp","persno","sampsta","hhtot","wave","year","idahhw",
  # ---- Demographics ----
  "indager","dhsex","indsex","dimar","fqend","fqethnr","edqual",
  # ---- SES ----
  "wpdes","wpwlyy","wpwlyyl","wpwlyyu","wpwlyye","wpwlyyr",
  "tothass_bu_i","tothass_bu_f","tothass_bu_t","totinc_bu_s","totinc_bu_f",
  "totinc_bu_t","eqtotinc_bu_s","eqtotinc_bu_f","eqtotinc_bu_t",
  # ---- Health ----
  "hegenh","heill","helim",
  # ---- Blood pressure ----
  "hebpchk","heeverbp","henvrbp","hehavebp",
  # ---- Diabetes ----
  "heacd","henvrdi","hehavedi",
  # ---- Cancer ----
  "heeverca","henvrca","hehaveca",
  # ---- Lung disease ----
  "heevercl","henvrcl","hehavecl",
  # ---- Heart problems ----
  "heartoa","heartra","heartot","hehavear","heeveros","henvros","hehaveos",
  # ---- Stroke ----
  "heeverst","henvrst","heothst","hemda1","hedimst","hedacst","heage","heager","heagery","henmst",
  # ---- Arthritis ----
  "heeverar","henvrar","heartoa","heartra","heartot","hehavear",
  # ---- ADRD ----
  "hedbdad","hedbdde",
  # ---- Psychiatric problems ----
  "heeverps","henvrps","hehaveps","hepsyde",
  # ---- ADL & IADL ----
  "headldr","headlwa","headlba","headlea","headlbe","headlwc",
  "headlma","headlpr","headlsh","headlph","headlme","headlho","headlmo",
  "headlda","headlsp",
  # ---- Marital quality ----
  "scptra","scptrb","scptrc","scptrd","scptre","scptrf","scptrg",
  # ---- Children contact ----
  "scchdg","scchdh","scchdj",
  # ---- Relatives contact ----
  "scfamg","scfamh","scfamj",
  # ---- Friends contact ----
  "scfrd","scfrdg","scfrdh","scfrdj","scfrdk",
  # ---- Loneliness ----
  "scfeela","scfeelb","scfeelc","scfeeld","scfeele",
  # ---- Depressive symptoms ----
  "psceda","pscedb","pscedc","pscedd","pscede","pscedf","pscedg","pscedh",
  # ---- Cognition  ----
  "cflisen","cflisd","cfdatd","cfdatm","cfdaty","cfday","cfdscr","cfani",
  "cfimp","cfwhteye","cfwhthea","cfwhttir","cfwhtill","cfwhtcon","cfwhtner",
  "cfwhtmen","cfwhtdis","cfwhtnoi","cfwhtlap","cfwhteng","cfwht95"
)

core_all <- core_all %>% 
  arrange(idauniq, wave) %>% 
  mutate(indout = case_when(
    wave == 1 ~ indoc,
    wave == 2 ~ w2indout,
    wave == 3 ~ w3indout,
    wave == 4 ~ w4indout,
    wave == 5 ~ w5indout,
    wave == 6 ~ w6indout,
    wave == 7 ~ w7indout,
    wave == 8 ~ w8indout,
    wave == 9 ~ w9indout,
    TRUE ~ NA_real_),
    hhout = case_when(
      wave == 1 ~ rthhout,
      wave == 2 ~ w2hhout,
      wave == 3 ~ w3hhout,
      wave == 4 ~ w4hhout,
      wave == 5 ~ w5hhout,
      wave == 6 ~ w6hhout,
      wave == 7 ~ w7hhout,
      wave == 8 ~ w8hhout,
      wave == 9 ~ w9hhout,
      TRUE ~ NA_real_),
    core_mem = case_when(
      wave == 1 & finstatw1 %in% c(1) ~ 1,
      wave == 2 & finstatw2 %in% c(1) ~ 1,
      wave == 3 & finstatw3 %in% c(1,7) ~ 1,
      wave == 4 & grepl("CM", finstat4) ~ 1,
      wave == 5 & grepl("CM", finstatw5) ~ 1,
      wave == 6 & finstatw6 %in% c(1,7,14,25) ~ 1,
      wave == 7 & finstatw7 %in% c(1,7,14,25,33) ~ 1,
      wave == 8 & finstat %in% c(1,7,14,25,33) ~ 1,
      wave == 9 & finstat %in% c(1,7,14,25,33,48) ~ 1,
      wave == 10 & corepartner %in% c(1) ~ 1,
      wave == 11 & corepartner %in% c(1) ~ 1,
      # Everything else (partners, non-sample members, missing)
      TRUE ~ 0)
  )%>%
  select(idauniq, year, wave, indout, hhout, core_mem, starts_with("finstat"),
         all_of(keep_vars), any_of(c("nrow", "nclm", "ncorrect", "nmissed", "nrowclm")))

write_csv(core_all, "path/to/data/core_all.csv")
