# Import libraries
library(dplyr)
library(lubridate)
library(readr)
library(stringr)
library(tidyr)
library(readxl)

# GNSIS file
GNSIS_FILE <- read_excel("~/RProjects/FINAL_GNSIS_DATA/GNSIS_DATABASE_v7.5.1_7.15.2020.xlsx", 
                         na = "NA") %>% mutate_if(is.character, str_trim)

# COPD data file
COPD_file <- read_excel("COPDstudy.xlsx", 
                        na = "NA") %>% mutate_if(is.character, str_trim)

GNSIS_MAIN <- left_join(GNSIS_FILE, COPD_file, by = "PT_ID")

# GNSIS2 update file
GNSIS2_UPDATE <- read_excel("~/RProjects/GNSIS2_UPDATE/GNSIS2_UPDATE_ALL_CASES_v1.1_3.24.2021.xlsx",
                            na = "NA") %>% mutate_if(is.character, str_trim)

GNSIS2_UPDATE <- GNSIS2_UPDATE %>% 
  mutate(across(ATRIAL_FIB_PREINDEX:MENIERE_VERTIGO_ALL_TIME, as.numeric))

GNSIS2_UPDATE_NEW_PTS_ONLY <- GNSIS2_UPDATE %>% 
  filter(!PT_ID %in% GNSIS_MAIN$PT_ID)

COPD_file_GNSIS2UPDT <- read_excel("COPDstudy_GNSIS2UPST_PTS.xlsx", 
                                   na = "NA") %>% mutate_if(is.character, str_trim)

GNSIS2_UPDATE_NEW_PTS_ONLY <- left_join(GNSIS2_UPDATE_NEW_PTS_ONLY, COPD_file_GNSIS2UPDT, by = "PT_ID")

# Bind rows
GNSIS_MAIN <- bind_rows(GNSIS_MAIN, GNSIS2_UPDATE_NEW_PTS_ONLY)


# Spirometry

GNSIS_PFT <- read_excel("GNSIS_PULM_SPIROMETRY_4.29.2021.xlsx", col_names = TRUE,
                        na = "NULL") %>% 
  mutate(STUDY_DTTM = as.Date(STUDY_DTTM),
         STUDY_SORT_ORDER = as.numeric(STUDY_SORT_ORDER),
         PX_PREDICTED_RES_VAL = as.numeric(PX_PREDICTED_RES_VAL),
         PX_ACTUAL_RES_VAL = as.numeric(PX_ACTUAL_RES_VAL),
         PX_POST_RES_VAL = as.numeric(PX_POST_RES_VAL),
         PX_PRE_PCT_PRED = as.numeric(PX_PRE_PCT_PRED),
         PX_POST_PCT_PRED = as.numeric(PX_POST_PCT_PRED),
         PX_STD_DEV = as.numeric(PX_STD_DEV))

GNSIS_PFT_GNSIS2UPDT <- read_excel("GNSIS2_UPDATE_PULM_SPIROMETRY_6.11.2021.xlsx",
                                   col_names = TRUE,
                                   na = "NULL") %>% 
  mutate(STUDY_DTTM = as.Date(STUDY_DTTM))

GNSIS_PFT <- bind_rows(GNSIS_PFT, GNSIS_PFT_GNSIS2UPDT)


GNSIS_PFT <- GNSIS_PFT %>% 
  filter(PX_COMP_NM %in% c("FEV1/FVC", "FEV1", "FVC"))

# FEV1/FVC

GNSIS_PFT_1 <- GNSIS_PFT %>% 
  select(PT_ID, STUDY_DTTM, PX_COMP_NM, PX_ACTUAL_RES_VAL, PX_ACTUAL_LNC_NM) %>% 
  filter(PX_COMP_NM == "FEV1/FVC") %>% 
  filter(!is.na(PX_ACTUAL_RES_VAL)) %>% 
  rename(FEV1_FVC_RATIO_PRE = PX_ACTUAL_RES_VAL) %>% 
  select(-PX_COMP_NM, -PX_ACTUAL_LNC_NM)

# FEV1, FEV1 % predicted

GNSIS_PFT_2 <- GNSIS_PFT %>% 
  select(PT_ID, STUDY_DTTM, PX_COMP_NM, PX_ACTUAL_RES_VAL, PX_ACTUAL_LNC_NM, PX_PRE_PCT_PRED) %>% 
  filter(PX_COMP_NM == "FEV1") %>% 
  filter(!is.na(PX_ACTUAL_RES_VAL)) %>% 
  rename(FEV1_PRE = PX_ACTUAL_RES_VAL,
         FEV1_PCT_PRED = PX_PRE_PCT_PRED) %>% 
  select(-PX_COMP_NM, -PX_ACTUAL_LNC_NM)

# FVC, FVC % predicted

GNSIS_PFT_3 <- GNSIS_PFT %>% 
  select(PT_ID, STUDY_DTTM, PX_COMP_NM, PX_ACTUAL_RES_VAL, PX_ACTUAL_LNC_NM, PX_PRE_PCT_PRED) %>% 
  filter(PX_COMP_NM == "FVC") %>% 
  filter(!is.na(PX_ACTUAL_RES_VAL)) %>% 
  rename(FVC_PRE = PX_ACTUAL_RES_VAL,
         FVC_PCT_PRED = PX_PRE_PCT_PRED) %>% 
  select(-PX_COMP_NM, -PX_ACTUAL_LNC_NM)

# 
GNSIS_PFT_COMBINED <- full_join(GNSIS_PFT_1, GNSIS_PFT_2, by = c("PT_ID", "STUDY_DTTM")) %>% 
  full_join(GNSIS_PFT_3, by = c("PT_ID", "STUDY_DTTM")) %>% 
  mutate(across(FEV1_FVC_RATIO_PRE:FVC_PCT_PRED, as.numeric)) %>% 
  mutate(SPIROM_DT = as.Date(STUDY_DTTM)) %>% 
  select(-STUDY_DTTM)

GNSIS_PFT_COMBINED <- GNSIS_PFT_COMBINED %>% 
  mutate(FEV1_FVC_BELOW_70 = as.numeric(FEV1_FVC_RATIO_PRE < 70))

#
GNSIS_PFT_COMBINED <- GNSIS_PFT_COMBINED %>% 
  left_join(GNSIS_MAIN[,c("PT_ID", "INDEX_DT", "COPD_ENTRY_DT")], by = "PT_ID")

GNSIS_PFT_COMBINED <- GNSIS_PFT_COMBINED %>% 
  filter(SPIROM_DT <= as.Date("2020-08-01")) %>% 
  mutate(TIMEDIFF_COPD_INDEX = as.numeric(difftime(COPD_ENTRY_DT, INDEX_DT, units = "days")),
         TIMEDIFF_SPIROM_INDEX = as.numeric(difftime(SPIROM_DT, INDEX_DT, units = "days")))

GNSIS_PFT_COMBINED2 <- GNSIS_PFT_COMBINED %>% 
  filter(FEV1_FVC_BELOW_70 == 1) %>% 
  group_by(PT_ID) %>% 
  slice_min(SPIROM_DT, with_ties = FALSE) %>% 
  ungroup()

# Joining with GNSIS

GNSIS <- GNSIS_MAIN %>% 
  left_join(GNSIS_PFT_COMBINED2[,1:8], by = "PT_ID")

# 
GNSIS <- GNSIS %>% 
  mutate(COPD_ENTRY_DT = 
           case_when(COPD_ALL_TIME == 1 ~ as.Date(COPD_ENTRY_DT),
                     COPD_ALL_TIME == 0 ~ NA_Date_)) %>% 
  mutate(COPD_ENTRY_DT_EARLIEST = pmin(COPD_ENTRY_DT, SPIROM_DT, na.rm = TRUE),
         COPD_ENTRY_DT_LATEST = pmax(COPD_ENTRY_DT, SPIROM_DT, na.rm = TRUE)) %>% 
  mutate(TIMEDIFF_COPD_INDEX = 
           case_when(
             COPD_ALL_TIME == 1 ~ as.numeric(difftime(COPD_ENTRY_DT_EARLIEST, INDEX_DT, units = "days")),
             COPD_ALL_TIME == 0 ~ NA_real_),
         TIMEDIFF_SPIROM_INDEX = as.numeric(difftime(SPIROM_DT, INDEX_DT, units = "days")))

GNSIS <- GNSIS %>% 
  mutate(COPD_DX_AFTER_INDEX = 
           case_when(COPD_AT_INDEX == 0 & COPD_ALL_TIME == 0 ~ 0,
                     COPD_AT_INDEX == 0 & COPD_ALL_TIME == 1 ~ 1,
                     COPD_AT_INDEX == 1 & COPD_ALL_TIME == 0 ~ 0,
                     COPD_AT_INDEX == 1 & COPD_ALL_TIME == 1 ~ 0),
         FEV1_FVC_BELOW_70_AFTER_INDEX = as.numeric(SPIROM_DT > INDEX_DT))


# Exclusions

# Excluding <18 years old

GNSIS_COHORT <- GNSIS %>%
  filter(AGE_AT_INDEX >=18)

# Only selecting patients till 2017-08-01
GNSIS_COHORT <- GNSIS_COHORT %>%
  filter(INDEX_DT <= as.Date("2017-08-01"))

# Excluding COPD diagnosis after stroke

GNSIS_COHORT <- GNSIS_COHORT %>% 
  filter(COPD_DX_AFTER_INDEX == 0 | FEV1_FVC_BELOW_70_AFTER_INDEX == 0)

GNSIS_COHORT <- GNSIS_COHORT %>% 
  mutate(AGE_AT_COPD_DX = round(as.numeric(difftime(COPD_ENTRY_DT_EARLIEST, PT_BIRTH_DT, units = "days"))/365.25, 1))

# 
# 
GNSIS_COHORT <- GNSIS_COHORT %>% 
  mutate(CASE_CONTROL1 = 
           case_when(
             COPD_ALL_TIME == 0 & (FEV1_FVC_BELOW_70 != 1 | is.na(FEV1_FVC_BELOW_70)) ~ "NO_COPD",
             COPD_AT_INDEX == 1 & AGE_AT_COPD_DX >= 40 ~ "COPD_ICD"
           ))

table(GNSIS_COHORT$CASE_CONTROL1, useNA = "ifany")

# 
GNSIS_COHORT <- GNSIS_COHORT %>% 
  mutate(CASE_CONTROL2 = 
           case_when(
             COPD_ALL_TIME == 0 & (FEV1_FVC_BELOW_70 != 1 | is.na(FEV1_FVC_BELOW_70)) ~ "NO_COPD",
             FEV1_FVC_BELOW_70_AT_INDEX == 1 ~ "COPD_SPIROM"
           ))

table(GNSIS_COHORT$CASE_CONTROL2, useNA = "ifany")

# 
GNSIS_COHORT <- GNSIS_COHORT %>% 
  mutate(CASE_CONTROL3 = 
           case_when(
             CASE_CONTROL1 == "NO_COPD" & CASE_CONTROL2 == "NO_COPD" ~ "NO_COPD",
             CASE_CONTROL1 == "COPD_ICD" & CASE_CONTROL2 == "COPD_SPIROM" ~ "COPD_COMBINED"
           ))

table(GNSIS_COHORT$CASE_CONTROL3, useNA = "ifany")

# 
INCONSISTENT <- GNSIS_COHORT %>% 
  filter(is.na(CASE_CONTROL1) & is.na(CASE_CONTROL2) & is.na(CASE_CONTROL3))

GNSIS_COHORT <- GNSIS_COHORT %>% 
  filter(!(PT_ID %in% INCONSISTENT$PT_ID))

# 
var_list1 <- c("PT_SEX",
               "AGE_AT_COPD_DX",
               "AGE_AT_INDEX",
               "TIME_COPD_TO_INDEX_STROKE",
               "HYPERTENSION_AT_INDEX",
               "ATRIAL_FIB_AT_INDEX",
               "DYSLIPIDEMIA_AT_INDEX",
               "DIABETES_AT_INDEX",
               "CHF_AT_INDEX",
               "MI_AT_INDEX",
               "PERI_VASC_DIS_AT_INDEX",
               "STATINS_PRIOR",
               "ANTI_HTN_PRIOR",
               "INDEX_INSURANCE_TYPE",
               "SMOKE_STTS",
               "NIHSS_AT_INDEX")

cat_list1 <- c("PT_SEX",
               "HYPERTENSION_AT_INDEX",
               "ATRIAL_FIB_AT_INDEX",
               "DYSLIPIDEMIA_AT_INDEX",
               "DIABETES_AT_INDEX",
               "CHF_AT_INDEX",
               "MI_AT_INDEX",
               "PERI_VASC_DIS_AT_INDEX",
               "STATINS_PRIOR",
               "ANTI_HTN_PRIOR",
               "INDEX_INSURANCE_TYPE",
               "SMOKE_STTS")

# 
table1_a <- print(tableone::CreateTableOne(vars = var_list1,
                                           factorVars = cat_list1,
                                           data = GNSIS_COHORT,
                                           strata = "CASE_CONTROL1",
                                           addOverall = TRUE),
                  noSpaces = TRUE,
                  nonnormal = c("AGE_AT_COPD_DX",
                                "AGE_AT_INDEX",
                                "BMI_CLOSEST_TO_INDEX",
                                "TIME_COPD_TO_INDEX_STROKE",
                                "NIHSS_AT_INDEX"),
                  contDigits = 1)

table1_b <- print(tableone::CreateTableOne(vars = var_list1,
                                           factorVars = cat_list1,
                                           data = GNSIS_COHORT,
                                           strata = "CASE_CONTROL2",
                                           addOverall = TRUE),
                  noSpaces = TRUE,
                  nonnormal = c("AGE_AT_COPD_DX",
                                "AGE_AT_INDEX",
                                "BMI_CLOSEST_TO_INDEX",
                                "TIME_COPD_TO_INDEX_STROKE",
                                "NIHSS_AT_INDEX"),
                  contDigits = 1)

table1_c <- print(tableone::CreateTableOne(vars = var_list1,
                                           factorVars = cat_list1,
                                           data = GNSIS_COHORT,
                                           strata = "CASE_CONTROL3",
                                           addOverall = TRUE),
                  noSpaces = TRUE,
                  nonnormal = c("AGE_AT_COPD_DX",
                                "AGE_AT_INDEX",
                                "BMI_CLOSEST_TO_INDEX",
                                "TIME_COPD_TO_INDEX_STROKE",
                                "NIHSS_AT_INDEX"),
                  contDigits = 1)

#####
GNSIS_COHORT <- GNSIS_COHORT %>% 
  mutate(FEV1_PCT_PRED_cat = 
           case_when(is.na(FEV1_PCT_PRED) ~ "0",
                     FEV1_FVC_BELOW_70_AT_INDEX == 1 & FEV1_PCT_PRED >= 80 ~ "GOLD1",
                     FEV1_FVC_BELOW_70_AT_INDEX == 1 & FEV1_PCT_PRED < 80  & FEV1_PCT_PRED >= 50 ~ "GOLD2",
                     FEV1_FVC_BELOW_70_AT_INDEX == 1 & FEV1_PCT_PRED < 50 & FEV1_PCT_PRED >= 30 ~ "GOLD3",
                     FEV1_FVC_BELOW_70_AT_INDEX == 1 & FEV1_PCT_PRED < 30 ~ "GOLD4"))

table(GNSIS_COHORT$CASE_CONTROL2)
table(GNSIS_COHORT$FEV1_PCT_PRED_cat)

###

# Survival 3 year

GNSIS3 <- GNSIS_COHORT %>% 
  mutate(STUDY_END_DT = as.Date("2020-08-01"))

GNSIS3 <- GNSIS3 %>% 
  mutate(TIMEDIFF_DEATH_INDEX = 
           case_when(PT_STATUS == "DECEASED" & !is.na(PT_DEATH_DT) ~ as.numeric(difftime(PT_DEATH_DT, INDEX_DT, units = "days")),
                     PT_STATUS == "DECEASED" & is.na(PT_DEATH_DT) ~ as.numeric(difftime(LAST_ACTIVE_DT, INDEX_DT, units = "days")),
                     PT_STATUS == "ALIVE" ~ NA_real_)
  )

GNSIS3 <- GNSIS3 %>% 
  mutate(TIMEDIFF_LASTACTIVE_INDEX = as.numeric(difftime(LAST_ACTIVE_DT, INDEX_DT, units = "days")),
         TIMEDIFF_STUDYEND_INDEX = as.numeric(difftime(STUDY_END_DT, INDEX_DT, units = "days"))) %>% 
  mutate(TIME_DEATH_CENSOR = if_else(!is.na(TIMEDIFF_DEATH_INDEX), TIMEDIFF_DEATH_INDEX, TIMEDIFF_STUDYEND_INDEX))

# Changing TIME_RECUR_CENSOR and TIME_DEATH_CENSOR which have values 0 to 0.5
GNSIS3$TIME_DEATH_CENSOR[GNSIS3$TIME_DEATH_CENSOR == 0] <- 0.5

# 
GNSIS3 <- GNSIS3 %>% 
  mutate(DEATH_3YR = 
           case_when(PT_STATUS == "ALIVE" ~ 0,
                     PT_STATUS == "DECEASED" & TIMEDIFF_DEATH_INDEX <= 3*365.25 ~ 1,
                     PT_STATUS == "DECEASED" & TIMEDIFF_DEATH_INDEX > 3*365.25 ~ 0),
         TIME_DEATH_CENSOR_3YR = if_else(TIME_DEATH_CENSOR <= 3*365.25, TIME_DEATH_CENSOR, 3*365.25))

#Survival analysis
library(survival)
library(survminer)

km_plot_death3year_a <-
  ggsurvplot(
    survfit(Surv(TIME_DEATH_CENSOR_3YR/365.25, DEATH_3YR) ~ CASE_CONTROL1, data = GNSIS3),
    conf.int = TRUE,
    conf.int.alpha = 0.25,
    censor = TRUE,
    censor.shape = "|",
    censor.size = 2,
    surv.scale = "percent",
    pval = TRUE,
    pval.coord = c(0,0.55),
    pval.size = 4,
    ylim = c(0.5, 1),
    xlim = c(0, 3),
    risk.table = TRUE,
    subtitle = "A",
    xlab = "Time in years",
    legend.title = "Groups",
    legend.labs = c("Group B: COPD (ICD definition)", "Group A: No COPD"),
    ggtheme = theme_bw()
  )

summary(survfit(Surv(TIME_DEATH_CENSOR_3YR/365.25, DEATH_3YR) ~ CASE_CONTROL1, data = GNSIS3), times = c(1,3))

km_plot_death3year_b <-
  ggsurvplot(
    survfit(Surv(TIME_DEATH_CENSOR_3YR/365.25, DEATH_3YR) ~ CASE_CONTROL2, data = GNSIS3),
    conf.int = TRUE,
    conf.int.alpha = 0.25,
    censor = TRUE,
    censor.shape = "|",
    censor.size = 2,
    surv.scale = "percent",
    pval = TRUE,
    pval.coord = c(0,0.55),
    pval.size = 4,
    ylim = c(0.5, 1),
    xlim = c(0, 3),
    risk.table = TRUE,
    subtitle = "B",
    xlab = "Time in years",
    legend.title = "Groups",
    legend.labs = c("Group C: COPD (Spirometry definition)", "Group A: No COPD"),
    ggtheme = theme_bw()
  )

summary(survfit(Surv(TIME_DEATH_CENSOR_3YR/365.25, DEATH_3YR) ~ CASE_CONTROL2, data = GNSIS3), times = c(1,3))

km_plot_death3year_c <-
  ggsurvplot(
    survfit(Surv(TIME_DEATH_CENSOR_3YR/365.25, DEATH_3YR) ~ CASE_CONTROL3, data = GNSIS3),
    conf.int = TRUE,
    conf.int.alpha = 0.25,
    censor = TRUE,
    censor.shape = "|",
    censor.size = 2,
    surv.scale = "percent",
    pval = TRUE,
    pval.coord = c(0,0.55),
    pval.size = 4,
    ylim = c(0.5, 1),
    xlim = c(0, 3),
    risk.table = TRUE,
    subtitle = "C",
    xlab = "Time in years",
    legend.title = "Groups",
    legend.labs = c("Group D: COPD (Combined definition)", "Group A: No COPD"),
    ggtheme = theme_bw()
  )

summary(survfit(Surv(TIME_DEATH_CENSOR_3YR/365.25, DEATH_3YR) ~ CASE_CONTROL3, data = GNSIS3), times = c(1,3))

# Combine plot

# tiff("fig3_km3year.tif", width = 27, height = 9, units = "in", res = 300, compression = "lzw")
# 
#   arrange_ggsurvplots(list(km_plot_death3year_a,
#                            km_plot_death3year_b,
#                            km_plot_death3year_c),
#                      ncol = 3,
#                      nrow = 1)
# dev.off()

# Cox model
GNSIS3 <- GNSIS3 %>% 
  mutate(COPD_ICD = as.numeric(CASE_CONTROL1 == "COPD_ICD"),
         COPD_SPIROM = as.numeric(CASE_CONTROL2 == "COPD_SPIROM"),
         COPD_COMBINED = as.numeric(CASE_CONTROL3 == "COPD_COMBINED"))

table(GNSIS3$COPD_ICD, useNA = "ifany")
table(GNSIS3$COPD_SPIROM, useNA = "ifany")
table(GNSIS3$COPD_COMBINED, useNA = "ifany")

coxmodel_death3year_a <- coxph(Surv(TIME_DEATH_CENSOR_3YR/365.25, DEATH_3YR) ~ 
                                 COPD_ICD +
                                 PT_SEX +
                                 AGE_AT_INDEX +
                                 HYPERTENSION_AT_INDEX +
                                 MI_AT_INDEX +
                                 PERI_VASC_DIS_AT_INDEX +
                                 strata(DYSLIPIDEMIA_AT_INDEX,
                                        DIABETES_AT_INDEX,
                                        CHF_AT_INDEX,
                                        ATRIAL_FIB_AT_INDEX), data = GNSIS3)

cox.zph(coxmodel_death3year_a)

coxmodel_death3year_a_results <- broom::tidy(coxmodel_death3year_a, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(conf_range = paste(conf.low, conf.high, sep = " - ")) %>%
  select(term, estimate, conf_range, p.value)
knitr::kable(coxmodel_death3year_a_results)

# 
coxmodel_death3year_b <- coxph(Surv(TIME_DEATH_CENSOR_3YR/365.25, DEATH_3YR) ~ 
                                 COPD_SPIROM +
                                 PT_SEX +
                                 AGE_AT_INDEX +
                                 HYPERTENSION_AT_INDEX +
                                 MI_AT_INDEX +
                                 PERI_VASC_DIS_AT_INDEX +
                                 strata(DYSLIPIDEMIA_AT_INDEX,
                                        DIABETES_AT_INDEX,
                                        CHF_AT_INDEX,
                                        ATRIAL_FIB_AT_INDEX), data = GNSIS3)

cox.zph(coxmodel_death3year_b)

coxmodel_death3year_b_results <- broom::tidy(coxmodel_death3year_b, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(conf_range = paste(conf.low, conf.high, sep = " - ")) %>%
  select(term, estimate, conf_range, p.value)
knitr::kable(coxmodel_death3year_b_results)

# 
coxmodel_death3year_c <- coxph(Surv(TIME_DEATH_CENSOR_3YR/365.25, DEATH_3YR) ~ 
                                 COPD_COMBINED +
                                 PT_SEX +
                                 AGE_AT_INDEX +
                                 HYPERTENSION_AT_INDEX +
                                 MI_AT_INDEX +
                                 PERI_VASC_DIS_AT_INDEX +
                                 strata(DYSLIPIDEMIA_AT_INDEX,
                                        DIABETES_AT_INDEX,
                                        CHF_AT_INDEX,
                                        ATRIAL_FIB_AT_INDEX), data = GNSIS3)

cox.zph(coxmodel_death3year_c)

coxmodel_death3year_c_results <- broom::tidy(coxmodel_death3year_c, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(conf_range = paste(conf.low, conf.high, sep = " - ")) %>%
  select(term, estimate, conf_range, p.value)
knitr::kable(coxmodel_death3year_c_results)

################################

GNSIS3_SUB3 <- GNSIS3 %>% 
  filter(!is.na(CASE_CONTROL1)) %>% 
  mutate(COPD_SUBCAT = 
           case_when(CHRONIC_BRONCHITIS_AT_INDEX == 1 & EMPHYSEMA_AT_INDEX == 0 ~ "CHRONIC_BRONCHITIS",
                     CHRONIC_BRONCHITIS_AT_INDEX == 0 & EMPHYSEMA_AT_INDEX == 1 ~ "EMPHYSEMA",
                     CHRONIC_BRONCHITIS_AT_INDEX == 1 & EMPHYSEMA_AT_INDEX == 1  ~ "EMPHYSEMA_CHR_BRON",
                     CHRONIC_BRONCHITIS_AT_INDEX == 0 & EMPHYSEMA_AT_INDEX == 0 ~ "0")) %>% 
  filter(COPD_SUBCAT != "0")

table(GNSIS3_SUB3$COPD_SUBCAT, GNSIS3_SUB3$CASE_CONTROL1, useNA = "ifany")

km_plot_death3year_subgroup_a <-
  ggsurvplot(
    survfit(Surv(TIME_DEATH_CENSOR_3YR/365.25, DEATH_3YR) ~ COPD_SUBCAT, data = GNSIS3_SUB3),
    conf.int = TRUE,
    conf.int.alpha = 0.25,
    censor = TRUE,
    censor.shape = "|",
    censor.size = 2,
    surv.scale = "percent",
    pval = TRUE,
    pval.coord = c(0,0.55),
    pval.size = 4,
    ylim = c(0.4, 1),
    xlim = c(0, 3),
    risk.table = TRUE,
    xlab = "Time in years",
    legend.title = "Groups",
    legend.labs = c("Chronic Bronchitis", "Emphysema", "Emphysema/Chronic Bronchitis"),
    ggtheme = theme_bw()
  )

# tiff("figs3_km3year_COPD.tif", width = 12, height = 9, units = "in", res = 300, compression = "lzw")
# 
# km_plot_death3year_subgroup_a
# dev.off()

pairwise_survdiff(Surv(TIME_DEATH_CENSOR_3YR/365.25, DEATH_3YR) ~ COPD_SUBCAT, data = GNSIS3_SUB3)

# Comparison within COPD groups
coxmodel_death3year_subgroup_a_within_COPD <- coxph(Surv(TIME_DEATH_CENSOR_3YR/365.25, DEATH_3YR) ~ 
                                                      COPD_SUBCAT +
                                                      PT_SEX +
                                                      AGE_AT_INDEX +
                                                      HYPERTENSION_AT_INDEX +
                                                      MI_AT_INDEX +
                                                      PERI_VASC_DIS_AT_INDEX +
                                                      strata(DIABETES_AT_INDEX, 
                                                             DYSLIPIDEMIA_AT_INDEX, CHF_AT_INDEX,
                                                             ATRIAL_FIB_AT_INDEX), data = GNSIS3_SUB3)

cox.zph(coxmodel_death3year_subgroup_a_within_COPD)

coxmodel_death3year_subgroup_a_within_COPD_results <- broom::tidy(coxmodel_death3year_subgroup_a_within_COPD, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(conf_range = paste(conf.low, conf.high, sep = " - ")) %>%
  select(term, estimate, conf_range, p.value)
knitr::kable(coxmodel_death3year_subgroup_a_within_COPD_results)

# ### Subgroup based on GOLD subgroup

GNSIS3_SUB4 <- GNSIS3 %>% 
  filter(!is.na(CASE_CONTROL2))

table(GNSIS3_SUB4$CASE_CONTROL2)

km_plot_death3year_subgroup_gold <-
  ggsurvplot(
    survfit(Surv(TIME_DEATH_CENSOR_3YR/365.25, DEATH_3YR) ~ FEV1_PCT_PRED_cat, data = GNSIS3_SUB4[GNSIS3_SUB4$FEV1_PCT_PRED_cat != "0",]),
    conf.int = TRUE,
    conf.int.alpha = 0.25,
    censor = TRUE,
    censor.shape = "|",
    censor.size = 2,
    surv.scale = "percent",
    pval = TRUE,
    pval.coord = c(0,0.55),
    pval.size = 4,
    ylim = c(0.2, 1),
    xlim = c(0, 3),
    risk.table = TRUE,
    xlab = "Time in years",
    legend.title = "Groups",
    legend.labs = c("GOLD 1", "GOLD 2", "GOLD 3", "GOLD 4"),
    ggtheme = theme_bw()
  )

# tiff("figs4_km3year_GOLD.tif", width = 12, height = 9, units = "in", res = 300, compression = "lzw")
# 
# km_plot_death3year_subgroup_gold
# dev.off()

# 


# Comparison between gold groups

# 
coxmodel_death3year_subgroup_gold_withinCOPD <- coxph(Surv(TIME_DEATH_CENSOR_3YR/365.25, DEATH_3YR) ~ 
                                                        FEV1_PCT_PRED_cat +
                                                        PT_SEX +
                                                        AGE_AT_INDEX +
                                                        HYPERTENSION_AT_INDEX +
                                                        MI_AT_INDEX +
                                                        PERI_VASC_DIS_AT_INDEX +
                                                        CHF_AT_INDEX +
                                                        ATRIAL_FIB_AT_INDEX +
                                                        strata(DIABETES_AT_INDEX,
                                                               DYSLIPIDEMIA_AT_INDEX), data = GNSIS3_SUB4[GNSIS3_SUB4$FEV1_PCT_PRED_cat != "0",])

cox.zph(coxmodel_death3year_subgroup_gold_withinCOPD)

coxmodel_death3year_subgroup_gold_withinCOPD_results <- broom::tidy(coxmodel_death3year_subgroup_gold_withinCOPD, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(conf_range = paste(conf.low, conf.high, sep = " - ")) %>%
  select(term, estimate, conf_range, p.value)
knitr::kable(coxmodel_death3year_subgroup_gold_withinCOPD_results)
