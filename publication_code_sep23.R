##############################
# Project: Family and Child Wellbeing During the COVID-19 Pandemic: A Family Systems Perspective
# Date: 8 Sep 2023
# Purpose: Code for analyses of the main manuscript
#############################
# Preamble
##########
packages <- c('gridExtra', 'kableExtra', 'tidyverse',
              'lavaan', 'mice', 'readr', #'DataExplorer' 
              'data.table', # 'semTools', 
              'tidyr', 'reshape2', 'psych')
lapply(packages, require, character.only = TRUE)

#####################
# Data preparation
#####################
# load unidentified data
########################
dat <- read_csv("code/fsm_analysis/Final Sep 23/fsm_raw_deid_sep23.csv") %>%
  mutate(ECONHard_w1 = Q15_2_w1, 
         ECONHard_w2 = Q15_2_w2,
         covid_risk_w1 = rowMeans(.[,c('Q15_1_w1','Q15_3_w1')], na.rm = TRUE),
         covid_risk_w2 = rowMeans(.[,c('Q15_1_w2','Q15_3_w2')], na.rm = TRUE),
         )

# create vectors of relevant variable names to facilitate the analysis
cov <- c('age_parent_imp', 'uae_national','ba_higher_education')
c_wellbeing_vars <- c('c_life_quality', 'c_physical_health', 'c_mental_health', 'c_social_act', 'c_sleep_quality')
c_wellbeing_vars_w1 <- sapply(c_wellbeing_vars, function(x) {paste(x, '_w1', sep = '')}, USE.NAMES = F)
c_wellbeing_vars_w2 <- sapply(c_wellbeing_vars, function(x) {paste(x, '_w2', sep = '')}, USE.NAMES = F)
c_cov_imp_relation_vars_w1 <- c('c_cov_imp_relation_w1', 'c_cov_imp_qtime_w1') 
c_cov_imp_relation_vars_w2 <- c('c_cov_imp_relation_w2', 'c_cov_imp_qtime_w2') 


# keep only the variables that will be used for the analysis
df <- dat %>% dplyr::select(cov, hhid, ECONHard_w1, ECONHard_w2, covid_risk_w1, covid_risk_w2, jobloss_resp_w1, jobloss_resp_w2,
                      Q44a_1_w1, Q44a_2_w1, Q44a_3_w1, Q44a_4_w1, Q44b_5_w1, Q44b_7_w1, Q44a_8_w1, Q44b_9_w1,
                      Q44a_1_w2, Q44a_2_w2, Q44a_3_w2, Q44a_4_w2, Q44b_5_w2, Q44b_7_w2, Q44a_8_w2, Q44b_9_w2,
                      Q19_1_w1, Q19_2_w1, Q19_3_w1, Q19_4_w1,
                      Q19_1_w2, Q19_2_w2, Q19_3_w2, Q19_4_w2,
                      Q24_1_w1, Q24_2_w1, Q24_3_w1,
                      Q24_1_w2, Q24_2_w2, Q24_3_w2,
                      c_life_quality_w1:c_sleep_quality_w2,
                      c_qual_time_sat_w1, c_qual_time_sat_w2,
                      child, c_Age, c_Sex,
                      c_cov_imp_relation_w1, c_cov_imp_relation_w2, 
                      c_cov_imp_qtime_w1, c_cov_imp_qtime_w2,
                      c_outdoor_act_w1, c_outdoor_act_w2, c_screen_time_w1, c_screen_time_w2,
                      hhid, region_ad,
                      c_qual_time_sat_w1, c_qual_time_sat_w2) %>%
  mutate_all(as.numeric) %>%
  mutate(hhid = as.character(hhid)) %>%
  dplyr::filter(!(is.na(c_Age) & child == 2), !(rowSums(is.na(.[,c_wellbeing_vars_w1])) == 5)) %>%
  arrange(child)

# missing data exploration
#####################
md.pattern(df)

# multiple imputation
#####################
# impute missing values using predictive mean match (pmm) algorithm
df_imp <- mice(df,m=1,maxit=20,
     meth='pmm',seed=500)
 densityplot(df_imp)
df_temp <- complete(df_imp, 1)

md.pattern(df_temp)
sum(is.na(df_temp))

wide <- df_temp %>% 
  # dplyr::select(c_Age, c_Sex, child, c_life_quality_w1:c_sleep_quality_w2) %>%
  mutate(c_wellbeing_w1 = rowMeans(.[,c_wellbeing_vars_w1], na.rm = TRUE),
         c_wellbeing_w2 = rowMeans(.[,c_wellbeing_vars_w2], na.rm = TRUE),
         # na_c_wellbeing_w1 = rowSums(is.na(.[,c_wellbeing_vars_w1])),
         # na_c_wellbeing_w2 = rowSums(is.na(.[,c_wellbeing_vars_w2])),
         # att_c_wellbeing_w1 = ifelse(rowSums(is.na(.[,c_wellbeing_vars_w1])) == 5, 1, 0),
         # att_c_wellbeing_w2 = ifelse(rowSums(is.na(.[,c_wellbeing_vars_w2])) == 5, 1, 0),
         # na_cov_imp_relation_w1 = rowSums(is.na(.[,c(c_cov_imp_relation_vars_w1, c_wellbeing_vars_w1)])),
         # na_cov_imp_relation_w2 = rowSums(is.na(.[,c(c_cov_imp_relation_vars_w2, c_wellbeing_vars_w2)])),
         # child_wb_w1 = rowMeans(.[,c_wellbeing_vars_w1], na.rm = TRUE),
         # child_wb_w2 = rowMeans(.[,c_wellbeing_vars_w2], na.rm = TRUE),
         pat_stress_w1 = rowMeans(.[c('Q19_1_w1', 'Q19_2_w1', 'Q19_3_w1', 'Q19_4_w1')], na.rm = TRUE),
         int_par_relat_w1 = rowMeans(.[c('Q24_1_w1', 'Q24_2_w1', 'Q24_3_w1')], na.rm = TRUE),
         # relat_child_w1 = rowMeans(.[c('c_cov_imp_relation_w1', 'c_cov_imp_qtime_w1')], na.rm = TRUE),
         pat_stress_w2 = rowMeans(.[c('Q19_1_w2', 'Q19_2_w2', 'Q19_3_w2', 'Q19_4_w2')], na.rm = TRUE),
         int_par_relat_w2 = rowMeans(.[c('Q24_1_w2', 'Q24_2_w2', 'Q24_3_w2')], na.rm = TRUE),
         # relat_child_w2 = rowMeans(.[c('c_cov_imp_relation_w2', 'c_cov_imp_qtime_w2')], na.rm = TRUE),
         child2 = ifelse(child == 2, 1, 0)
  )


# Select measures that will be used for the assessment
fit.indices <- c("cfi", "rmsea", "srmr")

###################################
# Confirmatory Factor Analysis (CFA)
###################################
# 1- Child wellbeing
#####################
# Wave 1
cfa_cwb1 <- '
ChildWB =~  c_sleep_quality_w1 + c_mental_health_w1 + c_physical_health_w1  + c_life_quality_w1 + c_social_act_w1

c_physical_health_w1 ~~    c_life_quality_w1
c_sleep_quality_w1 ~~    c_life_quality_w1
c_sleep_quality_w1 ~~ c_physical_health_w1
c_social_act_w1 ~~    c_life_quality_w1
'
fit_cfa_cwb1 <- cfa(cfa_cwb1, data = wide, estimator = "ML")
fitMeasures(fit_cfa_cwb1, fit.indices)
summary(fit_cfa_cwb1, fit.measures=TRUE, standardized=TRUE)
inspect(fit_cfa_cwb1,what="std")
alpha(wide[, c('c_sleep_quality_w1', 'c_mental_health_w1', 'c_physical_health_w1', 'c_life_quality_w1', 'c_social_act_w1')]) # Cronbach's alpha value

# Wave 2
cfa_cwb2 <- '
# ChildWB =~  c_sleep_quality + c_social_act + c_physical_health  + c_mental_health + c_life_quality
# ChildWB =~  c_sleep_quality + c_life_quality + c_physical_health # + c_mental_health + c_social_act
# ChildWB =~  c_sleep_quality + c_mental_health + c_physical_health  + c_social_act # + c_life_quality

ChildWB =~  c_sleep_quality_w2 + c_mental_health_w2 + c_physical_health_w2  + c_life_quality_w2 + c_social_act_w2

c_physical_health_w2 ~~    c_life_quality_w2
c_sleep_quality_w2 ~~    c_life_quality_w2
# c_social_act ~~ c_physical_health
c_sleep_quality_w2 ~~ c_physical_health_w2
c_social_act_w2 ~~    c_life_quality_w2
'
fit_cfa_cwb2 <- cfa(cfa_cwb2, data = wide, estimator = "ML")
fitMeasures(fit_cfa_cwb2, fit.indices)
summary(fit_cfa_cwb2, fit.measures=TRUE, standardized=TRUE)
inspect(fit_cfa_cwb2,what="std")
alpha(wide[, c('c_sleep_quality_w2', 'c_mental_health_w2', 'c_physical_health_w2', 'c_life_quality_w2', 'c_social_act_w2')]) # Cronbach's alpha value

# 2- Parent stress
#####################
# Wave 1
cfa_ps1 <- '
  PatStress =~ Q19_1_w1 + Q19_2_w1 + Q19_3_w1 + Q19_4_w1       # parenting stress

Q19_3_w1 ~~ Q19_4_w1
'
fit_cfa_ps1 <- cfa(cfa_ps1, data = wide, estimator = "ML")
fitMeasures(fit_cfa_ps1, fit.indices)
inspect(fit_cfa_ps1,what="std")
alpha(wide[, c('Q19_1_w1', 'Q19_2_w1', 'Q19_3_w1', 'Q19_4_w1')]) # Cronbach's alpha value

# Wave 2
cfa_ps2 <- '
  PatStress =~ Q19_1_w2 + Q19_2_w2 + Q19_3_w2 + Q19_4_w2       # parenting stress

Q19_3_w2 ~~ Q19_4_w2
'
fit_cfa_ps2 <- cfa(cfa_ps2, data = wide, estimator = "ML")
fitMeasures(fit_cfa_ps2, fit.indices)
inspect(fit_cfa_ps2,what="std")
alpha(wide[, c('Q19_1_w2', 'Q19_2_w2', 'Q19_3_w2', 'Q19_4_w2')]) # Cronbach's alpha value

# 3- Inter parental relationship problems
#####################
# Wave 1
cfa_pb1 <- '
  ParRelat =~ Q24_1_w1 + Q24_2_w1 + Q24_3_w1
'
fit_cfa_pb1 <- cfa(cfa_pb1, data = wide, estimator = "ML")
fitMeasures(fit_cfa_pb1, fit.indices)
inspect(fit_cfa_pb1,what="std")
alpha(wide[, c('Q24_1_w1', 'Q24_2_w1', 'Q24_3_w1')]) # Cronbach's alpha value

# Wave 2
cfa_pb2 <- '
  ParRelat =~ Q24_1_w2 + Q24_2_w2 + Q24_3_w2
'
# Configural invariance
fit_cfa_pb2 <- cfa(cfa_pb2, data = wide, estimator = "ML")
fitMeasures(fit_cfa_pb2, fit.indices)
inspect(fit_cfa_pb2,what="std")
alpha(wide[, c('Q24_1_w2', 'Q24_2_w2', 'Q24_3_w2')]) # Cronbach's alpha value


################################### 
# Measurement invariance across time
################################### 
# First do some data preparation
# Keep only data used for the measurement invariance analysis
wide_mi <- wide %>% 
  dplyr::select(Q44a_1_w1, Q44a_2_w1, Q44a_3_w1, Q44a_4_w1, Q44b_5_w1, Q44b_7_w1, Q44a_8_w1, Q44b_9_w1,
                Q44a_1_w2, Q44a_2_w2, Q44a_3_w2, Q44a_4_w2, Q44b_5_w2, Q44b_7_w2, Q44a_8_w2, Q44b_9_w2,
                Q19_1_w1, Q19_2_w1, Q19_3_w1, Q19_4_w1,
                Q19_1_w2, Q19_2_w2, Q19_3_w2, Q19_4_w2,
                Q24_1_w1, Q24_2_w1, Q24_3_w1,
                Q24_1_w2, Q24_2_w2, Q24_3_w2,
                c_life_quality_w1:c_sleep_quality_w2,
                hhid, child,
                # relat_child_w1, relat_child_w2,
                c_Sex, c_Age,)

# Reshape data from wide to long format
long_mi <- data.table::melt(setDT(wide_mi), measure.vars=list(c(1,9), c(2,10), c(3,11), c(4,12), c(5,13), c(6,14), c(7,15), c(8,16),
                                                              c(17,21), c(18,22), c(19,23), c(20,24),
                                                              c(25,28), c(26,29), c(27, 30),
                                                              c(31,32), c(33,34), c(35,36), c(37,38), c(39,40)), 
                            variable.name='wave', value.name=c('Q44a_1', 'Q44a_2', 'Q44a_3', 'Q44a_4', 'Q44a_5', 'Q44a_7', 'Q44a_8', 'Q44a_9',
                                                               'Q19_1', 'Q19_2', 'Q19_3', 'Q19_4', 
                                                               'Q24_1', 'Q24_2', 'Q24_3',
                                                               'c_life_quality', 'c_physical_health', 'c_mental_health', 'c_social_act', 'c_sleep_quality'#,
                                                               # 'c_relat_child'
                                                               ))[ 
                                                                 order(hhid)]
# Will check measurement invariance for each LV separately
# 1.a- Child wellbeing
#####################
mi_cwb <- '
ChildWB =~  c_sleep_quality + c_social_act + c_physical_health  + c_mental_health + c_life_quality

c_physical_health ~~    c_life_quality
c_sleep_quality ~~    c_life_quality
c_social_act ~~ c_physical_health
c_sleep_quality ~~ c_physical_health
c_social_act ~~    c_life_quality
'

# Configural invariance
config_cwb <- cfa(mi_cwb, data = long_mi, meanstructure=TRUE, group = "wave", estimator = "ML")
fitMeasures(config_cwb, fit.indices)

# Weak invariance
weak_cwb <- cfa(mi_cwb, data = long_mi, group = "wave", group.equal = c("loadings"), estimator = "ML")
fitMeasures(weak_cwb, fit.indices)
compareFit(config_cwb, weak_cwb) %>% summary()

# Strong/scalar invariance
strong_cwb <- cfa(mi_cwb, data = long_mi, estimator = "ML", group = "wave", group.equal = c("loadings","intercepts"))
fitMeasures(strong_cwb, fit.indices)
compareFit(weak_cwb, strong_cwb) %>% summary()

# 1.b- Child wellbeing -edited
#####################
mi_cwb <- '
ChildWB =~  c_sleep_quality + c_mental_health + c_physical_health  + c_life_quality # + c_social_act

c_physical_health ~~    c_life_quality
c_sleep_quality ~~    c_life_quality
'

# Configural invariance
config_cwb <- cfa(mi_cwb, data = long_mi, meanstructure=TRUE, group = "wave", estimator = "ML")
fitMeasures(config_cwb, fit.indices)
modindices(config_cwb, sort.=TRUE, minimum.value=10)

# Weak invariance
weak_cwb <- cfa(mi_cwb, data = long_mi, group = "wave", group.equal = c("loadings"), estimator = "ML")
fitMeasures(weak_cwb, fit.indices)
compareFit(config_cwb, weak_cwb) %>% summary()

# Strong/scalar invariance
strong_cwb <- cfa(mi_cwb, data = long_mi, estimator = "ML", group = "wave", group.equal = c("loadings","intercepts"))
fitMeasures(strong_cwb, fit.indices)
compareFit(weak_cwb, strong_cwb) %>% summary()


# 2- Parent stress
#####################
mi_ps <- '
  PatStress =~ Q19_1 + Q19_2 + Q19_3 + Q19_4       # parenting stress

Q19_3 ~~ Q19_4
'
# Configural invariance
config_ps <- cfa(mi_ps, data = long_mi, group = "wave", estimator = "ML")
fitMeasures(config_ps, fit.indices)

# Weak invariance
weak_ps <- cfa(mi_ps, data = long_mi, group = "wave", group.equal = c("loadings"), estimator = "ML")
fitMeasures(weak_ps, fit.indices)
compareFit(config_ps, weak_ps) %>% summary()

# Strong invariance
strong_ps <- cfa(mi_ps, data = long_mi, estimator = "ML", group = "wave", 
                 group.equal = c("loadings","intercepts"))
fitMeasures(strong_ps, fit.indices)
compareFit(weak_ps, strong_ps) %>% summary()


# 3- Inter parental relationship problems
#####################
mi_pb <- '
  ParRelat =~ Q24_1 + Q24_2 + Q24_3        # interparental Relationship problems 
'
# Configural invariance
config_pb <- cfa(mi_pb, data = long_mi, group = "wave", estimator = "ML")
fitMeasures(config_pb, fit.indices)

# Weak invariance
weak_pb <- cfa(mi_pb, data = long_mi, group = "wave", group.equal = c("loadings"), estimator = "ML")
fitMeasures(weak_pb, fit.indices)
compareFit(config_pb, weak_pb) %>% summary()

# Strong invariance
strong_pb <- cfa(mi_pb, data = long_mi, estimator = "ML", group = "wave", 
                 group.equal = c("loadings","intercepts"))
fitMeasures(strong_pb, fit.indices)
compareFit(strong_pb, weak_pb) %>% summary()


# 4- Combined model with 
# all 3 latent constructs
#########################
mi_com <- '
  ParRelat =~ Q24_1 + Q24_2 + Q24_3         # interparental Relationship problems 
  PatStress =~ Q19_1 + Q19_2 + Q19_3 + Q19_4       # parenting stress
  ChildWB =~  c_sleep_quality + c_mental_health + c_physical_health  + c_life_quality     # child wellbeing

c_physical_health ~~ c_life_quality
Q19_3 ~~ Q19_4
'
# Configural invariance
config_com <- cfa(mi_com, data = long_mi, group = "wave", estimator = "ML")
fitMeasures(config_com, fit.indices)

# Weak invariance
weak_com <- cfa(mi_com, data = long_mi, group = "wave", group.equal = c("loadings"), estimator = "ML")
fitMeasures(weak_com, fit.indices)
compareFit(config_com, weak_com) %>% summary()

# Strong invariance
strong_com <- cfa(mi_com, data = long_mi, estimator = "ML", group = "wave", 
                  group.equal = c("loadings","intercepts"))
fitMeasures(strong_com, fit.indices)
compareFit(strong_com, weak_com) %>% summary()

####################################
# Measurement invariance across the
# 2 children in the same family
####################################
wide_w1 <- wide %>%
  mutate(child_str = paste('c', .$child, sep = '')) %>%
  mutate(s_ECONHard_w1 = scale(ECONHard_w1, center = FALSE),
         s_covid_risk_w1 = scale(covid_risk_w1, center = FALSE),
         s_c_Age = scale(c_Age, center = FALSE),
         s_age_parent_imp = scale(age_parent_imp, center = FALSE),
         s_c_screen_time_w1 = scale(c_screen_time_w1, center = FALSE),
         s_c_outdoor_act_w1 = scale(c_outdoor_act_w1, center = FALSE),
         s_c_outdoor_act_w2 = scale(c_outdoor_act_w2, center = FALSE)) %>%
  dplyr::filter(c_Age < 97)
table(wide_w1$child_str)


c_wide_w1 <- pivot_wider(wide_w1, 
                         id_cols = c('age_parent_imp', 'uae_national', 'ba_higher_education', 'hhid', 'Q19_1_w1', 'Q19_2_w1', 'Q19_3_w1', 'Q19_4_w1', 
                                     'Q24_1_w1', 'Q24_2_w1', 'Q24_3_w1',
                                     's_age_parent_imp', 's_ECONHard_w1', 's_covid_risk_w1', 'jobloss_resp_w1', 'uae_national', 'ba_higher_education'),
                         names_from = child_str,
                         values_from = c('c_Sex', 'c_Age', 's_c_Age',
                                         'c_life_quality_w1', 'c_physical_health_w1', 'c_mental_health_w1', 'c_social_act_w1', 'c_sleep_quality_w1',
                                         # 'relat_child_w1',
                                         's_c_screen_time_w1', 's_c_outdoor_act_w1', 's_c_outdoor_act_w2', 'c_outdoor_act_w1',
                                         'c_qual_time_sat_w1')) %>%
  dplyr::filter(!is.na(c_Sex_c2), !is.na(c_Age_c2), !is.na(c_Age_c1), c_Age_c2 < 97)

dim(c_wide_w1)

temp1 <- wide_w1 %>% dplyr::filter(child2 == 1) %>% dplyr::select(-c(c_life_quality_w1:c_sleep_quality_w1, child))
c_long_w1 <- inner_join(temp1, wide_w1 %>% dplyr::select(c_life_quality_w1:c_sleep_quality_w1, hhid, child), by = 'hhid')
table(c_long_w1$child)

# 1- Child wellbeing
#####################
mi_cwb_c <- '
ChildWB =~  c_sleep_quality_w1 + c_mental_health_w1 + c_physical_health_w1  + c_life_quality_w1 

c_physical_health_w1 ~~    c_life_quality_w1
'

# Configural invariance
config_cwb_c <- cfa(mi_cwb_c, data = c_long_w1, meanstructure=TRUE, group = "child", estimator = "ML")
fitMeasures(config_cwb_c, fit.indices)

# Weak invariance
weak_cwb_c <- cfa(mi_cwb_c, data = c_long_w1, group = "child", group.equal = c("loadings"), estimator = "ML")
fitMeasures(weak_cwb_c, fit.indices)
compareFit(config_cwb_c, weak_cwb_c) %>% summary()

# Strong invariance
strong_cwb_c <- cfa(mi_cwb_c, data = c_long_w1, estimator = "ML", group = "child", group.equal = c("loadings","intercepts"))
fitMeasures(strong_cwb_c, fit.indices)
compareFit(weak_cwb_c, strong_cwb_c) %>% summary()

#####################
#####################
# SEM with combined model
#####################
#####################
# Do some data preparation first
# Scale control varibales
wide_c1 <- wide %>% dplyr::filter(child == 1) %>%
  mutate(s_ECONHard_w1 = scale(ECONHard_w1, center = FALSE),
         s_ECONHard_w2 = scale(ECONHard_w2, center = FALSE),
         s_covid_risk_w1 = scale(covid_risk_w1, center = FALSE),
         s_covid_risk_w2 = scale(covid_risk_w2, center = FALSE),
         s_c_Age = scale(c_Age, center = FALSE),
         s_age_parent_imp = scale(age_parent_imp, center = FALSE),
         s_c_screen_time_w1 = scale(c_screen_time_w1, center = FALSE),
         s_c_screen_time_w2 = scale(c_screen_time_w2, center = FALSE),
  )

####################
# Analysis 1: 
# Wave comparison
####################
# Specifying the model
wsem_7 <- '
# Wave1:
  ChildWB_w1 =~  c_physical_health_w1 + c_mental_health_w1 + c_sleep_quality_w1 + c_life_quality_w1 # child wellbeing
  PatStress_w1 =~ Q19_1_w1 + Q19_2_w1 + Q19_3_w1 + Q19_4_w1       # parenting stress
  ParRelat_w1 =~ Q24_1_w1 + Q24_2_w1 + Q24_3_w1        # interparental Relationship problems 
  
  c_physical_health_w1 ~~    c_life_quality_w1
  Q19_3_w1 ~~ Q19_4_w1

# Wave2:
  ChildWB_w2 =~ c_physical_health_w2 + c_mental_health_w2 + c_sleep_quality_w2 + c_life_quality_w2
  PatStress_w2 =~ Q19_1_w2 + Q19_2_w2 + Q19_3_w2 + Q19_4_w2       # parenting stress
  ParRelat_w2 =~ Q24_1_w2 + Q24_2_w2 + Q24_3_w2                   # interparental Relationship problems 

  c_physical_health_w2 ~~    c_life_quality_w2
  Q19_3_w2 ~~ Q19_4_w2

  c_physical_health_w1 ~~ c_physical_health_w2 
  c_life_quality_w1 ~~ c_life_quality_w2 
  c_sleep_quality_w1 ~~ c_sleep_quality_w2
  c_mental_health_w1 ~~ c_mental_health_w2
  Q19_1_w1 ~~ Q19_1_w2
  Q19_2_w1 ~~ Q19_2_w2
  Q19_3_w1 ~~ Q19_3_w2
  Q19_4_w1 ~~ Q19_4_w2
  Q24_1_w1	~~	Q24_1_w2
  Q24_2_w1	~~	Q24_2_w2
  Q24_3_w1	~~	Q24_3_w2

# Time Lagged Effects
# Each variable predicting itself
  s_ECONHard_w2 ~ s_ECONHard_w1
  s_covid_risk_w2 ~ s_covid_risk_w1
  jobloss_resp_w2 ~ jobloss_resp_w1
  
# Specifying structural model portion of FSM for both waves
  PatStress_w1 ~ uae_national + ba_higher_education + s_age_parent_imp + s_ECONHard_w1 + c*s_covid_risk_w1 + jobloss_resp_w1
  PatStress_w2 ~ uae_national + ba_higher_education + s_age_parent_imp + s_ECONHard_w2 + s_covid_risk_w2 + jobloss_resp_w2 + s_ECONHard_w1 + s_covid_risk_w1 + jobloss_resp_w1 + PatStress_w1
  
  c_qual_time_sat_w1 ~  uae_national + ba_higher_education + s_c_Age + c_Sex  + g1*PatStress_w1 
  c_qual_time_sat_w2 ~  uae_national + ba_higher_education + s_c_Age + c_Sex  + c11*PatStress_w1 + c_qual_time_sat_w1 + lb1*PatStress_w2
  
  ParRelat_w1 ~  uae_national + ba_higher_education + s_age_parent_imp + b1*PatStress_w1
  ParRelat_w2 ~  uae_national + ba_higher_education + s_age_parent_imp + ipr*PatStress_w2 + b1*PatStress_w1 + ParRelat_w1
  
  ChildWB_w1 ~  uae_national + ba_higher_education  + s_c_Age + c_Sex + c_outdoor_act_w1  + g2*c_qual_time_sat_w1 + b2*ParRelat_w1
  ChildWB_w2 ~  uae_national + ba_higher_education  + s_c_Age + c_Sex + c_outdoor_act_w1  + c12*c_qual_time_sat_w2  + p2*ParRelat_w2 + r1*c_qual_time_sat_w1 + b12*ParRelat_w1 + ChildWB_w1

# Covariances
  s_ECONHard_w2 ~~ s_covid_risk_w2 
  s_ECONHard_w2 ~~ jobloss_resp_w2
  s_ECONHard_w1 ~~ s_covid_risk_w1 
  s_ECONHard_w1 ~~  jobloss_resp_w1
  s_covid_risk_w1 ~~ jobloss_resp_w1

  ParRelat_w1 ~~ c_qual_time_sat_w1 # CALL
  ParRelat_w2 ~~ c_qual_time_sat_w2 # CALL
  
# Direct effects
  ChildWB_w1 ~ s1c1*PatStress_w1
  ChildWB_w2 ~ s1c2*PatStress_w1
  ChildWB_w2 ~ s2c2*PatStress_w2
  
# Indirect effects
  A1 := b1*b2 # PS1->IPRP1->CWB1 
  A2 := ipr*p2 # PS2->IPRP2->CWB2 
  A3 := g1*g2 # PS1->PCR1->CWB1 
  A4 := lb1*c12 # PS2->PCR2->CWB2 
 
  B1 := b1*b12 # PS1->IPRP1->CWB2
  B2 := b1*p2 # PS1->IPRP2->CWB2
  B3 := g1*r1 # PS1->PCR1->CWB2
  B4 := c11*c12 # PS1->PCR2->CWB2
'
# Fitting the model
wsem_7_fit <- sem(wsem_7, data = wide_c1, estimator = "MLM")
summary(wsem_7_fit)
fitMeasures(wsem_7_fit, fit.indices)
summary(wsem_7_fit, fit.measures=TRUE, standardized=TRUE,  rsquare=T)  %>% .$PE %>%
  kable("html", digits = 2, caption = "") %>%
  kable_styling(bootstrap_options=c("striped", "hover", "condensed", "responsive"),
                full_width=FALSE)
modindices(wsem_7_fit, sort.=TRUE, minimum.value=50)
parameterEstimates(wsem_7_fit, 
                   se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE, 
                   standardized = TRUE, rsquare=T)  %>%
  kable("html", digits = 2, caption = "") %>%
  kable_styling(bootstrap_options=c("striped", "hover", "condensed", "responsive"),
                full_width=FALSE)




####################
# Analysis 2: 
# Child comparison
####################
# Specifying the model
# csem_20
csem_7 <-'
# specifying measurement model portion
  ChildWB_w1_c1 =~  c_physical_health_w1_c1 + c_mental_health_w1_c1 + c_sleep_quality_w1_c1 + c_life_quality_w1_c1
  ChildWB_w1_c2 =~ c_physical_health_w1_c2 + c_mental_health_w1_c2 + c_sleep_quality_w1_c2 + c_life_quality_w1_c2
  
  c_physical_health_w1_c1 ~~    c_life_quality_w1_c1
  c_physical_health_w1_c2 ~~    c_life_quality_w1_c2

  PatStress_w1 =~  Q19_1_w1 + Q19_2_w1 + Q19_3_w1 + Q19_4_w1       # parenting stress
  ParRelat_w1 =~  Q24_1_w1 + Q24_2_w1 + Q24_3_w1         # interparental Relationship problems 

# correlating uniquenesses of same-scale items
  # both children
  c_physical_health_w1_c1 ~~ c_physical_health_w1_c2 # SUGGESTED BY MI
  c_sleep_quality_w1_c1 ~~   c_sleep_quality_w1_c2 # SUGGESTED BY MI
  c_mental_health_w1_c1 ~~      c_mental_health_w1_c2 # SUGGESTED BY MI
  c_life_quality_w1_c1 ~~ c_life_quality_w1_c2

  # parent 
  Q19_3_w1 ~~ Q19_4_w1

# regressions
  PatStress_w1 ~ uae_national + ba_higher_education + s_age_parent_imp + s_ECONHard_w1 + c*s_covid_risk_w1 + jobloss_resp_w1
  ParRelat_w1 ~  uae_national + ba_higher_education + s_age_parent_imp + m*PatStress_w1 
  c_qual_time_sat_w1_c1 ~ uae_national + ba_higher_education + s_age_parent_imp + c_Sex_c1   + b1*PatStress_w1
  c_qual_time_sat_w1_c2 ~ uae_national + ba_higher_education + s_age_parent_imp + c_Sex_c2  + g1*PatStress_w1
  ChildWB_w1_c1 ~ uae_national + ba_higher_education + c_Sex_c1 + s_c_Age_c1 + c_outdoor_act_w1_c1  + b2*c_qual_time_sat_w1_c1 + y1*ParRelat_w1
  ChildWB_w1_c2 ~ uae_national + ba_higher_education + c_Sex_c2  + s_c_Age_c2 + c_outdoor_act_w1_c2  + g2*c_qual_time_sat_w1_c2 + p1*ParRelat_w1 

# Covariances
  jobloss_resp_w1 ~~ s_ECONHard_w1
  jobloss_resp_w1 ~~ s_covid_risk_w1
  s_ECONHard_w1 ~~ s_covid_risk_w1
  ChildWB_w1_c1 ~~ ChildWB_w1_c2
  c_qual_time_sat_w1_c1 ~~ c_qual_time_sat_w1_c2 
  
# Direct effects of parent stress on child wellbeing
 ChildWB_w1_c1 ~ sc1*PatStress_w1
 ChildWB_w1_c2 ~ sc2*PatStress_w1

# Mediation chains
  A1 := m*y1 # PS->IPRP->CWB1
  A2 := m*p1 # PS->IPRP->CWB2 
  B1 := b1*b2 # PS->PCR1->CWB1
  B2 := g1*g2 # PS->PCR2->CWB2
  '
# Fitting the model
fit_csem_7 <- sem(csem_7, data = c_wide_w1, meanstructure=TRUE, std.lv=TRUE, estimator = "MLR")
fitmeasures(fit_csem_7, fit.indices) 

summary(fit_csem_7, fit.measures=TRUE, standardized=TRUE, rsquare=T) %>% .$PE %>%
  kable("html", digits = 2, caption = "") %>%
  kable_styling(bootstrap_options=c("striped", "hover", "condensed", "responsive"),
                full_width=FALSE) 

modindices(fit_csem_7, sort.=TRUE, minimum.value=10)
parameterEstimates(fit_csem_7, 
                   se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE, 
                   standardized = TRUE, rsquare=T)  %>%
  kable("html", digits = 2, caption = "") %>%
  kable_styling(bootstrap_options=c("striped", "hover", "condensed", "responsive"),
                full_width=FALSE)




#####
# Side exploration: Explore c_qual_time_sat var
####

hist(c_wide_w1$c_qual_time_sat_w1_c1)
hist(c_wide_w1$c_qual_time_sat_w1_c2)
hist(c_wide_w1$c_qual_time_sat_w1_c2)
hist(wide_c1$c_qual_time_sat_w1)
hist(wide_c1$c_qual_time_sat_w2)

wide %>% 
  ggplot(aes(c_qual_time_sat_w1, group=as.character(child)))+
  geom_histogram(aes(y=..count../sum(..count..))) 
  # geom_histogram()+

wide %>% dplyr::filter(child == 1) %>%
  ggplot(aes((c_qual_time_sat_w1)))+
  geom_bar(aes(y=..count../sum(..count..)))+
  ggtitle('Child 1 at wave 1')
wide %>% dplyr::filter(child == 2) %>%
  ggplot(aes(c_qual_time_sat_w1))+
  geom_bar(aes(y=..count../sum(..count..)))+
  ggtitle('Child 2 at wave 1')
wide <- wide %>%
  mutate(sd_c_qual_time_sat_w1 = (wide$c_qual_time_sat_w1 - mean(wide$c_qual_time_sat_w1))/sd(wide$c_qual_time_sat_w1))

wide %>% dplyr::filter(child == 1) %>%
  ggplot(aes((sd_c_qual_time_sat_w1)))+
  geom_bar(aes(y=..count../sum(..count..)))+
  ggtitle('Child 1 at wave 1')
wide %>% dplyr::filter(child == 2) %>%
  ggplot(aes(sd_c_qual_time_sat_w1))+
  geom_bar(aes(y=..count../sum(..count..)))+
  ggtitle('Child 2 at wave 1')

wide %>% dplyr::filter(child == 1) %>%
  ggplot(aes(c_qual_time_sat_w1))+
  geom_bar(aes(y=..count../sum(..count..)))+
  ggtitle('Child 1 at wave 1')
wide %>% dplyr::filter(child == 1) %>%
  ggplot(aes(c_qual_time_sat_w2))+
  geom_bar(aes(y=..count../sum(..count..)))+
  ggtitle('Child 1 at wave 2')

qqnorm(wide_c1$c_qual_time_sat_w1, main='Normal')
qqline(wide_c1$c_qual_time_sat_w1)
shapiro.test(wide_c1$c_qual_time_sat_w1)
ks.test(wide_c1$c_qual_time_sat_w1, 'pnorm')



