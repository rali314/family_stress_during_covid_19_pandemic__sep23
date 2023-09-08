##############################
# Project: Family and Child Wellbeing During the COVID-19 Pandemic: A Family Systems Perspective
# Date: 8 Sep 2023
# Purpose: Code for analyses of the supplemental material of the manuscript
#############################
#############################################
# Compare Analysis 1 and Analysis 2 samples
#############################################
# To see if respondents to child 1 only differ 
# systematically from those who responded to both children
#############################################
df_demo <- df %>% mutate(two_children = ifelse(child == 2, 1, 0))

# Subset analysis 1 sampl - households with at least 1 child
df_full <- df_demo %>% dplyr::filter(two_children == 0, c_Age < 97)
# Subset analysis 2 - households with 2 children
df_sub <- df_demo %>% dplyr::filter(two_children == 1, c_Age < 97) %>% dplyr::select(hhid, two_children)
# Merge and add an indicator variable for the analysis 2 sample (subsample)
df_match <- right_join(df_sub, df_full, by = 'hhid') %>% mutate(sub_sample = ifelse(is.na(two_children.x), 0, 1))

# Identify demographic variables 
demo_vars <- c('age_parent_imp', 'uae_national','ba_higher_education', 'c_Age', 'c_Sex')
# Run model to see significant differences between the two samples
samples_diff <- lapply(demo_vars, function(x){summary(lm(as.matrix(df_match[,x]) ~ df_match$sub_sample))})
samples_diff_tbl <- tibble(Variable = demo_vars,
                           'Full sample' = unlist(lapply(1:length(demo_vars), FUN = function(i) {samples_diff[[i]]$coef[1]})),
                           'Sub sample' = unlist(lapply(1:length(demo_vars), FUN = function(i) {samples_diff[[i]]$coef[2] + samples_diff[[i]]$coef[1]})),
                           'P-value' = unlist(lapply(1:length(demo_vars), FUN = function(i) {samples_diff[[i]]$coef[8]})),
                           Observations = unlist(lapply(1:length(demo_vars), FUN = function(i) {NROW(samples_diff[[i]]$residuals)}))
) %>% # subsample is the HHs that have at least 2 children to be able to run the child1-child2 comparison. Full sample is 783 and subsample is 111
  kable("html", digits = 2, caption = "Sample1-Sample2 differences") %>%
  kable_styling(bootstrap_options=c("striped", "hover", "condensed", "responsive"),
                full_width=FALSE)
samples_diff_tbl

#############################################
# Descriptive stats
#############################################
# Analysis 1 sample
####################
# Family descriptive stats
demog_stats_fs <- fBasics::basicStats(df_demo  %>% dplyr::filter(two_children == 0) %>% .[, demo_vars[1:3]] )
demog_stats_fs_tbl <- as.data.frame(t(demog_stats_fs)) %>% 
  mutate(Observations = nobs - NAs, 
         NA_p = NAs*100/nobs) %>%
  dplyr::select("Observations", "Mean", "Stdev", "Minimum", "Median", "Maximum") %>%
  kable("html", digits = 2, caption = "Demographic characteristics") %>%
  kable_styling(bootstrap_options=c("striped", "hover", "condensed", "responsive"),
                full_width=FALSE)
demog_stats_fs_tbl

# Child 1 descriptive statistics
df %>% mutate(two_children = ifelse(child == 2, 1, 0)) %>% 
  dplyr::filter(two_children == 0) %>% group_by(child) %>% 
  summarize(avg_age = mean(c_Age, na.rm = T), sd_age = sd(c_Age, na.rm = T), male = mean(c_Sex, na.rm = T))

# Quality time with child variable across the 2 childres
qual_time_c_desc <- fBasics::basicStats(c_wide_w1 %>% .[c('c_qual_time_sat_w1_c1', 'c_qual_time_sat_w1_c2')] )
qual_time_c_tbl <- as.data.frame(t(qual_time_c_desc)) %>% 
  # dplyr::select("Mean", "Stdev", "Minimum", "Median", "Maximum") %>%
  kable("html", digits = 2, caption = "Parent's rating of quality time spent with child in wave 1 across the 2 children") %>%
  kable_styling(bootstrap_options=c("striped", "hover", "condensed", "responsive"),
                full_width=FALSE)
qual_time_c_tbl

# Quality time with child variable across the 2 waves
qual_time_w_desc <- fBasics::basicStats(wide_c1 %>% .[c('c_qual_time_sat_w1', 'c_qual_time_sat_w2')] )
qual_time_w_tbl <- as.data.frame(t(qual_time_w_desc)) %>% 
  # dplyr::select("Mean", "Stdev", "Minimum", "Median", "Maximum") %>%
  kable("html", digits = 2, caption = "Parent's rating of quality time spent with child for child 1 across the 2 waves") %>%
  kable_styling(bootstrap_options=c("striped", "hover", "condensed", "responsive"),
                full_width=FALSE)
qual_time_w_tbl

# Analysis 2 sample
####################
# Family and child descriptive stats
fBasics::basicStats(c_wide_w1 %>% dplyr::filter(is.na(c_Age_c1) == FALSE) %>% 
                      .[, c('c_Age_c1', 'c_Age_c2', 'c_Sex_c1', 'c_Sex_c2', 'age_parent_imp', 'uae_national', 'ba_higher_education')]) %>% t() %>%
  as.data.frame() %>% 
  mutate(Observations = nobs - NAs, 
         NA_p = NAs*100/nobs) %>%
  dplyr::select( 'Observations', 'NA_p', "Mean", "Stdev", "Minimum", "Median", 'Minimum',"Maximum") %>%
  kable("html", digits = 2) %>%
  kable_styling(bootstrap_options=c("striped", "hover", "condensed", "responsive"),
                full_width=FALSE)

####################################
# Exploring outcome variables
####################################
# Create average scores of outcomes
####################################
# Analysis 1 sample
wide_cor <- wide %>% mutate(
  cwb1 = rowMeans(.[c('c_sleep_quality_w1', 'c_mental_health_w1', 'c_physical_health_w1', 'c_life_quality_w1', 'c_social_act_w1')]),
  cwb2 = rowMeans(.[c('c_sleep_quality_w2', 'c_mental_health_w2', 'c_physical_health_w2', 'c_life_quality_w2', 'c_social_act_w2')]),
  ps1 = rowMeans(.[c('Q19_1_w1', 'Q19_2_w1', 'Q19_3_w1', 'Q19_4_w1')]),
  ps2 = rowMeans(.[c('Q19_1_w2', 'Q19_2_w2', 'Q19_3_w2', 'Q19_4_w2')]),
  iprp1 = rowMeans(.[c('Q24_1_w1', 'Q24_2_w1', 'Q24_3_w1')]),
  iprp2 = rowMeans(.[c('Q24_1_w2', 'Q24_2_w2', 'Q24_3_w2')])
) %>% dplyr::filter(child == 2)

wide_cor1 <- wide_cor %>% dplyr::select(ECONHard_w1, covid_risk_w1, jobloss_resp_w1, cwb1, ps1, iprp1, relat_child_w1)
wide_cor2 <- wide_cor %>% dplyr::select(ECONHard_w2, covid_risk_w2, jobloss_resp_w2,cwb2, ps2, iprp2, relat_child_w2)

# Analysis 2 sample
c_wide_cor <- c_wide_w1 %>% mutate(
  cwb1 = rowMeans(.[c('c_sleep_quality_w1_c1', 'c_mental_health_w1_c1', 'c_physical_health_w1_c1', 'c_life_quality_w1_c1', 'c_social_act_w1_c1')]),
  cwb2 = rowMeans(.[c('c_sleep_quality_w1_c2', 'c_mental_health_w1_c2', 'c_physical_health_w1_c2', 'c_life_quality_w1_c2', 'c_social_act_w1_c2')]),
  ps1 = rowMeans(.[c('Q19_1_w1', 'Q19_2_w1', 'Q19_3_w1', 'Q19_4_w1')]),
  iprp1 = rowMeans(.[c('Q24_1_w1', 'Q24_2_w1', 'Q24_3_w1')]),
) 

c1_wide_cor <- c_wide_cor %>% dplyr::select(ps1, iprp1, relat_child_w1_c1, cwb1)
c2_wide_cor <- c_wide_cor %>% dplyr::select(relat_child_w1_c2, cwb2)

# Correlation matrix
####################################
# Analysis 1 sample
cor(cbind(wide_cor1, wide_cor2), method = 'pearson') %>%
  kable("html", digits = 2, caption = "") %>%
  kable_styling(bootstrap_options=c("striped", "hover", "condensed", "responsive"),
                full_width=FALSE)
# Analysis 2 sample
cor(cbind(c1_wide_cor, c2_wide_cor), method = 'pearson', use = 'pairwise.complete.obs') %>%
  kable("html", digits = 2, caption = "") %>%
  kable_styling(bootstrap_options=c("striped", "hover", "condensed", "responsive"),
                full_width=FALSE)

# Summary statistics
####################################
# Analysis 1 sample
cbind(wide_cor1, wide_cor2) %>% fBasics::basicStats(.) %>% t(.) %>% as.data.frame() %>% 
  dplyr::select("Mean", "Stdev") %>%
  kable("html", digits = 2) %>%
  kable_styling(bootstrap_options=c("striped", "hover", "condensed", "responsive"),
                full_width=FALSE)

# Analysis 2 sample
cbind(c1_wide_cor, c2_wide_cor) %>% fBasics::basicStats(.) %>% t(.) %>% as.data.frame() %>% 
  dplyr::select("Mean", "Stdev") %>%
  kable("html", digits = 2) %>%
  kable_styling(bootstrap_options=c("striped", "hover", "condensed", "responsive"),
                full_width=FALSE)

