##########################################################################################
# Specification Curve Analysis and Digital Technology Use
# R-Script 3.3: MCS Analyse Specification Curves
##########################################################################################
setwd(".../2_sca")
library(tidyverse)
library(here)
here()
#################################################
# Bind Both
#################################################
load("2_3_sca_mcs_results_cm.rda")
load("2_3_sca_mcs_results_pr.rda")

results_mcs_sca_cm$respondent <-
  rep("Cohort Member", nrow(results_mcs_sca_cm))
results_mcs_sca_pr$respondent <-
  rep("Parent", nrow(results_mcs_sca_pr))

results_mcs_sca_cm_TV$respondent <-
  rep("Cohort Member", nrow(results_mcs_sca_cm_TV))
results_mcs_sca_TV_pr$respondent <-
  rep("Parent", nrow(results_mcs_sca_TV_pr))

results_mcs_sca_cm_Computer$respondent <-
  rep("Cohort Member", nrow(results_mcs_sca_cm_Computer))
results_mcs_sca_Computer_pr$respondent <-
  rep("Parent", nrow(results_mcs_sca_Computer_pr))

results_mcs_sca_cm_Games$respondent <-
  rep("Cohort Member", nrow(results_mcs_sca_cm_Games))
results_mcs_sca_Games_pr$respondent <-
  rep("Parent", nrow(results_mcs_sca_Games_pr))

results_mcs_sca_cm_Internet$respondent <-
  rep("Cohort Member", nrow(results_mcs_sca_cm_Internet))
results_mcs_sca_Internet_pr$respondent <-
  rep("Parent", nrow(results_mcs_sca_Internet_pr))

results_mcs_sca_cm_SocialMedia$respondent <-
  rep("Cohort Member", nrow(results_mcs_sca_cm_SocialMedia))
results_mcs_sca_SocialMedia_pr$respondent <-
  rep("Parent", nrow(results_mcs_sca_SocialMedia_pr))

results_mcs_sca_total <- rbind(results_mcs_sca_cm, results_mcs_sca_pr)
results_mcs_sca_TV_total <- rbind(results_mcs_sca_cm_TV, results_mcs_sca_TV_pr)
results_mcs_sca_Computer_total <- rbind(results_mcs_sca_cm_Computer, results_mcs_sca_Computer_pr)
results_mcs_sca_Games_total <- rbind(results_mcs_sca_cm_Games, results_mcs_sca_Games_pr)
results_mcs_sca_Internet_total <- rbind(results_mcs_sca_cm_Internet, results_mcs_sca_Internet_pr)
results_mcs_sca_SocialMedia_total <- rbind(results_mcs_sca_cm_SocialMedia, results_mcs_sca_SocialMedia_pr)


save(results_mcs_sca_total, file = "2_3_sca_mcs_results.rda")
save(results_mcs_sca_TV_total, file = "2_3_sca_mcs_results_TV.rda")
save(results_mcs_sca_Computer_total, file = "2_3_sca_mcs_results_Computer.rda")
save(results_mcs_sca_Games_total, file = "2_3_sca_mcs_results_Games.rda")
save(results_mcs_sca_Internet_total, file = "2_3_sca_mcs_results_Internet.rda")
save(results_mcs_sca_SocialMedia_total, file = "2_3_sca_mcs_results_SocialMedia.rda")



####################################################################################
# Number of specifications
####################################################################################
nrow(results_mcs_sca_total)

## Amount of results and significant results with dominant sign
results_mcs_sca_total_sig <- results_mcs_sca_total %>% filter(p_value < 0.05)
pmax(table(sign(results_mcs_sca_total$effect))[[-1]],table(sign(results_mcs_sca_total$effect))[[1]])
pmax(table(sign(results_mcs_sca_total_sig$effect))[[-1]],table(sign(results_mcs_sca_total_sig$effect))[[1]])


####################################################################################
# Median effects
# total, separate x variables, controls/no controls
####################################################################################
results_mcs_sca_total %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                    median_effectsize = median(rsqrd, na.rm = TRUE), 
                                    median_n = median(number, na.rm = TRUE),
                                    median_se = median(standard_error, na.rm = TRUE))

results_mcs_sca_TV_total %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                    median_effectsize = median(rsqrd, na.rm = TRUE), 
                                    median_n = median(number, na.rm = TRUE),
                                    median_se = median(standard_error, na.rm = TRUE))

results_mcs_sca_Computer_total %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                       median_effectsize = median(rsqrd, na.rm = TRUE), 
                                       median_n = median(number, na.rm = TRUE),
                                       median_se = median(standard_error, na.rm = TRUE))
results_mcs_sca_Games_total %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                             median_effectsize = median(rsqrd, na.rm = TRUE), 
                                             median_n = median(number, na.rm = TRUE),
                                             median_se = median(standard_error, na.rm = TRUE))
results_mcs_sca_Internet_total %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                          median_effectsize = median(rsqrd, na.rm = TRUE), 
                                          median_n = median(number, na.rm = TRUE),
                                          median_se = median(standard_error, na.rm = TRUE))
results_mcs_sca_SocialMedia_total %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                             median_effectsize = median(rsqrd, na.rm = TRUE), 
                                             median_n = median(number, na.rm = TRUE),
                                             median_se = median(standard_error, na.rm = TRUE))

results_mcs_sca_total %>% filter(respondent == "Parent") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                       median_effectsize = median(rsqrd, na.rm = TRUE), 
                                                                       median_n = median(number, na.rm = TRUE),
                                                                       median_se = median(standard_error, na.rm = TRUE))
results_mcs_sca_total %>% filter(respondent == "Cohort Member") %>% summarise(median_effect = median(effect, na.rm = TRUE),
                                                                              median_effectsize = median(rsqrd, na.rm = TRUE), 
                                                                              median_n = median(number, na.rm = TRUE),
                                                                              median_se = median(standard_error, na.rm = TRUE))

results_mcs_sca_total %>% filter(controls == "Controls") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                       median_effectsize = median(rsqrd, na.rm = TRUE), 
                                                                       median_n = median(number, na.rm = TRUE),
                                                                       median_se = median(standard_error, na.rm = TRUE))
results_mcs_sca_total %>% filter(controls == "No Controls") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                          median_effectsize = median(rsqrd, na.rm = TRUE), 
                                                                          median_n = median(number, na.rm = TRUE),
                                                                          median_se = median(standard_error, na.rm = TRUE))

results_mcs_sca_total %>% filter(x_variable == "fctvho00r") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                          median_effectsize = median(rsqrd, na.rm = TRUE), 
                                                                          median_n = median(number, na.rm = TRUE),
                                                                          median_se = median(standard_error, na.rm = TRUE))
results_mcs_sca_total %>% filter(x_variable == "fccomh00r") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                          median_effectsize = median(rsqrd, na.rm = TRUE), 
                                                                          median_n = median(number, na.rm = TRUE),
                                                                          median_se = median(standard_error, na.rm = TRUE))
results_mcs_sca_total %>% filter(x_variable == "fccmex00r") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                          median_effectsize = median(rsqrd, na.rm = TRUE), 
                                                                          median_n = median(number, na.rm = TRUE),
                                                                          median_se = median(standard_error, na.rm = TRUE))
results_mcs_sca_total %>% filter(x_variable == "fcinth00r") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                          median_effectsize = median(rsqrd, na.rm = TRUE), 
                                                                          median_n = median(number, na.rm = TRUE),
                                                                          median_se = median(standard_error, na.rm = TRUE))
results_mcs_sca_total %>% filter(x_variable == "fcsome00r") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                          median_effectsize = median(rsqrd, na.rm = TRUE), 
                                                                          median_n = median(number, na.rm = TRUE),
                                                                          median_se = median(standard_error, na.rm = TRUE))

