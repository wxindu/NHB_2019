##########################################################################################
# Specification Curve Analysis and Digital Technology Use
# R-Script 1.3: MCS clean data and make variables
##########################################################################################
library(here)
here()
#######################################################
# Load libraries
#######################################################
library("foreign")
library("tidyverse")
library("psych")

#######################################################
# Read MCS Dataset
#######################################################

# String indicating data directory
# Should lead to the folder containing the YRBS data
# folder_data <- ".../2_datasets/mcs_raw_data/"

# load data which is in separate data files
# these data files can be access directly from the mcs once a data sharing agreement is agreed on
# data1 <-
#   read.csv(paste(folder_data, "mcs6_cm_assessment.csv", sep = ""))
data1 <- read.spss(here("/data/MCS/spss24/mcs6_cm_assessment.sav"), to.data.frame = TRUE)
# data2 <-
#   read.csv(paste(folder_data, "mcs6_cm_derived.csv", sep = ""))
data2 <- read.spss(here("/data/MCS/spss24/mcs6_cm_derived.sav"), to.data.frame = TRUE)
# data3 <-
#   read.csv(paste(folder_data, "mcs6_cm_interview.csv", sep = ""))
data3 <- read.spss(here("/data/MCS/spss24/mcs6_cm_interview.sav"), to.data.frame = TRUE)
# data4 <-
#   read.csv(paste(folder_data, "mcs6_cm_measurement.csv", sep = ""))
data4 <- read.spss(here("/data/MCS/spss24/mcs6_cm_measurement.sav"), to.data.frame = TRUE)
# data5 <-
#   read.csv(paste(folder_data, "mcs6_family_derived.csv", sep = ""))
data5 <- read.spss(here("/data/MCS/spss24/mcs6_family_derived.sav"), to.data.frame = TRUE)
# data6 <-
#   read.csv(paste(folder_data, "mcs6_parent_cm_interview.csv", sep = ""))
data6 <- read.spss(here("/data/MCS/spss24/mcs6_parent_cm_interview.sav"), to.data.frame = TRUE)
# data7 <-
#   read.csv(paste(folder_data, "mcs6_parent_assessment.csv", sep = ""))
data7 <- read.spss(here("/data/MCS/spss24/mcs6_parent_assessment.sav"), to.data.frame = TRUE)
# data8 <-
#   read.csv(paste(folder_data, "mcs6_parent_derived.csv", sep = ""))
data8 <- read.spss(here("/data/MCS/spss24/mcs6_parent_derived.sav"), to.data.frame = TRUE)
# data9 <-
#   read.csv(paste(folder_data, "mcs6_parent_interview.csv", sep = ""))
data9 <- read.spss(here("/data/MCS/spss24/mcs6_parent_interview.sav"), to.data.frame = TRUE)
# data10 <-
#   read.csv(paste(folder_data, "mcs6_proxy_partner_interview.csv", sep = ""))
data10 <- read.spss(here("/data/MCS/spss24/mcs6_proxy_partner_interview.sav"), to.data.frame = TRUE)
# data11 <-
#   read.csv(paste(folder_data, "mcs6_hhgrid.csv", sep = "")) 
data11 <- read.spss(here("/data/MCS/spss24/mcs6_hhgrid.sav"), to.data.frame = TRUE)


# change names of columns to lowercase
names(data1) <- tolower(names(data1))
names(data2) <- tolower(names(data2))
names(data3) <- tolower(names(data3))
names(data4) <- tolower(names(data4))
names(data5) <- tolower(names(data5))
names(data6) <- tolower(names(data6))
names(data7) <- tolower(names(data7))
names(data8) <- tolower(names(data8))
names(data9) <- tolower(names(data9))
names(data10) <- tolower(names(data10))
names(data11) <- tolower(names(data11))

#######################################################
# Combine different datasets
# We combine the datasets by making identification 
# variables for each child. We cannot use the family
# identification variables because they obfuscate 
# twins/triplets.
#######################################################

###################
# Cohort Members
###################
data1$mcsid1 <-
  ifelse(
    data1$fcnum00 == "1st Cohort Member of the family",
    paste(as.character(data1$mcsid), "_1", sep = ""),
    ifelse(
      data1$fcnum00 == "2nd Cohort Member of the family" ,
      paste(as.character(data1$mcsid), "_2", sep = ""),
      ifelse(data1$fcnum00 == "3rd Cohort Member of the family", paste(as.character(data1$mcsid), "_3", sep = ""), NA)
    )
  )
data1$mcsid <- as.character(data1$mcsid)
data2$mcsid2 <-
  ifelse(
    data2$fcnum00 == "1st Cohort Member of the family",
    paste(as.character(data2$mcsid), "_1", sep = ""),
    ifelse(
      data2$fcnum00 == "2nd Cohort Member of the family",
      paste(as.character(data2$mcsid), "_2", sep = ""),
      ifelse(data2$fcnum00 == "3rd Cohort Member of the family", paste(as.character(data2$mcsid), "_3", sep = ""), NA)
    )
  )
data3 <- data3 %>%
  mutate(mcsid = gsub(" ", "", mcsid))
data3$mcsid3 <-
  ifelse(
    data3$fcnum00 == "1st Cohort Member of the family",
    paste(as.character(data3$mcsid), "_1", sep = ""),
    ifelse(
      data3$fcnum00 == "2nd Cohort Member of the family",
      paste(as.character(data3$mcsid), "_2", sep = ""),
      ifelse(data3$fcnum00 == "3rd Cohort Member of the family", paste(as.character(data3$mcsid), "_3", sep = ""), NA)
    )
  )
data4$mcsid4 <-
  ifelse(
    data4$fcnum00 == "1st Cohort Member of the family",
    paste(as.character(data4$mcsid), "_1", sep = ""),
    ifelse(
      data4$fcnum00 == "2n Cohort Member of the family",
      paste(as.character(data4$mcsid), "_2", sep = ""),
      ifelse(data4$fcnum00 == "3rd Cohort Member of the family", paste(as.character(data4$mcsid), "_3", sep = ""), NA)
    )
  )
data5$mcsid5 <- as.character(data5$mcsid)
###################
# Parents
###################
data6 <- data6 %>%
  mutate(mcsid = gsub(" ", "", mcsid))
data6$mcsid6 <-
  paste(as.character(data6$mcsid),
        as.character(data6$fpnum00),
        sep = "_")
data7$mcsid7 <-
  paste(as.character(data7$mcsid),
        as.character(data7$fpnum00),
        sep = "_")
data8 <- data8 %>%
  mutate(mcsid = gsub(" ", "", mcsid))
data8$mcsid8 <-
  paste(as.character(data8$mcsid),
        as.character(data8$fpnum00),
        sep = "_")
data9 <- data9 %>%
  mutate(mcsid = gsub(" ", "", mcsid))
data9$mcsid9 <-
  paste(as.character(data9$mcsid),
        as.character(data9$fpnum00),
        sep = "_")
data10 <- data10 %>%
  mutate(mcsid = gsub(" ", "", mcsid))
data10$mcsid10 <-
  paste(as.character(data10$mcsid),
        as.character(data10$fpnum00),
        sep = "_")

###################
# Merge
###################
# Cohort Members
data_cm <-
  dplyr::left_join(data1, data2[, is.na(match(names(data2), names(data1)))], by = c("mcsid1" = "mcsid2"))
data_cm <-
  dplyr::left_join(data_cm, data3[, is.na(match(names(data3), names(data_cm)))], by = c("mcsid1" = "mcsid3"))
data_cm <-
  dplyr::left_join(data_cm, data4[, is.na(match(names(data4), names(data_cm)))], by = c("mcsid1" = "mcsid4"))
data_cm <-
  dplyr::left_join(data_cm, data5[, is.na(match(names(data5), names(data_cm)))], by = c("mcsid" = "mcsid5"))

# Parents
data_pa <-
  dplyr::left_join(data6, data7[, is.na(match(names(data7), names(data6)))], by = c("mcsid6" = "mcsid7"))
data_pa <-
  dplyr::left_join(data_pa, data8[, is.na(match(names(data8), names(data_pa)))], by = c("mcsid6" = "mcsid8"))
data_pa <-
  dplyr::left_join(data_pa, data9[, is.na(match(names(data9), names(data_pa)))], by = c("mcsid6" = "mcsid9"))

data_pa$mcsid1_r <-
  ifelse(
    data_pa$fcnum00 == "1st Cohort Member of the family",
    paste(as.character(data_pa$mcsid), "_1", sep = ""),
    ifelse(
      data_pa$fcnum00 == "2nd Cohort Member of the family",
      paste(as.character(data_pa$mcsid), "_2", sep = ""),
      ifelse(data_pa$fcnum00 == "3rd Cohort Member of the family", paste(
        as.character(data_pa$mcsid), "_3", sep = ""
      ), NA)
    )
  )
data_pa$fpnum00_r <- ifelse(data_pa$fpnum00 == 1, 1, 0)
data_pa_1 <- data_pa %>% filter(fpnum00_r == 1)

# Merge cohort members and parents, not merging duplicate rows
data <-
  dplyr::left_join(data_cm, data_pa_1[, is.na(match(names(data_pa_1), names(data_cm)))], by = c("mcsid1" = "mcsid1_r"))

###################
# Remove datasets
###################
# rm(data1)
# rm(data2)
# rm(data3)
# rm(data4)
# rm(data5)
# rm(data6)
# rm(data7)
# rm(data8)
# rm(data9)
# rm(data10)
# rm(data11)
# gc()

#######################################################
# Set missing values: any negative numbers
#######################################################
is.na(data[, ]) = data[, ] < 0

#######################################################
# Recode Well-being Measures: Cohort Member
# Change measures to 10 point scales 
# Reverse scales if necessary
#######################################################
# Well-being measures to a 10 point scale and reverse
data$fcscwk00 <- ifelse(data$fcscwk00 == "Completely happy       ", 1, ifelse(data$fcscwk00 == "Not at all happy       ", 7, data$fcscwk00))
data$fcscwk00r <- (10 - 1) * (data$fcscwk00 - 1) / (7 - 1) + 1

data$fcwylk00 <- ifelse(data$fcwylk00 == "Completely happy       ", 1, ifelse(data$fcwylk00 == "Not at all happy       ", 7, data$fcwylk00))
data$fcwylk00r <- (10 - 1) * (data$fcwylk00 - 1) / (7 - 1) + 1

data$fcfmly00 <- ifelse(data$fcfmly00 == "Completely happy       ", 1, ifelse(data$fcfmly00 == "Not at all happy       ", 7, data$fcfmly00))
data$fcfmly00r <- (10 - 1) * (data$fcfmly00 - 1) / (7 - 1) + 1

data$fcfrns00 <- ifelse(data$fcfrns00 == "Completely happy       ", 1, ifelse(data$fcfrns00 == "Not at all happy       ", 7, data$fcfrns00))
data$fcfrns00r <- (10 - 1) * (data$fcfrns00 - 1) / (7 - 1) + 1

data$fcschl00 <- ifelse(data$fcschl00 == "Completely happy       ", 1, ifelse(data$fcschl00 == "Not at all happy       ", 7, data$fcschl00))
data$fcschl00r <- (10 - 1) * (data$fcschl00 - 1) / (7 - 1) + 1

data$fclife00 <- ifelse(data$fclife00 == "Completely happy       ", 1, ifelse(data$fclife00 == "Not at all happy       ", 7, data$fclife00))
data$fclife00r <- (10 - 1) * (data$fclife00 - 1) / (7 - 1) + 1

data <- data %>%
  mutate(fcsati00 = ifelse(fcsati00 == "Strongly agree ", 1, 
                           ifelse(fcsati00 == "Agree  ", 2, 
                                  ifelse(fcsati00 == "Disagree       ", 3, 
                                         ifelse(fcsati00 == "Strongly disagree      ", 4, fcsati00)))))
data$fcsati00r <- (10 - 1) * (data$fcsati00 - 1) / (4 - 1) + 1

data <- data %>%
  mutate(fcgdql00 = ifelse(fcgdql00 == "Strongly agree ", 1, 
                           ifelse(fcgdql00 == "Agree  ", 2, 
                                  ifelse(fcgdql00 == "Disagree       ", 3, 
                                         ifelse(fcgdql00 == "Strongly disagree      ", 4, fcgdql00)))))
data$fcgdql00r <- (10 - 1) * (data$fcgdql00 - 1) / (4 - 1) + 1

data <- data %>%
  mutate(fcdowl00 = ifelse(fcdowl00 == "Strongly agree ", 1, 
                           ifelse(fcdowl00 == "Agree  ", 2, 
                                  ifelse(fcdowl00 == "Disagree       ", 3, 
                                         ifelse(fcdowl00 == "Strongly disagree      ", 4, fcdowl00)))))
data$fcdowl00r <- (10 - 1) * (data$fcdowl00 - 1) / (4 - 1) + 1

data <- data %>%
  mutate(fcvalu00 = ifelse(fcvalu00 == "Strongly agree ", 1, 
                           ifelse(fcvalu00 == "Agree  ", 2, 
                                  ifelse(fcvalu00 == "Disagree       ", 3, 
                                         ifelse(fcvalu00 == "Strongly disagree      ", 4, fcvalu00)))))
data$fcvalu00r <- (10 - 1) * (data$fcvalu00 - 1) / (4 - 1) + 1

data <- data %>%
  mutate(fcgdsf00 = ifelse(fcgdsf00 == "Strongly agree ", 1, 
                           ifelse(fcgdsf00 == "Agree  ", 2, 
                                  ifelse(fcgdsf00 == "Disagree       ", 3, 
                                         ifelse(fcgdsf00 == "Strongly disagree      ", 4, fcgdsf00)))))
data$fcgdsf00r <- (10 - 1) * (data$fcgdsf00 - 1) / (4 - 1) + 1

data <- data %>%
  mutate(fcmdsa00 = ifelse(fcmdsa00 == "Not true       ", 1, 
                           ifelse(fcmdsa00 == "Sometimes      ", 2, 
                                  ifelse(fcmdsa00 == "True   ", 3, fcmdsa00))))
data$fcmdsa00r <- (10 - 1) * (data$fcmdsa00 - 1) / (3 - 1) + 1

data <- data %>%
  mutate(fcmdsb00 = ifelse(fcmdsb00 == "Not true       ", 1, 
                           ifelse(fcmdsb00 == "Sometimes      ", 2, 
                                  ifelse(fcmdsb00 == "True   ", 3, fcmdsb00))))
data$fcmdsb00r <- (10 - 1) * (data$fcmdsb00 - 1) / (3 - 1) + 1

data <- data %>%
  mutate(fcmdsc00 = ifelse(fcmdsc00 == "Not true       ", 1, 
                           ifelse(fcmdsc00 == "Sometimes      ", 2, 
                                  ifelse(fcmdsc00 == "True   ", 3, fcmdsc00))))
data$fcmdsc00r <- (10 - 1) * (data$fcmdsc00 - 1) / (3 - 1) + 1

data <- data %>%
  mutate(fcmdsd00 = ifelse(fcmdsd00 == "Not true       ", 1, 
                           ifelse(fcmdsd00 == "Sometimes      ", 2, 
                                  ifelse(fcmdsd00 == "True   ", 3, fcmdsd00))))
data$fcmdsd00r <- (10 - 1) * (data$fcmdsd00 - 1) / (3 - 1) + 1

data <- data %>%
  mutate(fcmdse00 = ifelse(fcmdse00 == "Not true       ", 1, 
                           ifelse(fcmdse00 == "Sometimes      ", 2, 
                                  ifelse(fcmdse00 == "True   ", 3, fcmdse00))))
data$fcmdse00r <- (10 - 1) * (data$fcmdse00 - 1) / (3 - 1) + 1

data <- data %>%
  mutate(fcmdsf00 = ifelse(fcmdsf00 == "Not true       ", 1, 
                           ifelse(fcmdsf00 == "Sometimes      ", 2, 
                                  ifelse(fcmdsf00 == "True   ", 3, fcmdsf00))))
data$fcmdsf00r <- (10 - 1) * (data$fcmdsf00 - 1) / (3 - 1) + 1

data <- data %>%
  mutate(fcmdsg00 = ifelse(fcmdsg00 == "Not true       ", 1, 
                           ifelse(fcmdsg00 == "Sometimes      ", 2, 
                                  ifelse(fcmdsg00 == "True   ", 3, fcmdsg00))))
data$fcmdsg00r <- (10 - 1) * (data$fcmdsg00 - 1) / (3 - 1) + 1

data <- data %>%
  mutate(fcmdsh00 = ifelse(fcmdsh00 == "Not true       ", 1, 
                           ifelse(fcmdsh00 == "Sometimes      ", 2, 
                                  ifelse(fcmdsh00 == "True   ", 3, fcmdsh00))))
data$fcmdsh00r <- (10 - 1) * (data$fcmdsh00 - 1) / (3 - 1) + 1

data <- data %>%
  mutate(fcmdsi00 = ifelse(fcmdsi00 == "Not true       ", 1, 
                           ifelse(fcmdsi00 == "Sometimes      ", 2, 
                                  ifelse(fcmdsi00 == "True   ", 3, fcmdsi00))))
data$fcmdsi00r <- (10 - 1) * (data$fcmdsi00 - 1) / (3 - 1) + 1

data <- data %>%
  mutate(fcmdsj00 = ifelse(fcmdsj00 == "Not true       ", 1, 
                           ifelse(fcmdsj00 == "Sometimes      ", 2, 
                                  ifelse(fcmdsj00 == "True   ", 3, fcmdsj00))))
data$fcmdsj00r <- (10 - 1) * (data$fcmdsj00 - 1) / (3 - 1) + 1

data <- data %>%
  mutate(fcmdsk00 = ifelse(fcmdsk00 == "Not true       ", 1, 
                           ifelse(fcmdsk00 == "Sometimes      ", 2, 
                                  ifelse(fcmdsk00 == "True   ", 3, fcmdsk00))))
data$fcmdsk00r <- (10 - 1) * (data$fcmdsk00 - 1) / (3 - 1) + 1

data <- data %>%
  mutate(fcmdsl00 = ifelse(fcmdsl00 == "Not true       ", 1, 
                           ifelse(fcmdsl00 == "Sometimes      ", 2, 
                                  ifelse(fcmdsl00 == "True   ", 3, fcmdsl00))))
data$fcmdsl00r <- (10 - 1) * (data$fcmdsl00 - 1) / (3 - 1) + 1

data <- data %>%
  mutate(fcmdsm00 = ifelse(fcmdsm00 == "Not true       ", 1, 
                           ifelse(fcmdsm00 == "Sometimes      ", 2, 
                                  ifelse(fcmdsm00 == "True   ", 3, fcmdsm00))))
data$fcmdsm00r <- (10 - 1) * (data$fcmdsm00 - 1) / (3 - 1) + 1

data$fcscwk00r <- 11 - data$fcscwk00r
data$fcwylk00r <- 11 - data$fcwylk00r
data$fcfmly00r <- 11 - data$fcfmly00r
data$fcfrns00r <- 11 - data$fcfrns00r
data$fcschl00r <- 11 - data$fcschl00r
data$fclife00r <- 11 - data$fclife00r

data$fcsati00r <- 11 - data$fcsati00r
data$fcgdql00r <- 11 - data$fcgdql00r
data$fcdowl00r <- 11 - data$fcdowl00r
data$fcvalu00r <- 11 - data$fcvalu00r
data$fcgdsf00r <- 11 - data$fcgdsf00r

data$fcmdsa00r <- 11 - data$fcmdsa00r
data$fcmdsb00r <- 11 - data$fcmdsb00r
data$fcmdsc00r <- 11 - data$fcmdsc00r
data$fcmdsd00r <- 11 - data$fcmdsd00r
data$fcmdse00r <- 11 - data$fcmdse00r
data$fcmdsf00r <- 11 - data$fcmdsf00r
data$fcmdsg00r <- 11 - data$fcmdsg00r
data$fcmdsh00r <- 11 - data$fcmdsh00r
data$fcmdsi00r <- 11 - data$fcmdsi00r
data$fcmdsj00r <- 11 - data$fcmdsj00r
data$fcmdsk00r <- 11 - data$fcmdsk00r
data$fcmdsl00r <- 11 - data$fcmdsl00r
data$fcmdsm00r <- 11 - data$fcmdsm00r

###################
# Check correlations
###################
ds <-
  data[c(
    "fcmdsa00r",
    "fcmdsb00r",
    "fcmdsc00r",
    "fcmdsd00r",
    "fcmdse00r",
    "fcmdsf00r",
    "fcmdsg00r",
    "fcmdsh00r",
    "fcmdsi00r",
    "fcmdsj00r",
    "fcmdsk00r",
    "fcmdsl00r",
    "fcmdsm00r",
    "fcsati00r",
    "fcgdql00r",
    "fcdowl00r",
    "fcvalu00r",
    "fcgdsf00r",
    "fcscwk00r",
    "fcwylk00r",
    "fcfmly00r",
    "fcfrns00r",
    "fcschl00r",
    "fclife00r"
  )]
correlate <- psych::corr.test(ds)
correlate$r
rm(ds)

#######################################################
# Recode Well-being measures: Parent
# Reverse measures
#######################################################
data <- data %>%
  mutate(fpsdro00 = ifelse(fpsdro00 == "Not True       ", 0, 
                           ifelse(fpsdro00 == "Somewhat true  ", 1, 
                                  ifelse(fpsdro00 == "Certainly true ", 2, fpsdro00))))
data$fpsdro00 <- 2 - data$fpsdro00

data <- data %>%
  mutate(fpsdhs00 = ifelse(fpsdhs00 == "Not True       ", 0, 
                           ifelse(fpsdhs00 == "Somewhat true  ", 1, 
                                  ifelse(fpsdhs00 == "Certainly true ", 2, fpsdhs00))))
data$fpsdhs00 <- 2 - data$fpsdhs00

data <- data %>%
  mutate(fpsdtt00 = ifelse(fpsdtt00 == "Not True       ", 0, 
                           ifelse(fpsdtt00 == "Somewhat true  ", 1, 
                                  ifelse(fpsdtt00 == "Certainly true ", 2, fpsdtt00))))
data$fpsdtt00 <- 2 - data$fpsdtt00

data <- data %>%
  mutate(fpsdsp00 = ifelse(fpsdsp00 == "Not True       ", 0, 
                           ifelse(fpsdsp00 == "Somewhat true  ", 1, 
                                  ifelse(fpsdsp00 == "Certainly true ", 2, fpsdsp00))))
data$fpsdsp00 <- 2 - data$fpsdsp00

data <- data %>%
  mutate(fpsdmw00 = ifelse(fpsdmw00 == "Not True       ", 0, 
                           ifelse(fpsdmw00 == "Somewhat true  ", 1, 
                                  ifelse(fpsdmw00 == "Certainly true ", 2, fpsdmw00))))
data$fpsdmw00 <- 2 - data$fpsdmw00

data <- data %>%
  mutate(fpsdfs00 = ifelse(fpsdfs00 == "Not True       ", 0, 
                           ifelse(fpsdfs00 == "Somewhat true  ", 1, 
                                  ifelse(fpsdfs00 == "Certainly true ", 2, fpsdfs00))))
data$fpsdfs00 <- 2 - data$fpsdfs00

data <- data %>%
  mutate(fpsdfb00 = ifelse(fpsdfb00 == "Not True       ", 0, 
                           ifelse(fpsdfb00 == "Somewhat true  ", 1, 
                                  ifelse(fpsdfb00 == "Certainly true ", 2, fpsdfb00))))
data$fpsdfb00 <- 2 - data$fpsdfb00

data <- data %>%
  mutate(fpsdud00 = ifelse(fpsdud00 == "Not True       ", 0, 
                           ifelse(fpsdud00 == "Somewhat true  ", 1, 
                                  ifelse(fpsdud00 == "Certainly true ", 2, fpsdud00))))
data$fpsdud00 <- 2 - data$fpsdud00

data <- data %>%
  mutate(fpsddc00 = ifelse(fpsddc00 == "Not True       ", 0, 
                           ifelse(fpsddc00 == "Somewhat true  ", 1, 
                                  ifelse(fpsddc00 == "Certainly true ", 2, fpsddc00))))
data$fpsddc00 <- 2 - data$fpsddc00

data <- data %>%
  mutate(fpsdnc00 = ifelse(fpsdnc00 == "Not True       ", 0, 
                           ifelse(fpsdnc00 == "Somewhat true  ", 1, 
                                  ifelse(fpsdnc00 == "Certainly true ", 2, fpsdnc00))))
data$fpsdnc00 <- 2 - data$fpsdnc00

data <- data %>%
  mutate(fpsdoa00 = ifelse(fpsdoa00 == "Not True       ", 0, 
                           ifelse(fpsdoa00 == "Somewhat true  ", 1, 
                                  ifelse(fpsdoa00 == "Certainly true ", 2, fpsdoa00))))
data$fpsdoa00 <- 2 - data$fpsdoa00

data <- data %>%
  mutate(fpsdpb00 = ifelse(fpsdpb00 == "Not True       ", 0, 
                           ifelse(fpsdpb00 == "Somewhat true  ", 1, 
                                  ifelse(fpsdpb00 == "Certainly true ", 2, fpsdpb00))))
data$fpsdpb00 <- 2 - data$fpsdpb00

data <- data %>%
  mutate(fpsdcs00 = ifelse(fpsdcs00 == "Not True       ", 0, 
                           ifelse(fpsdcs00 == "Somewhat true  ", 1, 
                                  ifelse(fpsdcs00 == "Certainly true ", 2, fpsdcs00))))
data$fpsdcs00 <- 2 - data$fpsdcs00

data <- data %>%
  mutate(fpsdgb00 = ifelse(fpsdgb00 == "Not True       ", 0, 
                           ifelse(fpsdgb00 == "Somewhat true  ", 1, 
                                  ifelse(fpsdgb00 == "Certainly true ", 2, fpsdgb00))))
data$fpsdgb00 <- 2 - data$fpsdgb00

data <- data %>%
  mutate(fpsdfe00 = ifelse(fpsdfe00 == "Not True       ", 0, 
                           ifelse(fpsdfe00 == "Somewhat true  ", 1, 
                                  ifelse(fpsdfe00 == "Certainly true ", 2, fpsdfe00))))
data$fpsdfe00 <- 2 - data$fpsdfe00

data$fconduct <- 11 - as.numeric(data$fconduct)
data$fhyper <- 11 - as.numeric(data$fhyper)
data$fpeer <- 11 - as.numeric(data$fpeer)
data$femotion <- 11 - as.numeric(data$femotion)
data$febdtot <- 41 - as.numeric(data$febdtot)

###################
# Check correlations
###################
ds <-
  data[c(
    "fpsdpf00",
    "fpsdro00",
    "fpsdhs00",
    "fpsdsr00",
    "fpsdtt00",
    "fpsdsp00",
    "fpsdor00",
    "fpsdmw00",
    "fpsdhu00",
    "fpsdfs00",
    "fpsdgf00",
    "fpsdfb00",
    "fpsdud00",
    "fpsdlc00",
    "fpsddc00",
    "fpsdnc00",
    "fpsdky00",
    "fpsdoa00",
    "fpsdpb00",
    "fpsdvh00",
    "fpsdst00",
    "fpsdcs00",
    "fpsdgb00",
    "fpsdfe00",
    "fpsdte00", 
    "fconduct", 
    "fhyper",
    "femotion",
    "fpeer", 
    "fprosoc",
    "febdtot"
  )]
correlate <- psych::corr.test(ds)
correlate$r
rm(ds)

#######################################################
# Recode Digital Technology Use measures
#######################################################
# Reverse computer ownership measure
data <- data %>% 
  mutate(fccmex00 = ifelse(fccmex00 == "Yes    ", 1, 
                           ifelse(fccmex00 == "No     ", 2, fccmex00)))
data$fccmex00r <- 3 - data$fccmex00

# Convert measures to 10 point scale
# None   #
# Less than half an hour #
# Half an hour to less than 1 hour       #
# 1 hour to less than 2 hours    #
# 2 hours to less than 3 hours   #
# 3 hours to less than 5 hours   #
# 5 hours to less than 7 hours   #
# 7 hours or more#
data <- data %>%
  mutate(fctvho00 = ifelse(fctvho00 == "None   ", 1, 
                           ifelse(fctvho00 == "Less than half an hour ", 2, 
                                  ifelse(fctvho00 == "Half an hour to less than 1 hour       ", 3, 
                                         ifelse(fctvho00 == "1 hour to less than 2 hours    ", 4, 
                                                ifelse(fctvho00 == "2 hours to less than 3 hours   ", 5, 
                                                       ifelse(fctvho00 == "3 hours to less than 5 hours   ", 6, 
                                                              ifelse(fctvho00 == "5 hours to less than 7 hours   ", 7, 
                                                                     ifelse(fctvho00 == "7 hours or more", 8, fctvho00)))))))))
data$fctvho00r <- (10 - 1) * (data$fctvho00 - 1) / (8 - 1) + 1

data <- data %>%
  mutate(fccomh00 = ifelse(fccomh00 == "None   ", 1, 
                           ifelse(fccomh00 == "Less than half an hour ", 2, 
                                  ifelse(fccomh00 == "Half an hour to less than 1 hour       ", 3, 
                                         ifelse(fccomh00 == "1 hour to less than 2 hours    ", 4, 
                                                ifelse(fccomh00 == "2 hours to less than 3 hours   ", 5, 
                                                       ifelse(fccomh00 == "3 hours to less than 5 hours   ", 6, 
                                                              ifelse(fccomh00 == "5 hours to less than 7 hours   ", 7, 
                                                                     ifelse(fccomh00 == "7 hours or more", 8, fccomh00)))))))))
data$fccomh00r <- (10 - 1) * (data$fccomh00 - 1) / (8 - 1) + 1

data <- data %>%
  mutate(fcinth00 = ifelse(fcinth00 == "None   ", 1, 
                           ifelse(fcinth00 == "Less than half an hour ", 2, 
                                  ifelse(fcinth00 == "Half an hour to less than 1 hour       ", 3, 
                                         ifelse(fcinth00 == "1 hour to less than 2 hours    ", 4, 
                                                ifelse(fcinth00 == "2 hours to less than 3 hours   ", 5, 
                                                       ifelse(fcinth00 == "3 hours to less than 5 hours   ", 6, 
                                                              ifelse(fcinth00 == "5 hours to less than 7 hours   ", 7, 
                                                                     ifelse(fcinth00 == "7 hours or more", 8, fcinth00)))))))))
data$fcinth00r <- (10 - 1) * (data$fcinth00 - 1) / (8 - 1) + 1

data <- data %>%
  mutate(fcsome00 = ifelse(fcsome00 == "None   ", 1, 
                           ifelse(fcsome00 == "Less than half an hour ", 2, 
                                  ifelse(fcsome00 == "Half an hour to less than 1 hour       ", 3, 
                                         ifelse(fcsome00 == "1 hour to less than 2 hours    ", 4, 
                                                ifelse(fcsome00 == "2 hours to less than 3 hours   ", 5, 
                                                       ifelse(fcsome00 == "3 hours to less than 5 hours   ", 6, 
                                                              ifelse(fcsome00 == "5 hours to less than 7 hours   ", 7, 
                                                                     ifelse(fcsome00 == "7 hours or more", 8, fcsome00)))))))))
data$fcsome00r <- (10 - 1) * (data$fcsome00 - 1) / (8 - 1) + 1
data$fccmex00r <- (10 - 1) * (data$fccmex00r - 1) / (2 - 1) + 1

# Make technology use measure
data$tech <-
  rowMeans(subset(
    data,
    select = c(
      "fctvho00r",
      "fccomh00r",
      "fccmex00r",
      "fcinth00r",
      "fcsome00r"
    )
  ), na.rm = FALSE)

###################
# Check correlations
###################
tu <-
  data[c("fctvho00r",
         "fccomh00r",
         "fccmex00r",
         "fcinth00r",
         "fcsome00r",
         "tech")]
correlate <- psych::corr.test(tu)
correlate$r
rm(tu)

#######################################################
# Recode Control measures
#######################################################
# Make ethnicity, majority vs minority
# white = 1, other = 0
# NAs are also 0
data$fd06e00 <- ifelse(data$fd06e00 == "White  ", 1, 0)

# Mean of educational motivation
data <- data %>%
  mutate(fcscbe00 = ifelse(fcscbe00 == "All of the time", 1, 
                           ifelse(fcscbe00 == "Most of the time       ", 2, 
                                  ifelse(fcscbe00 == "Some of the time       ", 3, 
                                         ifelse(fcscbe00 == "Never  ", 4, fcscbe00)))))
data$fcscbe00r <- 6 - data$fcscbe00

data <- data %>%
  mutate(fcsint00 = ifelse(fcsint00 == "All of the time", 1, 
                           ifelse(fcsint00 == "Most of the time       ", 2, 
                                  ifelse(fcsint00 == "Some of the time       ", 3, 
                                         ifelse(fcsint00 == "Never  ", 4, fcsint00)))))
data$fcsint00r <- 6 - data$fcsint00

data <- data %>%
  mutate(fcsunh00 = ifelse(fcsunh00 == "All of the time", 1, 
                           ifelse(fcsunh00 == "Most of the time       ", 2, 
                                  ifelse(fcsunh00 == "Some of the time       ", 3, 
                                         ifelse(fcsunh00 == "Never  ", 4, fcsunh00)))))

data <- data %>%
  mutate(fcstir00 = ifelse(fcstir00 == "All of the time", 1, 
                           ifelse(fcstir00 == "Most of the time       ", 2, 
                                  ifelse(fcstir00 == "Some of the time       ", 3, 
                                         ifelse(fcstir00 == "Never  ", 4, fcstir00)))))

data <- data %>%
  mutate(fcscwa00 = ifelse(fcscwa00 == "All of the time", 1, 
                           ifelse(fcscwa00 == "Most of the time       ", 2, 
                                  ifelse(fcscwa00 == "Some of the time       ", 3, 
                                         ifelse(fcscwa00 == "Never  ", 4, fcscwa00)))))

data <- data %>%
  mutate(fcmnwo00 = ifelse(fcmnwo00 == "All of the time", 1, 
                           ifelse(fcmnwo00 == "Most of the time       ", 2, 
                                  ifelse(fcmnwo00 == "Some of the time       ", 3, 
                                         ifelse(fcmnwo00 == "Never  ", 4, fcmnwo00)))))

data$edumot <-
  rowMeans(subset(
    data,
    select = c(
      "fcscbe00r",
      "fcsint00r",
      "fcsunh00",
      "fcstir00",
      "fcscwa00",
      "fcmnwo00"
    )
  ), na.rm = FALSE)

# Mean of closeness to parents
data <- data %>%
  mutate(fcrlqm00 = ifelse(fcrlqm00 == "Not very close ", 1, 
                           ifelse(fcrlqm00 == "Fairly close   ", 2, 
                                  ifelse(fcrlqm00 == "Very close     ", 3, 
                                         ifelse(fcrlqm00 == "Extremely close", 4, 
                                                ifelse(fcrlqm00 == "I don't have a mother/I am not in conta", 5, fcrlqm00))))))
data <- data %>%
  mutate(fcrlqf00 = ifelse(fcrlqf00 == "Not very close ", 1, 
                           ifelse(fcrlqf00 == "Fairly close   ", 2, 
                                  ifelse(fcrlqf00 == "Very close     ", 3, 
                                         ifelse(fcrlqf00 == "Extremely close", 4, 
                                                ifelse(fcrlqf00 == "I don't have a mother/I am not in conta", 5, fcrlqf00))))))
is.na(data[,c("fcrlqm00", "fcrlqf00")]) = data[, c("fcrlqm00", "fcrlqf00")] == 5 # make NA if do not have father/mother, therefore exclude those with no parents 

data <- data %>%
  mutate(fcquam00 = ifelse(fcquam00 == "Most days      ", 1, 
                           ifelse(fcquam00 == "More than once a week  ", 2, 
                                  ifelse(fcquam00 == "Less than once a week  ", 3, 
                                         ifelse(fcquam00 == "Hardly ever    ", 4, 
                                                ifelse(fcquam00 == "Never  ", 5, fcquam00))))))

data <- data %>%
  mutate(fcquaf00 = ifelse(fcquaf00 == "Most days      ", 1, 
                           ifelse(fcquaf00 == "More than once a week  ", 2, 
                                  ifelse(fcquaf00 == "Less than once a week  ", 3, 
                                         ifelse(fcquaf00 == "Hardly ever    ", 4, 
                                                ifelse(fcquaf00 == "Never  ", 5, fcquaf00))))))

data$clpar <-
  rowMeans(subset(data, select = c(
    "fcrlqm00", "fcrlqf00", "fcquam00", "fcquaf00"
  )), na.rm = FALSE)

# Mean of sleep quality

data <- data %>%
  mutate(fcsltr00 = ifelse(fcsltr00 == "All of the time", 1, 
                           ifelse(fcsltr00 == "Most of the time       ", 2,
                                  ifelse(fcsltr00 == "A good bit of the time ", 3, 
                                         ifelse(fcsltr00 == "Some of the time       ", 4, 
                                                ifelse(fcsltr00 == "A little of the time   ", 5, 
                                                       ifelse(fcsltr00 == "None of the time       ", 6, fcsltr00)))))))

data$fcsltr00r <- 7 - data$fcsltr00 #reverse sleep difficulties question

data <- data %>%
  mutate(fcslln00 = ifelse(fcslln00 == "0-15 minutes   ", 1, 
                           ifelse(fcslln00 == "16-30 minutes  ", 2,
                                  ifelse(fcslln00 == "31-45 minutes  ", 3, 
                                         ifelse(fcslln00 == "46-60 minutes  ", 4, 
                                                ifelse(fcslln00 == "More than 60 minutes   ", 5, fcslln00))))))
data$sldif <-
  rowMeans(subset(data, select = c("fcslln00", "fcsltr00r")), na.rm = FALSE)

# Recode sex
# 1 = male, 0 = female
# data$fccsex00r <- 2 - data$fccsex00
data <- data %>% mutate(fccsex00r = ifelse(fccsex00 == "Male   ", 1, 0))

#######################################################
# Recode Comparison Variables
#######################################################
# Calculate time spent sleeping on a school night
# What time do you go to sleep at night (before 9 - up to after midnight)
data$fcslwk00r <- ifelse(data$fcslwk00 == "Before 9pm     ",
                         12 - 8.5,
                         ifelse(
                           data$fcslwk00 == "9 - 9:59pm     ",
                           12 - 9.5,
                           ifelse(
                             data$fcslwk00 == "10 - 10:59pm   ",
                             12 - 10.5,
                             ifelse(data$fcslwk00 == "11 - midnight  ", 12 - 11.5, 12 -
                                      12.5)
                           )
                         ))
# changed all NAs to after midnight

# What time do you wake up before 6 up to after 9)
data$fcwuwk00r <- ifelse(data$fcwuwk00 == "Before 6am     ", 5.5,
                         ifelse(data$fcwuwk00 == "6 - 6:59am     ", 6.5,
                                ifelse(
                                  data$fcwuwk00 == "7 - 7:59am     ", 7.5,
                                  ifelse(data$fcwuwk00 == "8 - 8:59am     ", 8.5, 9.5)
                                )))
data$sleeptime <- data$fcslwk00r + data$fcwuwk00r

# Recode Handedness
# 0 - right, 1 - either, 2 - left 
data$hand <-
  ifelse(data$fchand00 == "Right hand     ", 0, ifelse(data$fchand00 == "Either hand    ", 1, 2))

# Reverse appropriate measures

data <- data %>%
  mutate(fccycf00 = ifelse(fccycf00 == "Every day or almost every day  ", 1, 
                           ifelse(fccycf00 == "Several times a week   ", 2, 
                                  ifelse(fccycf00 == "Once or twice a week   ", 3, 
                                         ifelse(fccycf00 == "At least once a month  ", 4, 
                                                ifelse(fccycf00 == "Every few months       ", 5, 
                                                       ifelse(fccycf00 == "At least once a year   ", 6, 
                                                              ifelse(fccycf00 == "Less often or never    ", 7, 
                                                                     ifelse(fccycf00 == "Do not use a bicycle   ", 8, fccycf00)))))))))
data$fccycf00r <- 9 - data$fccycf00

data <- data %>%
  mutate(fcglas00 = ifelse(fcglas00 == "Yes    ", 1, 
                           ifelse(fcglas00 == "No     ", 2, fcglas00)))
data$fcglas00r <- 2 - data$fcglas00

data <- data %>%
  mutate(fcares00 = ifelse(fcares00 == "Yes    ", 1, 
                           ifelse(fcares00 == "No     ", 2, fcares00)))
data$fcares00r <- 2 - data$fcares00

data <- data %>%
  mutate(fccybu00 = ifelse(fccybu00 == "Most days      ", 1, 
                           ifelse(fccybu00 == "About once a week      ", 2, 
                                  ifelse(fccybu00 == "About once a month     ", 3, 
                                         ifelse(fccybu00 == "Every few months       ", 4, 
                                                ifelse(fccybu00 == "Less often     ", 5, 
                                                       ifelse(fccybu00 == "Never  ", 6, fccybu00)))))))
data$fccybu00r <- 7 - data$fccybu00

data <- data %>%
  mutate(fchurt00 = ifelse(fchurt00 == "Most days      ", 1, 
                           ifelse(fchurt00 == "About once a week      ", 2, 
                                  ifelse(fchurt00 == "About once a month     ", 3, 
                                         ifelse(fchurt00 == "Every few months       ", 4, 
                                                ifelse(fchurt00 == "Less often     ", 5, 
                                                       ifelse(fchurt00 == "Never  ", 6, fchurt00)))))))
data$fchurt00r <- 7 - data$fchurt00

data <- data %>%
  mutate(fccanb00 = ifelse(fccanb00 == "Yes    ", 1, 
                           ifelse(fccanb00 == "No     ", 2, fccanb00)))
data$fccanb00r <- 2 - data$fccanb00

data <- data %>%
  mutate(fcalfv00 = ifelse(fcalfv00 == "Yes    ", 1, 
                           ifelse(fcalfv00 == "No     ", 2, fcalfv00)))
data$fcalfv00r <- 2 - data$fcalfv00

####################################################################################
# Calculate number of participants
####################################################################################
table(data$fccsex00r)
data$fccage00 <- as.numeric(data$fccage00)
table(data$fccage00)
mean(data$fccage00, na.rm = TRUE)
sd(data$fccage00, na.rm = TRUE)
nrow(data %>% filter(!is.na(felig00) & felig00 == "Main Interview "))

####################################################################################
# Save as CSV for running large code on computer cluster
# remove all symbols not recognised by linux
####################################################################################
n <- names(data)
n <- gsub("mcsid", "mcsid", n)
names(data) <- n

#####################################
# Additional data wrangling
########################################

data <- data %>%
  mutate(fcpaab00 = ifelse(fcpaab00 == "No     ", 2, 
                           ifelse(fcpaab00 == "Yes    ", 1, fcpaab00))) %>%
  mutate(fpclsi00 = ifelse(fpclsi00 == "No     ", 2, 
                           ifelse(fpclsi00 == "Yes    ", 1, fpclsi00))) %>%
  mutate(fdacaq00 = ifelse(fdacaq00 == "NVQ Level 5    ", 5, 
                           ifelse(fdacaq00 == "NVQ Level 4    ", 4, 
                                  ifelse(fdacaq00 == "NVQ Level 3    ", 3, 
                                         ifelse(fdacaq00 == "NVQ Level 2    ", 2, 
                                                ifelse(fdacaq00 == "NVQ Level 1    ", 1, 
                                                       ifelse(fdacaq00 == "Overseas qualification only    ", 95, 
                                                              ifelse(fdacaq00 == "None of these  ", 95, fdacaq00)))))))) %>%
  mutate(fd05s00 = ifelse(fd05s00 == "Intermediate   ", 2, 
                          ifelse(fd05s00 == "Lo sup and tech", 4, 
                                 ifelse(fd05s00 == "Manag and profl", 1, 
                                        ifelse(fd05s00 == "Semi-rou and routine   ", 5, 
                                               ifelse(fd05s00 == "Sm emp and s-emp       ", 3, 
                                                      ifelse(is.na(fd05s00), -1, fd05s00))))))) %>%
  
  mutate(fpchti00 = ifelse(fpchti00 == "Just enough time       ", 3, 
                           ifelse(fpchti00 == "More than enough time  ", 2, 
                                  ifelse(fpchti00 == "Not quite enough time  ", 4, 
                                         ifelse(fpchti00 == "Nowhere near enough time       ", 5, 
                                                ifelse(fpchti00 == "Too much time  ", 1, 
                                                       fpchti00))))))
  
# y <-
#   c(
#     "fpsdte00" ##
#   )

data <- data %>%
  mutate(fpsdpf00 = ifelse(fpsdpf00 == "Not True       ", 0, 
                           ifelse(fpsdpf00 == "Somewhat true  ", 1, 
                                  ifelse(fpsdpf00 == "Certainly true ", 2, fpsdpf00)))) %>%
  mutate(fpsdsr00 = ifelse(fpsdsr00 == "Not True       ", 0, 
                           ifelse(fpsdsr00 == "Somewhat true  ", 1, 
                                  ifelse(fpsdsr00 == "Certainly true ", 2, fpsdsr00)))) %>%
  mutate(fpsdor00 = ifelse(fpsdor00 == "Not True       ", 0, 
                           ifelse(fpsdor00 == "Somewhat true  ", 1, 
                                  ifelse(fpsdor00 == "Certainly true ", 2, fpsdor00)))) %>%
  mutate(fpsdhu00 = ifelse(fpsdhu00 == "Not True       ", 0, 
                           ifelse(fpsdhu00 == "Somewhat true  ", 1, 
                                  ifelse(fpsdhu00 == "Certainly true ", 2, fpsdhu00)))) %>%
  mutate(fpsdgf00 = ifelse(fpsdgf00 == "Not True       ", 0, 
                           ifelse(fpsdgf00 == "Somewhat true  ", 1, 
                                  ifelse(fpsdgf00 == "Certainly true ", 2, fpsdgf00)))) %>%
  mutate(fpsdlc00 = ifelse(fpsdlc00 == "Not True       ", 0, 
                           ifelse(fpsdlc00 == "Somewhat true  ", 1, 
                                  ifelse(fpsdlc00 == "Certainly true ", 2, fpsdlc00)))) %>%
  mutate(fpsdky00 = ifelse(fpsdky00 == "Not True       ", 0, 
                           ifelse(fpsdky00 == "Somewhat true  ", 1, 
                                  ifelse(fpsdky00 == "Certainly true ", 2, fpsdky00)))) %>%
  mutate(fpsdvh00 = ifelse(fpsdvh00 == "Not True       ", 0, 
                           ifelse(fpsdvh00 == "Somewhat true  ", 1, 
                                  ifelse(fpsdvh00 == "Certainly true ", 2, fpsdvh00)))) %>%
  mutate(fpsdst00 = ifelse(fpsdst00 == "Not True       ", 0, 
                           ifelse(fpsdst00 == "Somewhat true  ", 1, 
                                  ifelse(fpsdst00 == "Certainly true ", 2, fpsdst00)))) %>%
  mutate(fpsdte00 = ifelse(fpsdte00 == "Not True       ", 0, 
                           ifelse(fpsdte00 == "Somewhat true  ", 1, 
                                  ifelse(fpsdte00 == "Certainly true ", 2, fpsdte00))))

data <- data %>%
  mutate(fcbrkn00 = ifelse(fcbrkn00 == "Never  ", 1, 
                           ifelse(fcbrkn00 == "Some days, but not all days    ", 2, 
                                  ifelse(fcbrkn00 == "Every day      ", 3, fcbrkn00)))) 
data <- data %>%
  mutate(fcfrut00 = ifelse(fcfrut00 == "Never  ", 1, 
                           ifelse(fcfrut00 == "Some days, but not all days    ", 2, 
                                  ifelse(fcfrut00 == "Every day      ", 3, fcfrut00)))) %>%
  mutate(fcvegi00 = ifelse(fcvegi00 == "Never  ", 1, 
                           ifelse(fcvegi00 == "Some days, but not all days    ", 2, 
                                  ifelse(fcvegi00 == "Every day      ", 3, fcvegi00))))

write.csv(file = "1_3_prep_mcs_data.csv", data)
