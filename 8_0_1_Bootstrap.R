library(tidyverse)
library(here)
library(parallel)
library(heplots)

resultsframe <- function(x_var, y_var) {
  
  # Setup Specification Names
  levels_x <- x_var
  levels_y <- y_var
  levels_cont <- c("No Controls", "Controls")
  levels_comb <- c("Mean", "1 or more")
  
  # Calculate Number of Combinations/Analyses to run
  combinations <-
    length(levels_x) * length(levels_y) * length(levels_cont) * length(levels_comb)
  
  # Setup results frame
  results_frame <-
    data.frame(matrix(NA, nrow = combinations, ncol = 4))
  colnames(results_frame) <-
    c(
      "x_variable",
      "y_variable",
      "controls",
      "combination"
    )
  
  # Write combinations into results frame
  results_frame$x_variable <- rep(levels_x, each = nrow(results_frame) / length(levels_x))
  results_frame$y_variable <- rep(rep(levels_y, each = nrow(results_frame) / (length(levels_x) * length(levels_y))), times = length(levels_x))
  results_frame$controls <- rep(rep(rep(levels_cont,each = nrow(results_frame) / (length(levels_x) * length(levels_y) * length(levels_cont))), times = length(levels_x)), times = length(levels_y))
  results_frame$combination <- rep(rep(rep(rep(levels_comb,each = nrow(results_frame) / (length(levels_comb) * length(levels_x) * length(levels_y) * length(levels_cont))), times = length(levels_x)), times = length(levels_cont)))
  return(results_frame)
}

SCA_single <- function(study, x_var, y_var, controls, combination, data, mc.cores = 8L){
  # combination of well being variables
  if(combination == "Mean"){
    data$dv <- data %>%
      select(y_var) %>%
      rowMeans(na.rm = FALSE)
    print(1)
  } else {
    d1 <-
      data %>%
      select(y_var) %>%
      apply(1, sum)
    data$dv <- ifelse(d1 == 0, 0, ifelse(is.na(d1), NA, 1))
  }
  # tech
  data$iv <- data %>%
    select(x_var)
  print(2)
  #################################################
  # Run Correlations
  #################################################
  
  if (controls == "No Controls") {
    reg <- lm(scale(dv) ~ scale(iv), data = data)
  } else if (controls == "Controls") {
    if (study == "yrbs"){
      reg <-
        lm(scale(dv) ~ scale(iv) + scale(race_di),
           data = data)
    } else if (study == "mcs"){
      reg <- lm(
        scale(dv) ~ scale(iv) + scale(edumot) +
          scale(fd06e00) + scale(clpar) + scale(fcpaab00) +
          scale(fpwrdscm) + scale(fdacaq00) + scale(fd05s00) + 
          scale(fpclsi00) + scale(fpchti00) + scale(fdkessl), #+ scale(fdtots00) +
        #scale(foede000),
        data = data)
    }
  }
  #################################################
  # Extract Variables
  #################################################
  coef <- summary(reg)$coef[[2,1]] %>% ifelse(. == 0, NA, .)
  t_value <- summary(reg)$coef[[2,3]] %>% ifelse(. == 0, NA, .)
  number <- nobs(reg)
  p_value <- summary(reg)$coef[[2,4]]
  standard_error <-
    summary(reg)$coef[[2, 2]] %>% ifelse(. == 0, NA, .)
  rsqrd <- etasq(reg)["scale(iv)", "Partial eta^2"]
  
  return(list(coef, t_value, number, p_value, standard_error, rsqrd))
}


SCA_full <- function(study, data, core = 8){
  if(study == "yrbs"){
    ######## yrbs
    x_variables <- c("q80_n", "q81_n", "tech")
    y <-  c("q25_n", "q26_n", "q27_n", "q28_nd", "q29_nd")
    y_variables <-
      (do.call("c", lapply(seq_along(y), function(i)
        combn(y, i, FUN = base::list))))
    c_variables <- c("Means", "Choose 1")
    controls <- c("race_di")
    data <- data[, c(
      "q80_n", "q81_n", "tech", "q25_n", "q26_n", "q27_n", "q28_nd", "q29_nd", "year", "race_di"
    )]
  } else if (study == "mcs"){
    ######## mcs
    x_variables <-
      c("fctvho00r", "fccomh00r", "fccmex00r", "fcinth00r", "fcsome00r", "tech")
    y <-
      c(
        "fcmdsa00r", "fcmdsb00r", "fcmdsc00r", "fcmdsd00r", "fcmdse00r", "fcmdsf00r", "fcmdsg00r",
        "fcmdsh00r", "fcmdsi00r", "fcmdsj00r", "fcmdsk00r", "fcmdsl00r", "fcmdsm00r", "fcsati00r",
        "fcgdql00r", "fcdowl00r", "fcvalu00r", "fcgdsf00r", "fcscwk00r", "fcwylk00r", "fcfmly00r",
        "fcfrns00r", "fcschl00r", "fclife00r"
      )
    y_variables <-
      (do.call("c", lapply(seq_along(y), function(i)
        combn(y, i, FUN = base::list))))
    y_variables <-
      sample(y_variables[-(1:length(y))], 806, replace = FALSE)
    controls <-
      c(
        "edumot", "fd06e00", "clpar", "fcpaab00", "fpwrdscm", "fdacaq00", "fd05s00", "fpclsi00", # nn
        "fpchti00", "fdkessl" 
      )
    data <-
      data[, c(
        "fctvho00r", "fccomh00r", "fccmex00r", "fcinth00r", "fcsome00r", "fcmdsa00r", "fcmdsb00r",
        "fcmdsc00r", "fcmdsd00r", "fcmdse00r", "fcmdsf00r", "fcmdsg00r", "fcmdsh00r", "fcmdsi00r",
        "fcmdsj00r", "fcmdsk00r", "fcmdsl00r", "fcmdsm00r", "fcsati00r", "fcgdql00r", "fcdowl00r",
        "fcvalu00r", "fcgdsf00r", "fcscwk00r", "fcwylk00r", "fcfmly00r", "fcfrns00r", "fcschl00r",
        "fclife00r", "edumot", "fd06e00", "clpar", "fcpaab00", "fpwrdscm", "fdacaq00", "fd05s00",
        "fpwrdscm", "fpclsi00", "fpchti00", "fdkessl",
        #"fdtots00",
        #"foede000",
        "tech"
      )]
  }
  r1 <- resultsframe(x_var = x_variables, y_var = y_variables)
  dupdata <- list(data)[rep(1, nrow(r1))]
  l <- mcmapply(FUN = SCA_single, 
                rep(study, nrow(r1)),
                x_var = r1$x_variable, 
                y_var = r1$y_variable, 
                controls = r1$controls, 
                combination = r1$combination, 
                data = dupdata, mc.cores = core)
  data <- bind_cols(r1, 
                    coef = unlist(l[1, ]), 
                    t_value = unlist(l[2, ]), 
                    number = unlist(l[3, ]), 
                    p_value = unlist(l[4, ]), 
                    se = unlist(l[5, ]), 
                    rsqrd = unlist(l[6,]))
  return(data)
}

data <- read.csv(here("1_1_prep_yrbs_data.csv"), header = TRUE, sep = ",")
SCA_result_yrbs <- SCA_full("yrbs", data)
SCA_result_yrbs %>% summarize(median = median(coef))

data <- read_csv(here("1_3_prep_mcs_data.csv"), guess_max = 80000)
system.time(SCA_result_mcs <- SCA_full("mcs", data))
SCA_result_mcs %>% summarize(median = median(coef))


boot_SCA <- function(study, data, boot_num, cores = 8){
  boots <- mcmapply(FUN = sample, list(1:nrow(data))[rep(1, boot_num)], rep(nrow(data), boot_num), replace = TRUE)
  if (study == "yrbs"){
    SCA_boot_result <- data.frame(median_coef = rep(0, boot_num), 
                                  median_q80_n_coef = rep(0, boot_num), 
                                  median_q81_n_coef = rep(0, boot_num),
                                  median_tech_coef = rep(0, boot_num),
                                  dom_share = rep(0, boot_num), 
                                  dom_share_q80_n = rep(0, boot_num), 
                                  dom_share_q81_n = rep(0, boot_num),
                                  dom_share_tech = rep(0, boot_num),
                                  dom_sig_share = rep(0, boot_num), 
                                  dom_sig_share_q80_n = rep(0, boot_num),
                                  dom_sig_share_q81_n = rep(0, boot_num), 
                                  dom_sig_share_tech = rep(0, boot_num)
    )
  } else if (study == "mcs"){
    SCA_boot_result <- data.frame(median_coef = rep(0, boot_num), 
                                  median_q80_n_coef = rep(0, boot_num), 
                                  median_q81_n_coef = rep(0, boot_num),
                                  median_tech_coef = rep(0, boot_num),
                                  dom_share = rep(0, boot_num), 
                                  dom_share_q80_n = rep(0, boot_num), 
                                  dom_share_q81_n = rep(0, boot_num),
                                  dom_share_tech = rep(0, boot_num),
                                  dom_sig_share = rep(0, boot_num), 
                                  dom_sig_share_q80_n = rep(0, boot_num),
                                  dom_sig_share_q81_n = rep(0, boot_num), 
                                  dom_sig_share_tech = rep(0, boot_num)
    )
  }
  for (i in 1:boot_num){
    boot_index <- boots[,i]
    data <- data[boot_index,]
    SCA_boot_one <- SCA_full(study, data)
    if (study == "yrbs"){
      SCA_boot_one_q80_n <- SCA_boot_one %>% filter(x_variable == "q80_n")
      SCA_boot_one_q81_n <- SCA_boot_one %>% filter(x_variable == "q81_n")
      SCA_boot_one_tech <- SCA_boot_one %>% filter(x_variable == "tech")
      
      SCA_boot_result[i, "median_coef"] <- median(SCA_boot_one$coef)
      SCA_boot_result[i, "median_q80_n_coef"] <- median(SCA_boot_one_q80_n$coef)
      SCA_boot_result[i, "median_q81_n_coef"] <- median(SCA_boot_one_q81_n$coef)
      SCA_boot_result[i, "median_tech_coef"] <- median(SCA_boot_one_tech$coef)
      
      dom_sign = "-1"
      SCA_boot_result[i, "dom_share"] <- sum(sign(SCA_boot_one$coef) == -1)/nrow(SCA_boot_one)
      SCA_boot_result[i, "dom_share_q80_n"] <- sum(sign(SCA_boot_one_q80_n$coef) == -1)/nrow(SCA_boot_one_q80_n)
      SCA_boot_result[i, "dom_share_q81_n"] <- sum(sign(SCA_boot_one_q81_n$coef) == -1)/nrow(SCA_boot_one_q81_n)
      SCA_boot_result[i, "dom_share_tech"] <- sum(sign(SCA_boot_one_tech$coef) == -1)/nrow(SCA_boot_one_tech)
      
      SCA_boot_result[i, "dom_sig_share"] <- sum(sign(SCA_boot_one$coef) == -1 & SCA_boot_one$p_value < 0.05)/nrow(SCA_boot_one)
      SCA_boot_result[i, "dom_sig_share_q80_n"] <- sum(sign(SCA_boot_one_q80_n$coef) == -1 & SCA_boot_one$p_value < 0.05)/nrow(SCA_boot_one_q80_n)
      SCA_boot_result[i, "dom_sig_share_q81_n"] <- sum(sign(SCA_boot_one_q81_n$coef) == -1 & SCA_boot_one$p_value < 0.05)/nrow(SCA_boot_one_q81_n)
      SCA_boot_result[i, "dom_sig_share_tech"] <- sum(sign(SCA_boot_one_tech$coef) == -1 & SCA_boot_one$p_value < 0.05)/nrow(SCA_boot_one_tech)
    }
  }
  return(SCA_boot_result)
}
system.time(boot_test <- boot_SCA("yrbs", data, 10))
write_csv(boot_test, here("data"))