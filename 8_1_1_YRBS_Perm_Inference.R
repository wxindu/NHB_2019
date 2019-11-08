library(tidyverse)
library(here)
library(ggplot2)

perm_data <- read.csv(here("yrbs_permutation_frame.csv"))
load(here("2_1_sca_yrbs_results.rda"))

sign_share <- results_yrbs_sca %>%
  mutate(sign = sign(effect)) %>%
  group_by(sign) %>%
  summarize(n = n(), prop = n()/372)
# -1 is the dominant sign for the whole sca

sig_sign_share <- results_yrbs_sca %>%
  mutate(sign = sign(effect), sig = p_value < 0.05) %>%
  group_by(sign, sig) %>%
  summarize(n = n(), prop = n/372)

# share of the negative sign
perm_data %>%
  mutate(share_dom_sign = sign.neg.boot/(sign.neg.boot + sign.pos.boot)) %>%
  ggplot(aes(x = share_dom_sign)) + geom_density() + geom_vline(xintercept = pull(sign_share[1,3]), color = "red")

# share of the negative sign that are significant
perm_data %>%
  mutate(share_dom_sig_sign = sign.sig.neg.boot / 372) %>%
  ggplot(aes(x = share_dom_sig_sign)) + geom_density() + geom_vline(xintercept = pull(sig_sign_share[2,4]), color = "red")

