library(here)
here()
library(tidyverse)

grade8_08_1 <-   read.spss(
  here("data", "MTF", "2008-grade8-form1.sav"),
  use.value.labels = FALSE,
  to.data.frame = TRUE,
  use.missings = TRUE
)
grade8_08_2 <-   read.spss(
  here("data", "MTF", "2008-grade8-form2.sav"),
  use.value.labels = FALSE,
  to.data.frame = TRUE,
  use.missings = TRUE
)
grade8_08_3 <-   read.spss(
  here("data", "MTF", "2008-grade8-form3.sav"),
  use.value.labels = FALSE,
  to.data.frame = TRUE,
  use.missings = TRUE
)
grade8_08_4 <-   read.spss(
  here("data", "MTF", "2008-grade8-form4.sav"),
  use.value.labels = FALSE,
  to.data.frame = TRUE,
  use.missings = TRUE
)
grade10_08_1 <-   read.spss(
  here("data", "MTF", "2008-grade10-form1.sav"),
  use.value.labels = FALSE,
  to.data.frame = TRUE,
  use.missings = TRUE
)
grade10_08_2 <-   read.spss(
  here("data", "MTF", "2008-grade10-form2.sav"),
  use.value.labels = FALSE,
  to.data.frame = TRUE,
  use.missings = TRUE
)
grade10_08_3 <-   read.spss(
  here("data", "MTF", "2008-grade10-form3.sav"),
  use.value.labels = FALSE,
  to.data.frame = TRUE,
  use.missings = TRUE
)
grade10_08_4 <-   read.spss(
  here("data", "MTF", "2008-grade10-form4.sav"),
  use.value.labels = FALSE,
  to.data.frame = TRUE,
  use.missings = TRUE
)

grade8_08 <- left_join(grade8_08_1, grade8_08_2, by = c("CASEID", "V1", "V3", "V4", "V5", "V507", "V508", "V509")) %>%
  left_join(grade8_08_3, by = "CASEID") %>%
  left_join(grade8_08_4, by = "CASEID") %>%
  mutate(year = 2008, V501 = 8) %>%
  select(V1, V501, )
  select(V1, V501, V7326, V7325, V7381, V7552, V7544, V7551, V7553, V7589, V7590, V7562, V7563, 
         V7255, V7302, V8512, V8502, V8505, V8509, V8514, V8536, V8504, V8501, V8508, V8507, 
         V8511, V7501, V7502, V7504, V7507, V7508, V7202, V8503, V8502, 
         V8513, V8531, V1070, V7505, V7208, V7216, V7217, V7329, V7221, V7254)

  
grade10_08 <- full_join(grade10_08_1, grade10_08_2, by = "CASEID") %>%
  full_join(grade10_08_3, by = "CASEID") %>%
  full_join(grade10_08_4, by = "CASEID") %>%
  mutate(year = 2008, V501 = 10)

rbind(grade8_08, grade10_08)
