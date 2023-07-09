library(tidyverse)
library(naniar)
library(lubridate)


covid <- read_csv("data/original_covid.csv") %>% 
  # Clean up names and replace NA values with `NA`
  janitor::clean_names() %>% 
  replace_with_na(replace = list(date_died = "9999-99-99", intubed = 99)) %>% 
  # Factorize the relevant variables
  mutate(
    age = age,
    usmer = factor(usmer, levels = c(1, 2)),
    sex = factor(sex, levels = c(1, 2), labels = c("F", "M")),
    patient_type = factor(patient_type, levels = c(1, 2), labels = c("home", "hospitalized")),
    date_died = dmy(date_died),
    died = factor(ifelse(is.na(date_died), "No", "Yes")),
    intubed = factor(intubed, levels = c(1, 97), labels = c("Yes", "No")),
    pneumonia = factor(pneumonia, levels = c(1, 2), labels = c("Yes", "No")),
    pregnant = factor(pregnant, levels = c(1, 2), labels = c("Yes", "No")),
    diabetes = factor(diabetes, levels = c(1, 2), labels = c("Yes", "No")),
    copd = factor(copd, levels = c(1, 2), labels = c("Yes", "No")),
    asthma = factor(asthma, levels = c(1, 2), labels = c("Yes", "No")),
    inmsupr = factor(inmsupr, levels = c(1, 2), labels = c("Yes", "No")),
    hipertension = factor(hipertension, levels = c(1, 2), labels = c("Yes", "No")),
    other_disease = factor(other_disease, levels = c(1, 2), labels = c("Yes", "No")),
    cardiovascular = factor(cardiovascular, levels = c(1, 2), labels = c("Yes", "No")),
    obesity = factor(obesity, levels = c(1, 2), labels = c("Yes", "No")),
    renal_chronic = factor(renal_chronic, levels = c(1, 2), labels = c("Yes", "No")),
    tobacco = factor(tobacco, levels = c(1, 2), labels = c("Yes", "No")),
    clasiffication_final = factor(clasiffication_final, levels = c(1, 2, 3, 4, 5, 6, 7)),
    clasiffication_final = fct_collapse(clasiffication_final,
                                        `1` = "1",
                                        `2` = "2",
                                        `3` = "3",
                                        None = c("4", "5", "6", "7")),
    icu = factor(icu, levels = c(1, 2), labels = c("Yes", "No"))
  ) %>% 
  # Remove problematic variables (due to missingness)
  select(-date_died, -medical_unit, -usmer)

head(covid)

# Saving as RDS to preserve column types
saveRDS(covid, file = "data/tidied_covid.rds")
