library(tidyverse)
library(tidymodels)
tidymodels_prefer()
library(lubridate)
library(naniar)

covid <- read_rds("data/tidied_covid.rds")

# Set a seed
set.seed(404)

# Splitting data
covid_split <- initial_split(covid, .8, strata = patient_type)
covid_train <- training(covid_split)
covid_test <- testing(covid_split)

# Checking dimensions are correct
dim(covid_train)[1] + dim(covid_test)[1]
dim(covid)

# Creating folds for V-fold cross-validation
covid_folds <- vfold_cv(covid_train, v = 10, repeats = 3)

# Creating a recipe (to be repeated in further R scripts)
recipe <- recipe(patient_type ~ ., data = covid_train) %>% 
  step_rm(pregnant, intubed, icu) %>% 
  step_impute_mode(all_factor_predictors()) %>%
  step_dummy(all_factor_predictors()) %>% 
  step_scale(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  prep()

bake(recipe, new_data = NULL) %>% 
  skimr::skim_without_charts()
