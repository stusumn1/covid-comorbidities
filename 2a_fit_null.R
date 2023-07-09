library(tidyverse)
library(tidymodels)
tidymodels_prefer()
library(lubridate)
library(naniar)

covid <- read_rds("data/tidied_covid.rds")

# Initial setup------
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
recipe <- recipe(patient_type ~ ., data = covid_train, strata = patient_type) %>% 
  step_rm(pregnant, intubed, icu) %>% 
  step_impute_mode(all_factor_predictors()) %>%
  step_dummy(all_factor_predictors()) %>% 
  step_scale(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  prep()

# Fitting logistic regression models-----

# Models
null_model <- null_model() %>% 
  set_engine("parsnip") %>% 
  set_mode("classification") 


# Building workflows
null_workflow <- workflow() %>% 
  add_model(null_model) %>% 
  add_recipe(recipe)


# Model tuning-----

null_fit <- fit_resamples(
  null_workflow,
  resamples = covid_folds
)

null_results <- null_fit %>% 
  collect_metrics()

null_results
