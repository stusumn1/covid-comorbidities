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
covid_folds <- vfold_cv(covid_train, v = 5, repeats = 3)

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
lr_model <- logistic_reg(
  penalty = tune(), 
  mixture = 1, 
  mode = "classification"
  ) %>% 
  set_engine("glmnet")


# Building workflows
lr_workflow <- workflow() %>% 
  add_model(lr_model) %>% 
  add_recipe(recipe)

# Model tuning-----

# I'll use 10 candidate values in a simple tibble to tune the `penalty` hyperparameter
# Technique found here: https://www.tidymodels.org/start/case-study/#first-model.

lr_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 10))

#lr_tuned <- 
 # lr_workflow %>% 
  #tune_grid(covid_folds,
   #         grid = lr_reg_grid,
    #        control = control_grid(save_pred = TRUE),
     #       metrics = metric_set(roc_auc, accuracy))

lr_tuned <- readRDS(file = "fitted/lr_tuned.rds")

# Assessing model

lr_res <- show_best(lr_tuned, metric = "accuracy") %>% select(-.estimator)

lr_best <- select_best(lr_tuned, metric = "accuracy")

show_best(lr_tuned, metric = "accuracy") %>% select(-.estimator)
