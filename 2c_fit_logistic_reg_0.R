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

lr_model0 <- logistic_reg(
  penalty = tune(), 
  mixture = 0, 
  mode = "classification"
) %>% 
  set_engine("glmnet")

# Building workflows

lr_workflow0 <- workflow() %>% 
  add_model(lr_model0) %>% 
  add_recipe(recipe)

# Model tuning-----

# I'll use 10 candidate values in a simple tibble to tune the `penalty` hyperparameter
# Technique found here: https://www.tidymodels.org/start/case-study/#first-model.

lr_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 10))

#lr_tuned0 <- 
 # lr_workflow0 %>% 
  #tune_grid(covid_folds,
   #         grid = lr_reg_grid,
    #        control = control_grid(save_pred = TRUE),
     #       metrics = metric_set(roc_auc, accuracy))


# Assessing model

lr_tuned0 <- readRDS("fitted/lr_tuned0.rds")

lr_best0 <- select_best(lr_tuned0, metric = "accuracy")

# Fitting our tuned models-----

lr_workflow_tuned0 <- lr_workflow0 %>% 
  finalize_workflow(lr_best0)

show_best(lr_tuned0, metric = "accuracy") %>% select(-.estimator)

