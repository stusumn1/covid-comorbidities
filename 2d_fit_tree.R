library(tidyverse)
library(tidymodels)
tidymodels_prefer()
library(lubridate)
library(naniar)
library(rpart)

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
tree_model <- decision_tree(
  mode = "classification",
  tree_depth = tune(),
  min_n = tune()
) %>% 
  set_engine("rpart")


# Building workflows
tree_workflow <- workflow() %>% 
  add_model(tree_model) %>% 
  add_recipe(recipe)

# Model tuning-----

tree_params <- extract_parameter_set_dials(tree_model)
tree_grid <- grid_regular(tree_params, levels = 5)

#tree_tuned <- 
 # tree_workflow %>% 
  #tune_grid(covid_folds,
   #         grid = tree_grid,
    #        control = control_grid(save_pred = TRUE),
     #       metrics = metric_set(accuracy))


tree_tuning_results <- readRDS("results/tree_tuning_results.rds")

tree_best <- select_best(tree_tuned, metric = "accuracy")
