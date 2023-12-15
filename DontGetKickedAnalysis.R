library(tidyverse)
library(tidymodels)
library(vroom)
library(timetk)
library(patchwork)
library(modeltime) #Extensions of tidymodels to time series
library(timetk) #Some nice time series functions
library(forecast)

trainOG <- vroom("./STAT348/dont-get-kicked/DontGetKicked/training.csv", na=c("", "NA", "NULL", "NOT AVAIL")) 
testOG <- vroom("./STAT348/dont-get-kicked/DontGetKicked/test.csv", na=c("", "NA", "NULL", "NOT AVAIL"))



smalltrain <- head(trainOG,500)
train <- smalltrain %>%
  mutate(IsBadBuy = as.factor(IsBadBuy))




penlog_recipe <- recipe(IsBadBuy ~., data = train) %>%
  update_role(RefId, new_role = 'ID') %>%
  update_role_requirements("ID", bake = FALSE) %>%
  step_mutate(IsBadBuy = factor(IsBadBuy), skip = TRUE) %>%
  step_mutate(IsOnlineSale = factor(IsOnlineSale)) %>%
  step_mutate_at(all_nominal_predictors(), fn = factor) %>%
  #step_mutate_at(contains("MMR"), fn = numeric) %>%
  step_rm(contains('MMR')) %>%
  step_rm(BYRNO, WheelTypeID, VehYear, VNST, VNZIP1, PurchDate, # these variables don't seem very informative, or are repetitive
          AUCGUART, PRIMEUNIT, # these variables have a lot of missing values
          Model, SubModel, Trim) %>% # these variables have a lot of levels - could also try step_other()
  step_corr(all_numeric_predictors(), threshold = .7) %>%
  step_other(all_nominal_predictors(), threshold = .0001) %>%
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_impute_median(all_numeric_predictors())

# prep <- prep(penlog_recipe)
# baked <- bake(prep, new_data = train)
# View(baked)
# dim(baked)


penlog_model <- logistic_reg(mixture=tune(),penalty=tune()) %>%
  set_engine("glmnet")

penlog_wf <- workflow() %>%
  add_recipe(penlog_recipe) %>%
  add_model(penlog_model)


L <- 10
## Grid of values to tune over
penlog_tuning_grid <- grid_regular(penalty(),
                                   mixture(),
                                   levels = L) ## L^2 total tuning possibilities


K <- 10
## Split data for CV
folds <- vfold_cv(train, v = K, repeats=1)


## Run the CV
penlog_CV_results <- penlog_wf %>%
  tune_grid(resamples=folds,
            grid=penlog_tuning_grid,
            metrics=metric_set(gain_capture)) #Or leave metrics NULL

## Find Best Tuning Parameters
penlog_bestTune <- penlog_CV_results %>%
  select_best("gain_capture")

## Finalize the Workflow & fit it
penlog_final_wf <- penlog_wf %>%
  finalize_workflow(penlog_bestTune) %>%
  fit(data=train)

## Predict
penlog_pred <- penlog_final_wf %>%
  predict(new_data = testOG, type="prob") %>%
  bind_cols(.,testOG) %>% # bind predictions with test data
  select(RefId, .pred_1) %>% # Just keep datetime and predictions
  rename(IsBadBuy = .pred_1) # rename pred to count (for submission to Kaggle)

penlog_pred
# vroom_write(penlog_pred, "penlog_pred.csv", delim = ',')














bart_recipe <- recipe(IsBadBuy ~., data = train) %>%
  update_role(RefId, new_role = 'ID') %>%
  update_role_requirements("ID", bake = FALSE) %>%
  step_mutate(IsBadBuy = factor(IsBadBuy), skip = TRUE) %>%
  step_mutate(IsOnlineSale = factor(IsOnlineSale)) %>%
  step_mutate_at(all_nominal_predictors(), fn = factor) %>%
  #step_mutate_at(contains("MMR"), fn = numeric) %>%
  step_rm(contains('MMR')) %>%
  step_rm(BYRNO, WheelTypeID, VehYear, VNST, VNZIP1, PurchDate, # these variables don't seem very informative, or are repetitive
          AUCGUART, PRIMEUNIT, # these variables have a lot of missing values
          Model, SubModel, Trim) %>% # these variables have a lot of levels - could also try step_other()
  step_corr(all_numeric_predictors(), threshold = .7) %>%
  step_other(all_nominal_predictors(), threshold = .0001) %>%
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_impute_median(all_numeric_predictors())

bart_model <- bart(
  mode = "classification",
  engine = "dbarts",
  trees = 500,
  #prior_terminal_node_coef = NULL,
  #prior_terminal_node_expo = NULL,
  #prior_outcome_range = NULL
)

bart_wf <- workflow() %>%
  add_model(bart_model) %>%
  add_recipe(bart_recipe) %>%
  fit(data=train)


bart_pred <- predict(bart_wf, new_data=testOG, type = "prob") %>%
  bind_cols(.,testOG) %>% # bind predictions with test data
  select(RefId, .pred_1) %>% # Just keep datetime and predictions
  rename(IsBadBuy = .pred_1) # rename pred to count (for submission to Kaggle)

bart_pred

vroom_write(bart_pred, "bart_pred.csv", delim = ',')












# recipe making
my_recipe <- recipe(IsBadBuy ~., data = train) %>%
  update_role(RefId, new_role = 'ID') %>%
  update_role_requirements("ID", bake = FALSE) %>%
  step_mutate(IsBadBuy = factor(IsBadBuy), skip = TRUE) %>%
  step_mutate(IsOnlineSale = factor(IsOnlineSale)) %>%
  step_mutate_at(all_nominal_predictors(), fn = factor) %>%
  #step_mutate_at(contains("MMR"), fn = numeric) %>%
  step_rm(contains('MMR')) %>%
  step_rm(BYRNO, WheelTypeID, VehYear, VNST, VNZIP1, PurchDate, # these variables don't seem very informative, or are repetitive
          AUCGUART, PRIMEUNIT, # these variables have a lot of missing values
          Model, SubModel, Trim) %>% # these variables have a lot of levels - could also try step_other()
  step_corr(all_numeric_predictors(), threshold = .7) %>%
  step_other(all_nominal_predictors(), threshold = .0001) %>%
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_lencode_mixed(all_nominal_predictors(), outcome = vars(IsBadBuy)) %>%
  step_impute_median(all_numeric_predictors())



