library(tidymodels)
library(readxl)
library(doParallel)
library(finetune)
library(bonsai)
library(vip)
library(DALEXtra)
library(here)

registerDoParallel(cores = parallel::detectCores(logical = FALSE))

df <- read_excel("./data/trayectorias.xlsx") %>%
  mutate(
    exito = factor(exito, levels = c(1, 0), labels = c("1", "0"))
  )

set.seed(123)
df_split <- initial_split(df)
train <- training(df_split)
test <- testing(df_split)

df_recipe <- recipe(exito ~ ., data = train) %>%
  step_rm(termina, gol, tiro, termina_tiro, participa_golero,
          xG, zona_fin, x_fin, presion, match_id,
          possession_team.name, possession_team.id) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())

model_specs <- list(
  lasso = logistic_reg(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet"),
  
  random_forest = rand_forest(mtry = tune(), min_n = tune(), trees = 500) %>%
    set_engine("ranger", importance = "impurity") %>%
    set_mode("classification"),
  
  lightgbm = boost_tree(
    trees = tune(), learn_rate = tune(), tree_depth = tune(),
    min_n = tune(), loss_reduction = tune()
  ) %>%
    set_engine("lightgbm") %>%
    set_mode("classification"),
  
  decision_tree = decision_tree(
    cost_complexity = tune(), tree_depth = tune(), min_n = tune()
  ) %>%
    set_engine("rpart") %>%
    set_mode("classification")
)

workflow_set <- workflow_set(
  preproc = list(base_recipe = df_recipe),
  models = model_specs)

cv_folds <- vfold_cv(train, v = 5)

metrics <- metric_set(accuracy, roc_auc, sens, yardstick::spec)

control <- control_race(
  save_pred = TRUE,
  parallel_over = "everything",
  save_workflow = TRUE)

set.seed(123)
race_results <- workflow_set %>%
  workflow_map(
    seed = 123,
    resamples = cv_folds,
    grid = 20,
    metrics = metrics,
    control = control
  )

dir.create("./models", showWarnings = FALSE)

lapply(
  race_results$wflow_id,
  function(model_id) {
    tuning_results <- race_results %>%
      extract_workflow_set_result(model_id)
    
    saveRDS(tuning_results, file = file.path("./models", paste0(model_id, "_tune_results.rds")))
    
    best_params <- tuning_results %>%
      select_best(metric = "roc_auc")
    
    model <- race_results %>%
      extract_workflow(model_id) %>%
      finalize_workflow(best_params) %>%
      fit(data = train)
    
    saveRDS(model, file = file.path("./models", paste0(model_id, "_model.rds")))
  }
)

best_params <- race_results %>%
  extract_workflow_set_result('base_recipe_random_forest') %>% 
  select_best(metric = "roc_auc")

random_forest <- race_results %>%
  extract_workflow('base_recipe_random_forest') %>%
  finalize_workflow(best_params) %>%
  fit(data = train)

train$exito_num <- as.numeric(train$exito) - 1

explainer_rf <- explain_tidymodels(
  model = random_forest,
  data = train %>% select(-exito, -exito_num), 
  y = train$exito_num,
  label = "Random Forest",
  verbose = FALSE
)

vip_rf <- model_parts(explainer_rf)

saveRDS(vip_rf, file = "./models/vip_rf.rds")

train <- train %>% mutate(dataset = "train")
test <- test %>% mutate(dataset = "test")
df_marked <- bind_rows(train, test)


write.csv(df_marked, "./models/data.csv", row.names = FALSE)
