## RF por separado para mundial Masculino y Femenino
library(tidymodels)
library(readxl)
library(doParallel)
library(finetune)
library(bonsai)
library(vip)
library(DALEXtra)
library(here)

df_M <- read_excel("./data/trayectorias.xlsx") %>%
  select(-c(termina, gol, tiro, termina_tiro, participa_golero,
            xG, zona_fin, x_fin, presion, match_id,
            possession_team.name, possession_team.id)) %>%
  mutate(
    exito = factor(exito, levels = c(1, 0), labels = c("1", "0"))  ) %>% 
  filter(mundial=='M') %>% select(-team.name,-mundial)
df_F <- read_excel("./data/trayectorias.xlsx") %>%
  select(-c(termina, gol, tiro, termina_tiro, participa_golero,
            xG, zona_fin, x_fin, presion, match_id,
            possession_team.name, possession_team.id)) %>%
  mutate(
    exito = factor(exito, levels = c(1, 0), labels = c("1", "0"))  ) %>% 
  filter(mundial=='F')  %>% select(-team.name,-mundial)

set.seed(123)
df_split_M <- initial_split(df_M)
train_M <- training(df_split_M)
test_M <- testing(df_split_M)

df_split_F <- initial_split(df_F)
train_F <- training(df_split_F)
test_F <- testing(df_split_F)

df_recipe_M <- recipe(exito ~ ., data = train_M) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())
df_recipe_F <- recipe(exito ~ ., data = train_F) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())

model_specs <- list(

  random_forest = rand_forest(mtry = tune(), min_n = tune(), trees = 500) %>%
    set_engine("ranger", importance = "impurity") %>%
    set_mode("classification") ,
  
  lightgbm = boost_tree(
    trees = tune(), learn_rate = tune(), tree_depth = tune(),
    min_n = tune(), loss_reduction = tune()
  ) %>%
    set_engine("lightgbm") %>%
    set_mode("classification")

)

workflow_set_M <- workflow_set(
  preproc = list(recipe1 = df_recipe_M#,
                 #recipe2 = df_recipe_F
                 ),
  models = model_specs)
workflow_set_F <- workflow_set(
  preproc = list(#recipe1 = df_recipe_M
                 recipe2 = df_recipe_F
  ),
  models = model_specs)

cv_folds_M <- vfold_cv(train_M, v = 5, strata = exito)
cv_folds_F <- vfold_cv(train_F, v = 5, strata = exito)


metrics <- metric_set(accuracy, roc_auc, sens, yardstick::spec)

control <- control_race(
  save_pred = TRUE,
  parallel_over = "everything",
  save_workflow = TRUE)

set.seed(123)
race_results_M <- workflow_set_M %>%
  workflow_map(
    seed = 123,
    resamples = cv_folds_M,
    grid = 20,
    metrics = metrics,
    control = control, 
    verbose = TRUE
  )
race_results_F <- workflow_set_F %>%
  workflow_map(
    seed = 123,
    resamples = cv_folds_F,
    grid = 20,
    metrics = metrics,
    control = control,
    verbose = TRUE
  )

dir.create("./models", showWarnings = FALSE)

lapply(
  race_results_F$wflow_id,
  function(model_id) {
    tuning_results <- race_results_F %>%
      extract_workflow_set_result(model_id)
    
    saveRDS(tuning_results, file = file.path("./models", paste0(model_id, "_tune_results.rds")))
    
    best_params <- tuning_results %>%
      select_best(metric = "roc_auc")
    
    model <- race_results_F %>%
      extract_workflow(model_id) %>%
      finalize_workflow(best_params) %>%
      fit(data = train_F)
    
    saveRDS(model, file = file.path("./models", paste0(model_id, "_model.rds")))
  }
)

best_params_M <- race_results_M %>%
  extract_workflow_set_result('recipe1_random_forest') %>% 
  select_best(metric = "roc_auc")
best_params_M <- race_results_M %>%
  extract_workflow_set_result('recipe1_lightgbm') %>% 
  select_best(metric = "roc_auc")
best_params_F <- race_results_F %>%
  extract_workflow_set_result('recipe2_random_forest') %>% 
  select_best(metric = "roc_auc")
best_params_F <- race_results_F %>%
  extract_workflow_set_result('recipe2_lightgbm') %>% 
  select_best(metric = "roc_auc")

random_forest_M <- race_results_M %>%
  extract_workflow('recipe1_random_forest') %>%
  finalize_workflow(best_params_M) %>%
  fit(data = train_M)
lightgbm_M <- race_results_M %>%
  extract_workflow('recipe1_lightgbm') %>%
  finalize_workflow(best_params_M) %>%
  fit(data = train_M)
random_forest_F <- race_results_F %>%
  extract_workflow('recipe2_random_forest') %>%
  finalize_workflow(best_params_F) %>%
  fit(data = train_F)
lightgbm_F <- race_results_F %>%
  extract_workflow('recipe2_lightgbm') %>%
  finalize_workflow(best_params_F) %>%
  fit(data = train_F)


train_M$exito_num <- as.numeric(train_M$exito) - 1
train_F$exito_num <- as.numeric(train_F$exito) - 1


explainer_rf1 <- explain_tidymodels(
  model = random_forest_M,
  data = train_M %>% select(-exito, -exito_num), 
  y = train_M$exito_num,
  label = "Random Forest",
  verbose = FALSE
)
explainer_lgbm1 <- explain_tidymodels(
  model = lightgbm_M,
  data = train_M %>% select(-exito, -exito_num), 
  y = train_M$exito_num,
  label = "Light GBM",
  verbose = FALSE
)
explainer_rf2 <- explain_tidymodels(
  model = random_forest_F,
  data = train_F %>% select(-exito, -exito_num), 
  y = train_F$exito_num,
  label = "Random Forest",
  verbose = FALSE
)
explainer_lgbm2 <- explain_tidymodels(
  model = lightgbm_F,
  data = train_F %>% select(-exito, -exito_num), 
  y = train_F$exito_num,
  label = "Light GBM",
  verbose = FALSE
)
random_forest_M = readRDS("./models/recipe1_random_forest_model.rds")
random_forest_F = readRDS("./models/recipe2_random_forest_model.rds")
random_forest_tune_results_M <- readRDS("./models/recipe1_random_forest_tune_results.rds")
random_forest_tune_results_F <- readRDS("./models/recipe2_random_forest_tune_results.rds")



rf_predictions_testM <- predict(random_forest_M, test_M) %>%
  bind_cols(test_M)

rf_conf_matrix_testM <- conf_mat(
  data = rf_predictions_testM,
  truth = exito,
  estimate = .pred_class
)
a<-autoplot(rf_conf_matrix_testM, type = "heatmap") +
  labs(
    title = "Matriz de Confusión - Random Forest (Mundial Masculino)",
    x = "Clase Predicha",
    y = "Clase Real"
  ) +
  theme_minimal()+theme(legend.position = 'none')
c<-random_forest_M %>%
  extract_fit_engine() %>%
  vip::vip(geom = "col", aesthetics = list(fill = "steelblue")) +
  labs(
    title = "Importancia de las Variables - Random Forest (Mundial Masculino)",
    x = "Importancia Relativa",
    y = "Variable"
  ) +
  theme_minimal()



rf_predictions_testF <- predict(random_forest_F, test_F) %>%
  bind_cols(test_F)

rf_conf_matrix_testF <- conf_mat(
  data = rf_predictions_testF,
  truth = exito,
  estimate = .pred_class
)
b<-autoplot(rf_conf_matrix_testF, type = "heatmap") +
  labs(
    title = "Matriz de Confusión - Random Forest (Mundial Femenino)",
    x = "Clase Predicha",
    y = "Clase Real"
  ) +
  theme_minimal()
d<-random_forest_F %>%
  extract_fit_engine() %>%
  vip::vip(geom = "col", aesthetics = list(fill = "steelblue")) +
  labs(
    title = "Importancia de las Variables - Random Forest (Mundial Femenino)",
    x = "Importancia Relativa",
    y = "Variable"
  ) +
  theme_minimal()
a+b
c+d



lgbm_predictions_testM <- predict(lightgbm_M, test_M) %>%
  bind_cols(test_M)

lgbm_conf_matrix_testM <- conf_mat(
  data = lgbm_predictions_testM,
  truth = exito,
  estimate = .pred_class
)
a<-autoplot(lgbm_conf_matrix_testM, type = "heatmap") +
  labs(
    title = "Matriz de Confusión - LGBM (Mundial Masculino)",
    x = "Clase Predicha",
    y = "Clase Real"
  ) +
  theme_minimal()+theme(legend.position = 'none')
c<-lightgbm_M %>%
  extract_fit_engine() %>%
  vip::vip(geom = "col", aesthetics = list(fill = "steelblue")) +
  labs(
    title = "Importancia de las Variables - LGBM (Mundial Masculino)",
    x = "Importancia Relativa",
    y = "Variable"
  ) +
  theme_minimal()



lgbm_predictions_testF <- predict(lightgbm_F, test_F) %>%
  bind_cols(test_F)

lgbm_conf_matrix_testF <- conf_mat(
  data = lgbm_predictions_testF,
  truth = exito,
  estimate = .pred_class
)
b<-autoplot(lgbm_conf_matrix_testF, type = "heatmap") +
  labs(
    title = "Matriz de Confusión - LGBM (Mundial Femenino)",
    x = "Clase Predicha",
    y = "Clase Real"
  ) +
  theme_minimal()
d<-lightgbm_F %>%
  extract_fit_engine() %>%
  vip::vip(geom = "col", aesthetics = list(fill = "steelblue")) +
  labs(
    title = "Importancia de las Variables - LGBM (Mundial Femenino)",
    x = "Importancia Relativa",
    y = "Variable"
  ) +
  theme_minimal()
a+b
c+d






model_specs2 <- list(

  lightgbm = boost_tree(
    trees = tune(), learn_rate = tune(), tree_depth = tune(),
    min_n = tune(), loss_reduction = tune()
  ) %>%
    set_engine("lightgbm") %>%
    set_mode("classification")
  
)
workflow_set_M <- workflow_set(
  preproc = list(recipe1 = df_recipe_M#,
                 #recipe2 = df_recipe_F
  ),
  models = model_specs2)
workflow_set_F <- workflow_set(
  preproc = list(#recipe1 = df_recipe_M
    recipe2 = df_recipe_F
  ),
  models = model_specs2)
