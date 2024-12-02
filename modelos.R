library(tidymodels)
library(readxl)
library(doParallel)
library(finetune)
library(bonsai)

# Configurar paralelización
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

# Cargar y procesar datos
df <- read_excel("./data/trayectorias.xlsx") %>%
  select(-c(termina, gol, tiro, termina_tiro, participa_golero,
            xG, zona_fin, x_fin, presion, match_id, team.name,
            possession_team.name, possession_team.id)) %>%
  mutate(
    exito = factor(exito, levels = c(1, 0), labels = c("1", "0")),
    grupo = ifelse(x_inicio < 50, "Grupo A", "Grupo B")
  )

set.seed(123)
df_split <- initial_split(df)
train <- training(df_split)
test <- testing(df_split)

# Crear receta
df_recipe <- recipe(exito ~ ., data = train) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())

# Especificar modelos
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

# Crear conjunto de workflows
workflow_set <- workflow_set(
  preproc = list(base_recipe = df_recipe),
  models = model_specs
)

# Definir validación cruzada
cv_folds <- vfold_cv(train, v = 5, strata = exito)

# Definir métricas deseadas
metrics <- metric_set(accuracy, roc_auc, sens, yardstick::spec)

# Configurar control
control <- control_race(
  save_pred = TRUE,
  parallel_over = "everything",
  save_workflow = TRUE
)

# Ejecutar tuning
set.seed(123)
race_results <- workflow_set %>%
  workflow_map(
    "tune_race_anova",
    seed = 123,
    resamples = cv_folds,
    grid = 20,
    metrics = metrics,  # Agregar métricas personalizadas
    control = control
  )

# Crear carpetas para guardar modelos y resultados
model_save_path <- "./models"
tune_save_path <- "./tune_results"
dir.create(model_save_path, showWarnings = FALSE)
dir.create(tune_save_path, showWarnings = FALSE)

# Guardar resultados y modelos ajustados
lapply(
  race_results$wflow_id,
  function(model_id) {
    # Extraer resultados del tuning
    tuning_results <- race_results %>%
      extract_workflow_set_result(model_id)
    
    # Guardar resultados del tuning
    saveRDS(tuning_results, file = file.path(tune_save_path, paste0(model_id, "_tune_results.rds")))
    
    # Seleccionar mejores parámetros
    best_params <- tuning_results %>%
      select_best(metric = "roc_auc")
    
    # Ajustar modelo final
    model <- race_results %>%
      extract_workflow(model_id) %>%
      finalize_workflow(best_params) %>%
      fit(data = train)
    
    # Guardar modelo ajustado
    saveRDS(model, file = file.path(model_save_path, paste0(model_id, "_model.rds")))
    message(paste("Modelo y tuning de", model_id, "guardados correctamente."))
  }
)

# Guardar datos marcados como train/test
train <- train %>% mutate(dataset = "train")
test <- test %>% mutate(dataset = "test")
df_marked <- bind_rows(train, test)
write.csv(df_marked, "./data/data.csv", row.names = FALSE)

message("Datos marcados y guardados en './data/data.csv'.")
