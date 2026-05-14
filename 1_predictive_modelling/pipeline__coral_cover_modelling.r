# habitat modelling pipeline

source('functions.r')
source("machine_learning_functions.r")
library(sf)
library(usdm)

# ..... Constants ====
skip_model_training <- F

actual_var <- paste0(response_var_x,'.actual')
actual_var.short <- paste0(response_var_x.short,'.act')

predict_var <- paste0(response_var_x,'.predicted')
predict_var.short <- paste0(response_var_x.short,'.pred')



model_data <- model_data %>%
  mutate(model_obs_id = row_number()) %>%
  dplyr::select(any_of(c('model_obs_id', id_vars, grouping_vars_x, replicate_unit_var, response_var_x, features) %>% na.omit()))

id_vars <- c(id_vars,'model_obs_id') %>% unique()


# ..... Multiple runs ====
# .......... Run models ====

if(save_intermediates) {
  
  out_dir.temp <- paste0(out_dir,'TEMP/')
  
  if(!dir.exists(out_dir.temp)) { dir.create(out_dir.temp, recursive = T) }
  
  temp_files <- list.files(out_dir.temp)
  
  if(length(temp_files) == n_runs && overwrite.intermediates == F) {
    skip_model_training <- T
  }
  
}




cat(crayon::magenta('training models. data splits will be grouped by ',replicate_unit_var,
                   '. make sure this is correct.\n'))



if(!skip_model_training) {
  
  plan(multisession, workers = cores)
  
  xgb_models <- future_lapply(1:n_runs, 
                              future.stdout = F,
                              future.seed = T,
                              function(x) { 
  
  model_data_x <- model_data
  
  model_data_x <- model_data_x %>%
    mutate(across(any_of(features.numeric), ~as.numeric(.x)))
  
  if(save_intermediates) {
  
    out_file.temp <- paste0(out_dir.temp,'intermediate_model_results_',x,'.rds')
    
    if(file.exists(out_file.temp) &&
       save_intermediates &&
       overwrite.intermediates == F) {
      cat(crayon::magenta("file already exists, skipping\n"))
      return()
    }
    
  }
                             
  cat(crayon::green("running model ",x," of ", n_runs,'\n'))
  
  if(problem_class == 'classification') {
    
    if(balance_factors_bool) {
      model_data_x <- balance_factor_by_smallest_class(data = model_data_x,
                                                       class_var = response_var_x)
    }
    
  }


  model_data_x <- model_data_x %>%
    select(any_of(c(response_var_x, 'model_obs_id', id_vars, replicate_unit_var, features) %>% na.omit()))
  
  model_data_x <- model_data_x %>%
    drop_na()
  
  
  
  t_start <- Sys.time()
  print('setting up model for xgboost')
  xgb_data_list <- set_up_model_data(
    input_data = model_data_x,
    response_var = response_var_x,
    features = features,
    id_vars = id_vars,
    features.one_hot = features.one_hot,
    replicate_unit_var = replicate_unit_var,
    train_test_split_type = 'partition',
    train_watchlist_split_type = 'partition',
    response_type = response_type,
    train_test_split_val = train_test_split_val,
    train_watchlist_split_val = train_watchlist_split_val,
    split_function = split_data,
    force_min_max = T
  )
  
  d1 <- nrow(xgb_data_list[[1]])
  d2 <- nrow(xgb_data_list[[2]])
  d3 <- nrow(xgb_data_list[[3]])
  da <- d1+d2+d3
  
  cat(crayon::cyan("data split:",round(d1/da,2),"training,",round(d2/da,2),"watchlist,",round(d3/da,2),"testing\n"))
  
  
  
  # .......... Collinar variable groupings ====
  if(collinearity_filter) {

    VIFs <- vifstep(xgb_data_list$train %>% dplyr::select(any_of(features.numeric)) %>% as.data.frame(),
                    th = cutoff)
    
    non_collinear_features <- c(VIFs@results$Variables, features.numeric.hardcoded) %>% unique() %>% na.omit()
    
    collinear_features <- xgb_data_list$train %>% dplyr::select(any_of(features.numeric)) %>% names()
    collinear_features <- collinear_features[which(collinear_features %in% non_collinear_features == F)]
    
    
    plot_collinear_groups <- generate_non_collinear_groups(
      data = xgb_data_list$train,
      features = features.numeric,
      cutoff = 0,
      cutoff_type = cutoff_type
    )

    plot_collinear_groups <- generate_non_collinear_groups(
      data = xgb_data_list$train,
      features = features.numeric,
      cutoff = cutoff,
      cutoff_type = cutoff_type
    )
    
    features.numeric2 <- non_collinear_features
    
    features.one_hot2 <- xgb_data_list$features[which(str_detect(xgb_data_list$features, paste0(features.one_hot,collapse ='|')))]
    
    xgb_data_list$features <- c(features.numeric2, features.one_hot2, features.forced) %>% na.omit() %>% unique()
    
  } else {
    
  }
  
  
  # ...............  Generate weights ====
  if(exists('weighting_function')) {
    
    xgb_data_list$train <- weighting_function(xgb_data_list$train)
  
  }
  
  
  # ............... XGBoost Specific ====
  print('generating xgboost matrices')
  model_matrices.xgb <- convert_data_to_xgb_matrix(
    features = xgb_data_list$features,
    xgb_data_list = xgb_data_list[1:3],
    response_var = response_var_x,
    response_type = response_type,
    weights_var = if_else(exists('weighting_function'), '.weights', NA)
  )
  

  print('generating xgboost parameters')
  params <- set_up_parameters.xgboost(
    xgb_num_class = NULL,
    
    xgb_booster = xgb_booster,
    global_objective = global_objective,
    global_eval_metric = global_eval_metric,
    global_device = global_device,
    global_nthread = global_nthread,
    
    quantile_alpha = quantile_alpha,
    
    tweedie_variance_power = tweedie_variance_power,
    
    tree_max_depth = tree_max_depth,
    tree_eta = tree_eta,
    tree_subsample = tree_subsample,
    tree_lambda = tree_lambda,
    tree_alpha = tree_alpha,
    tree_num_parallel_tree = tree_num_parallel_tree,
    tree_method = tree_method,
    
    linear_lambda_bias = linear_lambda_bias,
    
    training_rounds = training_rounds,
    early_stopping_rounds = early_stopping_rounds,
    print_progress = print_progress,
    print_every_n = print_every_n
  )

  
  print('training xgboost model')
  xgboost_model <- train_model.xgboost(
    train_data = model_matrices.xgb$train,
    watchlist_data = model_matrices.xgb$watchlist,
    params = params,
    training_rounds = training_rounds,
    
    feval = custom_metric,
    maximize = F,
    
    early_stopping_rounds = early_stopping_rounds,
    print_progress = print_progress,
    print_every_n = print_every_n)
  
  t_end <- Sys.time() - t_start
  print(paste0('finished in: ', round(t_end, 3), ' seconds'))
  
  xgb_model <- (
    list(
      xgboost_model = xgboost_model,
      # test_set_results = test_set_results,
      model_matrices.xgb = model_matrices.xgb,
      xgb_data_list = xgb_data_list
    )
  )
  
  predicted <-  predict(object = xgboost_model, 
                        newdata = model_matrices.xgb$train)
  
  residuals <- xgb_data_list$train[[response_var_x]] - predicted 
  residuals <- residuals %>% round(3)
  
  xgb_model$residuals <- residuals 
  
  
  predictions <- 
    generate_predictions_table.xgboost(
      model = xgb_model$xgboost_model,
      new_data.xgb_matrix = xgb_model$model_matrices.xgb$test,
      new_data.dataframe = xgb_model$xgb_data_list$test,
      training_data.xgb_matrix = xgb_model$model_matrices.xgb$train,
      response_var = response_var_x)
  
  
  if(global_objective == "binary:logistic") {
    
    predictions <- predictions %>%
      mutate(!!actual_var := as.numeric(get(actual_var))) %>%
      mutate(!!actual_var := ifelse(get(actual_var) >= binary_threshold, 1, 0)) %>%
      mutate(!!predict_var := ifelse(get(predict_var) >= binary_threshold, 1, 0))
    
  }
  
  # ............... Back to general ====
  predictions <- predictions %>%
    group_by(!!grouping_vars_x) %>%
    add_naive_error(actual_var = actual_var) %>%
    calculate_errors(predict_var = predict_var,
                     actual_var = actual_var,
                     problem_class = problem_class,
                     binary_threshold = binary_threshold) %>%
    mutate(run = x) %>% 
    relocate(any_of(matches(features)))
  
  xgb_model[['predictions']] <- predictions
  
  
  shaps_x <- get_shap_values.xgboost(trained_xgb_model_object = xgb_model,
                                     response_var = response_var_x,
                                     id_vars = id_vars,
                                     predinteraction = F,
                                     problem_class = problem_class,
                                     quick_method = quick_shaps,
                                     quick_n_thresh = quick_n_thresh)
  shaps_x <- shaps_x %>% mutate(run = x)

  xgb_model[['shap_values']] <- shaps_x
  
  if(save_intermediates) {
    
    gc()
    readr::write_rds(x = xgb_model, file = out_file.temp)
    rm(xgb_model)
    
  } else {
    
    gc()
    xgb_model
    
  }
  
}) %>% time_it(T)
  plan(multisession, workers = 1)
  Sys.sleep(3)
  gc()
  Sys.sleep(3)
}


