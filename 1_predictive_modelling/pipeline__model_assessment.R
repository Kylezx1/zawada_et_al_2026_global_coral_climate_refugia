# Model assessment pipeline

source('functions.r')
source("machine_learning_functions.r")
library(sf)
library(usdm)
library(arrow)

out_dir <- out_dir

actual_var <- paste0(response_var_x,'.actual')
actual_var.short <- paste0(response_var_x.short,'.act')

predict_var <- paste0(response_var_x,'.predicted')
predict_var.short <- paste0(response_var_x.short,'.pred')

source_crs_wkt <- source_crs_wkt
grouping_vars_x <- grouping_vars_x

id_vars <- id_vars
x_var <- x_var
y_var <- y_var

error_ratio_threshold <- error_ratio_threshold
predicted_r_squared_threshold <- predicted_r_squared_threshold


# Main ====
cat(crayon::green("Running model assessment pipeline\n"))

if(save_intermediates) {
  cat(crayon::cyan(".....Combining model outputs\n"))
  out_dir.temp <- paste0(out_dir,'TEMP/')
  temp_files.full <- list.files(out_dir.temp, full.names = T)
  
  xgb_models <- lapply(1:length(temp_files.full), function(x) {
    print(x)
    read_rds(temp_files.full[x])
  })
  
}


# ..... Model performance ====
cat(crayon::cyan(".....Generating model assessment summaries\n"))

out_dir.fitted <- paste0(out_dir,'fitted_values/')

out_name.predictions <- paste0(out_dir.fitted,'model_predictions_raw.csv')
out_name.performance <- paste0(out_dir,'model_performance_summary.csv')
out_name.model_performance_summary <- paste0(out_dir.fitted,'model_predictions_summarised_long.csv')

out_name.model_quality_summary <- paste0(out_dir,'model_quality_summary.csv')

out_name.model_performance_metrics <- paste0(out_dir,'model_performance_metrics.csv')
out_name.model_performance_metrics.all <- paste0(out_dir,'model_performance_metrics_all_models.csv')

out_name.predictions_summary <- paste0(out_dir.fitted,'model_predictions_summarised_wide.shp')

out_name.shaps_long <- paste0(out_dir,'shap_values_long.parquet')



if(!dir.exists(out_dir.fitted)) dir.create(out_dir.fitted)

cat(crayon::cyan("..........Getting raw test set values\n"))

xgb_models.predictions <- lapply(1:length(xgb_models), function(x) {
  xgb_models[[x]]$predictions %>% 
    mutate(model_absolute_error_uplift = 1 - (error.model.abs/error.naive_prediction.abs))
  
}) %>% bind_rows() 

if(!file.exists(out_name.predictions) || overwrite.exports == T) {

  write_csv(x = xgb_models.predictions,
            file = paste0(out_dir.fitted,'model_predictions_raw.csv'))
  
}


cat(crayon::cyan("..........Assessing model performance\n"))
if(!file.exists(out_name.performance) || overwrite.exports == T) {
  
  model_runs.performance.summary <- xgb_models.predictions %>%
    group_by(across(any_of(c(grouping_vars_x, 'run') %>% na.omit()))) %>%
    summarise_errors()
  
  write_csv(x = model_runs.performance.summary,
            file = out_name.performance)
    
}


# ......... Test set assessment ====
cat(crayon::cyan("..........Assessing test value ensemble predictions\n"))
if(!file.exists(out_name.model_performance_summary) || overwrite.exports == T) {
  
  prediction_summary <- xgb_models.predictions %>%
    group_by(across(any_of(id_vars))) %>%
    summarise(mean_prediction = mean(get(predict_var)),
              sd_prediction = sd(get(predict_var)),
              mean_abs_error = mean(abs(get(predict_var) - get(actual_var))),
              sd_abs_error = sd(abs(get(predict_var) - get(actual_var)))) %>%
    ungroup() %>%
    mutate(mean_prediction_0_to_1 = rescale_0_to_1(mean_prediction),
           sd_prediction_0_to_1 = rescale_0_to_1(sd_prediction),
           mean_abs_error_0_to_1 = rescale_0_to_1(mean_abs_error),
           sd_abs_error_0_to_1 = rescale_0_to_1(sd_abs_error)) 
  
  prediction_summary.long.unscaled <- prediction_summary %>%
    
    pivot_longer(cols = c('mean_prediction',
                          'sd_prediction',
                          'mean_abs_error',
                          'sd_abs_error'
    ),
    names_to = 'variable',
    values_to = 'value')
  
  prediction_summary.long.scaled <- prediction_summary %>%
    
    mutate(mean_prediction = rescale_0_to_1(mean_prediction),
           sd_prediction = rescale_0_to_1(sd_prediction),
           mean_abs_error = rescale_0_to_1(mean_abs_error),
           sd_abs_error = rescale_0_to_1(sd_abs_error))%>%
    
    pivot_longer(cols = c('mean_prediction',
                          'sd_prediction',
                          'mean_abs_error',
                          'sd_abs_error'
    ),
    names_to = 'variable',
    values_to = 'value_rescaled')
  
  prediction_summary.long <- full_join(prediction_summary.long.unscaled, 
                                       prediction_summary.long.scaled) %>%
    select(-any_of(c('mean_prediction',
                     'sd_prediction',
                     'mean_abs_error',
                     'sd_abs_error',
                     'mean_prediction_0_to_1',
                     'sd_prediction_0_to_1',
                     'mean_abs_error_0_to_1',
                     'sd_abs_error_0_to_1')))
  
  write_csv(x = prediction_summary.long,
            file = paste0(out_dir.fitted,'model_predictions_summarised_long.csv'))
  
  if(export_as_spatial) {
    
    prediction_summary.spatial <- prediction_summary.long %>%
      st_as_sf(coords = c(x_var, y_var)) %>%
      mutate(!!x_var := prediction_summary.long[[x_var]],
             !!y_var := prediction_summary.long[[y_var]])
    
    
    st_crs(prediction_summary.spatial) <- source_crs_wkt
    
    st_write(prediction_summary.spatial,
             paste0(out_dir.fitted,'model_predictions_summarised_long.shp'),
             append = F)
    
  }
  
  write_csv(x = prediction_summary,
            file = paste0(out_dir.fitted,'model_predictions_summarised_wide.csv'))
  
  if(export_as_spatial) {
    
    prediction_summary.spatial <- prediction_summary %>%
      st_as_sf(coords = c(x_var, y_var)) %>%
      mutate(!!x_var := prediction_summary[[x_var]],
             !!y_var := prediction_summary[[y_var]])
    
    
    st_crs(prediction_summary.spatial) <- source_crs_wkt
    
    st_write(prediction_summary.spatial,
             paste0(out_dir.fitted,'model_predictions_summarised_wide.shp'),
             append = F)
    
  }
  
}

# .......... Per model perfomance metrics ====
cat(crayon::cyan("..........Calculating per-model per-group model performance metrics\n"))
if(!file.exists(out_name.model_performance_metrics) || overwrite.exports == T) {
  
  model_performance_metrics.fitted <- lapply(1:length(xgb_models), function(x) {
    
    print(x)
    
    if(is.na(grouping_vars_x)) {
      model_grouping_var_x <- NA
    } else {
      model_grouping_var_x <-  xgb_models[[x]]$xgb_data_list$train[[grouping_vars_x]]
    } 
    
    observations_x <- xgb_models[[x]]$xgb_data_list$train[[response_var_x]]
    # model_grouping_var_x <-  xgb_models[[x]]$xgb_data_list$train[[grouping_vars_x]]
    residuals_x <- xgb_models[[x]]$residuals
    
    
    data <- data.frame(
      observed = observations_x,
      residuals = residuals_x,
      group = model_grouping_var_x
    ) %>% 
      mutate(predicted = observed + residuals)
    
    r2_by_group <- data %>%
      # filter(group == 'Persian Gulf') %>%
      group_by(group) %>%
      mutate(observed_mean = mean(observed)) %>% 
      mutate(res_sq = residuals^2, obs_sq = (observed - observed_mean)^2) %>% 
      summarise(
        ss_r = sum(res_sq),
        ss_t = sum(obs_sq),
        r_squared = 1 - (ss_r / ss_t)
      )
    
    
    get_sig_stars <- function(p_value) {
      
      if(is.na(p_value) | is.nan(p_value)) {
        return(NA)
      } 
      
      if (p_value < 0.001) {
        return("***")
      } else if (p_value < 0.01) {
        return("**")
      } else if (p_value < 0.05) {
        return("*")
      } else {
        return("-")
      }
    }
    
    # Run linear regression of y ~ predicted by group and extract coefficients
    model_results <- data %>%
      group_by(group) %>%
      do(broom::tidy(lm(observed ~ predicted, data = .))) %>%
      ungroup() %>%
      mutate(sig = sapply(p.value, get_sig_stars)) %>%
      select(group, term, estimate, p.value, sig) %>% 
      mutate(term = ifelse(term == '(Intercept)', 'intercept', 'slope')) %>% 
      pivot_wider(id_cols = 'group',
                  names_from = 'term',
                  values_from = c('estimate', 'p.value', 'sig'))
    
    out <- r2_by_group %>% 
      left_join(model_results) %>% 
      mutate(model_run = x) %>% 
      mutate(across(where(is.numeric), ~ {
        x <- .
        x[!is.finite(x)] <- NA
        x
      }))
    
    
  }) %>% bind_rows()
  
  
  model_performance_metrics.fitted.summary <- model_performance_metrics.fitted %>% 
    group_by(group) %>% 
    summarise(r_squared.fitted.mean = mean(r_squared, na.rm = T),
              r_squared.fitted.sd = sd(r_squared, na.rm = T))
  
  
  model_performance_metrics.predictions <- lapply(1:length(xgb_models), function(x) {
    
    print(x)
    
    if(is.na(grouping_vars_x)) {
      model_grouping_var_x <- NA
    } else {
      model_grouping_var_x <-  xgb_models[[x]]$xgb_data_list$test[[grouping_vars_x]]
    } 
    
    observations_x <- xgb_models[[x]]$predictions[[paste0(response_var_x,'.actual')]]
    # model_grouping_var_x <-  xgb_models[[x]]$predictions[[grouping_vars_x]]
    predictions_x <- xgb_models[[x]]$predictions[[paste0(response_var_x,'.predicted')]]
    errors_x <- xgb_models[[x]]$predictions[['error.model']]
    
    
    data <- data.frame(
      observed = observations_x,
      predicted = predictions_x,
      errors = errors_x,
      group = model_grouping_var_x
    )
    
    r2_by_group <- data %>%
      # filter(group == 'Andaman-Nicobar Islands') %>%
      group_by(group) %>%
      mutate(observed_mean = mean(observed)) %>% 
      mutate(res_sq = errors^2, obs_sq = (observed - observed_mean)^2) %>% 
      summarise(
        ss_r = sum(res_sq),
        ss_t = sum(obs_sq),
        r_squared.predictions = 1 - (ss_r / ss_t)
      )
    
    get_sig_stars <- function(p_value) {
      
      if(is.na(p_value) || is.nan(p_value)) {
        return(NA)
      } 
      
      if (p_value < 0.001) {
        return("***")
      } else if (p_value < 0.01) {
        return("**")
      } else if (p_value < 0.05) {
        return("*")
      } else {
        return("-")
      }
    }
    
    # Run linear regression of y ~ predicted by group and extract coefficients
    model_results <- data %>%
      group_by(group) %>%
      # do(broom::tidy(lm(observed ~ predicted, data = .))) %>%
      
      do({
        # Fit the model for the group
        model <- lm(observed ~ predicted, data = .)
        # Get the residual degrees of freedom for the model
        df_res <- model$df.residual
        # Tidy the model coefficients
        tidy_model <- broom::tidy(model)
        # Adjust the test for the slope: H0: beta_slope == 1
        tidy_model <- tidy_model %>%
          mutate(
            # For the slope, compute a new t-statistic comparing the estimate to 1.
            t_adj = if_else(term == "predicted", (estimate - 1) / std.error, NA_real_),
            # For the slope, compute a new two-sided p-value using the adjusted t-statistic.
            p_value = if_else(term == "predicted",
                              2 * pt(-abs(t_adj), df = df_res),
                              p.value)
          )
        tidy_model
      }) %>% 
      
      ungroup() %>%
      mutate(sig = sapply(p.value, get_sig_stars)) %>%
      select(group, term, estimate, p.value, sig) %>% 
      mutate(term = ifelse(term == '(Intercept)', 'intercept.calibration', 'slope.calibration')) %>% 
      pivot_wider(id_cols = 'group',
                  names_from = 'term',
                  values_from = c('estimate', 'p.value', 'sig'))
    
    
    out <- r2_by_group %>% 
      left_join(model_results) %>% 
      mutate(model_run = x) %>% 
      mutate(across(where(is.numeric), ~ {
        x <- .
        x[!is.finite(x)] <- NA
        x
      }))
    
    
  }) %>% bind_rows()
  
  model_performance_metrics.predictions.summary <- model_performance_metrics.predictions %>% 
    group_by(group) %>% 
    summarise(r_squared.predictions.mean = mean(r_squared.predictions, na.rm=T),
              r_squared.predictions.sd = sd(r_squared.predictions, na.rm=T),
              
              calibration_intercept.estimate.mean = mean(estimate_intercept.calibration, na.rm=T),
              calibration_intercept.p_value.mean = mean(p.value_intercept.calibration, na.rm=T),
              
              calibration_slope.estimate.mean = mean(estimate_slope.calibration, na.rm=T),
              calibration_slope.p_value.mean = mean(p.value_slope.calibration, na.rm=T)
    )
  
  
  model_runs.performance.summary <- xgb_models.predictions %>%
    group_by(across(any_of(c(grouping_vars_x, 'run') %>% na.omit()))) %>%
    summarise_errors()
  
  
  if(!is.na(grouping_vars_x[1])) {
    
    model_runs.performance.summary.overall <- xgb_models.predictions %>%
      group_by(across(any_of(c(grouping_vars_x) %>% na.omit()))) %>% 
      summarise_errors()
    
    model_runs.performance.summary.naive_models <- model_runs.performance.summary %>%
      group_by(across(any_of(c(grouping_vars_x)))) %>%
      summarise(error.naive.mean = mean(error.naive.mean),
                error.naive.abs.mean = mean(error.naive.abs.mean),
                error.naive.median = median(error.naive.mean),
                error.naive.median.abs = median(error.naive.abs.mean))
    
  } else {
    
    model_runs.performance.summary.overall <- xgb_models.predictions %>%
      summarise_errors()
    
    model_runs.performance.summary.naive_models <- model_runs.performance.summary %>%
      summarise(error.naive.mean = mean(error.naive.mean),
                error.naive.abs.mean = mean(error.naive.abs.mean),
                error.naive.median = median(error.naive.mean),
                error.naive.median.abs = median(error.naive.abs.mean))
    
  }
  
  
  
  
  if(is.na(grouping_vars_x)) {
    
    model_summary <- model_runs.performance.summary.overall %>% 
      mutate(group = NA)
    
    model_summary_all_models <- model_runs.performance.summary %>% 
      mutate(group = NA) 
    
  } else {
    
    model_summary <- model_runs.performance.summary.overall %>% 
      rename(group = grouping_vars_x)
    
    model_summary_all_models <- model_runs.performance.summary %>% 
      rename(group = grouping_vars_x)
    
  } 
  
  
  model_summary_all_models <- model_summary_all_models %>% 
    rename(model_run = run) %>% 
    left_join(model_performance_metrics.fitted) %>% 
    left_join(model_performance_metrics.predictions %>% select(-ss_t, -ss_r)) %>% 
    mutate(model_absolute_error_ratio = 1-(error.model_abs.mean / error.naive.abs.mean)) %>% 
    mutate(across(is.numeric, ~round(.x,3)))
  
  
  model_summary <- model_summary %>% 
    # rename(group = grouping_vars_x) %>% 
    left_join(model_performance_metrics.fitted.summary) %>% 
    left_join(model_performance_metrics.predictions.summary) %>% 
    mutate(across(is.numeric, ~round(.x,3)))
  
  
  if(is.na(grouping_vars_x)) {
    
    model_summary <- model_summary %>% 
      mutate(group = "All")
    
    model_summary_all_models <- model_summary_all_models %>% 
      mutate(group = "All") 
    
  }
  
  write_csv(x = model_summary,
            file = out_name.model_performance_metrics)
  
  
  write_csv(x = model_summary_all_models,
            file = out_name.model_performance_metrics.all)
  
}



if(!is.na(accuracy_filter)) {
  
  xgb_models_2 <- list()
  
  lapply(1:length(xgb_models), function(x) {
    
    model_accuracy_x <- model_runs.performance.summary$error.model_abs.mean[x]
    
    if(model_accuracy_x <= accuracy_filter) {
      
      xgb_models_2 <<- c(xgb_models_2, xgb_models[x])
      
    }
    
  })
  
  xgb_models <- xgb_models_2
  
}


if(predictive_model_selection == T) {
  
  model_summary_all_models <- read_csv(out_name.model_performance_metrics.all)
  
  if(!file.exists(out_name.model_quality_summary) || overwrite.exports == T) {
    
    grouping_vars_x2 <- ifelse(is.na(grouping_vars_x), 'All', grouping_vars_x)
    
    model_filter_table <- model_summary_all_models %>% 
      # filter(model_absolute_error_ratio >0+error_ratio_threshold) %>% 
      # filter(r_squared.predictions > 0+predicted_r_squared_threshold) %>% 
      mutate(model_quality = case_when(
        model_absolute_error_ratio >= error_ratio_threshold*3 & r_squared.predictions >= predicted_r_squared_threshold*3 ~ "great",
        model_absolute_error_ratio >= error_ratio_threshold & r_squared.predictions >= predicted_r_squared_threshold ~ "good",
        model_absolute_error_ratio >= error_ratio_threshold & r_squared.predictions >= -predicted_r_squared_threshold ~ "low",
        model_absolute_error_ratio >= -error_ratio_threshold & r_squared.predictions >= predicted_r_squared_threshold ~ "low",
        model_absolute_error_ratio < -error_ratio_threshold | r_squared.predictions < -predicted_r_squared_threshold ~ "negative",
        model_absolute_error_ratio >= -error_ratio_threshold & model_absolute_error_ratio <= error_ratio_threshold &
          r_squared.predictions >= -predicted_r_squared_threshold & r_squared.predictions <= predicted_r_squared_threshold ~ "none",
        is.na(model_absolute_error_ratio) | is.na(r_squared.predictions) ~ 'unknowable',
        TRUE ~ NA_character_))  %>% 
      select(!!grouping_vars_x2 := group,
             run = model_run,
             model_quality,
             model_absolute_error_ratio,
             r_squared.predictions) %>% 
      distinct() %>% 
      filter(model_quality != 'unknowable') %>% 
      filter(!is.na(model_quality)) %>% 
      
      mutate(selected_model = if_else(model_quality %in% c('great', 'good'), 'Y', 'N'))
    
    model_quality_table <- model_filter_table %>% 
      group_by(across(grouping_vars_x2)) %>% 
      summarise(n_models = n(),
                all_models_absolute_error_ratio.mean = mean(model_absolute_error_ratio, na.rm = T),
                all_models_r_squared.predictions.mean = mean(r_squared.predictions, na.rm = T)) %>% 
      right_join(model_filter_table) %>% 
      group_by(across(c(grouping_vars_x2,model_quality))) %>% 
      summarise(quality_count = n(),
                n_models = first(n_models),
                all_model_absolute_error_ratio.mean = first(all_models_absolute_error_ratio.mean),
                all_models_r_squared.predictions.mean = first(all_models_r_squared.predictions.mean)) %>% 
      mutate(quality_pc = ((quality_count/n_models)*100) %>% round())
    
    
    selected_model_r_sq_and_uplift <- model_filter_table %>% 
      filter(model_quality %in% c('great', 'good')) %>% 
      group_by(across(grouping_vars_x2)) %>% 
      summarise(n_good_models = n(),
                good_models_absolute_error_ratio.mean = mean(model_absolute_error_ratio, na.rm = T),
                good_models_r_squared.predictions.mean = mean(r_squared.predictions, na.rm = T))
    
    
    model_quality_table <- model_quality_table %>% 
      left_join(selected_model_r_sq_and_uplift)
    
    model_quality_pc_columns <- model_quality_table %>% 
      pivot_wider(id_cols = grouping_vars_x2,
                  names_from = model_quality,
                  values_from = quality_pc,
                  values_fill = 0) %>% 
      relocate(any_of(c('great', 'good', 'low', 'none', 'negative')), .after = grouping_vars_x2) %>% 
      left_join(model_quality_table %>% 
                  select(-model_quality, -quality_pc, -quality_count)) %>% 
      distinct() %>% 
      arrange(-n_good_models)
    
    write_csv(x = model_quality_pc_columns,
              file = out_name.model_quality_summary)
    
  } else {
    
    model_filter_table <- model_summary_all_models %>% 
      select(!!grouping_vars_x := group,
             model_run) %>% 
      distinct() %>% 
      mutate(selected_model = 'Y')
    
  }
 
  write_csv(model_filter_table, paste0(out_dir,'model_filter_table.csv'))
   
}


# .......... SHAP values ====

cat(crayon::cyan("..........Getting SHAP Values\n"))

if(!file.exists(out_name.shaps_long) || overwrite.exports == T) {
  
  shap_values <- lapply(1:length(xgb_models), function(x) {
    
    out <- xgb_models[[x]]$shap_values
    
    out
    
  }) %>% bind_rows() 
  
  
  features_wide <- shap_values %>%
    select(-any_of(c('BIAS', 'observation_id', 'data_type', 'run', 'model_obs_id', id_vars))) %>%
    names()
  
  shap_values.long <- shap_values %>%
    pivot_longer(cols = c(features_wide),
                 names_to = 'feature',
                 values_to = 'shap') %>%
    mutate(shap_abs = abs(shap)) %>%
    arrange(-shap_abs) %>%
    mutate(shap_abs = round(shap_abs, 5))
  
  
  write_parquet(x = shap_values.long, 
                sink = out_name.shaps_long,
                compression = 'ZSTD')
    
}
