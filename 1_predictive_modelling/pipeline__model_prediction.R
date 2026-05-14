# Prediction pipeline

source('functions.r')
source("machine_learning_functions.r")
library(sf)
library(usdm)


model_filter_table <- model_filter_table
in_file.coral_prediction_present <- in_file.coral_prediction_present
prediction_provinces <- prediction_provinces  
features.numeric <- features.numeric

cores.prediction <- cores.prediction
grouping_vars_x <- grouping_vars_x

predict_var <- paste0(response_var_x,'.predicted')

response_var <- response_var_x
response_var_x.short <- response_var_x.short
response_type <- response_type
id_vars <- id_vars

features.numeric <- features.numeric
features.one_hot <- features.one_hot

export_as_spatial <- export_as_spatial

rasterisation_res <- rasterisation_res
prediction_signif_figures <- prediction_signif_figures

source_crs_wkt <- source_crs_wkt


# Main ====

out_dir.preds <- paste0(out_dir,'predictions/',pred_dir_extra)

cat(crayon::cyan("for province",province_y,"\n"))

if(!dir.exists(out_dir.preds)) dir.create(out_dir.preds, recursive = T)


if(!file.exists(paste0(out_dir.preds,'model_predictions','.parquet')) ||
   overwrite.predictions == T) {
  
  plan(multisession, workers = cores.prediction)

  out_dir.temp <- paste0(out_dir,'TEMP/')
  temp_files.full <- list.files(out_dir.temp, full.names = T)
  
  # TEMP
  if(province_y == 'Atlantic Caribbean') {
    splits <- 2
  } else {
    splits <- 1
  }
  #
  
  splits <- 4
  
  
  smart.lapply(1:length(temp_files.full),
                # future.stdout = F,
                cores = cores.prediction,
                mem_check = T,
                function(x) {
                 
                 time_it({
                   print(x)
                   
                   if(file.exists(paste0(out_dir.preds,'model_predictions_',x,'.parquet')) && overwrite.intermediates == F) {
                     cat(crayon::yellow(".......... Contemporary prediction file",x,"already exists, skipping\n"))
                     return()
                     
                   }
                   
                  
                   
                   xgb_model <- read_rds(temp_files.full[x])
                   
                   features_x <- xgb_model$xgb_data_list$features
                   
                   features_x.numeric <- features_x[which(features_x %in% features.numeric)]
                   
                   features_x.one_hot <- str_extract(features_x, paste0(features.one_hot, collapse = '|')) %>%
                     unique() %>%
                     na.omit()
                   
                   features_x <- c(features_x.numeric, features_x.one_hot)

                   prediction_data_x.ready <-
                     set_up_model_data(input_data = prediction_data %>% mutate(run = x),
                                       response_var = response_var_x,
                                       response_type = response_type,
                                       features = features_x,
                                       id_vars = id_vars,
                                       features.one_hot = features_x.one_hot,
                                       replicate_unit_var = NA,
                                       train_test_split_val = 0,
                                       train_watchlist_split_val = 0)
                   
                   
                   train_features.data <- xgb_model$xgb_data_list$train[0,]
                   train_features.names <- xgb_model$xgboost_model$feature_names
                   
                   prediction_present_id_vars <-  prediction_data_x.ready[[1]] %>%
                     dplyr::select(any_of(id_vars))
                   
                   
                   prediction_data_x.ready[[1]] <- 
                     prediction_data_x.ready[[1]] %>%
                     
                     bind_rows(train_features.data) %>%
                     select(any_of(c(train_features.names, response_var_x))) %>%
                     mutate(!!response_var_x := 0)
                   
                   pred_data.xgb_matrix <- 
                     convert_data_to_xgb_matrix(xgb_data_list = prediction_data_x.ready[1],
                                                features = train_features.names,
                                                weights_var = '',
                                                response_var = response_var_x,
                                                response_type = response_type,
                                                missing = NA) %>%
                     .[[1]]
                   
                   
                   
                   model_x <- xgb_model$xgboost_model
                   data_x <- xgb_model$model_matrices.xgb$train
                   preds_x <- predict(object = model_x,
                                      data = data_x,
                                      newdata = pred_data.xgb_matrix) %>% 
                     signif(prediction_signif_figures)
                   
                   out <- tibble(!!predict_var := preds_x) %>% 
                     bind_cols(prediction_present_id_vars) %>% 
                     mutate(model_run = x)
                   
                   model_check <- out %>% 
                     select(any_of(c('model_run', grouping_vars_x) %>% na.omit())) %>% 
                     rename(run = model_run) %>%
                     distinct() %>% 
                     inner_join(model_filter_table) %>% 
                     rename(model_run = run)
                   
                   if(nrow(model_check) == 0) {
                     
                     cat(crayon::yellow("Province not included in this model. Skipping\n"))
                     return()
                     
                   }
                   
                   out <- out %>% 
                     left_join(model_check %>% dplyr::select(any_of(c('model_run', grouping_vars_x, 'selected_model') %>% na.omit())))# %>% 
                     # mutate(run = x)
                  
                    if(nrow(out) > nrow(prediction_data)) {
                      
                      cat(crayon::red("WARNING: more predictions than input data. Stopping\n"))
                      stop()
                      
                    }
                   
                   write_parquet(x = out,
                                 sink = paste0(out_dir.preds,'model_predictions_',x,'.parquet'),
                                 compression = 'zstd')
                   
                   rm(out)
                   gc()
                 })
                 
               }) %>% invisible() %>% time_it()
  
  rm(prediction_data)
  gc()
  
  preds_files <- list.files(out_dir.preds,
                            pattern = paste0("^model_predictions_[0-9]*\\.parquet$"),
                            full.names = T)
  
  
  ds <- open_dataset(out_dir.preds, format = "parquet") 
  
  ds_small <- ds %>%
    select(all_of(c(id_vars, predict_var, "selected_model")))
  
  # Mean & sd across SELECTED models (Y)
  mean_sd_y <- ds_small %>%
    filter(selected_model == "Y") %>%
    group_by(across(all_of(id_vars))) %>%
    summarise(
      !!paste0(predict_var, ".mean") := mean(!!sym(predict_var), na.rm = TRUE),
      # If your Arrow build lacks sd(), use sqrt(var()):
      !!paste0(predict_var, ".sd")   := sd(!!sym(predict_var), na.rm = TRUE),
      n_models = n()
    )
  
  # SD across ALL models
  sd_all <- ds_small %>%
    group_by(across(all_of(id_vars))) %>%
    summarise(!!paste0(predict_var, ".sd_all_models") := sd(!!sym(predict_var), na.rm = TRUE))
  
  # Join and either collect to R (small) or write out directly (big)
  out_arrow <- left_join(mean_sd_y, sd_all, by = id_vars)
  
  # Option A: write result to disk (best for memory)
  write_parquet(x = out_arrow,
                sink = paste0(out_dir.preds,"model_predictions",'.parquet'),
                compression_level = 'zstd') %>% time_it(T)
  
  
  plan(multisession, workers = 1)
  
  lapply(preds_files, file.remove)
  
  
} else {

  cat(crayon::magenta(paste0(out_dir.preds,'model_predictions','.parquet already exists, skipping.\n')))
  
}


if(export_as_spatial) {
  
  if(!file.exists(paste0(out_dir.preds,'mean_prediction','.tif')) || overwrite.exports == T) {
    
    
    predictions_summarised <- read_parquet(paste0(out_dir.preds,'model_predictions','.parquet'))
    
    predictions_summarised.spatial <- predictions_summarised %>% 
      st_as_sf(coords = c(x_var, y_var), remove = F) %>% 
      
      rename(!!paste0(response_var_x.short,'_me') := paste0(predict_var,'.mean'),
             !!paste0(response_var_x.short,'_sd') := paste0(predict_var,'.sd'),
             !!paste0(response_var_x.short,'_sdA') := paste0(predict_var,'.sd_all_models'))
    
    
    st_crs(predictions_summarised.spatial) <- source_crs_wkt
    
    st_write(predictions_summarised.spatial,
             paste0(out_dir.preds,'predictions','.shp'),
             append = F)
    
    
    
    # Raseterisation
    template <- rast(vect(predictions_summarised.spatial), res = rasterisation_res)
    
    preds.rast <- terra::rasterize(predictions_summarised.spatial, 
                                   template, 
                                   field = paste0(response_var_x.short,'_me'),
                                   fun='mean',
                                   cover = F)
    
    writeRaster(preds.rast,
                filename =  paste0(out_dir.preds,'mean_prediction','.tif'),
                overwrite=T)
    
    
    preds.rast <- terra::rasterize(predictions_summarised.spatial, 
                                   template, 
                                   field = paste0(response_var_x.short,'_sd'),
                                   fun='mean',
                                   cover = F)
    
    writeRaster(preds.rast,
                filename = paste0(out_dir.preds,'sd_prediction','.tif'),
                overwrite =T )
    
    
    
    preds.rast <- terra::rasterize(predictions_summarised.spatial, 
                                   template, 
                                   field = paste0(response_var_x.short,'_sdA'),
                                   fun='mean',
                                   cover = F)
    
    writeRaster(preds.rast,
                filename = paste0(out_dir.preds,'sd_prediction_all_models','.tif'),
                overwrite = T)
    
    
    rm(predictions_summarised.spatial)
    rm(predictions.mean)
    rm(predictions.sd)
    rm(preds.rast)
    gc()
    
  } else {
    
    cat(crayon::magenta(paste0(out_dir.preds,'model_predictions spatial exports already exist. skipping.\n')))
    
  }
  
  
}
