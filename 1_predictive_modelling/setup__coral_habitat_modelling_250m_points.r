# Coral habitat modelling Bloomberg Life Histories

# Data preparation

# Set up ====
rm(list=ls())


source('habitat_modelling_pipeline_defaults.r')

# ..... Libraries ====
library(tidyverse)
library(ggbeeswarm)
library(snakecase)
library(arrow)

library(future.apply)
options(future.globals.maxSize = 1024*10*1024^2)

source('functions.r')


# ..... Directories and files ====
in_dir.data <- "Z:/Dropbox/Lenfest_kenya/data/PROJECT_SPECIFIC/bloomberg/modelling_data/20250906/"

in_file.coral <- 'bloomberg_life_histories_20250906_model_training.parquet'

in_file.coral_prediction_present <- 'bloomberg_life_histories_20250331_testing_contemporary.csv'
in_file.coral_prediction_present <- 'Z:/Dropbox/Work/projects/mq_wcs/wcs_kenya/analysis/r_spatial_tools/output/combine_pu_matched_datasets/global_coral_datasets_pre_webinar/central_indian_ocean_islands/bloomberg_life_histories_pre_webinar_predictions_contemporary.csv'

in_file.coral_prediction_present <- c('Z:/Dropbox/Lenfest_kenya/data/PROJECT_SPECIFIC/bloomberg/modelling_data/20250820/bloomberg_life_histories_20250820_model_testing_contemporary_2020.parquet')





in_file.coral_prediction_future <- c('coral_habitat_prediction_data_wio_250m_points_lits_time_replicates_testing_future_ssp370.csv',
                                     'coral_habitat_prediction_data_wio_250m_points_lits_time_replicates_testing_future_ssp585.csv')


out_dir.start <- "output/coral/bloomberg_20250915/"


future_scenario_names <- c('ssp370',
                           'ssp585')

pred_dir_extra <- ''


# ..... Flags ====

single_model_test_run = F

save_output_data = T
export_as_spatial = F

p.exploratory = F

p.scatter = T
p.boxplot = T
p.shaps = T
p.shap_partials = F
p.map = T

p.prediction_present = F
p.prediction_future = F

p.model_performance = T

map_crop = T

residual_analyses = F
spatial_autocorrelation_vars = T
temporal_autocorrelation_vars = T



model_data_function <- function(data) {
  
  data %>% 
    mutate(coral_cover_pc = round(as.numeric(coral_cover_pc))) %>% 
    mutate(rep_check = paste0(round(as.numeric(pu_x),5),'_',round(as.numeric(pu_y,5)),'_',time,'_',floor(coral_cover_pc))) %>% 
    group_by(rep_check) %>%
    dplyr::slice(1) %>%
    ungroup() %>% 
    select(-rep_check) %>% 
    mutate(decade =  floor(as.numeric(time) / 10) * 10) %>% 
    mutate(demidecade =  floor(as.numeric(time) / 5) * 5) %>% 
    mutate(rep_unit_var = paste0(round(as.numeric(pu_y),1),'_',round(as.numeric(pu_x),1),'_',demidecade)) %>% 
    mutate(ecoregion = to_snake_case(ecoregion)) %>% 
    return()
  
  
}


modal_data_transform_function <- function(data) {
  
  data %>% 
    mutate(andrello_sediments = as.numeric(andrello_sediments) %>% sqrt()) %>% 
    mutate(human_gravity_total = as.numeric(human_gravity_total) %>% sqrt()) %>% 
    mutate(min_distance_from_land_gebco = as.numeric(min_distance_from_land_gebco) %>% sqrt())
  
}


prediction_data_filter <- function(data, prediction_provinces) {
  
  data %>% 
    filter(coral_province %in% prediction_provinces)
  
}



# ..... Variants table ====
coral_provinces <- c("Africa-India",
                     "Andaman-Nicobar Islands",
                     "Atlantic Caribbean",
                     "Australian",
                     "Fiji-Caroline Islands",
                     "Hawaii-Line Islands",
                     "Indonesian" ,
                     "Japan-Vietnam",
                     "Pacific Caribbean",
                     "Persian Gulf",
                     "Polynesia",
                     "Red Sea",
                     "Tonga-Samoa",
                     "West Africa",
                     "Brazil"
)



set_ups <- list(
  
  
  x1 = list(response_var = 'coral_cover_pc',
            response_var.short = 'crl_pc',
            filter_function = list(function(data) { data %>% 
                mutate(coral_cover_pc = as.numeric(sqrt(coral_cover_pc))) %>% 
                # filter(file_source_desc %in% c("nee_lits_back_corrected_life_histories",
                #                                "mermaid_cover",
                #                                "wcs_wio_lits",
                #                                "guest_et_al_cover")) %>%
                # filter(total_coral_vs_lh_total_delta <= 7) %>% 
                # filter(ecoregion == 'hawaii') %>% 
                return() }),
            out_dir_extra = 'total_coral_cover.all_data',
            out_name_extra = 'total_coral_cover.all_data',
            grouping_vars = 'coral_province'),
  

  x4 = list(response_var = 'stress_tolerant_cover_pc',
            response_var.short = 'sts_pc',
            filter_function = list(function(data) { data %>%
                mutate(stress_tolerant_cover_pc = as.numeric(stress_tolerant_cover_pc)/100) %>%
                filter(file_source_desc %in% c("nee_lits_back_corrected_life_histories",
                                               "mermaid_cover",
                                               "wcs_wio_lits",
                                               "guest_et_al_cover")) %>%
                filter(total_coral_vs_lh_total_delta <= 7) %>%
                return() }),
            out_dir_extra = 'stress_tolerant',
            out_name_extra = 'stress_tolerant',
            grouping_vars = 'coral_province'),
  
  
  x5 = list(response_var = 'competitive_cover_pc',
            response_var.short = 'com_pc',
            filter_function = list(function(data) { data %>%
                mutate(competitive_cover_pc = as.numeric(competitive_cover_pc)/100) %>%
                filter(file_source_desc %in% c("nee_lits_back_corrected_life_histories",
                                               "mermaid_cover",
                                               "wcs_wio_lits",
                                               "guest_et_al_cover")) %>%
                filter(total_coral_vs_lh_total_delta <= 7) %>%
                return() }),
            out_dir_extra = 'competitive',
            out_name_extra = 'competitive',
            grouping_vars = 'coral_province'),
  
  
  x6 = list(response_var = 'weedy_cover_pc',
            response_var.short = 'wee_pc',
            filter_function = list(function(data) { data %>%
                mutate(weedy_cover_pc = as.numeric(weedy_cover_pc)/100) %>%
                filter(file_source_desc %in% c("nee_lits_back_corrected_life_histories",
                                               "mermaid_cover",
                                               "wcs_wio_lits",
                                               "guest_et_al_cover")) %>%
                filter(total_coral_vs_lh_total_delta <= 7) %>%
                return() }),
            out_dir_extra = 'weedy',
            out_name_extra = 'weedy',
            grouping_vars = 'coral_province')
  
)



variant_table <- bind_rows(set_ups)


# ..... Constants ====

source_crs_wkt <- sf::st_crs(4326)$wkt

id_vars.character <- c("pu_id")
id_vars.character <- c('ecoregion', 'coral_province')

id_vars.numeric <- c('pu_lon', 
                     'pu_lat',
                     'time')

x_var <- "pu_lon"
y_var <- "pu_lat"
time_var <- 'time'


id_vars <- c(id_vars.character, id_vars.numeric) %>% na.omit()


replicate_unit_var <- 'rep_unit_var'

features.numeric <- c(

  
  # ..... Thermal stress ====
  
  "dhw_max_6_year_max",
  "dhw_max_6_year_mean",
  
  "sst_kurtosis_6_year_mean",
  "sst_kurtosis_34_year_mean",

  "sst_q90_6_year_mean",
  "sst_q90_34_year_mean",

  "sst_skewness_6_year_mean",
  "sst_skewness_34_year_mean",
  
  'sst_climatology_noaa',

  
  # ..... Environmental suitability ====
  
  "delta_sst_mean_6_year_mean",
  "delta_sst_median_6_year_mean",
  "delta_sst_q90_6_year_mean",
  "delta_sst_sd_6_year_mean",
  "delta_sst_skewness_6_year_mean",
  "delta_sst_kurtosis_6_year_mean",
  
  
  "salinity_mean_max_depth_2020_bio_oracle_3",
  
  # ..... Nutrients and sedimentation ====
  
  'andrello_sediments',

  # .... Trace elements ====
  
  'calcite_mean_surface_bio_oracle_2.2',
  
  "iron_mean_max_depth_2020_bio_oracle_3",
  
  "nitrate_mean_max_depth_2020_bio_oracle_3",
  "oxygen_mean_max_depth_2020_bio_oracle_3",
  
  "ph_mean_max_depth_2020_bio_oracle_3",
  "phosphate_mean_max_depth_2020_bio_oracle_3",
  "p04_to_n03_mean_max_depth_2020_bio_oracle_3",
  
  
  # ..... Physical variables ====

  "depth_m.observed",
  
  "slope_mean_2020_bio_oracle_3",
  
  "min_distance_from_land_gebco",
  "min_distance_from_500m_depth_gebco",
  
  "terrain_ruggedness_index_2020_bio_oracle_3",
  "topographic_position_index_2020_bio_oracle_3",
  "current_velocity_mean_max_depth_2020_bio_oracle_3",

  
  # ..... Human pressure ====
  
  "human_gravity_total",
  
  # ..... Cyclones ====
  
  # "cyclone_count_34_year_sum",
  "cyclone_count_6_year_sum",
  # "cyclone_intense_count_34_year_sum",
  # "cyclone_intense_count_6_year_sum",
  # "cyclone_max_annual_wind_34_year_max",
  "cyclone_max_annual_wind_34_year_mean",
  "cyclone_max_annual_wind_6_year_max",
  # "cyclone_max_annual_wind_6_year_mean",
  # "cyclone_sever_count_34_year_sum",
  "cyclone_sever_count_6_year_sum",
  
  
  # 'rain_long_term_mean_merra_2',
  'diffuse_attenuation_mean_bio_oracle_2',
  'par_mean_bio_oracle_2',
  'light_at_bottom_range_bio_oracle_2',
  'light_at_bottom_mean_bio_oracle_2',
  
  # ..... Connectivty ====
  
  "connectivity.centrality.idw",
  "connectivity.indegree.idw",
  "connectivity.inflow.idw",
  "connectivity.local_retention.idw",
  "connectivity.outdegree.idw",
  "connectivity.outflow.idw",
  "connectivity.self_recruitment.idw"
  
  
)

features.numeric.hardcoded <- NA

features.one_hot <- c( 'coral_province')

features <- c(features.numeric, features.one_hot) %>% na.omit()


collinearity_filter <- T
cutoff <- 7
cutoff_type <- 'vif'

features.forced <- c('')

# ..... XGBoost settings ====

# ..... Required ====
n_runs <- 100




problem_class <- 'regression'
response_type <- 'regression'
global_objective <- 'reg:squaredlogerror'
global_eval_metric <- c('rmsle')


quantile_alpha <- NULL

tweedie_variance_power <- NULL

custom_metric <- NULL

train_test_split_val <- 0.2
train_watchlist_split_val <- 0.25


# .......... Changed defaults ====

quick_shaps <- T
quick_n_thresh <- 1000

# ..... Multicore settings ====
cores <- 2
cores.prediction <- 1

# ..... Intermediate exports ====
save_intermediates = T

overwrite.intermediates = F
overwrite.exports = F
overwrite.plots = F
overwrite.plots.explanatory = F
overwrite.predictions = F

accuracy_filter <- NA

variable_naming_table <- NA

predictive_model_selection <- T

prediction_provinces <-  c("Africa-India",
                           "Andaman-Nicobar Islands",
                           "Atlantic Caribbean",
                           "Australian",
                           "Fiji-Caroline Islands",
                           "Hawaii-Line Islands",
                           "Indonesian",
                           "Japan-Vietnam",
                           "Pacific Caribbean",
                           "Persian Gulf",
                           "Polynesia",
                           "Red Sea",
                           "Tonga-Samoa")

prediction_years <- c("2020", 
                      'ssp370', 
                      'ssp585')

prediction_depths <- c(10)

prediction_signif_figures <- 4
rasterisation_res <- 0.003


error_ratio_threshold <- 0.1
predicted_r_squared_threshold <- 0.1

maximum_single_file_rows <- 1000000


if(!dir.exists(out_dir.start)) { dir.create(out_dir.start, recursive = T) }

# Create a copy of this model set up in the output folder for later retrieval
file.copy("setups/habitat_modelling_pipeline/coral_bloomberg/coral_habitat_modelling_250m_points_raw_lits_mean_cover_wio.r",
          paste0(out_dir.start,'model_setup.r'),
          overwrite = T)

x = 1
# Main ==== 


# Variant loop

lapply(1:nrow(variant_table), function(x) {
  
  # ..... Get variant values ====
  
  out_dir_extra_x <- variant_table$out_dir_extra[[x]]
  out_name_extra_x <- variant_table$out_name_extra[x]
  
  filter_function_x <- variant_table$filter_function[[x]]
  
  response_var_x <- variant_table$response_var[[x]]
  response_var_x.short <- variant_table$response_var.short[[x]]
  
  grouping_vars_x <- variant_table$grouping_vars[x]
  
  # ..... Create directories
  out_dir <- paste0(out_dir.start,response_var_x,'/',out_dir_extra_x,'/')
  
  
  if(!dir.exists(out_dir)) { dir.create(out_dir, recursive = T) }
  
  # ..... Dataset loading ====
  
  model_data <- read_parquet(paste0(in_dir.data,in_file.coral), as_data_frame = T)
  
  model_data <- model_data %>%
    
    model_data_function() %>%
    
    modal_data_transform_function() %>% 
    
    filter_function_x() %>%
    
    dplyr::rename(pu_lon = pu_x,  pu_lat = pu_y) %>%

    return()
  
  na_count <- sapply(model_data, function(x) sum(is.na(x)))
  
  na_count_df <- data.frame(variable = names(model_data), na_count = na_count) %>%
    mutate(na_prop = (na_count/nrow(model_data))*100)


  # ..... Run models ====
  cat(crayon::magenta("Running models:",out_dir,"\n"))
  source('pipelines/habitat_modelling_pipeline.r', local = T)
  
  plan(multisession, workers = 1)
  
  beepr::beep()
  
  
})



# ..... Model assessment ====  
lapply(1:nrow(variant_table), function(x) {
  
  # ..... Get variant values ====
  
  out_dir_extra_x <- variant_table$out_dir_extra[[x]]
  out_name_extra_x <- variant_table$out_name_extra[x]
  
  filter_function_x <- variant_table$filter_function[[x]]
  
  response_var_x <- variant_table$response_var[[x]]
  response_var_x.short <- variant_table$response_var.short[[x]]
  
  grouping_vars_x <- variant_table$grouping_vars[x]
  
  # ..... Create directories
  out_dir <- paste0(out_dir.start,response_var_x,'/',out_dir_extra_x,'/')
  
  
  if(!dir.exists(out_dir)) { dir.create(out_dir, recursive = T) }
  
  
  # ..... Model assessment ====
  cat(crayon::magenta("Assessing models:",out_dir,"\n"))
  source('pipelines/model_assessment_pipeline.R', local = T)
  
  plan(multisession, workers = 1)
  
  beepr::beep()
  
})

quality_files <- list.files(out_dir.start, 
                            full.names = T, 
                            recursive = T,
                            pattern = 'model_quality_summary.csv')

all_quality <- lapply(1:length(quality_files), function(x) {
  
  
  subset <- quality_files[x] %>% str_split("/")
  subset <- subset [[1]][5]
  
  data_x <- read_csv(quality_files[x]) %>% 
    select(coral_province, 
           `Number of qualifying models` = n_good_models,
           `MAE ratio selected models` = good_models_absolute_error_ratio.mean,
           `Predictive R2 selected models` = good_models_r_squared.predictions.mean) %>% 
    mutate(subset = subset)
  
}) %>% bind_rows()

all_quality <- all_quality %>% 
  filter(subset != "total_coral_cover") %>% 
  pivot_wider(id_cols = coral_province,
              names_from = subset,
              values_from = c("Number of qualifying models",
                              "MAE ratio selected models",
                              "Predictive R2 selected models"))

write_csv(all_quality,
          paste0(out_dir.start,'combined_results/life_history_cover_pc/model_performance_table_all.csv'))


# ..... Generate plots ====  
lapply(1:nrow(variant_table), function(x) {
  
  # ..... Get variant values ====
  
  out_dir_extra_x <- variant_table$out_dir_extra[[x]]
  out_name_extra_x <- variant_table$out_name_extra[x]
  
  filter_function_x <- variant_table$filter_function[[x]]
  
  response_var_x <- variant_table$response_var[[x]]
  response_var_x.short <- variant_table$response_var.short[[x]]
  
  grouping_vars_x <- variant_table$grouping_vars[x]
  
  # ..... Create directories
  out_dir <- paste0(out_dir.start,response_var_x,'/',out_dir_extra_x,'/')
  
  
  if(!dir.exists(out_dir)) { dir.create(out_dir, recursive = T) }
  
  
  # ..... Model assessment ====
  cat(crayon::magenta("Generating plots:",out_dir,"\n"))
  source('pipelines/model_plotting.r', local = T)
  
  plan(multisession, workers = 1)
  
  beepr::beep()
  
})


# Generating predictions: generalist  model_predictions_Africa-India_1980_10m 
# ..... Generate predictions ====  
lapply(1:nrow(variant_table), 
       function(x) {
         
         # ..... Get variant values ====
         
         out_dir_extra_x <- variant_table$out_dir_extra[[x]]
         out_name_extra_x <- variant_table$out_name_extra[x]
         
         filter_function_x <- variant_table$filter_function[[x]]
         
         response_var_x <- variant_table$response_var[[x]]
         response_var_x.short <- variant_table$response_var.short[[x]]
         
         grouping_vars_x <- variant_table$grouping_vars[x]
         
         # ..... Create directories
         out_dir <- paste0(out_dir.start,response_var_x,'/',out_dir_extra_x,'/')
         
         if(!dir.exists(out_dir)) { dir.create(out_dir, recursive = T) }
         
         year_and_provinces <- expand_grid(province = prediction_provinces,
                                           year = prediction_years,
                                           depth_m.observed = prediction_depths)
         
         
         
         
         model_filter_table <- read_csv(paste0(out_dir,"model_filter_table.csv"))
         

         # ..... year and province loop ====
         lapply(1:nrow(year_and_provinces), 
                function(y) {
                  
                  in_dir_prediction_data <- "Z:/Dropbox/Lenfest_kenya/data/PROJECT_SPECIFIC/bloomberg/modelling_data/20250906_corrected_provinces/"
                  
                  prediction_files <- list.files(in_dir_prediction_data, full.names = T)
                  
                  province_y <- year_and_provinces$province[y]
                  year_y <- year_and_provinces$year[y]
                  
                  depth_y <- year_and_provinces$depth_m.observed[y]
                  
                  pred_dir_extra <- paste0(province_y,'_',year_y,'_',depth_y,'m/')
                  
                  out_name.preds_y <- paste0("model_predictions_",province_y,"_",year_y,"_",depth_y,"m")
                  
                  
                  file_y <- prediction_files[which(str_detect(prediction_files, year_y))]
                  
                  prediction_data <- read_parquet(file_y, as_data_frame = T) %>%
                    prediction_data_filter(prediction_provinces = province_y) %>%
                    
                    rename(pu_lon = pu_x,
                           pu_lat = pu_y) %>%
                    
                    mutate(across(any_of(features.numeric), as.numeric)) %>%
                    mutate(across(any_of(features.numeric), signif, digits = 7)) %>%
                    
                    mutate(depth_m.observed = depth_y)
                  
                  if(nrow(prediction_data) > 0) {
                    
                    cat(crayon::magenta("Generating predictions:",out_name.preds_y,"\n"))
                    source('pipelines/prediction_pipeline.R', local = T)
                    
                  } else {
                    
                    cat(crayon::yellow("No data for province:",province_y,"\n"))
                    
                  }
                  plan(multisession, workers = 1)
                  
                  beepr::beep()
                  
                  
                })
         
         
       })




# ..... Residuals analysis ====  
lapply(1:nrow(variant_table), function(x) {
  
  # ..... Get variant values ====
  
  out_dir_extra_x <- variant_table$out_dir_extra[[x]]
  out_name_extra_x <- variant_table$out_name_extra[x]
  
  filter_function_x <- variant_table$filter_function[[x]]
  
  response_var_x <- variant_table$response_var[[x]]
  response_var_x.short <- variant_table$response_var.short[[x]]
  
  grouping_vars_x <- variant_table$grouping_vars[x]
  
  # ..... Create directories
  out_dir <- paste0(out_dir.start,response_var_x,'/',out_dir_extra_x,'/')
  
  
  if(!dir.exists(out_dir)) { dir.create(out_dir, recursive = T) }
  
  
  # ..... Model assessment ====
  cat(crayon::magenta("Generating residual analysis for:",out_dir,"\n"))
  source('pipelines/residuals_analysis.R', local = T)
  
  plan(multisession, workers = 1)
  
  beepr::beep()
  
})



