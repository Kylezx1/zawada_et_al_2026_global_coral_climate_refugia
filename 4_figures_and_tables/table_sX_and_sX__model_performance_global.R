# Bloomberg paper figure 1: model accuracy report
rm(list=ls())


# Setup ====

library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggforce)
library(patchwork)
library(colorspace)
library(rnaturalearth)
library(rnaturalearthdata)

source('functions.R')

set.seed(42)

in_file.accuracy <- "output/coral/bloomberg_20250913/coral_cover_pc/total_coral_cover/fitted_values/model_predictions_raw.csv"


province_drop <- c("Brazil", "West Africa")

map_bounds_buffer_m <- 500000  # 500 km
pie_radius_m <- 250000  

colours.global <- "#990011"

response <- "coral_cover_pc"

wdg <- position_dodge2(width = 0.7, preserve = "single", padding = 0.05)


model_dir.base <- "output/coral/bloomberg_20250913/"

model_response_dirs <- c("coral_cover_pc/total_coral_cover.all_data/",
                         "competitive_cover_pc/competitive/",
                         "stress_tolerant_cover_pc/stress_tolerant/",
                         "weedy_cover_pc/weedy/")

response_vars <- c("coral_cover_pc",
                   "competitive_cover_pc",
                   "stress_tolerant_cover_pc",
                   "weedy_cover_pc")

# Main ====


x = 1
training_stats_all_responses <- lapply(1:length(model_response_dirs), function(x) {
  
  
  response_var_x = response_vars[x]
  

  cat(crayon::cyan(".....Combining model outputs\n"))
  out_dir.temp <- paste0(model_dir.base,model_response_dirs[x],'TEMP/')
  temp_files.full <- list.files(out_dir.temp, full.names = T)
  
  xgb_models <- lapply(1:length(temp_files.full), function(x) {
    print(x)
    read_rds(temp_files.full[x])
  })
  
  training_stats <- lapply(1:length(xgb_models), function(y) {
    
    residuals <- xgb_models[[y]]$residuals
    training <- xgb_models[[y]]$xgb_data_list$train[[response_var_x]]
    coral_province <- xgb_models[[y]]$xgb_data_list$train$coral_province
    model_number <- y
    
    out_x <- tibble(response = response_var_x,
                    model_number = model_number,
                    coral_province = coral_province,
                    residuals = residuals,
                    training = training)
    
  }) %>% bind_rows()
  
}) %>% bind_rows()




training_stats.summary <- training_stats_all_responses %>% 
  
  group_by(response, model_number) %>% 
  
  summarise(r2_train = 1 - sum((residuals)^2) / sum((training - mean(training))^2) %>% round(1),
            mae_train = mean(abs(residuals))*100 %>% round(1)) %>% 
  
  ungroup() %>% 
  group_by(response) %>% 
  
  summarise(r2_sd = sd(r2_train),
            r2_mean = mean(r2_train),
            mae_sd = sd(mae_train),
            mae_mean = mean(mae_train))


write_csv(x = training_stats.summary,
          file = "bloomberg_figures/erl_revision/bloomberg_paper_table_training_statistics.csv")


training_stats.summary.province <- training_stats_all_responses %>% 
  
  # filter(response == "competitive_cover_pc",
  #        coral_province == "Japan-Vietnam",
  #        model_number == 61
  #        ) %>%
  
  group_by(response, coral_province, model_number) %>% 
  
  summarise(r2_train = (1 - sum((residuals)^2) / sum((training - mean(training))^2)) %>% round(1),
            mae_train = (mean(abs(residuals))*100) %>% round(1)) %>% 
  
  filter(is.finite(r2_train),
         is.finite(mae_train)) %>% 
  
  ungroup() %>% 
  group_by(response, coral_province) %>% 
  
  summarise(n_models = n(),
            r2_sd = sd(r2_train),
            r2_mean = mean(r2_train),
            mae_sd = sd(mae_train),
            mae_mean = mean(mae_train))

write_csv(x = training_stats.summary.province,
          file = "bloomberg_figures/erl_revision/bloomberg_paper_table_training_statistics_by_province.csv")





# Testing 
testing_stats_all_responses <- lapply(1:length(model_response_dirs), function(x) {
  
  
  response_var_x = response_vars[x]
  
  
  cat(crayon::cyan(".....Combining model outputs\n"))
  out_dir.temp <- paste0(model_dir.base,model_response_dirs[x],'TEMP/')
  temp_files.full <- list.files(out_dir.temp, full.names = T)
  
  xgb_models <- lapply(1:length(temp_files.full), function(x) {
    print(x)
    read_rds(temp_files.full[x])
  })
  
  testing_stats <- lapply(1:length(xgb_models), function(y) {
    
    residuals <- xgb_models[[y]]$predictions$error.model
    testing <- xgb_models[[y]]$xgb_data_list$test[[response_var_x]]
    coral_province <- xgb_models[[y]]$xgb_data_list$test$coral_province
    model_number <- y
    
    out_x <- tibble(response = response_var_x,
                    model_number = model_number,
                    coral_province = coral_province,
                    residuals = residuals,
                    testing = testing)
    
  }) %>% bind_rows()
  
}) %>% bind_rows()




testing_stats.summary <- testing_stats_all_responses %>% 
  
  group_by(response, model_number) %>% 
  
  summarise(r2_test = 1 - sum((residuals)^2) / sum((testing - mean(testing))^2) %>% round(1),
            mae_test = mean(abs(residuals))*100 %>% round(1)) %>% 
  
  ungroup() %>% 
  group_by(response) %>% 
  
  summarise(r2_sd = sd(r2_test),
            r2_mean = mean(r2_test),
            mae_sd = sd(mae_test),
            mae_mean = mean(mae_test))


write_csv(x = testing_stats.summary,
          file = "bloomberg_figures/erl_revision/bloomberg_paper_table_testing_statistics.csv")


testing_stats.summary.province <- testing_stats_all_responses %>% 
  
  # filter(response == "competitive_cover_pc",
  #        coral_province == "Japan-Vietnam",
  #        model_number == 61
  #        ) %>%
  
  group_by(response, coral_province, model_number) %>% 
  
  summarise(r2_test = (1 - sum((residuals)^2) / sum((testing - mean(testing))^2)) %>% round(1),
            mae_test = (mean(abs(residuals))*100) %>% round(1)) %>% 
  
  filter(is.finite(r2_test),
         is.finite(mae_test)) %>% 
  
  ungroup() %>% 
  group_by(response, coral_province) %>% 
  
  summarise(n_models = n(),
            r2_sd = sd(r2_test),
            r2_mean = mean(r2_test),
            mae_sd = sd(mae_test),
            mae_mean = mean(mae_test))

write_csv(x = testing_stats.summary.province,
          file = "bloomberg_figures/erl_revision/bloomberg_paper_table_testing_statistics_by_province.csv")


