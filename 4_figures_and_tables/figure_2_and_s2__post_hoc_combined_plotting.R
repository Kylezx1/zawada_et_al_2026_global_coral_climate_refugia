# Bloomberg post hoc analysis

# Set up ====

rm(list=ls())

source('machine_learning_functions.r')
source('functions.r')
library(tidyverse)
library(ggbeeswarm)
library(egg)
library(stringi)
library(ggh4x)
library(ggtext)
library(snakecase)
library(colorspace)
library(tidytext)

in_dir <-  "output/coral/bloomberg_20250913/"

out_dir <- "output/coral/bloomberg_20250913/combined_results/"

in_file.variable_renaming <- 'data/predictor_variable_naming_table.csv'

shap_files <- list.files(in_dir, pattern = 'shap_values.csv', full.names = T, recursive = T)

model_dirs <- list.files(in_dir, pattern = 'TEMP', all.files = T, recursive = T, full.names = T, include.dirs = T)

analysis_subsets <- list.dirs(in_dir, recursive = F, full.names = F)

#colours <- c('#008877', '#0055AA','#AA0055', '#5500AA', '#AA5500')

colours <- c('#AA0055', '#d55e00','#0072b2', '#019e73', '#006666')
colours <- colours[1:4]


shap_summary_colours <- c('#AA0055', 
                          darken('#AA0055',0.2),
                          
                          
                          '#d55e00',
                          darken('#d55e00', 0.2),
                          
                          '#0072b2',
                          darken('#0072b2',0.2),
                          
                          '#019e73',
                          darken('#019e73',0.2),
                          
                          '#006666',
                          darken('#006666',0.2))
shap_summary_colours <- shap_summary_colours[1:8]

id_vars <- c(
  "observation_id",
  "coral_province",
  "country",
  "ecoregion",
  "observer",
  "pu_lat",
  "pu_lon",
  "min_yr",
  "max_yr")

features.numeric <- c(
  
  
  # ..... Low direct mechanism ====
  # "pu_lat",
  # "pu_lon",
  # 'time',
  
  # ..... Thermal stress ====
  
  "dhw_max_6_year_max",
  "dhw_max_6_year_mean",
  
  # "sst_rate_of_rise_10_year",
  # "sst_rate_of_rise_34_year",
  # 
  #   "sst_kurtosis_6_year_mean",
  #   "sst_kurtosis_34_year_mean",
  # 
  #   # "sst_max_6_year_mean",
  #   # "sst_max_34_year_mean",
  # 
  #   # "sst_min_6_year_mean",
  #   # "sst_q05_6_year_mean",
  # 
  #   "sst_q90_6_year_mean",
  #   "sst_q90_34_year_mean",
  # 
  #   # "sst_q95_6_year_mean",
  #   # "sst_q95_34_year_mean",
  # 
  #   "sst_skewness_6_year_mean",
  #   "sst_skewness_34_year_mean",
  
  'sst_climatology_noaa',
  # 
  #   'sst_bimodality_6_year_mean',
  #   "sst_bimodality_34_year_mean",
  
  
  # ..... Environmental suitability ====
  
  # "temp_mean_max_depth_2020_bio_oracle_3",
  # 
  #   "sst_mean_6_year_mean",
  #   "sst_mean_34_year_mean",
  
  # "sst_min_34_year_mean",
  
  # "sst_q05_34_year_mean",
  # 
  #   "sst_sd_6_year_mean",
  #   "sst_sd_34_year_mean",
  
  #
  # "par_mean_1998_to_2019",
  # "kdpar_mean_1998_to_2019",
  
  
  "delta_sst_mean_6_year_mean",
  "delta_sst_median_6_year_mean",
  "delta_sst_q90_6_year_mean",
  "delta_sst_sd_6_year_mean",
  "delta_sst_skewness_6_year_mean",
  "delta_sst_kurtosis_6_year_mean",
  
  
  "salinity_mean_max_depth_2020_bio_oracle_3",
  
  # "mean_sea_level_rate_of_rise_23_year",
  
  
  # ..... Nutrients and sedimentation ====
  
  'andrello_sediments',
  
  
  # "rusle_sedimentation",
  
  # "cdd_mean_1998_to_2019",
  
  # "chl_a_mean_2003_to_2011",
  #
  # "tsm_mean_2003_to_2011",
  
  
  # .... Trace elements ====
  
  'calcite_mean_surface_bio_oracle_2.2',
  
  "iron_mean_max_depth_2020_bio_oracle_3",
  
  "nitrate_mean_max_depth_2020_bio_oracle_3",
  "oxygen_mean_max_depth_2020_bio_oracle_3",
  
  "ph_mean_max_depth_2020_bio_oracle_3",
  "phosphate_mean_max_depth_2020_bio_oracle_3",
  "p04_to_n03_mean_max_depth_2020_bio_oracle_3",
  
  
  # ..... Physical variables ====
  "depth_gebco",
  
  # "depth_m.observed",
  
  "slope_mean_2020_bio_oracle_3",
  
  "min_distance_from_land_gebco",
  "min_distance_from_500m_depth_gebco",
  
  "terrain_ruggedness_index_2020_bio_oracle_3",
  "topographic_position_index_2020_bio_oracle_3",
  "current_velocity_mean_max_depth_2020_bio_oracle_3",
  #
  # "distance_to_mangrove",
  # "distance_to_seagrass",
  
  # "slope_bio_oracle_3",
  
  
  # ..... Human pressure ====
  
  # "human_gravity_nearest_city_or_market",
  # "human_gravity_nearest_population",
  "human_gravity_total",
  # "travel_time_to_market",
  # "log_gravity_maina" #,
  
  
  
  # ..... Seascape integrity ====
  
  # "mangrove_support",
  # "seagrass_support",
  #
  # ..... Conservation governance ====
  # "conservation_protection_status"
  
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

features.one_hot <- c('coral_province')
features.one_hot <- NA

features.one_hot.drop <- 'coral_province'

features.all <- c(features.one_hot, features.numeric) %>% na.omit()


facet_bool <- T
cat_bool <- F
smooth_method <- 'gam'
smooth_formula <- "y ~ s(x, bs = 'tp', k = 10)" %>% as.formula()


analysis_groups <- list(
  
  
  lh_cover = list(analysis_subsets = list(c("coral_cover_pc",
                                            "weedy_cover_pc",
                                            "competitive_cover_pc",
                                            "stress_tolerant_cover_pc" #,
                                            #"generalist_cover_pc"
                                            )),
                      analysis_subsets_droptext = '_cover_pc',
                      subset_type.full = 'life history cover %',
                      subset_type.clean = 'life_history_cover_pc',
                  data_filter = list(function(data) { return(data) }),
                  rescale_response = F)
  
  
)


data_update_function <- function(data) {
  
  data %>% 
    mutate(!!subset_type.clean_x := str_replace(get(subset_type.clean_x),
                                                pattern = analysis_subsets_droptext_x, 
                                                replacement = "")) %>% 
    mutate(life_history_cover_pc = ifelse(life_history_cover_pc == 'coral', 'total_coral', life_history_cover_pc)) %>% 
    mutate(life_history_cover_pc = factor(life_history_cover_pc, levels = c(
      'total_coral',
      'competitive',
      'stress_tolerant',
      'weedy' #,
      #'generalist'
    )))
  
}


shap_summary_update_function <- function(data) {
  
  data %>% 
    mutate(subset_even = factor(subset_even, levels = c(
      'total_coral_FALSE',
      'total_coral_TRUE',
      'competitive_FALSE',
      'competitive_TRUE',
      'stress_tolerant_FALSE',
      'stress_tolerant_TRUE',
      'weedy_FALSE',
      'weedy_TRUE'#,
      #'generalist_FALSE',
      #'generalist_TRUE'
    )))
  
}



setups <- bind_rows(analysis_groups)

overwrite.plots <- F


# Main ====

data.variable_table <- read_csv(in_file.variable_renaming)


setup = 1
lapply(1:nrow(setups), function(setup) {
  
  setup_x <- setups[setup,]
  
  analysis_subsets_x <- setup_x$analysis_subsets[[1]] %>% sort()
  
  analysis_subsets_droptext_x <- setup_x$analysis_subsets_droptext
  
  subset_type.full_x <- setup_x$subset_type.full
  subset_type.clean_x <- setup_x$subset_type.clean
  
  data_filter_x <- setup_x$data_filter[[1]]
  
  rescale_response_x <- setup_x$rescale_response
  
  
  analysis_subsets.regex <- paste0(analysis_subsets_x, collapse = '|')
  
  shap_files_x <- shap_files[which(str_detect(shap_files, analysis_subsets.regex))]
  
  model_dirs_x <- model_dirs[which(str_detect(model_dirs, analysis_subsets.regex))]
  
  bloomberg_out_dir <- "Z:/Dropbox/Global Coral Refugia Life Histories/bloomberg_coral_sanctuaries_2024/papers/1_global loss and refugia/figures and tables/"
  
  out_dir_x <- paste0(out_dir,subset_type.clean_x,'/')
  
  if(!dir.exists(out_dir_x)) dir.create(out_dir_x, recursive = T)
  
  
  cat(crayon::magenta("Executing for analysis group:",subset_type.clean_x,'\n'))
  
  # ..... SHAP Partial plots ====
  
  cat(crayon::magenta(".....SHAP Partial plots\n"))
  smart.lapply(1:length(features.all), cores = 1, function(x) {
    
    variable_x <- features.all[x]
    
    out_name_x <- paste0(out_dir_x,subset_type.clean_x,"__",variable_x,"_shap_values.png")
    
    
    
    if(file.exists(out_name_x) && overwrite.plots == F) {
      
      cat(crayon::yellow("..........file",out_name_x,"already exists. Skipping\n"))
      
      
    } else {
      
      
      cat(crayon::green("...Running for",subset_type.full_x,', variable',variable_x,'\n'))
      
      plot_data.shap <- smart.lapply(1:length(analysis_subsets_x), cores = 5, function(y) {
        
        print(y)
        
        subset_x <- analysis_subsets_x[[y]]
        shaps_x <- shap_files_x[y]
        
        model_dir_x <- model_dirs_x[y]
        model_list.full <- list.files(model_dir_x, full.names = T)
        
        xgb_models <- lapply(model_list.full, read_rds)
        
        features_all_models <- lapply(1:length(xgb_models), function(z) {xgb_models[[z]]$xgb_data_list$features}) %>%
          unlist() %>%
          unique() %>% 
          sort()
        
        # out_name.shaps_y <- paste0(out_dir.shaps,'shap_values_',response_var_x,'__',variable_x,'.png')
        
        if(is.na(features.one_hot[1])) {
          cat_bool_y <- F
        } else {
          cat_bool_y <- ifelse(str_detect(variable_x, features.one_hot) %>% sum() >= 1, T, F) 
        }
    
        
        data_x <- combine_shaps_and_variables(xgb_model_objs = xgb_models,
                                              variable = variable_x,
                                              summary_bool = T,
                                              type = 'train',
                                              id_vars = id_vars)
        
        
        if(nrow(data_x) == 0) {
          
          return(tibble())
          
        } else {
          
          data_x <- data_x %>% 
            mutate(!!subset_type.clean_x := subset_x)
          
          if(rescale_response_x) {
            
            data_x <- data_x %>%
              mutate(!!paste0(variable_x,".shap.mean") := rescale_0_to_1(get(paste0(variable_x,".shap.mean"))))
            
          }
          
          data_x
          
        }
        
      })
      
      plot_data.shap <- plot_data.shap %>% 
        bind_rows()
      
      if(nrow(plot_data.shap) == 0) {
        
        return(tibble())
        
      } else {
        
        plot_data.shap <- plot_data.shap %>% 
          ungroup() %>% 
          data_update_function() %>% 
          mutate(weights = get(paste0(variable_x, '.shap.sd')) %>% rescale_0_to_1())
        
        
        plot_data.shap <- plot_data.shap %>%
          
          mutate(plus_sd = ifelse(is.na(plus_sd),0,plus_sd),
                 minus_sd = ifelse(is.na(minus_sd),0,minus_sd))
        
        
        n_cats <- plot_data.shap[[subset_type.clean_x]] %>% unique() %>% length()
        
        
        # if(!is.na(colours) && length(colours) == n_cats) {
        #   
        #   colours <- colours[1:n_cats]
        #   
        # } else {
        #   
        #   colours <- interleave_colors(num_colors = n_cats,
        #                                num_splits = 2,
        #                                end = 0.9)
        #   
        # }
        
        
        plot_data.shap <- rename_from_lookup(plot_data.shap, data.variable_table)
        
        variable_x.plot <- generic_rename(variable_x, data.variable_table)
        
        facet_levels <- levels(plot_data.shap[[subset_type.clean_x]])
        
        tags <- tibble(
          !!subset_type.clean_x := factor(facet_levels,
                                          levels = facet_levels),
          tag = LETTERS[seq_along(facet_levels)]
        )
        
        
        plot <- ggplot(plot_data.shap,
                       aes(
                         x = get(variable_x.plot),
                         y = get(paste0(variable_x.plot, '.shap.mean')),
                         
                         colour = get(subset_type.clean_x),
                         fill = get(subset_type.clean_x),
                         
                         weight = weights
                         
                         
                       ))+
          
          geom_hline(yintercept = 0, linetype = 'dashed') 
        
        if(cat_bool) {
          
          plot <- plot + geom_boxplot(aes(x = as.character(get(variable_x.plot))),
                                      colour = '#555577')
          
        } else {
          
          plot <- plot + 
            
            geom_errorbar(colour = 'grey', width = 0, alpha = 0.5,
                          aes(ymax = plus_sd,
                              ymin = minus_sd)) +
            # geom_point(size = 3, shape = 21, fill = '#555577', colour = 'white') +
            # geom_point(size = 3, shape = 21, alpha = 0.2) +
            
            # geom_smooth(method= 'lm', linewidth = 1, alpha = 0.2) +
            # geom_smooth(method= 'lm', colour = "white", se = F, linewidth = 2) +
            # geom_smooth(method= 'lm', linewidth = 1, se = F, linetype = 'dashed') +
            
            # geom_smooth(method= smooth_method, linewidth = 1, alpha = 0.2) +
            # geom_smooth(method= smooth_method, colour = "white", se = F, linewidth = 2) +
            # stat_smooth(geom = 'line', method= smooth_method, formula = smooth_formula, linewidth = 1, se = F, alpha = 0.2,
            #             aes(group = `Coral faunal province`)) +
            
            
            geom_smooth(method= smooth_method, formula = smooth_formula, linewidth = 1, alpha = 0.2) +
            geom_smooth(method= smooth_method, formula = smooth_formula, colour = "white", se = F, linewidth = 2) +
            geom_smooth(method= smooth_method, formula = smooth_formula, linewidth = 1, se = F)
            
           
          # geom_smooth(method= 'lm', colour = 'darkred')
          # geom_hex() +
          
        }
        
        
        if(facet_bool) {
          
          plot <- plot + 
            facet_wrap(~get(subset_type.clean_x), scales = "free_y")
          
         plot <-  plot + 
           geom_text(data = tags, aes(label = tag), 
                     x = -Inf, 
                     y =  Inf,
                     hjust = -0.4, 
                     vjust = 1.4,
                     size = 5,
                     inherit.aes = F)
          
        }
        
        plot <- plot + 
          
          # scale_colour_viridis_d() +
          # scale_fill_viridis_d() +
          
          scale_fill_manual(values = colours) +
          scale_colour_manual(values = colours) +
          
          
          
          # scale_fill_manual(values = c("#AA0055", "#555555", "#00AA55", "#0055AA", "#5500AA")) +
          # scale_colour_manual(values = c("#AA0055", "#555555", "#00AA55", "#0055AA", "#5500AA")) +
          
          labs(y = paste0('Effect on response output'),
               x = paste0(variable_x.plot),
               colour = NULL,
               fill = NULL) +
          
          guides(colour = 'none',
                 fill = 'none') +
          
          theme_article() +
          
          theme(panel.grid = element_blank(),
                plot.background = element_rect(fill = 'white'))
        
        plot <- plot +
          
          scale_y_continuous(labels = function(x) x * 100)
          
        
        # plot
        
        ggsave(filename = out_name_x,
               plot = plot,
               height = 0.4714*1200,
               width = 1200,
               scale = 2,
               units = 'px')
      }
        
    }
    
      
  }
    
  ) %>% time_it(T)
  
  
  # ..... Shap correlation plots ====
  cat(crayon::magenta(".....SHAP Correlation plots\n"))
  
  
  out_name_x <- paste0(out_dir_x,subset_type.clean_x,"_shap_correlations.png")
  out_name_x.data <- paste0(out_dir_x,subset_type.clean_x,"_shap_correlations.csv")
  
  if(file.exists(out_name_x) && overwrite.plots == F) {
    
    cat(crayon::yellow("..........file",out_name_x,"already exists. Skipping\n"))
    plot_data.shap.cor <- read_csv(out_name_x.data)
    
  } else {
  
    
    cat(crayon::green("..........generating SHAP correlation plot for",subset_type.full_x,'\n'))
    plot_data.shap.cor <- smart.lapply(1:length(analysis_subsets_x),
                                   function(x) {
      
      subset_x <- analysis_subsets_x[x]
      shaps_x <- shap_files_x[x]
      
      model_dir_x <- model_dirs_x[x]
      model_list.full <- list.files(model_dir_x, full.names = T)
      
      cat(crayon::green("...............Aggregating SHAP correlations for",subset_x))
      
      xgb_models <- lapply(model_list.full, read_rds)
      
      features_all_models <- lapply(1:length(xgb_models), function(y) {xgb_models[[y]]$xgb_data_list$features}) %>%
        unlist() %>%
        unique() %>% 
        sort()
      
      # out_name.shaps_y <- paste0(out_dir.shaps,'shap_values_',response_var_x,'__',variable_x,'.png')
      
      # if(is.na(features.one_hot[1])) {
      #   cat_bool_y <- F
      # } else {
      #   cat_bool_y <- ifelse(str_detect(variable_x, features.one_hot) %>% sum() >= 1, T, F) 
      # }
      
      data_x <- get_shap_correlation_data(xgb_model_objs = xgb_models, 
                                          variables = features_all_models,
                                          type = 'train', 
                                          id_vars = id_vars,
                                          output = 'correlation')
      
      data_x <- data_x %>% 
        mutate(!!subset_type.clean_x := subset_x)
      
    }) %>% bind_rows()
    
    
    plot_data.shap.cor <- plot_data.shap.cor %>% 
      ungroup() %>% 
      data_update_function()
    
    plot_data.shap.cor <- plot_data.shap.cor %>% 
      left_join(data.variable_table %>% rename(feature = variable)) %>% 
      mutate(feature_new = updated_variable) %>% 
      mutate(feature_new = ifelse(!is.na(units), 
                              paste0(feature_new, "\n(", units, ")"),
                              feature_new)) %>% 
      mutate(feature_new = ifelse(is.na(feature_new), 
                                  feature, 
                                  feature_new))
    
    levels(plot_data.shap.cor[[subset_type.clean_x]]) <- to_sentence_case( levels(plot_data.shap.cor[[subset_type.clean_x]]))
    plot_data.shap.cor[['variable_group']] <- to_sentence_case(plot_data.shap.cor[['variable_group']])
    
    write_csv(plot_data.shap.cor, out_name_x.data)

    shap_plot <- ggplot(data = plot_data.shap.cor %>% 
                          filter(!str_detect(feature,
                                            paste0(features.one_hot.drop,collapse = '|'))),
                        aes(x = mean_shap_cor, 
                            xmin = mean_shap_cor - sd_shap_cor,
                            xmax = mean_shap_cor + sd_shap_cor,
                            y = reorder(feature_new, mean_shap_cor),
                            fill = mean_shap_cor,
                            size = n_times_in_model
                            )) +
      
      geom_vline(aes(xintercept = 0,  colour = get(subset_type.clean_x)), linewidth = 8, alpha = 0.2) +
      
      geom_vline(xintercept = 0, linewidth = 1, colour = 'white', alpha = 0.4) +
      geom_vline(xintercept = 0, linewidth = 0.5, colour = 'black', linetype = 'dashed', alpha = 0.4) +
      
      
      geom_errorbarh(linewidth = 0.5) +
      
      geom_point(shape = 21) +
      
      facet_grid(variable_group~get(subset_type.clean_x), 
                 scales = 'free',
                 space = 'free_y') +
      
      # scale_fill_viridis_d() +
      scale_fill_distiller(type = "div", palette = "RdBu", direction = 1)  +
      # scale_x_continuous(expand = c(0, 0)) + 
      scale_size_continuous(range = c(1,4)) +
      scale_color_manual(values = colours) +
      
      
      
      # coord_flip() +
      
      guides(
        fill = guide_colourbar(
          # legend.text.position = "center",
          nrow   = 1,
          byrow  = TRUE,
          title.position = "top"
        ),
        
        size = guide_legend(
          # legend.text.position = "center",
          nrow   = 1,
          byrow  = TRUE,
          title.position = "top",
          override.aes = list(linetype = 0)
        ),
        
        linewidth = "none",
        linetype = 'none',
        colour = 'none'
        
      ) +
      
      labs(y = 'Variable',
           x = 'Directionality\n(Mean SHAP correlation +/- SD)',
           fill = 'Directionality\n(Mean SHAP correlation +/- SD)',
           size = 'N times feature in model') +
      
      theme_bw(base_size = 12) +
      theme(# axis.text.x = element_blank(),
        # axis.ticks.x = element_blank(),
        axis.text.y.left= element_text(face = 'bold'),
        
        # axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),
        
        strip.text.y = element_markdown(
          angle = -90,
          hjust = 0.5,
          vjust = 0.5,
        ),
        
        
        strip.text.x.top = element_markdown(
          angle = 0,
          hjust = 0.5,       
          vjust = 0,         
          face  = "bold"
        ),
        
        strip.background = element_blank(),
        # panel.spacing.y = unit(1, "lines"),
        # panel.spacing.x = unit(1, "lines"),
        
        panel.grid.major.x = element_blank(),
        
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        
        panel.grid = element_blank()) +
      
      theme(panel.grid.major.y = element_line(colour = "#EEEEEE"),
            
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.box = "horizontal")
    
    shap_plot
    
    
    ggsave(filename = out_name_x,
           plot = shap_plot,
           width = 1200,
           height = 1.4142*1200,
           scale = 2.5,
           units = 'px')
    
    
  }
    
    
    # ..... SHAP Summary plot ====
    cat(crayon::green(paste0('generating SHAP summary\n')))

    out_name_x <- paste0(out_dir_x,subset_type.clean_x,"_shap_feature_weight_summary_ranked.csv")

    if(file.exists(out_name_x) && overwrite.plots == F) {

      cat(crayon::yellow("...file",out_name_x,"already exists. Skipping\n"))
      plot_data.shap.importance.sum_sum <- read_csv(out_name_x)

    } else {

      plot_data.shap.importance.all <- lapply(1:length(analysis_subsets_x), function(x) {

        subset_x <- analysis_subsets_x[x]
        shaps_x <- shap_files_x[x]

        model_dir_x <- model_dirs_x[x]
        model_list.full <- list.files(model_dir_x, full.names = T)


        xgb_models <- lapply(model_list.full, read_rds)

        shap_values <- lapply(1:length(xgb_models), function(x) {

          out <- xgb_models[[x]]$shap_values

          out

        }) %>% bind_rows()

        # features_wide <- shap_values %>%
        #   select(-any_of(c('BIAS', 'observation_id', 'data_type', 'run',  id_vars))) %>%
        #   names()

        features_wide <- lapply(1:length(xgb_models), function(y) {xgb_models[[y]]$xgb_data_list$features}) %>%
          unlist() %>%
          unique() %>%
          sort()
        
        features_wide <- features_wide[which(!str_detect(features_wide, paste0(features.one_hot.drop,collapse = "|")))]
        
        features_wide <- paste0(features_wide,'.shap')



        shap_values.long <- shap_values %>%
          pivot_longer(cols = any_of(features_wide),
                       names_to = 'feature',
                       values_to = 'shap') %>%
          mutate(shap_abs = abs(shap)) %>%
          arrange(-shap_abs) %>%
          mutate(shap_abs = round(shap_abs, 5))


        shap_values.long <- shap_values.long %>%
          mutate(!!subset_type.clean_x := subset_x)

      }) %>% time_it(T)

      plot_data.shap.importance <- bind_rows(plot_data.shap.importance.all)

      plot_data.shap.importance <- plot_data.shap.importance %>% 
        ungroup() %>% 
        data_update_function()
      
      plot_data.shap.importance <- plot_data.shap.importance %>% 
        mutate(feature = str_replace(feature, '.shap', '')) %>% 
        left_join(data.variable_table %>% rename(feature = variable)) %>% 
        mutate(feature_new = updated_variable) %>% 
        mutate(feature_new = ifelse(!is.na(units), 
                                    paste0(feature_new, "\n(", units, ")"),
                                    feature_new)) %>% 
        mutate(feature_new = ifelse(is.na(feature_new), 
                                    feature, 
                                    feature_new))
      
      # n_cats <- plot_data.shap.importance[[subset_type.clean_x]] %>% unique() %>% length()
      # 
      # if(!is.na(colours[1]) && length(colours) == n_cats) {
      #   
      #   colours <- colours[1:n_cats]
      #   
      # } else {
      #   
      #   colours <- interleave_colors(num_colors = n_cats,
      #                                num_splits = 2,
      #                                end = 0.9)
      #   
      # }
      

      plot_data.shap.importance.sum <- plot_data.shap.importance %>%
        filter(data_type == 'train') %>%
        filter(!is.na(shap_abs)) %>% 
        group_by(across(any_of(c("feature_new", "feature", "run", subset_type.clean_x)))) %>%
        summarise(shap_abs = mean(shap_abs, na.rm = T)) %>%
        arrange(run)
      
      levels(plot_data.shap.importance.sum[[subset_type.clean_x]]) <- to_sentence_case(levels(plot_data.shap.importance.sum[[subset_type.clean_x]]))
      
      
      
      plot_data.shap.importance.sum_sum <- plot_data.shap.importance.sum %>%
        group_by(across(any_of(c("feature_new", "feature", subset_type.clean_x)))) %>%
        summarise(shap_abs_all_sum = sum(shap_abs, na.rm = T),
                  shap_abs_all_mean = mean(shap_abs, na.rm = T),
                  shap_abs_all_sd = sd(shap_abs, na.rm = T),
                  n = n())
      
      max_n <- plot_data.shap.importance.sum_sum$n %>% max()
        
      
      plot_data.shap.importance.sum_sum <- plot_data.shap.importance.sum_sum %>% 
        mutate(n_cat = ifelse(n < max_n, 'vif_inflated', 'all_models'))


      plot_data.shap.importance.sum <- plot_data.shap.importance.sum %>% 
        left_join(plot_data.shap.importance.sum_sum)
      
      
      colour_table <- expand_grid(!!subset_type.clean_x := plot_data.shap.importance.sum[[subset_type.clean_x]] %>% unique(),
                                  run = plot_data.shap.importance.sum$run %>% unique()) %>% 
        mutate(is_even = ifelse(run %% 2 == 0, T, F)) %>% 
        mutate(subset_even = paste0(get(subset_type.clean_x), '_', is_even))
    
      
      plot_data.shap.importance.sum <- plot_data.shap.importance.sum %>% 
        ungroup() %>% 
        left_join(colour_table) %>% 
        shap_summary_update_function()
      
      
      plot_data.shap.importance.sum <- plot_data.shap.importance.sum %>%
        left_join(data.variable_table %>% rename(feature = variable))
      
      plot_data.shap.importance.sum[['variable_group']] <- to_sentence_case(plot_data.shap.importance.sum[['variable_group']])
     
      plot_data.shap.importance.sum_sum <- plot_data.shap.importance.sum_sum %>% 
        group_by(across(any_of(subset_type.clean_x))) %>%               
        arrange(desc(shap_abs_all_mean)) %>%
        mutate(feature_rank = row_number())  
      
      write_csv(plot_data.shap.importance.sum_sum, 
                 file = paste0(out_dir_x,subset_type.clean_x,"_shap_feature_weight_summary_ranked.csv"))
      
      
      shap_plot <- ggplot(data = plot_data.shap.importance.sum,
                          aes(y = shap_abs_all_mean,
                              x = feature_new,
                              fill = get(subset_type.clean_x),
                              colour = get(subset_type.clean_x))) +

        geom_hline(yintercept = 0) +

        geom_bar(alpha = 0.6, linewidth = 0.5,
                 stat = 'summary', fun = 'mean',
                 position = 'dodge') +

        geom_errorbar(aes(ymin = shap_abs_all_mean - shap_abs_all_sd,
                          ymax = shap_abs_all_mean),
                      width = 0.2, linewidth = 0.5, position = position_dodge(0.9),
                      colour = 'white') +
        
        geom_errorbar(aes(ymin = shap_abs_all_mean,
                          ymax = shap_abs_all_mean + shap_abs_all_sd),
                      width = 0.2, linewidth = 0.5, position = position_dodge(0.9)) +
        
        
        # geom_bar(fill = NA, colour = 'black',
        #          stat = 'summary', fun = 'mean',
        #          aes(y = shap_abs,
        #              x = reorder(feature_new, shap_abs_all_sum)),
        #          inherit.aes = F) +


        # facet_grid(n_cat~get(subset_type.clean_x), scales = 'free', space = 'free_y') +

        facet_grid(variable_group~get(subset_type.clean_x), 
                   scales = 'free',
                   space = 'free_y') +
        
        # facet_grid2(variable_group ~ get(subset_type.clean_x),
        #             scales      = "free_y",
        #             switch      = "x",
        #             independent = "y",
        #             strip = strip_nested(by_layer_y = T,
        #                                  by_layer_x = F,
        #                                  size = 'variable'
        #             ),
        #             shrink = T,
        # 
        #             labeller   = labeller(variable_group = label_value,
        #                                   feature_new = label_value,
        #                                   !!subset_type.clean_x := label_both)
        # 
        # ) +
        
        
        
        # scale_fill_viridis_d() +
        scale_fill_manual(values = colours) +
        scale_colour_manual(values = colours) +

        scale_x_discrete(limits=rev) +
        
        scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.03, 0.04, 0.05),
                           labels = c('0%', '0.5%', '1%', '2%', '3%', '4%', '5%'),
                           expand = expansion(mult = c(0, .1))) +
        
        
        coord_flip() +

        labs(x = 'feature',
             y = 'Mean absolute SHAP\n+/- SD',
             fill = NULL,
             colour = NULL) +

        guides(fill = 'none',
               colour = 'none') +

        theme_article(base_size = 12) +
        
        theme(panel.grid = element_blank(),
              plot.background = element_rect(fill = 'white'),
              
              axis.text.y = element_text(face = 'bold'),
              
              axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
              # axis.ticks = element_blank(), 
              
              strip.placement = "outside",
              strip.text.y.left = element_text(
                angle = 0,
                hjust = 0,
                vjust = 0.5,
                face  = "bold"
              ),
              
              
              strip.text.x.top = element_text(
                angle = 0,
                hjust = 0.5,       
                vjust = 0,         
                face  = "bold"
              ),
              
              
              strip.background = element_blank(),
              # panel.spacing.y = unit(1, "lines"),
              panel.spacing.x = unit(0.5, "lines"),
              
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_blank(),
              
              legend.position = "bottom",
              legend.direction = "horizontal",
              legend.box = "horizontal")
      

      shap_plot

      ggsave(filename = out_name_x,
             plot = shap_plot,
             width = 1200,
             height = 1.4142*1200,
             scale = 2.5,
             units = 'px')

    }
  
  
  # ..... SHAP Cor + Importance plot ====
  
  out_name_x <- paste0(out_dir_x,subset_type.clean_x,"_shap_correlation_and_importance_points.png")
  
  plot_data.shap.cor_importance <- plot_data.shap.cor %>% 
    full_join(plot_data.shap.importance.sum_sum)
  
  plot_data.shap.cor_importance <- plot_data.shap.cor_importance %>% 
    group_by(get(subset_type.clean_x)) %>% 
    mutate(
      shap_abs_all_mean.01 = (shap_abs_all_mean - min(shap_abs_all_mean, na.rm = TRUE)) /
        (max(shap_abs_all_mean, na.rm = TRUE) - min(shap_abs_all_mean, na.rm = TRUE))
    ) %>%
    ungroup()
  
  plot_data.shap.cor_importance2 <- plot_data.shap.cor_importance %>% 
    mutate(life_history_cover_pc = factor(life_history_cover_pc, levels = c(
      'Total coral',
      'Competitive',
      'Stress tolerant',
      'Weedy' #,
      #'generalist'
    ))) %>% 
    mutate(variable_group = str_replace(variable_group, "Connectivity", "")) %>% 
    mutate(feature_new = str_replace(feature_new, "Connectivity self_recruitment", "Connectivity self-recruitment")) 
  
  
  plot_data.shap.cor_importance2.total_cover_order <- plot_data.shap.cor_importance2 %>% 
    filter(life_history_cover_pc == "Total coral") %>% 
    select(feature,
           shap_abs_all_mean.01.total_cover = shap_abs_all_mean.01) %>% 
    mutate(shap_abs_all_mean.01.total_cover = ifelse(is.na(shap_abs_all_mean.01.total_cover), 0 , shap_abs_all_mean.01.total_cover))
    
  
  plot_data.shap.cor_importance2 <- plot_data.shap.cor_importance2 %>% 
    left_join(plot_data.shap.cor_importance2.total_cover_order)
  
  plot_data.shap.cor_importance2$variable_group2 <- as_factor(plot_data.shap.cor_importance2$variable_group)
 
  plot_data.shap.cor_importance2$variable_group2 <- fct_recode(plot_data.shap.cor_importance2$variable_group2,
                                                               Temperature = "Thermal",
                                                               `Physical oceanography` = "Physical oceanography",
                                                               `Ocean chemistry` = "Ocean chemistry",
                                                               `Light` = "Light",
                                                               `Cyclones` = "Cyclones",
                                                               ` ` = "",
                                                               `Human pressure` = "Local pressure")
  
  
  plot_data.shap.cor_importance2$variable_group2 <- factor(plot_data.shap.cor_importance2$variable_group2,
                                                           levels = c("Temperature",
                                                                      "Physical oceanography",
                                                                      "Ocean chemistry",
                                                                      "Light",
                                                                      "Cyclones",
                                                                      " ",
                                                                      "Human pressure"),
                                                           labels = c("Temperature",
                                                                      "Physical\noceanography",
                                                                      "Ocean\nchemistry",
                                                                      "Light",
                                                                      "Cyclones",
                                                                      " ",
                                                                      "Human\npressure"))
  
  
  
  
  global_labeller <- labeller(
    variable_group = label_wrap_gen(multi_line = T, width = 10)
  )
  
  
  shap_plot <- ggplot(data = plot_data.shap.cor_importance2 %>% 
                        
                        # mutate(variable_group = str_replace(feature_new, "Connectivity self_recruitment", 'Connectivity self recruitment')) %>% 
                        # 
                        # mutate(variable_group = str_replace(variable_group, "Physcial oceanography", 'Physical\noceanography')) %>% 
                        # mutate(variable_group = str_replace(variable_group, "Connectivity", '')) %>% 
                        filter(!str_detect(feature,
                                           paste0(features.one_hot.drop,collapse = '|'))),
                      aes(x = mean_shap_cor, 
                          xmin = mean_shap_cor - sd_shap_cor,
                          xmax = mean_shap_cor + sd_shap_cor,
                          y = reorder_within(feature_new, shap_abs_all_mean.01, variable_group2),
                          alpha = shap_abs_all_mean.01,
                          colour = life_history_cover_pc,
                          size = n_times_in_model
                      )) +
    
    # geom_vline(aes(xintercept = 0,  colour = get(subset_type.clean_x)), linewidth = 8, alpha = 0.2) +
    
    geom_vline(xintercept = 0, linewidth = 1, colour = 'white', alpha = 0.4) +
    geom_vline(xintercept = 0, linewidth = 0.5, colour = 'black', linetype = 'dashed', alpha = 0.4) +
    
    
    geom_errorbarh(linewidth = 0.5, width = 0.5, aes(colour = life_history_cover_pc)) +
    
    geom_point(shape = 19, colour = "white", alpha = 1) +
    geom_point(shape = 19) +
    
    facet_grid(variable_group2~get(subset_type.clean_x),
               scales = 'free',
               space = 'free_y',
               labeller = global_labeller) +
    
    # scale_fill_viridis_d() +
    # scale_fill_gradient(low = "#FFFFFF", high = "#009955")  +
    # scale_x_continuous(expand = c(0, 0)) + 
    scale_size_continuous(range = c(1,4)) +
    scale_fill_manual(values = colours) +
    
    scale_colour_manual(values = colours) +
    
    scale_alpha_continuous(range = c(0.33, 1)) +
    
    scale_y_reordered() +
    
    
    # coord_flip() +
    
    guides(
      colour = guide_legend(
        nrow   = 2,
        byrow  = FALSE,
        theme = theme(legend.title = element_text(hjust = 0.5)),
        title.position = "top",
        override.aes = list(linetype = 0, size = 5)
      ),
      
      size = guide_legend(
        # legend.text.position = "center",
        nrow   = 2,
        byrow  = FALSE,
        theme = theme(legend.title = element_text(hjust = 0.5)),
        title.position = "top",
        override.aes = list(linetype = 0)
      ),
      
      alpha  = guide_legend(
        nrow   = 2,
        byrow  = FALSE,
        theme = theme(legend.title = element_text(hjust = 0.5)),
        title.position = "top",
        override.aes = list(linetype = 0,
                            size = 5, 
                            fill = "grey10",
                            colour = "black",
                            shape = 19) #,
        # barheight = unit(35, "pt"),
        # barwidth  = unit(6, "pt"),
        ),
        
      
      linewidth = "none",
      linetype = 'none',
      colour = 'none'
      
    ) +
    
    labs(y = 'Variable',
         x = 'Directionality\n(Mean SHAP correlation +/- SD)',
         colour = "Response variable\n(Total & Life History)",
         alpha = 'Feature importance\n(Scaled mean absolute SHAP)',
         size = 'Number of models\nfeature is in') +
    
    theme_bw(base_size = 16) +
    theme(# axis.text.x = element_blank(),
      # axis.ticks.x = element_blank(),
      axis.text.y.left= element_text(face = 'bold'),
      
      # axis.text.y = element_blank(),
      # axis.ticks.y = element_blank(),
      
      # strip.text.y = element_markdown(
      #   angle = -90,
      #   hjust = 0.5,
      #   vjust = 0.5,
      # ),
      
      
      strip.text.x.top = element_markdown(
        angle = 0,
        hjust = 0.5,       
        vjust = 0,         
        face  = "bold"
      ),
      
      strip.background = element_blank(),
      # panel.spacing.y = unit(1, "lines"),
      # panel.spacing.x = unit(1, "lines"),
      
      panel.grid.major.x = element_blank(),
      
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      
      panel.grid = element_blank()) +
    
    theme(panel.grid.major.y = element_line(colour = "#EEEEEE"))

  shap_plot
  
  
  
  ggsave(filename = out_name_x,
         plot = shap_plot,
         width = 1600,
         height = 1.4142*1400,
         scale = 2.5,
         units = 'px')
  
  
  ggsave(filename = "bloomberg_figures/erl_revision/bloomberg_figure_2_shap_drivers_direction_and_magnitude.png",
         plot = shap_plot,
         width = 1600,
         height = 1.4142*1400,
         scale = 2.5,
         units = 'px')
  
  ggsave(filename = "bloomberg_figures/erl_revision/bloomberg_figure_2_shap_drivers_direction_and_magnitude.pdf",
         plot = shap_plot,
         width = 1600,
         height = 1.4142*1400,
         scale = 2.5,
         units = 'px')
  
  
  
  
  # ..... Mega SHAP partial plot ====
  cat(crayon::magenta("..... SHAP Partial plots single plot\n"))
  
  out_name_x <- paste0(out_dir_x,subset_type.clean_x,"_shap_partials_megaplot.png")
  
  plot_data.shap <- smart.lapply(1:length(analysis_subsets_x),
                                 function(x) {
                                   
                                   subset_x <- analysis_subsets_x[x]
                                   shaps_x <- shap_files_x[x]
                                   
                                   model_dir_x <- model_dirs_x[x]
                                   model_list.full <- list.files(model_dir_x, full.names = T)
                                   
                                   cat(crayon::green("............... Aggregating SHAP correlations for",subset_x))
                                   
                                   xgb_models <- lapply(model_list.full, read_rds)
                                   
                                   features_all_models <- lapply(1:length(xgb_models), function(y) {xgb_models[[y]]$xgb_data_list$features}) %>%
                                     unlist() %>%
                                     unique() %>% 
                                     sort()
                                   
                                   # out_name.shaps_y <- paste0(out_dir.shaps,'shap_values_',response_var_x,'__',variable_x,'.png')
                                   
                                   # if(is.na(features.one_hot[1])) {
                                   #   cat_bool_y <- F
                                   # } else {
                                   #   cat_bool_y <- ifelse(str_detect(variable_x, features.one_hot) %>% sum() >= 1, T, F) 
                                   # }
                                   
                                   data_x <- get_shap_correlation_data(xgb_model_objs = xgb_models, 
                                                                       variables = features_all_models,
                                                                       type = 'train', 
                                                                       id_vars = id_vars,
                                                                       output = 'raw')
                                   
                                   data_x <- data_x %>% 
                                     mutate(!!subset_type.clean_x := subset_x)
                                   
                                 }) %>% bind_rows() %>% time_it(T)
  
  
  plot_data.shap <- plot_data.shap %>% 
    ungroup() %>% 
    data_update_function()
  
  plot_data.shap <- plot_data.shap %>% 
    left_join(data.variable_table %>% rename(feature = variable)) %>% 
    mutate(feature_new = updated_variable) %>% 
    mutate(feature_new = ifelse(!is.na(units), 
                                paste0(feature_new, "\n(", units, ")"),
                                feature_new)) %>% 
    mutate(feature_new = ifelse(is.na(feature_new), 
                                feature, 
                                feature_new))
  
  plot_data.shap <- plot_data.shap %>% 
    group_by(across(any_of(c(id_vars, subset_type.clean_x, "feature", 'feature_new', 'variable_group')))) %>% 
    summarise(shap_value.mean = mean(SHAP_value),
              observation_value.mean = mean(observation_value),
              n_times_in_model = n()) %>% 
    ungroup()
  
  feature_numeric_summaries <- plot_data.shap %>%
    group_by(across(all_of(c('feature_new')))) %>%
    summarise(feat.min = min(observation_value.mean, na.rm = T) %>% signif(2),
              feat.max = max(observation_value.mean, na.rm = T) %>% signif(2),
              feat.mean = mean(observation_value.mean, na.rm = T) %>% signif(2),
              feat.sd = sd(observation_value.mean, na.rm = T) %>% signif(2)) %>%
    ungroup() %>%
    mutate(feature_new_with_summaries = paste0(feature_new,
                                               "<br><span style='color:#AAAAAA;'>"
                                               ,' [ ',feat.min,' : ',feat.mean,' : ',feat.max,' ]',
                                               "</span>"))
  
  plot_data.shap <- plot_data.shap %>%
    left_join(feature_numeric_summaries)
  
  levels(plot_data.shap[[subset_type.clean_x]]) <- to_sentence_case(levels(plot_data.shap[[subset_type.clean_x]]))
  
  plot_data.shap[['variable_group']] <- to_sentence_case(plot_data.shap[['variable_group']])
  
  
  
  
  plot_data.shap.2 <- plot_data.shap #%>% 
    # left_join(plot_data.shap.cor_importance2.total_cover_order)
  # 
  # plot_data.shap.2$variable_group2 <- as_factor(plot_data.shap.2$variable_group)
  # 
  # plot_data.shap.2$variable_group2 <- fct_recode(plot_data.shap.2$variable_group2,
  #                                                Temperature = "Thermal",
  #                                                `Physical oceanography` = "Physical oceanography",
  #                                                `Ocean chemistry` = "Ocean chemistry",
  #                                                `Light` = "Light",
  #                                                `Cyclones` = "Cyclones",
  #                                                ` ` = "Connectivity",
  #                                                `Human pressure` = "Local pressure")
  # 
  # 
  # plot_data.shap.2$variable_group2 <- factor(plot_data.shap.2$variable_group2,
  #                                                          levels = c("Temperature",
  #                                                                     "Physical oceanography",
  #                                                                     "Ocean chemistry",
  #                                                                     "Light",
  #                                                                     "Cyclones",
  #                                                                     " ",
  #                                                                     "Human pressure"))
  
  
  
  
  plot_data.shap.2$feature_new_with_summaries <- factor(plot_data.shap.2$feature,
                                                        levels = c("delta_sst_sd_6_year_mean",
                                                                   "delta_sst_q90_6_year_mean",
                                                                   "delta_sst_skewness_6_year_mean",
                                                                   "sst_climatology_noaa",
                                                                   "delta_sst_kurtosis_6_year_mean",
                                                                   "dhw_max_6_year_max",
                                                                   "dhw_max_6_year_mean",
                                                                   
                                                                   "min_distance_from_500m_depth_gebco",
                                                                   "depth_gebco",
                                                                   
                                                                   "p04_to_n03_mean_max_depth_2020_bio_oracle_3",
                                                                   "calcite_mean_surface_bio_oracle_2.2",
                                                                   
                                                                   "par_mean_bio_oracle_2",
                                                                   "diffuse_attenuation_mean_bio_oracle_2",
                                                                   "light_at_bottom_mean_bio_oracle_2",
                                                                   "light_at_bottom_range_bio_oracle_2",
                                                                   
                                                                   "cyclone_max_annual_wind_6_year_max",
                                                                   "cyclone_max_annual_wind_34_year_mean",
                                                                   "cyclone_count_6_year_sum",
                                                                   "cyclone_sever_count_6_year_sum",
                                                                   
                                                                   "connectivity.self_recruitment.idw",          
                                                                   
                                                                   "human_gravity_total",
                                                                   "min_distance_from_land_gebco",
                                                                   "andrello_sediments",
                                                                   
                                                                   "coral_province_Africa-India",
                                                                   "coral_province_Andaman-Nicobar Islands",
                                                                   "coral_province_Atlantic Caribbean",
                                                                   "coral_province_Australian",
                                                                   "coral_province_Brazil",
                                                                   "coral_province_Fiji-Caroline Islands",
                                                                   "coral_province_Hawaii-Line Islands",
                                                                   "coral_province_Indonesian",
                                                                   "coral_province_Japan-Vietnam",
                                                                   "coral_province_Pacific Caribbean",
                                                                   "coral_province_Persian Gulf",
                                                                   "coral_province_Polynesia",
                                                                   "coral_province_Red Sea",
                                                                   "coral_province_Tonga-Samoa"
                                                                   ),
                                                        
                                                        labels = c(
                                                          "SST kurtosis 6 year mean<br><span style='color:#AAAAAA;'> [ -1.8 : -1.5 : -0.56 ]</span>",                        
                                                          "SST 0.9 quantile 6 year mean\n(°C)<br><span style='color:#AAAAAA;'> [ 23 : 29 : 33 ]</span>",                     
                                                          "SST standard deviation 6 year mean\n(°C)<br><span style='color:#AAAAAA;'> [ 0.27 : 1.3 : 5.4 ]</span>",           
                                                          "SST skewness 6 year mean<br><span style='color:#AAAAAA;'> [ -0.81 : -0.13 : 0.92 ]</span>",                       
                                                          "SST maximuim monthly mean\n(°C)<br><span style='color:#AAAAAA;'> [ 23 : 29 : 34 ]</span>",
                                                          "Degree heating weeks 6 year max\n(°C-weeks)<br><span style='color:#AAAAAA;'> [ 0 : 4.2 : 30 ]</span>",            
                                                          "Degree heating weeks 6 year mean\n(°C-weeks)<br><span style='color:#AAAAAA;'> [ 0 : 1.5 : 13 ]</span>",           
                                                          
                                                          "Minimum distance from 500m depth\n(m)<br><span style='color:#AAAAAA;'> [ 0 : 43000 : 930000 ]</span>",            
                                                          "Depth\n(m)<br><span style='color:#AAAAAA;'> [ -2900 : -25 : 480 ]</span>",                                        
                                                          
                                                          "PO4 to NO3 ratio at depth long term mean<br><span style='color:#AAAAAA;'> [ -7.5 : -1.1 : 9.5 ]</span>",          
                                                          "Calcite at surface long term mean\n(mol/m³)<br><span style='color:#AAAAAA;'> [ 5.1e-05 : 0.0035 : 0.056 ]</span>",
                                                          
                                                          "PAR long term mean\n(E·m⁻²·day⁻¹)<br><span style='color:#AAAAAA;'> [ 29 : 44 : 51 ]</span>",                      
                                                          "Light attenuation long term mean\n(m⁻¹)<br><span style='color:#AAAAAA;'> [ 0.019 : 0.071 : 0.4 ]</span>",         
                                                          "Light at bottom long term mean\n(E·m⁻²·day⁻¹)<br><span style='color:#AAAAAA;'> [ 0 : 36 : 49 ]</span>",           
                                                          "Light at bottom long term range\n(E·m⁻²·day⁻¹)<br><span style='color:#AAAAAA;'> [ 0 : 23 : 49 ]</span>",          
                                                          
                                                          "Cyclone count 6 year sum<br><span style='color:#AAAAAA;'> [ 0 : 2.1 : 6 ]</span>",                                
                                                          "Cyclone maximum wind 6 year max\n(Kph)<br><span style='color:#AAAAAA;'> [ 0 : 54 : 150 ]</span>",                 
                                                          "Cyclone maximum wind 34 year mean\n(Kph)<br><span style='color:#AAAAAA;'> [ 0 : 22 : 83 ]</span>",                
                                                          "Severe cyclone count 6 year sum<br><span style='color:#AAAAAA;'> [ 0 : 0.83 : 6 ]</span>",                        
                                                          
                                                          "Connectivity self-recruitment<br><span style='color:#AAAAAA;'> [ 0 : 17 : 170 ]</span>",                          
                                                          
                                                          "Total human gravity<br><span style='color:#AAAAAA;'> [ 0 : 540 : 42000 ]</span>",                                 
                                                          "Minimum distance from land\n(m)<br><span style='color:#AAAAAA;'> [ 0 : 4200 : 290000 ]</span>",                   
                                                          "Sedimentation long term\n(tons/km²)<br><span style='color:#AAAAAA;'> [ 0 : 550 : 120000 ]</span>",            
                                                          
                                                          "coral_province_Africa-India<br><span style='color:#AAAAAA;'> [ 0 : 0.15 : 1 ]</span>",                            
                                                          "coral_province_Andaman-Nicobar Islands<br><span style='color:#AAAAAA;'> [ 0 : 0.016 : 1 ]</span>",                
                                                          "coral_province_Atlantic Caribbean<br><span style='color:#AAAAAA;'> [ 0 : 0.18 : 1 ]</span>",                      
                                                          "coral_province_Australian<br><span style='color:#AAAAAA;'> [ 0 : 0.15 : 1 ]</span>",                              
                                                          "coral_province_Brazil<br><span style='color:#AAAAAA;'> [ 0 : 0.0056 : 1 ]</span>",                                
                                                          "coral_province_Fiji-Caroline Islands<br><span style='color:#AAAAAA;'> [ 0 : 0.09 : 1 ]</span>",                   
                                                          "coral_province_Hawaii-Line Islands<br><span style='color:#AAAAAA;'> [ 0 : 0.062 : 1 ]</span>",                    
                                                          "coral_province_Indonesian<br><span style='color:#AAAAAA;'> [ 0 : 0.2 : 1 ]</span>",                               
                                                          "coral_province_Japan-Vietnam<br><span style='color:#AAAAAA;'> [ 0 : 0.059 : 1 ]</span>",                          
                                                          "coral_province_Pacific Caribbean<br><span style='color:#AAAAAA;'> [ 0 : 0.0028 : 1 ]</span>",                     
                                                          "coral_province_Persian Gulf<br><span style='color:#AAAAAA;'> [ 0 : 0.012 : 1 ]</span>",                           
                                                          "coral_province_Polynesia<br><span style='color:#AAAAAA;'> [ 0 : 0.035 : 1 ]</span>",                              
                                                          "coral_province_Red Sea<br><span style='color:#AAAAAA;'> [ 0 : 0.017 : 1 ]</span>",                                
                                                          "coral_province_Tonga-Samoa<br><span style='color:#AAAAAA;'> [ 0 : 0.021 : 1 ]</span>"
                                                          )               
                                                        )
  
  
  shap_partials_plot <- ggplot(data = plot_data.shap.2 %>% 
                                 filter(!str_detect(feature_new_with_summaries,
                                                    paste0(features.one_hot.drop,
                                                           collapse = '|'))), #%>% 
                               # filter(abs(observation_value.mean - feat.mean) <= 3 * feat.sd),
                               aes(x = observation_value.mean,
                                   y = shap_value.mean,
                                   colour = get(subset_type.clean_x),
                                   fill = get(subset_type.clean_x))) +
    
    geom_point(alpha = 0.1, shape = 20, size = 1) +

    geom_smooth(method = smooth_method, formula = smooth_formula, linewidth = 1.5, alpha = 0.2, colour = 'white') +
    geom_smooth(method = smooth_method, formula = smooth_formula, linewidth = 1, alpha = 0.2, se = F) +
    
    # geom_smooth(method = 'lm', linewidth = 1, alpha = 0.2) +
    
    
    # geom_hline(yintercept = 0.1, linetype = 'solid', colour = '#AAAAAA') +
    # geom_hline(yintercept = -0.1, linetype = 'solid', colour = '#AAAAAA') +
    
    geom_hline(yintercept = 0, linetype = 'solid', linewidth = 1.5, alpha = 0.4, colour = 'white') +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    
    # geom_vline(xintercept = 0, linetype = 'solid', linewidth = 1.5, alpha = 0.4, colour = 'white') +
    # geom_vline(xintercept = 0, linetype = 'dashed') +
    
    geom_vline(aes(xintercept = feat.mean), linetype = 'solid', linewidth = 1.5, alpha = 0.4, colour = '#999999') +
    geom_vline(aes(xintercept = feat.mean), linetype = 'dotted', colour = 'white') +
    
    # facet_grid(feature_new_with_summaries ~get(subset_type.clean_x),
    #             scales = 'free',
    #             switch = "y",
    #             shrink = T) +
    
    facet_grid2(feature_new_with_summaries ~ get(subset_type.clean_x),
                scales      = "free",
                switch      = "y",
                independent = "all",
                strip = strip_nested(by_layer_y = T,
                                     by_layer_x = F,
                                     size = 'variable'
                ),
                shrink = T,

                labeller   = labeller(variable_group = label_blank,
                                      feature_new_with_summaries = label_value,
                                      !!subset_type.clean_x := label_both)

    ) +
    
    scale_fill_manual(values = colours) +
    scale_colour_manual(values = colours) +
    
    labs(y = paste0('Effect on response output'),
         x = paste0('Feature'),
         colour = NULL,
         fill = NULL) +
    
    guides(colour = 'none',
           fill = 'none') +
    
    theme_article(base_size = 12) +
    
    theme(panel.grid = element_blank(),
          plot.background = element_rect(fill = 'white'),
          
          axis.text = element_blank(),
          axis.ticks = element_blank(), 
          
          strip.placement = "outside",
          strip.text.y.left = element_markdown(
            angle = 0,
            hjust = 0,
            vjust = 0.5,
            face  = "bold"
          ),
          
          
          strip.text.x.top = element_markdown(
            angle = 0,
            hjust = 0.5,       
            vjust = 0,         
            face  = "bold"
          ),
          
          
          strip.background = element_blank(),
          panel.spacing.y = unit(0, "lines"),
          panel.spacing.x = unit(0, "lines"),
          
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.box = "horizontal")
  
  shap_partials_plot
  
  
  ggsave(filename = out_name_x,
         plot = shap_partials_plot,
         width = 1200,
         height = 1.4142*1200,
         scale = 2.5,
         units = 'px')
  
  
  ggsave(filename = paste0(bloomberg_out_dir,'bloomberg_figure_S4_conditional_partial_plots.png'),
         plot = shap_partials_plot,
         width = 1200,
         height = 1.4142*1200,
         scale = 2.5,
         units = 'px')
  
  
  
  
  
  
  
  ## ..... Model performance plot ====
  cat(crayon::green(paste0('generating Model performance summary\n')))
  
  out_name_x <- paste0(out_dir_x,subset_type.clean_x,"_model_performance_plot.png")
  
  if(file.exists(out_name_x) && overwrite.plots == F) {
    
    cat(crayon::yellow("...file",out_name_x,"already exists. Skipping\n"))
    
    
  } else {
    
    plot_data.model_performance <- lapply(1:length(analysis_subsets_x), function(x) {
      
      subset_x <- analysis_subsets_x[x]
      shaps_x <- shap_files_x[x]
      
      model_dir_x <- model_dirs_x[x]
      model_list.full <- list.files(model_dir_x, full.names = T)
      
      
      prediction_summary_x <- list.files(path = paste0(in_dir,subset_x),
                                         pattern = 'model_predictions_raw.csv',
                                         recursive = T,
                                         full.names = T) %>% 
        read_csv() %>%
        mutate(!!subset_type.clean_x := subset_x) %>% 
        
        select(any_of(c(subset_type.clean_x,
                        "coral_province",
                        paste0(subset_x,".predicted"),
                        paste0(subset_x,".actual"),
                        "rep_unit_var",
                        "ecoregion",
                        "pu_lon",
                        "pu_lat",
                        "time",
                        "depth_m.observed",
                        "model_obs_id",
                        "naive_prediction",
                        "error.model",
                        "error.model.abs",
                        "error.model.pc",
                        "error.model.abs_pc",
                        "error.model.sq",
                        "error.naive_prediction",
                        "error.naive_prediction.abs",
                        "error.naive.sq",
                        "run",
                        "model_absolute_error_uplift",
                        "life_history_cover_pc"))) %>% 
        
        # Mutate depth_range using case when to create depth categories
        mutate(depth_range = case_when(
          depth_m.observed < 10 ~ "<10m",
          depth_m.observed >= 10 & depth_m.observed < 20 ~ "10-20m",
          depth_m.observed >= 20 & depth_m.observed < 30 ~ "20-30m",
          depth_m.observed >= 30 & depth_m.observed < 40 ~ "30-40m",
          depth_m.observed >= 40 ~ "40m+",
          TRUE ~ NA_character_
        )) %>% 
        
        # mutate time_range using case when for decades from 1960 to 2020
        mutate(time_range = case_when(
          time >= 1960 & time < 1970 ~ "1960s",
          time >= 1970 & time < 1980 ~ "1970s",
          time >= 1980 & time < 1990 ~ "1980s",
          time >= 1990 & time < 2000 ~ "1990s",
          time >= 2000 & time < 2010 ~ "2000s",
          time >= 2010 & time < 2020 ~ "2010s",
          time >= 2020 ~ "2020s",
          TRUE ~ NA_character_
        ))
        
        
      # Summarise three times by province, time, and depth, combine to list
      prediction_summary_stats <- smart.lapply(c('coral_province',
                                                 'time_range',
                                                 'depth_range'),
                                              function(y) {
                                                
                                                summary_out <- prediction_summary_x %>%
                                                  group_by(across(any_of(c(subset_type.clean_x, y, 'run')))) %>%
                                                  summarise(error.model.abs.mean = mean(error.model.abs, na.rm = T),
                                                            error.naive.abs.mean = mean(error.naive_prediction.abs, na.rm = T),
                                                            n = n()) %>%
                                                  ungroup()
                                                
                                                summary_out
                                                
                                                })
      
      
      
      }) %>% bind_rows()
    
    
    plot_data.model_performance <- plot_data.model_performance %>%
      rename(model_errors = error.model_abs.mean,
             naive_errors = error.naive.abs.mean) %>%
      mutate(model_error_reduction = naive_errors - model_errors) %>% 
      pivot_longer(cols = c('model_errors',
                            'naive_errors',
                            'model_error_reduction'),
                   ) %>% 
      mutate(name = factor(name, levels = c('model_errors',  
                                            'naive_errors',
                                            'model_error_reduction')))
    
    plot_data.model_performance <- plot_data.model_performance %>% 
      ungroup() %>% 
      data_update_function()
    
    levels(plot_data.model_performance[[subset_type.clean_x]]) <- to_sentence_case(levels(plot_data.model_performance[[subset_type.clean_x]]))
    levels(plot_data.model_performance[['name']]) <- to_sentence_case(levels(plot_data.model_performance[['name']]))
    
    plot_data.model_performance$coral_province %>% unique()
    
    plot_data.model_performance <- plot_data.model_performance %>% 
      mutate(coral_province = factor(coral_province, levels = c(
        "Atlantic Caribbean",
        "Hawaii-Line Islands",
        "Pacific Caribbean",
        "Red Sea",
        "Australian",
        "Japan-Vietnam",
        "Persian Gulf",
        "Polynesia",
        "Andaman-Nicobar Islands",
        "Indonesian",
        "Africa-India",
        "Brazil",
        "Fiji-Caroline Islands",
        "Tonga-Samoa",
        "West Africa"
      )))
      
    performance_plot <- ggplot(data = plot_data.model_performance,
                               aes(x = name, 
                                   y = value,
                                   colour = get(subset_type.clean_x))) +
      
      geom_hline(yintercept = 0, linetype = 'solid', linewidth = 1.5, alpha = 0.4, colour = 'white') +
      geom_hline(yintercept = 0, linetype = 'dashed') +
      
      geom_boxplot(size = 0.5, show.legend = T) +
      # geom_beeswarm(size = 3, alpha = 0.2, method = 'hex',
      #               show.legend = T) +
      
      facet_grid2(coral_province ~ get(subset_type.clean_x),
                  scales      = "free_y",
                  switch      = "y",
                  # independent = "y",
                  strip = strip_nested(by_layer_y = T,
                                       by_layer_x = F,
                                       size = 'variable'
                  ),
                  shrink = T,
                  
                  labeller   = labeller(variable_group = label_blank,
                                        feature_new_with_summaries = label_value,
                                        !!subset_type.clean_x := label_both)
                  
      ) +
      
      scale_fill_manual(values = colours) +
      scale_colour_manual(values = colours) +
      
      scale_y_continuous(breaks = c(-0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5),
                         labels = c('-20%', '-10%', '0%', '10%', '20%', '30%', '40%', '50%')) +
      
      labs(y = paste0(''),
           x = paste0('Model type'),
           colour = NULL,
           fill = NULL) +
      
      guides(colour = 'none',
             fill = 'none') +
      
      theme_article(base_size = 12) +
      
      theme(panel.grid = element_blank(),
            plot.background = element_rect(fill = 'white'),
            
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            # axis.ticks.x = element_blank(), 
            
            strip.placement = "outside",
            strip.text.y.left = element_markdown(
              angle = 0,
              hjust = 0,
              vjust = 0.5,
              face  = "bold"
            ),
            
            
            strip.text.x.top = element_markdown(
              angle = 0,
              hjust = 0.5,       
              vjust = 0,         
              face  = "bold"
            ),
            
            
            strip.background = element_blank(),
            # panel.spacing.y = unit(0, "lines"),
            # panel.spacing.x = unit(0, "lines"),
            
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.box = "horizontal")
    
    performance_plot
    
    ggsave(filename = out_name_x,
           plot = performance_plot,
           width = 1200,
           height = 1.4142*1200,
           scale = 2.5,
           units = 'px')
    
  }
  
  
  # ...... Predicted vs actuals plot ====
  cat(crayon::green(paste0('generating actuals vs predicted plots\n')))
  
  out_name_x <- paste0(out_dir_x,subset_type.clean_x,"_actuals_vs_predicted_plot.png")
  
  if(file.exists(out_name_x) && overwrite.plots == F) {
    
    cat(crayon::yellow("...file",out_name_x,"already exists. Skipping\n"))
    
    
  } else {
    
    plot_data.model_performance <- lapply(1:length(analysis_subsets_x), function(x) {
      
      subset_x <- analysis_subsets_x[x]
      shaps_x <- shap_files_x[x]
      
      model_dir_x <- model_dirs_x[x]
      model_list.full <- list.files(model_dir_x, full.names = T)
      
      xgb_models <- lapply(model_list.full, read_rds)
      
      features_all_models <- lapply(1:length(xgb_models), function(y) {xgb_models[[y]]$xgb_data_list$features}) %>%
        unlist() %>%
        unique() %>% 
        sort()
      
      predict_var <- paste0(subset_x,'.predicted')
      actual_var <- paste0(subset_x,'.actual')
      
      
      predictions <- lapply(1:length(xgb_models), function(y) {xgb_models[[y]]$predictions}) %>%
        bind_rows() %>% 
        group_by(across(c('model_obs_id', 'coral_province'))) %>%
        summarise(predicted_sd = sd(get(predict_var)),
                  predicted = mean(get(predict_var)),
                  actual = mean(get(actual_var))) %>%
        mutate(x_min =predicted - predicted_sd,
               x_max = predicted + predicted_sd) %>%
        mutate(!!subset_type.clean_x := subset_x) %>% 
        ungroup()
      
    }) %>% bind_rows()
    
    
    axis_ranges <- c(plot_data.model_performance[['predicted']],
                     plot_data.model_performance[['actual']]) %>%
      range()
    
    plot_data.model_performance <- plot_data.model_performance %>% 
      ungroup() %>% 
      data_update_function()
    
    levels(plot_data.model_performance[[subset_type.clean_x]]) <- to_sentence_case(levels(plot_data.model_performance[[subset_type.clean_x]]))
    
    plot_data.model_performance <- plot_data.model_performance %>% 
      mutate(coral_province = factor(coral_province, levels = c(
        "Indonesian",
        "Africa-India",
        "Hawaii-Line Islands",
        "Australian",
        "Fiji-Caroline Islands",
        "Polynesia",
        "Andaman-Nicobar Islands",
        "Japan-Vietnam",
        "Tonga-Samoa",  
        "Red Sea",
        "Persian Gulf",
        "Pacific Caribbean"
      )))
    
    
    plot <- ggplot(data = plot_data.model_performance,
                   aes(x = predicted, 
                       xmin = x_min,
                       xmax = x_max,
                       y = actual,
                       colour = get(subset_type.clean_x))) +
      
      geom_errorbarh(alpha = 0.2) +
      geom_point(alpha = 0.3, shape = 19) +
      # geom_hex(binwidth = bin_width) +
      
      geom_abline(slope = 1, intercept = 0, 
                  linewidth = 1.5, 
                  colour = 'white',
                  linetype = 'solid') +
      
      
      geom_abline(slope = 1, intercept = 0, 
                  linewidth = 1, 
                  colour = 'darkgrey',
                  linetype = 'dashed') +
      
      geom_smooth(method = 'lm', linewidth = 1.5, colour = 'white') +
      geom_smooth(method = 'lm', linewidth = 1) +
      
      
      
      facet_grid2(coral_province ~ get(subset_type.clean_x),
                  # scales      = "free_y",
                  switch      = "y",
                  # independent = "y",
                  strip = strip_nested(by_layer_y = T,
                                       by_layer_x = F,
                                       size = 'variable'
                  ),
                  shrink = T,
                  
                  labeller   = labeller(variable_group = label_blank,
                                        feature_new_with_summaries = label_value,
                                        !!subset_type.clean_x := label_both)
                  
      ) +
      
      scale_fill_manual(values = colours) +
      scale_colour_manual(values = colours) +
      
      scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                         labels = c("0%", '25%', '50%', '75%', '100%')) +
      
      scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                         labels = c("0%", '25%', '50%', '75%', '100%')) +
      
      
      
      
      labs(y = paste0('Observed cover'),
           x = paste0('Precicted cover'),
           colour = NULL,
           fill = NULL) +
      
      guides(colour = 'none',
             fill = 'none') +
      
      theme_article(base_size = 12) +
      
      theme(panel.grid = element_blank(),
            plot.background = element_rect(fill = 'white'),
            
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            # axis.ticks.x = element_blank(), 
            
            strip.placement = "outside",
            strip.text.y.left = element_text(
              angle = 0,
              hjust = 0,
              vjust = 0.5,
              face  = "bold"
            ),
            
            
            strip.text.x.top = element_text(
              angle = 0,
              hjust = 0.5,       
              vjust = 0,         
              face  = "bold"
            ),
            
            
            strip.background = element_blank(),
            # panel.spacing.y = unit(0, "lines"),
            panel.spacing.x = unit(1, "lines"),
            
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.box = "horizontal") +
      
      coord_fixed(xlim = axis_ranges,
                  ylim = axis_ranges)
  
  plot
  
  ggsave(filename = out_name_x,
         plot = plot,
         width = 1200,
         height = 1.4142*1200,
         scale = 2.5,
         units = 'px')
    
  }
  
  
})
  

smart.lapply(1:length(analysis_subsets_x), cores = 1, function(y) {
  
  print(y)
  
  subset_x <- analysis_subsets_x[[y]]
  shaps_x <- shap_files_x[y]
  
  model_dir_x <- model_dirs_x[y]
  model_list.full <- list.files(model_dir_x, full.names = T)
  
  xgb_models <- lapply(model_list.full, read_rds)
  
  out_dir_x <- paste0("Z:/Dropbox/Global Coral Refugia Life Histories/bloomberg_coral_sanctuaries_2024/results/life_history_models_first_pass/xgboost_models/",
                      subset_x,'/')
  
  if(!dir.exists(out_dir_x)) { dir.create(out_dir_x, recursive = T)}
  
  lapply(1:length(xgb_models), function(x) {
    
    data_x <- xgb_models[[x]]$xgb_data_list
    
    write_rds(data_x,
              file = paste0(out_dir_x,"xgboost_model_data_",x,'.rds'))
    
  })
  
})

plot(x = seq(from = 0, to= 1, length.out = 100),
     y = sqrt(seq(from = 0, to= 1, length.out = 100)))




data <- read_csv("C:/Users/MQ43846173/Downloads/Coral_cover_data (1)/Coral_cover_data.csv")


data.summary <- data %>% 
  select(-Taxon, -Percent_cover) %>%
  distinct()
