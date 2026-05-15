# Bloomberg paper figure SX.
# ..... SSP370 Vs 585


rm(list=ls())

# Setup ====

library(tidyverse)
library(ggrepel)
library(patchwork)

library(ggridges)

source("functions.R")

# Constants


in_file.selections.370 <- "Z:/Dropbox/Lenfest_kenya/data/PROJECT_SPECIFIC/bloomberg/life_history_predictions/October_analysis/all_selections_with_beyer_and_eez.rds"

in_file.selections.585 <- "Z:/Dropbox/Lenfest_kenya/data/PROJECT_SPECIFIC/bloomberg/life_history_predictions/erl_revision_2/all_selections_with_beyer_and_eez.rds"

out_dir <- "bloomberg_figures/erl_revision/"


data.370 <- read_rds(in_file.selections.370) %>% 
  st_drop_geometry() %>% 
  ungroup() %>% 
  mutate(cover.future.370 = (effective_cover_future_m2_total_cover/extent_area_present_m2_total_cover)*100) %>% 
  dplyr::select(grid_id, cover.future.370)
 



data.585 <- read_rds(in_file.selections.585) %>% 
  st_drop_geometry() %>% 
  ungroup() %>% 
  mutate(cover.future.585 = (effective_cover_future_m2_total_cover/extent_area_present_m2_total_cover)) %>% 
  dplyr::select(grid_id, cover.future.585, effective_cover_future_m2_total_cover, extent_area_present_m2_total_cover, grid_area_m2) 
  

data.585 <- read_rds(in_file.selections.585) %>% 
  st_drop_geometry() %>% 
  ungroup() %>% 
  mutate(cover.future.585 = (effective_cover_future_m2_total_cover/extent_area_present_m2_total_cover)*100) %>% 
  dplyr::select(grid_id, cover.future.585) %>% 
  
  filter(cover.future.585 < 50)



data.plot <- data.370 %>% 
  full_join(data.585) %>% 
  mutate(diff = cover.future.370 - cover.future.585)



plot <- ggplot(data = data.plot,
               aes(x = cover.future.585,
                   y = cover.future.370,
                   colour = diff)) +
  
  geom_point(alpha = 0.3, shape = 19) +
  
  geom_abline(slope = 1, intercept = 0, linewidth = 1, colour = '#222222') +
  
  geom_smooth(method = 'lm', colour = "#994400") +
  
  scale_color_gradient2(high = '#222299', mid = '#999999', low = '#992222') +
  
  coord_equal() +
  
  guides(colour = guide_colourbar(title.position = "top", title.hjust=0.5)) +
  
  labs(x = "Coral cover 2050 SSP585",
       y = "Coral cover 2050 SSP370",
       colour = "SSP370 - SSP585") +
  
  # legend along bottom
  theme(
    legend.position = "bottom",
    legend.box      = "horizontal",
    legend.title    = element_text(hjust = 0.5),
    legend.direction = "horizontal",
    legend.key.width = unit(dev.size()[1] / 4, "inches")
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(linewidth = 0.5, color = '#222222'),
        panel.grid = element_blank())



ggsave(plot = plot,
       filename = paste0(out_dir,'bloomberg_figure_sx_coral_cover_ssp370_vs_ssp585.png'),
       height = 1212,
       width = 1000,
       scale = 3,
       units = 'px')


ggsave(plot = plot,
       filename = paste0(out_dir,'bloomberg_figure_sx_coral_cover_ssp370_vs_ssp585.pdf'),
       height = 1212,
       width = 1000,
       scale = 3,
       units = 'px')


