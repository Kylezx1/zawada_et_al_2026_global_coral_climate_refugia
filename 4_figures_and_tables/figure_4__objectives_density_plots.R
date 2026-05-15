# Bloomberg paper figure 4.
# ..... Selected reef area by EEZ

rm(list=ls())

# Setup ====

library(tidyverse)
library(ggrepel)
library(patchwork)

library(ggridges)

source("functions.R")

# Constants


in_file.selections <- "Z:/Dropbox/Lenfest_kenya/data/PROJECT_SPECIFIC/bloomberg/life_history_predictions/erl_revision_2//all_selections_with_beyer_and_eez.rds"

in_file.eez_regions <- "Z:/offline_data/data/politics_and_governance/jurisdictions/economic/exclusive_economic_zones/table/coral_eezs_with_regions.csv"

out_dir <- "bloomberg_figures/erl_revision/ssp370/"


# Main ====

# ..... Load data ====


data.selections <- read_rds(in_file.selections) %>% 
  st_drop_geometry() %>% 
  
  ungroup() %>% 
  
  select(ecoregion = ECOREGION,
         selected,
         obj_temporal_z,
         obj_risk_z,
         obj_complementarity_z,
         obj_spatial_z)

# pivot by z scores
data.selections.long <- data.selections %>% 
  pivot_longer(cols = starts_with('obj_'),
               names_to = 'objective',
               values_to = 'z_score') %>% 
  mutate(objective = case_when(objective == 'obj_temporal_z' ~ 'Coral cover present and future',
                               objective == 'obj_risk_z' ~ 'Risk',
                               objective == 'obj_complementarity_z' ~ 'Life history diversity',
                               objective == 'obj_spatial_z' ~ 'Spatial cohesion',
                               TRUE ~ objective))


data.selections.long <- data.selections.long %>% 
  mutate(objective = factor(objective, 
                            levels = c("Risk",
                                       "Spatial cohesion",
                                       "Life history diversity",
                                       "Coral cover present and future"),
                            labels = c("4. Risk",
                                       "3. Spatial cohesion",
                                       "2. Life history diversity",
                                       "1. Coral cover present and future")))




global_labeller <- labeller(
  ecoregion = label_wrap_gen(multi_line = T, width = 20)
)


data.selections.long <- data.selections.long %>% 
  filter(ecoregion != "Agulhas")


plot <- ggplot(data = data.selections.long,
       aes(x = z_score, 
           y = objective, 
           fill = ifelse(selected == 'in_portfolio', "Selected","Not Selected"),
           colour = ifelse(selected == 'in_portfolio', "Selected","Not Selected"))) +
  
  ggridges::geom_density_ridges(alpha = 0.7, scale = 0.9, quantile_lines = TRUE, quantiles = c(0.25, 0.5, 0.75)) +
  
  facet_wrap(~ecoregion, ncol = 5, labeller = global_labeller) +
  
  scale_fill_manual(values = c("Selected"="#EEEE00","Not Selected"="#880000"), name = "Selection Status") +
  scale_colour_manual(values = c("Selected"="#888888","Not Selected"="#EEEEEE"), name = "Selection Status") +
  
  guides(colour = NULL) +
  
  coord_cartesian(xlim = c(-2.5, 2.5), expand = F) +
  
  labs(x = "Z-Score", y = "Objective") +
  
  theme_minimal(base_size = 13) +

  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(linewidth = 0.5, color = '#222222'),
        panel.grid = element_blank())


ggsave(plot = plot,
       filename = paste0(out_dir,'bloomberg_figure_4_objective_density_plots.png'),
       height = 1212,
       width = 1000,
       scale = 3,
       units = 'px')


ggsave(plot = plot,
       filename = paste0(out_dir,'bloomberg_figure_4_objective_density_plots.pdf'),
       height = 1212,
       width = 1000,
       scale = 3,
       units = 'px')
