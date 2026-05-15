# Bloomberg paper figure 4.
# ..... Selected reef area by EEZ

rm(list=ls())

# Setup ====

library(tidyverse)
library(ggrepel)
library(patchwork)

source("functions.R")

# Constants

in_file.selections <- "Z:/Dropbox/Lenfest_kenya/data/PROJECT_SPECIFIC/bloomberg/life_history_predictions/October_analysis/selection_summary_by_eez.csv" 

in_file.eez_regions <- "Z:/offline_data/data/politics_and_governance/jurisdictions/economic/exclusive_economic_zones/table/coral_eezs_with_regions.csv"

in_file.eez_provinces <- "Z:/offline_data/data/politics_and_governance/jurisdictions/economic/exclusive_economic_zones/table/coral_eezs_with_provinces.csv"

out_dir <- "bloomberg_figures/erl_revision/ssp370/"


# Main ====

# ..... Load data ====

data.selections_by_eez <- read_csv(in_file.selections) %>% 
  filter(total_reef_extent != 0) %>% 
  filter(total_reef_extent > 250) %>% 
  filter(!is.na(territory))

data.eez_regions <- read_csv(in_file.eez_regions) %>%
  rename(territory = EEZ, region = Region)



eez_order <- data.selections_by_eez %>% 
  arrange(total_reef_extent) %>% 
  select(territory) %>% 
  left_join(data.eez_regions)

region_columns <- eez_order %>% 
  group_by(region) %>% 
  summarise(n_eezs = n())



data.eez_provinces <- read_csv(in_file.eez_provinces) %>%
  rename(territory = EEZ, province = Province)



eez_order <- data.selections_by_eez %>% 
  arrange(total_reef_extent) %>% 
  select(territory) %>% 
  left_join(data.eez_provinces)

province_columns <- eez_order %>% 
  group_by(province) %>% 
  summarise(n_eezs = n())




province_columns$col <- NA_character_

sumA <- 0L
sumB <- 0L

for (i in seq_len(nrow(province_columns))) {
  if (sumA <= sumB) {
    province_columns$col[i] <- "A"
    sumA <- sumA + province_columns$n_eezs[i]
  } else {
    province_columns$col[i] <- "B"
    sumB <- sumB + province_columns$n_eezs[i]
  }
}




data.selections_by_eez.pivot <- data.selections_by_eez %>% 
  
  mutate(territory = factor(territory, levels = eez_order$territory)) %>% 
  
  select(-country) %>% 
  
  mutate(reef_extent_km2.50_plus_total = reef_extent_km2.50_plus_only + reef_extent_km2.both_methods) %>% 
  
  mutate(reef_extent_km2.non_50_plus = total_reef_extent - reef_extent_km2.50_plus_total) %>% 
  
  mutate(percent.50_plus_total = reef_extent_km2.50_plus_total / total_reef_extent * 100) %>% 
  
  pivot_longer(cols = -territory) %>% 
  
  left_join(data.eez_provinces) %>% 
  
  left_join(province_columns)


data.selections_by_eez.pivot.all <- data.selections_by_eez.pivot %>% 
  
  group_by(name) %>% 
  
  summarise(value = sum(value, na.rm = T))

# ..... Plots

df <- data.selections_by_eez.pivot %>%
  filter(name %in% c("reef_extent_km2.non_50_plus", "reef_extent_km2.50_plus_total", "percent.50_plus_total", "total_reef_extent")) %>%
  mutate(
    name = factor(name, c("reef_extent_km2.50_plus_total", "reef_extent_km2.non_50_plus", "percent.50_plus_total", "total_reef_extent")),
    lbl  = label_number(big.mark = ",", accuracy = 1)(value)
  ) %>% 
  
  mutate(territory = factor(territory, levels = eez_order$territory))


df <- df %>% filter(territory != 'Brazil')

df.out <- df %>% 
  select(province,territory, name, value) %>% 
  pivot_wider(id_cols = c("province","territory"),
              values_from = value,
              names_from = name)

write_csv(df.out, paste0(out_dir,"bloomberg_paper_table_s5_prioritisations_by_eez_ssp370.csv"))


df.small_sum.marker <- df %>% 
  filter(name == 'total_reef_extent') %>% 
  mutate(small_countries = ifelse(value < 250, 'small', 'large')) %>% 
  select(-name, -value, -n_eezs, -lbl)

df <- df %>% 
  left_join(df.small_sum.marker) %>% 
  mutate(territory2 = ifelse(small_countries == 'small', 'Small countries', as.character(territory))) %>% 
  
  group_by(territory2, province, name, col) %>% 
  
  summarise(value = sum(value)) %>% 
  
  mutate(
    name = factor(name, c("reef_extent_km2.50_plus_total", "reef_extent_km2.non_50_plus", "percent.50_plus_total", "total_reef_extent")),
    lbl  = label_number(big.mark = ",", accuracy = 1)(value)
  ) %>% 
  
  rename(territory = territory2) %>% 
  
  ungroup()



eez_order <- df %>% 
  filter(name == 'total_reef_extent') %>% 
  arrange(value) %>% 
  select(province, territory) %>% 
  distinct()



province_columns <- eez_order %>% 
  group_by(province) %>% 
  summarise(n_eezs = n())


province_columns$col <- NA_character_

province_columns <- province_columns %>% arrange(-n_eezs)

sumA <- 0L
sumB <- 0L

for (i in seq_len(nrow(province_columns))) {
  if (sumA <= sumB) {
    province_columns$col[i] <- "A"
    sumA <- sumA + province_columns$n_eezs[i]
  } else {
    province_columns$col[i] <- "B"
    sumB <- sumB + province_columns$n_eezs[i]
  }
}



df <- df %>%
  
  select(-col) %>% 
  
  left_join(province_columns)


df <- df %>% 
  mutate(territory = factor(territory, levels = eez_order$territory %>% unique()))







plot.theme <-  theme(panel.grid.major.y = element_blank(),
                     panel.grid.minor.y = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     
                     strip.background = element_rect(fill = '#EEEEEE'),
                     
                     axis.text.x = element_text(angle = -30, hjust = 0, vjust = 0.5, colour = '#999999'))

plot.left <- ggplot(
  
  data = df %>% filter(col == "A") %>% filter(value != 0) %>% filter(name %in% c("reef_extent_km2.non_50_plus", "reef_extent_km2.50_plus_total")),
  aes(y = territory, x = value, fill = name)) +
  
  # stacked bars (geom_col defaults to stack)
  geom_vline(xintercept = 0, colour = "#222222") +
  
  geom_col(show.legend = F, width = 0.9, colour = '#888888', linewidth = 0.5) +
  
  # labels centered within each stacked segment
  geom_text(data = df %>% filter(col == "A") %>% filter(name == "total_reef_extent") %>% 
              
              select(territory, province, value) %>% 
              
              left_join(df %>% filter(name == "reef_extent_km2.50_plus_total")  %>% select(province, territory, lbl)),
    aes(label = lbl,
        x = value,
        y = territory),
    # position = position_stack(vjust = 0.5),
    # x = Inf,
    hjust = -0.1,
    nudge_x = 1000,
    colour = "#009999",
    size = 3,
    lineheight = 0.9,
    inherit.aes = F
  ) +
  
  # labels centered within each stacked segment
  geom_text(data = df %>% filter(col == "A") %>% filter(name == "total_reef_extent") %>% 
              
              select(territory, province, value) %>% 
              
              left_join(df %>% filter(name == "percent.50_plus_total")  %>% select(province, territory, lbl)),
            aes(label = paste0('(',lbl,'%)'),
                x = value,
                y = territory),
            # position = position_stack(vjust = 0.5),
            # x = Inf,
            hjust = -1.3,
            nudge_x = 2000,
            colour = "#009999",
            size = 3,
            lineheight = 0.9,
            inherit.aes = F
  ) +
  
  geom_text(data = df %>% filter(col == "A") %>% filter(name == 'total_reef_extent'),
            aes(label = lbl),
            # position = position_stack(vjust = 0.5),
            x = -1000,
            hjust = 1.1,
            colour = "#888888",
            size = 3,
            lineheight = 0.9
  ) +
  
  
  facet_wrap(province~.,
             scales = "free_y",
             space = "free_y",

             # ncol = 2
             ) +
  
  scale_x_continuous(limits = c(-35000, 170000),
                     breaks = c(0, 10000, 50000)
                     ) +
  
  scale_fill_manual(
    values = c("#009999", "#FFFFFF"),
    limits = c("reef_extent_km2.50_plus_total", "reef_extent_km2.non_50_plus"),
    breaks = c("reef_extent_km2.50_plus_total", "reef_extent_km2.non_50_plus"),
    labels = c("Selected", "Not selected")
  ) +
  
  
  
  labs(y = "", x = "") +
  
  theme_bw(base_size = 14) +
  
  plot.theme
  
 
plot.left



plot.right <- ggplot(
  data = df %>% filter(col == "B") %>% filter(value != 0) %>% filter(name %in% c("reef_extent_km2.non_50_plus", "reef_extent_km2.50_plus_total")),
  aes(y = territory, x = value, fill = name)
) +
  # stacked bars (geom_col defaults to stack)
  geom_vline(xintercept = 0, colour = "#222222") +
  
  geom_col(show.legend = F, width = 0.9, colour = 'grey50', linewidth = 0.5) +
  
  
  # labels centered within each stacked segment
  geom_text(data = df %>% filter(col == "B") %>% filter(name == "total_reef_extent") %>% 
              
              select(territory, province, value) %>% 
              
              left_join(df %>% filter(name == "reef_extent_km2.50_plus_total")  %>% select(province, territory, lbl)),
            aes(label = lbl,
                x = value,
                y = territory),
            # position = position_stack(vjust = 0.5),
            # x = Inf,
            hjust = -0.1,
            nudge_x = 1000,
            colour = "#009999",
            size = 3,
            lineheight = 0.9,
            inherit.aes = F
  ) +
  
  # labels centered within each stacked segment
  geom_text(data = df %>% filter(col == "B") %>% filter(name == "total_reef_extent") %>% 
              
              select(territory, province, value) %>% 
              
              left_join(df %>% filter(name == "percent.50_plus_total")  %>% select(province, territory, lbl)),
            aes(label = paste0('(',lbl,'%)'),
                x = value,
                y = territory),
            # position = position_stack(vjust = 0.5),
            # x = Inf,
            hjust = -1.3,
            nudge_x = 2000,
            colour = "#009999",
            size = 3,
            lineheight = 0.9,
            inherit.aes = F
  ) +
  
  geom_text(data = df %>% filter(col == "B") %>% filter(name == 'total_reef_extent'),
            aes(label = ifelse(value > 0, lbl, NA_character_)),
            # position = position_stack(vjust = 0.5),
            x = -1000,
            hjust = 1.1,
            colour = "grey50",
            size = 3,
            lineheight = 0.9
  ) +
  
  
  facet_wrap(province~.,
             scales = "free_y",
             space = "free_y",
             
             # ncol = 2
  ) +
  
  scale_x_continuous(limits = c(-35000, 170000),
                     breaks = c(0, 10000, 50000)
                     ) +
  
  scale_y_discrete(position = 'right') +
  
  scale_fill_manual(
    values = c("#009999", "#FFFFFF"),
    limits = c("reef_extent_km2.50_plus_total", "reef_extent_km2.non_50_plus"),
    breaks = c("reef_extent_km2.50_plus_total", "reef_extent_km2.non_50_plus"),
    labels = c("Selected", "Not selected")
  ) +
  
  
  
  labs(y = "", x = "") +
  
  theme_bw(base_size = 14) +
  
  plot.theme

plot.right




plot.legend <- ggplot(
  data = df %>% filter(col == "B"),
  aes(y = territory, x = value, fill = name)
) +
  # stacked bars (geom_col defaults to stack)
  
  labs(fill = 'Reef extent (km2)') +
  
  geom_col(show.legend = T) +
  
  scale_fill_manual(
    values = c("#FFFFFF", "#009999"),
    limits = c("reef_extent_km2.50_plus_total", "reef_extent_km2.non_50_plus"),
    breaks = c("reef_extent_km2.50_plus_total", "reef_extent_km2.non_50_plus"),
    labels = c("Not selected", "Selected")
  ) +
  
  guides(fill = guide_legend(title.position="top", 
                             title.hjust = 0.5, 
                             override.aes = list(colour = 'black'))) +
  
  theme_void(base_size = 18) +
  theme(
    legend.position = "bottom",          # or "right"
    legend.direction = "horizontal",
    legend.box = "vertical",              # puts title above the keys
    legend.box.just = "center",
    legend.title = element_text(size = 18),
    legend.text  = element_text(size = 12),
    plot.margin  = margin(0, 0, 0, 0)
  )


plot.legend <- ggpubr::get_legend(plot.legend)






design <- "
11111111111112222222222222
11111111111112222222222222
11111111111112222222222222
11111111111112222222222222
11111111111112222222222222
11111111111112222222222222
11111111111112222222222222
11111111111112222222222222
11111111111112222222222222
11111111111112222222222222
11111111111112222222222222
11111111111112222222222222
11111111111112222222222222
11111111111112222222222222
11111111111112222222222222
11111111111112222222222222
11111111111112222222222222
33333333333333333333333333
"

final_fig <- plot.left / plot.right / plot.legend +
  plot_layout(design = design) 

final_fig

ggsave(plot = final_fig,
       filename = paste0(out_dir,'bloomberg_figure_5_prioritisations_by_eez.png'),
       height = 1414,
       width = 1200,
       units = 'px',
       scale = 2.5)


ggsave(plot = final_fig,
       filename = paste0(out_dir,'bloomberg_figure_5_prioritisations_by_eez.pdf'),
       height = 1414,
       width = 1200,
       units = 'px',
       scale = 2.5)















