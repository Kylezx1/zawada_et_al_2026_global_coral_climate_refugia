# Bloomberg paper figure 4.
# ..... Selected reef area by EEZ

rm(list=ls())

# Setup ====

library(tidyverse)
library(ggrepel)
library(patchwork)

library(sf)
library(terra)

library(stars)


source("functions.R")

# Constants

in_file.selections <- "Z:/Dropbox/Lenfest_kenya/data/PROJECT_SPECIFIC/bloomberg/life_history_predictions/erl_revision_2/all_selections_with_beyer_and_eez.rds" 

in_file.mpas <- "Z:/offline_data/data/politics_and_governance/conservation_governance/protected_areas/extent/manually_defined/global/2022/unep_wcmc/marine_polygons_year_iucn_cast_wide.shp"

out_dir <- "bloomberg_figures/erl_revision/ssp370/"


# Main ====

# ..... Load data ====


data.selections <- read_rds(in_file.selections)


data.mpas <- st_read(in_file.mpas)



buf_deg <- 0.05  # degrees
# buf_deg <- 0.1
e <- ext(data.selections)
e <- ext(xmin(e)-buf_deg, xmax(e)+buf_deg, ymin(e)-buf_deg, ymax(e)+buf_deg)

r_m <- rast(ext = e, crs = crs(data.selections), resolution = buf_deg)  # exactly 2000 m cells


data.mpas.rast <- rasterize(data.mpas %>% filter(icun_ny == 1) %>% vect(), 
                            r_m, 
                            field = 'icun_ny',
                            touches = T)



bb <- st_bbox(c(xmin = -14e+06, 
                xmax = 16e+06, 
                ymax = 40e+05, 
                ymin = -40e+05), 
              crs = st_crs(data.mpas))


data.mpas.rast <- crop(data.mpas.rast, bb)

data.selections.xy <- data.selections %>% 
  select(grid_id, geometry) %>% 
  st_centroid() %>% 
  vect()


data.iucn_any <- extract(data.mpas.rast, data.selections.xy)

data.selections[["iucn_any"]] <- data.iucn_any$icun_ny

data.selections.by_mpa <- data.selections %>% 
  
  filter(country != "Brazil") %>% 
  
  st_drop_geometry() %>% 
  
  mutate(iucn_any = ifelse(is.na(iucn_any), 0, iucn_any)) %>% 
  
  group_by(iucn_any, selected) %>% 
  
  summarise(area_km2 = sum(extent_area_present_m2_total_cover)/1000^2)



data.selections.by_eez <- data.selections %>% 
  
  st_drop_geometry() %>% 
  
  mutate(iucn_any = ifelse(is.na(iucn_any), 0, iucn_any)) %>% 
  
  group_by(territory, selected) %>% 
  
  summarise(area_km2 = sum(extent_area_present_m2_total_cover)/1000^2)






data.selections.beyer.eez <- data.selections %>% 
  st_drop_geometry() %>% 
  group_by(territory, is_bcu, selected) %>% 
  summarise(area = sum(extent_area_present_m2_total_cover)) %>% 
  paste0()


data.selections_by_eez <- data.selections %>% 
  st_drop_geometry() %>% 
  group_by(territory, is_bcu, selected) %>% 
  summarise(area = sum(extent_area_present_m2_total_cover))




eez_order <- data.selections_by_eez %>% 
  arrange(total_reef_extent) %>% 
  select(territory) %>% 
  left_join(data.eez_regions)

region_columns <- eez_order %>% 
  group_by(region) %>% 
  summarise(n_eezs = n())


region_columns$col <- NA_character_

sumA <- 0L
sumB <- 0L

for (i in seq_len(nrow(region_columns))) {
  if (sumA <= sumB) {
    region_columns$col[i] <- "A"
    sumA <- sumA + region_columns$n_eezs[i]
  } else {
    region_columns$col[i] <- "B"
    sumB <- sumB + region_columns$n_eezs[i]
  }
}



data.selections_by_eez.pivot <- data.selections_by_eez %>% 
  
  mutate(territory = factor(territory, levels = eez_order$territory)) %>% 
  
  select(-country) %>% 
  
  mutate(reef_extent_km2.50_plus_total = reef_extent_km2.50_plus_only + reef_extent_km2.both_methods) %>% 
  
  mutate(reef_extent_km2.non_50_plus = total_reef_extent - reef_extent_km2.50_plus_total) %>% 
  
  mutate(percent.50_plus_total = reef_extent_km2.50_plus_total / total_reef_extent * 100) %>% 
  
  pivot_longer(cols = -territory) %>% 
  
  left_join(data.eez_regions) %>% 
  
  left_join(region_columns)


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


df.small_sum.marker <- df %>% 
  filter(name == 'total_reef_extent') %>% 
  mutate(small_countries = ifelse(value < 250, 'small', 'large')) %>% 
  select(-name, -value, -n_eezs, -lbl)

df <- df %>% 
  left_join(df.small_sum.marker) %>% 
  mutate(territory2 = ifelse(small_countries == 'small', 'Small countries', as.character(territory))) %>% 
  
  group_by(territory2, region, name, col) %>% 
  
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
  select(region, territory) %>% 
  distinct()



region_columns <- eez_order %>% 
  group_by(region) %>% 
  summarise(n_eezs = n())


region_columns$col <- NA_character_

sumA <- 0L
sumB <- 0L

for (i in seq_len(nrow(region_columns))) {
  if (sumA <= sumB) {
    region_columns$col[i] <- "A"
    sumA <- sumA + region_columns$n_eezs[i]
  } else {
    region_columns$col[i] <- "B"
    sumB <- sumB + region_columns$n_eezs[i]
  }
}


region_columns$col[8] <- "A"


df <- df %>%
  
  select(-col) %>% 
  
  left_join(region_columns)


df <- df %>% 
  mutate(territory = factor(territory, levels = eez_order$territory %>% unique()))







plot.theme <-  theme(panel.grid.major.y = element_blank(),
                     panel.grid.minor.y = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     
                     axis.text.x = element_text(angle = -30, hjust = 0, vjust = 0.5))

plot.left <- ggplot(
  
  data = df %>% filter(col == "A") %>% filter(value != 0) %>% filter(name %in% c("reef_extent_km2.non_50_plus", "reef_extent_km2.50_plus_total")),
  aes(y = territory, x = value, fill = name)) +
  
  # stacked bars (geom_col defaults to stack)
  geom_vline(xintercept = 0, colour = "#222222") +
  
  geom_col(show.legend = F, width = 0.9, colour = 'grey50', linewidth = 0.5) +
  
  # labels centered within each stacked segment
  geom_text(data = df %>% filter(col == "A") %>% filter(name == "total_reef_extent") %>% 
              
              select(territory, region, value) %>% 
              
              left_join(df %>% filter(name == "reef_extent_km2.50_plus_total")  %>% select(region, territory, lbl)),
    aes(label = lbl,
        x = value,
        y = territory),
    # position = position_stack(vjust = 0.5),
    # x = Inf,
    hjust = 0,
    nudge_x = 1000,
    colour = "#009999",
    size = 3,
    lineheight = 0.9,
    inherit.aes = F
  ) +
  
  # labels centered within each stacked segment
  geom_text(data = df %>% filter(col == "A") %>% filter(name == "total_reef_extent") %>% 
              
              select(territory, region, value) %>% 
              
              left_join(df %>% filter(name == "percent.50_plus_total")  %>% select(region, territory, lbl)),
            aes(label = paste0('(',lbl,'%)'),
                x = value,
                y = territory),
            # position = position_stack(vjust = 0.5),
            # x = Inf,
            hjust = -1.7,
            nudge_x = 2000,
            colour = "#009999",
            size = 2,
            lineheight = 0.9,
            inherit.aes = F
  ) +
  
  geom_text(data = df %>% filter(col == "A") %>% filter(name == 'total_reef_extent'),
            aes(label = lbl),
            # position = position_stack(vjust = 0.5),
            x = -1000,
            hjust = 1,
            colour = "grey50",
            size = 3,
            lineheight = 0.9
  ) +
  
  
  facet_wrap(region~.,
             scales = "free_y",
             space = "free_y",

             # ncol = 2
             ) +
  
  scale_x_continuous(limits = c(-35000, 150000),
                     breaks = c(0, 10000, 50000)
                     ) +
  
  scale_fill_manual(
    values = c("#009999", "#FFFFFF"),
    limits = c("reef_extent_km2.50_plus_total", "reef_extent_km2.non_50_plus"),
    breaks = c("reef_extent_km2.50_plus_total", "reef_extent_km2.non_50_plus"),
    labels = c("Selected", "Not selected")
  ) +
  
  
  
  labs(y = "", x = "") +
  
  theme_bw(base_size = 12) +
  
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
              
              select(territory, region, value) %>% 
              
              left_join(df %>% filter(name == "reef_extent_km2.50_plus_total")  %>% select(region, territory, lbl)),
            aes(label = lbl,
                x = value,
                y = territory),
            # position = position_stack(vjust = 0.5),
            # x = Inf,
            hjust = 0,
            nudge_x = 1000,
            colour = "#009999",
            size = 3,
            lineheight = 0.9,
            inherit.aes = F
  ) +
  
  # labels centered within each stacked segment
  geom_text(data = df %>% filter(col == "B") %>% filter(name == "total_reef_extent") %>% 
              
              select(territory, region, value) %>% 
              
              left_join(df %>% filter(name == "percent.50_plus_total")  %>% select(region, territory, lbl)),
            aes(label = paste0('(',lbl,'%)'),
                x = value,
                y = territory),
            # position = position_stack(vjust = 0.5),
            # x = Inf,
            hjust = -1.7,
            nudge_x = 2000,
            colour = "#009999",
            size = 2,
            lineheight = 0.9,
            inherit.aes = F
  ) +
  
  geom_text(data = df %>% filter(col == "B") %>% filter(name == 'total_reef_extent'),
            aes(label = ifelse(value > 0, lbl, NA_character_)),
            # position = position_stack(vjust = 0.5),
            x = -1000,
            hjust = 1,
            colour = "grey50",
            size = 3,
            lineheight = 0.9
  ) +
  
  
  facet_wrap(region~.,
             scales = "free_y",
             space = "free_y",
             
             # ncol = 2
  ) +
  
  scale_x_continuous(limits = c(-35000, 150000),
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
  
  theme_bw(base_size = 12) +
  
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
#33333#######2222222222222
"

final_fig <- plot.left / plot.right / plot.legend +
  plot_layout(design = design) 

final_fig

ggsave(plot = final_fig,
       filename = paste0(out_dir,'bloomberg_figure_4_prioritisations_by_eez.png'),
       height = 1414,
       width = 1200,
       units = 'px',
       scale = 2.5)
















