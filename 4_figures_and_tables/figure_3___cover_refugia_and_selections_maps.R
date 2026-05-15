# Bloomberg figure 3 - avoidance, resistance, recovery prioritisations by country

# Setup ====
rm(list=ls())

# ..... Libraries and functions ====
library(tidyverse)
library(terra)
library(sf)
library(tidyterra)
library(patchwork)

library(ggforce)

library(colorspace)

library(rnaturalearth)
library(rnaturalearthdata)

source('functions.R')

out_dir <- "bloomberg_figures/erl_revision/ssp370/"


# ..... Constants ====

in_file.selections <- "Z:/Dropbox/Lenfest_kenya/data/PROJECT_SPECIFIC/bloomberg/life_history_predictions/erl_revision_2/all_selections_with_beyer_and_eez.rds"

in_file.prov <- "Z:/offline_data/data/ecological/marine/coral/provinces/mapped/global/2024/sal_keith_updated/CoralProvincespOLY_PRJ_extended_and_updated.shp"

in_file.ecoregions <- "Z:/offline_data/data/geography/biogeography/marine_ecoregions/global/2024/tnc/marine_ecoregions_of_the_world/meow_ecos_provinces.shp"

in_file.eezs <- "Z:/offline_data/data/politics_and_governance/jurisdictions/economic/exclusive_economic_zones/manually_defined_polygons/global/current/marine_regions.org/World_EEZ_v12_20231025/eez_v12.shp"

in_dir.reef_extent_global <- "Z:/offline_data/data/ecological/marine/coral/extent/mixed_methods/global/contemporary/aca_and_wcmc_250m_global_grids/agulhas/coral_wcmc_and_aca_points_250m_agulhas.tif"

in_file.beyer <- "Z:/offline_data/data/politics_and_governance/conservation_governance/refugia/coral/global/2018/expert_opinion/beyer_et_al/50Reefs_max_return_BCUs.shp"


province_drop <- c("Brazil", "West Africa")


map_bounds_buffer_m <- 500000  # 500 km

target_crs <- "+proj=eqearth +lon_0=160 +datum=WGS84 +units=m +no_defs"
target_crs <- '+proj=goode +lon_0=160 +datum=WGS84'
target_crs <- "+proj=cea +lat_ts=22 +lon_0=150 +datum=WGS84 +units=m +no_defs"


# Main ====

# ..... Load and transform data ====


# ..... Export and rasterise by ecoregion ====

data.ecoregions <- st_read(in_file.ecoregions) %>% 
  select(province = PROVINCE,
         geometry)

data.ecoregions <- st_transform(data.ecoregions, target_crs)

data.ecoregions <- data.ecoregions %>% 
  validate_and_fix_polygons() %>%  
  rotate_if_needed() %>% 
  validate_and_fix_polygons()



data.eezs <- st_read(in_file.eezs) 

data.eezs <- st_transform(data.eezs, target_crs)

data.eezs <- data.eezs %>% validate_and_fix_polygons()




world <- ne_countries(scale = "large", returnclass = "sf")  %>% 
  st_transform(target_crs)

world <- world %>% validate_and_fix_polygons() %>%  rotate_if_needed() %>% validate_and_fix_polygons()


# coral_provinces bbox in the same CRS as `world`
# bb <- sf::st_bbox(data.selections)

bb <- st_bbox(c(xmin = -14e+06, 
                xmax = 16e+06, 
                ymax = 40e+05, 
                ymin = -40e+05), 
              crs = target_crs)


# crop
world <- sf::st_crop(world, ext(bb))

# optional cleanup
world <- world %>% 
  sf::st_make_valid()  %>% 
  dplyr::filter(!sf::st_is_empty(geometry))



data.selections <- read_rds(in_file.selections)


data.selections <- data.selections %>% 
  mutate(future.cover.pc = (effective_cover_future_m2_total_cover/extent_area_present_m2_total_cover)*100)



data.selections.errors <- data.selections %>% 
  filter(future.cover.pc > 50)

st_write(data.selections %>% dplyr::select(geometry, 
                                           future.cover.pc, 
                                           effective_cover_future_m2_total_cover,
                                           extent_area_present_m2_total_cover),
         "selections_errors_ssp585.shp", append = F)


data.selections <- st_make_valid(data.selections)

# If your polygons cross the dateline, wrap before projecting (optional but robust):
data.selections <- st_wrap_dateline(data.selections,
                                    options = c("WRAPDATELINE=YES","DATELINEOFFSET=180"),
                                    quiet = TRUE)

data.selections <- st_transform(data.selections, target_crs)

data.selections <- data.selections %>% validate_and_fix_polygons()



# crop
data.selections <- sf::st_crop(data.selections, ext(bb))

# optional cleanup
data.selections <- data.selections %>% 
  sf::st_make_valid()  %>% 
  dplyr::filter(!sf::st_is_empty(geometry))



data.lh_dominance <- data.selections %>% 

  st_drop_geometry() %>% 
  
  mutate(total_coral_cover_pc = effective_cover_present_m2_total_cover/extent_area_present_m2_total_cover) %>% 
  
  mutate(lh_total = effective_cover_present_m2_competitive+effective_cover_present_m2_stress_tolerant+effective_cover_present_m2_weedy) %>% 
  
  mutate(effective_cover_present_m2_competitive.01 = rescale_0_to_1(effective_cover_present_m2_competitive)) %>%
  mutate(effective_cover_present_m2_stress_tolerant.01 = rescale_0_to_1(effective_cover_present_m2_stress_tolerant)) %>%
  mutate(effective_cover_present_m2_weedy.01 = rescale_0_to_1(effective_cover_present_m2_weedy)) %>%
  
  rowwise() %>% 
  mutate(max_lh = max(c_across(c('effective_cover_present_m2_competitive','effective_cover_present_m2_stress_tolerant','effective_cover_present_m2_weedy')), na.rm = T)) %>% 
  mutate(max_lh.01 = max(c_across(c('effective_cover_present_m2_competitive.01','effective_cover_present_m2_stress_tolerant.01','effective_cover_present_m2_weedy.01')), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(dominant_lh = case_when(
    max_lh.01 == effective_cover_present_m2_competitive.01 ~ 'competitive',
    max_lh.01 == effective_cover_present_m2_stress_tolerant.01 ~ 'stress_tolerant',
    max_lh.01 == effective_cover_present_m2_weedy.01 ~ 'weedy'
  )) %>% 
  
  mutate(dominant_lh.abs = case_when(
    max_lh == effective_cover_present_m2_competitive ~ 'competitive',
    max_lh == effective_cover_present_m2_stress_tolerant ~ 'stress_tolerant',
    max_lh == effective_cover_present_m2_weedy ~ 'weedy'
  ))


data.selections <- data.selections %>% 
  left_join(data.lh_dominance %>% dplyr::select(grid_id, total_coral_cover_pc, dominant_lh, max_lh.01, dominant_lh.abs, max_lh))

data.selections <- data.selections %>% 
  mutate(delta_2020_to_2050 = effective_cover_future_m2_total_cover - effective_cover_present_m2_total_cover)


data.selections <- data.selections %>% 
  mutate(total_coral_cover_pc.future = effective_cover_future_m2_total_cover/extent_area_present_m2_total_cover) %>% 
  mutate(delta_2020_to_2050_pc = total_coral_cover_pc.future - total_coral_cover_pc)


write_rds(data.selections, paste0(out_dir,'selections_processed.rds'))



data.selections <- read_rds(paste0(out_dir,'selections_processed.rds'))

data.selections.wgs84 <- quick_project(data.selections, st_crs(4326))


st_write(data.selections.wgs84, paste0(out_dir,'selections_processed.geojson'))



plot.prioritisations.global <- ggplot(data.selections, aes(fill = selected)) +
  
  geom_sf(colour = NA) +
  
  # geom_sf_label(data = world_x, aes(label = admin),
  #              inherit.aes = F) +
  
  
  geom_sf(data = world, fill = "#AAAAAA", linewidth = 0.5) +
  
  scale_fill_manual(values = c("#00AAAA", '#AA0033'),
                    labels = c("Selected", "Not selected")) +
  
  coord_sf(clip = 'on', 
           expand = F) +
  
  guides(
    fill = guide_legend(
      title = "Prioritisations @ 5.5 km2",
      # legend.text.position = "center",
      byrow  = TRUE,
      title.position = "top",
      theme = theme(legend.title = element_text(hjust = 0.5))
    ),
  ) +
  
  labs(x = "Longitude", y = "Latitude") +
  
  theme_minimal(base_size = 22) +
  
  theme(panel.grid.major.y = element_line(colour = "#EEEEEE"),
        
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal")


ggsave("prioritisations_by_ecoregion/GLOBAL/prioritisations_selected.png",
       plot.prioritisations.global,
       width = 1414,
       height = 1000,
       units = 'px',
       dpi = 320,
       scale = 7)


plot.cover_2020.global <- ggplot(data.selections, aes(fill = effective_cover_present_m2_total_cover/extent_area_present_m2_total_cover)) +
  
  
  geom_sf(colour = NA) +
  
  # geom_sf_text(data = world_x, aes(label = admin),
  #              inherit.aes = F) +
  
  
  geom_sf(data = world, fill = "#AAAAAA", linewidth = 0.2) +
  
  scale_fill_iriviridis_c(reverse = T, end = 0.8) +
  
  coord_sf(clip = 'on', expand = F) +
  
  guides(
    fill = guide_colourbar(
      title = "Coral cover 2020\n(% @ 5.5 km2)",
      barheight = 1,
      barwidth = 10,
      # legend.text.position = "center",
      byrow  = TRUE,
      title.position = "top",
      theme = theme(legend.title = element_text(hjust = 0.5))
    ),
  ) +
  
  labs(x = "Longitude", y = "Latitude") +
  
  theme_minimal(base_size = 22) +
  
  theme(panel.grid.major.y = element_line(colour = "#EEEEEE"),
        
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal")


ggsave("prioritisations_by_ecoregion/GLOBAL/coral_cover_2020_percent.png",
       plot.cover_2020.global,
       width = 1414,
       height = 1000,
       units = 'px',
       dpi = 320,
       scale = 7)


plot.arr.global <- ggplot(data.selections %>% 
                            mutate(dominant_lh = case_when(
                              dominant_lh.abs == 'competitive' ~ "Avoidance\n(competitive)",
                              dominant_lh.abs == 'stress_tolerant' ~ "Resistance\n(stress tolerant)",
                              dominant_lh.abs == 'weedy' ~ "Recovery\n(weedy)"
                            )), aes(fill = dominant_lh)) +
  
  
  geom_sf(colour = NA) +
  
  # geom_sf_text(data = world_x, aes(label = admin),
  #              inherit.aes = F) +
  
  
  geom_sf(data = world, fill = "#AAAAAA", linewidth = 0.2) +
  
  # scale_fill_iriviridis_c(reverse = T, end = 0.8) +
  scale_fill_manual(values = c('#d55e00','#019e73', '#0072b2')) +
  
  coord_sf(clip = 'on', expand = F) +
  
  guides(
    fill = guide_legend(
      title = "Change in coral cover\n(2020 to 2050 SSP585)",
      barheight = 1,
      barwidth = 10,
      # legend.text.position = "center",
      byrow  = TRUE,
      title.position = "top",
      theme = theme(legend.title = element_text(hjust = 0.5))
    ),
  ) +
  
  labs(x = "Longitude", y = "Latitude") +
  
  theme_minimal(base_size = 22) +
  
  theme(panel.grid.major.y = element_line(colour = "#EEEEEE"),
        
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal")


ggsave("prioritisations_by_ecoregion/GLOBAL/ARR_dominance.png",
       plot.arr.global,
       width = 1414,
       height = 1000,
       units = 'px',
       dpi = 320,
       scale = 7)



plot.deltas.global <- ggplot(data.selections, aes(fill = delta_2020_to_2050_pc)) +
  
  
  geom_sf(colour = NA) +
  
  # geom_sf_text(data = world_x, aes(label = admin),
  #              inherit.aes = F) +
  
  
  geom_sf(data = world, fill = "#AAAAAA", linewidth = 0.2) +
  
  # scale_fill_iriviridis_c(reverse = T, end = 0.8) +
  scale_fill_gradient2(low = "#880000", mid = '#AAAAAA', high = "#000088") +
  
  coord_sf(clip = 'on', expand = F) +
  
  guides(
    fill = guide_colourbar(
      title = "Refugia type\n(Dominant life history)",
      # legend.text.position = "center",
      byrow  = TRUE,
      title.position = "top",
      theme = theme(legend.title = element_text(hjust = 0.5))
    ),
  ) +
  
  labs(x = "Longitude", y = "Latitude") +
  
  theme_minimal(base_size = 22) +
  
  theme(panel.grid.major.y = element_line(colour = "#EEEEEE"),
        
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal")


ggsave("prioritisations_by_ecoregion/GLOBAL/2020_to_2050_deltas_ssp585.png",
       plot.deltas.global,
       width = 1414,
       height = 1000,
       units = 'px',
       dpi = 320,
       scale = 7)



data.selections_250m <- rast("Z:/Dropbox/Work/projects/mq_wcs/wcs_kenya/analysis/modern_portfolio_theory/prioritisations_250m.tif")

data.coral_2020 <- rast("Z:/Dropbox/Lenfest_kenya/data/PROJECT_SPECIFIC/bloomberg/life_history_predictions/paper_202601/composite_predictions_total_coral_2020.tif")

data.coral_2050_ssp585 <- rast("Z:/Dropbox/Lenfest_kenya/data/PROJECT_SPECIFIC/bloomberg/life_history_predictions/paper_202601/composite_predictions_total_cover_ssp585.tif")

data.coral_2050_ssp370 <- rast("Z:/Dropbox/Lenfest_kenya/data/PROJECT_SPECIFIC/bloomberg/life_history_predictions/erl_revision/composite_predictions_total_coral_ssp370.tif")




world <- ne_countries(scale = "large", returnclass = "sf")  %>% 
  st_transform(target_crs)

world <- world %>% validate_and_fix_polygons() %>%  rotate_if_needed() %>% validate_and_fix_polygons()



x = 1
plots.prioritisations.by_ecoregion <- lapply(1:nrow(data.ecoregions), function(x) {
  
  print(x)
  
  ecoregion_x <- data.ecoregions[x,]
  
  ecoregion_x <- quick_project(ecoregion_x, st_crs(data.selections_250m))
  
  dir.create(paste0("prioritisations_by_ecoregion/erl_revision/",ecoregion_x$province[1]), recursive = T)
  
  tryCatch(
    {
      
      data_selections_x <- crop(data.selections_250m, ecoregion_x, mask=TRUE)
      data.coral_2020_x <- crop(data.coral_2020, ecoregion_x, mask=TRUE)
      data.coral_2050_x <- crop(data.coral_2050_ssp585, ecoregion_x, mask=TRUE)
   
    },
    error = function(e) {
      message("No data in region")
      return()
    }
  )
  
  
  data_selections_x <- as.factor(data_selections_x)

  if(all(is.na(terra::values(data_selections_x)))) {
    print("No data in region")
    return(NULL)
    
  }
  
  
  world_x <- quick_project(world, st_crs(data.selections_250m)) 

  world_x <- world_x %>% st_intersection(ecoregion_x)
  
  
  
  plot.prioritisations.x <- ggplot(data_selections_x, aes(fill = selected)) +
    
    geom_spatraster(data = data_selections_x) +
    
    # geom_sf_label(data = world_x, aes(label = admin),
    #              inherit.aes = F) +
    

    geom_sf(data = world_x, fill = "#AAAAAA") +
    
    scale_fill_manual(values = c("#00AAAA", '#AA0033'),
                      labels = c("Selected", "Not selected")) +
    
    coord_sf(clip = 'on', 
             expand = T) +
    
    guides(
      fill = guide_legend(
        title = "Prioritisations @ 250 m x 250 m",
        # legend.text.position = "center",
        byrow  = TRUE,
        title.position = "top",
        theme = theme(legend.title = element_text(hjust = 0.5))
      ),
    ) +
    
    ggtitle(paste0(ecoregion_x$province[1])) +
    
    
    labs(x = "Longitude", y = "Latitude") +
  
    theme_minimal(base_size = 22) +
    
    theme(panel.grid.major.y = element_line(colour = "#EEEEEE"),
          
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.box = "horizontal")
  
  
  
  ggsave(paste0("prioritisations_by_ecoregion/",ecoregion_x$province[1],"/prioritisations_selected_",ecoregion_x$province[1],".png"),
         plot.prioritisations.x,
         height = 1414,
         width = 1000,
         units = 'px',
         dpi = 320,
         scale = 5)
  
  
  
  plot <- ggplot() +
    
    
    geom_spatraster(data = data.coral_2020_x*100) +
    
    # geom_sf_text(data = world_x, aes(label = admin),
    #              inherit.aes = F) +
    
    
    geom_sf(data = world_x, fill = "#AAAAAA") +
    
    scale_fill_iriviridis_c(reverse = T, end = 0.8, limits = c(5, 45)) +
    
    coord_sf(clip = 'on', expand = T) +
    
    guides(
      fill = guide_colourbar(
        title = "Coral cover % 2020\n(% @ 250 m x 250 m )",
        barheight = 1,
        barwidth = 20,
        # legend.text.position = "center",
        byrow  = TRUE,
        title.position = "top",
        theme = theme(legend.title = element_text(hjust = 0.5))
      ),
    ) +
    
    labs(x = "Longitude", y = "Latitude") +
    
    ggtitle(paste0(ecoregion_x$province[1])) +
    
    theme_minimal(base_size = 22) +
    
    theme(panel.grid.major.y = element_line(colour = "#EEEEEE"),
          
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.box = "horizontal")
  
  
  ggsave(paste0("prioritisations_by_ecoregion/",ecoregion_x$province[1],"/coral_cover_2020_percent_",ecoregion_x$province[1],".png"),
         plot,
         height = 1414,
         width = 1000,
         units = 'px',
         dpi = 320,
         scale = 5)
  
  
  plot <- ggplot() +
    
    
    geom_spatraster(data = data.coral_2050_x*100) +
    
    # geom_sf_text(data = world_x, aes(label = admin),
    #              inherit.aes = F) +
    
    
    geom_sf(data = world_x, fill = "#AAAAAA") +
    
    scale_fill_iriviridis_c(reverse = T, end = 0.8, limits = c(5, 45)) +
    
    coord_sf(clip = 'on', expand = T) +
    
    guides(
      fill = guide_colourbar(
        title = "Coral cover % 2020\n(% @ 250 m x 250 m )",
        barheight = 1,
        barwidth = 20,
        # legend.text.position = "center",
        byrow  = TRUE,
        title.position = "top",
        theme = theme(legend.title = element_text(hjust = 0.5))
      ),
    ) +
    
    labs(x = "Longitude", y = "Latitude") +
    
    ggtitle(paste0(ecoregion_x$province[1])) +
    
    theme_minimal(base_size = 22) +
    
    theme(panel.grid.major.y = element_line(colour = "#EEEEEE"),
          
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.box = "horizontal")
  
  
  ggsave(paste0("prioritisations_by_ecoregion/",ecoregion_x$province[1],"/coral_cover_2050_ssp585_percent_",ecoregion_x$province[1],".png"),
         plot,
         height = 1414,
         width = 1000,
         units = 'px',
         dpi = 320,
         scale = 5)
  
  list(ecoregion_x$province, plot.prioritisations.x)
  
})


plots2 <- compact(plots)


data.selections.centroids <- st_centroid(data.selections) %>% st_coordinates()

data.selections$lat <- data.selections.centroids[,"Y"]
data.selections$lon <- data.selections.centroids[,"X"]

data.selections <- data.selections %>% 
  mutate(lat.snap = (round(lat/50000,0))*50000,
         lon.snap = (round(lon/50000,0))*50000)



data.selections <- data.selections %>% 
  mutate(regrid_group = paste0(lat.snap,'_',lon.snap))


data.selected_pixel_count <- data.selections %>% 
  
  st_drop_geometry() %>% 
  
  group_by(regrid_group, selected) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = regrid_group,
              names_from = selected,
              values_from = n,
              values_fill = 0) %>% 
  
  mutate(total_cells = in_portfolio + not_selected) %>% 
  mutate(prop.selected = in_portfolio/total_cells)
  

data.coral_cover_pc <- data.selections %>% 
  
  st_drop_geometry() %>%
  
  group_by(regrid_group) %>% 
  summarise(total_coral_cover_pc.2020 = mean(total_coral_cover_pc)*100,
            total_coral_cover_pc.2050 = mean(total_coral_cover_pc.future)*100,
            delta_2020_to_2050_pc = mean(delta_2020_to_2050_pc)*100) %>% 
  ungroup()


data.arr <- data.selections %>% 
  
  st_drop_geometry() %>%
  
  group_by(regrid_group) %>% 
  
  summarise(effective_cover_present_m2_competitive = mean(effective_cover_present_m2_competitive, na.rm = T),
            effective_cover_present_m2_stress_tolerant = mean(effective_cover_present_m2_stress_tolerant, na.rm = T),
            effective_cover_present_m2_weedy = mean(effective_cover_present_m2_weedy, na.rm = T)) %>%
  
  
  mutate(effective_cover_present_m2_competitive.01 = rescale_0_to_1(effective_cover_present_m2_competitive)) %>%
  mutate(effective_cover_present_m2_stress_tolerant.01 = rescale_0_to_1(effective_cover_present_m2_stress_tolerant)) %>%
  mutate(effective_cover_present_m2_weedy.01 = rescale_0_to_1(effective_cover_present_m2_weedy)) %>%
  
  ungroup() %>% 
  
  rowwise() %>% 
  
  mutate(max_lh = max(c_across(c('effective_cover_present_m2_competitive','effective_cover_present_m2_stress_tolerant','effective_cover_present_m2_weedy')), na.rm = T)) %>% 
  mutate(max_lh.01 = max(c_across(c('effective_cover_present_m2_competitive.01','effective_cover_present_m2_stress_tolerant.01','effective_cover_present_m2_weedy.01')), na.rm = T)) %>% 
  
  ungroup() %>% 
  
  mutate(dominant_lh = case_when(
    max_lh.01 == effective_cover_present_m2_competitive.01 ~ 'competitive',
    max_lh.01 == effective_cover_present_m2_stress_tolerant.01 ~ 'stress_tolerant',
    max_lh.01 == effective_cover_present_m2_weedy.01 ~ 'weedy'
  )) %>% 
  
  mutate(dominant_lh.abs = case_when(
    max_lh == effective_cover_present_m2_competitive ~ 'competitive',
    max_lh == effective_cover_present_m2_stress_tolerant ~ 'stress_tolerant',
    max_lh == effective_cover_present_m2_weedy ~ 'weedy'
  )) %>% 
  
  ungroup()



data.selections.regridded <- data.selections %>% 
  group_by(regrid_group) %>% 
  summarise(n = n()) %>% 
  
  left_join(data.selected_pixel_count) %>% 
  
  left_join(data.coral_cover_pc) %>% 
  
  left_join(data.arr)


st_write(data.selections.regridded, "bloomberg_figures/erl_revision/ssp370/50km_variable_aggregations.shp", append = F)

write_rds(data.selections.regridded, "bloomberg_figures/erl_revision/ssp370/50km_variable_aggregations.rds")


data.selections.regridded <- read_rds("bloomberg_figures/erl_revision/ssp370/50km_variable_aggregations.rds")


data.selections.regridded.vect <- vect(data.selections.regridded)


data.selections.regridded.vect <- data.selections.regridded.vect %>% 
  mutate(pc.selected = prop.selected*100)


buf_deg <- 50000  # metres
# buf_deg <- 0.1
e <- ext(data.selections.regridded.vect)
e <- ext(xmin(e)-buf_deg, xmax(e)+buf_deg, ymin(e)-buf_deg, ymax(e)+buf_deg)

r_m <- rast(ext = e, crs = crs(data.selections.regridded.vect), resolution = buf_deg)  # exactly 2000 m cells



out.selections.prop <- rasterize(data.selections.regridded.vect, r_m, field = 'prop.selected',
                                 touches = T, 
                                 )

out.selections.pc <- rasterize(data.selections.regridded.vect, r_m, field = 'pc.selected',
                                 touches = T, 
)

out.selections.raw <- rasterize(data.selections.regridded.vect, r_m, field = 'in_portfolio',
                                touches = T)

out.selections.cover_2020 <- rasterize(data.selections.regridded.vect, r_m, field = 'total_coral_cover_pc.2020',
                                       touches = T)

out.selections.cover_2050 <- rasterize(data.selections.regridded.vect, r_m, field = 'total_coral_cover_pc.2050',
                                       touches = T)


out.selections.cover_deltas <- rasterize(data.selections.regridded.vect, r_m, field = 'delta_2020_to_2050_pc',
                                         touches = T)


out.selections.ARR_01 <- rasterize(data.selections.regridded.vect, r_m, field = 'dominant_lh',
                                touches = T)


out.selections.ARR <- rasterize(data.selections.regridded.vect, r_m, field = 'dominant_lh.abs',
                                   touches = T)





out <- c(out.selections.prop, 
         out.selections.pc,
         out.selections.raw,
         out.selections.cover_2020,
         out.selections.cover_2050,
         out.selections.cover_deltas,
         out.selections.ARR,
         out.selections.ARR_01
         )


# out <- out %>% project(target_crs)

writeRaster(out, 
            file = paste0(out_dir,'outputs_summarised_to_',buf_deg,'_metres.tif'),
            overwrite = T)

out <- rast(paste0(out_dir,'outputs_summarised_to_',buf_deg,'_metres.tif'))



# writeRaster(out, 'TESTING.tif')

# World map and provinces



# coral_provinces bbox in the same CRS as `world`
bb <- sf::st_bbox(out)

bb <- st_bbox(c(xmin = -14e+06, 
                xmax = 16e+06, 
                ymax = 40e+05, 
                ymin = -40e+05), 
              crs = target_crs)


out.crop <- crop(out, ext(bb))


world <- ne_countries(scale = "large", returnclass = "sf")  %>% 
  st_transform(st_crs(out))

world <- world %>% validate_and_fix_polygons() %>%  rotate_if_needed() %>% validate_and_fix_polygons()




# crop
world <- sf::st_crop(world, ext(out.crop))

# optional cleanup
world <- world %>% 
  sf::st_make_valid()  %>% 
  dplyr::filter(!sf::st_is_empty(geometry))



# ..... Plots ====


out.crop$selection_filter <- ifel(out.crop$prop.selected > 0, 1, NA)

out.crop$selection_hinge <- ifel(out.crop$prop.selected > 0, out.crop$prop.selected, NA)



# plots 50km agg

plot.pc_selected.global <- ggplot() +
  geom_spatraster(data = out.crop, aes(fill = pc.selected),
                  maxcell = 5e+08, na.rm = T) +
  # geom_spatraster(data = out$selection_filter, fill = 'darkgrey',
  #                 maxcell = 5e+06, na.rm = T) +
  geom_sf(data = world, fill = "#E5E5E5") +
  
  # scale_fill_gradient2(low = "#222222", mid = '#880000', high = '#EEEE00', midpoint = 0) +
  scale_fill_gradient(low = '#880000', high = '#EEEE00', na.value = "#CFD5D5") +
  
  
  # scale_fill_gradient2(low =  '#AA0033', mid = "#006666", high = "#00AAAA", midpoint = 5) +
  # scale_fill_gradient(low =  '#AA0033', high = "#00AAAA") +
  
  coord_sf(clip = 'on', expand = F) +
  
  labs(fill = "% of reef extent selected") +
  
  theme_bw(base_size = 24) +
  
  # legend along bottom
  theme(
    legend.position = "bottom",
    legend.box      = "horizontal",
    legend.title    = element_text(hjust = 0.5),
    legend.direction = "horizontal",
    legend.key.width = unit(dev.size()[1] / 4, "inches")
  ) +
  
  guides(fill = guide_colourbar(title.position = "top", title.hjust=0.5)) +
         
  
  theme(panel.background = element_rect(fill = '#FFFFFF'),
        panel.grid = element_blank())

plot.pc_selected.global


ggsave(plot = plot.pc_selected.global,
       filename = paste0(out_dir,'yellow_pixels_heatmap.png'),
       width = 1990,
       height = 1080,
       scale = 3,
       units = 'px')

ggsave(plot = plot.pc_selected.global,
       filename = paste0(out_dir,'yellow_pixels_heatmap.pdf'),
       width = 1990,
       height = 1080,
       scale = 3,
       units = 'px')


plot.coral_cover_present <- ggplot() +
  geom_spatraster(data = out.crop, aes(fill = total_coral_cover_pc.2020),
                  maxcell = 5e+08, na.rm = T) +
  # geom_spatraster(data = out$selection_filter, fill = 'darkgrey',
  #                 maxcell = 5e+06, na.rm = T) +
  geom_sf(data = world, fill = "#E5E5E5") +
  
  # scale_fill_gradient2(low = "#222222", mid = '#880000', high = '#EEEE00', midpoint = 0) +
  scale_fill_iriviridis_c(end = 1, reverse = T, na.value = "#CFD5D5",
                          limits = c(5, 42)) +
  
  coord_sf(clip = 'on', expand = F) +
  
  labs(fill = "Coral cover % 2020") +
  
  theme_bw(base_size = 24) +
  
  # legend along bottom
  theme(
    legend.position = "bottom",
    legend.box      = "horizontal",
    legend.title    = element_text(hjust = 0.5),
    legend.direction = "horizontal",
    legend.key.width = unit(dev.size()[1] / 4, "inches")
  ) +
  
  guides(fill = guide_colourbar(title.position = "top", title.hjust=0.5)) +
  
  
  theme(panel.background = element_rect(fill = '#FFFFFF'),
        panel.grid = element_blank())

plot.coral_cover_present


ggsave(plot = plot.coral_cover_present,
       filename = paste0(out_dir,'coral_cover_2020.png'),
       width = 1990,
       height = 1080,
       scale = 3,
       units = 'px')

ggsave(plot = plot.coral_cover_present,
       filename = paste0(out_dir,'coral_cover_2020.pdf'),
       width = 1990,
       height = 1080,
       scale = 3,
       units = 'px')



plot.coral_cover_future <- ggplot() +
  geom_spatraster(data = out.crop, aes(fill = total_coral_cover_pc.2050),
                  maxcell = 5e+08, na.rm = T) +
  # geom_spatraster(data = out$selection_filter, fill = 'darkgrey',
  #                 maxcell = 5e+06, na.rm = T) +
  geom_sf(data = world, fill = "#E5E5E5") +
  
  # scale_fill_gradient2(low = "#222222", mid = '#880000', high = '#EEEE00', midpoint = 0) +
  scale_fill_iriviridis_c(end = 1, reverse = T, na.value = "#CFD5D5",
                          limits = c(5, 42)) +
  
  coord_sf(clip = 'on', expand = F) +
  
  labs(fill = "Coral cover % 2050 SSP370") +
  
  theme_bw(base_size = 24) +
  
  # legend along bottom
  theme(
    legend.position = "bottom",
    legend.box      = "horizontal",
    legend.title    = element_text(hjust = 0.5),
    legend.direction = "horizontal",
    legend.key.width = unit(dev.size()[1] / 4, "inches")
  ) +
  
  guides(fill = guide_colourbar(title.position = "top", title.hjust=0.5)) +
  
  
  theme(panel.background = element_rect(fill = '#FFFFFF'),
        panel.grid = element_blank())

plot.coral_cover_future


ggsave(plot = plot.coral_cover_future,
       filename = paste0(out_dir,'coral_cover_2050_ssp370.png'),
       width = 1990,
       height = 1080,
       scale = 3,
       units = 'px')

ggsave(plot = plot.coral_cover_future,
       filename = paste0(out_dir,'coral_cover_2050_ssp370.pdf'),
       width = 1990,
       height = 1080,
       scale = 3,
       units = 'px')




plot.coral_deltas <- ggplot() +
  geom_spatraster(data = out.crop, aes(fill = delta_2020_to_2050_pc),
                  maxcell = 5e+08, na.rm = T) +
  # geom_spatraster(data = out$selection_filter, fill = 'darkgrey',
  #                 maxcell = 5e+06, na.rm = T) +
  geom_sf(data = world, fill = "#E5E5E5") +
  
  scale_fill_gradient2(low = "#CC0000", mid = '#FFFFFF', high = '#0000CC', midpoint = 0,
                       na.value =  "#CFD5D5") +
  # scale_fill_iriviridis_c(end = 0.9, reverse = T) +
  
  coord_sf(clip = 'on', expand = F) +
  
  labs(fill = "Change in coral cover\n2020 to 2050 SSP585") +
  
  theme_bw(base_size = 24) +
  
  # legend along bottom
  theme(
    legend.position = "bottom",
    legend.box      = "horizontal",
    legend.title    = element_text(hjust = 0.5),
    legend.direction = "horizontal",
    legend.key.width = unit(dev.size()[1] / 4, "inches")
  ) +
  
  guides(fill = guide_colourbar(title.position = "top", title.hjust=0.5)) +
  
  
  theme(panel.background = element_rect(fill = '#FFFFFF'),
        panel.grid = element_blank())

plot.coral_deltas


ggsave(plot = plot.coral_deltas,
       filename = paste0(out_dir,'coral_cover_deltas_2020_to_2050.png'),
       width = 1990,
       height = 1080,
       scale = 3,
       units = 'px')




plot.arr <- ggplot() +
  geom_spatraster(data = out.crop, aes(fill = dominant_lh.abs),
                  maxcell = 5e+08, na.rm = T) +
  # geom_spatraster(data = out$selection_filter, fill = 'darkgrey',
  #                 maxcell = 5e+06, na.rm = T) +
  geom_sf(data = world, fill = "#E5E5E5") +
  
  scale_fill_manual(values = c(lighten('#d55e00', 0.2),lighten('#019e73', 0.2),lighten('#0072b2', 0.2)), na.translate = F,
                    labels = c("Avoidance\n(competitive)", "Resistance\n(stress tolerant)", "Recovery\n(weedy)"),
                    na.value =  "#CFD5D5") +
  # scale_fill_iriviridis_c(end = 0.9, reverse = T) +
  
  coord_sf(clip = 'on', expand = F) +
  
  labs(fill = "Coral refugia by life history\n(absolute dominance)") +
  
  guides(
    fill = guide_legend(
      barheight = 1,
      barwidth = 10,
      # legend.text.position = "center",
      byrow  = TRUE,
      title.position = "top",
      theme = theme(legend.title = element_text(hjust = 0.5))
    ),
  ) +
  
  theme_bw(base_size = 24) +
  
  # legend along bottom
  theme(
    legend.position = "bottom",
    legend.box      = "horizontal",
    legend.title    = element_text(hjust = 0.5),
    legend.direction = "horizontal",
    legend.key.width = unit(dev.size()[1] / 4, "inches")
  ) +
  
  
  theme(panel.background = element_rect(fill =  "#CFD5D5"),
        panel.grid = element_blank())

plot.arr


ggsave(plot = plot.arr,
       filename = paste0(out_dir,'ARR_refugia_absolute.png'),
       width = 1990,
       height = 1080,
       scale = 3,
       units = 'px')



plot.arr.01 <- ggplot() +
  geom_spatraster(data = out.crop, aes(fill = dominant_lh),
                  maxcell = 5e+08, na.rm = T) +
  # geom_spatraster(data = out$selection_filter, fill = 'darkgrey',
  #                 maxcell = 5e+06, na.rm = T) +
  geom_sf(data = world, fill = "#E5E5E5") +
  
  scale_fill_manual(values = c(lighten('#d55e00', 0.2),lighten('#019e73', 0.2),lighten('#0072b2', 0.2)), na.translate = F,
                    labels = c("Avoidance\n(competitive)", "Resistance\n(stress tolerant)", "Recovery\n(weedy)")) +
  # scale_fill_iriviridis_c(end = 0.9, reverse = T) +
  
  coord_sf(clip = 'on', expand = F) +
  
  labs(fill = "Coral refugia by life history\n(relative dominance)") +
  
  guides(
    fill = guide_legend(
      barheight = 1,
      barwidth = 10,
      # legend.text.position = "center",
      byrow  = TRUE,
      title.position = "top",
      theme = theme(legend.title = element_text(hjust = 0.5))
    ),
  ) +
  
  theme_bw(base_size = 24) +
  
  # legend along bottom
  theme(
    legend.position = "bottom",
    legend.box      = "horizontal",
    legend.title    = element_text(hjust = 0.5),
    legend.direction = "horizontal",
    legend.key.width = unit(dev.size()[1] / 4, "inches")
  ) +
  
  
  theme(panel.background = element_rect(fill =  "#CFD5D5"),
        panel.grid = element_blank())

plot.arr.01


ggsave(plot = plot.arr.01,
       filename = paste0(out_dir,'ARR_refugia_relative.png'),
       width = 1990,
       height = 1080,
       scale = 3,
       units = 'px')




design <- "
111111111
222222222
333333333
444444444
"


# Globals with inset 

final_fig <- {plot.coral_cover_present} / 
  {plot.coral_cover_future}  / 
  {plot.arr.01}  /
  {plot.pc_selected.global} /
  
  plot_layout(design = design)


final_fig

ggsave(plot = final_fig,
       filename = paste0(out_dir,'composite_maps.png'),
       width = 1990,
       height = 4360,
       scale = 3,
       units = 'px')





# inset examples ====

ecoregion_x <- data.ecoregions[x,]


bb <- st_bbox(ecoregion_x)

bb <- st_bbox(c(xmin = -5500000, 
                xmax = -6500000, 
                ymax = 0500000, 
                ymin = 0000000), 
              crs = target_crs) %>% st_as_sfc()

data_x <- data.selections %>% st_intersection(bb)
world_x <- world %>% st_intersection(bb)


plot.selections.inset <-  ggplot(data_x, aes(fill = selected)) +
  
  geom_sf(colour = NA) +
  
  # geom_sf_label(data = world_x, aes(label = admin),
  #              inherit.aes = F) +
  
  
  geom_sf(data = world_x, fill = "#AAAAAA", linewidth = 0.5) +
  
  scale_fill_manual(values = c('#EEEE00', '#880000'),
                    labels = c("Selected", "Not selected")) +
  
  coord_sf(clip = 'on', 
           expand = F) +
  
  guides(
    fill = guide_none(),
  ) +
  
  labs(x = NA, y = NA) +
  
  theme_minimal(base_size = 22) +
  
  theme(panel.grid.major.y = element_line(colour = "grey50"),
        
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal") +
  
  theme(panel.background = element_rect(fill = 'grey50'),
        panel.grid = element_blank())

plot.selections.inset




design <- "
1111111115
222222222#
333333333#
444444444#
"


# Globals with inset 

final_fig <- {plot.coral_cover_present + theme(legend.position = "none")} / 
  {plot.coral_deltas + theme(legend.position = "none")}  / 
  {plot.arr + theme(legend.position = "none")}  /
  {plot.pc_selected.global  + theme(legend.position = "none")} /
  
  plot.selections.inset +
  
  plot_layout(design = design,
              guides = "collect") &
  theme(legend.position = "bottom")


final_fig

