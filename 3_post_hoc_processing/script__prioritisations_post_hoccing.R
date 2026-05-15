# Bloomberg post hoccing 2

rm(list=ls())

library(tidyverse)
library(sf)

source("functions.R")


out_dir <- "output/life_history_predictions/"

# Load in all selections

data.selections <- read_rds("output/life_history_predictions/all_selections.rds")


data.selections <- data.selections %>% 
  mutate(uid = row_number())

# Load in Beyer BCUs

data.beyer <- st_read("Z:/offline_data/data/politics_and_governance/conservation_governance/refugia/coral/global/2018/expert_opinion/beyer_et_al/beyer_refugia_single_cells.shp")


# Join to Beyer BCUs

data.beyer <- data.beyer %>% filter(is_bcu == 'refugia') %>% select(geometry, is_bcu, BCU_nam)


data.beyer <- data.beyer %>% group_by(BCU_nam) %>% summarise()

data.beyer <- data.beyer %>% validate_and_fix_polygons()


# st_write(data.beyer, "BEYER_TESTING.shp")

data.both <- st_join(data.selections, data.beyer, join = st_intersects, left = TRUE)


data.both <- data.both %>% group_by(uid) %>% arrange(BCU_nam) %>% slice(1)


# st_write(data.both, "BEYER_AND_PLUS_JOIN_TESTING.shp")


# Load in EEZ shapefile

data.eez <- st_read("Z:/offline_data/data/politics_and_governance/jurisdictions/economic/exclusive_economic_zones/manually_defined_polygons/global/current/marine_regions.org/World_EEZ_v12_20231025/eez_v12.shp")

data.eez <- data.eez %>% validate_and_fix_polygons()

data.eez <- data.eez %>% select(geometry, territory = TERRITORY1, country = SOVEREIGN1)


# Join to Selections

data.both2 <- st_join(data.both, data.eez, join = st_intersects, left = TRUE)

data.both2 <- data.both2 %>% group_by(uid) %>% arrange(territory) %>% slice(1)

data.both2 <-   data.both2 %>% 
  mutate(effective_cover_km2 = effective_cover_present_m2_total_cover/1000000) %>% 
  
  mutate(is_bcu = ifelse(is.na(BCU_nam), 'non_bcu', 'is_bcu')) %>% 
  
  mutate(selected = ifelse(selected == T, 'in_portfolio', 'not_selected')) 




write_rds(data.both2, "Z:/Dropbox/Lenfest_kenya/data/PROJECT_SPECIFIC/bloomberg/life_history_predictions/erl_revision_2/all_selections_with_beyer_and_eez.rds")

# Export shapefiles by EEZ loop

eezs <- data.both2$territory %>% unique()

out_dir.eezs <- "Z:/Dropbox/Global Coral Refugia Life Histories/bloomberg_coral_sanctuaries_2024/data/prioritisations/by_eez/"

library(snakecase)

lapply(1:length(eezs), function(x) {
  
  eez_x <- eezs[x]
  
  data.eez_x <- data.both2 %>% 
    filter(territory == eez_x) %>% 
    select(territory, country, selected, geometry)
  
  dir.create(paste0(out_dir.eezs,to_snake_case(eez_x),'/'), recursive = T)
  
  st_write(data.eez_x, paste0(out_dir.eezs,to_snake_case(eez_x),"/",'prioritisations_',to_snake_case(eez_x),".shp"))
  
  
})



# Create tables


data.all.no_geom <- data.both2 %>% 
  st_drop_geometry()


# data.all.no_geom <- data.all.no_geom %>% 




# Table of selections by eez

table.selections_by_eez.effective_cover <- data.all.no_geom %>% 
  
  group_by(country, territory, selected) %>% 
  summarise(effective_cover_km2 = sum(effective_cover_km2) %>% round(0),
            reef_extent_km2 = sum(pixel_area/1000000) %>% round(0)) %>% 
  
  pivot_wider(id_cols = territory,
              names_from = selected,
              values_from = c("effective_cover_km2", "reef_extent_km2"),
              values_fill = 0) %>% 
  
  
  # mutate(not_selected = round(not_selected,0),
  #        in_portfolio = round(in_portfolio,0)) %>% 
  
  mutate(total_effective_cover = effective_cover_km2_not_selected + effective_cover_km2_in_portfolio,
         total_reef_extent = reef_extent_km2_not_selected + reef_extent_km2_in_portfolio) %>% 
  mutate(pc_selected.effective = ((effective_cover_km2_in_portfolio/total_effective_cover)*100) %>% round(0),
         pc_selected.reef_extent = ((reef_extent_km2_in_portfolio/total_reef_extent)*100) %>% round(0)) %>% 
  arrange(-total_reef_extent)


table.selections_by_eez.effective_cover.og <-  data.all.no_geom %>% 
  
  mutate(is_bcu = ifelse(is.na(BCU_nam), 'is_bcu', 'non_bcu')) %>% 
  
  group_by(territory, is_bcu) %>% 
  summarise(effective_cover_km2 = sum(effective_cover_km2) %>% round(0),
            reef_extent_km2 = sum(pixel_area/1000000) %>% round(0)) %>% 
  
  pivot_wider(id_cols = territory,
              names_from = is_bcu,
              values_from = c("effective_cover_km2", "reef_extent_km2"),
              values_fill = 0) %>% 
  
  
  # mutate(non_bcu = round(non_bcu,0),
  #        is_bcu = round(is_bcu,0)) %>% 
  
  mutate(total_effective_cover = effective_cover_km2_non_bcu + effective_cover_km2_is_bcu,
         total_reef_extent = reef_extent_km2_non_bcu + reef_extent_km2_is_bcu) %>% 
  mutate(pc_selected.effective = ((effective_cover_km2_is_bcu/total_effective_cover)*100) %>% round(0),
         pc_selected.reef_extent = ((reef_extent_km2_is_bcu/total_reef_extent)*100) %>% round(0)) %>% 
  arrange(-total_reef_extent)




table.selections_by_eez.effective_cover.both <-  data.all.no_geom %>% 
  
  
  group_by(territory, is_bcu, selected) %>% 
  summarise(effective_cover_km2 = sum(effective_cover_km2) %>% round(0),
            reef_extent_km2 = sum(pixel_area/1000000) %>% round(0)) %>% 
  
  mutate(combination = case_when(is_bcu == "is_bcu" & selected == "in_portfolio" ~ "both_methods",
                                 is_bcu == "non_bcu" & selected == "in_portfolio" ~ "50_plus_only",
                                 is_bcu == "is_bcu" & selected == "not_selected" ~ "50_og_only",
                                 is_bcu == "non_bcu" & selected == "not_selected" ~ "not_selected")) %>% 
  
  mutate(combination = paste0('effective_cover_km2.',combination)) %>% 
  
  pivot_wider(id_cols = territory,
              names_from = combination,
              values_from = effective_cover_km2,
              values_fill = 0) %>% 
  
  
  mutate(total_effective_cover = `effective_cover_km2.50_og_only` +  `effective_cover_km2.50_plus_only` + effective_cover_km2.not_selected + `effective_cover_km2.both_methods`) %>% 
  
  mutate(pc_selected.effective_cover_km2.og = ((`effective_cover_km2.50_og_only`/total_effective_cover)*100) %>% round(0),
         pc_selected.effective_cover_km2.plus = ((`effective_cover_km2.50_plus_only`/total_effective_cover)*100) %>% round(0),
         pc_selected.effective_cover_km2.both = ((`effective_cover_km2.both_methods`/total_effective_cover)*100) %>% round(0),
         pc_selected.effective_cover_km2.none = ((`effective_cover_km2.not_selected`/total_effective_cover)*100) %>% round(0)
  ) 

table.selections_by_eez.reef_extent.both <-  data.all.no_geom %>% 
  
  
  group_by(territory, is_bcu, selected) %>% 
  summarise(effective_cover_km2 = sum(effective_cover_km2) %>% round(0),
            reef_extent_km2 = sum(pixel_area/1000000) %>% round(0)) %>% 
  
  mutate(combination = case_when(is_bcu == "is_bcu" & selected == "in_portfolio" ~ "both_methods",
                                 is_bcu == "non_bcu" & selected == "in_portfolio" ~ "50_plus_only",
                                 is_bcu == "is_bcu" & selected == "not_selected" ~ "50_og_only",
                                 is_bcu == "non_bcu" & selected == "not_selected" ~ "not_selected")) %>% 
  
  mutate(combination = paste0('reef_extent_km2.',combination)) %>% 
  
  pivot_wider(id_cols = territory,
              names_from = combination,
              values_from = reef_extent_km2,
              values_fill = 0) %>% 
  
  
  mutate(total_reef_extent = `reef_extent_km2.50_og_only` +  `reef_extent_km2.50_plus_only` + reef_extent_km2.not_selected + `reef_extent_km2.both_methods`) %>% 
  
  mutate(pc_selected.reef_extent_km2.og = ((`reef_extent_km2.50_og_only`/total_reef_extent)*100) %>% round(0),
         pc_selected.reef_extent_km2.plus = ((`reef_extent_km2.50_plus_only`/total_reef_extent)*100) %>% round(0),
         pc_selected.reef_extent_km2.both = ((`reef_extent_km2.both_methods`/total_reef_extent)*100) %>% round(0),
         pc_selected.reef_extent_km2.none = ((`reef_extent_km2.not_selected`/total_reef_extent)*100) %>% round(0)
  ) 



table.selections_by_eez.both <- full_join(table.selections_by_eez.reef_extent.both,table.selections_by_eez.effective_cover.both) %>% 
  arrange(-total_reef_extent) %>% 
  left_join(data.eez %>% st_drop_geometry() %>% distinct()) %>% 
  relocate(country)



write_csv(table.selections_by_eez.both,
          file = paste0(out_dir,'selection_summary_by_eez_ssp585.csv'))





table.selections_by_eez.raw_area <- data.all.no_geom %>% 
  
  # mutate(selected = ifelse(selected == T, 'in_portfolio', 'not_selected')) %>% 
  
  group_by(territory, selected) %>% 
  summarise(area = sum(pixel_area) %>% round(0)) %>% 
  
  pivot_wider(id_cols = territory,
              names_from = selected,
              values_from = area,
              values_fill = 0) %>% 
  
  mutate(not_selected = round(not_selected,0),
         in_portfolio = round(in_portfolio,0)) %>% 
  
  mutate(total_area = not_selected + in_portfolio) %>% 
  mutate(pc_selected = ((in_portfolio/total_area)*100) %>% round(0)) %>% 
  arrange(-total_area)


write_csv(table.selections_by_eez.raw_area,
          file = paste0(out_dir,'selection_summary_by_eez_raw_area_ssp585.csv'))
