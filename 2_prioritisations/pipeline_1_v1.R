# =============================================================================
# CORAL LIFE HISTORY DATA PROCESSING PIPELINE - WITH SQUARE/HEX/GEOGRAPHIC GRID
# Process coral cover predictions with multiple grid options
# =============================================================================

# 1. INITIALIZATION AND CONFIGURATION

## Clear environment and load packages
rm(list = ls())
suppressPackageStartupMessages({
  library(terra)        # Spatial data processing
  library(data.table)   # Fast data manipulation
  library(sf)           # Simple features for spatial data
  library(dplyr)        # Data manipulation
  library(logger)       # Logging
  library(progressr)    # Progress tracking
  library(tictoc)       # Timing
  library(pryr)         # Memory monitoring
  library(future)       # Parallel backend
  library(parallelly)   # availableCores()
  library(future.apply) # Parallel lapply
})

sf::sf_use_s2(FALSE)

## Initialize logging (file + console)
log_dir <- "logs"
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
log_file <- file.path(log_dir, "analysis.log")
logger::log_layout(logger::layout_glue_colors)
logger::log_threshold(logger::INFO)
logger::log_appender(logger::appender_tee(log_file))  # console + file
logger::log_info("Script started at {Sys.time()}")

## Initialize progress handlers
handlers(global = TRUE)
handlers("progress", "rstudio")

## Initialize parallel processing (robust core detection)
available_cores <- tryCatch(future::availableCores(), error = function(e) 2L)
workers_to_use  <- max(1L, min(as.integer(available_cores) - 2L, 15L))
future::plan(multisession, workers = workers_to_use)
data.table::setDTthreads(workers_to_use)
logger::log_info("Parallel processing initialized with {workers_to_use} workers")

## USER CONFIGURATION - MODIFY THESE SETTINGS
PARENT_FOLDER <- "~/Dropbox/8-MQ_Shared/Lenfest_kenya/data/PROJECT_SPECIFIC/bloomberg/life_history_predictions/October_analysis/"
REGION_CHECKLIST <- list(
  "Africa-India"= TRUE,
  "Andaman-Nicobar Islands" = TRUE,   
  "Atlantic Caribbean" = TRUE,                  
  "Australian"   = TRUE,                          
  "Brazil"    = TRUE,    
  "Fiji-Caroline Islands"= TRUE,           
  "Hawaii-Line Islands" = TRUE,   
  "Indonesian"   = TRUE,                  
  "Japan-Vietnam"   = TRUE,                  
   "Pacific Caribbean"  = TRUE,                     
  "Persian Gulf"  = TRUE,                             
  "Polynesia"   = TRUE,      
  "Red Sea"   = TRUE,      
  "Tonga-Samoa"   = TRUE,    
  "West Africa"  = TRUE
)

## Core configuration
CONFIG <- list(
  folders = c("competitive_cover_pc", "coral_cover_pc","weedy_cover_pc","stress_tolerant_cover_pc"),
  scenarios = c("historical", "present", "future"),
  grid_type = "hexagonal",          # Options: "geographic", "hexagonal", "square"
  grid_res  = 0.04,              # geographic grid size (deg)
  
  # Keep hex config for back-compat (not used here); corrected to 25 km²
  hex_grid_area_m2  = 5e6,      # 25 km² hex area
  # Squares target ~5 km²: side length ≈ 2236 m (integer chosen for stability)
  square_grid_res_m = 2236L,     # side length (m) -> ~5 km² cells
  
  buffer_distance = 250,         # m (join robustness)
  export_comparison = FALSE,
  base_years = list(historical = 2000, present = 2020, future = 2050),
  n_models = 100L,
  pixel_area_m2 = 250*250,
  chunk_size = 1e6L,
  max_points_per_chunk = 5e5,
  cleanup_interval = 100000,
  # Strict-mode fallback control (used in process_region)
  allow_fallback = FALSE,        # FALSE = stop if hex/square fails; TRUE = fallback to geographic
  min_cells_hex  = 1L            # require at least this many aggregated cells to accept hex/square
)

## Back-compat
FOLDERS   <- CONFIG$folders
SCENARIOS <- CONFIG$scenarios
GRID_RES  <- CONFIG$grid_res

# 2. PROJECTION AND UTILITY FUNCTIONS 
                            
get_azimuthal <- function(lon_center, lat_center) {
  sprintf('+proj=laea +lat_0=%f +lon_0=%f +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs',
          lat_center, lon_center)
}

# Legacy helper kept for compatibility checks; no longer used at runtime
find_utm_zone <- function(sf_object) {
  sf_object <- sf::st_transform(sf_object, 3857) # project to avoid lon/lat centroid warnings
  centroid  <- sf::st_centroid(sf_object)
  ll        <- sf::st_coordinates(sf::st_transform(centroid, 4326))
  lon <- ll[1]; lat <- ll[2]
  utm_zone <- floor((lon + 180) / 6) + 1
  hemisphere <- ifelse(lat >= 0, "+north", "+south")
  paste0("+proj=utm +zone=", utm_zone, " ", hemisphere, " +datum=WGS84 +units=m +no_defs")
}

# Shared LAEA CRS centered on region bbox (IDL-safe); used by both hex & square
compute_laea_crs <- function(region_sf) {
  bb <- sf::st_bbox(region_sf)
  lon <- c(as.numeric(bb["xmin"]), as.numeric(bb["xmax"]))
  lat <- c(as.numeric(bb["ymin"]), as.numeric(bb["ymax"]))
  lon_rad <- lon * pi/180
  cx <- atan2(mean(sin(lon_rad)), mean(cos(lon_rad))) * 180/pi
  if (cx > 180)  cx <- cx - 360
  if (cx < -180) cx <- cx + 360
  cy <- mean(lat)
  get_azimuthal(cx, cy)
}

force_sf_with_crs <- function(x, crs) {
  if (inherits(x, "sf")) {
    if (is.na(sf::st_crs(x))) sf::st_crs(x) <- sf::st_crs(crs)
    return(x)
  }
  if (!"geometry" %in% names(x)) stop("Object lacks 'geometry' column")
  sfx <- sf::st_sf(x)
  sf::st_crs(sfx) <- sf::st_crs(crs)
  sfx
}

# 3. CORE DATA PROCESSING FUNCTIONS 

assign_ecoregions <- function(grid_dt) {
  stopifnot(all(c("grid_lon","grid_lat") %in% names(grid_dt)))
  logger::log_info("Assigning ECOREGION (IDL-safe): within → intersects → nearest (1:1)")
  
  # Unique points in WGS84
  pts_dt <- unique(grid_dt[, .(grid_lon, grid_lat)])
  pts_sf <- sf::st_as_sf(pts_dt, coords = c("grid_lon","grid_lat"), crs = 4326, remove = FALSE)
  
  # Load MEOW polygons
  shp <- "~/Dropbox/8-MQ_Shared/Lenfest_kenya/data/PROJECT_SPECIFIC/bloomberg/life_history_predictions/pre_webinar_20250313/marine_ecoregions_of_the_world_provinces/meow_ecos_provinces.shp"
  if (!file.exists(shp)) {
    logger::log_warn("MEOW shapefile not found: {shp} — ECOREGION will be NA.")
    return(data.table(grid_lon = pts_dt$grid_lon, grid_lat = pts_dt$grid_lat, ECOREGION = NA_character_))
  }
  poly <- tryCatch(sf::st_read(shp, quiet = TRUE), error = function(e) NULL)
  if (is.null(poly) || nrow(poly) == 0) {
    logger::log_warn("Failed to read MEOW shapefile (empty). ECOREGION will be NA.")
    return(data.table(grid_lon = pts_dt$grid_lon, grid_lat = pts_dt$grid_lat, ECOREGION = NA_character_))
  }
  
  # Choose a province/name attribute (unchanged output name ECOREGION)
  pick_attr <- function(x) {
    nms <- names(x); score <- rep(0L, length(nms))
    score <- score + grepl("(?i)prov(ince|inc|_name)?$", nms) * 5L
    score <- score + grepl("(?i)eco(reg|region|_name)?$", nms) * 4L
    score <- score + grepl("(?i)name(_1)?$", nms) * 3L
    score[nms %in% attr(x, "sf_column")] <- -Inf
    nms[order(score, nchar(nms), decreasing = TRUE)[1]]
  }
  eco_attr <- pick_attr(poly)
  logger::log_info("Using MEOW attribute column: {eco_attr}")
  
  # Local LAEA (IDL-safe)
  bb <- sf::st_bbox(pts_sf)
  cx <- as.numeric((bb["xmin"] + bb["xmax"])/2)
  cy <- as.numeric((bb["ymin"] + bb["ymax"])/2)
  laea <- sprintf('+proj=laea +lat_0=%f +lon_0=%f +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs', cy, cx)
  
  pts_laea  <- sf::st_transform(pts_sf, laea)
  
  # No dissolve — just clean, keep all parts
  poly_laea <- sf::st_transform(poly, laea)
  poly_laea <- sf::st_make_valid(poly_laea)
  suppressWarnings(poly_laea <- sf::st_buffer(poly_laea, 0))   # topological clean only
  poly_laea <- poly_laea[!sf::st_is_empty(poly_laea), ]        # drop empties if any
  
  # Per-feature area (used for the deterministic “largest polygon” tie-break)
  areas <- as.numeric(sf::st_area(poly_laea))
  
  
  n <- nrow(pts_laea)
  eco <- rep(NA_character_, n)
  
  # Pass 1: WITHIN (choose largest polygon if multiple)
  w <- sf::st_within(pts_laea, poly_laea)
  has_w <- lengths(w) > 0
  if (any(has_w)) {
    pick_w <- vapply(seq_len(n), function(i){
      ids <- w[[i]]; if (length(ids)) ids[which.max(areas[ids])] else NA_integer_
    }, integer(1))
    eco[has_w] <- as.character(poly_laea[[eco_attr]][pick_w[has_w]])
  }
  logger::log_info("ECOREGION matched by WITHIN: {sum(has_w)}/{n}")
  
  # Pass 2: INTERSECTS (largest polygon)
  miss <- which(is.na(eco))
  if (length(miss)) {
    itx <- sf::st_intersects(pts_laea[miss,], poly_laea)
    has_i <- lengths(itx) > 0
    if (any(has_i)) {
      pick_i <- vapply(seq_along(miss), function(k){
        ids <- itx[[k]]; if (length(ids)) ids[which.max(areas[ids])] else NA_integer_
      }, integer(1))
      eco[miss[has_i]] <- as.character(poly_laea[[eco_attr]][pick_i[has_i]])
      logger::log_info("ECOREGION matched by INTERSECTS: +{sum(has_i)} (cum {sum(!is.na(eco))})")
    }
  }
  
  # Pass 3: NEAREST
  if (anyNA(eco)) {
    miss <- which(is.na(eco))
    nn <- sf::st_nearest_feature(pts_laea[miss,], poly_laea)
    eco[miss] <- as.character(poly_laea[[eco_attr]][nn])
    logger::log_info("ECOREGION assigned by NEAREST: +{length(miss)} (cum {sum(!is.na(eco))})")
  }
  
  out <- data.table(grid_lon = pts_dt$grid_lon, grid_lat = pts_dt$grid_lat, ECOREGION = eco)
  # Hard 1:1 guarantee (one row per lon/lat)
  out <- unique(out, by = c("grid_lon","grid_lat"))
  if (anyNA(out$ECOREGION)) {
    ex <- head(out[is.na(ECOREGION)], 5)
    logger::log_warn("ECOREGION remained NA for {sum(is.na(out$ECOREGION))} cells; e.g. {paste(sprintf('(%.3f, %.3f)', ex$grid_lon, ex$grid_lat), collapse='; ')}")
  } else {
    logger::log_info("ECOREGION assignment complete: 0 NA")
  }
  out
}



process_geographic_grid <- function(all_data) {
  logger::log_info("Calculating grid coordinates for {GRID_RES}° resolution...")
  all_data[, `:=`(
    grid_lon = round(as.numeric(pu_lon) / GRID_RES, 0) * GRID_RES,
    grid_lat = round(as.numeric(pu_lat) / GRID_RES, 0) * GRID_RES
  )]
  logger::log_info("Aggregating data to {GRID_RES}° grid resolution...")
  
  grid_data <- all_data[, .(
    effective_cover_historical_m2 = sum(effective_cover_m2[scenario == "historical"], na.rm = TRUE),
    effective_cover_present_m2    = sum(effective_cover_m2[scenario == "present"],    na.rm = TRUE),
    effective_cover_future_m2     = sum(effective_cover_m2[scenario == "future"],     na.rm = TRUE),
    sd_historical_m2 = sqrt(sum(effective_sd_m2[scenario == "historical"]^2, na.rm = TRUE)),
    sd_present_m2    = sqrt(sum(effective_sd_m2[scenario == "present"]^2,    na.rm = TRUE)),
    sd_future_m2     = sqrt(sum(effective_sd_m2[scenario == "future"]^2,     na.rm = TRUE)),
    n_pixels_present = sum(scenario == "present"),
    avg_cover_per_pixel_historical = mean(effective_cover_m2[scenario == "historical"], na.rm = TRUE),
    avg_cover_per_pixel_present    = mean(effective_cover_m2[scenario == "present"],    na.rm = TRUE),
    avg_cover_per_pixel_future     = mean(effective_cover_m2[scenario == "future"],     na.rm = TRUE)
  ), by = .(grid_lon, grid_lat, life_history)]
  
  grid_data[, `:=`(
    effective_cover_historical_m2 = pmax(0, effective_cover_historical_m2),
    effective_cover_present_m2    = pmax(0, effective_cover_present_m2),
    effective_cover_future_m2     = pmax(0, effective_cover_future_m2)
  )]
  grid_data[, extent_area_present_m2 := n_pixels_present * CONFIG$pixel_area_m2]
  logger::log_info("Geographic grid aggregation complete: {formatC(nrow(grid_data), big.mark=',')} grid rows")
  grid_data
}

load_lifehistory_data <- function(folder, scenario, region_path) {
  if (scenario %in% c("historical", "present")) {
    file_path <- file.path(region_path, folder, "predictions_present.csv")
  } else if (scenario == "future") {
    file_path <- file.path(region_path, folder, "predictions_future.csv")
  } else {
    logger::log_error("Unknown scenario: {scenario}")
    return(NULL)
  }
  if (!file.exists(file_path)) {
    logger::log_warn("File not found: {file_path}")
    return(NULL)
  }
  tryCatch({
    dt <- fread(file_path, select = c("pu_lon", "pu_lat", "time",
                                      paste0(folder, ".predicted.mean"),
                                      paste0(folder, ".predicted.sd")))
    base_year <- CONFIG$base_years[[scenario]]
    dt <- dt[time == base_year]
    mean_col <- paste0(folder, ".predicted.mean")
    sd_col   <- paste0(folder, ".predicted.sd")
    out <- dt[, .(
      pu_lon = as.numeric(pu_lon),
      pu_lat = as.numeric(pu_lat),
      predicted_mean = pmin(pmax(as.numeric(get(mean_col)), 0), 1),
      predicted_sd   = pmax(as.numeric(get(sd_col)), 0)
    )]
    out[, `:=`(
      life_history = folder,
      scenario     = scenario,
      n_models     = CONFIG$n_models,
      effective_cover_m2 = predicted_mean * CONFIG$pixel_area_m2,
      effective_sd_m2    = predicted_sd   * CONFIG$pixel_area_m2
    )]
    logger::log_info("Processed {file_path} for {scenario}: {nrow(out)} rows")
    out
  }, error = function(e) {
    logger::log_error("Error loading {file_path}: {e$message}")
    NULL
  })
}

# 4. GRID CREATION FUNCTIONS --------------------------------------------------

create_hexagonal_grid <- function(region_bbox, cell_area_m2 = 1e6) {
  tic("Hexagonal grid creation")
  region_sf <- st_as_sf(st_as_sfc(st_bbox(c(
    xmin = region_bbox$min_lon, ymin = region_bbox$min_lat,
    xmax = region_bbox$max_lon, ymax = region_bbox$max_lat
  ), crs = 4326)))
  
  # Local equal-area 
  laea_crs  <- compute_laea_crs(region_sf)
  region_prj <- st_transform(region_sf, laea_crs)
  side_length <- sqrt((2 * cell_area_m2) / (3 * sqrt(3)))
  logger::log_info("Creating hexagonal grid with side length: {round(side_length, 2)} m")
  
  hex_grid_raw <- st_make_grid(region_prj, cellsize = side_length, square = FALSE, what = "polygons")
  hex_sf_raw   <- st_sf(geometry = hex_grid_raw)
  area_raw     <- as.numeric(st_area(hex_sf_raw))        # <-- area BEFORE buffer
  
  # buffered geometry ONLY for robust joins
  hex_sf_join  <- st_buffer(hex_sf_raw, CONFIG$buffer_distance)
  
  # centroids from RAW cells for stable WGS84 ids
  cent_wgs84 <- st_transform(st_centroid(hex_sf_raw), 4326)
  coords     <- st_coordinates(cent_wgs84)
  
  hex_dt <- data.table(
    grid_id     = seq_len(nrow(hex_sf_raw)),
    grid_lon    = coords[, 1],
    grid_lat    = coords[, 2],
    grid_area_m2= area_raw,                             # <-- keep correct area
    geometry    = st_geometry(hex_sf_join)              # <-- use buffered geom for joins
  )
  
  t <- toc(quiet = TRUE)
  logger::log_info("Hexagonal grid created: {nrow(hex_dt)} hexagons in {round(t$toc - t$tic, 2)} s")
  list(grid = hex_dt, crs = laea_crs)
}

# LAEA-based squares (no UTM), area ~ (cell_size_m)^2, default from CONFIG
create_square_grid <- function(region_bbox, cell_size_m = CONFIG$square_grid_res_m) {
  tic("Square grid creation")
  logger::log_info("Creating square grid (LAEA) with side length {cell_size_m} m (~{round((cell_size_m^2)/1e6, 3)} km²)...")
  
  region_sf <- st_as_sf(st_as_sfc(st_bbox(c(
    xmin = region_bbox$min_lon, ymin = region_bbox$min_lat,
    xmax = region_bbox$max_lon, ymax = region_bbox$max_lat
  ), crs = 4326)))
  
  laea_crs    <- compute_laea_crs(region_sf)
  region_prj  <- st_transform(region_sf, laea_crs)
  
  square_grid_raw <- st_make_grid(region_prj, cellsize = cell_size_m, square = TRUE, what = "polygons")
  square_sf_raw   <- st_sf(geometry = square_grid_raw)
  area_raw        <- as.numeric(st_area(square_sf_raw))  # equal-area (LAEA)
  square_sf_join  <- st_buffer(square_sf_raw, CONFIG$buffer_distance)
  
  cent_wgs84 <- st_transform(st_centroid(square_sf_raw), 4326)
  coords     <- st_coordinates(cent_wgs84)
  
  square_dt <- data.table(
    grid_id      = seq_len(nrow(square_sf_raw)),
    grid_lon     = coords[, 1],
    grid_lat     = coords[, 2],
    grid_area_m2 = area_raw,                             # <-- correct area
    geometry     = st_geometry(square_sf_join)           # <-- buffered geom for joins
  )
  
  t <- toc(quiet = TRUE)
  logger::log_info("Square grid created: {nrow(square_dt)} cells in {round(t$toc - t$tic, 2)} s")
  list(grid = square_dt, crs = laea_crs)
}


# ---- FAST SQUARE BINNING (no full grid build, no spatial joins) --------------

# Build occupied-cell polygons from LAEA centers (vectorized)
cells_to_polygons <- function(cells_dt, cell_size_m, crs) {
  half <- cell_size_m / 2
  # Build rectangles from centers in LAEA
  x1 <- cells_dt$cx - half; x2 <- cells_dt$cx + half
  y1 <- cells_dt$cy - half; y2 <- cells_dt$cy + half
  # 5-point ring per row
  rings <- Map(function(a,b,c,d) matrix(c(a,c,  b,c,  b,d,  a,d,  a,c), ncol = 2, byrow = TRUE),
               x1, x2, y1, y2)
  sfc <- sf::st_sfc(lapply(rings, function(m) sf::st_polygon(list(m))), crs = crs)
  sf::st_sf(
    grid_id      = seq_len(nrow(cells_dt)),
    grid_lon     = cells_dt$grid_lon,
    grid_lat     = cells_dt$grid_lat,
    grid_area_m2 = rep.int(cell_size_m^2, nrow(cells_dt)),
    geometry     = sfc
  )
}

# Aggregate to square grid via integer binning in LAEA (blazing fast)
aggregate_square_by_binning <- function(all_data, cell_size_m, region_bbox) {
  # Region bbox -> WGS84 polygon -> LAEA CRS
  region_sf <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(
    xmin = region_bbox$min_lon, ymin = region_bbox$min_lat,
    xmax = region_bbox$max_lon, ymax = region_bbox$max_lat
  ), crs = 4326)))
  laea_crs <- compute_laea_crs(region_sf)
  
  # Project all PU coordinates with PROJ directly (no sf geometry overhead)
  pts_wgs <- cbind(as.numeric(all_data$pu_lon), as.numeric(all_data$pu_lat))
  pts_laea <- sf::sf_project(from = sf::st_crs(4326), to = sf::st_crs(laea_crs), pts = pts_wgs)
  x <- pts_laea[,1]; y <- pts_laea[,2]
  
  # Grid origin: LAEA bbox min corner (matches st_make_grid default behavior)
  bbp <- sf::st_bbox(sf::st_transform(region_sf, laea_crs))
  x0 <- as.numeric(bbp["xmin"]); y0 <- as.numeric(bbp["ymin"])
  
  # Integer binning -> grid indices
  ix <- floor((x - x0) / cell_size_m)
  iy <- floor((y - y0) / cell_size_m)
  
  # Attach grid indices & a stable (temporary) id
  all_data[, `:=`(ix = as.integer(ix), iy = as.integer(iy))]
  all_data[, grid_id := sprintf("px_%d_%d", ix, iy)]   # remapped to px_lon_lat later
  
  # Compute LAEA centers per occupied cell (unique ix/iy), then back to lon/lat
  cells <- unique(all_data[, .(ix, iy)], by = c("ix","iy"))
  cells[, `:=`(
    cx = x0 + (ix + 0.5)*cell_size_m,
    cy = y0 + (iy + 0.5)*cell_size_m
  )]
  centers_ll <- sf::sf_project(from = sf::st_crs(laea_crs), to = sf::st_crs(4326), pts = as.matrix(cells[, .(cx, cy)]))
  cells[, `:=`(grid_lon = centers_ll[,1], grid_lat = centers_ll[,2])]
  
  # Join centers/areas back (vectorized, no polygons needed for aggregation)
  all_data <- merge(
    all_data,
    cells[, .(ix, iy, grid_lon, grid_lat)],
    by = c("ix","iy"),
    all.x = TRUE,
    sort = FALSE
  )
  all_data[, grid_area_m2 := cell_size_m^2]  # constant in LAEA
  
  # ---- Aggregate exactly like your previous path
  grid_data <- all_data[, .(
    effective_cover_historical_m2 = sum(effective_cover_m2[scenario == "historical"], na.rm = TRUE),
    effective_cover_present_m2    = sum(effective_cover_m2[scenario == "present"],    na.rm = TRUE),
    effective_cover_future_m2     = sum(effective_cover_m2[scenario == "future"],     na.rm = TRUE),
    
    sd_historical_m2 = sqrt(sum(effective_sd_m2[scenario == "historical"]^2, na.rm = TRUE)),
    sd_present_m2    = sqrt(sum(effective_sd_m2[scenario == "present"]^2,    na.rm = TRUE)),
    sd_future_m2     = sqrt(sum(effective_sd_m2[scenario == "future"]^2,     na.rm = TRUE)),
    
    n_pixels_present = sum(scenario == "present"),
    avg_cover_per_pixel_historical = mean(effective_cover_m2[scenario == "historical"], na.rm = TRUE),
    avg_cover_per_pixel_present    = mean(effective_cover_m2[scenario == "present"],    na.rm = TRUE),
    avg_cover_per_pixel_future     = mean(effective_cover_m2[scenario == "future"],     na.rm = TRUE)
  ), by = .(grid_id, grid_lon, grid_lat, grid_area_m2, life_history)]
  
  grid_data[, `:=`(
    effective_cover_historical_m2 = pmax(0, effective_cover_historical_m2),
    effective_cover_present_m2    = pmax(0, effective_cover_present_m2),
    effective_cover_future_m2     = pmax(0, effective_cover_future_m2),
    extent_area_present_m2        = n_pixels_present * CONFIG$pixel_area_m2
  )]
  
  # Return also the minimal metadata needed to build polygons for artifacts
  list(
    grid_data  = grid_data,
    cells_meta = cells[, .(grid_id_tmp = sprintf("px_%d_%d", ix, iy),
                           grid_lon, grid_lat, cx, cy)],
    crs        = laea_crs,
    cell_size  = cell_size_m
  )
}





# 5. OPTIMIZED AGGREGATION FUNCTIONS ------------------------------------------

aggregate_to_grid_fast <- function(point_data, grid_info) {
  tic("Grid aggregation")
  logger::log_info("Starting fast grid aggregation (keyed join; no in-place sf writes)...")
  
  # -- inputs --
  grid_sf <- force_sf_with_crs(grid_info$grid, grid_info$crs)
  suppressWarnings(sf::st_agr(grid_sf) <- "constant")
  
  total_points <- nrow(point_data)
  chunk_size   <- CONFIG$max_points_per_chunk
  chunks       <- seq(1, total_points, by = chunk_size)
  logger::log_info("Processing {total_points} points in {length(chunks)} chunks")
  
  results <- vector("list", length(chunks))
  
  for (i in seq_along(chunks)) {
    chunk_start <- chunks[i]
    chunk_end   <- min(chunk_start + chunk_size - 1L, total_points)
    logger::log_info("Processing chunk {i}/{length(chunks)}: points {chunk_start} to {chunk_end}")
    
    # Original attributes for aggregation
    pt_attr <- as.data.table(point_data[chunk_start:chunk_end])
    pt_attr[, idx := .I]                      # stable key within chunk
    
    # Build sf points for spatial join
    points_sf <- sf::st_as_sf(pt_attr, coords = c("pu_lon", "pu_lat"), crs = 4326)
    points_pr <- sf::st_transform(points_sf, sf::st_crs(grid_sf))
    points_pr$idx <- pt_attr$idx              # preserve idx through joins
    
    # Pass 1: within
    j_within <- sf::st_join(points_pr, grid_sf, join = sf::st_within, left = TRUE)
    
    # Pass 2: intersects for the misses
    miss <- is.na(j_within$grid_id)
    if (any(miss)) {
      j_inter <- sf::st_join(points_pr[miss, ], grid_sf, join = sf::st_intersects, left = TRUE)
    } else {
      j_inter <- NULL
    }
    
    # Resolve assignments by idx (prefer "within", then "intersects")
    within_dt <- as.data.table(sf::st_drop_geometry(j_within))[
      , .(idx, grid_id, grid_lon, grid_lat, grid_area_m2)
    ]
    
    inter_dt <- if (!is.null(j_inter)) {
      as.data.table(sf::st_drop_geometry(j_inter))[
        , .(idx, grid_id, grid_lon, grid_lat, grid_area_m2)
      ]
    } else data.table(idx = integer(), grid_id = NA_character_,
                      grid_lon = numeric(), grid_lat = numeric(), grid_area_m2 = numeric())
    
    assigned <- rbindlist(list(within_dt, inter_dt), use.names = TRUE, fill = TRUE)
    assigned <- assigned[!is.na(grid_id)][, .SD[1], by = idx]  # first non-NA per point (within has priority)
    
    # Join back to attributes; drop unassigned points
    points_dt <- merge(pt_attr, assigned, by = "idx", all.x = TRUE, sort = FALSE)
    points_dt <- points_dt[!is.na(grid_id)]
    
    # Aggregate
    if (nrow(points_dt) > 0L) {
      results[[i]] <- points_dt[, .(
        effective_cover_historical_m2 = sum(effective_cover_m2[scenario == "historical"], na.rm = TRUE),
        effective_cover_present_m2    = sum(effective_cover_m2[scenario == "present"],    na.rm = TRUE),
        effective_cover_future_m2     = sum(effective_cover_m2[scenario == "future"],     na.rm = TRUE),
        
        sd_historical_m2 = sqrt(sum(effective_sd_m2[scenario == "historical"]^2, na.rm = TRUE)),
        sd_present_m2    = sqrt(sum(effective_sd_m2[scenario == "present"]^2,    na.rm = TRUE)),
        sd_future_m2     = sqrt(sum(effective_sd_m2[scenario == "future"]^2,     na.rm = TRUE)),
        
        n_pixels_present = sum(scenario == "present"),
        avg_cover_per_pixel_historical = mean(effective_cover_m2[scenario == "historical"], na.rm = TRUE),
        avg_cover_per_pixel_present    = mean(effective_cover_m2[scenario == "present"],    na.rm = TRUE),
        avg_cover_per_pixel_future     = mean(effective_cover_m2[scenario == "future"],     na.rm = TRUE)
      ), by = .(grid_id, grid_lon, grid_lat, grid_area_m2, life_history)]
    } else {
      results[[i]] <- NULL
    }
    
    # cleanup
    rm(pt_attr, points_sf, points_pr, j_within, j_inter, within_dt, inter_dt, assigned, points_dt)
    if (i %% 2 == 0) gc()
    logger::log_info("Chunk {i} completed")
  }
  
  results <- Filter(Negate(is.null), results)
  if (!length(results)) {
    logger::log_warn("No data could be aggregated to grid")
    return(NULL)
  }
  
  grid_data <- rbindlist(results)
  
  # collapse over chunks
  grid_data <- grid_data[, .(
    effective_cover_historical_m2 = sum(effective_cover_historical_m2, na.rm = TRUE),
    effective_cover_present_m2    = sum(effective_cover_present_m2,    na.rm = TRUE),
    effective_cover_future_m2     = sum(effective_cover_future_m2,     na.rm = TRUE),
    
    sd_historical_m2 = sqrt(sum(sd_historical_m2^2, na.rm = TRUE)),
    sd_present_m2    = sqrt(sum(sd_present_m2^2,    na.rm = TRUE)),
    sd_future_m2     = sqrt(sum(sd_future_m2^2,     na.rm = TRUE)),
    
    n_pixels_present = sum(n_pixels_present, na.rm = TRUE),
    avg_cover_per_pixel_historical = mean(avg_cover_per_pixel_historical, na.rm = TRUE),
    avg_cover_per_pixel_present    = mean(avg_cover_per_pixel_present,    na.rm = TRUE),
    avg_cover_per_pixel_future     = mean(avg_cover_per_pixel_future,     na.rm = TRUE)
  ), by = .(grid_id, grid_lon, grid_lat, grid_area_m2, life_history)]
  
  # clamp, extent, finish
  grid_data[, `:=`(
    effective_cover_historical_m2 = pmax(0, effective_cover_historical_m2),
    effective_cover_present_m2    = pmax(0, effective_cover_present_m2),
    effective_cover_future_m2     = pmax(0, effective_cover_future_m2)
  )]
  grid_data[, extent_area_present_m2 := n_pixels_present * CONFIG$pixel_area_m2]
  
  time_taken <- toc(quiet = TRUE)
  logger::log_info("Grid aggregation completed: {nrow(grid_data)} cells in {round(time_taken$toc - time_taken$tic, 2)}s")
  grid_data
}

# 6. MAIN PROCESSING FUNCTION -------------------------------------------------

process_region <- function(region_name, region_path) {
  logger::log_info("=== PROCESSING REGION: {region_name} ===")
  logger::log_info("Region path: {region_path}")
  logger::log_info("Grid type: {CONFIG$grid_type}")
  setwd(region_path)
  tic(region_name)
  
  with_progress({
    # progress steps (exact count; no warnings)
    steps <- c(
      "Loading data","Combining data","Creating spatial grid",
      if (CONFIG$grid_type %in% c("square","hexagonal")) c("Building grid","Aggregating to grid") else "Using geographic grid",
      "Assigning ecoregions","Calculating metrics","Standardizing grid outputs","Final validation","Exporting results"
    )
    steps <- unlist(steps, use.names = FALSE)
    p <- progressor(along = steps)
    .step_i <- 0L
    next_step <- function() { .step_i <<- .step_i + 1L; p(steps[.step_i]) }
    
    
    # 6.1 DATA LOADING
    next_step()
    combinations <- expand.grid(folder = FOLDERS, scenario = SCENARIOS, stringsAsFactors = FALSE)
    all_data_list <- future_lapply(seq_len(nrow(combinations)), function(i) {
      load_lifehistory_data(combinations$folder[i], combinations$scenario[i], region_path)
    }, future.seed = TRUE)
    all_data_list <- all_data_list[!sapply(all_data_list, is.null)]
    if (!length(all_data_list)) {
      logger::log_error("No data loaded for region {region_name}. Skipping.")
      return(NULL)
    }
    
    next_step()
    all_data <- rbindlist(all_data_list, fill = TRUE)
    mem_usage <- pryr::mem_used()
    logger::log_info("Memory usage after data loading: {round(mem_usage/1024^3, 2)} GB")
    scenario_counts <- all_data[, .N, by = scenario]
    logger::log_info("Data loaded by scenario: {paste(paste(scenario_counts$scenario, scenario_counts$N, sep='='), collapse=', ')}")
    gc()
    logger::log_info("Total data loaded: {formatC(nrow(all_data), big.mark=',')} rows")
    
    # 6.2 CLEANING
    all_data <- as.data.table(all_data)
    all_data[, life_history := sub("_cover_pc$", "", life_history)]
    all_data[, life_history := sub("coral", "total_cover", life_history)]
    
    # 6.3 GRID
    next_step()
    region_bbox <- all_data[, .(min_lon = min(pu_lon), max_lon = max(pu_lon),
                                min_lat = min(pu_lat), max_lat = max(pu_lat))]
    grid_data <- NULL; grid_info <- NULL
    used_grid_type <- CONFIG$grid_type
    
    if (CONFIG$grid_type == "square") {
      next_step(); logger::log_info("Using SQUARE grid (fast binning)...")
      res <- aggregate_square_by_binning(all_data, CONFIG$square_grid_res_m, region_bbox)
      next_step(); grid_data <- res$grid_data
      
      # Build polygons ONLY for occupied cells (for artifacts/standardization)
      cells_dt <- res$cells_meta
      grid_polys_sf <- cells_to_polygons(
        cells_dt[, .(grid_lon, grid_lat, cx, cy)],
        res$cell_size,
        res$crs
      )
      # Keep interface compatible with downstream code
      grid_info <- list(grid = grid_polys_sf, crs = res$crs)
      
    } else if (CONFIG$grid_type == "hexagonal") {
      next_step(); logger::log_info("Using HEXAGONAL grid...")
      grid_info <- create_hexagonal_grid(region_bbox, CONFIG$hex_grid_area_m2)
      next_step(); grid_data <- aggregate_to_grid_fast(all_data, grid_info)
      
    } else if (CONFIG$grid_type == "geographic") {
      next_step(); logger::log_info("Using GEOGRAPHIC grid...")
      grid_data <- process_geographic_grid(all_data)
      used_grid_type <- "geographic"
    }
    
    
    # ── STRICT-MODE CHECK: prevent silent fallback ────────────────────────────
    if (CONFIG$grid_type %in% c("square","hexagonal")) {
      if (is.null(grid_data) || nrow(grid_data) < CONFIG$min_cells_hex) {
        msg <- sprintf(
          "Aggregation to %s grid produced no valid cells (n=%s). Refusing fallback (allow_fallback=%s).",
          CONFIG$grid_type,
          ifelse(is.null(grid_data), "NULL", nrow(grid_data)),
          CONFIG$allow_fallback
        )
        if (isFALSE(CONFIG$allow_fallback)) {
          logger::log_error(msg)
          stop(msg)
        } else {
          logger::log_warn(msg)
          logger::log_warn("Falling back to GEOGRAPHIC grid...")
          grid_data <- process_geographic_grid(all_data)
          used_grid_type <- "geographic"
        }
      }
    }
    
    # 6.4 ECOREGIONS
    next_step()
    grid_points_with_eco <- assign_ecoregions(grid_data)
    
    # (Optional) belt-and-braces: ensure 1 row per lon/lat in ECO table
    grid_points_with_eco <- unique(grid_points_with_eco, by = c("grid_lon","grid_lat"))
    
    # NEW (safe and fast update-join; no schema change)
    data.table::setkeyv(grid_data, c("grid_lon","grid_lat"))
    data.table::setkeyv(grid_points_with_eco, c("grid_lon","grid_lat"))
    grid_data[grid_points_with_eco,
              ECOREGION := i.ECOREGION,
              on = .(grid_lon, grid_lat),
              mult = "first"]
    
    # Optional sanity: warn if ECO table still had dup lon/lat
    dup_check <- grid_points_with_eco[, .N, by = .(grid_lon, grid_lat)][N > 1L]
    if (nrow(dup_check)) {
      logger::log_warn("ECOREGION table had {nrow(dup_check)} duplicate lon/lat; used mult='first' deterministically.")
    }
    
    # Restore your original key used downstream
    setkeyv(grid_data, c("grid_lon", "grid_lat", "life_history"))
    
    # Optional: confirm no NA left in ECOREGION
    na_eco <- grid_data[is.na(ECOREGION), .N]
    if (na_eco > 0L) logger::log_warn("{na_eco} rows still have NA ECOREGION after assignment.")
    
    
    # --- Propagate ECOREGION to all life-history rows at each lon/lat ---
    grid_data[, ECOREGION := {
      v <- as.character(ECOREGION)
      fill <- if (all(is.na(v))) NA_character_ else v[which(!is.na(v))[1L]]
      rep(fill, .N)
    }, by = .(grid_lon, grid_lat)]
    
    # optional: report if anything is still NA (e.g., missing MEOW file)
    na_left <- grid_data[is.na(ECOREGION), .N]
    if (na_left > 0L) logger::log_warn("{na_left} rows still have NA ECOREGION after group fill.")
    
    
    
    # 6.5 METRICS
    next_step()
    if (!"grid_area_m2" %in% names(grid_data)) {
      grid_data[, grid_area_m2 := ((111320 * cos(grid_lat * pi/180) * GRID_RES) * (110574 * GRID_RES))]
    }
    grid_data[, `:=`(
      cover_density_historical = effective_cover_historical_m2 / grid_area_m2,
      cover_density_present    = effective_cover_present_m2    / grid_area_m2,
      cover_density_future     = effective_cover_future_m2     / grid_area_m2,
      absolute_change_m2 = effective_cover_future_m2 - effective_cover_present_m2,
      relative_change_pct = ifelse(effective_cover_present_m2 > 0,
                                   (effective_cover_future_m2 - effective_cover_present_m2) / effective_cover_present_m2 * 100, NA_real_),
      absolute_change_historical_present_m2 = effective_cover_present_m2 - effective_cover_historical_m2,
      relative_change_historical_present_pct = ifelse(effective_cover_historical_m2 > 0,
                                                      (effective_cover_present_m2 - effective_cover_historical_m2) / effective_cover_historical_m2 * 100, NA_real_),
      absolute_change_historical_future_m2 = effective_cover_future_m2 - effective_cover_historical_m2,
      relative_change_historical_future_pct = ifelse(effective_cover_historical_m2 > 0,
                                                     (effective_cover_future_m2 - effective_cover_historical_m2) / effective_cover_historical_m2 * 100, NA_real_)
    )]
    implausible_density <- grid_data[cover_density_historical > 1 | cover_density_present > 1 | cover_density_future > 1, .N]
    if (implausible_density > 0) {
      logger::log_warn("{implausible_density} grid cells had coral cover density >100% - capping")
      grid_data[, `:=`(
        cover_density_historical = pmin(1, cover_density_historical),
        cover_density_present    = pmin(1, cover_density_present),
        cover_density_future     = pmin(1, cover_density_future)
      )]
    }
    
    # 6.6 STANDARDIZE + GUARANTEE ECOREGION IN ARTIFACTS
    next_step()
    if (!is.null(grid_info) && "grid" %in% names(grid_info)) {
      grid_sf <- force_sf_with_crs(grid_info$grid, grid_info$crs)
      
      grid_sf$grid_id_orig <- grid_sf$grid_id
      if (is.na(sf::st_crs(grid_sf))) sf::st_crs(grid_sf) <- sf::st_crs(grid_info$crs)
      cent_projected <- suppressWarnings(sf::st_centroid(grid_sf))
      cent_wgs84     <- sf::st_transform(cent_projected, 4326)
      ll <- sf::st_coordinates(cent_wgs84)
      px_id <- sprintf("px_%0.6f_%0.6f", ll[,1], ll[,2])
      grid_sf$grid_id <- px_id
      
      if ("grid_id" %in% names(grid_data) && "grid_id_orig" %in% names(grid_sf)) {
        id_map <- data.table(
          grid_id_orig = as.character(grid_sf$grid_id_orig),
          grid_id_px   = as.character(grid_sf$grid_id)
        )
        grid_data[, grid_id := as.character(grid_id)]
        if (grid_data[, any(grid_id %in% id_map$grid_id_orig)]) {
          grid_data <- merge(
            grid_data,
            id_map,
            by.x = "grid_id",
            by.y = "grid_id_orig",
            all.x = TRUE,
            sort = FALSE
          )
          grid_data[, grid_id := fifelse(!is.na(grid_id_px), grid_id_px, grid_id)]
          grid_data[, grid_id_px := NULL]
        } else {
          logger::log_warn("grid_id→px_* remap by original id failed; attempting lon/lat remap")
          grid_data[, grid_id := sprintf("px_%0.6f_%0.6f", grid_lon, grid_lat)]
        }
      } else {
        grid_data[, grid_id := sprintf("px_%0.6f_%0.6f", grid_lon, grid_lat)]
      }
      
      logger::log_info("Attaching ECOREGION to grid polygons via spatial join...")
      gd_pts_wgs <- sf::st_as_sf(
        unique(grid_data[, .(grid_lon, grid_lat, ECOREGION)]),
        coords = c("grid_lon","grid_lat"), crs = 4326
      )
      # Project both to a local LAEA to avoid the "assumes planar" warning
      bb <- sf::st_bbox(gd_pts_wgs)
      cx <- as.numeric((bb["xmin"] + bb["xmax"]) / 2); cy <- as.numeric((bb["ymin"] + bb["ymax"]) / 2)
      laea <- sprintf('+proj=laea +lat_0=%f +lon_0=%f +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs', cy, cx)
      cent_laea <- sf::st_transform(cent_wgs84, laea)
      gd_laea   <- sf::st_transform(gd_pts_wgs, laea)
      nearest_idx <- sf::st_nearest_feature(cent_laea, gd_laea)
      eco_vals <- as.character(gd_pts_wgs$ECOREGION[nearest_idx])
      
      grid_sf$ECOREGION <- eco_vals
      if (!"grid_area_m2" %in% names(grid_sf))
        grid_sf$grid_area_m2 <- as.numeric(sf::st_area(grid_sf))
      
      grid_polys_laea <- dplyr::select(grid_sf, grid_id, ECOREGION, grid_area_m2)
      saveRDS(grid_polys_laea, file.path(region_path, "grid_polygons_laea.rds"))
      
      grid_points_laea <- tryCatch(
        suppressWarnings(sf::st_centroid(grid_polys_laea)),
        error = function(e) sf::st_point_on_surface(grid_polys_laea)
      )
      grid_points_wgs84 <- sf::st_transform(grid_points_laea, 4326)
      saveRDS(grid_points_laea, file.path(region_path, "grid_points_laea.rds"))
      saveRDS(grid_points_wgs84, file.path(region_path, "grid_points_wgs84.rds"))
      
      region_extent_m2 <- NA_real_
      if (all(c("life_history","extent_area_present_m2") %in% names(grid_data))) {
        if ("grid_id" %in% names(grid_data)) {
          ext_tab <- unique(grid_data[life_history == "total_cover", c("grid_id","extent_area_present_m2")])
          if (!nrow(ext_tab)) ext_tab <- unique(grid_data[, c("grid_id","extent_area_present_m2")])
          region_extent_m2 <- sum(ext_tab$extent_area_present_m2, na.rm = TRUE)
        }
      }
      crs_obj <- sf::st_crs(grid_polys_laea)
      crs_sidecar <- list(
        input     = crs_obj$input,
        epsg      = crs_obj$epsg,
        wkt       = crs_obj$wkt,
        grid_type = used_grid_type,
        region    = region_name,
        saved_at  = as.character(Sys.time())
      )
      saveRDS(crs_sidecar, file.path(region_path, "grid_crs.rds"))
      if (!is.null(crs_sidecar$wkt)) {
        writeLines(crs_sidecar$wkt, con = file.path(region_path, "grid_crs_wkt.txt"))
      }
      saveRDS(list(
        region_extent_m2 = region_extent_m2,
        computed_from    = "Part-1 aggregation (extent_area_present_m2, unique per cell total_cover)",
        saved_at         = Sys.time()
      ), file.path(region_path, "region_extent_m2.rds"))
      
      enhanced_polygons <- grid_sf
      enhanced_polygons$region_name <- region_name
      enhanced_polygons$grid_type   <- used_grid_type
      enhanced_polygons$crs_string  <- grid_info$crs
      saveRDS(enhanced_polygons, file.path(region_path, "original_grid_polygons.rds"))
      
      bbox_val <- tryCatch(sf::st_bbox(enhanced_polygons), error = function(e) NA)
      crs_metadata <- list(
        region_name = region_name,
        grid_type   = used_grid_type,
        crs_string  = grid_info$crs,
        crs_proj4   = grid_info$crs,
        bbox        = bbox_val,
        creation_time = Sys.time(),
        pipeline_version = "1.0"
      )
      saveRDS(crs_metadata, file.path(region_path, "crs_metadata.rds"))
      logger::log_info("Saved standardized grid artifacts (ECOREGION present and px_* ids aligned)")
    } else {
      logger::log_warn("No grid_info available for standardized output generation")
    }
    
    # 6.7 VALIDATION
    next_step()
    historical_summary <- grid_data[, .(
      n_cells_with_historical = sum(effective_cover_historical_m2 > 0, na.rm = TRUE),
      total_historical_cover  = sum(effective_cover_historical_m2, na.rm = TRUE),
      avg_historical_cover    = mean(effective_cover_historical_m2, na.rm = TRUE)
    )]
    logger::log_info("Historical data summary: {historical_summary$n_cells_with_historical} cells with historical data")
    
    required_fields <- c(
      "grid_lon","grid_lat","life_history",
      "effective_cover_present_m2","effective_cover_future_m2","effective_cover_historical_m2",
      "sd_present_m2","sd_future_m2","sd_historical_m2",
      "n_pixels_present",
      "avg_cover_per_pixel_present","avg_cover_per_pixel_future","avg_cover_per_pixel_historical",
      "extent_area_present_m2","ECOREGION","grid_area_m2",
      "cover_density_present","cover_density_future","cover_density_historical",
      "absolute_change_m2","relative_change_pct",
      "absolute_change_historical_present_m2","relative_change_historical_present_pct",
      "absolute_change_historical_future_m2","relative_change_historical_future_pct"
    )
    missing_fields <- setdiff(required_fields, names(grid_data))
    if (length(missing_fields) > 0) {
      logger::log_error("Missing required fields: {paste(missing_fields, collapse=', ')}")
      stop("Required fields missing from output")
    } else {
      logger::log_info("All required fields present for downstream processing")
    }
    
    # 6.8 EXPORT
    # (Avoid extra progress ticks to silence 'not listening' warning)
    output_grid <- if (exists("used_grid_type")) used_grid_type else CONFIG$grid_type
    
    # Derive a whole-number km² label for squares (robust to tiny area drift).
    square_km2_label <- tryCatch({
      if (!is.null(grid_info) && "grid" %in% names(grid_info) &&
          "grid_area_m2" %in% names(grid_info$grid)) {
        as.integer(round(stats::median(grid_info$grid$grid_area_m2, na.rm = TRUE) / 1e6))
      } else {
        as.integer(round((CONFIG$square_grid_res_m^2) / 1e6))
      }
    }, error = function(e) {
      as.integer(round((CONFIG$square_grid_res_m^2) / 1e6))
    })
    
    output_file <- switch(
      output_grid,
      "square"     = file.path(region_path, sprintf("%s_square_%dkm2.csv", region_name, square_km2_label)),
      "hexagonal"  = file.path(region_path, paste0(region_name, "_hex_25km.csv")),
      "geographic" = file.path(region_path, paste0(region_name, "_5k.csv"))
    )
    
    logger::log_info("Exporting results to: {output_file}")
    #data.table::fwrite(grid_data, output_file)
    data.table::fwrite(grid_data, output_file, nThread = data.table::getDTthreads())
    logger::log_info("Exported data to {output_file}")
    
  })  # <-- closes with_progress
  
  # timing + cleanup outside progress
  time_taken <- toc(quiet = TRUE)
  logger::log_info("Region {region_name} completed in {round(time_taken$toc - time_taken$tic, 2)}s")
  if (exists("all_data")) rm(all_data)
  if (exists("grid_data")) rm(grid_data)
  gc()
  TRUE
}  # <-- closes process_region()

# 7. MAIN EXECUTION LOOP ------------------------------------------------------

required_functions <- c(
  "get_azimuthal",
  "compute_laea_crs",
  "force_sf_with_crs",
  "assign_ecoregions",
  "process_geographic_grid",
  "load_lifehistory_data",
  "create_square_grid",
  "create_hexagonal_grid",
  "aggregate_to_grid_fast",
  "process_region"
)

missing_functions <- required_functions[!sapply(required_functions, exists)]
if (length(missing_functions) > 0) {
  logger::log_error(sprintf("Missing functions: %s", paste(missing_functions, collapse = ", ")))
  stop("Critical functions are missing from the code")
} else {
  logger::log_info("All required functions are present")
}

logger::log_info("Starting regional processing with {CONFIG$grid_type} grid...")
logger::log_info("Parent folder: {PARENT_FOLDER}")

tic("Total processing time")
for (region_name in names(REGION_CHECKLIST)) {
  if (isTRUE(REGION_CHECKLIST[[region_name]])) {
    region_path <- file.path(PARENT_FOLDER, region_name)
    if (!dir.exists(region_path)) {
      logger::log_warn("Region folder does not exist: {region_path}. Skipping.")
      next
    }
    tryCatch({
      success <- process_region(region_name, region_path)
      if (isTRUE(success)) {
        logger::log_info("Successfully processed region: {region_name}")
      } else {
        logger::log_error("Failed to process region: {region_name}")
      }
    }, error = function(e) {
      logger::log_error(sprintf("Error processing region %s: %s", region_name, conditionMessage(e)))
    })
    gc()
  } else {
    logger::log_info(sprintf("Skipping region (disabled in checklist): %s", region_name))
  }
}

total_time <- toc(quiet = TRUE)
logger::log_info(sprintf("All regional processing completed at %s", Sys.time()))
logger::log_info(sprintf("Total processing time: %.2f minutes", (total_time$toc - total_time$tic)/60))
logger::log_info(sprintf("R version: %s", R.version$version.string))
logger::log_info(sprintf("Platform: %s", R.version$platform))
logger::log_info(sprintf("Loaded packages: %s", paste(.packages(), collapse = ", ")))


# =============================================================================
# END OF PIPELINE
# =============================================================================


##sanity checks:

#1) Load artifacts and CSV
rp <- file.path(PARENT_FOLDER, "Africa-India")
gp  <- readRDS(file.path(rp, "grid_polygons_laea.rds"))     # polygons (LAEA)
gpt <- readRDS(file.path(rp, "grid_points_wgs84.rds"))      # centroids (WGS84)
dt  <- data.table::fread(file.path(rp, "Africa-India_square_5km2.csv"))
#2) Geometry & CRS sanity
all(sf::st_is_valid(gp))                         # should be TRUE
sf::st_is_longlat(gp)                            # should be FALSE (LAEA)
sf::st_crs(gp)$input                             # should show +proj=laea...
nrow(gp)                                         # ~ number of occupied cells
#3) Area checks (≈ 5 km² each)
areas_km2 <- as.numeric(sf::st_area(gp)) / 1e6
summary(areas_km2)                               # median ≈ 5
median(areas_km2)                                # ≈ 5
unique(dt$grid_area_m2)[1] / 1e6                 # also ≈ 5
#4) ID and centroid consistency
# 1) grid_id format and uniqueness
all(grepl("^px_-?\\d+\\.\\d{6}_-?\\d+\\.\\d{6}$", dt$grid_id))
data.table::uniqueN(dt$grid_id) == nrow(dt[, .N, by = grid_id])

# 2) CSV lon/lat vs saved points
csv_pts <- sf::st_as_sf(unique(dt[, .(grid_lon, grid_lat)]),
                        coords = c("grid_lon","grid_lat"), crs = 4326)
# nearest-neighbor distance check (should be ~0)
max(as.numeric(sf::st_distance(sf::st_geometry(csv_pts)[1:100],
                               sf::st_geometry(gpt)[1:100], by_element = TRUE)))
#5) Coverage & totals
# number of occupied cells should match polygon count
data.table::uniqueN(dt$grid_id) == nrow(gp)

# totals still reasonable (no negatives / densities ≤ 1)
stopifnot(all(dt$effective_cover_present_m2 >= 0))
stopifnot(all(dt$cover_density_present <= 1 + 1e-9))
#Quick visual spot-check
plot(sf::st_geometry(gp[sample(seq_len(nrow(gp)), min(5000, nrow(gp))), ]))
points(sf::st_coordinates(gpt)[sample(seq_len(nrow(gpt)), min(5000, nrow(gpt))), ], pch = 20, cex = 0.4)


