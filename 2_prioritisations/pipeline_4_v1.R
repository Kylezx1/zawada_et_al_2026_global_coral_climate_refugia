# ─────────────────────────────────────────────────────────────────────────────
# CORAL CONSERVATION PORTFOLIO: HYBRID POST-OPTIMIZATION ANALYSIS (PIPELINE 4)
# FIXED/ALIGNED: Reads P3 results directly; grid_id-based transfer; saved-CRS
# ADDED: Full figure/table suite (Fig 1–6b, tables), P2 metric attach, combined_grid.rds
#saves a lightweight, WGS84 copy to Selection/combined_grid.rds (IDL-safe) for quick re-plots or external tools.
#The full working analysis_grid.rds is saved in the projected CRS (so you don’t lose meter-based correctness).
# ─────────────────────────────────────────────────────────────────────────────
rm(list=ls())
suppressPackageStartupMessages({
  library(sf)
  library(data.table)
  library(dplyr)
  library(purrr)
  library(ggplot2)
  library(tidyr)
  library(scales)
  library(patchwork)
  library(kableExtra)
  library(leaflet)
  library(leaflet.extras)
  library(htmlwidgets)
  library(htmltools)
  library(forcats)
  library(ggridges)
  library(FNN)
  library(cli)
})

sf::sf_use_s2(FALSE)

# ---- Configuration ----
PARENT_FOLDER <- "~/Dropbox/8-MQ_Shared/Lenfest_kenya/data/PROJECT_SPECIFIC/bloomberg/life_history_predictions/October_analysis/"
DELTA_TYPE <- "present-future"


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

# ---- Helpers (robust/aligned with P1–P3) ----
`%||%` <- function(a, b) if (!is.null(a)) a else b

detect_object_type <- function(obj) {
  if (is.null(obj)) return("null")
  if (inherits(obj, "sf")) return("sf")
  if (inherits(obj, "data.table")) return("data.table")
  if (inherits(obj, "data.frame")) return("data.frame")
  if (inherits(obj, "list")) return("list")
  return(class(obj)[1])
}

detect_geometry_type <- function(sf_obj) {
  if (is.null(sf_obj) || nrow(sf_obj) == 0) return("empty")
  if (!inherits(sf_obj, "sf")) return(paste0("non-sf (", class(sf_obj)[1], ")"))
  geoms <- unique(sf::st_geometry_type(sf_obj))
  if (length(geoms) == 1) as.character(geoms) else "mixed"
}

convert_to_sf <- function(obj, target_crs = NULL) {
  if (inherits(obj, "sf")) {
    if (!is.null(target_crs) && !is.na(sf::st_crs(obj)) && !identical(sf::st_crs(obj), target_crs)) {
      return(sf::st_transform(obj, target_crs))
    }
    return(obj)
  }
  if (inherits(obj, c("data.table","data.frame"))) {
    geom_cols <- names(obj)[vapply(obj, function(x) inherits(x, "sfc"), logical(1))]
    if (length(geom_cols)) {
      sf_obj <- sf::st_as_sf(obj)
      if (!is.null(target_crs) && !is.na(sf::st_crs(sf_obj)) && !identical(sf::st_crs(sf_obj), target_crs)) {
        sf_obj <- sf::st_transform(sf_obj, target_crs)
      }
      return(sf_obj)
    }
    if (all(c("grid_lon","grid_lat") %in% names(obj))) {
      crs_use <- target_crs %||% 4326
      return(sf::st_as_sf(obj, coords = c("grid_lon","grid_lat"), crs = crs_use))
    }
    if (all(c("lon","lat") %in% names(obj))) {
      crs_use <- target_crs %||% 4326
      return(sf::st_as_sf(obj, coords = c("lon","lat"), crs = crs_use))
    }
  }
  stop("Cannot convert object to sf: no geometry/coordinates found")
}

# Load saved CRS from P1 sidecars
# REPLACE THE read_saved_crs FUNCTION with this robust version:
read_saved_crs <- function(region_path) {
  cli::cli_alert_info("Looking for CRS files in: {region_path}")
  
  # 1. Try grid_crs.rds first
  crs_rds <- file.path(region_path, "grid_crs.rds")
  if (file.exists(crs_rds)) {
    cli::cli_alert_info("Found grid_crs.rds")
    sc <- tryCatch(readRDS(crs_rds), error = function(e) {
      cli::cli_alert_warning("Failed to read grid_crs.rds: {e$message}")
      NULL
    })
    
    if (!is.null(sc)) {
      cli::cli_alert_info("CRS sidecar contents: {paste(names(sc), collapse=', ')}")
      
      # Try WKT first (most reliable)
      if (!is.null(sc$wkt) && nzchar(sc$wkt)) {
        cli::cli_alert_success("Using WKT from sidecar")
        return(sf::st_crs(sc$wkt))
      }
      # Try input string
      if (!is.null(sc$input) && nzchar(sc$input)) {
        cli::cli_alert_success("Using CRS string: {sc$input}")
        return(sf::st_crs(sc$input))
      }
      # Try EPSG
      if (!is.null(sc$epsg)) {
        cli::cli_alert_success("Using EPSG: {sc$epsg}")
        return(sf::st_crs(as.integer(sc$epsg)))
      }
    }
  }
  
  # 2. Try grid_crs_wkt.txt
  wtxt <- file.path(region_path, "grid_crs_wkt.txt")
  if (file.exists(wtxt)) {
    cli::cli_alert_info("Found grid_crs_wkt.txt")
    w <- tryCatch(paste(readLines(wtxt, warn=FALSE), collapse="\n"), error=function(e) {
      cli::cli_alert_warning("Failed to read WKT file: {e$message}")
      NULL
    })
    if (!is.null(w) && nzchar(w)) {
      cli::cli_alert_success("Using WKT from text file")
      return(sf::st_crs(w))
    }
  }
  
  # 3. Try crs_metadata.rds (backup)
  meta_file <- file.path(region_path, "crs_metadata.rds")
  if (file.exists(meta_file)) {
    cli::cli_alert_info("Found crs_metadata.rds")
    meta <- tryCatch(readRDS(meta_file), error = function(e) NULL)
    if (!is.null(meta) && !is.null(meta$crs_string)) {
      cli::cli_alert_success("Using CRS from metadata: {meta$crs_string}")
      return(sf::st_crs(meta$crs_string))
    }
  }
  
  cli::cli_alert_warning("No CRS files found or all failed")
  return(NULL)
}

# REPLACE THE ensure_region_crs FUNCTION with this version:
ensure_region_crs <- function(sf_obj, region_path) {
  stopifnot(inherits(sf_obj, "sf"))
  
  current_crs <- sf::st_crs(sf_obj)
  if (!is.na(current_crs)) {
    cli::cli_alert_info("Object already has CRS: {current_crs$input}")
    return(sf_obj)
  }
  
  cli::cli_alert_warning("Object has NA CRS - attempting to assign from saved files")
  
  # Try to read saved CRS
  crs_saved <- read_saved_crs(region_path)
  
  if (!is.null(crs_saved)) {
    sf::st_crs(sf_obj) <- crs_saved
    cli::cli_alert_success("Assigned saved CRS: {crs_saved$input %||% crs_saved$epsg %||% 'WKT'}")
    return(sf_obj)
  }
  
  # Last resort: create LAEA based on data extent
  cli::cli_alert_warning("No saved CRS found, creating fallback LAEA")
  bb <- sf::st_bbox(sf_obj)
  
  # Check if coordinates look like they might already be in degrees
  coords <- sf::st_coordinates(sf_obj)
  if (max(abs(coords)) < 180 && min(abs(coords)) > 0.001) {
    cli::cli_alert_info("Coordinates look like degrees, assigning WGS84")
    sf::st_crs(sf_obj) <- 4326
    return(sf_obj)
  }
  
  # Otherwise use LAEA centered on data
  cx <- as.numeric((bb["xmin"] + bb["xmax"]) / 2)
  cy <- as.numeric((bb["ymin"] + bb["ymax"]) / 2)
  laea <- sprintf("+proj=laea +lat_0=%f +lon_0=%f +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs", cy, cx)
  sf::st_crs(sf_obj) <- sf::st_crs(laea)
  cli::cli_alert_warning("Assigned fallback LAEA CRS centered at ({round(cx,1)}, {round(cy,1)})")
  
  return(sf_obj)
}

# ADD THIS FUNCTION to debug CRS issues
diagnose_crs_issue <- function(region_name) {
  cli::cli_h2("CRS Diagnosis for: {region_name}")
  
  region_path <- file.path(PARENT_FOLDER, region_name)
  
  # 1. Check what CRS files exist
  crs_files <- c(
    "grid_crs.rds",
    "grid_crs_wkt.txt", 
    "crs_metadata.rds",
    "grid_polygons_laea.rds"
  )
  
  cli::cli_alert_info("Checking for CRS files:")
  for (f in crs_files) {
    fp <- file.path(region_path, f)
    if (file.exists(fp)) {
      cli::cli_alert_success("✓ {f} ({round(file.size(fp)/1024, 1)} KB)")
    } else {
      cli::cli_alert_danger("✗ {f} (missing)")
    }
  }
  
  # 2. Test reading the CRS
  saved_crs <- read_saved_crs(region_path)
  if (!is.null(saved_crs)) {
    cli::cli_alert_success("CRS successfully read: {saved_crs$input %||% saved_crs$epsg %||% 'WKT'}")
  } else {
    cli::cli_alert_danger("FAILED to read any CRS")
  }
  
  # 3. Check analysis grid
  ag_file <- file.path(region_path, "Selection", "analysis_grid.rds")
  if (file.exists(ag_file)) {
    ag <- readRDS(ag_file)
    current_crs <- sf::st_crs(ag)
    cli::cli_alert_info("Analysis grid CRS: {current_crs$input %||% 'MISSING'}")
    
    # Test applying CRS
    if (is.na(current_crs)) {
      cli::cli_alert_info("Testing CRS assignment...")
      ag_fixed <- ensure_region_crs(ag, region_path)
      new_crs <- sf::st_crs(ag_fixed)
      cli::cli_alert_info("After fix: {new_crs$input %||% 'STILL MISSING'}")
    }
  }
  
  return(saved_crs)
}

# RUN THIS BEFORE YOUR MAIN LOOP:
test_regions <- names(REGION_CHECKLIST)[1:2]  # Test first 2 regions
for (region in test_regions) {
  diagnose_crs_issue(region)
}


# Assign saved projected CRS to any sf with NA CRS (region LAEA)
ensure_region_crs <- function(sf_obj, region_path) {
  stopifnot(inherits(sf_obj, "sf"))
  if (!is.na(sf::st_crs(sf_obj))) return(sf_obj)
  crs_saved <- read_saved_crs(region_path)
  if (!is.null(crs_saved)) {
    sf::st_crs(sf_obj) <- crs_saved
    cli::cli_alert_success("Assigned saved projected CRS: {crs_saved$input %||% crs_saved$epsg %||% 'WKT'}")
    return(sf_obj)
  }
  # last-resort LAEA centered on bbox
  bb <- sf::st_bbox(sf_obj)
  cx <- as.numeric((bb["xmin"] + bb["xmax"]) / 2)
  cy <- as.numeric((bb["ymin"] + bb["ymax"]) / 2)
  laea <- sprintf("+proj=laea +lat_0=%f +lon_0=%f +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs", cy, cx)
  sf::st_crs(sf_obj) <- sf::st_crs(laea)
  cli::cli_alert_warning("Assigned fallback LAEA CRS centered at ({round(cx,3)}, {round(cy,3)})")
  sf_obj
}

# Prefer direct use of P3 grids; fallback to hybrid transfer if needed
combine_p3_grids <- function(gurobi_results) {
  cli::cli_alert_info("Combining per-ecoregion grids from Pipeline 3…")
  grids <- list()
  for (nm in names(gurobi_results)) {
    r <- gurobi_results[[nm]]
    if (!is.null(r$grid) && inherits(r$grid, "sf")) {
      g <- r$grid
      if (!"ECOREGION" %in% names(g)) g$ECOREGION <- nm
      grids[[nm]] <- g
    }
  }
  if (!length(grids)) return(NULL)
  suppressWarnings({ g_all <- do.call(rbind, grids) })
  cli::cli_alert_success("Combined {length(grids)} ecoregions → {nrow(g_all)} features")
  g_all
}

# Extract selection data (grid_id + centroid x/y in PROJECTED CRS) for hybrid fallback
extract_selection_data <- function(gurobi_results, region_path) {
  cli::cli_alert_info("Extracting selection data from P3 (projected x/y)…")
  acc <- list()
  for (i in seq_along(gurobi_results)) {
    nm <- names(gurobi_results)[i] %||% paste0("Ecoregion_", i)
    res <- gurobi_results[[i]]
    if (is.null(res$grid) || !inherits(res$grid, "sf")) next
    g <- res$grid
    if (is.na(sf::st_crs(g))) g <- ensure_region_crs(g, region_path)
    sel_col <- intersect(c("selected","selection","in_portfolio","portfolio","select"), names(g))[1]
    if (is.na(sel_col)) {
      cli::cli_alert_warning("No selection column found for {nm}; skipping")
      next
    }
    cent <- suppressWarnings(sf::st_centroid(g))
    xy <- sf::st_coordinates(cent)  # projected meters
    df <- sf::st_drop_geometry(g)
    acc[[nm]] <- data.frame(
      grid_id  = as.character(df$grid_id %||% NA_character_),
      ECOREGION= as.character(df$ECOREGION %||% nm),
      x        = as.numeric(xy[,1]),
      y        = as.numeric(xy[,2]),
      selected = as.logical(df[[sel_col]]),
      stringsAsFactors = FALSE
    )
  }
  if (!length(acc)) {
    cli::cli_alert_danger("No selection data could be extracted")
    return(NULL)
  }
  out <- dplyr::bind_rows(acc)
  out <- out[!is.na(out$selected), , drop = FALSE]
  attr(out, "crs") <- read_saved_crs(region_path)
  cli::cli_alert_success("Extracted {nrow(out)} rows; {sum(out$selected)} selected (projected x/y)")
  out
}

# Load original polygons from P1; respect CRS sidecar if needed
load_original_polygons <- function(region_path) {
  target_crs <- read_saved_crs(region_path)
  poly_file <- file.path(region_path, "grid_polygons_laea.rds")
  if (!file.exists(poly_file)) {
    cli::cli_alert_warning("P1 polygons not found: {poly_file}")
    return(NULL)
  }
  poly <- tryCatch(readRDS(poly_file), error = function(e) NULL)
  if (is.null(poly)) {
    cli::cli_alert_warning("Failed to read {poly_file}")
    return(NULL)
  }
  poly_sf <- if (inherits(poly, "sf")) poly else sf::st_as_sf(poly)
  if (is.na(sf::st_crs(poly_sf)) && !is.null(target_crs)) sf::st_crs(poly_sf) <- target_crs
  if (!is.null(target_crs) && !identical(sf::st_crs(poly_sf), target_crs)) {
    poly_sf <- sf::st_transform(poly_sf, target_crs)
  }
  gt <- detect_geometry_type(poly_sf)
  if (!gt %in% c("POLYGON","MULTIPOLYGON","mixed")) {
    cli::cli_alert_warning("P1 geometry is not polygonal: {gt}")
    return(NULL)
  }
  bb <- sf::st_bbox(poly_sf)
  cli::cli_alert_success("Loaded P1 polygons: {nrow(poly_sf)} features; CRS={sf::st_crs(poly_sf)$input %||% 'NA'}")
  cli::cli_alert_info("  BBOX: X [{round(bb$xmin,1)}, {round(bb$xmax,1)}], Y [{round(bb$ymin,1)}, {round(bb$ymax,1)}]")
  poly_sf
}

# Hybrid transfer: prefer join by grid_id; fallback to nearest polygon using PROJECTED x/y
create_hybrid_geometry <- function(selection_data, original_polygons, region_path) {
  if (is.null(selection_data) || !nrow(selection_data)) {
    cli::cli_alert_warning("No selection data for hybrid geometry")
    return(list(has_polygons = FALSE, data = NULL))
  }
  if (is.null(original_polygons) || !nrow(original_polygons)) {
    cli::cli_alert_warning("No original polygons provided")
    return(list(has_polygons = FALSE, data = NULL))
  }
  # Attempt 1: grid_id join
  if ("grid_id" %in% names(selection_data) && "grid_id" %in% names(original_polygons)) {
    cli::cli_alert_info("Applying grid_id-based selection transfer…")
    sel_tbl <- selection_data %>%
      dplyr::select(grid_id, selected) %>%
      dplyr::group_by(grid_id) %>%
      dplyr::summarise(selected = any(selected), .groups = "drop")
    out <- dplyr::left_join(original_polygons, sel_tbl, by = "grid_id")
    out$selected <- as.logical(out$selected %||% FALSE)
    cli::cli_alert_success("grid_id transfer complete: {sum(out$selected)} polygons selected")
    return(list(has_polygons = TRUE, data = out))
  }
  # Attempt 2: NN on projected x/y
  cli::cli_alert_info("grid_id not available; using nearest-neighbor in projected CRS…")
  target_crs <- sf::st_crs(original_polygons)
  if (is.na(target_crs)) {
    original_polygons <- ensure_region_crs(original_polygons, region_path)
    target_crs <- sf::st_crs(original_polygons)
  }
  if (!all(c("x","y") %in% names(selection_data))) {
    cli::cli_alert_danger("selection_data lacks x/y; cannot do NN without grid_id")
    return(list(has_polygons = TRUE, data = original_polygons %>% dplyr::mutate(selected = FALSE)))
  }
  sel_pts <- sf::st_as_sf(selection_data, coords = c("x","y"), crs = target_crs)
  poly_cent <- sf::st_centroid(original_polygons)
  sel_xy  <- sf::st_coordinates(sel_pts)
  poly_xy <- sf::st_coordinates(poly_cent)
  nn <- FNN::get.knnx(poly_xy, sel_xy, k = 1)
  nearest_poly <- as.integer(nn$nn.index[,1])
  selected_flags <- rep(FALSE, nrow(original_polygons))
  if (length(nearest_poly)) {
    selected_flags[unique(nearest_poly[which(sel_pts$selected)])] <- TRUE
  }
  original_polygons$selected <- selected_flags
  cli::cli_alert_success("Nearest-neighbor transfer: {sum(selected_flags)} polygons selected")
  list(has_polygons = TRUE, data = original_polygons)
}

# Dateline-safe WGS84
# ENHANCED DATELINE HANDLING
handle_dateline_crossing <- function(sf_obj_wgs84) {
  if (is.null(sf_obj_wgs84) || !inherits(sf_obj_wgs84, "sf") || nrow(sf_obj_wgs84) == 0) {
    return(sf_obj_wgs84)
  }
  
  # Ensure WGS84 CRS
  if (is.na(sf::st_crs(sf_obj_wgs84))) {
    sf::st_crs(sf_obj_wgs84) <- 4326
  } else if (!identical(sf::st_crs(sf_obj_wgs84)$epsg, 4326L)) {
    sf_obj_wgs84 <- sf::st_transform(sf_obj_wgs84, 4326)
  }
  
  # Check for dateline crossing
  bbox <- sf::st_bbox(sf_obj_wgs84)
  spans_dateline <- (bbox["xmin"] < -170 && bbox["xmax"] > 170) || 
    (bbox["xmax"] - bbox["xmin"] > 300)
  
  if (spans_dateline) {
    cli::cli_alert_info("Dateline crossing detected - applying longitude shift")
    
    # Method 1: Try st_shift_longitude (preferred)
    shifted <- tryCatch({
      result <- sf::st_shift_longitude(sf_obj_wgs84)
      cli::cli_alert_success("Applied st_shift_longitude successfully")
      result
    }, error = function(e) {
      cli::cli_alert_warning("st_shift_longitude failed: {e$message}")
      
      # Method 2: Manual coordinate adjustment
      cli::cli_alert_info("Attempting manual coordinate adjustment")
      coords <- sf::st_coordinates(sf_obj_wgs84)
      if (!is.null(coords)) {
        # Shift negative longitudes to positive
        coords[, "X"] <- ifelse(coords[, "X"] < 0, coords[, "X"] + 360, coords[, "X"])
        
        # Recreate geometry (this is simplified - may need adjustment for your geometry types)
        if (all(st_geometry_type(sf_obj_wgs84) == "POINT")) {
          shifted_geom <- st_sfc(lapply(1:nrow(coords), function(i) {
            st_point(coords[i, c("X", "Y")])
          }), crs = 4326)
          sf_obj_wgs84 <- st_set_geometry(sf_obj_wgs84, shifted_geom)
        }
        cli::cli_alert_success("Manual coordinate adjustment completed")
      }
      sf_obj_wgs84
    })
    
    return(shifted)
  }
  
  cli::cli_alert_info("No dateline crossing detected")
  return(sf_obj_wgs84)
}

# Extent calc aligned to P1–P3
calculate_grid_extent <- function(grid_data, use_geometry_area = TRUE) {
  if (is.null(grid_data) || !nrow(grid_data)) return(0)
  x <- convert_to_sf(grid_data)
  
  # STRICT priority: reef extent first; grid area last
  EXTENT_PRIORITY <- c(
    "extent_area_present_m2_total_cover",
    "extent_area_present_m2",
    "pixel_area", "pixel_area_m2",
    "grid_area_m2"  # last resort only
  )
  
  # 1) Prefer per-row extent column if present
  for (cn in EXTENT_PRIORITY) {
    if (cn %in% names(x)) {
      if (identical(cn, "grid_area_m2"))
        cli::cli_alert_warning("Using 'grid_area_m2' as a last-resort for extent. Check upstream extent export.")
      return(sum(x[[cn]], na.rm = TRUE))
    }
  }
  
  # 2) If polygons and allowed, compute geometry area
  gt <- detect_geometry_type(x)
  if (use_geometry_area && gt %in% c("POLYGON","MULTIPOLYGON","mixed")) {
    return(as.numeric(sum(sf::st_area(x), na.rm = TRUE)))
  }
  
  # 3) Absolute last fallback (not ideal)
  cli::cli_alert_warning("No extent columns or polygon area available; using 250m pixel default.")
  nrow(x) * (250 * 250)
}


# Attach P2 metrics (effective_cover_present_m2_total_cover) if absent
attach_p2_metrics <- function(analysis_grid, region_path, delta_type) {
  delta_suffix <- gsub("-", "_", delta_type)
  p2_file <- file.path(region_path, paste0("conservation_metrics_", delta_suffix, ".rds"))
  if (!file.exists(p2_file)) {
    cli::cli_alert_info("P2 file not found for metrics attach: {basename(p2_file)}")
    return(analysis_grid)
  }
  p2 <- tryCatch(readRDS(p2_file), error = function(e) NULL)
  if (is.null(p2) || !inherits(p2, "sf")) return(analysis_grid)
  keep <- c("grid_id","effective_cover_present_m2_total_cover","extent_area_present_m2_total_cover")
  keep <- intersect(keep, names(p2))
  if (!length(keep)) return(analysis_grid)
  mtab <- sf::st_drop_geometry(p2)[, keep, drop = FALSE]
  mtab <- dplyr::distinct(mtab, grid_id, .keep_all = TRUE)
  # only add if columns not present
  add_cols <- setdiff(names(mtab), intersect(names(mtab), names(analysis_grid)))
  if (length(add_cols) > 1) {
    analysis_grid <- dplyr::left_join(analysis_grid, mtab, by = "grid_id")
    cli::cli_alert_success("Attached P2 metrics to analysis grid")
  }
  analysis_grid
}

# Interactive map (works for polygons or points)
# ENHANCED INTERACTIVE MAP FUNCTION
create_interactive_map <- function(analysis_grid, region_name, output_dir, region_path = NULL) {
  cli::cli_alert_info("Creating interactive map using clean polygon files...")
  
  tryCatch({
    # --- LOAD CLEAN POLYGONS INSTEAD OF ANALYSIS_GRID ---
    poly_file <- file.path(region_path, "grid_polygons_laea.rds")
    if (!file.exists(poly_file)) {
      poly_file <- file.path(region_path, "original_grid_polygons.rds")
    }
    
    if (!file.exists(poly_file)) {
      cli::cli_alert_warning("No polygon files found, using analysis grid with points")
      # Fallback to points if no polygon files
      return(create_points_fallback(analysis_grid, region_name, output_dir, region_path))
    }
    
    cli::cli_alert_info("Loading clean polygons: {basename(poly_file)}")
    clean_polygons <- readRDS(poly_file)
    
    # Check if polygons have CRS
    poly_crs <- sf::st_crs(clean_polygons)
    if (is.na(poly_crs)) {
      cli::cli_alert_warning("Polygons missing CRS, using analysis grid CRS")
      clean_polygons <- ensure_region_crs(clean_polygons, region_path)
    }
    
    # --- MERGE SELECTION DATA FROM ANALYSIS_GRID ---
    if (!is.null(analysis_grid) && "selected" %in% names(analysis_grid) && "grid_id" %in% names(analysis_grid)) {
      cli::cli_alert_info("Merging selection data from analysis results...")
      selection_data <- sf::st_drop_geometry(analysis_grid[, c("grid_id", "selected")])
      selection_data <- selection_data[!duplicated(selection_data$grid_id), ]
      
      clean_polygons <- merge(clean_polygons, selection_data, by = "grid_id", all.x = TRUE)
      cli::cli_alert_success("Merged selection data: {sum(clean_polygons$selected, na.rm = TRUE)} selected cells")
    } else {
      cli::cli_alert_warning("No selection data found, all cells marked as not selected")
      clean_polygons$selected <- FALSE
    }
    
    clean_polygons$selected[is.na(clean_polygons$selected)] <- FALSE
    
    # --- TRANSFORM TO WGS84 ---
    cli::cli_alert_info("Transforming polygons to WGS84...")
    analysis_grid_wgs84 <- sf::st_transform(clean_polygons, 4326)
    
    # Make sure geometries are valid
    analysis_grid_wgs84 <- sf::st_make_valid(analysis_grid_wgs84)
    
    # Remove empty geometries
    empty_count <- sum(sf::st_is_empty(analysis_grid_wgs84))
    if (empty_count > 0) {
      cli::cli_alert_warning("Removing {empty_count} empty geometries")
      analysis_grid_wgs84 <- analysis_grid_wgs84[!sf::st_is_empty(analysis_grid_wgs84), ]
    }
    
    if (nrow(analysis_grid_wgs84) == 0) {
      cli::cli_alert_danger("No valid geometries after filtering")
      return(NULL)
    }
    
    cli::cli_alert_info("Final polygon count: {nrow(analysis_grid_wgs84)} ({sum(analysis_grid_wgs84$selected)} selected)")
    
    # --- SIMPLIFY POLYGONS FOR PERFORMANCE ---
    vertex_count <- tryCatch({
      sum(lengths(sf::st_geometry(analysis_grid_wgs84)))
    }, error = function(e) NA)
    
    if (!is.na(vertex_count) && vertex_count > 100000) {
      cli::cli_alert_info("Simplifying polygons ({vertex_count} vertices)...")
      analysis_grid_wgs84 <- sf::st_simplify(analysis_grid_wgs84, preserveTopology = TRUE, dTolerance = 0.0001)
    }
    
    # --- OCEAN BASEMAP WITH POLYGONS ---
    cli::cli_alert_info("Building ocean basemap with polygons...")
    
    # Split data
    selected_data <- analysis_grid_wgs84[analysis_grid_wgs84$selected, ]
    not_selected_data <- analysis_grid_wgs84[!analysis_grid_wgs84$selected, ]
    
    # Initialize map with ocean basemaps
    m <- leaflet::leaflet(
      options = leaflet::leafletOptions(
        preferCanvas = TRUE,
        worldCopyJump = TRUE
      )
    ) %>%
      leaflet::addProviderTiles(
        "Esri.OceanBasemap",
        group = "Ocean Basemap"
      ) %>%
      leaflet::addProviderTiles(
        "CartoDB.Positron",
        group = "Light Map"
      ) %>%
      leaflet::addProviderTiles(
        "Esri.WorldImagery",
        group = "Satellite"
      ) %>%
      leaflet::addProviderTiles(
        "OpenStreetMap.Mapnik",
        group = "OpenStreetMap"
      )
    
    # --- ADD NOT-SELECTED POLYGONS (GREY) ---
    if (nrow(not_selected_data) > 0) {
      m <- m %>% leaflet::addPolygons(
        data = not_selected_data,
        fillColor = "#9CA3AF",  # GREY
        fillOpacity = 0.5,
        color = "#6B7280",
        weight = 0.5,
        opacity = 0.7,
        group = "Not Selected",
        popup = ~paste(
          "<b>Not Selected</b><br>",
          "Ecoregion: ", ECOREGION %||% "Unknown", "<br>",
          "Grid ID: ", grid_id %||% "Unknown"
        ),
        highlightOptions = leaflet::highlightOptions(
          weight = 2,
          color = "#4B5563",
          fillOpacity = 0.7,
          bringToFront = FALSE
        )
      )
      cli::cli_alert_success("Added {nrow(not_selected_data)} not-selected polygons (grey)")
    }
    
    # --- ADD SELECTED POLYGONS (RED) ---
    if (nrow(selected_data) > 0) {
      m <- m %>% leaflet::addPolygons(
        data = selected_data,
        fillColor = "#FF0000",  # RED
        fillOpacity = 0.8,
        color = "#CC0000",
        weight = 1.5,
        opacity = 0.9,
        group = "Selected",
        popup = ~paste(
          "<b>SELECTED</b><br>",
          "Ecoregion: ", ECOREGION %||% "Unknown", "<br>",
          "Grid ID: ", grid_id %||% "Unknown"
        ),
        highlightOptions = leaflet::highlightOptions(
          weight = 3,
          color = "#990000",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      )
      cli::cli_alert_success("Added {nrow(selected_data)} selected polygons (red)")
    }
    
    # --- SET VIEW ---
    bbox <- sf::st_bbox(analysis_grid_wgs84)
    if (all(is.finite(bbox)) && (bbox$xmax - bbox$xmin) > 0.001 && (bbox$ymax - bbox$ymin) > 0.001) {
      m <- m %>% leaflet::fitBounds(
        lng1 = bbox$xmin, lat1 = bbox$ymin,
        lng2 = bbox$xmax, lat2 = bbox$ymax
      )
      cli::cli_alert_success("Set bounds to data extent")
    } else {
      coords <- sf::st_coordinates(sf::st_centroid(analysis_grid_wgs84))
      center_lng <- mean(coords[,1], na.rm = TRUE)
      center_lat <- mean(coords[,2], na.rm = TRUE)
      m <- m %>% leaflet::setView(lng = center_lng, lat = center_lat, zoom = 6)
      cli::cli_alert_info("Set view to center")
    }
    
    # --- LAYER CONTROLS ---
    overlay_groups <- c()
    if (nrow(selected_data) > 0) overlay_groups <- c(overlay_groups, "Selected")
    if (nrow(not_selected_data) > 0) overlay_groups <- c(overlay_groups, "Not Selected")
    
    if (length(overlay_groups) > 0) {
      m <- m %>% leaflet::addLayersControl(
        baseGroups = c("Ocean Basemap", "Light Map", "Satellite", "OpenStreetMap"),
        overlayGroups = overlay_groups,
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )
    } else {
      m <- m %>% leaflet::addLayersControl(
        baseGroups = c("Ocean Basemap", "Light Map", "Satellite", "OpenStreetMap"),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )
    }
    
    # Add legend
    m <- m %>% leaflet::addLegend(
      position = "bottomright",
      colors = c("#FF0000", "#9CA3AF"),
      labels = c("Selected", "Not Selected"),
      title = "Selection Status",
      opacity = 1
    )
    
    # --- SAVE MAP ---
    map_file <- file.path(output_dir, "interactive", 
                          paste0("portfolio_map_", gsub(" ", "_", region_name), ".html"))
    dir.create(dirname(map_file), showWarnings = FALSE, recursive = TRUE)
    
    htmlwidgets::saveWidget(
      widget = m,
      file = map_file,
      selfcontained = FALSE,
      title = paste("Portfolio Selection -", region_name)
    )
    
    cli::cli_alert_success("Polygon map saved: {map_file}")
    
    if (file.exists(map_file)) {
      file_size <- file.size(map_file)
      cli::cli_alert_success("HTML file created ({round(file_size/1024/1024, 2)} MB)")
    }
    
    return(m)
    
  }, error = function(e) {
    cli::cli_alert_danger("Polygon map creation failed: {conditionMessage(e)}")
    cli::cli_alert_info("Falling back to points...")
    return(create_points_fallback(analysis_grid, region_name, output_dir, region_path))
  })
}

# COMPLETE POINTS FALLBACK FUNCTION
create_points_fallback <- function(analysis_grid, region_name, output_dir, region_path = NULL) {
  cli::cli_alert_info("Using reliable points fallback...")
  
  tryCatch({
    if (is.null(analysis_grid) || !inherits(analysis_grid, "sf") || nrow(analysis_grid) == 0) {
      cli::cli_alert_warning("No features to map in fallback")
      return(NULL)
    }
    
    # --- CRS FIX ---
    current_crs <- sf::st_crs(analysis_grid)
    if (is.na(current_crs)) {
      cli::cli_alert_warning("CRS is missing, attempting recovery...")
      analysis_grid <- ensure_region_crs(analysis_grid, region_path)
    }
    
    # Transform to WGS84 for Leaflet
    analysis_grid_wgs84 <- sf::st_transform(analysis_grid, 4326)
    
    # Ensure selected column exists and is logical
    if (!"selected" %in% names(analysis_grid_wgs84)) {
      analysis_grid_wgs84$selected <- FALSE
    }
    analysis_grid_wgs84$selected <- as.logical(analysis_grid_wgs84$selected)
    analysis_grid_wgs84$selected[is.na(analysis_grid_wgs84$selected)] <- FALSE
    
    cli::cli_alert_info("Points fallback - Selection: {sum(analysis_grid_wgs84$selected)} selected, {sum(!analysis_grid_wgs84$selected)} not selected")
    
    # Convert to centroids for reliable display
    points_data <- sf::st_centroid(analysis_grid_wgs84)
    coords <- sf::st_coordinates(points_data)
    
    # Create clean data frame
    map_data <- data.frame(
      lng = coords[,1],
      lat = coords[,2],
      selected = points_data$selected,
      ecoregion = points_data$ECOREGION %||% "Unknown",
      grid_id = points_data$grid_id %||% "Unknown",
      stringsAsFactors = FALSE
    )
    
    # Remove invalid coordinates
    valid_rows <- !is.na(map_data$lng) & !is.na(map_data$lat) & 
      map_data$lng >= -180 & map_data$lng <= 180 &
      map_data$lat >= -90 & map_data$lat <= 90
    
    if (sum(valid_rows) == 0) {
      cli::cli_alert_danger("No valid coordinates in fallback")
      return(NULL)
    }
    
    map_data <- map_data[valid_rows, ]
    
    # --- OCEAN BASEMAP WITH POINTS ---
    m <- leaflet::leaflet() %>%
      leaflet::addProviderTiles(
        "Esri.OceanBasemap",
        group = "Ocean Basemap"
      ) %>%
      leaflet::addProviderTiles(
        "CartoDB.Positron",
        group = "Light Map"
      ) %>%
      leaflet::addProviderTiles(
        "Esri.WorldImagery",
        group = "Satellite"
      )
    
    # Add points
    selected_data <- map_data[map_data$selected, ]
    not_selected_data <- map_data[!map_data$selected, ]
    
    if (nrow(selected_data) > 0) {
      m <- m %>% leaflet::addCircleMarkers(
        data = selected_data,
        lng = ~lng, lat = ~lat,
        radius = 6,
        color = "#FF0000",
        fillColor = "#FF0000",
        fillOpacity = 0.8,
        group = "Selected",
        popup = ~paste("<b>SELECTED</b><br>Ecoregion: ", ecoregion)
      )
    }
    
    if (nrow(not_selected_data) > 0) {
      m <- m %>% leaflet::addCircleMarkers(
        data = not_selected_data, 
        lng = ~lng, lat = ~lat,
        radius = 3,
        color = "#9CA3AF",  # GREY
        fillColor = "#9CA3AF",
        fillOpacity = 0.4,
        group = "Not Selected",
        popup = ~paste("Ecoregion: ", ecoregion)
      )
    }
    
    # Set view
    if (nrow(map_data) > 0) {
      center_lng <- mean(map_data$lng, na.rm = TRUE)
      center_lat <- mean(map_data$lat, na.rm = TRUE)
      m <- m %>% leaflet::setView(lng = center_lng, lat = center_lat, zoom = 6)
    }
    
    # Layer control
    overlay_groups <- c()
    if (nrow(selected_data) > 0) overlay_groups <- c(overlay_groups, "Selected")
    if (nrow(not_selected_data) > 0) overlay_groups <- c(overlay_groups, "Not Selected")
    
    if (length(overlay_groups) > 0) {
      m <- m %>% leaflet::addLayersControl(
        baseGroups = c("Ocean Basemap", "Light Map", "Satellite"),
        overlayGroups = overlay_groups,
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )
    }
    
    # Add legend
    m <- m %>% leaflet::addLegend(
      position = "bottomright",
      colors = c("#FF0000", "#9CA3AF"),
      labels = c("Selected", "Not Selected"),
      title = "Selection Status"
    )
    
    # Save map
    map_file <- file.path(output_dir, "interactive", 
                          paste0("portfolio_map_", gsub(" ", "_", region_name), ".html"))
    dir.create(dirname(map_file), showWarnings = FALSE, recursive = TRUE)
    
    htmlwidgets::saveWidget(m, map_file, selfcontained = FALSE)
    cli::cli_alert_success("Points fallback map saved: {map_file}")
    
    return(m)
    
  }, error = function(e) {
    cli::cli_alert_danger("Points fallback also failed: {e$message}")
    return(NULL)
  })
}

# COMPLETE POINTS FALLBACK FUNCTION
create_points_fallback <- function(analysis_grid, region_name, output_dir, region_path = NULL) {
  cli::cli_alert_info("Using reliable points fallback...")
  
  tryCatch({
    if (is.null(analysis_grid) || !inherits(analysis_grid, "sf") || nrow(analysis_grid) == 0) {
      cli::cli_alert_warning("No features to map in fallback")
      return(NULL)
    }
    
    # --- CRS FIX ---
    current_crs <- sf::st_crs(analysis_grid)
    if (is.na(current_crs)) {
      cli::cli_alert_warning("CRS is missing, attempting recovery...")
      analysis_grid <- ensure_region_crs(analysis_grid, region_path)
    }
    
    # Transform to WGS84 for Leaflet
    analysis_grid_wgs84 <- sf::st_transform(analysis_grid, 4326)
    
    # Ensure selected column exists and is logical
    if (!"selected" %in% names(analysis_grid_wgs84)) {
      analysis_grid_wgs84$selected <- FALSE
    }
    analysis_grid_wgs84$selected <- as.logical(analysis_grid_wgs84$selected)
    analysis_grid_wgs84$selected[is.na(analysis_grid_wgs84$selected)] <- FALSE
    
    cli::cli_alert_info("Points fallback - Selection: {sum(analysis_grid_wgs84$selected)} selected, {sum(!analysis_grid_wgs84$selected)} not selected")
    
    # Convert to centroids for reliable display
    points_data <- sf::st_centroid(analysis_grid_wgs84)
    coords <- sf::st_coordinates(points_data)
    
    # Create clean data frame
    map_data <- data.frame(
      lng = coords[,1],
      lat = coords[,2],
      selected = points_data$selected,
      ecoregion = points_data$ECOREGION %||% "Unknown",
      grid_id = points_data$grid_id %||% "Unknown",
      stringsAsFactors = FALSE
    )
    
    # Remove invalid coordinates
    valid_rows <- !is.na(map_data$lng) & !is.na(map_data$lat) & 
      map_data$lng >= -180 & map_data$lng <= 180 &
      map_data$lat >= -90 & map_data$lat <= 90
    
    if (sum(valid_rows) == 0) {
      cli::cli_alert_danger("No valid coordinates in fallback")
      return(NULL)
    }
    
    map_data <- map_data[valid_rows, ]
    
    # --- OCEAN BASEMAP WITH POINTS ---
    m <- leaflet::leaflet() %>%
      leaflet::addProviderTiles(
        "Esri.OceanBasemap",
        group = "Ocean Basemap"
      ) %>%
      leaflet::addProviderTiles(
        "CartoDB.Positron",
        group = "Light Map"
      ) %>%
      leaflet::addProviderTiles(
        "Esri.WorldImagery",
        group = "Satellite"
      )
    
    # Add points
    selected_data <- map_data[map_data$selected, ]
    not_selected_data <- map_data[!map_data$selected, ]
    
    if (nrow(selected_data) > 0) {
      m <- m %>% leaflet::addCircleMarkers(
        data = selected_data,
        lng = ~lng, lat = ~lat,
        radius = 6,
        color = "#FF0000",
        fillColor = "#FF0000",
        fillOpacity = 0.8,
        group = "Selected",
        popup = ~paste("<b>SELECTED</b><br>Ecoregion: ", ecoregion)
      )
    }
    
    if (nrow(not_selected_data) > 0) {
      m <- m %>% leaflet::addCircleMarkers(
        data = not_selected_data, 
        lng = ~lng, lat = ~lat,
        radius = 3,
        color = "#9CA3AF",  # GREY
        fillColor = "#9CA3AF",
        fillOpacity = 0.4,
        group = "Not Selected",
        popup = ~paste("Ecoregion: ", ecoregion)
      )
    }
    
    # Set view
    if (nrow(map_data) > 0) {
      center_lng <- mean(map_data$lng, na.rm = TRUE)
      center_lat <- mean(map_data$lat, na.rm = TRUE)
      m <- m %>% leaflet::setView(lng = center_lng, lat = center_lat, zoom = 6)
    }
    
    # Layer control
    overlay_groups <- c()
    if (nrow(selected_data) > 0) overlay_groups <- c(overlay_groups, "Selected")
    if (nrow(not_selected_data) > 0) overlay_groups <- c(overlay_groups, "Not Selected")
    
    if (length(overlay_groups) > 0) {
      m <- m %>% leaflet::addLayersControl(
        baseGroups = c("Ocean Basemap", "Light Map", "Satellite"),
        overlayGroups = overlay_groups,
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )
    }
    
    # Add legend
    m <- m %>% leaflet::addLegend(
      position = "bottomright",
      colors = c("#FF0000", "#9CA3AF"),
      labels = c("Selected", "Not Selected"),
      title = "Selection Status"
    )
    
    # Save map
    map_file <- file.path(output_dir, "interactive", 
                          paste0("portfolio_map_", gsub(" ", "_", region_name), ".html"))
    dir.create(dirname(map_file), showWarnings = FALSE, recursive = TRUE)
    
    htmlwidgets::saveWidget(m, map_file, selfcontained = FALSE)
    cli::cli_alert_success("Points fallback map saved: {map_file}")
    
    return(m)
    
  }, error = function(e) {
    cli::cli_alert_danger("Points fallback also failed: {e$message}")
    return(NULL)
  })
}


# ADD THIS FUNCTION to test browser compatibility
test_browser_map <- function() {
  cli::cli_h2("Browser Compatibility Test")
  
  # Create a super simple test map
  test_data <- data.frame(
    lng = c(-74.0060, -118.2437, 139.6503, 0),
    lat = c(40.7128, 34.0522, 35.6762, 0),
    city = c("New York", "Los Angeles", "Tokyo", "Center"),
    selected = c(TRUE, FALSE, TRUE, FALSE)
  )
  
  m <- leaflet::leaflet(test_data) %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(
      lng = ~lng, lat = ~lat,
      color = ~ifelse(selected, "red", "blue"),
      popup = ~city
    ) %>%
    leaflet::setView(lng = 0, lat = 0, zoom = 2)
  
  test_file <- file.path(PARENT_FOLDER, "BROWSER_TEST_map.html")
  htmlwidgets::saveWidget(m, test_file, selfcontained = FALSE)
  
  cli::cli_alert_success("Browser test map saved: {test_file}")
  cli::cli_alert_info("PLEASE OPEN THIS FILE IN YOUR BROWSER")
  cli::cli_alert_info("Does this simple test map show up?")
  
  return(test_file)
}

# RUN THIS BEFORE YOUR MAIN LOOP:
test_browser_map()




# Figure + table suite (Fig 1–6b, tables) — filenames preserved
generate_figures_and_tables <- function(analysis_grid, region_name, selection_dir,
                                        region_path, delta_type, region_summary) {
  cli::cli_alert_info("Generating figures and tables…")
  dir.create(file.path(selection_dir, "tables"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(selection_dir, "hybrid_visualizations"), showWarnings = FALSE, recursive = TRUE)
  
  # Attach P2 metrics if we don't yet have effective cover present
  present_col <- intersect(c("cell_cover_present_m2",
                             "effective_cover_present_m2_total_cover",
                             "effective_cover_present_m2"),
                           names(analysis_grid))
  if (!length(present_col)) {
    analysis_grid <- attach_p2_metrics(analysis_grid, region_path, delta_type)
    present_col <- intersect(c("effective_cover_present_m2_total_cover","effective_cover_present_m2"), names(analysis_grid))
  }
  present_col <- present_col[1] %||% NA_character_
  
  
  # Build a robust selection mask once and reuse it
  sel <- analysis_grid$selected
  
  if ("Selected_Cells" %in% names(region_summary)) {
    stopifnot(sum(sel, na.rm = TRUE) == region_summary$Selected_Cells[1])
  }
  
  # tolerate character "TRUE"/"FALSE" etc.
  if (!is.logical(sel)) sel <- tolower(as.character(sel)) %in% c("true","t","1","yes","y")
  sel[is.na(sel)] <- FALSE
  
  present_selected_km2 <- if (!is.na(present_col)) sum(analysis_grid[[present_col]][sel], na.rm = TRUE)/1e6 else NA_real_
  
  
  
  
  # Effective cover totals (km²) if available
  present_total_km2    <- if (!is.na(present_col)) sum(analysis_grid[[present_col]], na.rm = TRUE)/1e6 else NA_real_
  present_selected_km2 <- if (!is.na(present_col)) sum(analysis_grid[[present_col]][analysis_grid$selected], na.rm = TRUE)/1e6 else NA_real_
  present_share        <- if (is.finite(present_total_km2) && present_total_km2 > 0)
    present_selected_km2/present_total_km2 else NA_real_
  
  # Extent protection summary (from region_summary already computed)
  original_extent_km2   <- region_summary$Original_Extent_km2[1]
  protected_extent_km2  <- region_summary$Protected_Extent_km2[1]
  extent_protection_pct <- region_summary$Extent_Protection_Percent[1] / 100
  
  # Save combined grid (WGS84) for any future regen compatibility
  combined_path <- file.path(selection_dir, "combined_grid.rds")
  try({
    saveRDS(sf::st_transform(analysis_grid, 4326), combined_path)
  }, silent = TRUE)
  
  
  # ---- Recompute reef-extent protection locally (strict basis) ----
  AREA_CANDIDATES <- c(
    "pixel_area",                        # P3: reef extent per cell (m²)
    "extent_area_present_m2_total_cover",# P1/P2
    "extent_area_present_m2"             # legacy
  )
  area_col <- intersect(AREA_CANDIDATES, names(analysis_grid))[1]
  if (length(area_col) == 0 || is.na(area_col) || !nzchar(area_col)) {
    cli::cli_abort("P4/figures: No reef-extent column found (looked for: {paste(AREA_CANDIDATES, collapse=', ')}).")
  }
  
  extent_total_m2 <- sum(analysis_grid[[area_col]], na.rm = TRUE)
  extent_sel_m2   <- sum(analysis_grid[[area_col]][sel], na.rm = TRUE)
  extent_pct_num  <- if (extent_total_m2 > 0) 100 * extent_sel_m2 / extent_total_m2 else NA_real_  # numeric percent
  
  # Optional: sanity log + warn if mismatch with region_summary
  rs_pct <- suppressWarnings(as.numeric(region_summary$Extent_Protection_Percent[1]))
  if (is.finite(rs_pct) && is.finite(extent_pct_num) && abs(rs_pct - extent_pct_num) > 1) {
    cli::cli_alert_warning("Figure1 extent % (recomputed={round(extent_pct_num,1)}%) differs from region_summary={round(rs_pct,1)}%")
  }
  
  
  
  # ----- Figure 1: Spatial distribution -----
  grid_wgs <- tryCatch(sf::st_transform(analysis_grid, 4326), error=function(e) analysis_grid)
  grid_wgs <- handle_dateline_crossing(grid_wgs)
  grid_wgs$status <- factor(ifelse(grid_wgs$selected, "Selected", "Not selected"),
                            levels = c("Not selected","Selected"))
  fig1 <- ggplot(grid_wgs) +
    geom_sf(aes(fill = status), color = NA, alpha = 0.95) +
    scale_fill_manual(values = c("Not selected"="#9CA3AF","Selected"="#d73027"), name = "Selection") +
    labs(
      title = "Spatial Distribution of Coral Conservation Priorities",
      subtitle = sprintf(
        "Eff. cover: %s selected | Coral extent: %s protected",
        ifelse(is.finite(present_share), paste0(round(100*present_share,1),"%"), "NA"),
        ifelse(is.finite(extent_pct_num), paste0(round(extent_pct_num,1),"%"), "NA")
      ),
      caption = sprintf(
        "Original extent: %s km² | Protected extent: %s km²",
        ifelse(is.finite(extent_total_m2), round(extent_total_m2/1e6,1), "NA"),
        ifelse(is.finite(extent_sel_m2),   round(extent_sel_m2/1e6,1),   "NA")
      )
    ) +
    theme_void() + theme(legend.position = "bottom")
  
  ggsave(file.path(selection_dir, "Figure1_Spatial_Distribution.png"), fig1, width = 12, height = 8, dpi = 300, bg = "white")
  
  # ----- Figure 2: Z-score distributions -----
  zcols <- c("obj_temporal_z","obj_complementarity_z","obj_risk_z","obj_spatial_z")
  have_z <- intersect(zcols, names(analysis_grid))
  if (length(have_z)) {
    zlong <- analysis_grid |>
      sf::st_drop_geometry() |>
      dplyr::select(selected, dplyr::all_of(have_z)) |>
      tidyr::pivot_longer(-selected, names_to = "Objective", values_to = "Z") |>
      dplyr::mutate(Objective = dplyr::recode(Objective,
                                              obj_temporal_z        = "Coral Cover",
                                              obj_complementarity_z = "Trait Diversity",
                                              obj_risk_z            = "Risk Minimization",
                                              obj_spatial_z         = "Spatial Cohesion"
      ),
      selection_status = ifelse(selected,"Selected","Not Selected"))
    
    fig2 <- ggplot(zlong, aes(Objective, Z, fill = selection_status)) +
      geom_boxplot(alpha=0.85, outlier.shape=NA, width=0.7) +
      geom_jitter(aes(color = selection_status), width=0.2, alpha=0.25, size=0.8) +
      geom_hline(yintercept = 0, linetype="dashed", color="gray40") +
      scale_fill_manual(values = c("Selected"="#d73027","Not Selected"="#9CA3AF"), name="Selection") +
      scale_color_manual(values = c("Selected"="#d73027","Not Selected"="#9CA3AF"), guide = "none") +
      labs(title="Objective Performance (Z-scores)", x=NULL, y="Z-score") +
      theme(axis.text.x = element_text(angle=45, hjust=1), legend.position="bottom")
    ggsave(file.path(selection_dir,"Figure2_ZScore_Performance.png"), fig2, width=10, height=8, dpi=300, bg="white")
  }
  
  # ----- Figure 3 & 3b: Normalized objective scores -----
  raw_cols <- intersect(c("obj_temporal","obj_complementarity","obj_risk","obj_spatial"), names(analysis_grid))
  if (length(raw_cols)) {
    norm_df <- analysis_grid |>
      sf::st_drop_geometry() |>
      dplyr::select(selected, dplyr::all_of(raw_cols)) |>
      tidyr::pivot_longer(-selected, names_to="Objective", values_to="Score") |>
      dplyr::mutate(
        Objective = dplyr::recode(
          Objective,
          obj_temporal="Coral Cover",
          obj_complementarity="Trait Diversity",
          obj_risk="Risk Minimization",
          obj_spatial="Spatial Cohesion"
        )
      ) |>
      dplyr::group_by(Objective) |>
      dplyr::mutate(
        Score_min = min(Score, na.rm = TRUE),
        Score_max = max(Score, na.rm = TRUE),
        Score_rng = pmax(Score_max - Score_min, 0),
        Score_norm = dplyr::if_else(Score_rng > 0, (Score - Score_min)/Score_rng, 0)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-Score_min, -Score_max, -Score_rng)
    
    
    norm_sum <- norm_df |>
      dplyr::group_by(Objective, selected) |>
      dplyr::summarise(n=dplyr::n(), mean=mean(Score_norm,na.rm=TRUE), sd=sd(Score_norm,na.rm=TRUE), .groups="drop") |>
      dplyr::mutate(se = sd/sqrt(pmax(1,n)))
    
    fig3 <- ggplot(norm_sum, aes(Objective, mean, fill = selected)) +
      geom_col(position = position_dodge(width=0.6), width=0.55, alpha=0.9) +
      geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(width=0.6), width=0.2) +
      scale_fill_manual(values = c("TRUE"="#d73027","FALSE"="#9CA3AF"),
                        labels=c("FALSE"="Not Selected","TRUE"="Selected"), name="Selection") +
      labs(title="Normalized Objective Scores (means ± SE)", x=NULL, y="Mean normalized score") +
      theme(axis.text.x = element_text(angle=45, hjust=1), legend.position="bottom")
    ggsave(file.path(selection_dir,"Figure3_Normalized_Objective_Means.png"), fig3, width=12, height=8, dpi=300, bg="white")
    
    fig3b <- ggplot(norm_df,
                    aes(x = Score_norm, y = Objective, fill = ifelse(selected, "Selected","Not Selected"))) +
      ggridges::geom_density_ridges(alpha = 0.7, scale = 0.9, quantile_lines = TRUE, quantiles = c(0.25, 0.5, 0.75)) +
      scale_fill_manual(values = c("Selected"="#d73027","Not Selected"="#9CA3AF"), name = "Selection Status") +
      labs(title = "Normalized Objective Score Distributions",
           x = "Normalized Score (0–1)", y = "Objective") +
      theme(legend.position = "bottom")
    ggsave(file.path(selection_dir,"Figure3b_Ridgeline_Normalized.png"), fig3b, width=12, height=8, dpi=300, bg="white")
  }
  
  # ----- Figure 4: Trade-off (temporal vs complementarity Z) -----
  if (all(c("obj_temporal_z","obj_complementarity_z") %in% names(analysis_grid))) {
    corr <- with(sf::st_drop_geometry(analysis_grid),
                 suppressWarnings(cor(obj_temporal_z, obj_complementarity_z, use = "complete.obs")))
    df4 <- analysis_grid |>
      sf::st_drop_geometry() |>
      dplyr::mutate(selection_status = ifelse(selected,"Selected","Not Selected"))
    fig4 <- ggplot(df4, aes(obj_temporal_z, obj_complementarity_z, color = selection_status)) +
      geom_point(aes(alpha=selection_status), size=2) +
      geom_vline(xintercept=0, linetype="dashed", color="gray60") +
      geom_hline(yintercept=0, linetype="dashed", color="gray60") +
      geom_smooth(method="lm", se=FALSE, color="black", linetype="dotted", linewidth=0.8) +
      annotate("text", x = min(df4$obj_temporal_z, na.rm=TRUE),
               y = max(df4$obj_complementarity_z, na.rm=TRUE),
               hjust = 0, vjust = 1, label = paste0("Correlation: ", round(corr, 3)),
               size = 4, fontface = "italic") +
      scale_color_manual(values=c("Selected"="#d73027","Not Selected"="#9CA3AF"), name="Selection") +
      scale_alpha_manual(values=c("Selected"=0.8,"Not Selected"=0.4), guide="none") +
      labs(title="Trade-off: Coral Cover vs Trait Diversity (Z)",
           x="Coral Cover (Z)", y="Trait Diversity (Z)") +
      theme(legend.position="bottom")
    ggsave(file.path(selection_dir,"Figure4_Tradeoff_Analysis.png"), fig4, width=10, height=8, dpi=300, bg="white")
  }
  
  # ----- Figure 5: Ecoregion representation -----
  if ("ECOREGION" %in% names(analysis_grid)) {
    eco_summ <- analysis_grid |>
      sf::st_drop_geometry() |>
      dplyr::group_by(ECOREGION) |>
      dplyr::summarise(total_cells=dplyr::n(),
                       selected_cells=sum(selected),
                       selection_rate=selected_cells/total_cells, .groups="drop") |>
      dplyr::mutate(ECOREGION = forcats::fct_reorder(ECOREGION, selection_rate))
    fig5 <- ggplot(eco_summ, aes(ECOREGION, selection_rate)) +
      geom_col(fill="#2E8B57", alpha=0.88, width=0.7) +
      geom_text(aes(label=paste0(round(100*selection_rate,0),"%")), hjust=-0.1, size=3.8) +
      coord_flip() +
      scale_y_continuous(labels=scales::percent, limits=c(0, max(eco_summ$selection_rate)*1.15)) +
      labs(title="Grid Selection Rate by Marine Ecoregion", x="Ecoregion", y="Share of grids selected") +
      theme(axis.text.y = element_text(size=10, face="bold"), panel.grid.major.y = element_blank())
    ggsave(file.path(selection_dir,"Figure5_Ecoregion_Representation.png"), fig5, width=12, height=10, dpi=300, bg="white")
  }
  
  # ----- Figure 6a/6b: Dumbbell & stacked means (Z) -----
  if (length(have_z)) {
    stats_by <- analysis_grid |>
      sf::st_drop_geometry() |>
      dplyr::select(selected, dplyr::all_of(have_z)) |>
      tidyr::pivot_longer(-selected, names_to = "Objective", values_to = "Z") |>
      dplyr::mutate(Objective = dplyr::recode(Objective,
                                              obj_temporal_z="Coral Cover (Z)",
                                              obj_complementarity_z="Trait Diversity (Z)",
                                              obj_risk_z="Risk Minimization (Z)",
                                              obj_spatial_z="Spatial Cohesion (Z)")) |>
      dplyr::group_by(Objective, selected) |>
      dplyr::summarise(N=dplyr::n(), Mean=mean(Z,na.rm=TRUE), SD=sd(Z,na.rm=TRUE), .groups="drop")
    
    dumbbell_df <- stats_by |>
      dplyr::select(Objective, selected, Mean) |>
      dplyr::mutate(group = ifelse(selected,"Selected","Not Selected")) |>
      dplyr::select(-selected) |>
      tidyr::pivot_wider(names_from=group, values_from=Mean) |>
      dplyr::mutate(Objective = factor(Objective,
                                       levels=c("Coral Cover (Z)","Trait Diversity (Z)","Risk Minimization (Z)","Spatial Cohesion (Z)")))
    
    fig6a <- ggplot(dumbbell_df, aes(x = `Not Selected`, xend = `Selected`, y = Objective)) +
      geom_segment(aes(y=Objective, yend=Objective), color="gray70", linewidth=1) +
      geom_point(aes(x=`Not Selected`), color="#9CA3AF", size=3) +
      geom_point(aes(x=`Selected`),     color="#d73027", size=3) +
      labs(title="Z-score Means: Selected vs Not Selected", x="Z-score (mean)", y=NULL,
           subtitle = "Points show group means; segments indicate improvement.") +
      theme(panel.grid.major.y = element_blank())
    ggsave(file.path(selection_dir,"Figure6_Dumbbell_ZScore_Means.png"), fig6a, width=9, height=6, dpi=300, bg="white")
    
    perf_bar <- analysis_grid |>
      sf::st_drop_geometry() |>
      dplyr::group_by(selected) |>
      dplyr::summarise(
        `Coral Cover` = mean(obj_temporal_z, na.rm = TRUE),
        `Trait Diversity` = mean(obj_complementarity_z, na.rm = TRUE),
        `Risk Minimization` = mean(obj_risk_z, na.rm = TRUE),
        `Spatial Cohesion` = mean(obj_spatial_z, na.rm = TRUE),
        .groups="drop"
      ) |>
      tidyr::pivot_longer(-selected, names_to = "Objective", values_to = "ZScore") |>
      dplyr::mutate(selection_status = ifelse(selected, "Selected", "Not Selected"),
                    Objective = factor(Objective, levels = c("Coral Cover", "Trait Diversity", "Risk Minimization", "Spatial Cohesion")))
    
    fig6b <- ggplot(perf_bar, aes(x = selection_status, y = ZScore, fill = Objective)) +
      geom_col(position = "dodge", alpha = 0.9) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
      scale_fill_manual(values = c("#d73027", "#fdae61", "#4575b4", "#74add1")) +
      labs(title = "Conservation Objective Performance Profile",
           subtitle = "Average Z-score by selection status",
           x = "Selection Status", y = "Average Z-Score", fill = "Objective") +
      theme(legend.position = "bottom")
    ggsave(file.path(selection_dir,"Figure6b_StackedBar_Performance.png"), fig6b, width=10, height=8, dpi=300, bg="white")
  }
  
  # ----- Tables -----
  # Table: target attainment (present effective cover; 30% target)
  # ----- Tables -----
  # Separate (A) Coral EXTENT (area; 30% target) from (B) Effective coral cover (informative).
  target_share <- 0.30
  
  # Pick an existing per-row AREA column (no recomputation)
  # AREA_CANDIDATES <- c(
  #   "extent_area_present_m2_total_cover",  # reef extent (preferred)
  #   "extent_area_present_m2",
  #   "pixel_area", "pixel_area_m2",
  #   "grid_area_m2"                         # last resort ONLY
  # )
  # area_col <- intersect(AREA_CANDIDATES, names(analysis_grid))[1]
  # if (length(area_col) == 0 || is.na(area_col) || !nzchar(area_col)) {
  #   stop("P4: No reef-extent column found (looked for: ",
  #        paste(AREA_CANDIDATES, collapse = ", "), ").")
  # }
  
  # --- Reef-extent area selection (NO geometry area, NO grid area) ---
  # --- Reef-extent basis ONLY (no geometry area, no grid area) ---
  # Ensure we have a reef-extent column; if missing, attach from P2 now
  AREA_CANDIDATES <- c(
    "pixel_area",                        # preferred (P3)
    "extent_area_present_m2_total_cover",# P2/P1
    "extent_area_present_m2"             # legacy name
  )
  
  if (!any(AREA_CANDIDATES %in% names(analysis_grid))) {
    # try to bring it in from P2 safely
    analysis_grid <- attach_p2_metrics(analysis_grid, region_path, delta_type)
  }
  
  area_col <- intersect(AREA_CANDIDATES, names(analysis_grid))[1]
  if (is.na(area_col) || !nzchar(area_col)) {
    cli::cli_abort("P4: Could not find a reef-extent column; looked for: {paste(AREA_CANDIDATES, collapse=', ')}")
  }
  
  # coerce to numeric just in case
  analysis_grid[[area_col]] <- suppressWarnings(as.numeric(analysis_grid[[area_col]]))
  
  original_total_extent <- sum(analysis_grid[[area_col]], na.rm = TRUE)
  protected_extent      <- sum(analysis_grid[[area_col]][sel], na.rm = TRUE)
  protection_pct        <- if (original_total_extent > 0) 100 * protected_extent / original_total_extent else NA_real_
  
  cli::cli_alert_info(sprintf(
    "Reef-extent basis: %0.1f%% selected (%0.1f / %0.1f km²) via %s",
    protection_pct, protected_extent/1e6, original_total_extent/1e6, area_col
  ))
  
  # convert to km² for region_summary below
  original_extent_km2  <- original_total_extent / 1e6
  protected_extent_km2 <- protected_extent / 1e6
  
  area_col <- intersect(AREA_CANDIDATES, names(analysis_grid))[1]
  
  # --- (A) Extent target attainment by ecoregion (30% target on AREA)
  if (!is.na(area_col) && nzchar(area_col) && "ECOREGION" %in% names(analysis_grid)) {
    eco_extent <- analysis_grid |>
      sf::st_drop_geometry() |>
      dplyr::transmute(ECOREGION, selected, .area_m2 = .data[[area_col]]) |>
      dplyr::group_by(ECOREGION) |>
      dplyr::summarise(
        total_extent_km2     = sum(.area_m2, na.rm = TRUE) / 1e6,
        protected_extent_km2 = sum(ifelse(selected, .area_m2, 0), na.rm = TRUE) / 1e6,
        .groups = "drop"
      ) |>
      dplyr::mutate(
        protection_pct = dplyr::if_else(total_extent_km2 > 0,
                                        protected_extent_km2 / total_extent_km2, NA_real_),
        meets_target = dplyr::case_when(
          is.finite(protection_pct) & protection_pct >= target_share ~ "Yes",
          is.finite(protection_pct)                                  ~ "No",
          TRUE                                                       ~ "NA"
        )
      ) |>
      dplyr::arrange(dplyr::desc(protection_pct))
    
    eco_extent_total <- eco_extent |>
      dplyr::summarise(
        ECOREGION            = "TOTAL",
        total_extent_km2     = sum(total_extent_km2, na.rm = TRUE),
        protected_extent_km2 = sum(protected_extent_km2, na.rm = TRUE),
        protection_pct       = protected_extent_km2 / total_extent_km2,
        meets_target         = ifelse(is.finite(protection_pct) & protection_pct >= target_share, "Yes", "No"),
        .groups = "drop"
      )
    
    extent_out <- dplyr::bind_rows(eco_extent, eco_extent_total) |>
      dplyr::mutate(
        `Total Extent (km²)`       = round(total_extent_km2, 2),
        `Protected Extent (km²)`   = round(protected_extent_km2, 2),
        `Protected % (Area)`       = round(100 * protection_pct, 1)
      ) |>
      dplyr::select(ECOREGION, `Total Extent (km²)`, `Protected Extent (km²)`,
                    `Protected % (Area)`, meets_target)
    
    extent_table <- kbl(
      extent_out,
      caption = sprintf("Ecoregion target attainment — Coral EXTENT (area). Target = %d%% of coral extent protected.",
                        round(100 * target_share)),
      col.names = c("Ecoregion","Total Extent (km²)","Protected Extent (km²)","Protected (%)","Meets 30% Target?"),
      align = c("l","r","r","r","c")
    ) |>
      kable_classic(full_width = FALSE, html_font = "Arial")
    
    out_png <- file.path(selection_dir, "tables", "Table_Target_Attainment.png")
    save_kable(extent_table, out_png)
    try(save_kable(extent_table, sub("\\.png$", ".html", out_png)), silent = TRUE)
  } else {
    cli::cli_alert_warning("No per-row area column found (candidates: {paste(AREA_CANDIDATES, collapse=', ')}). Skipping extent target table.")
  }
  
  # --- (B) Total absolute coral cover protected (present), informative (no target)
  if (!is.na(present_col)) {
    abs_by_eco <- analysis_grid |>
      sf::st_drop_geometry() |>
      dplyr::group_by(ECOREGION) |>
      dplyr::summarise(
        total_abs_km2    = sum(!!rlang::sym(present_col), na.rm = TRUE) / 1e6,
        selected_abs_km2 = sum(ifelse(selected, !!rlang::sym(present_col), 0), na.rm = TRUE) / 1e6,
        .groups = "drop"
      ) |>
      dplyr::mutate(share_selected = dplyr::if_else(total_abs_km2 > 0,
                                                    selected_abs_km2 / total_abs_km2, NA_real_)) |>
      dplyr::arrange(dplyr::desc(share_selected))
    
    abs_total <- abs_by_eco |>
      dplyr::summarise(
        ECOREGION        = "TOTAL",
        total_abs_km2    = sum(total_abs_km2, na.rm = TRUE),
        selected_abs_km2 = sum(selected_abs_km2, na.rm = TRUE),
        share_selected   = selected_abs_km2 / total_abs_km2,
        .groups = "drop"
      )
    
    abs_out <- dplyr::bind_rows(abs_by_eco, abs_total) |>
      dplyr::mutate(
        `Total absolute coral cover (km²)`    = round(total_abs_km2, 2),
        `Selected absolute coral cover (km²)` = round(selected_abs_km2, 2),
        `Selected % (absolute cover)`         = round(100 * share_selected, 1)
      ) |>
      dplyr::select(ECOREGION,
                    `Total absolute coral cover (km²)`,
                    `Selected absolute coral cover (km²)`,
                    `Selected % (absolute cover)`)
    
    abs_table <- kbl(
      abs_out,
      caption = paste0(
        "Total absolute coral cover (present) protected. ",
        "Reported for transparency; no formal 30% target applies to absolute cover."
      ),
      col.names = c("Ecoregion",
                    "Total absolute coral cover (km²)",
                    "Selected absolute coral cover (km²)",
                    "Selected (%)"),
      align = c("l","r","r","r")
    ) |>
      kable_classic(full_width = FALSE, html_font = "Arial")
    
    # Keep the same filename for compatibility; contents now use the new wording.
    out_abs_png <- file.path(selection_dir, "tables", "Table_Effective_Cover_Protected.png")
    save_kable(abs_table, out_abs_png)
    try(save_kable(abs_table, sub("\\.png$", ".html", out_abs_png)), silent = TRUE)
    
    # Optional: also write an alias file with the new name (non-breaking)
    try({
      out_abs_png2 <- file.path(selection_dir, "tables", "Table_Total_Absolute_Coral_Cover_Protected.png")
      save_kable(abs_table, out_abs_png2)
      save_kable(abs_table, sub("\\.png$", ".html", out_abs_png2))
    }, silent = TRUE)
  }
  
  
  cli::cli_alert_success("Tables generated using existing area column (no recomputation).")
}  



create_summary_report <- function(region_summary, analysis_grid, output_dir) {
  # Keep minimal CSV + static selection map (as in your previous version)
  cli::cli_alert_info("Creating summary report…")
  tryCatch({
    summary_stats <- data.frame(
      Metric = c("Total Grid Cells","Selected Cells","Selection Rate (%)",
                 "Original Extent (km²)","Protected Extent (km²)","Protection Rate (%)",
                 "Geometry Type","Hybrid Geometry"),
      Value = c(
        format(region_summary$Total_Grid_Cells, big.mark = ","),
        format(region_summary$Selected_Cells, big.mark = ","),
        round(region_summary$Selection_Rate, 1),
        round(region_summary$Original_Extent_km2, 1),
        round(region_summary$Protected_Extent_km2, 1),
        round(region_summary$Extent_Protection_Percent, 1),
        region_summary$Grid_Type,
        ifelse(region_summary$Hybrid_Geometry_Available, "Yes", "No")
      )
    )
    summary_file <- file.path(output_dir, "tables", "portfolio_summary.csv")
    write.csv(summary_stats, summary_file, row.names = FALSE)
    cli::cli_alert_success("Saved summary table: {summary_file}")
    
    if (nrow(analysis_grid) > 0) {
      sel_plot <- ggplot() +
        geom_sf(data = analysis_grid, aes(fill = selected), color = NA, alpha = 0.8) +
        scale_fill_manual(values = c("FALSE" = "lightblue", "TRUE" = "red")) +
        labs(title = paste("Portfolio Selection —", region_summary$Region),
             subtitle = paste("Selection rate:", region_summary$Selection_Rate, "%"),
             fill = "Selected") +
        theme_minimal() +
        theme(legend.position = "bottom")
      plot_file <- file.path(output_dir, "hybrid_visualizations",
                             paste0("selection_map_", gsub(" ", "_", region_summary$Region), ".png"))
      ggsave(plot_file, sel_plot, width = 10, height = 8, dpi = 300)
      cli::cli_alert_success("Saved selection map: {plot_file}")
    }
    summary_stats
  }, error = function(e) {
    cli::cli_alert_warning("Summary report failed: {e$message}")
    NULL
  })
}


# ---- MAIN PER-REGION FUNCTION ----
process_region_pipeline4 <- function(region_name, region_path, delta_type) {
  cli::cli_h2("Processing Region: {region_name}")
  tryCatch({
    oldwd <- getwd(); setwd(region_path); on.exit(setwd(oldwd), add = TRUE)
    
    selection_dir <- file.path(region_path, "Selection")
    if (!dir.exists(selection_dir)) dir.create(selection_dir, recursive = TRUE)
    dir.create(file.path(selection_dir, "interactive"), showWarnings = FALSE, recursive = TRUE)
    dir.create(file.path(selection_dir, "tables"),      showWarnings = FALSE, recursive = TRUE)
    dir.create(file.path(selection_dir, "hybrid_visualizations"), showWarnings = FALSE, recursive = TRUE)
    
    delta_suffix <- gsub("-", "_", delta_type)
    portfolio_file <- file.path(selection_dir, paste0("portfolio_optimization_results_", delta_suffix, ".rds"))
    if (!file.exists(portfolio_file)) {
      cli::cli_alert_warning("Pipeline 3 output not found: {portfolio_file}")
      return(NULL)
    }
    
    gurobi_results <- readRDS(portfolio_file)
    cli::cli_alert_success("Loaded Pipeline 3 results")
    
    # --- Preferred path: use P3 grids directly (already carry 'selected') ---
    analysis_grid <- combine_p3_grids(gurobi_results)
    if (!is.null(analysis_grid) && inherits(analysis_grid, "sf") && is.na(sf::st_crs(analysis_grid))) {
      analysis_grid <- ensure_region_crs(analysis_grid, region_path)
    }
    
    # If combine failed (e.g., non-sf), use hybrid (grid_id join → NN in projected CRS)
    used_hybrid <- FALSE
    if (is.null(analysis_grid) || !inherits(analysis_grid, "sf")) {
      used_hybrid <- TRUE
      selection_data <- extract_selection_data(gurobi_results, region_path)
      if (is.null(selection_data)) {
        cli::cli_alert_danger("No selection data; cannot proceed for {region_name}")
        return(NULL)
      }
      p1_polys <- load_original_polygons(region_path)
      if (is.null(p1_polys)) {
        cli::cli_alert_warning("No P1 polygons; using point-based analysis in projected CRS")
        crs_proj <- attr(selection_data, "crs") %||% read_saved_crs(region_path)
        analysis_grid <- sf::st_as_sf(selection_data, coords = c("x","y"),
                                      crs = crs_proj %||% sf::NA_crs_)
        analysis_grid <- ensure_region_crs(analysis_grid, region_path)
      } else {
        hyb <- create_hybrid_geometry(selection_data, p1_polys, region_path)
        analysis_grid <- hyb$data
      }
    }
    
    if (!"selected" %in% names(analysis_grid)) {
      cli::cli_alert_warning("'selected' flag missing; defaulting to FALSE")
      analysis_grid$selected <- FALSE
    }
    if (!"ECOREGION" %in% names(analysis_grid)) {
      analysis_grid$ECOREGION <- NA_character_
    }
    
    analysis_grid <- suppressWarnings(sf::st_make_valid(analysis_grid))
    analysis_grid$selected[is.na(analysis_grid$selected)] <- FALSE
    
    sel_count <- sum(analysis_grid$selected, na.rm = TRUE)
    cli::cli_alert_info("Analysis grid: {nrow(analysis_grid)} features; {sel_count} selected")
    
    analysis_grid <- suppressWarnings(sf::st_make_valid(analysis_grid))
    analysis_grid$selected[is.na(analysis_grid$selected)] <- FALSE
    
    # # --- Extents (aligned to P1–P3) ---
    # has_polygons <- detect_geometry_type(analysis_grid) %in% c("POLYGON","MULTIPOLYGON","mixed")
    # original_total_extent <- calculate_grid_extent(analysis_grid, use_geometry_area = has_polygons)
    # 
    # # protected extent
    # if (sel_count > 0) {
    #   if (has_polygons) {
    #     protected_extent <- as.numeric(sum(sf::st_area(analysis_grid[analysis_grid$selected, ]), na.rm = TRUE))
    #   } else {
    #     avg_cell <- original_total_extent / nrow(analysis_grid)
    #     protected_extent <- sel_count * avg_cell
    #   }
    # } else {
    #   protected_extent <- 0
    # }
    # 
    # protection_pct <- if (original_total_extent > 0) 100 * protected_extent / original_total_extent else NA_real_
    # original_extent_km2  <- original_total_extent / 1e6
    # protected_extent_km2 <- protected_extent / 1e6
    # grid_type_label <- if (has_polygons) "Polygon/Hexagon" else "Point"
    # 
    # 
    # # --- Effective cover (present) selection summary (km² + %) ---
    # present_candidates <- c(
    #   "effective_cover_present_m2_total_cover",  # preferred (from P2)
    #   "effective_cover_present_m2",              # fallback
    #   "cell_cover_present_m2"                    # legacy name if present
    # )
    # present_col <- intersect(present_candidates, names(analysis_grid))[1]
    # 
    # # If missing, try attaching from P2 on disk
    # if (is.na(present_col) || is.null(present_col)) {
    #   analysis_grid <- attach_p2_metrics(analysis_grid, region_path, delta_type)
    #   present_col <- intersect(present_candidates, names(analysis_grid))[1]
    # }
    # 
    # eff_total_km2 <- if (!is.na(present_col)) sum(analysis_grid[[present_col]], na.rm = TRUE) / 1e6 else NA_real_
    # eff_sel_km2   <- if (!is.na(present_col)) sum(analysis_grid[[present_col]][analysis_grid$selected], na.rm = TRUE) / 1e6 else NA_real_
    # eff_sel_pct   <- if (is.finite(eff_total_km2) && eff_total_km2 > 0) 100 * eff_sel_km2 / eff_total_km2 else NA_real_
    # 
    # cli::cli_alert_info(
    #   "Effective cover selected: {ifelse(is.finite(eff_sel_km2), round(eff_sel_km2,1), NA)} km² of {ifelse(is.finite(eff_total_km2), round(eff_total_km2,1), NA)} km² ({ifelse(is.finite(eff_sel_pct), round(eff_sel_pct,1), NA)}%)"
    # )
    # 
    # 
    # cli::cli_alert_success("Protected extent: {round(protected_extent_km2,2)} km² of {round(original_extent_km2,2)} km² ({round(protection_pct,1)}%)")
    # 
    # # --- Region summary + outputs (filenames preserved) ---
    # region_summary <- data.frame(
    #   Region = region_name,
    #   Grid_Type = grid_type_label,
    #   Total_Grid_Cells = nrow(analysis_grid),
    #   Selected_Cells = sel_count,
    #   Selection_Rate = if (nrow(analysis_grid) > 0) round(100 * sel_count / nrow(analysis_grid), 1) else 0,
    #   Original_Extent_km2 = round(original_extent_km2, 2),
    #   Protected_Extent_km2 = round(protected_extent_km2, 2),
    #   Extent_Protection_Percent = round(protection_pct, 1),
    #   Effective_Cover_Selected_km2 = round(eff_sel_km2, 2),
    #   Effective_Cover_Selected_Percent = round(eff_sel_pct, 1),
    #   Hybrid_Geometry_Available = used_hybrid || has_polygons,
    #   CRS_Used = sf::st_crs(analysis_grid)$input %||% "WGS84",
    #   stringsAsFactors = FALSE
    # )
    # 
    # 
    # write.csv(region_summary, file.path(selection_dir, "region_summary.csv"), row.names = FALSE)
    # cli::cli_alert_success("Saved region summary")
    # 
    # --- Reef-extent basis ONLY for region_summary ---
    # Build selection mask once
    sel <- analysis_grid$selected
    if (!is.logical(sel)) sel <- tolower(as.character(sel)) %in% c("true","t","1","yes","y")
    sel[is.na(sel)] <- FALSE
    
    # Use the same reef-extent column candidates as the figures/tables
    AREA_CANDIDATES <- c("pixel_area", "extent_area_present_m2_total_cover", "extent_area_present_m2")
    if (!any(AREA_CANDIDATES %in% names(analysis_grid))) {
      analysis_grid <- attach_p2_metrics(analysis_grid, region_path, delta_type)
    }
    area_col <- intersect(AREA_CANDIDATES, names(analysis_grid))[1]
    stopifnot(!is.na(area_col) && nzchar(area_col))
    analysis_grid[[area_col]] <- suppressWarnings(as.numeric(analysis_grid[[area_col]]))
    
    # Compute reef-extent totals (m²)
    original_total_extent <- sum(analysis_grid[[area_col]], na.rm = TRUE)
    protected_extent      <- sum(analysis_grid[[area_col]][sel], na.rm = TRUE)
    protection_pct        <- if (original_total_extent > 0) 100 * protected_extent / original_total_extent else NA_real_
    
    # (optional) effective cover for reporting
    present_col <- intersect(c("effective_cover_present_m2_total_cover","effective_cover_present_m2",
                               "cell_cover_present_m2"), names(analysis_grid))[1]
    eff_sel_km2 <- if (!is.na(present_col)) sum(analysis_grid[[present_col]][sel], na.rm=TRUE)/1e6 else NA_real_
    eff_tot_km2 <- if (!is.na(present_col)) sum(analysis_grid[[present_col]],    na.rm=TRUE)/1e6 else NA_real_
    eff_pct     <- if (is.finite(eff_tot_km2) && eff_tot_km2 > 0) 100*eff_sel_km2/eff_tot_km2 else NA_real_
    
    # Build region_summary ON THIS BASIS
    original_extent_km2  <- original_total_extent / 1e6
    protected_extent_km2 <- protected_extent      / 1e6
    grid_type_label <- if (detect_geometry_type(analysis_grid) %in% c("POLYGON","MULTIPOLYGON","mixed")) "Polygon/Hexagon" else "Point"
    
    region_summary <- data.frame(
      Region = region_name,
      Grid_Type = grid_type_label,
      Total_Grid_Cells = nrow(analysis_grid),
      Selected_Cells   = sum(sel),
      Selection_Rate   = if (nrow(analysis_grid) > 0) round(100 * mean(sel), 1) else 0,
      Original_Extent_km2          = round(original_extent_km2, 2),
      Protected_Extent_km2         = round(protected_extent_km2, 2),
      Extent_Protection_Percent    = round(protection_pct, 1),
      Effective_Cover_Selected_km2 = if (is.finite(eff_sel_km2)) round(eff_sel_km2, 2) else NA_real_,
      Effective_Cover_Selected_Percent = if (is.finite(eff_pct)) round(eff_pct, 1) else NA_real_,
      Hybrid_Geometry_Available = detect_geometry_type(analysis_grid) %in% c("POLYGON","MULTIPOLYGON","mixed"),
      CRS_Used = sf::st_crs(analysis_grid)$input %||% "WGS84",
      stringsAsFactors = FALSE
    )
    
    write.csv(region_summary, file.path(selection_dir, "region_summary.csv"), row.names = FALSE)
    cli::cli_alert_success("Saved region summary (reef-extent basis via {area_col})")
    
    
    
    # Map + report (as before)
    cli::cli_alert_info("Map diag — n={nrow(analysis_grid)}, crs={sf::st_crs(analysis_grid)$input %||% 'NA'}, empty={sum(sf::st_is_empty(analysis_grid))}")
    # --- Upstream quick sanity (before mapping) ---
    cli::cli_alert_info("Upstream diag: crs={sf::st_crs(analysis_grid)$input %||% 'NA'}; is_longlat={sf::st_is_longlat(analysis_grid)}")
    print(head(sf::st_bbox(analysis_grid)))
    print(table(sf::st_is_empty(analysis_grid)))
    print(table(analysis_grid$selected, useNA = "ifany"))
    # --- end sanity ---
    
    create_interactive_map(analysis_grid, region_name, selection_dir, region_path)
    
    create_summary_report(region_summary, analysis_grid, selection_dir)
    
    # Full figure/table suite (new) — filenames preserved
    generate_figures_and_tables(analysis_grid, region_name, selection_dir,
                                region_path, delta_type, region_summary)
    
    # --- Add region-level effective-cover metrics into analysis_grid before saving ---
    present_candidates <- c(
      "effective_cover_present_m2_total_cover",
      "effective_cover_present_m2",
      "cell_cover_present_m2"
    )
    present_col <- intersect(present_candidates, names(analysis_grid))[1]
    
    if (!is.na(present_col) && nzchar(present_col)) {
      present_total_km2    <- sum(analysis_grid[[present_col]], na.rm = TRUE) / 1e6
      present_selected_km2 <- sum(analysis_grid[[present_col]][isTRUE(analysis_grid$selected)], na.rm = TRUE) / 1e6
      present_share_pct    <- if (present_total_km2 > 0) 100 * present_selected_km2 / present_total_km2 else NA_real_
      
      # Optional: record which reef-extent area column was used in this region (helpful provenance)
      extent_candidates <- c("pixel_area", "extent_area_present_m2_total_cover", "extent_area_present_m2")
      extent_col <- intersect(extent_candidates, names(analysis_grid))[1] %||% NA_character_
      
      # Store as attributes (not duplicated across rows)
      attr(analysis_grid, "Effective_Cover_Selected_km2")     <- present_selected_km2
      attr(analysis_grid, "Effective_Cover_Selected_Percent") <- present_share_pct
      attr(analysis_grid, "Effective_Cover_Present_Column")   <- present_col
      attr(analysis_grid, "Reef_Extent_Area_Column")          <- extent_col
      
      # Also store as constant columns so GIS/table tools can see them
      analysis_grid$effective_cover_selected_km2_global     <- rep(present_selected_km2, nrow(analysis_grid))
      analysis_grid$effective_cover_selected_percent_global <- rep(present_share_pct,    nrow(analysis_grid))
      
      cli::cli_alert_success(
        "Stamped effective-cover into analysis_grid: {round(present_selected_km2,1)} km² ({round(present_share_pct,1)}%)"
      )
    } else {
      cli::cli_alert_warning("No effective-cover column found (looked for: {paste(present_candidates, collapse=', ')}) — skipping stamp.")
    }
    
    # Save analysis grid for downstream use (kept filename)
    analysis_grid_file <- file.path(selection_dir, "analysis_grid.rds")
    saveRDS(analysis_grid, analysis_grid_file)
    cli::cli_alert_success("Saved analysis grid: {analysis_grid_file}")
    
    cli::cli_alert_success("Completed Pipeline 4 for {region_name}")
    region_summary
  }, error = function(e) {
    cli::cli_alert_danger("Error processing region {region_name}: {e$message}")
    NULL
  })
}

# ─────────────────────────────────────────────────────────────────────────────
# MAIN EXECUTION
# ─────────────────────────────────────────────────────────────────────────────

cli::cli_h1("Coral Conservation Portfolio - Pipeline 4: Complete Analysis")
cli::cli_alert_info("FEATURES:")
cli::cli_alert_info("✓ Direct use of P3 selection grids (preferred)")
cli::cli_alert_info("✓ grid_id-based hybrid transfer (fallback), then NN (projected)")
cli::cli_alert_info("✓ CRS reuse from P1 sidecars; auto-assign if NA")
cli::cli_alert_info("✓ Interactive maps, summary CSVs, and full figure/table suite")

all_region_summaries <- list()

for (region_name in names(REGION_CHECKLIST)) {
  if (isTRUE(REGION_CHECKLIST[[region_name]])) {
    region_path <- file.path(PARENT_FOLDER, region_name)
    if (!dir.exists(region_path)) {
      cli::cli_alert_warning("Region folder does not exist: {region_path}. Skipping.")
      next
    }
    rs <- process_region_pipeline4(region_name, region_path, DELTA_TYPE)
    if (!is.null(rs)) all_region_summaries[[region_name]] <- rs
    gc()
  } else {
    cli::cli_alert_info("Skipping region (disabled in checklist): {region_name}")
  }
}

# Global summaries (filenames preserved)
if (length(all_region_summaries) > 0) {
  global_summary <- dplyr::bind_rows(all_region_summaries)
  hybrid_regions <- sum(global_summary$Hybrid_Geometry_Available)
  total_regions  <- nrow(global_summary)
  
  global_totals <- data.frame(
    Metric = c("Total Original Extent","Total Protected Extent",
               "Global Protection Rate","Regions with Hybrid Geometry","Hybrid Implementation Rate"),
    Value  = c(
      paste0(round(sum(global_summary$Original_Extent_km2), 1), " km²"),
      paste0(round(sum(global_summary$Protected_Extent_km2), 1), " km²"),
      paste0(round(100 * sum(global_summary$Protected_Extent_km2) / sum(global_summary$Original_Extent_km2), 1), "%"),
      paste0(hybrid_regions, " of ", total_regions, " regions"),
      paste0(round(100 * hybrid_regions / total_regions, 1), "%")
    )
  )
  
  write.csv(global_summary, file.path(PARENT_FOLDER, "global_portfolio_summary.csv"), row.names = FALSE)
  write.csv(global_totals, file.path(PARENT_FOLDER, "global_protection_totals.csv"), row.names = FALSE)
  
  cli::cli_h1("Pipeline 4 Complete")
  cli::cli_alert_success("Processed {length(all_region_summaries)} regions successfully")
  cli::cli_alert_success("Global coral extent protection: {global_totals$Value[3]}")
  cli::cli_alert_success("Hybrid geometry available in {hybrid_regions}/{total_regions} regions")
} else {
  cli::cli_alert_warning("No regions were successfully processed")
}

cli::cli_alert_info("R session info:")
cli::cli_alert_info("R version: {R.version$version.string}")
cli::cli_alert_info("Platform: {R.version$platform}")
cli::cli_alert_info("Loaded packages: {paste(.packages(), collapse=', ')}")
