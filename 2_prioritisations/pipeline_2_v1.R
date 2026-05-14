# ================================
# PIPELINE 2 — CALCULATING METRICS 
# ================================
rm(list=ls())

# ---- Packages ----
suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(glue)
  library(cli)
  library(sf)
  library(logger)
  library(future)
  library(furrr)
})


STRICT_REEF_EXTENT <- TRUE
options(datatable.verbose = FALSE)
sf::sf_use_s2(FALSE)

# ---- Logging ----
log_dir <- "logs"
if (!dir.exists(log_dir)) dir.create(log_dir)
logger::log_appender(logger::appender_file(file.path(log_dir, "pipeline2_conservation.log")))
logger::log_layout(logger::layout_glue)
logger::log_info("Pipeline 2 started at {Sys.time()}")

# ---- Parallel ----
available_cores <- future::availableCores()
workers_to_use  <- min(available_cores - 2, 12)
plan(multisession, workers = workers_to_use)
logger::log_info("Parallel: {workers_to_use} workers")

# ---- User Config ----
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

# ---- Constants & helpers ----
`%||%` <- function(a,b) if (!is.null(a)) a else b

TRAIT_CORE <- c("competitive","stress_tolerant","weedy")
ALL_LHT    <- c(TRAIT_CORE, "total_cover")

active_trait_set <- function(wide_dt) {
  have_effP <- paste0("effective_cover_present_m2_", TRAIT_CORE)
  present   <- TRAIT_CORE[vapply(have_effP, function(cn) cn %in% names(wide_dt), logical(1))]
  list(core_avail = present, n_core = length(present))
}

# Projections (parity with P1)
.get_region_bbox <- function(region_path) {
  grid_files <- c(
    Sys.glob(file.path(region_path, "*_5k.csv")),
    Sys.glob(file.path(region_path, "*_hex_*.csv")),
    Sys.glob(file.path(region_path, "*_square_*.csv"))
  )
  if (!length(grid_files)) return(list(min_lon=-180,max_lon=180,min_lat=-90,max_lat=90))
  sample_dt <- fread(grid_files[1], nrows = 1000)
  list(
    min_lon = min(sample_dt$grid_lon, na.rm=TRUE),
    max_lon = max(sample_dt$grid_lon, na.rm=TRUE),
    min_lat = min(sample_dt$grid_lat, na.rm=TRUE),
    max_lat = max(sample_dt$grid_lat, na.rm=TRUE)
  )
}
get_azimuthal <- function(lon0, lat0) {
  sprintf('+proj=laea +lat_0=%f +lon_0=%f +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs', lat0, lon0)
}

# Prefer CRS recorded by Pipeline 1 (sidecar files)
try_get_p1_crs <- function(region_path) {
  crs_rds <- file.path(region_path, "grid_crs.rds")
  crs_txt <- file.path(region_path, "grid_crs_wkt.txt")
  
  if (file.exists(crs_rds)) {
    sc <- tryCatch(readRDS(crs_rds), error = function(e) NULL)
    if (!is.null(sc)) {
      if (!is.null(sc$wkt)   && nzchar(sc$wkt))   return(sc$wkt)
      if (!is.null(sc$input) && nzchar(sc$input)) return(sc$input)
      if (!is.null(sc$epsg)  && is.finite(sc$epsg)) return(paste0("EPSG:", sc$epsg))
    }
  }
  if (file.exists(crs_txt)) {
    w <- tryCatch(readLines(crs_txt, warn = FALSE), error = function(e) NULL)
    if (!is.null(w) && length(w)) return(paste(w, collapse = "\n"))
  }
  NULL
}

# Compute an IDL-safe local LAEA from a bbox (matches P1 logic)
compute_laea_crs_from_bbox <- function(bbox) {
  lon <- c(as.numeric(bbox$min_lon), as.numeric(bbox$max_lon))
  lat <- c(as.numeric(bbox$min_lat), as.numeric(bbox$max_lat))
  lon_rad <- lon * pi/180
  cx <- atan2(mean(sin(lon_rad)), mean(cos(lon_rad))) * 180/pi
  if (cx >  180) cx <- cx - 360
  if (cx < -180) cx <- cx + 360
  cy <- mean(lat)
  get_azimuthal(cx, cy)
}


get_region_projection <- function(region_path) {
  # 1) Use CRS written by Pipeline 1, if present
  p1_crs <- try_get_p1_crs(region_path)
  if (!is.null(p1_crs)) {
    cli_alert_info("Using CRS from Part 1 sidecar.")
    return(p1_crs)
  }
  
  # 2) Fallback: infer from grid files and compute a local LAEA if needed
  hex_files    <- Sys.glob(file.path(region_path, "*_hex_*.csv"))
  square_files <- Sys.glob(file.path(region_path, "*_square_*.csv"))
  geo_files    <- Sys.glob(file.path(region_path, "*_5k.csv"))
  
  if (length(hex_files) || length(square_files)) {
    cli_alert_info("Detected hex/square grid → computing local LAEA from bbox.")
    bbox <- .get_region_bbox(region_path)
    return(compute_laea_crs_from_bbox(bbox))
  } else if (length(geo_files)) {
    cli_alert_info("Detected geographic grid → WGS84 (EPSG:4326).")
    return("EPSG:4326")
  } else {
    cli_warn("No grid files detected → WGS84 fallback (EPSG:4326).")
    return("EPSG:4326")
  }
}


# Delta helpers
calculate_delta <- function(wide_dt, delta_type, lht) {
  present <- paste0("effective_cover_present_m2_", lht)
  future  <- paste0("effective_cover_future_m2_",  lht)
  hist    <- paste0("effective_cover_historical_m2_", lht)
  switch(delta_type,
         "present-future" = if (all(c(present,future) %in% names(wide_dt))) wide_dt[[present]] - wide_dt[[future]] else rep(0, nrow(wide_dt)),
         "historical-present" = if (all(c(hist,present) %in% names(wide_dt))) wide_dt[[hist]] - wide_dt[[present]] else rep(0, nrow(wide_dt)),
         "historical-future"  = if (all(c(hist,future) %in% names(wide_dt))) wide_dt[[hist]] - wide_dt[[future]]  else rep(0, nrow(wide_dt)),
         { if (all(c(present,future) %in% names(wide_dt))) wide_dt[[present]] - wide_dt[[future]] else rep(0, nrow(wide_dt)) }
  )
}
get_delta_denominator <- function(wide_dt, delta_type, lht) {
  present <- paste0("effective_cover_present_m2_", lht)
  hist    <- paste0("effective_cover_historical_m2_", lht)
  switch(delta_type,
         "present-future"      = if (present %in% names(wide_dt)) pmax(wide_dt[[present]], 1.0) else rep(1.0, nrow(wide_dt)),
         "historical-present"  = if (hist %in% names(wide_dt))    pmax(wide_dt[[hist]], 1.0)    else rep(1.0, nrow(wide_dt)),
         "historical-future"   = if (hist %in% names(wide_dt))    pmax(wide_dt[[hist]], 1.0)    else rep(1.0, nrow(wide_dt)),
         if (present %in% names(wide_dt)) pmax(wide_dt[[present]], 1.0) else rep(1.0, nrow(wide_dt))
  )
}

#  save with CRS sidecar
#  RDS writer (atomic, retries, safe sidecar)
safe_saveRDS <- function(object, file, crs_obj = NULL, retries = 3L, sleep_sec = 0.5) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  
  # Basic checks (don't error – just warn and continue to attempt save)
  if (!dir.exists(dirname(file))) {
    warning("safe_saveRDS: output directory does not exist: ", dirname(file))
  }
  # NOTE: file.access can be unreliable on some systems; warn only
  if (file.access(dirname(file), 2) != 0) {
    warning("safe_saveRDS: may not have write permission to: ", dirname(file))
  }
  
  attempt  <- 1L
  ok       <- FALSE
  last_msg <- NULL
  
  # temp path for atomic write
  tmp <- file.path(dirname(file), paste0(".tmp_", basename(file), "_", Sys.getpid(), "_", sample.int(1e6, 1)))
  on.exit(try(unlink(tmp), silent = TRUE), add = TRUE)
  
  while (attempt <= retries && !ok) {
    try({
      # write to temp
      saveRDS(object, tmp)
      # atomic move on same filesystem
      if (!file.rename(tmp, file)) {
        # fallback: copy then remove
        if (!file.copy(tmp, file, overwrite = TRUE)) {
          last_msg <- "file.copy returned FALSE"
          stop("Atomic rename failed and fallback copy failed")
        }
        unlink(tmp)
      }
      ok <- TRUE
    }, silent = TRUE) -> res
    
    if (!ok) {
      # capture best available error text
      if (inherits(res, "try-error")) last_msg <- as.character(res)
      Sys.sleep(sleep_sec)
      attempt <- attempt + 1L
    }
  }
  
  if (!ok) {
    stop(paste0("safe_saveRDS failed for ", file, if (!is.null(last_msg)) paste0(" :: ", last_msg) else ""))
  }
  
  # Write CRS sidecar safely (don't fail region on sidecar issues)
  if (inherits(object, "sf")) {
    # prefer explicit crs_obj if provided; else derive from object
    crs <- crs_obj %||% tryCatch(sf::st_crs(object), error = function(e) NULL)
    
    sidecar <- list(
      input    = if (!is.null(crs)) crs$input else NULL,
      epsg     = if (!is.null(crs)) crs$epsg  else NULL,
      wkt      = if (!is.null(crs)) crs$wkt   else NULL,
      saved_at = as.character(Sys.time())
    )
    
    sc_path <- sub("\\.rds$", "_crs.rds", file, ignore.case = TRUE)
    tryCatch(saveRDS(sidecar, sc_path),
             error = function(e) warning("safe_saveRDS: failed to write CRS sidecar: ", conditionMessage(e)))
  }
  
  invisible(TRUE)
}

# ---- Parameters (aligned with P1) ----
params <- list(
  temporal = list(
    beta = 0.8,
    delta_weight = 1,
    risk_weight = 0.7,
    conserve_weight = 0.4,
    delta_type = "present-future"  # or "historical-present", "historical-future"
  ),
  spatial = list(
    grid_res = 0.04,
    chunk_size = 1e5,
    excluded_ecoregions = c("Cape Verde",""),
    target_coverage = 0.3
  ),
  optimization = list(k_nn = 15, workers = max(1, parallel::detectCores()-2)),
  conservation = list(
    use_effective_cover = TRUE,
    pixel_area_m2 = 250*250,
    trait_threshold = 0.05,
    use_density_for_objectives = FALSE,
    convert_to_km2 = FALSE,
    min_threshold = 1e-6
  )
)

# ========================
# CORE PER-REGION FUNCTION
# ========================
process_region_conservation <- function(region_name, region_path) {
  logger::log_info("=== Region: {region_name} ===")
  original_wd <- getwd(); setwd(region_path); on.exit(setwd(original_wd))
  cli_h2("Conservation Processing for Region: {region_name}")
  
  tryCatch({
    # 1) Load Part 1 output
    cli_progress_step("Loading data from Part 1 output")
    .pick_input_file <- function() {
      cand <- c(Sys.glob("*_hex_*.csv"), Sys.glob("*_square_*.csv"), Sys.glob("*_5k.csv"))
      if (!length(cand)) cli_abort("No grid file found in {getwd()}.")
      if (length(cand)>1) cli_alert_warning("Multiple grid files; using first: {cand[1]}")
      cand[1]
    }
    input_file <- .pick_input_file()
    cli_alert_info("Using grid file: {input_file}")
    grid_dt <- fread(input_file, showProgress = FALSE)
    setDT(grid_dt)
    
    # Mint stable grid_id if missing (before any pivots)
    # Only mint grid_id if missing or blank
    if (!"grid_id" %in% names(grid_dt)) {
      cli::cli_alert_info("grid_id column not present; creating it from lon/lat.")
      grid_dt[, grid_id := NA_character_]
    }
    
    n_na <- sum(is.na(grid_dt$grid_id) | grid_dt$grid_id == "")
    if (n_na > 0) {
      cli::cli_alert_info("Minting grid_id for {n_na} rows from lon/lat (stable).")
      grid_dt[is.na(grid_id) | grid_id == "",
              grid_id := sprintf(
                "px_%s_%s",
                format(round(grid_lon, 6), nsmall = 6, trim = TRUE),
                format(round(grid_lat, 6), nsmall = 6, trim = TRUE)
              )]
    } else {
      cli::cli_alert_success("grid_id present for all rows — no minting needed.")
    }
    
    # character consistently
    grid_dt[, grid_id := as.character(grid_id)]
    
    
    # Required columns
    required_cols <- c(
      "grid_id","grid_lon","grid_lat","life_history","ECOREGION",
      "effective_cover_present_m2","effective_cover_future_m2",
      "sd_present_m2","sd_future_m2","grid_area_m2"
    )
    missing_required <- setdiff(required_cols, names(grid_dt))
    if (length(missing_required)) cli_abort("Missing required columns from Part 1: {paste(missing_required, collapse=', ')}")
    
    # Optionals / delta_type
    delta_type <- params$temporal$delta_type
    if (delta_type %in% c("historical-present","historical-future")) {
      if (!"effective_cover_historical_m2" %in% names(grid_dt))
        cli_abort("delta_type={delta_type} requires historical columns in Part 1 output.")
      if (!"sd_historical_m2" %in% names(grid_dt))
        cli_abort("delta_type={delta_type} requires sd_historical_m2 in Part 1 output.")
    }
    
    # if (!"extent_area_present_m2" %in% names(grid_dt)) {
    #   cli_alert_warning("extent_area_present_m2 missing; using grid_area_m2 fallback.")
    #   grid_dt[, extent_area_present_m2 := grid_area_m2]
    # }
    if (!"extent_area_present_m2" %in% names(grid_dt)) {
      if (isTRUE(STRICT_REEF_EXTENT)) {
        cli::cli_abort("P2: Missing 'extent_area_present_m2' in Part 1 output — supply reef extent from P1.")
      } else {
        cli::cli_alert_warning("P2: extent missing; falling back to 'grid_area_m2' (NOT reef extent).")
        grid_dt[, extent_area_present_m2 := grid_area_m2]
      }
    }
    
    # 2) Clean & filter
    cli_progress_step("Filtering data (aligned with Part 1 validation)")
    cover_cols <- c("effective_cover_present_m2","effective_cover_future_m2")
    if ("effective_cover_historical_m2" %in% names(grid_dt)) cover_cols <- c(cover_cols,"effective_cover_historical_m2")
    for (cc in cover_cols) grid_dt[[cc]] <- pmax(0, grid_dt[[cc]])
    
    grid_dt <- grid_dt[
      ECOREGION != "" &
        !ECOREGION %in% params$spatial$excluded_ecoregions &
        complete.cases(grid_lon,grid_lat,grid_area_m2,effective_cover_present_m2,effective_cover_future_m2,sd_present_m2,sd_future_m2)
    ]
    grid_dt[, ECOREGION := as.character(ECOREGION)]
    cli_alert_info("Filtered data: {format(nrow(grid_dt), big.mark=',')} rows, {length(unique(grid_dt$ECOREGION))} ecoregions")
    
    # 3) Pivot to wide (robust dcast with real formula)
    cli_progress_step("Reshaping data to wide format using effective cover in m²")
    
    # ============================================================================
    # LEGACY DENSITY COLUMN CREATION - COMMENTED OUT FOR MEMORY OPTIMIZATION
    # ============================================================================
    # RATIONALE: These pre-computed density columns are redundant and consume 
    # significant memory. We now calculate density on-demand only when needed
    # for trait presence detection. This reduces memory footprint by ~30-40%.
    #
    # grid_dt[, cover_density_present := pmin(1, effective_cover_present_m2 / grid_area_m2)]
    # grid_dt[, cover_density_future  := pmin(1, effective_cover_future_m2  / grid_area_m2)]
    # if ("effective_cover_historical_m2" %in% names(grid_dt)) {
    #   grid_dt[, cover_density_historical := pmin(1, effective_cover_historical_m2 / grid_area_m2)]
    # }
    # ============================================================================
    
    value_vars_effective <- c("effective_cover_present_m2","effective_cover_future_m2","sd_present_m2","sd_future_m2")
    if ("effective_cover_historical_m2" %in% names(grid_dt)) {
      value_vars_effective <- c(value_vars_effective,"effective_cover_historical_m2","sd_historical_m2")
    }
    
    # ============================================================================
    # LEGACY DENSITY PIVOTING - COMMENTED OUT FOR MEMORY OPTIMIZATION
    # ============================================================================
    # RATIONALE: Removing density columns from pivoting eliminates redundant
    # ~9 columns per trait (present/future/historical * 3 density types).
    # This significantly reduces memory usage and speeds up serialization.
    #
    # value_vars_density <- c("cover_density_present","cover_density_future")
    # if ("cover_density_historical" %in% names(grid_dt)) value_vars_density <- c(value_vars_density,"cover_density_historical")
    # ============================================================================
    
    value_vars_area <- c("grid_area_m2","extent_area_present_m2")
    
    id_vars <- c("grid_id","grid_lon","grid_lat","ECOREGION")
    stopifnot(all(id_vars %in% names(grid_dt)))
    fml <- as.formula(sprintf("%s ~ life_history", paste(id_vars, collapse=" + ")))
    
    wide_dt_effective <- data.table::as.data.table(
      data.table::dcast(grid_dt, fml, value.var = value_vars_effective, sep = "_", fill = 0)
    )
    
    # ============================================================================
    # LEGACY DENSITY DATA TABLE CREATION - COMMENTED OUT
    # ============================================================================
    # wide_dt_density <- data.table::as.data.table(
    #   data.table::dcast(grid_dt, fml, value.var = value_vars_density, sep = "_", fill = 0)
    # )
    # ============================================================================
    
    wide_dt_area <- data.table::as.data.table(
      data.table::dcast(grid_dt, fml, value.var = value_vars_area, sep = "_", fill = 0)
    )
    
    # ============================================================================
    # MODIFIED MERGE - EXCLUDES DENSITY TABLE
    # ============================================================================
    # RATIONALE: We only merge effective cover and area tables, eliminating
    # the redundant density table. This maintains all essential data while
    # reducing memory footprint.
    wide_dt <- merge(wide_dt_effective, wide_dt_area, by = id_vars, all = TRUE)
    # wide_dt <- merge(wide_dt, wide_dt_density, by = id_vars, all = TRUE) # LEGACY
    setDT(wide_dt)
    
    # 3b) Determine active traits (define cfg BEFORE using it anywhere)
    cfg <- active_trait_set(wide_dt)
    cli_alert_info("Trait availability → {if (length(cfg$core_avail)) paste(cfg$core_avail, collapse=', ') else 'none'} (total_cover always present)")
    
    # Ensure extent & grid_area columns exist (STRICT: prefer reef extent; never silently swap)
    extent_col <- "extent_area_present_m2_total_cover"
    
    # Look for any extent columns produced by the dcast (ideally *_total_cover)
    extent_cols <- grep("^extent_area_present_m2_", names(wide_dt), value = TRUE)
    
    if (!extent_col %in% names(wide_dt)) {
      if (extent_col %in% extent_cols) {
        # exact total_cover extent exists under expected name after pivot
        # (nothing to do)
      } else if (isTRUE(STRICT_REEF_EXTENT)) {
        cli_abort("P2: Missing '{extent_col}' from pivot. Supply reef extent in P1 (life_history='total_cover').")
      } else {
        # legacy fallback (explicit warning)
        grid_area_cols <- grep("^grid_area_m2_", names(wide_dt), value = TRUE)
        if (length(grid_area_cols)) {
          cli_alert_warning("P2: '{extent_col}' missing; substituting from '{grid_area_cols[1]}' (NOT reef extent).")
          wide_dt[, (extent_col) := get(grid_area_cols[1])]
        } else {
          cli_abort("P2: No '{extent_col}' or 'grid_area_m2_*' columns available.")
        }
      }
    }
    
    # Keep a unified grid_area_m2 (used elsewhere), but this is NOT used as reef extent
    if (!"grid_area_m2" %in% names(wide_dt)) {
      grid_area_cols <- grep("^grid_area_m2_", names(wide_dt), value = TRUE)
      if (length(grid_area_cols)) {
        wide_dt[, grid_area_m2 := get(grid_area_cols[1])]
        cli_alert_info("Added unified grid_area_m2 column from {grid_area_cols[1]}")
      } else {
        cli_abort("P2: 'grid_area_m2' missing and no 'grid_area_m2_*' columns available.")
      }
    }
    
    
    # 4) Metrics (effective-cover m²)
    cli_progress_step("Calculating conservation metrics using delta type: {params$temporal$delta_type}")
    min_threshold <- params$conservation$min_threshold %||% 1e-6
    wide_dt_clean <- copy(wide_dt)
    
    for (lht in c("total_cover", cfg$core_avail)) {
      present <- paste0("effective_cover_present_m2_", lht)
      future  <- paste0("effective_cover_future_m2_",  lht)
      hist    <- paste0("effective_cover_historical_m2_", lht)
      sd_p    <- paste0("sd_present_m2_", lht)
      sd_f    <- paste0("sd_future_m2_",  lht)
      
      req <- c(present,future,sd_p,sd_f)
      if (params$temporal$delta_type %in% c("historical-present","historical-future")) req <- c(req, hist)
      if (!all(req %in% names(wide_dt_clean))) { cli_warn("Missing metrics for {lht}, skipping"); next }
      
      set(wide_dt_clean, j=present, value=pmax(wide_dt_clean[[present]], min_threshold))
      set(wide_dt_clean, j=future,  value=pmax(wide_dt_clean[[future ]], min_threshold))
      if (hist %in% names(wide_dt_clean)) set(wide_dt_clean, j=hist, value=pmax(wide_dt_clean[[hist]], min_threshold))
      
      future_weight <- if (lht == "stress_tolerant") 0.25 else (1 - params$temporal$beta)
      temporal_value <- params$temporal$beta * wide_dt_clean[[present]] + future_weight * wide_dt_clean[[future]]
      set(wide_dt_clean, j=paste0("temporal_score_", lht), value=temporal_value)
      
      absolute_change  <- calculate_delta(wide_dt_clean, params$temporal$delta_type, lht)
      delta_denom      <- get_delta_denominator(wide_dt_clean, params$temporal$delta_type, lht)
      delta_penalty    <- pmax(absolute_change / delta_denom, 0)
      set(wide_dt_clean, j=paste0("delta_penalty_", lht), value=delta_penalty)
      
      present_risk <- wide_dt_clean[[sd_p]] / (wide_dt_clean[[present]] + 1.0)
      future_risk  <- wide_dt_clean[[sd_f]] / (wide_dt_clean[[future ]] + 1.0)
      base_risk    <- sqrt((present_risk^2 + future_risk^2)/2)
      local_risk   <- 0.8*base_risk + 0.2*(params$temporal$delta_weight * delta_penalty)
      set(wide_dt_clean, j=paste0("local_risk_", lht), value=local_risk)
    }
    
    wide_dt <- wide_dt_clean; rm(wide_dt_clean); gc()
    
    # 4b) Trait presence flags (ROBUST DENSITY-BASED WITH RARITY-AWARE THRESHOLDS)
    cli_progress_step("Creating trait presence flags using robust density thresholds")
    
    # ============================================================================
    # NEW IMPLEMENTATION: RARITY-AWARE TRAIT PRESENCE DETECTION
    # ============================================================================
    # RATIONALE: Replaces relative thresholding (5% of max density) with absolute
    # density thresholds that account for ecological rarity and persistence strategies.
    #
    # - Competitive corals (1%): Need higher densities to be ecologically significant
    # - Stress-tolerant corals (0.2%): Often rare but ecologically valuable, lower threshold
    # - Weedy corals (1%): Need meaningful coverage to function ecologically
    #
    # This ensures trait presence flags indicate ecologically meaningful coverage
    # rather than artifacts of density calculation methods.
    # ============================================================================
    
    # Define ecologically meaningful density thresholds accounting for trait rarity
    trait_density_thresholds <- list(
      competitive     = 0.01,   # 1% density threshold - competitive corals need meaningful coverage
      stress_tolerant = 0.002,  # 0.2% density threshold - stress-tolerant persist at lower densities
      weedy           = 0.01    # 1% density threshold - weedy corals need meaningful coverage
    )
    
    for (lht in cfg$core_avail) {
      # Use effective cover (m²) and grid area to calculate density on-demand
      # This handles variable grid areas from buffering and coastal clipping
      cover_col <- paste0("effective_cover_present_m2_", lht)
      tcol <- paste0("trait_present_", lht)
      
      if (!cover_col %in% names(wide_dt)) { 
        cli_warn("Missing cover column {cover_col} for trait flags"); next 
      }
      if (!"grid_area_m2" %in% names(wide_dt)) {
        cli_warn("Missing grid_area_m2 column for density calculation"); next
      }
      
      # Calculate coral density for this trait (cover area / grid area)
      # This approach handles variable grid areas from buffering and coastal clipping
      cover_density <- wide_dt[[cover_col]] / wide_dt$grid_area_m2
      
      # Apply absolute density threshold (not relative to max density)
      threshold <- trait_density_thresholds[[lht]]
      trait_present <- cover_density > threshold
      
      # Set the trait presence flag
      set(wide_dt, j = tcol, value = trait_present)
      
      # Log coverage statistics for ecological validation
      n_present <- sum(trait_present, na.rm = TRUE)
      total_cells <- sum(!is.na(trait_present))
      cli_alert_info("Trait {lht}: {n_present}/{total_cells} cells ({round(n_present/total_cells*100, 1)}%) above {threshold*100}% density threshold")
    }
    
    # ============================================================================
    # LEGACY TRAIT PRESENCE IMPLEMENTATION - COMMENTED OUT
    # ============================================================================
    # RATIONALE: Old method used relative thresholds (5% of max density) which
    # could flag ecologically insignificant coverage as "present". This led to
    # misleading complementarity optimization in P3.
    #
    # trait_threshold <- params$conservation$trait_threshold
    # for (lht in cfg$core_avail) {
    #   dcol <- paste0("cover_density_present_", lht)
    #   tcol <- paste0("trait_present_", lht)
    #   if (!dcol %in% names(wide_dt)) { cli_warn("Missing density col {dcol} for trait flags"); next }
    #   maxv <- suppressWarnings(max(wide_dt[[dcol]], na.rm=TRUE)); if (!is.finite(maxv)) maxv <- 0
    #   rel_thr <- trait_threshold * maxv
    #   set(wide_dt, j=tcol, value = wide_dt[[dcol]] > rel_thr)
    # }
    # ============================================================================
    
    # Preserve existing trait coverage reporting for backward compatibility
    trait_cols <- grep("^trait_present_", names(wide_dt), value = TRUE)
    if (length(trait_cols)) {
      covg <- wide_dt[, lapply(.SD, function(x) round(sum(x,na.rm=TRUE)/.N*100,1)), .SDcols=trait_cols]
      cli_alert_info("Final trait coverage with robust thresholds:")
      for (nm in names(covg)) cli_alert_info("  {nm}: {covg[[nm]]}% of cells")
    }
    
    # 5) Spatial conversion (match P1 projection)
    cli_progress_step("Creating spatial features with correct projection")
    region_crs <- get_region_projection(region_path)
    grid_sf <- st_as_sf(wide_dt, coords = c("grid_lon","grid_lat"), crs = 4326)
    if (!identical(region_crs, "EPSG:4326")) grid_sf <- st_transform(grid_sf, region_crs)
    if (any(st_is_empty(grid_sf$geometry))) cli_abort("Empty geometries in grid_sf.")
    
    # 6) Risk aggregation
    cli_progress_step("Calculating total risk metrics")
    rn_total <- "local_risk_total_cover"
    if (rn_total %in% names(grid_sf)) {
      grid_sf$total_risk <- st_drop_geometry(grid_sf)[[rn_total]]
    } else {
      rn_comp <- "local_risk_competitive"
      grid_sf$total_risk <- if (rn_comp %in% names(grid_sf)) st_drop_geometry(grid_sf)[[rn_comp]] else NA_real_
    }
    
    # 7) Validation + ranges
    cli_progress_step("Validating output data structure")
    req_final <- c("grid_id","ECOREGION","extent_area_present_m2_total_cover","temporal_score_total_cover","total_risk")
    miss_final <- setdiff(req_final, names(grid_sf))
    if (length(miss_final)) cli_warn("Missing optional columns: {paste(miss_final, collapse=', ')}")
    bbox <- st_bbox(grid_sf)
    cli_alert_info("Spatial extent: X [{round(bbox$xmin,1)}, {round(bbox$xmax,1)}], Y [{round(bbox$ymin,1)}, {round(bbox$ymax,1)}]")
    
    # 7b) Prefer P1 polygon geometry by grid_id (no spatial join)
    p1_poly_path <- file.path(region_path, "grid_polygons_laea.rds")
    p1_crs_path  <- file.path(region_path, "grid_crs.rds")
    use_polygons <- FALSE
    
    if (file.exists(p1_poly_path)) {
      grid_polys <- readRDS(p1_poly_path)
      
      if (inherits(grid_polys, "sf")) {
        # Restore CRS from sidecar if missing (silent)
        if (is.na(sf::st_crs(grid_polys)) && file.exists(p1_crs_path)) {
          sc <- readRDS(p1_crs_path)
          try({
            if (!is.null(sc$wkt))      sf::st_crs(grid_polys) <- sf::st_crs(sc$wkt)
            else if (!is.null(sc$input)) sf::st_crs(grid_polys) <- sf::st_crs(sc$input)
            else if (!is.null(sc$epsg))  sf::st_crs(grid_polys) <- sf::st_crs(as.integer(sc$epsg))
          }, silent = TRUE)
        }
        
        # Required polygon columns
        req_cols  <- c("grid_id","ECOREGION","grid_area_m2")
        miss_poly <- setdiff(req_cols, names(grid_polys))
        
        if (!length(miss_poly)) {
          # --- build metrics table from POINT grid_sf (drop geometry) ---
          metrics_cols <- setdiff(names(grid_sf), attr(grid_sf, "sf_column"))
          p2_metrics   <- data.table::as.data.table(sf::st_drop_geometry(grid_sf[, metrics_cols, drop = TRUE]))
          p2_metrics   <- unique(p2_metrics, by = "grid_id")
          
          # Ensure the two 'must-have' metric columns exist
          if ("extent_area_present_m2" %in% names(p2_metrics) &&
              !("extent_area_present_m2_total_cover" %in% names(p2_metrics))) {
            p2_metrics[, extent_area_present_m2_total_cover := extent_area_present_m2]
          }
          if (!("effective_cover_present_m2_total_cover" %in% names(p2_metrics))) {
            if ("effective_cover_present_m2" %in% names(p2_metrics)) {
              p2_metrics[, effective_cover_present_m2_total_cover := effective_cover_present_m2]
            } else {
              p2_metrics[, effective_cover_present_m2_total_cover := NA_real_]
            }
          }
          
          # --- avoid name collisions on join (no .x/.y) ---
          grid_polys$grid_id <- as.character(grid_polys$grid_id)
          p2_metrics[, grid_id := as.character(grid_id)]
          
          poly_keep <- c("grid_id","ECOREGION","grid_area_m2")            # already on polygons
          p2_add    <- setdiff(names(p2_metrics), poly_keep)               # only add new fields
          p2_metrics_slim <- as.data.frame(p2_metrics[, c("grid_id", p2_add), with = FALSE])
          
          # Safe left join (no suffixes)
          p2_sf <- dplyr::left_join(grid_polys, p2_metrics_slim, by = "grid_id")
          
          # Require these after join to accept polygon geometry
          must_have <- c("grid_id","ECOREGION","grid_area_m2",
                         "extent_area_present_m2_total_cover",
                         "effective_cover_present_m2_total_cover")
          
          if (!length(setdiff(must_have, names(p2_sf)))) {
            grid_sf <- p2_sf
            use_polygons <- TRUE
            cli_alert_success("Using P1 polygon geometry (joined by grid_id).")
          } else {
            cli_alert_warning("Polygon swap skipped (missing fields after join).")
          }
        } else {
          cli_alert_warning("grid_polygons_laea.rds missing: {paste(miss_poly, collapse=', ')}")
        }
      } else {
        cli_alert_warning("grid_polygons_laea.rds exists but is not sf; keeping point geometry.")
      }
    } else {
      cli_alert_info("P1 polygons not found; keeping point geometry.")
    }
    
    
    # 7c) Save with CRS sidecar
    delta_suffix <- gsub("-", "_", params$temporal$delta_type)
    output_file  <- file.path(region_path, glue::glue("conservation_metrics_{delta_suffix}.rds"))
    
    p2_crs <- tryCatch(sf::st_crs(grid_sf), error = function(e) NULL)
    
    # Ensure parent dir exists (harmless if it already does)
    dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
    
    safe_saveRDS(grid_sf, output_file, crs_obj = p2_crs)
    cli::cli_alert_success("Saved conservation metrics → {output_file}")
    
    cli_alert_info("Geometry used: {if (use_polygons) 'polygons (P1)'}{if (!use_polygons) 'points (P2)'}")
    return(TRUE)
    
  }, error = function(e) {
    cli_alert_danger("Failed to process region {region_name}: {e$message}")
    logger::log_error("Region {region_name} failed: {e$message}")
    return(FALSE)
  })
}

# ========================
# MAIN LOOP
# ========================
cli_h1("Pipeline 2 — Conservation Metrics (Robust)")
.t0 <- Sys.time()
for (region_name in names(REGION_CHECKLIST)) {
  if (!REGION_CHECKLIST[[region_name]]) {
    cli::cli_alert_info("Skipping region (disabled in checklist): {region_name}")
    next
  }
  region_path <- file.path(PARENT_FOLDER, region_name)
  if (!dir.exists(region_path)) {
    cli_alert_warning("Region folder does not exist: {region_path}. Skipping.")
    next
  }
  process_region_conservation(region_name, region_path)
  gc()
}
cli_h1("Processing Summary")
cli_alert_success("Done. Total time: {round(as.numeric(difftime(Sys.time(), .t0, units='mins')), 2)} min")
logger::log_info("Pipeline 2 finished at {Sys.time()}")

