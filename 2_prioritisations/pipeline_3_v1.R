# ============================ PIPELINE 3 (P3) ================================
# - Area budget uses *reef extent* (extent_area_present_m2_total_cover)
# - Ecological objectives use *effective cover* (from P2 temporal/risk metrics)
# - Preserves grid_id/ECOREGION/CRS exactly as in P2
# - Produces per-ecoregion + composite outputs with selection flag
# - Spatial cohesion distances use *saved local CRS* (fixed per region)
# ============================================================================

rm(list = ls())
suppressPackageStartupMessages({
  library(sf)
  library(dbscan)
  library(FNN)
  library(fst)
  library(dplyr)
  library(purrr)
  library(Matrix)
  library(gurobi)
  library(future)
  library(furrr)
  library(data.table)
  library(tibble)
  library(parallelly)
  library(cli)
  library(glue)
  library(withr)   # <-- NEW: safer working-directory scoping
})

# Use planar ops
sf::sf_use_s2(FALSE)

# Threads & futures: keep the whole pipeline sequential for stability
data.table::setDTthreads(max(1L, availableCores() - 2L))
options(future.rng.onMisuse = "ignore")
plan(sequential)   # <-- keep only this plan


# Optimal parallel setup for 16-core machine
available_cores <- availableCores()
cli::cli_alert_info("Available cores: {available_cores}")

# Use 12 workers for preparation (leave 4 for OS/Gurobi)
workers_to_use <- 15
plan(multisession, workers = workers_to_use)
cli::cli_alert_success("Using {workers_to_use} parallel workers for preparation")

# Explicitly set sequential plan (no parallel workers)
plan(sequential)
cli::cli_alert_info("Using sequential processing for all operations")

# ----------------------------- Config ----------------------------------------
portfolio_params <- list(
  gurobi = list(
    threads         = 4,       # will be coerced to 1 if also using parallel workers
    MultiObjMethod  = 1,
    time_limit      = 900,     # base target seconds (adaptive scaling applies)
    mipgap          = 0.10,
    heuristics      = 0.05,
    LogToConsole    = 1,
    LogFile         = "gurobi_optimization.log",
    run_in_parallel = FALSE    # IMPORTANT: keep FALSE for Gurobi safety; prep runs in parallel
  ),
  constraints = list(
    budget_type = "area",         # keep "area"
    budget_value = 0.30,          # 30% of reef extent
    min_trait_representation = 1, # ALL present traits represented
    enforce_minimum_cell = TRUE   # bump budget to ≥ smallest positive cell if needed
  ),
  objective_weights = list(
    temporal         = 1.6,
    temporal_eff     = 0.9,   # NEW: density/efficiency temporal weight (smaller than absolute)
    risk             = 1.2,
    complementarity  = 1.4,
    spatial_cohesion = 1.6
  ),
  objective_priority = list(
    temporal         = 0,  # highest priority
    temporal_eff     = 0,  # NEW: same priority as temporal so it has real influence
    spatial_cohesion = 1,
    complementarity  = 2,
    risk             = 3
  ),
  trait_weights = list(
    competitive      = 0.33,
    stress_tolerant  = 0.33,
    weedy            = 0.33,
    total_cover      = 0
  ),
  scaling = list(
    use_effective_cover = TRUE,  # (for objectives)
    convert_to_km2      = FALSE, # keep m² internally
    min_threshold       = 1e-8,
    epsilon             = 1e-6
  ),
  spatial = list(
    k_min = 5, 
    k_max = 30,                    # REDUCED from 30 for faster spatial calculations
    k_scale_factor = 50,          # INCREASED from 50 for more aggressive scaling
    fragmentation_penalty_weight = 0.1  # set 0 to disable
  )
)

# Conservative, analysis-safe performance controls
portfolio_params$performance <- list(
  max_ecoregions_per_chunk = 3,   # process up to 3 ecoregions per batch
  chunk_pause_seconds      = 5,   # small breather between batches + GC
  max_time_limit           = 1800,# cap each ecoregion at 30 min (before retries)
  time_scaling_exponent    = 0.7  # sub-linear scaling of TimeLimit by n
)

# MUST MATCH P2
DELTA_TYPE    <- "present-future"
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

# --------------------------- Small helpers -----------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b
num1 <- function(x, default = NA_real_) {
  if (is.null(x)) return(default)
  if (is.list(x) || is.function(x) || length(x) == 0L) return(default)
  y <- suppressWarnings(as.numeric(x)[1])
  if (length(y) == 0L || is.na(y) || is.infinite(y)) default else y
}
sanitize_filename <- function(name) gsub("[^a-zA-Z0-9]", "_", name)

# --- Robust error helpers (avoid meta-errors inside handlers) ---
safe_err_msg <- function(e) {
  tryCatch(conditionMessage(e),
           error = function(e2) "unknown error (could not extract message)")
}
safe_err_print <- function(e) {
  try(print(e), silent = TRUE)
  invisible(NULL)
}

# Detect available coral life-history traits (presence flags)
detect_coral_traits <- function(df) {
  coral_traits <- c("competitive","stress_tolerant","weedy")
  trait_cols <- grep("^trait_present_", names(df), value = TRUE)
  detected <- gsub("^trait_present_", "", trait_cols)
  intersect(detected, coral_traits)
}

# Scale to Z with zero-variance safety
scale_z <- function(x) {
  x <- as.numeric(x)
  s <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

# ---------------- CRS helpers (READ ONLY from region folder) ------------------
# Prefer canonical 'region_crs_fixed.rds' → 'crs_metadata.rds' → 'grid_crs.rds'
read_region_crs <- function(region_path) {
  fixed <- file.path(region_path, "region_crs_fixed.rds")
  if (file.exists(fixed)) {
    x <- readRDS(fixed); if (inherits(x, "crs") && !is.na(x)) return(x)
  }
  meta_f <- file.path(region_path, "crs_metadata.rds")
  if (file.exists(meta_f)) {
    a <- readRDS(meta_f)
    if (is.list(a)) {
      if (!is.null(a$wkt) && nzchar(a$wkt))               return(sf::st_crs(a$wkt))
      if (!is.null(a$crs_string) && nzchar(a$crs_string)) return(sf::st_crs(a$crs_string))
      if (!is.null(a$crs_proj4) && nzchar(a$crs_proj4))   return(sf::st_crs(a$crs_proj4))
      if (!is.null(attr(a$bbox, "crs"))) {
        c2 <- attr(a$bbox, "crs")
        if (!is.null(c2$wkt)   && nzchar(c2$wkt))   return(sf::st_crs(c2$wkt))
        if (!is.null(c2$input) && nzchar(c2$input)) return(sf::st_crs(c2$input))
      }
    }
  }
  sidecar <- file.path(region_path, "grid_crs.rds")
  if (file.exists(sidecar)) {
    b <- readRDS(sidecar)
    if (inherits(b, "crs") && !is.na(b)) return(b)
    if (is.list(b)) {
      if (!is.null(b$wkt)   && nzchar(b$wkt))      return(sf::st_crs(b$wkt))
      if (!is.null(b$input) && nzchar(b$input))    return(sf::st_crs(b$input))
      if (!is.null(b$epsg)  && is.finite(b$epsg))  return(sf::st_crs(as.integer(b$epsg)))
    }
  }
  stop("read_region_crs: no usable CRS found in ", region_path)
}

# Attach/transform any sf to the region CRS (NO bbox guessing; tolerant to equivalence)
as_region_crs <- function(x, region_crs) {
  stopifnot(inherits(x, "sf"))
  cur <- sf::st_crs(x)
  if (is.na(cur)) { sf::st_crs(x) <- region_crs; return(x) }
  if (!identical(cur$wkt, region_crs$wkt) && !identical(cur$epsg, region_crs$epsg)) {
    return(sf::st_transform(x, region_crs))
  }
  x
}

# ----------------------- Validation from P2 ----------------------------------
validate_input_data <- function(grid_sf) {
  stopifnot(inherits(grid_sf, "sf"))
  crs <- sf::st_crs(grid_sf)
  cli::cli_alert_info("Using CRS from P2: {crs$input %||% 'NA'}")
  
  req_cols <- c(
    "grid_id","ECOREGION",
    "temporal_score_total_cover","total_risk", 
    "extent_area_present_m2_total_cover",
    "effective_cover_present_m2_total_cover"
  )
  miss <- setdiff(req_cols, names(grid_sf))
  if (length(miss)) {
    if ("extent_area_present_m2_total_cover" %in% miss) {
      stop("P3 requires 'extent_area_present_m2_total_cover' from P1 aggregation (via P2). Missing: ",
           paste(miss, collapse=", "))
    } else {
      cli::cli_alert_warning("Optional columns missing (OK if objectives precomputed): {paste(miss, collapse=', ')}")
    }
  }
  
  grid_sf$ECOREGION <- as.character(grid_sf$ECOREGION)
  grid_sf$grid_id   <- as.character(grid_sf$grid_id)
  
  # === SKIP GEOMETRY VALIDATION FOR LARGE DATASETS ===
  n <- nrow(grid_sf)
  if (n > 100000) {
    cli::cli_alert_warning("Large dataset ({n} cells) - skipping geometry validation for speed")
  } else {
    if (any(!sf::st_is_valid(grid_sf))) {
      grid_sf <- sf::st_make_valid(grid_sf)
      cli::cli_alert_success("Fixed invalid geometries")
    }
  }
  
  grid_sf
}

# ---------------- NEW: explicit planar CRS validator --------------------------
validate_crs_planar <- function(crs) {
  sfc_probe <- sf::st_sfc(sf::st_point(c(0, 0)), crs = crs)
  if (isTRUE(sf::st_is_longlat(sfc_probe))) {
    stop("CRS must be planar for distance/area calculations, but got geographic: ",
         crs$input %||% "unknown")
  }
}

# --------- Ecoregion prep (budget & objectives) with fixed-CRS KNN ----------
prepare_ecoregion_data <- function(grid_sf, eco, params, knn_crs) {
  cli::cli_alert_info("Prepare ecoregion: {eco}")
  idx <- which(grid_sf$ECOREGION == eco)
  if (!length(idx)) stop("Ecoregion not found: ", eco)
  g  <- grid_sf[idx, , drop = FALSE]
  df <- as.data.frame(sf::st_drop_geometry(g), stringsAsFactors = FALSE)
  
  # ---- budget area (reef extent) ----
  if (!"extent_area_present_m2_total_cover" %in% names(df)) {
    stop("Missing extent_area_present_m2_total_cover in ecoregion ", eco)
  }
  x_extent <- suppressWarnings(as.numeric(df$extent_area_present_m2_total_cover))
  na_idx   <- which(!is.finite(x_extent))
  zero_idx <- which(is.finite(x_extent) & x_extent <= 0)
  bad_area <- union(na_idx, zero_idx)
  if (length(bad_area)) {
    cli::cli_alert_info("Extent NA/zero in {eco}: NA={length(na_idx)}, zero={length(zero_idx)}")
    cli::cli_alert_warning("Dropping {length(bad_area)} cells with NA/≤0 extent in {eco}")
  }
  pixel_area <- x_extent
  if (length(bad_area)) {
    keep <- setdiff(seq_len(nrow(df)), bad_area)
    g  <- g[keep, , drop = FALSE]
    df <- df[keep, , drop = FALSE]
    pixel_area <- pixel_area[keep]
  }
  if (!length(pixel_area)) stop("All cells in ", eco, " have invalid/zero extent.")
  total_area <- sum(pixel_area, na.rm = TRUE)
  
  # ---- CRS-safe centroids for neighbor calcs (use region fixed CRS) ----
  g_for_knn <- g
  current <- sf::st_crs(g_for_knn)
  if (is.null(current) || is.na(current) || (!identical(current$wkt, knn_crs$wkt))) {
    g_for_knn <- suppressMessages(sf::st_transform(g_for_knn, knn_crs))
  }
  coords <- tryCatch(
    sf::st_coordinates(sf::st_centroid(sf::st_geometry(g_for_knn))),
    error = function(e) sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(g_for_knn)))
  )
  
  # ---- adaptive k with tiny-n safeguards ----
  n <- nrow(coords)
  if (n == 0) stop("No valid cells remain in ecoregion after extent filtering: ", eco)
  if (n == 1) {
    spat_score <- 1
    frag_risk  <- 0
  } else {
    k_raw <- max(params$spatial$k_min, floor(n / params$spatial$k_scale_factor))
    k     <- min(params$spatial$k_max, max(1, min(k_raw, n - 1)))
    nn <- FNN::get.knn(coords, k = k)
    distvec <- as.numeric(nn$nn.dist)
    sigma <- stats::median(distvec[is.finite(distvec) & distvec > 0], na.rm = TRUE)
    if (!is.finite(sigma) || sigma <= 0) sigma <- 1
    spat_score <- vapply(seq_len(n), function(i) mean(exp(-pmax(nn$nn.dist[i,],0) / sigma)), numeric(1))
    frag_risk  <- vapply(seq_len(n), function(i) pmin(mean(nn$nn.dist[i,]) / (sigma + 1e-6), 1), numeric(1))
  }
  
  # ---- NA-safe helper for objectives ----
  fix_vec <- function(x, fallback = 0) {
    x <- as.numeric(x)
    if (all(!is.finite(x))) return(rep(fallback, length(x)))
    med <- stats::median(x[is.finite(x)], na.rm = TRUE)
    x[!is.finite(x)] <- med
    x
  }
  
  # ---- Objectives (already computed in P2 with effective cover) ----
  obj_temporal <- fix_vec(df$temporal_score_total_cover %||% 0)
  obj_risk     <- -fix_vec(df$total_risk %||% 0)  # maximize negative risk = minimize risk
  obj_comp     <- rep(0, n)
  # --- NEW: temporal efficiency (density) = temporal_score / reef extent (pixel_area) ---
  # pixel_area is already the ecoregion's per-cell REEF EXTENT (m²), filtered and NA/<=0 dropped above
  obj_temporal_eff <- obj_temporal / pmax(pixel_area, 1)  # dimensionless efficiency
  
  
  # ---- Complementarity from trait presence ----
  trait_names <- detect_coral_traits(df)
  trait_mat   <- matrix(0, nrow = n, ncol = 0)
  if (length(trait_names)) {
    tp_cols <- intersect(paste0("trait_present_", trait_names), names(df))
    if (length(tp_cols)) {
      mat <- as.matrix(df[, tp_cols, drop = FALSE])
      mat <- ifelse(mat, 1, 0)
      presence_counts <- colSums(mat, na.rm = TRUE)
      # bounded/log-scaled rarity weights
      props <- presence_counts / max(1L, n)
      w <- 1 - log(props + 0.01) / log(1.01)
      w <- w / sum(w)
      eps <- params$scaling$epsilon %||% 1e-6
      obj_comp <- vapply(seq_len(n), function(i) sum(sqrt((mat[i,] * w) + eps)), numeric(1))
      trait_mat <- Matrix::Matrix(mat, sparse = TRUE)
      colnames(trait_mat) <- trait_names   # human-readable trait names
      cli::cli_alert_success("Complementarity using {length(trait_names)} trait(s): {paste(trait_names, collapse=', ')}")
    } else {
      cli::cli_alert_warning("No valid trait flags found in {eco}; complementarity=0")
    }
  } else {
    cli::cli_alert_info("No trait flags in {eco}; complementarity=0")
  }
  
  # ---- Z-scale ----
  obj_temporal_z <- scale_z(obj_temporal)
  obj_risk_z     <- scale_z(obj_risk)
  obj_comp_z     <- scale_z(obj_comp)
  obj_spat_z     <- scale_z(spat_score)
  obj_temporal_eff_z <- scale_z(obj_temporal_eff) 
  
  
  # Optional fragmentation penalty
  frag_wt <- params$spatial$fragmentation_penalty_weight %||% 0
  if (is.finite(frag_wt) && frag_wt != 0) {
    obj_risk_z <- obj_risk_z - (frag_wt * scale_z(frag_risk))
  }
  
  # ---- attach to sf ----
  g$obj_temporal           <- obj_temporal
  g$obj_risk               <- obj_risk
  g$obj_complementarity    <- obj_comp
  g$obj_spatial            <- spat_score
  g$obj_temporal_z         <- obj_temporal_z
  g$obj_risk_z             <- obj_risk_z
  g$obj_complementarity_z  <- obj_comp_z
  g$obj_spatial_z          <- obj_spat_z
  g$fragmentation_risk     <- frag_risk
  g$pixel_area             <- pixel_area
  g$obj_temporal_eff       <- obj_temporal_eff      # NEW (raw efficiency)
  g$obj_temporal_eff_z     <- obj_temporal_eff_z    # NEW (Z-scored efficiency)
  
  
  list(
    data         = g,
    n_pixels     = nrow(g),
    total_area   = total_area,
    trait_matrix = trait_mat,
    trait_names  = trait_names
  )
}

# ---------------- NEW: Gurobi retry wrapper ----------------------------------
# Progressive relaxation (gap/time/heuristics) for a couple of retries.
run_gurobi_with_retry <- function(model, base_params, max_retries = 2) {
  call_gurobi <- function(m, p) tryCatch(gurobi(m, params = p), error = function(e) NULL)
  sol <- call_gurobi(model, base_params)
  if (!is.null(sol) && !is.null(sol$x)) return(sol)
  p <- base_params
  for (k in seq_len(max_retries)) {
    p$MIPGap     <- min(0.50, (p$MIPGap %||% 0.10) * 1.5)
    p$TimeLimit  <- (p$TimeLimit %||% 900) * 1.5
    p$Heuristics <- min(0.30, (p$Heuristics %||% 0.05) * 1.5)
    cli::cli_alert_info("Retry {k}: Time={round(p$TimeLimit)}s, Gap={round(p$MIPGap,3)}, Heuristics={round(p$Heuristics,3)}")
    sol <- call_gurobi(model, p)
    if (!is.null(sol) && !is.null(sol$x)) return(sol)
    if (k < max_retries) Sys.sleep(5)
  }
  NULL
}

# ------------------------- Gurobi optimization --------------------------------
run_gurobi_optimization <- function(ecoregion, params) {
  d <- ecoregion$data
  n <- nrow(d)
  
  # ---- budget ----
  cons <- params$constraints
  area_vector <- as.numeric(d$pixel_area)
  area_vector[!is.finite(area_vector) | area_vector < 0] <- 0
  tot_area    <- sum(area_vector, na.rm = TRUE)
  area_budget <- (cons$budget_value %||% 0.3) * tot_area
  
  # Optional: bump budget to smallest positive cell
  if (isTRUE(cons$enforce_minimum_cell %||% FALSE) && area_budget > 0 && is.finite(tot_area) && tot_area > 0) {
    min_cell <- suppressWarnings(min(area_vector[area_vector > 0], na.rm = TRUE))
    if (is.finite(min_cell) && area_budget < min_cell) {
      cli::cli_alert_info("Budget < smallest cell ({round(area_budget/1e6,2)} vs {round(min_cell/1e6,2)} km²) — bumping to min cell.")
      area_budget <- min_cell
    }
  }
  
  cli::cli_alert_info("Area budget: {round(area_budget/1e6,2)} km² / {round(tot_area/1e6,2)} km²")
  
  # ---- objectives (z-scored) ----
  z <- list(
    temporal = as.numeric(d$obj_temporal_z),
    temporal_eff = as.numeric(d$obj_temporal_eff_z),  #correct for density
    risk     = as.numeric(d$obj_risk_z),
    comp     = as.numeric(d$obj_complementarity_z),
    spatial  = as.numeric(d$obj_spatial_z)
  )
  
  for (nm in names(z)) {
    v <- z[[nm]]
    if (all(!is.finite(v))) z[[nm]] <- rep(0, length(v)) else z[[nm]][!is.finite(z[[nm]])] <- 0
  }
  
  # ---- traits ----
  Tmat <- ecoregion$trait_matrix
  m <- if (inherits(Tmat, "Matrix")) ncol(Tmat) else ncol(as.matrix(Tmat))
  min_rep <- cons$min_trait_representation %||% 0
  
  # variables
  vtype <- if (m > 0) c(rep("B", n), rep("B", m), rep("B", m)) else rep("B", n)
  lb    <- rep(0, length(vtype))
  ub    <- rep(1, length(vtype))
  nvars <- length(vtype)
  
  # constraints
  rows <- 1L + (if (m>0) m else 0) + (if (m>0 && min_rep>0) m + 1L else 0)
  A   <- Matrix::Matrix(0, nrow = rows, ncol = nvars, sparse = TRUE)
  rhs <- numeric(rows); sense <- rep("", rows); r <- 1L
  
  # budget
  A[r, 1:n] <- area_vector; rhs[r] <- area_budget; sense[r] <- "<="; r <- r + 1L
  
  # y_j <= sum trait_ij * x_i
  if (m > 0) {
    for (j in seq_len(m)) {
      A[r, 1:n] <- -Tmat[, j]
      A[r, n + j] <- 1
      rhs[r] <- 0; sense[r] <- "<="; r <- r + 1L
    }
  }
  
  # min rep: z_j <= y_j; sum z_j >= ceil(min_rep*m)  (if enabled)
  if (m > 0 && min_rep > 0) {
    thresh <- if (m == 1) 1 else ceiling(min_rep * m)
    for (j in seq_len(m)) {
      A[r, n + j]     <- -1
      A[r, n + m + j] <-  1
      rhs[r] <- 0; sense[r] <- "<="; r <- r + 1L
    }
    A[r, (n + m + 1):(n + m + m)] <- 1
    rhs[r] <- thresh; sense[r] <- ">="; r <- r + 1L
  }
  
  # helper: always return an objective vector of length nvars
  pad_to_nvars <- function(v) {
    v <- as.numeric(v)
    if (length(v) < nvars) c(v, rep(0, nvars - length(v)))
    else if (length(v) > nvars) v[1:nvars]
    else v
  }
  
  # objective list (multi-objective; priorities/weights preserved)
  ow <- params$objective_weights
  op <- params$objective_priority
  multiobj <- list()
  add_obj <- function(list0, vec, pr, wt) {
    wt <- num1(wt, 0); if (!is.finite(wt) || wt == 0) return(list0)
    vecn <- pad_to_nvars(vec)
    c(list0, list(list(objn = vecn,
                       priority = as.integer(num1(pr, 1L)),
                       weight   = as.numeric(wt))))
  }
  
  multiobj <- add_obj(multiobj, z$temporal,     op$temporal,      ow$temporal)
  multiobj <- add_obj(multiobj, z$temporal_eff, op$temporal_eff,  ow$temporal_eff)  # NEW
  multiobj <- add_obj(multiobj, z$risk,         op$risk,          ow$risk)
  if (m > 0) {
    multiobj <- add_obj(multiobj, z$comp,   op$complementarity,  ow$complementarity)
    obj_rep <- c(rep(0, n), rep(1, m), rep(0, m))  # already length n + 2m
    multiobj <- add_obj(multiobj, obj_rep,  op$complementarity,  ow$complementarity)
  }
  multiobj <- add_obj(multiobj, z$spatial,  op$spatial_cohesion, ow$spatial_cohesion)
  
  model <- list(
    A = A, rhs = rhs, sense = sense,
    vtype = vtype, lb = lb, ub = ub,
    modelsense = "max",
    multiobj = multiobj
  )
  
  # ---- Gurobi params (avoid oversubscription if running in parallel) ----
  nworkers <- tryCatch(future::nbrOfWorkers(), error = function(e) 1L)
  threads  <- num1(params$gurobi$threads, 0)
  if (nworkers > 1 && threads > 1) threads <- 1
  
  # Adaptive TimeLimit: sub-linear growth with hard cap
  base_time <- params$gurobi$time_limit %||% 300
  max_time  <- params$performance$max_time_limit %||% 3600
  expo      <- params$performance$time_scaling_exponent %||% 0.7
  adaptive  <- min(max_time, max(base_time, (n^expo) * 15))
  
  prms <- list(
    Threads        = threads,
    TimeLimit      = adaptive,
    MIPGap         = num1(params$gurobi$mipgap, 0.05),
    Heuristics     = num1(params$gurobi$heuristics, 0.05),
    MultiObjMethod = num1(params$gurobi$MultiObjMethod, 1),
    LogToConsole   = num1(params$gurobi$LogToConsole, 1),
    LogFile        = params$gurobi$LogFile %||% "gurobi_optimization.log"
  )
  
  # ---- Progress timing ----
  cli::cli_alert_info("Starting Gurobi optimization for {n} cells...")
  .t_gurobi <- Sys.time()
  
  # ---- Try with retry wrapper before heuristic fallback ----
  gurobi_error <- NULL
  sol <- tryCatch(run_gurobi_with_retry(model, prms, max_retries = 2),
                  error = function(e) { gurobi_error <<- safe_err_msg(e); NULL })
  
  # ---- Report optimization time ----
  gurobi_time <- as.numeric(difftime(Sys.time(), .t_gurobi, units = "mins"))
  cli::cli_alert_success("Gurobi completed in {round(gurobi_time, 1)} minutes")
  
  if (is.null(sol) || is.null(sol$x)) {
    cli::cli_alert_warning("Gurobi failed — heuristic greedy fallback")
    if (!is.null(gurobi_error)) cli::cli_alert_warning("Gurobi error: {gurobi_error}")
    
    # value-per-area heuristic
    #value <- z$temporal + z$spatial + z$comp + z$risk
    value <- z$temporal + 0.75*z$temporal_eff + z$spatial + z$comp + z$risk
    value[!is.finite(value)] <- 0
    denom <- pmax(area_vector, 1)
    order_idx <- order(value / denom, decreasing = TRUE)
    
    remaining <- area_budget
    sel <- rep(FALSE, n)
    for (i in order_idx) {
      ai <- area_vector[i]; if (ai <= 0) next
      if (ai <= remaining) { sel[i] <- TRUE; remaining <- remaining - ai }
      if (remaining <= 0) break
    }
    
    if (!any(sel) && isTRUE(cons$enforce_minimum_cell %||% FALSE)) {
      candidate <- which.min(ifelse(area_vector > 0, area_vector, Inf))
      if (is.finite(candidate)) sel[candidate] <- TRUE
    }
    
    sel_idx <- which(sel)
  } else {
    x_sol <- sol$x[1:n]
    sel_idx <- which(x_sol > 0.5)
  }
  
  d$selected <- FALSE
  if (length(sel_idx)) d$selected[sel_idx] <- TRUE
  
  sel_area <- sum(d$pixel_area[d$selected], na.rm = TRUE)
  list(
    grid          = d,
    stats         = tibble::tibble(
      total_pixels        = n,
      selected_pixels     = length(sel_idx),
      total_area          = tot_area,
      selected_area       = sel_area,
      area_selection_rate = ifelse(tot_area > 0, sel_area / tot_area, NA_real_),
      method              = if (is.null(sol) || is.null(sol$x)) "heuristic_fallback" else "gurobi_optimization"
    ),
    trait_matrix  = ecoregion$trait_matrix,
    trait_names   = ecoregion$trait_names
  )
}

# ---------------- NEW: Post-optimization QC validator -------------------------
validate_portfolio <- function(results) {
  # Produces a compact QC table per ecoregion: n_selected, area, and trait coverage
  purrr::imap_dfr(results, function(eco_res, eco) {
    sel <- eco_res$grid$selected
    tm  <- eco_res$trait_matrix
    coverage <- NA
    if (!is.null(tm) && ncol(tm) > 0 && any(sel)) {
      coverage <- paste0(
        colnames(tm), ":", ifelse(colSums(tm[sel, , drop = FALSE]) > 0, "Y", "N"),
        collapse = ";"
      )
    }
    tibble::tibble(
      ecoregion        = eco,
      n_selected       = sum(sel),
      area_selected_m2 = sum(eco_res$grid$pixel_area[sel], na.rm = TRUE),
      trait_coverage   = coverage
    )
  })
}


# Parallel plan for preparation (keep solver sequential by default)
# -------------------------------- MAIN ---------------------------------------
cli::cli_h1("Coral Conservation Portfolio Optimization — Pipeline 3")

# Sequential processing (already set at top) - no change needed here
.t0 <- Sys.time()
delta_suffix <- gsub("-", "_", DELTA_TYPE)

for (region_name in names(REGION_CHECKLIST)) {
  if (!REGION_CHECKLIST[[region_name]]) {
    cli::cli_alert_info("Skipping region (disabled): {region_name}")
    next
  }
  
  region_path <- file.path(PARENT_FOLDER, region_name)
  if (!dir.exists(region_path)) {
    cli::cli_alert_warning("Region folder missing: {region_path}. Skipping.")
    next
  }
  
  cli::cli_h2("Processing Region: {region_name}")
  
  # progress step per region
  .step_id <- cli::cli_progress_step(
    "Processing {region_name} @ {format(Sys.time(), '%H:%M:%S')}",
    msg_done = "Finished {region_name}"
  )
  
  # P2 existence check BEFORE changing directory
  p2_file <- file.path(region_path, paste0("conservation_metrics_", delta_suffix, ".rds"))
  if (!file.exists(p2_file)) {
    cli::cli_alert_warning("P2 not found: {basename(p2_file)}. Skipping.")
    try(cli::cli_progress_done(.step_id), silent = TRUE)
    next
  }
  
  # Scope all region work inside the region folder safely
  withr::with_dir(region_path, {
    tryCatch({
      # ---- EARLY HEALTH CHECKS ------------------------------------------------
      gurobi_ok <- TRUE
      if (!requireNamespace("gurobi", quietly = TRUE)) gurobi_ok <- FALSE
      if (gurobi_ok) {
        test_model <- list(A = matrix(1,1,1), rhs = 1, sense = "<=", obj = 1,
                           modelsense = "max", vtype = "B")
        gurobi_ok <- !is.null(tryCatch(
          gurobi::gurobi(test_model, params = list(OutputFlag = 0)),
          error = function(e) { cli::cli_alert_warning(paste("Gurobi test failed:", safe_err_msg(e))); NULL }
        ))
      }
      if (!gurobi_ok) stop("Gurobi package not available or license check failed")
      # ------------------------------------------------------------------------
      
      grid_sf <- readRDS(basename(p2_file))
      cli::cli_alert_success("P2 file loaded: {nrow(grid_sf)} features")
      
      # Diagnostics (verbose but safe)
      cli::cli_alert_info("DIAGNOSTIC: Grid columns: {paste(names(grid_sf), collapse=', ')}")
      
      # Validate + coerce
      .t_val <- Sys.time()
      grid_sf <- validate_input_data(grid_sf)
      cli::cli_alert_success("validate_input_data completed in {round(difftime(Sys.time(), .t_val, units='secs'), 2)}s")
      
      # CRS: read region-fixed CRS and enforce it
      REGION_CRS <- read_region_crs(region_path)
      validate_crs_planar(REGION_CRS)
      grid_sf    <- as_region_crs(grid_sf, REGION_CRS)
      knn_crs    <- REGION_CRS
      
      crs_label <- tryCatch({
        REGION_CRS$input %||% REGION_CRS$epsg %||% substr(REGION_CRS$wkt %||% "", 1, 80) %||% "unknown"
      }, error = function(e) "unknown")
      cli::cli_alert_info("KNN/Spatial cohesion CRS → {crs_label}")
      
      na_extent <- sum(!is.finite(grid_sf$extent_area_present_m2_total_cover) |
                         grid_sf$extent_area_present_m2_total_cover <= 0, na.rm = TRUE)
      cli::cli_alert_info("Extent sanity: {na_extent} cells with NA/≤0 extent (will be dropped per-ecoregion)")
      cli::cli_alert_success("Loaded P2: {nrow(grid_sf)} features, {length(unique(grid_sf$ECOREGION))} ecoregions")
      
      # Ecoregions (sequential, smallest→largest)
      eco_counts  <- table(grid_sf$ECOREGION)
      eco_ordered <- names(sort(eco_counts))
      cli::cli_alert_info("=== PROCESSING {length(eco_ordered)} ECOREGIONS SEQUENTIALLY ===")
      
      eco_bar <- cli::cli_progress_bar(
        name = "Processing ecoregions",
        total = length(eco_ordered),
        clear = FALSE
      )
      
      results <- list()
      for (i in seq_along(eco_ordered)) {
        eco <- eco_ordered[i]
        eco_size <- eco_counts[eco]
        
        cli::cli_h3("Eco {i}/{length(eco_ordered)}: {eco} ({eco_size} cells)")
        
        .t_prep <- Sys.time()
        eco_prepped <- prepare_ecoregion_data(grid_sf, eco, portfolio_params, knn_crs = REGION_CRS)
        prep_time <- difftime(Sys.time(), .t_prep, units = "secs")
        cli::cli_alert_info("Preparation: {round(prep_time, 2)}s")
        
        .t_opt <- Sys.time()
        eco_result <- run_gurobi_optimization(eco_prepped, portfolio_params)
        opt_time <- difftime(Sys.time(), .t_opt, units = "secs")
        cli::cli_alert_info("Optimization: {round(opt_time, 2)}s")
        
        results[[eco]] <- eco_result
        
        # PROGRESS: increment explicitly
        cli::cli_progress_update(id = eco_bar, inc = 1)   # <-- CHANGED
        
        cli::cli_alert_success("Completed {eco} in {round(as.numeric(prep_time + opt_time), 1)}s total")
        rm(eco_prepped); gc()
      }
      cli::cli_progress_done(id = eco_bar)
      cli::cli_alert_success("Completed all {length(eco_ordered)} ecoregions in {region_name}!")
      
      # Collate & QC
      stats_tbl <- purrr::map_dfr(results, "stats", .id = "ecoregion")
      print(stats_tbl)
      qc_tbl <- validate_portfolio(results)
      print(qc_tbl)
      
      # Save outputs
      sel_dir <- file.path(region_path, "Selection")
      if (!dir.exists(sel_dir)) dir.create(sel_dir, recursive = TRUE)
      
      p3_rds <- file.path(sel_dir, paste0("portfolio_optimization_results_", delta_suffix, ".rds"))
      saveRDS(results, p3_rds)
      cli::cli_alert_success("Saved P3 results: {p3_rds}")
      
      comp_list <- list()
      for (eco in names(results)) {
        g <- results[[eco]]$grid
        keep <- c("grid_id","ECOREGION","pixel_area","selected",
                  "obj_temporal","obj_risk","obj_complementarity","obj_spatial",
                  "obj_temporal_z","obj_risk_z","obj_complementarity_z","obj_spatial_z",
                  "fragmentation_risk")
        keep <- intersect(keep, names(g))
        g_out <- g[, keep, drop = FALSE]
        g_out <- as_region_crs(g_out, REGION_CRS)
        
        eco_slug <- sanitize_filename(eco)
        shp <- file.path(sel_dir, paste0("portfolio_", eco_slug, ".shp"))
        geojson <- file.path(sel_dir, paste0("portfolio_", eco_slug, ".geojson"))
        try(sf::st_write(g_out, shp, quiet = TRUE, delete_dsn = TRUE))
        try(sf::st_write(g_out, geojson, quiet = TRUE, delete_dsn = TRUE))
        comp_list[[eco_slug]] <- g_out
      }
      
      if (length(comp_list)) {
        comp <- do.call(rbind, comp_list)
        comp <- as_region_crs(comp, REGION_CRS)
        comp_shp  <- file.path(sel_dir, paste0("portfolio_composite_", sanitize_filename(region_name), ".shp"))
        comp_gj   <- file.path(sel_dir, paste0("portfolio_composite_", sanitize_filename(region_name), ".geojson"))
        comp_rds  <- file.path(sel_dir, paste0("portfolio_composite_", sanitize_filename(region_name), ".rds"))
        try(sf::st_write(comp, comp_shp, quiet = TRUE, delete_dsn = TRUE))
        try(sf::st_write(comp, comp_gj,  quiet = TRUE, delete_dsn = TRUE))
        saveRDS(comp, comp_rds)
        cli::cli_alert_success("Saved composite portfolio: {basename(comp_rds)}")
      }
      
      stats_csv <- file.path(sel_dir, paste0("portfolio_summary_", delta_suffix, ".csv"))
      write.csv(stats_tbl, stats_csv, row.names = FALSE)
      qc_csv <- file.path(sel_dir, paste0("portfolio_qc_", delta_suffix, ".csv"))
      write.csv(qc_tbl, qc_csv, row.names = FALSE)
      cli::cli_alert_success("Saved summary stats: {basename(stats_csv)} and QC: {basename(qc_csv)}")
      
    }, error = function(e) {
      err <- safe_err_msg(e)
      cli::cli_alert_danger(sprintf("Error in region %s: %s", region_name, err))
      cli::cli_alert_info("Error details (object print follows):"); safe_err_print(e)
    })
  })  # with_dir
  
  try(cli::cli_progress_done(.step_id), silent = TRUE)
  gc()
}

cli::cli_h1("P3 done")
cli::cli_alert_info("Elapsed (min): {round(as.numeric(difftime(Sys.time(), .t0, units='mins')), 2)}")

