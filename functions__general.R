# Functions
library(tidyverse)
library(smart.lapply) # from https://github.com/Kylezx1/smart.lapply



list_object_memory <- function(env = globalenv()) {
  objs <- ls(envir = env)
  sizes <- sapply(objs, function(x) object.size(get(x, envir = env)))
  sort(sizes, decreasing = TRUE)
}


rescale_0_to_1 <- function(x, including_zero=F) {
  
  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)
  
  if(including_zero) {
    x_min <- min(x_min, 0)
  }
  
  if(x_min == x_max) {
    return(rep(1, length(x)))
  } else {
    (x - x_min) / (x_max - x_min)
  }
  
}



time_it <- function(expr, with_sound = F, sound = 2) {
  start_time <- Sys.time()
  result <- eval(expr, envir = parent.frame())
  end_time <- Sys.time() - start_time
  
  print(end_time)
  cat("Elapsed Time: ", end_time, "\n")
  if(with_sound) { beepr::beep(sound = sound) }
  
  invisible(result)  # Return the result without printing it
}


quick_project <- function(input_layer,
                          target_crs_wkt,
                          input_layer_type = c('polygon', 'raster')) {
  out <- input_layer
  
  if(input_layer_type[1] == 'polygon') {
    
    out <- st_transform(out, crs = target_crs_wkt)
    
  } else if(input_layer_type[1] == 'raster') {
    
    out <- terra::project(out, target_crs_wkt)
    
  }
  
  return(out)
  
}


timestamp_cat <- function() {cat(crayon::black("Execute time:",timestamp(prefix = '', suffix = '', quiet = T)))}

catpastenames <- function(data) {cat(paste0('"', names(data),'",\n'))}


generic_rename <- function(col_names, lookup) {
  reduce(
    seq_len(nrow(lookup)),
    .init = col_names,
    .f = function(nms, i) {
      old  <- lookup$variable[i]
      new  <- lookup$updated_variable[i]
      if (!is.na(lookup$units[i])) new <- paste0(new, " (", lookup$units[i], ")")
      str_replace(nms, fixed(old), new)
    }
  )
}


rename_from_lookup <- function(.data, lookup) {
  .data %>%
    rename_with(.fn = generic_rename, lookup, .cols = everything())
}


label_blank <- function(x) {
  # x is a character (or factor) vector of current strip labels
  rep("", length(x))
}


interleave_colors <- function(num_colors, num_splits,
                              begin = 0, end = 1, option = "D") {
  library(viridis)
  
  # Generate the colors using the viridis palette
  colors <- viridis(num_colors, begin = begin, end = end, option = option)
  
  # Calculate the number of colors per split
  colors_per_split <- ceiling(num_colors / num_splits)
  
  # Create a matrix of colors with the specified number of splits
  color_matrix <- matrix(colors[1:(colors_per_split * num_splits)], nrow = colors_per_split)
  
  
  # Remove 
  
  # Interleave the colors from each split
  interleaved_colors <- as.vector(t(color_matrix)) %>% na.omit()
  
  return(interleaved_colors)
}



library(scales)


.iriviridis_cols <- c("#f6fd96", "#00e422", "#13a2b9", "#2c1c89")

# continuous palette generator
iriviridis_pal_c <- function(begin = 0, end = 1, reverse = FALSE) {
  cols <- if (reverse) rev(.iriviridis_cols) else .iriviridis_cols
  function(x) {
    x <- begin + x * (end - begin)
    scales::gradient_n_pal(cols)(x)
  }
}

# discrete palette generator
iriviridis_pal_d <- function(begin = 0, end = 1, reverse = FALSE) {
  pal_c <- iriviridis_pal_c(begin, end, reverse)
  function(n) pal_c(seq(0, 1, length.out = n))
}

# ---- continuous scales ----
scale_colour_iriviridis_c <- function(...,
                                      begin = 0, end = 1, reverse = FALSE,
                                      na.value = NA) {
  cols <- iriviridis_pal_c(begin, end, reverse)(seq(0, 1, length.out = 256))
  ggplot2::scale_colour_gradientn(colours = cols, na.value = na.value, ...)
}
scale_color_iriviridis_c <- scale_colour_iriviridis_c

scale_fill_iriviridis_c <- function(...,
                                    begin = 0, end = 1, reverse = FALSE,
                                    na.value = NA) {
  cols <- iriviridis_pal_c(begin, end, reverse)(seq(0, 1, length.out = 256))
  ggplot2::scale_fill_gradientn(colours = cols, na.value = na.value, ...)
}

# ---- discrete scales ----
scale_colour_iriviridis_d <- function(...,
                                      begin = 0, end = 1, reverse = FALSE) {
  ggplot2::discrete_scale("colour", "iriviridis",
                          iriviridis_pal_d(begin, end, reverse), ...)
}
scale_color_iriviridis_d <- scale_colour_iriviridis_d

scale_fill_iriviridis_d <- function(...,
                                    begin = 0, end = 1, reverse = FALSE) {
  ggplot2::discrete_scale("fill", "iriviridis",
                          iriviridis_pal_d(begin, end, reverse), ...)
}



fib <- function(x, y, y_max = 4000000) {
  s <- x + y
  c(s, if (s <= y_max) fib(y, s, y_max))
}


validate_and_fix_polygons <- function(input_layer, 
                                      cores = 1,
                                      drop_invalid = F) {
  
  feat_valid <- st_is_valid(input_layer)
  
  
  if(drop_invalid) {
    
    out <- input_layer[feat_valid,]
    return(out)
    
  }
  
  out <- input_layer
  
  if(!all(feat_valid)) {
    
    invalid_feats <- which(!feat_valid)
    valid_feats <- which(feat_valid)
    
    print(paste0('fixing ', length(invalid_feats), ' features'))
    
    invalid_polygons <- input_layer[invalid_feats, ]
    valid_polygons <- input_layer[valid_feats, ]
    
    if(cores != 1 && length(invalid_feats) > 2000) {
      
      cores <- min(c(detectCores() - 1, cores))
      
      splits <- split(invalid_feats, sort(1:length(invalid_feats) %% cores))
      
      data.feat_ready.ls <- 
        smart.lapply(cores = cores,
                     1:length(splits),
                     function(x) {
                       
                       data.feat_ready <- st_make_valid(invalid_polygons[splits[[x]], ])
                       
                     })
      
      invalid_polygons_fixed <- do.call(rbind, data.feat_ready.ls)
      
    } else {
      
      invalid_polygons_fixed <- st_make_valid(invalid_polygons)
      
    }
    
    out <- rbind(valid_polygons, invalid_polygons_fixed)
    
  }
  
  return(out)
  
}


rotate_if_needed <- function(dataset, override = c('none', 'wrap', 'do_not_wrap')) {
  # Match the override option
  override <- match.arg(override)
  
  # Early return based on override option
  if (override == "do_not_wrap") {
    return(dataset)
  }
  
  incoming_crs <- st_crs(dataset)
  
  # if(inherits(dataset, c('sf', 'SpatVector'))) {
  #   
  #   dataset <- quick_project(dataset, 
  #                            sf::st_crs(4326)$wkt, # Project to global to check.
  #                            input_layer_type = 'polygon')
  #   
  # } else if (inherits(dataset, c("Raster", "SpatRaster", 'SpatRasterDataset'))) {
  #   
  #   dataset <- quick_project(dataset, 
  #                            sf::st_crs(4326)$wkt, # Project to global to check.
  #                            input_layer_type = 'raster')
  #   
  # }
  # 
  
  # Detect if it's an sf or raster object
  if (inherits(dataset, c("sf", "SpatVector"))) {
    
    bbox <- st_bbox(dataset)
  } else if (inherits(dataset, "Raster")) {
    
    bbox <- extent(dataset)
    bbox <- as.numeric(c(bbox@xmin, bbox@ymin, bbox@xmax, bbox@ymax))
    names(bbox) <- c("xmin", "ymin", "xmax", "ymax")
    
  } else if(inherits(dataset, "SpatRaster")) {
    
    bbox <- st_bbox(dataset)
    
  } else {
    stop("Dataset must be an 'sf' or 'Raster' object")
  }
  
  
  
  # Check if the dataset crosses the dateline
  if (override == "wrap" || override == 'none' && bbox["xmin"] <= -180|| bbox["xmax"] >= 180) {
    warning("Dataset crosses the dateline, rotating...")
    # For sf objects
    if (inherits(dataset, c("sf"))) {
      rotated_dataset <- st_wrap_dateline(dataset, options = c("WRAPDATELINE=YES"), quiet = F)
    } 
    # For Raster objects
    else if (inherits(dataset, c("Raster", "SpatRaster", "SpatVector"))) {
      rotated_dataset <- rotate(dataset, split = T)
    }
  } else {
    rotated_dataset <- dataset
  }
  
  # if(inherits(dataset, c('sf', 'SpatVector'))) {
  #   
  #   rotated_dataset <- quick_project(rotated_dataset, 
  #                                    incoming_crs$wkt, # Project to global to check.
  #                                    input_layer_type = 'polygon')
  #   
  # } else if (inherits(rotated_dataset, c("Raster", "SpatRaster", 'SpatRasterDataset'))) {
  #   
  #   rotated_dataset <- quick_project(rotated_dataset, 
  #                                    incoming_crs$wkt, # Project to global to check.
  #                                    input_layer_type = 'raster')
  #   
  # }
  
  return(rotated_dataset)
}
