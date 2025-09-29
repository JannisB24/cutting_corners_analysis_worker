first_move_function <- function(data,
                                time_pattern  = "^els_slider\\.1\\.player\\.slider_history_time_\\d+$",
                                value_pattern = "^els_slider\\.1\\.player\\.slider_history_\\d+$",
                                ignore_zero = TRUE) {
  # Locate time and value columns
  time_cols  <- grep(time_pattern,  names(data), value = TRUE)
  value_cols <- grep(value_pattern, names(data), value = TRUE)
  if (!length(time_cols))  stop("No columns matched time_pattern: ", time_pattern)
  if (!length(value_cols)) stop("No columns matched value_pattern: ", value_pattern)
  
  # Map by slider index (numeric suffix)
  get_idx <- function(nm) as.integer(sub(".*_(\\d+)$", "\\1", nm))
  t_idx <- get_idx(time_cols)
  v_idx <- get_idx(value_cols)
  
  time_by_idx  <- setNames(time_cols,  t_idx)
  value_by_idx <- setNames(value_cols, v_idx)
  
  common_idx <- sort(intersect(t_idx, v_idx))
  if (!length(common_idx)) stop("No overlapping slider indices between time and value columns.")
  if (length(common_idx) < max(length(unique(t_idx)), length(unique(v_idx)))) {
    missing_time  <- setdiff(v_idx, t_idx)
    missing_value <- setdiff(t_idx, v_idx)
    if (length(missing_time))
      warning("No time column for slider indices: ", paste(sort(unique(missing_time)), collapse = ", "))
    if (length(missing_value))
      warning("No value column for slider indices: ", paste(sort(unique(missing_value)), collapse = ", "))
  }
  
  n <- nrow(data)
  first_ts    <- rep(NA_real_, n)
  first_val   <- rep(NA_real_, n)
  dist50_val  <- rep(NA_real_, n)  # |first_moved_value - 50|
  diff_init50 <- rep(NA_real_, n)  # |initial value of first-moved slider - 50|
  
  for (i in seq_len(n)) {
    best_ts <- Inf
    best_val <- NA_real_
    best_slider <- Inf
    best_init_val <- NA_real_
    
    for (k in common_idx) {
      t_raw <- data[i, time_by_idx[as.character(k)], drop = TRUE]
      v_raw <- data[i, value_by_idx[as.character(k)], drop = TRUE]
      
      # Skip if both are missing/empty
      if ((is.na(t_raw) || !nzchar(trimws(as.character(t_raw)))) &&
          (is.na(v_raw) || !nzchar(trimws(as.character(v_raw))))) next
      
      # Normalize whitespace (also handle non-breaking spaces) and split on pipes
      t_str <- gsub("\u00A0", " ", as.character(t_raw))
      v_str <- gsub("\u00A0", " ", as.character(v_raw))
      t_parts <- unlist(strsplit(t_str, "\\s*\\|\\s*"), use.names = FALSE)
      v_parts <- unlist(strsplit(v_str, "\\s*\\|\\s*"), use.names = FALSE)
      t_parts <- t_parts[nzchar(t_parts)]
      v_parts <- v_parts[nzchar(v_parts)]
      
      # If decimals may use commas, uncomment:
      # t_parts <- gsub(",", ".", t_parts, fixed = TRUE)
      # v_parts <- gsub(",", ".", v_parts, fixed = TRUE)
      
      t_nums <- suppressWarnings(as.numeric(t_parts))
      v_nums <- suppressWarnings(as.numeric(v_parts))
      
      # Align by shortest length
      m <- min(length(t_nums), length(v_nums))
      if (m == 0L) next
      t_nums <- t_nums[seq_len(m)]
      v_nums <- v_nums[seq_len(m)]
      
      # Candidate movement indices for this slider
      if (ignore_zero) {
        cand <- which(is.finite(t_nums) & t_nums > 0)
      } else {
        cand <- which(is.finite(t_nums))
      }
      if (!length(cand)) next
      
      j <- cand[which.min(t_nums[cand])]
      ts <- t_nums[j]
      val <- v_nums[j]
      init_val <- v_nums[1]  # initial value (usually at time 0)
      
      if (!is.na(ts)) {
        # Tie-breaker: smallest timestamp, then lowest slider index
        if (ts < best_ts || (is.finite(ts) && is.finite(best_ts) && ts == best_ts && k < best_slider)) {
          best_ts <- ts
          best_val <- val
          best_slider <- k
          best_init_val <- init_val
        }
      }
    }
    
    if (is.finite(best_ts)) {
      first_ts[i]    <- best_ts
      first_val[i]   <- best_val
      dist50_val[i]  <- if (!is.na(best_val)) abs(best_val - 50) else NA_real_
      diff_init50[i] <- if (!is.na(best_init_val)) abs(best_init_val - 50) else NA_real_
    } else {
      first_ts[i]    <- NA_real_
      first_val[i]   <- NA_real_
      dist50_val[i]  <- NA_real_
      diff_init50[i] <- NA_real_
    }
  }
  
  data$first_move_timestamp   <- first_ts
  data$first_moved_value_z      <- scale(first_val)
  data$first_move_distance_50_z <- scale(dist50_val)       # |first_moved_value - 50|
  data$first_move_difficulty_z  <- scale(diff_init50)      # |initial value (time 0) of first-moved slider - 50|
  data
}

data <- first_move_function(data)

# median first move in low and high treatment
print(median(data$first_move_timestamp[data$els_slider.1.player.treatment == "low" & !is.na(data$first_move_timestamp)]))
print(median(data$first_move_timestamp[data$els_slider.1.player.treatment == "high" & !is.na(data$first_move_timestamp)]))

# regression of first move time on treatment and moved_ratio_50_all, controlling for first_move_difficulty and ability
first_move_ratio_50_regression <- lm(first_move_timestamp ~ els_slider.1.player.treatment + moved_ratio_50_all +  first_move_distance_50_z + first_move_difficulty_z + ability_sliders_50_z, data = data)
summary(first_move_ratio_50_regression)

first_move_count_50_regression <- lm(first_move_timestamp ~ els_slider.1.player.treatment + sliders_at_50_count +  first_move_distance_50_z + first_move_difficulty_z + ability_sliders_50_z, data = data)
summary(first_move_count_50_regression)

cognitive_conflict_regression(first_move_ratio_50_regression, first_move_count_50_regression,
                              title = "Regression Results: Cognitive Conflict and Movement Behavior",
                              covariate.labels = c("Treatment (Low)", "Moved Ratio (50)", "Sliders Count (50)", "Distance of First Moved Value from 50 (z)", "Difficulty of First Moved Slider (z)", "Ability (z)", "Constant"))