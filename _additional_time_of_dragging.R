# when are sliders moved
extract_movement_times <- function(data) {
  time_cols <- grep("els_slider\\.1\\.player\\.slider_history_time_", names(data), value = TRUE)
  all_movement_times <- c()
  for (col in time_cols) {
    time_histories <- data[[col]]
    for (time_history in time_histories) {
      if (!is.na(time_history) && time_history != "") {
        times <- as.numeric(str_split(time_history, " \\| ")[[1]])
        movement_times <- times[times > 0 & !is.na(times)]
        all_movement_times <- c(all_movement_times, movement_times)
      }
    }
  }
  return(all_movement_times)
}
movement_times <- extract_movement_times(data)
max_time <- max(max(movement_times), 120)
bins <- seq(0, max_time + 10, by = 10)
bin_labels <- paste0(bins[-length(bins)], "-", bins[-1])
movement_counts <- hist(movement_times, breaks = bins, plot = FALSE)$counts
total_sliders <- nrow(data) * 48
movement_ratios <- movement_counts / total_sliders
plot_data <- data.frame(
  time_bin = factor(bin_labels, levels = bin_labels),
  ratio = movement_ratios,
  bin_midpoint = bins[-length(bins)] + 5  # For positioning
)

ggplot(plot_data, aes(x = time_bin, y = ratio)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Ratio of Sliders Moved Over Time",
    subtitle = "Distribution of slider movements in 10-second bins",
    x = "Time Bins (seconds)",
    y = "Ratio of Sliders Moved",
    caption = paste("Total sliders:", total_sliders, "| Total movements:", sum(movement_counts))
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray60")
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))

# when sliders are moved to 50
extract_times_for_value_50 <- function(data) {
  value_cols <- grep("els_slider\\.1\\.player\\.slider_history_[0-9]+$", names(data), value = TRUE)
  all_times_to_50 <- c()
  for (slider_num in 0:47) {
    value_col <- paste0("els_slider.1.player.slider_history_", slider_num)
    time_col <- paste0("els_slider.1.player.slider_history_time_", slider_num)
    if (value_col %in% names(data) && time_col %in% names(data)) {
      for (row in 1:nrow(data)) {
        value_history <- data[[value_col]][row]
        time_history <- data[[time_col]][row]
        if (is.na(value_history) || is.na(time_history) || 
            value_history == "" || time_history == "") {
          next
        }
        values <- as.numeric(str_split(value_history, " \\| ")[[1]])
        times <- as.numeric(str_split(time_history, " \\| ")[[1]])
        if (length(values) == length(times)) {
          value_50_positions <- which(values == 50 & times > 0)
          times_to_50 <- times[value_50_positions]
          all_times_to_50 <- c(all_times_to_50, times_to_50)
        }
      }
    }
  }
  return(all_times_to_50)
}

times_to_50 <- extract_times_for_value_50(data)
max_time <- max(max(times_to_50), 120)
bins <- seq(0, max_time + 10, by = 10)
bin_labels <- paste0(bins[-length(bins)], "-", bins[-1])
movement_counts <- hist(times_to_50, breaks = bins, plot = FALSE)$counts
plot_data <- data.frame(
  time_bin = factor(bin_labels, levels = bin_labels),
  count = movement_counts,
  bin_start = bins[-length(bins)]
)

ggplot(plot_data, aes(x = time_bin, y = count)) +
  geom_col(fill = "darkred", alpha = 0.7) +
  labs(
    title = "Number of Sliders Moved to Value 50 Over Time",
    subtitle = "Distribution of movements to value 50 in 10-second bins",
    x = "Time Bins (seconds)",
    y = "Number of Slider Movements to 50",
    caption = paste("Total movements to 50:", sum(movement_counts))
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray60")
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks())

# check whether dragging to 50 was the first attempt
extract_first_attempt_to_50 <- function(data) {
  all_times_first_50 <- c()
  for (slider_num in 0:47) {
    value_col <- paste0("els_slider.1.player.slider_history_", slider_num)
    time_col <- paste0("els_slider.1.player.slider_history_time_", slider_num)
    if (value_col %in% names(data) && time_col %in% names(data)) {
      for (row in 1:nrow(data)) {
        value_history <- data[[value_col]][row]
        time_history <- data[[time_col]][row]
        if (is.na(value_history) || is.na(time_history) || 
            value_history == "" || time_history == "") {
          next
        }
        values <- as.numeric(str_split(value_history, " \\| ")[[1]])
        times <- as.numeric(str_split(time_history, " \\| ")[[1]])
        if (length(values) >= 2 && length(values) == length(times)) {
          if (values[2] == 50) {
            first_attempt_time <- times[2]
            
            if (first_attempt_time > 0) {
              all_times_first_50 <- c(all_times_first_50, first_attempt_time)
            }
          }
        }
      }
    }
  }
  
  return(all_times_first_50)
}

create_first_attempt_plot <- function(data, treatment_name) {
  # Get raw first-attempt times in seconds and drop anything > 120
  times_first_50 <- extract_first_attempt_to_50(data)
  times_first_50 <- times_first_50[!is.na(times_first_50) & times_first_50 <= 120]
  
  # Parameters
  bin_width <- 10
  bins <- seq(0, 120, by = bin_width)  # last bin is (110, 120], no 120–130 bin
  
  # Histogram counts using (a, b] bins to include 120 in the last bin
  movement_hist <- hist(
    times_first_50,
    breaks = bins,
    plot = FALSE,
    include.lowest = TRUE,
    right = TRUE
  )
  movement_counts <- movement_hist$counts
  total_n <- sum(movement_counts)
  
  # Build plot data with continuous x (bin midpoints) for an exact median line
  plot_data <- data.frame(
    bin_start = bins[-length(bins)],
    bin_end   = bins[-1],
    bin_mid   = bins[-length(bins)] + bin_width / 2,
    count     = movement_counts
  )
  
  # Optionally trim trailing zero-only bins (keeps behavior similar to your original)
  if (any(movement_counts > 0)) {
    last_non_zero <- max(which(movement_counts > 0))
    plot_data <- plot_data[1:last_non_zero, , drop = FALSE]
  }
  
  # X limit up to the last kept bin (or 120 if nothing trimmed)
  x_max <- if (nrow(plot_data) > 0) max(plot_data$bin_end) else 120
  
  ggplot(plot_data, aes(x = bin_mid, y = count)) +
    geom_col(
      width = bin_width,
      fill = if (treatment_name == "Low Treatment") "#4A90E2" else "#E94A2B",
      color = "white",
      alpha = 0.8
    ) +
    scale_x_continuous(
      breaks = seq(0, x_max, by = 30),
      limits = c(0, x_max),
      expand = expansion(mult = 0.02)
    ) +
    # Force last vertical tick to be 250
    scale_y_continuous(
      breaks = seq(0, 250, by = 50),
      limits = c(0, 250),
      expand = c(0, 0)
    ) +
    labs(
      x = "Time (in Seconds)",
      y = "Number of First Attempts to 50"
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 12, face = "bold"),
    )
}

plot_low <- create_first_attempt_plot(data_low, "Low Treatment")
ggsave("../Masterthesis-Overleaf/Images/first_attempts_low.png", plot_low, width = 8, height = 5)
print( plot_low )
plot_high <- create_first_attempt_plot(data_high, "High Treatment")
ggsave("../Masterthesis-Overleaf/Images/first_attempts_high.png", plot_high, width = 8, height = 5)
print( plot_high )

# check whether participants corrected themselves
extract_correction_times_to_50 <- function(data) {
  all_correction_times <- c()
  for (slider_num in 0:47) {
    value_col <- paste0("els_slider.1.player.slider_history_", slider_num)
    time_col <- paste0("els_slider.1.player.slider_history_time_", slider_num)
    if (value_col %in% names(data) && time_col %in% names(data)) {
      for (row in 1:nrow(data)) {
        value_history <- data[[value_col]][row]
        time_history <- data[[time_col]][row]
        if (is.na(value_history) || is.na(time_history) || 
            value_history == "" || time_history == "") {
          next
        }
        values <- as.numeric(str_split(value_history, " \\| ")[[1]])
        times <- as.numeric(str_split(time_history, " \\| ")[[1]])
        if (length(values) >= 3 && length(values) == length(times)) {
          initial_value <- values[1]
          if (initial_value >= 40 && initial_value <= 60) {
            next
          }
          correction_found <- FALSE
          for (i in 2:(length(values)-1)) { 
            current_value <- values[i]
            if (current_value >= 40 && current_value <= 60 && current_value != 50) {
              for (j in (i+1):length(values)) {
                if (values[j] == 50) {
                  correction_time <- times[j]
                  if (correction_time > 0) {
                    all_correction_times <- c(all_correction_times, correction_time)
                    correction_found <- TRUE
                    break  
                  }
                }
              }
              
              if (correction_found) {
                break  
              }
            }
          }
        }
      }
    }
  }
  
  return(all_correction_times)
}

create_correction_plot <- function(data, treatment_name) {
  # Get raw correction times and drop anything > 120
  correction_times <- extract_correction_times_to_50(data)
  correction_times <- correction_times[!is.na(correction_times) & correction_times <= 120]
  
  if (length(correction_times) == 0) {
    cat(paste("No correction behaviors (<=120s) found in", treatment_name, "\n"))
    return(NULL)
  }
  
  # Binning: 10-second bins up to 120 (last bin is (110, 120])
  bin_width <- 10
  bins <- seq(0, 120, by = bin_width)
  bin_labels <- paste0(bins[-length(bins)], "-", bins[-1])
  
  # Histogram counts; include 120 in last bin
  correction_hist <- hist(
    correction_times,
    breaks = bins,
    plot = FALSE,
    include.lowest = TRUE,
    right = TRUE
  )
  correction_counts <- correction_hist$counts
  
  plot_data <- data.frame(
    bin_start = bins[-length(bins)],
    bin_end   = bins[-1],
    bin_mid   = bins[-length(bins)] + bin_width / 2,
    count     = correction_counts
  )
  
  # Trim trailing zero-only bins for a compact plot
  if (any(correction_counts > 0)) {
    last_non_zero <- max(which(correction_counts > 0))
    plot_data <- plot_data[1:last_non_zero, , drop = FALSE]
    bin_labels <- bin_labels[1:last_non_zero]
    correction_counts <- correction_counts[1:last_non_zero]
  }
  
  x_max <- if (nrow(plot_data) > 0) max(plot_data$bin_end) else 120
  total_n <- sum(correction_counts)
  
  plot <- ggplot(plot_data, aes(x = bin_mid, y = count)) +
    geom_col(
      width = bin_width,
      fill = if (treatment_name == "Low Treatment") "#4A90E2" else "#E94A2B",
      color = "white",
      alpha = 0.8
    ) +
    scale_x_continuous(
      breaks = seq(0, x_max, by = 30),
      limits = c(0, x_max),
      expand = expansion(mult = 0.02)
    ) +
    # Force the last y-axis tick to be 75
    scale_y_continuous(
      breaks = seq(0, 75, by = 15),
      limits = c(0, 75),
      expand = c(0, 0)
    ) +
    labs(
      x = "Time (in Seconds)",
      y = "Number of Corrections to 50"
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 12, face = "bold")
    )
  
  # Console summary (per-bin counts)
  for (i in seq_along(correction_counts)) {
    if (correction_counts[i] > 0) {
      cat(bin_labels[i], ":", correction_counts[i], "corrections\n")
    }
  }
  
  return(plot)
}

plot_low <- create_correction_plot(data_low, "Low Treatment")
ggsave("../Masterthesis-Overleaf/Images/corrections_low.png", plot_low, width = 8, height = 5, dpi = 400)
print( plot_low )
plot_high <- create_correction_plot(data_high, "High Treatment")
ggsave("../Masterthesis-Overleaf/Images/corrections_high.png", plot_high, width = 8, height = 5, dpi = 400)
print( plot_high )

# test whether these differences in behavior are significant
count_first_attempts_per_person <- function(data) {
  first_attempts_counts <- c()
  for (row in 1:nrow(data)) {
    person_first_attempts <- 0
    for (slider_num in 0:47) {
      value_col <- paste0("els_slider.1.player.slider_history_", slider_num)
      if (value_col %in% names(data)) {
        value_history <- data[[value_col]][row]
        if (!is.na(value_history) && value_history != "") {
          values <- as.numeric(str_split(value_history, " \\| ")[[1]])
          if (length(values) >= 2 && values[2] == 50) {
            person_first_attempts <- person_first_attempts + 1
          }
        }
      }
    }
    first_attempts_counts <- c(first_attempts_counts, person_first_attempts)
  }
  return(first_attempts_counts)
}

count_corrections_per_person <- function(data) {
  correction_counts <- c()
  for (row in 1:nrow(data)) {
    person_corrections <- 0
    for (slider_num in 0:47) {
      value_col <- paste0("els_slider.1.player.slider_history_", slider_num)
      if (value_col %in% names(data)) {
        value_history <- data[[value_col]][row]
        if (!is.na(value_history) && value_history != "") {
          values <- as.numeric(str_split(value_history, " \\| ")[[1]])
          if (length(values) >= 3) {
            initial_value <- values[1]
            if (initial_value >= 40 && initial_value <= 60) {
              next
            }
            correction_found <- FALSE
            for (i in 2:(length(values)-1)) {
              current_value <- values[i]
              if (current_value >= 40 && current_value <= 60 && current_value != 50) {
                for (j in (i+1):length(values)) {
                  if (values[j] == 50) {
                    person_corrections <- person_corrections + 1
                    correction_found <- TRUE
                    break
                  }
                }
                if (correction_found) break
              }
            }
          }
        }
      }
    }
    correction_counts <- c(correction_counts, person_corrections)
  }
  return(correction_counts)
}

first_attempts_low <- count_first_attempts_per_person(data_low)
first_attempts_high <- count_first_attempts_per_person(data_high)
corrections_low <- count_corrections_per_person(data_low)
corrections_high <- count_corrections_per_person(data_high)

wilcox.test(first_attempts_low, first_attempts_high)
wilcox.test(corrections_low, corrections_high)

cat("\n=== DESCRIPTIVE STATISTICS ===\n")
cat("FIRST ATTEMPTS TO 50:\n")
cat("Low Treatment - Mean:", round(mean(first_attempts_low), 2), 
    ", Median:", median(first_attempts_low), 
    ", SD:", round(sd(first_attempts_low), 2), "\n")
cat("High Treatment - Mean:", round(mean(first_attempts_high), 2), 
    ", Median:", median(first_attempts_high), 
    ", SD:", round(sd(first_attempts_high), 2), "\n")

cat("\nCORRECTIONS TO 50:\n")
cat("Low Treatment - Mean:", round(mean(corrections_low), 2), 
    ", Median:", median(corrections_low), 
    ", SD:", round(sd(corrections_low), 2), "\n")
cat("High Treatment - Mean:", round(mean(corrections_high), 2), 
    ", Median:", median(corrections_high), 
    ", SD:", round(sd(corrections_high), 2), "\n")

# investigate whether time of drag matters
extract_first_attempt_times_per_person <- function(data) {
  person_times <- c()
  for (row in 1:nrow(data)) {
    individual_times <- c()
    for (slider_num in 0:47) {
      value_col <- paste0("els_slider.1.player.slider_history_", slider_num)
      time_col <- paste0("els_slider.1.player.slider_history_time_", slider_num)
      if (value_col %in% names(data) && time_col %in% names(data)) {
        value_history <- data[[value_col]][row]
        time_history <- data[[time_col]][row]
        if (!is.na(value_history) && !is.na(time_history) && 
            value_history != "" && time_history != "") {
          values <- as.numeric(str_split(value_history, " \\| ")[[1]])
          times <- as.numeric(str_split(time_history, " \\| ")[[1]])
          if (length(values) >= 2 && length(times) >= 2 && values[2] == 50) {
            individual_times <- c(individual_times, times[2])
          }
        }
      }
    }
    if (length(individual_times) > 0) {
      person_times <- c(person_times, median(individual_times)) 
    }
  }
  return(person_times)
}

extract_correction_times_per_person <- function(data) {
  person_times <- c()
  for (row in 1:nrow(data)) {
    individual_times <- c()
    for (slider_num in 0:47) {
      value_col <- paste0("els_slider.1.player.slider_history_", slider_num)
      time_col <- paste0("els_slider.1.player.slider_history_time_", slider_num)
      if (value_col %in% names(data) && time_col %in% names(data)) {
        value_history <- data[[value_col]][row]
        time_history <- data[[time_col]][row]
        if (!is.na(value_history) && !is.na(time_history) && 
            value_history != "" && time_history != "") {
          values <- as.numeric(str_split(value_history, " \\| ")[[1]])
          times <- as.numeric(str_split(time_history, " \\| ")[[1]])
          if (length(values) >= 3 && length(times) >= 3) {
            initial_value <- values[1]
            if (!(initial_value >= 40 && initial_value <= 60)) {
              correction_found <- FALSE
              for (i in 2:(length(values)-1)) {
                current_value <- values[i]
                if (current_value >= 40 && current_value <= 60 && current_value != 50) {
                  for (j in (i+1):length(values)) {
                    if (values[j] == 50) {
                      individual_times <- c(individual_times, times[j])
                      correction_found <- TRUE
                      break
                    }
                  }
                  if (correction_found) break
                }
              }
            }
          }
        }
      }
    }
    if (length(individual_times) > 0) {
      person_times <- c(person_times, median(individual_times))
    }
  }
  return(person_times)
}


first_attempt_times_low <- extract_first_attempt_times_per_person(data_low)
first_attempt_times_high <- extract_first_attempt_times_per_person(data_high)

correction_times_low <- extract_correction_times_per_person(data_low)
correction_times_high <- extract_correction_times_per_person(data_high)

wilcox.test(first_attempt_times_low, first_attempt_times_high)
wilcox.test(correction_times_low, correction_times_high)

cat("=== TIMING ANALYSIS ===\n")
cat("FIRST ATTEMPT TIMES:\n")
if (length(first_attempt_times_low) > 0) {
  cat("Low Treatment - Mean:", round(mean(first_attempt_times_low), 2), 
      "s, Median:", round(median(first_attempt_times_low), 2), "s\n")
}
if (length(first_attempt_times_high) > 0) {
  cat("High Treatment - Mean:", round(mean(first_attempt_times_high), 2), 
      "s, Median:", round(median(first_attempt_times_high), 2), "s\n")
}

cat("\nCORRECTION TIMES:\n")
if (length(correction_times_low) > 0) {
  cat("Low Treatment - Mean:", round(mean(correction_times_low), 2), 
      "s, Median:", round(median(correction_times_low), 2), "s\n")
}
if (length(correction_times_high) > 0) {
  cat("High Treatment - Mean:", round(mean(correction_times_high), 2), 
      "s, Median:", round(median(correction_times_high), 2), "s\n")
}



