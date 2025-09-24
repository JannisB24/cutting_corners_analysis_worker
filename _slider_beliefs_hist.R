slider_beliefs_pers_hist_ggplot <- function(data, var_name, treatment_label, labels) {
  # Only keep non-NA values
  values <- data[[var_name]]
  values_clean <- values[!is.na(values)]
  
  # Define Likert values and convert to factor
  likert_values <- c(1, 2, 3, 4)
  df <- data.frame(values = factor(values_clean, levels = likert_values, labels = labels))
  
  # Calculate mean as a factor index for optional vline (not always meaningful for discrete)
  mean_val <- mean(values_clean)
  n_obs <- length(values_clean)
  
  # Create plot with consistent design language
  p <- ggplot(df, aes(x = values)) +
    geom_bar(
      aes(y = ..count../sum(..count..)),
      fill = "#4A90E2", color = "white", alpha = 0.8
    ) +
    scale_x_discrete(drop = FALSE) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      breaks = seq(0, 1, by = 0.2),
      limits = c(0, 1),
      expand = expansion(mult = 0.02)
    ) +
    labs(
      x = "Response Scale",
      y = "Proportion"
    ) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 8)) # or your preferred size
  print(p)
  return(p)
}

slider_beliefs_group_hist_ggplot <- function(data, var_name, treatment_label, labels) {
  # Only keep non-NA values
  values <- data[[var_name]]
  values_clean <- values[!is.na(values)]
  
  # Define the Likert values and convert to factor
  likert_values <- c(-3, -1, 1, 3)
  df <- data.frame(values = factor(values_clean, levels = likert_values, labels = labels))
  
  n_obs <- length(values_clean)
  mean_val <- mean(values_clean)
  
  # Create plot with consistent design
  p <- ggplot(df, aes(x = values)) +
    geom_bar(
      aes(y = ..count../sum(..count..)),
      fill = "#4A90E2", color = "white", alpha = 0.8
    ) +
    scale_x_discrete(drop = FALSE) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      breaks = seq(0, 1, by = 0.2),
      limits = c(0, 1),
      expand = expansion(mult = 0.02)
    ) +
    labs(
      x = "Response Scale",
      y = "Proportion"
    ) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 8)) # or your preferred size
  print(p)
  return(p)
}

slider_beliefs_emp_hist_ggplot <- function(data, treatment_label, y_max = 0.5) {
  # Extract and clean values
  values <- data[["slider_beliefs.1.player.empirical_belief_compliance"]]
  values_clean <- values[!is.na(values)]
  df <- data.frame(values = values_clean)
  
  n_obs <- length(values_clean)
  mean_val <- round(mean(values_clean), 2)
  median_val <- round(median(values_clean), 2)
  
  # Histogram bins: 0–10, 10–20, ..., 90–100
  bin_breaks <- seq(0, 100, by = 10)
  bin_labels <- function(x) gsub(",", "–", gsub("\\[|\\)|\\]", "", x))
  bins <- cut(values_clean, breaks = bin_breaks, right = TRUE, include.lowest = TRUE)
  
  p <- ggplot(df, aes(x = bins)) +
    geom_bar(aes(y = ..count../sum(..count..)), fill = "#4A90E2", color = "white", alpha = 0.8) +
    scale_x_discrete(
      drop = FALSE,
      labels = bin_labels
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = expansion(mult = 0.02),
      limits = c(0, y_max),
      breaks = seq(0, y_max, by = 0.1)
    ) +
    labs(
      x = "Belief about % of others who will comply (0–100, in 10-point bins)",
      y = "Proportion"
    ) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 8)) # or your preferred size
  
  print(p)
  invisible(values_clean)
}