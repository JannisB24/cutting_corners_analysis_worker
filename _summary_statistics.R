create_summary_table_slider <- function(data, var_name) {
  # Function to calculate summary stats for a vector
  calc_stats <- function(x) {
    x <- x[!is.na(x)]  # Remove NAs
    if(length(x) == 0) {
      return(rep(NA, 7))
    }
    c(
      min = round(min(x), 4),
      q25 = round(quantile(x, 0.25), 4),
      median = round(median(x), 4),
      mean = round(mean(x), 4),
      q75 = round(quantile(x, 0.75), 4),
      max = round(max(x), 4),
      sd = round(sd(x), 4)
    )
  }
  
  # Get the variable for each group
  var_all <- data[[var_name]]
  var_low <- data[data$els_slider.1.player.treatment == "low", var_name]
  var_high <- data[data$els_slider.1.player.treatment == "high", var_name]
  
  # Calculate stats for each group
  all_stats <- calc_stats(var_all)
  low_stats <- calc_stats(var_low)
  high_stats <- calc_stats(var_high)
  
  # Create results dataframe
  results <- data.frame(
    Group = c("All", "Low", "High"),
    Min = c(all_stats[1], low_stats[1], high_stats[1]),
    Q25 = c(all_stats[2], low_stats[2], high_stats[2]),
    Median = c(all_stats[3], low_stats[3], high_stats[3]),
    Mean = c(all_stats[4], low_stats[4], high_stats[4]),
    Q75 = c(all_stats[5], low_stats[5], high_stats[5]),
    Max = c(all_stats[6], low_stats[6], high_stats[6]),
    SD = c(all_stats[7], low_stats[7], high_stats[7])
  )
  
  return(results)
}

# Function to create all summary tables
create_all_summary_tables <- function(data) {
  variables <- c(
    "sliders_at_50_count", 
    "sliders_in_range_count", 
    "ratio_50_all", 
    "ratio_50_outside", 
    "ratio_50_inside", 
    "ratio_4060_all", 
    "ratio_4060_outside", 
    "ratio_4060_inside", 
    "moved_ratio_50_all", 
    "moved_ratio_50_outside", 
    "moved_ratio_50_inside"
  )
  
  # Create a list to store all tables
  summary_tables <- list()
  
  # Generate table for each variable
  for(var in variables) {
    if(var %in% names(data)) {
      summary_tables[[var]] <- create_summary_table_slider(data, var)
      cat("\n=== Summary Statistics for", var, "===\n")
      print(summary_tables[[var]])
      cat("\n")
    } else {
      cat("Warning: Variable", var, "not found in data\n")
    }
  }
  
  return(invisible(summary_tables))
}

summary_statistics_age <- function(data) {
  # Calculate summary statistics
  stats <- c(
    N = sum(!is.na(data$demographics.1.player.age)),
    Mean = mean(data$demographics.1.player.age, na.rm = TRUE),
    SD = sd(data$demographics.1.player.age, na.rm = TRUE),
    Median = median(data$demographics.1.player.age, na.rm = TRUE),
    Min = min(data$demographics.1.player.age, na.rm = TRUE),
    Max = max(data$demographics.1.player.age, na.rm = TRUE),
    Q25 = quantile(data$demographics.1.player.age, 0.25, na.rm = TRUE),
    Q75 = quantile(data$demographics.1.player.age, 0.75, na.rm = TRUE)
  )
  
  # Round numeric values
  stats <- round(stats, 1)
  
  # Create a data frame for better printing
  stats_df <- data.frame(Statistic = names(stats), Value = stats)
  
  # Print the table
  print(stats_df, row.names = FALSE)
  
  # Return invisibly for assignment if needed
  invisible(stats_df)
}

gender_summary_table <- function(data) {
  # Generate frequency table
  freq_table <- table(data$demographics.1.player.gender)
  
  # Calculate percentages
  percentages <- prop.table(freq_table) * 100
  
  # Create summary table
  gender_df <- data.frame(
    Gender = names(freq_table),
    Count = as.numeric(freq_table),
    Percentage = round(as.numeric(percentages), 1)
  )
  
  # Add percentage symbol
  gender_df$Percentage <- paste0(gender_df$Percentage, "%")
  
  # Print the table
  print(gender_df, row.names = FALSE)
  
  # Return invisibly for assignment if needed
  invisible(gender_df)
}

summary_statistics_gender <- function(data) {
  # Generate frequency table
  freq_table <- table(data$demographics.1.player.gender)
  
  # Calculate percentages
  percentages <- prop.table(freq_table) * 100
  
  # Create summary table
  gender_df <- data.frame(
    Gender = names(freq_table),
    Count = as.numeric(freq_table),
    Percentage = round(as.numeric(percentages), 1)
  )
  
  # Add percentage symbol
  gender_df$Percentage <- paste0(gender_df$Percentage, "%")
  
  # Print the table
  print(gender_df, row.names = FALSE)
  
  # Return invisibly for assignment if needed
  invisible(gender_df)
}

summary_statistics_education <- function(data, include_order = TRUE) {
  # Define order if requested (for more meaningful presentation)
  if (include_order) {
    education_order <- c(
      "elementary_school", "middle_school", "high_school", 
      "bachelor_degree", "master_degree", "phd", 
      "other", "prefer_not_to_say"
    )
    
    # Convert to ordered factor
    data$demographics.1.player.education <- factor(data$demographics.1.player.education, levels = education_order)
  }
  
  # Generate frequency table
  freq_table <- table(data$demographics.1.player.education)
  
  # Calculate percentages
  percentages <- prop.table(freq_table) * 100
  
  # Create summary table
  education_df <- data.frame(
    Education = names(freq_table),
    Count = as.numeric(freq_table),
    Percentage = round(as.numeric(percentages), 1)
  )
  
  # Add percentage symbol
  education_df$Percentage <- paste0(education_df$Percentage, "%")
  
  # Print the table
  print(education_df, row.names = FALSE)
  
  # Return invisibly for assignment if needed
  invisible(education_df)
}

summary_statistics_management_education <- function(data) {
  # Define order (optional)
  mgmt_order <- c(
    "techniker", "meister", "mba", "other", "none", "prefer_not_to_say"
  )
  
  # Convert to ordered factor
  data$demographics.1.player.management_education <- factor(data$demographics.1.player.management_education, levels = mgmt_order)
  
  # Generate frequency table
  freq_table <- table(data$demographics.1.player.management_education)
  
  # Calculate percentages
  percentages <- prop.table(freq_table) * 100
  
  # Create summary table
  mgmt_df <- data.frame(
    Management_Education = names(freq_table),
    Count = as.numeric(freq_table),
    Percentage = round(as.numeric(percentages), 1)
  )
  
  # Add percentage symbol
  mgmt_df$Percentage <- paste0(mgmt_df$Percentage, "%")
  
  # Print the table
  print(mgmt_df, row.names = FALSE)
  
  # Return invisibly for assignment if needed
  invisible(mgmt_df)
}

# Summary table for work experience as manager
summary_statistics_work_experience_manager <- function(data) {
  # Calculate summary statistics
  stats <- c(
    N = sum(!is.na(data$demographics.1.player.work_experience_manager)),
    Mean = mean(data$demographics.1.player.work_experience_manager, na.rm = TRUE),
    SD = sd(data$demographics.1.player.work_experience_manager, na.rm = TRUE),
    Median = median(data$demographics.1.player.work_experience_manager, na.rm = TRUE),
    Min = min(data$demographics.1.player.work_experience_manager, na.rm = TRUE),
    Max = max(data$demographics.1.player.work_experience_manager, na.rm = TRUE),
    Q25 = quantile(data$demographics.1.player.work_experience_manager, 0.25, na.rm = TRUE),
    Q75 = quantile(data$demographics.1.player.work_experience_manager, 0.75, na.rm = TRUE)
  )
  
  # Round numeric values
  stats <- round(stats, 1)
  
  # Create a data frame for better printing
  stats_df <- data.frame(Statistic = names(stats), Value = stats)
  
  # Print the table
  print(stats_df, row.names = FALSE)
  
  # Return invisibly for assignment if needed
  invisible(stats_df)
}

# Summary table for management experience
summary_statistics_management_experience <- function(data) {
  # Calculate summary statistics
  stats <- c(
    N = sum(!is.na(data$demographics.1.player.management_experience)),
    Mean = mean(data$demographics.1.player.management_experience, na.rm = TRUE),
    SD = sd(data$demographics.1.player.management_experience, na.rm = TRUE),
    Median = median(data$demographics.1.player.management_experience, na.rm = TRUE),
    Min = min(data$demographics.1.player.management_experience, na.rm = TRUE),
    Max = max(data$demographics.1.player.management_experience, na.rm = TRUE),
    Q25 = quantile(data$demographics.1.player.management_experience, 0.25, na.rm = TRUE),
    Q75 = quantile(data$demographics.1.player.management_experience, 0.75, na.rm = TRUE)
  )
  
  # Round numeric values
  stats <- round(stats, 1)
  
  # Create a data frame for better printing
  stats_df <- data.frame(Statistic = names(stats), Value = stats)
  
  # Print the table
  print(stats_df, row.names = FALSE)
  
  # Return invisibly for assignment if needed
  invisible(stats_df)
}

# Summary table for number of people led
summary_statistics_people_led <- function(data) {
  # Calculate summary statistics
  stats <- c(
    N = sum(!is.na(data$demographics.1.player.people_lead)),
    Mean = mean(data$demographics.1.player.people_lead, na.rm = TRUE),
    SD = sd(data$demographics.1.player.people_lead, na.rm = TRUE),
    Median = median(data$demographics.1.player.people_lead, na.rm = TRUE),
    Min = min(data$demographics.1.player.people_lead, na.rm = TRUE),
    Max = max(data$demographics.1.player.people_lead, na.rm = TRUE),
    Q25 = quantile(data$demographics.1.player.people_lead, 0.25, na.rm = TRUE),
    Q75 = quantile(data$demographics.1.player.people_lead, 0.75, na.rm = TRUE)
  )
  
  # Round numeric values
  stats <- round(stats, 1)
  
  # Create a data frame for better printing
  stats_df <- data.frame(Statistic = names(stats), Value = stats)
  
  # Print the table
  print(stats_df, row.names = FALSE)
  
  # Return invisibly for assignment if needed
  invisible(stats_df)
}

summary_statistics_political_orientation <- function(data) {
  # Generate frequency table
  freq_table <- table(data$demographics.1.player.political_orientation)
  
  # Calculate percentages
  percentages <- prop.table(freq_table) * 100
  
  # Create summary table
  political_df <- data.frame(
    Political_Orientation = names(freq_table),
    Count = as.numeric(freq_table),
    Percentage = round(as.numeric(percentages), 1)
  )
  
  # Add percentage symbol
  political_df$Percentage <- paste0(political_df$Percentage, "%")
  
  # Print the table
  print(political_df, row.names = FALSE)
  
  # Return invisibly for assignment if needed
  invisible(political_df)
}