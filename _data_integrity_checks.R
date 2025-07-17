# Function to check time calculations
check_times <- function(data) {
  time_fields <- c(
    "intro.1.player.introduction",
    "els_slider.1.player.worker_pre",
    "els_slider.1.player.worker",
    "els_slider.1.player.worker_explanation",
    "els_slider.1.player.worker_results",
    "els_slider.1.player.slider_instructions",
    "els_slider.1.player.comparison",
    "els_slider.1.player.display_rule",
    "els_slider.1.player.slider_task",
    "slider_beliefs.1.player.norm_pers",
    "slider_beliefs.1.player.norm_group",
    "slider_beliefs.1.player.empirical",
    "moral_foundations.1.player.part_1",
    "moral_foundations.1.player.part_2",
    "rule_orientation.1.player.part_1",
    "ability.1.player.ability_instructions",
    "ability.1.player.ability_slider",
    "demographics.1.player.demographics",
  )
  
  failed_fields <- character()
  
  for (field in time_fields) {
    start_field <- paste0(field, "_start_time")
    end_field <- paste0(field, "_end_time")
    time_field <- paste0(field, "_time")
    
    # Check if all required fields exist in the data
    if (start_field %in% names(data) && end_field %in% names(data) && time_field %in% names(data)) {
      # Calculate the time difference
      calculated_time <- data[[end_field]] - data[[start_field]]
      recorded_time <- data[[time_field]]
      max_diff <- max(abs(calculated_time - recorded_time), na.rm = TRUE)
      
      # Allow small floating point differences (0.01 seconds)
      if (max_diff > 0.01) {
        failed_fields <- c(failed_fields, field)
      }
    }
  }
  
  if (length(failed_fields) > 0) {
    return(list(
      status = "fail",
      message = paste("Time calculation inconsistencies found in fields:", paste(failed_fields, collapse = ", "))
    ))
  } else {
    return(list(
      status = "success",
      message = "All time calculations are consistent"
    ))
  }
}

# Function to check ELS score
check_els_score <- function(data) {
  # ELS items (excluding attention check)
  els_items <- c(
    "els_slider.1.player.manager_personal_life",
    "els_slider.1.player.manager_results_obtained", 
    "els_slider.1.player.manager_listen_employees",
    "els_slider.1.player.manager_discipline_employees", 
    "els_slider.1.player.manager_fair_decisions",
    "els_slider.1.player.manager_trust", 
    # Skipping attention check: "els_slider.1.player.manager_attention_check"
    "els_slider.1.player.manager_discuss_employees", 
    "els_slider.1.player.manager_set_example",
    "els_slider.1.player.manager_right_thing", 
    "els_slider.1.player.manager_best_interest"
  )
  
  # Calculate sum of individual items
  els_sum <- rowSums(data[, els_items], na.rm = FALSE)
  # Compare with recorded total
  els_diff <- els_sum - data$els_slider.1.player.els_score
  
  # Check if there are significant differences
  if (max(abs(els_diff), na.rm = TRUE) > 0.01) {
    inconsistent_count <- sum(abs(els_diff) > 0.01, na.rm = TRUE)
    return(list(
      status = "fail",
      message = paste0("ELS Score inconsistencies found in ", inconsistent_count, " observations")
    ))
  } else {
    return(list(
      status = "success",
      message = "All ELS scores are consistent with item sums"
    ))
  }
}

# Function to check Moral Foundations score
check_mf_score <- function(data) {
  # Moral Foundations items (excluding catch items)
  mf_items <- c(
    "moral_foundations_20.1.player.ingroup_lovecountry",
    "moral_foundations_20.1.player.ingroup_betray",
    "moral_foundations_20.1.player.ingroup_history",
    "moral_foundations_20.1.player.ingroup_family",
    "moral_foundations_20.1.player.authority_respect",
    "moral_foundations_20.1.player.authority_traditions",
    "moral_foundations_20.1.player.authority_kidrespect",
    "moral_foundations_20.1.player.authority_sexroles",
    "moral_foundations_20.1.player.purity_decency",
    "moral_foundations_20.1.player.purity_disgusting",
    "moral_foundations_20.1.player.purity_harmless",
    "moral_foundations_20.1.player.purity_unnatural"
    # Excluding catch items:
    # "moral_foundations_20.1.player.catch_math"
    # "moral_foundations_20.1.player.catch_good"
  )
  
  # Calculate sum of individual items
  mf_sum <- rowSums(data[, mf_items], na.rm = FALSE)
  # Compare with recorded total
  mf_diff <- mf_sum - data$moral_foundations_20.1.player.moral_foundations_score
  
  # Check if there are significant differences
  if (max(abs(mf_diff), na.rm = TRUE) > 0.01) {
    inconsistent_count <- sum(abs(mf_diff) > 0.01, na.rm = TRUE)
    return(list(
      status = "fail",
      message = paste0("Moral Foundations Score inconsistencies found in ", inconsistent_count, " observations")
    ))
  } else {
    return(list(
      status = "success",
      message = "All Moral Foundations scores are consistent with item sums"
    ))
  }
}

# Function to check Rule Orientation score
check_ro_score <- function(data) {
  ro_items <- c(
    "rule_orientation.1.player.moral_principles",
    "rule_orientation.1.player.unreasonable",
    "rule_orientation.1.player.expensive",
    "rule_orientation.1.player.not_enforced",
    "rule_orientation.1.player.others_break",
    "rule_orientation.1.player.unable",
    "rule_orientation.1.player.others_justified",
    "rule_orientation.1.player.not_know",
    "rule_orientation.1.player.not_understand",
    "rule_orientation.1.player.not_published",
    "rule_orientation.1.player.no_harm",
    "rule_orientation.1.player.benefit"
  )
  
  # Calculate sum of individual items
  ro_sum <- rowSums(data[, ro_items], na.rm = FALSE)
  # Compare with recorded total
  ro_diff <- ro_sum - data$rule_orientation.1.player.rule_orientation_score
  
  # Check if there are significant differences
  if (max(abs(ro_diff), na.rm = TRUE) > 0.01) {
    inconsistent_count <- sum(abs(ro_diff) > 0.01, na.rm = TRUE)
    return(list(
      status = "fail",
      message = paste0("Rule Orientation Score inconsistencies found in ", inconsistent_count, " observations")
    ))
  } else {
    return(list(
      status = "success",
      message = "All Rule Orientation scores are consistent with item sums"
    ))
  }
}

# Check slider history
check_slider_history <- function(data) {
  failed_sliders <- character()
  inconsistent_count <- 0
  
  # Check all 48 sliders (0 to 47)
  for (slider_num in 0:47) {
    initial_col <- paste0("els_slider.1.player.slider_initial_", slider_num)
    final_col <- paste0("els_slider.1.player.slider_final_", slider_num)
    history_col <- paste0("els_slider.1.player.slider_history_", slider_num)
    
    # Check if all required columns exist
    if (all(c(initial_col, final_col, history_col) %in% names(data))) {
      
      for (i in 1:nrow(data)) {
        initial_val <- data[[initial_col]][i]
        final_val <- data[[final_col]][i]
        history_val <- data[[history_col]][i]
        
        # Skip if any values are NA
        if (!is.na(initial_val) && !is.na(final_val) && !is.na(history_val)) {
          
          # Convert history to character and split by " | "
          history_str <- as.character(history_val)
          
          # If initial equals final, history could be just the single value
          if (initial_val == final_val) {
            # History should be either just the value or "value | value"
            if (history_str != as.character(initial_val) && 
                history_str != paste(initial_val, initial_val, sep = " | ")) {
              # Check if it's a valid single-value history
              history_positions <- suppressWarnings(as.numeric(strsplit(history_str, " \\| ")[[1]]))
              if (length(history_positions) == 1 && history_positions[1] != initial_val) {
                failed_sliders <- c(failed_sliders, paste0("slider_", slider_num))
                inconsistent_count <- inconsistent_count + 1
              } else if (length(history_positions) > 1 && 
                         (history_positions[1] != initial_val || 
                          history_positions[length(history_positions)] != final_val)) {
                failed_sliders <- c(failed_sliders, paste0("slider_", slider_num))
                inconsistent_count <- inconsistent_count + 1
              }
            }
          } else {
            # If initial != final, history must contain " | " and start with initial, end with final
            if (!grepl(" \\| ", history_str)) {
              failed_sliders <- c(failed_sliders, paste0("slider_", slider_num))
              inconsistent_count <- inconsistent_count + 1
            } else {
              history_positions <- suppressWarnings(as.numeric(strsplit(history_str, " \\| ")[[1]]))
              
              # Check if conversion was successful and positions are valid
              if (any(is.na(history_positions)) || length(history_positions) < 2) {
                failed_sliders <- c(failed_sliders, paste0("slider_", slider_num))
                inconsistent_count <- inconsistent_count + 1
              } else {
                # Check if first position matches initial and last matches final
                if (history_positions[1] != initial_val || 
                    history_positions[length(history_positions)] != final_val) {
                  failed_sliders <- c(failed_sliders, paste0("slider_", slider_num))
                  inconsistent_count <- inconsistent_count + 1
                }
              }
            }
          }
        }
      }
    }
  }
  
  if (inconsistent_count > 0) {
    # Show only first few failed sliders to avoid overwhelming output
    display_failed <- if(length(failed_sliders) > 10) {
      paste(c(head(failed_sliders, 10), "..."), collapse = ", ")
    } else {
      paste(failed_sliders, collapse = ", ")
    }
    
    return(list(
      status = "fail",
      message = paste0("Slider history inconsistencies found in ", inconsistent_count, 
                       " slider instances. Examples: ", display_failed)
    ))
  } else {
    return(list(
      status = "success", 
      message = "All slider histories are consistent with initial and final values"
    ))
  }
}

# Function to check equal distribution of initial slider values
check_slider_initial_distribution <- function(data) {
  # Collect all initial slider values from all participants and all sliders
  all_initial_values <- c()
  
  for (slider_num in 0:47) {
    initial_col <- paste0("els_slider.1.player.slider_initial_", slider_num)
    if (initial_col %in% names(data)) {
      values <- data[[initial_col]]
      all_initial_values <- c(all_initial_values, values[!is.na(values)])
    }
  }
  
  if (length(all_initial_values) == 0) {
    return(list(
      status = "fail",
      message = "No valid initial slider values found"
    ))
  }
  
  # Create frequency table for values 0-100
  value_counts <- table(factor(all_initial_values, levels = 0:100))
  observed_frequencies <- as.numeric(value_counts)
  
  # Calculate expected frequency (uniform distribution)
  total_values <- length(all_initial_values)
  expected_frequency <- total_values / 101  # 101 possible values (0-100)
  
  # Perform chi-square goodness of fit test
  # Only include values that were actually observed to avoid issues with zero expected frequencies
  observed_values <- which(observed_frequencies > 0)
  
  if (length(observed_values) < 2) {
    return(list(
      status = "fail",
      message = "Insufficient variation in initial values for distribution test"
    ))
  }
  
  # Chi-square test
  chi_square_result <- chisq.test(observed_frequencies[observed_values], 
                                  p = rep(1/101, length(observed_values)))
  
  # Calculate coefficient of variation as additional measure
  cv <- sd(observed_frequencies) / mean(observed_frequencies)
  
  # Calculate range of frequencies
  freq_range <- max(observed_frequencies) - min(observed_frequencies[observed_frequencies > 0])
  
  # Determine status based on chi-square p-value
  # Using p < 0.05 as threshold for "significantly non-uniform"
  if (chi_square_result$p.value < 0.05) {
    return(list(
      status = "fail",
      message = paste0("Initial slider values are not uniformly distributed. ",
                       "Chi-square p-value: ", format(chi_square_result$p.value, scientific = TRUE), 
                       ", CV: ", round(cv, 4),
                       ", Frequency range: ", freq_range,
                       " (Expected frequency: ", round(expected_frequency, 2), ")")
    ))
  } else {
    return(list(
      status = "success",
      message = paste0("Initial slider values are approximately uniformly distributed. ",
                       "Chi-square p-value: ", format(chi_square_result$p.value, scientific = TRUE),
                       ", CV: ", round(cv, 4),
                       ", Frequency range: ", freq_range,
                       " (Expected frequency: ", round(expected_frequency, 2), ")")
    ))
  }
}

# Function to check slider ratio and difficulty calculations
check_slider_calculations <- function(data) {
  failed_checks <- character()
  inconsistent_count <- 0
  
  for (i in 1:nrow(data)) {
    # Get all final slider values for this participant
    final_vals <- numeric(48)
    valid_sliders <- 0
    
    for (slider_num in 0:47) {
      final_col <- paste0("els_slider.1.player.slider_final_", slider_num)
      if (final_col %in% names(data) && !is.na(data[i, final_col])) {
        final_vals[slider_num + 1] <- data[i, final_col]
        valid_sliders <- valid_sliders + 1
      } else {
        final_vals[slider_num + 1] <- NA
      }
    }
    
    # Skip if no valid sliders
    if (valid_sliders == 0) next
    
    # Remove NAs for calculations
    valid_final_vals <- final_vals[!is.na(final_vals)]
    
    # Check slider_50_all_ratio
    if ("els_slider.1.player.slider_50_all_ratio" %in% names(data)) {
      calculated_50_ratio <- sum(valid_final_vals == 50) / length(valid_final_vals)
      recorded_50_ratio <- data[i, "els_slider.1.player.slider_50_all_ratio"]
      
      if (!is.na(recorded_50_ratio) && abs(calculated_50_ratio - recorded_50_ratio) > 0.001) {
        failed_checks <- c(failed_checks, paste0("Row ", i, ": slider_50_all_ratio"))
        inconsistent_count <- inconsistent_count + 1
      }
    }
    
    # Check slider_40_60_all_ratio
    if ("els_slider.1.player.slider_40_60_all_ratio" %in% names(data)) {
      calculated_4060_ratio <- sum(valid_final_vals >= 40 & valid_final_vals <= 60) / length(valid_final_vals)
      recorded_4060_ratio <- data[i, "els_slider.1.player.slider_40_60_all_ratio"]
      
      if (!is.na(recorded_4060_ratio) && abs(calculated_4060_ratio - recorded_4060_ratio) > 0.001) {
        failed_checks <- c(failed_checks, paste0("Row ", i, ": slider_40_60_all_ratio"))
        inconsistent_count <- inconsistent_count + 1
      }
    }
    
    # Check slider_50_difficulty
    if ("els_slider.1.player.slider_50_difficulty" %in% names(data)) {
      calculated_50_difficulty <- sum(abs(valid_final_vals - 50))
      recorded_50_difficulty <- data[i, "els_slider.1.player.slider_50_difficulty"]
      
      if (!is.na(recorded_50_difficulty) && abs(calculated_50_difficulty - recorded_50_difficulty) > 0.001) {
        failed_checks <- c(failed_checks, paste0("Row ", i, ": slider_50_difficulty"))
        inconsistent_count <- inconsistent_count + 1
      }
    }
    
    # Check slider_40_60_difficulty
    if ("els_slider.1.player.slider_40_60_difficulty" %in% names(data)) {
      # Calculate distance to 40-60 zone for each slider
      distances_to_zone <- sapply(valid_final_vals, function(val) {
        if (val >= 40 & val <= 60) {
          return(0)  # Inside the zone
        } else if (val < 40) {
          return(40 - val)  # Distance to lower bound
        } else {
          return(val - 60)  # Distance to upper bound
        }
      })
      
      calculated_4060_difficulty <- sum(distances_to_zone)
      recorded_4060_difficulty <- data[i, "els_slider.1.player.slider_40_60_difficulty"]
      
      if (!is.na(recorded_4060_difficulty) && abs(calculated_4060_difficulty - recorded_4060_difficulty) > 0.001) {
        failed_checks <- c(failed_checks, paste0("Row ", i, ": slider_40_60_difficulty"))
        inconsistent_count <- inconsistent_count + 1
      }
    }
  }
  
  if (inconsistent_count > 0) {
    # Show only first few failed checks to avoid overwhelming output
    display_failed <- if(length(failed_checks) > 15) {
      paste(c(head(failed_checks, 15), "..."), collapse = ", ")
    } else {
      paste(failed_checks, collapse = ", ")
    }
    
    return(list(
      status = "fail",
      message = paste0("Slider calculation inconsistencies found in ", inconsistent_count, 
                       " instances. Examples: ", display_failed)
    ))
  } else {
    return(list(
      status = "success",
      message = "All slider ratio and difficulty calculations are correct"
    ))
  }
}

# Main function to run all checks
data_integrity_checks <- function(data) {
  # Run all checks
  time_check <- check_times(data)
  els_check <- check_els_score(data)
  mf_check <- check_mf_score(data)
  ro_check <- check_ro_score(data)
  age_exp_check <- check_age_experience(data)
  slider_history_check <- check_slider_history(data)
  slider_distribution_check <- check_slider_initial_distribution(data)
  slider_calculations_check <- check_slider_calculations(data)
  
  # Collect results
  checks <- list(
    "Time calculations" = time_check,
    "ELS score" = els_check,
    "Moral Foundations score" = mf_check,
    "Rule Orientation score" = ro_check,
    "Age and experience relationships" = age_exp_check,
    "Slider history consistency" = slider_history_check,
    "Slider initial distribution" = slider_distribution_check,
    "Slider calculations" = slider_calculations_check
  )
  
  # Check if any checks failed
  failed_checks <- sapply(checks, function(check) check$status == "fail")
  
  if (any(failed_checks)) {
    # Get messages from failed checks
    failed_messages <- sapply(checks[failed_checks], function(check) check$message)
    failed_names <- names(checks)[failed_checks]
    
    # Create combined message
    combined_message <- paste0(
      "Data integrity checks: fail\n",
      "Failed checks: ", paste(failed_names, collapse = ", "), "\n",
      paste(failed_names, ":", failed_messages, collapse = "\n")
    )
    
    return(combined_message)
  } else {
    return("Data integrity checks: success")
  }
}