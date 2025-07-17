# Function to exclude standard oTree variables based on patterns
exclude_otree_standard_vars <- function(data) {
  # Define the prefixes
  prefixes <- c(
    "intro.1.player.",
    "set_rule.1.player.",
    "els_slider.1.player.",
    "moral_foundations_20.1.player.",
    "rule_orientation.1.player.",
    "demographics.1.player.",
    "payment.1.player."
  )
  
  # Define the suffixes to exclude
  suffixes <- c(
    "id_in_subsession", 
    "round_number", 
    "id_in_group", 
    "payoff"
  )
  
  # Find all variables that match the pattern (prefix + suffix)
  vars_to_exclude <- character()
  
  for (prefix in prefixes) {
    for (suffix in suffixes) {
      var_name <- paste0(prefix, suffix)
      if (var_name %in% names(data)) {
        vars_to_exclude <- c(vars_to_exclude, var_name)
      }
    }
  }
  
  # Exclude the variables if any were found
  if (length(vars_to_exclude) > 0) {
    data <- data[, !names(data) %in% vars_to_exclude]
  }
  
  return(data)
}