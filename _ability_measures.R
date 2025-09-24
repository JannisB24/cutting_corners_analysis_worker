ability_measures <- function(data) {
  data$ability_sliders_50 <- NA
  data$ability_difficulty <- NA
  data$ability_time_taken <- NA
  data$ability_score <- NA
  
  for(i in 1:nrow(data)) {
    # Get initial and final values for ability task (sliders 0-9)
    initial_vals <- as.numeric(data[i, paste0("ability.1.player.slider_initial_", 0:9)])
    final_vals <- as.numeric(data[i, paste0("ability.1.player.slider_final_", 0:9)])
    
    # Calculate how many sliders they got to 50
    sliders_at_50 <- sum(final_vals == 50, na.rm = TRUE)
    data$ability_sliders_50[i] <- sliders_at_50
    
    # Calculate difficulty (sum of distances from initial to 50)
    distances <- abs(initial_vals - 50)
    data$ability_difficulty[i] <- sum(distances, na.rm = TRUE)
    
    # Get time taken and convert to numeric
    time_raw <- data[i, "ability.1.player.ability_slider_time"]
    data$ability_time_taken[i] <- as.numeric(time_raw)
  }
  
  # Calculate mean difficulty for normalization
  mean_difficulty <- mean(data$ability_difficulty, na.rm = TRUE)
  
  # Calculate ability score
  for(i in 1:nrow(data)) {
    sliders_50 <- data$ability_sliders_50[i]
    difficulty <- data$ability_difficulty[i]
    time_taken <- data$ability_time_taken[i]
    
    if(!is.na(sliders_50) && !is.na(difficulty)) {
      # Base score: sliders completed * relative difficulty
      base_score <- sliders_50 * (difficulty / mean_difficulty)
      
      # If they completed all 10 sliders, multiply by time factor
      if(sliders_50 == 10 && !is.na(time_taken) && time_taken > 0) {
        # Time multiplier: 25 / time_taken
        # Faster completion = higher multiplier
        time_multiplier <- 25 / time_taken
        data$ability_score[i] <- base_score * time_multiplier
      } else {
        # No time adjustment for those who didn't complete all 10
        data$ability_score[i] <- base_score
      }
    }
  }
  
  return(data)
}