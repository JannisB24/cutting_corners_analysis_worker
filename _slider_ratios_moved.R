# Function to calculate rule-following ratios among moved sliders only
slider_ratios_moved <- function(data) {
  results <- data.frame(
    participant_id = 1:nrow(data),
    
    # Measure 1: Active Rule-Following (exclude init at 50)
    ratio_50_active_following = NA,
    count_active_following = NA,
    
    # Measure 2: Effort-Required Rule-Following (exclude init at 40-60) 
    ratio_50_effort_required = NA,
    count_effort_required = NA,
    
    # Measure 3: Total Engagement Rule-Following (include all moved)
    ratio_50_total_engagement = NA,
    count_total_engagement = NA,
  )
  
  for(i in 1:nrow(data)) {
    # Get initial and final values
    initial_vals <- as.numeric(data[i, paste0("els_slider.1.player.slider_initial_", 0:47)])
    final_vals <- as.numeric(data[i, paste0("els_slider.1.player.slider_final_", 0:47)])
    
    # Remove NAs
    valid_indices <- !is.na(initial_vals) & !is.na(final_vals)
    initial_vals <- initial_vals[valid_indices]
    final_vals <- final_vals[valid_indices]
    
    if(length(initial_vals) > 0) {
      # Get moved sliders (for measures 1-3)
      moved_indices <- initial_vals != final_vals
      moved_initial <- initial_vals[moved_indices]
      moved_final <- final_vals[moved_indices]
      
      # Measure 1: Active Rule-Following (moved, exclude those who started at 50)
      if(sum(moved_indices) > 0) {
        active_indices <- moved_initial != 50
        if(sum(active_indices) > 0) {
          results$ratio_50_active_following[i] <- sum(moved_final[active_indices] == 50) / sum(active_indices)
          results$count_active_following[i] <- sum(active_indices)
        }
        
        # Measure 2: Effort-Required Rule-Following (moved, exclude those who started in 40-60)
        effort_indices <- moved_initial < 40 | moved_initial > 60
        if(sum(effort_indices) > 0) {
          results$ratio_50_effort_required[i] <- sum(moved_final[effort_indices] == 50) / sum(effort_indices)
          results$count_effort_required[i] <- sum(effort_indices)
        }
        
        # Measure 3: Total Engagement Rule-Following (all moved sliders)
        results$ratio_50_total_engagement[i] <- sum(moved_final == 50) / length(moved_final)
        results$count_total_engagement[i] <- length(moved_final)
      }
    }
  }
  
  return(results)
}