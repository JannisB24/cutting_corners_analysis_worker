slider_ratios <- function(data) {
  results <- data.frame(
    participant_id = 1:nrow(data),
    
    ratio_50_all = NA,
    ratio_4060_all = NA,
    ratio_50_outside = NA,
    ratio_4060_outside = NA,
    ratio_50_inside = NA,
    ratio_4060_inside = NA,
    ratio_50_arbitrary_compliance = NA,
    count_arbitrary_compliance = NA
  )
  
  for(i in 1:nrow(data)) {
    # Get all slider values
    initial_vals <- as.numeric(data[i, paste0("els_slider.1.player.slider_initial_", 0:47)])
    final_vals <- as.numeric(data[i, paste0("els_slider.1.player.slider_final_", 0:47)])
    
    # Remove NAs
    valid_indices <- !is.na(initial_vals) & !is.na(final_vals)
    if(sum(valid_indices) > 0) {
      initial_clean <- initial_vals[valid_indices]
      final_clean <- final_vals[valid_indices]
      
      # Original calculations (all sliders)
      results$ratio_50_all[i] <- sum(final_clean == 50) / length(final_clean)
      results$ratio_4060_all[i] <- sum(final_clean >= 40 & final_clean <= 60) / length(final_clean)
      
      # Split by initial position
      outside_indices <- initial_clean < 40 | initial_clean > 60
      inside_indices <- initial_clean >= 40 & initial_clean <= 60
      
      if(sum(outside_indices) > 0) {
        results$ratio_50_outside[i] <- sum(final_clean[outside_indices] == 50) / sum(outside_indices)
        results$ratio_4060_outside[i] <- sum(final_clean[outside_indices] >= 40 & final_clean[outside_indices] <= 60) / sum(outside_indices)
      }
      
      if(sum(inside_indices) > 0) {
        results$ratio_50_inside[i] <- sum(final_clean[inside_indices] == 50) / sum(inside_indices)
        results$ratio_4060_inside[i] <- sum(final_clean[inside_indices] >= 40 & final_clean[inside_indices] <= 60) / sum(inside_indices)
        results$ratio_50_arbitrary_compliance[i] <- sum(final_clean[inside_indices] == 50) / sum(inside_indices)
        results$count_arbitrary_compliance[i] <- sum(inside_indices)
      }
    }
  }
  
  return(results)
}