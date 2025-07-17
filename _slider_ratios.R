slider_ratios <- function(data) {
  data$ratio_50_all <- NA
  data$ratio_4060_all <- NA
  data$ratio_outside_all <- NA
  
  # Sliders that started outside 40-60
  data$ratio_50_outside <- NA
  data$ratio_4060_outside <- NA
  data$ratio_outside_outside <- NA
  
  # Sliders that started inside 40-60
  data$ratio_50_inside <- NA
  data$ratio_4060_inside <- NA
  data$ratio_outside_inside <- NA
  
  for(i in 1:nrow(data)) {
    initial_vals <- as.numeric(data[i, paste0("els_slider.1.player.slider_initial_", 0:47)])
    final_vals <- as.numeric(data[i, paste0("els_slider.1.player.slider_final_", 0:47)])
    
    valid_indices <- !is.na(initial_vals) & !is.na(final_vals)
    if(sum(valid_indices) > 0) {
      initial_clean <- initial_vals[valid_indices]
      final_clean <- final_vals[valid_indices]
      
      # All sliders - three mutually exclusive categories
      data$ratio_50_all[i] <- sum(final_clean == 50) / length(final_clean)
      data$ratio_4060_all[i] <- sum(final_clean >= 40 & final_clean <= 60 & final_clean != 50) / length(final_clean)
      data$ratio_outside_all[i] <- sum(final_clean < 40 | final_clean > 60) / length(final_clean)
      
      outside_indices <- initial_clean < 40 | initial_clean > 60
      inside_indices <- initial_clean >= 40 & initial_clean <= 60
      
      # Sliders that started outside 40-60
      if(sum(outside_indices) > 0) {
        outside_final <- final_clean[outside_indices]
        data$ratio_50_outside[i] <- sum(outside_final == 50) / sum(outside_indices)
        data$ratio_4060_outside[i] <- sum(outside_final >= 40 & outside_final <= 60 & outside_final != 50) / sum(outside_indices)
        data$ratio_outside_outside[i] <- sum(outside_final < 40 | outside_final > 60) / sum(outside_indices)
      }
      
      # Sliders that started inside 40-60
      if(sum(inside_indices) > 0) {
        inside_final <- final_clean[inside_indices]
        data$ratio_50_inside[i] <- sum(inside_final == 50) / sum(inside_indices)
        data$ratio_4060_inside[i] <- sum(inside_final >= 40 & inside_final <= 60 & inside_final != 50) / sum(inside_indices)
        data$ratio_outside_inside[i] <- sum(inside_final < 40 | inside_final > 60) / sum(inside_indices)
      }
    }
  }
  
  return(data)
}