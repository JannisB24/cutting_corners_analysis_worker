slider_ratios_moved <- function(data) {
  # Initialize new columns - comprehensive movement analysis
  data$moved_ratio_50_all <- NA           
  data$moved_ratio_4060_all <- NA         
  data$moved_ratio_outside_all <- NA      
  data$moved_count_all <- NA              
  
  data$moved_ratio_50_non50_start <- NA   
  data$moved_ratio_4060_non50_start <- NA 
  data$moved_ratio_outside_non50_start <- NA 
  data$moved_count_non50_start <- NA      
  
  data$moved_ratio_50_outside <- NA       
  data$moved_ratio_4060_outside <- NA     
  data$moved_ratio_outside_outside <- NA  
  data$moved_count_outside <- NA          
  
  data$moved_ratio_50_inside <- NA        
  data$moved_ratio_4060_inside <- NA      
  data$moved_ratio_outside_inside <- NA   
  data$moved_count_inside <- NA           
  
  for(i in 1:nrow(data)) {
    initial_vals <- as.numeric(data[i, paste0("els_slider.1.player.slider_initial_", 0:47)])
    final_vals <- as.numeric(data[i, paste0("els_slider.1.player.slider_final_", 0:47)])
    valid_indices <- !is.na(initial_vals) & !is.na(final_vals)
    initial_vals <- initial_vals[valid_indices]
    final_vals <- final_vals[valid_indices]
    
    if(length(initial_vals) > 0) {
      moved_indices <- initial_vals != final_vals
      moved_initial <- initial_vals[moved_indices]
      moved_final <- final_vals[moved_indices]
      
      if(sum(moved_indices) > 0) {
        # All moved sliders
        data$moved_ratio_50_all[i] <- sum(moved_final == 50) / length(moved_final)
        data$moved_ratio_4060_all[i] <- sum(moved_final >= 40 & moved_final <= 60 & moved_final != 50) / length(moved_final)
        data$moved_ratio_outside_all[i] <- sum(moved_final < 40 | moved_final > 60) / length(moved_final)
        data$moved_count_all[i] <- length(moved_final)
        
        # Among moved sliders that didn't start at 50
        non50_start_indices <- moved_initial != 50
        if(sum(non50_start_indices) > 0) {
          non50_final <- moved_final[non50_start_indices]
          data$moved_ratio_50_non50_start[i] <- sum(non50_final == 50) / sum(non50_start_indices)
          data$moved_ratio_4060_non50_start[i] <- sum(non50_final >= 40 & non50_final <= 60 & non50_final != 50) / sum(non50_start_indices)
          data$moved_ratio_outside_non50_start[i] <- sum(non50_final < 40 | non50_final > 60) / sum(non50_start_indices)
          data$moved_count_non50_start[i] <- sum(non50_start_indices)
        } else {
          # No non-50 starters that moved - set ratios to 0
          data$moved_ratio_50_non50_start[i] <- 0
          data$moved_ratio_4060_non50_start[i] <- 0
          data$moved_ratio_outside_non50_start[i] <- 0
          data$moved_count_non50_start[i] <- 0
        }
        
        # Among moved sliders that started outside 40-60
        outside_indices <- moved_initial < 40 | moved_initial > 60
        if(sum(outside_indices) > 0) {
          outside_final <- moved_final[outside_indices]
          data$moved_ratio_50_outside[i] <- sum(outside_final == 50) / sum(outside_indices)
          data$moved_ratio_4060_outside[i] <- sum(outside_final >= 40 & outside_final <= 60 & outside_final != 50) / sum(outside_indices)
          data$moved_ratio_outside_outside[i] <- sum(outside_final < 40 | outside_final > 60) / sum(outside_indices)
          data$moved_count_outside[i] <- sum(outside_indices)
        } else {
          # No outside starters that moved - set ratios to 0
          data$moved_ratio_50_outside[i] <- 0
          data$moved_ratio_4060_outside[i] <- 0
          data$moved_ratio_outside_outside[i] <- 0
          data$moved_count_outside[i] <- 0
        }
        
        # Among moved sliders that started inside 40-60 (but not 50)
        inside_indices <- moved_initial >= 40 & moved_initial <= 60 & moved_initial != 50
        if(sum(inside_indices) > 0) {
          inside_final <- moved_final[inside_indices]
          data$moved_ratio_50_inside[i] <- sum(inside_final == 50) / sum(inside_indices)
          data$moved_ratio_4060_inside[i] <- sum(inside_final >= 40 & inside_final <= 60 & inside_final != 50) / sum(inside_indices)
          data$moved_ratio_outside_inside[i] <- sum(inside_final < 40 | inside_final > 60) / sum(inside_indices)
          data$moved_count_inside[i] <- sum(inside_indices)
        } else {
          # No inside starters that moved - set ratios to 0
          data$moved_ratio_50_inside[i] <- 0
          data$moved_ratio_4060_inside[i] <- 0
          data$moved_ratio_outside_inside[i] <- 0
          data$moved_count_inside[i] <- 0
        }
      } else {
        # No sliders moved at all - set all ratios and counts to 0
        data$moved_ratio_50_all[i] <- 0
        data$moved_ratio_4060_all[i] <- 0
        data$moved_ratio_outside_all[i] <- 0
        data$moved_count_all[i] <- 0
        
        data$moved_ratio_50_non50_start[i] <- 0
        data$moved_ratio_4060_non50_start[i] <- 0
        data$moved_ratio_outside_non50_start[i] <- 0
        data$moved_count_non50_start[i] <- 0
        
        data$moved_ratio_50_outside[i] <- 0
        data$moved_ratio_4060_outside[i] <- 0
        data$moved_ratio_outside_outside[i] <- 0
        data$moved_count_outside[i] <- 0
        
        data$moved_ratio_50_inside[i] <- 0
        data$moved_ratio_4060_inside[i] <- 0
        data$moved_ratio_outside_inside[i] <- 0
        data$moved_count_inside[i] <- 0
      }
    }
  }
  
  return(data)
}