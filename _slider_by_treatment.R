slider_by_treatment <- function(data_subset) {
  sliders_at_50_per_obs <- numeric(nrow(data_subset))
  sliders_in_range_per_obs <- numeric(nrow(data_subset))
  
  for(row in 1:nrow(data_subset)) {
    count_at_50 <- 0
    count_in_range <- 0
    
    for(i in 0:47) {
      col_name <- paste0("els_slider.1.player.slider_final_", i)
      if(col_name %in% names(data_subset)) {
        slider_value <- data_subset[[col_name]][row]
        
        if(!is.na(slider_value)) {
          if(slider_value == 50) {
            count_at_50 <- count_at_50 + 1
          }
        
          if(slider_value >= 40 & slider_value <= 60) {
            count_in_range <- count_in_range + 1
          }
        }
      }
    }
    sliders_at_50_per_obs[row] <- count_at_50
    sliders_in_range_per_obs[row] <- count_in_range
  }
  
  data_subset$sliders_at_50_count <- sliders_at_50_per_obs
  data_subset$sliders_in_range_count <- sliders_in_range_per_obs
  
  return(data_subset)
}