slider_hist <- function(sliders_at_50, sliders_in_range, treatment_name) {
  par(mfrow = c(1, 2))
  
  hist(sliders_at_50, 
       breaks = seq(-0.5, 48.5, by = 1),
       main = paste("Sliders at 50 -", treatment_name, "Treatment"),
       xlab = "Number of Sliders at 50",
       ylab = "Frequency",
       xlim = c(1, 48))
  
  hist(sliders_in_range, 
       breaks = seq(-0.5, 48.5, by = 1),
       main = paste("Sliders in 40-60 Range -", treatment_name, "Treatment"),
       xlab = "Number of Sliders in 40-60",
       ylab = "Frequency",
       xlim = c(1, 48))
  
  par(mfrow = c(1, 1))
}

# Edited slider_by_treatment function (no histograms, no cat statements)
slider_by_treatment <- function(data_subset, treatment_name) {
  
  # sliders exactly dragged to 50
  sliders_at_50 <- sapply(0:47, function(i) {
    col_name <- paste0("els_slider.1.player.slider_final_", i)
    if(col_name %in% names(data_subset)) {
      sum(data_subset[[col_name]] == 50, na.rm = TRUE)
    } else {
      0
    }
  })
  
  # sliders dragged to 40-60
  sliders_in_range <- sapply(0:47, function(i) {
    col_name <- paste0("els_slider.1.player.slider_final_", i)
    if(col_name %in% names(data_subset)) {
      sum(data_subset[[col_name]] >= 40 & data_subset[[col_name]] <= 60, na.rm = TRUE)
    } else {
      0
    }
  })
  
  return(list(sliders_at_50 = sliders_at_50, sliders_in_range = sliders_in_range))
}