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

