slider_beliefs_emp_hist <- function(data, treatment_label) {
  # Extract the variable
  values <- data[["slider_beliefs.1.player.empirical_belief_compliance"]]
  
  # Remove NAs
  values_clean <- values[!is.na(values)]
  
  # Create histogram
  hist(values_clean, 
       breaks = seq(0, 100, by = 10),  # Bins of 10 from 0 to 100
       main = paste("Empirical Belief Compliance:", treatment_label),
       xlab = "Belief about % of others who will comply (0-100)",
       ylab = "Frequency",
       col = "lightgreen",
       border = "black",
       xlim = c(0, 100))
  
  # Add sample size and mean
  n_obs <- length(values_clean)
  mean_val <- round(mean(values_clean), 2)
  median_val <- round(median(values_clean), 2)
  mtext(paste("N =", n_obs, ", Mean =", mean_val, ", Median =", median_val), 
        side = 3, line = 0.5, cex = 0.8)
  
  return(values_clean)
}