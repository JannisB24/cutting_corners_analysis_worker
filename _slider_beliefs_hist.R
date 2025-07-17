slider_beliefs_hist <- function(data, var_name, treatment_label) {
  # Extract the variable
  values <- data[[var_name]]
  
  # Remove NAs
  values_clean <- values[!is.na(values)]
  
  # Create histogram
  hist(values_clean, 
       breaks = c(-3.5, -1.5, 0.5, 1.5, 3.5),  # Bins centered on -3, -1, 1, 3
       labels = c("-3", "-1", "1", "3"),
       main = paste("Normative Beliefs:", treatment_label, "\n", gsub(".*\\.", "", var_name)),
       xlab = "Response Scale",
       ylab = "Frequency",
       col = "lightblue",
       border = "black",
       xlim = c(-4, 4),
       xaxt = "n")  # Don't show x-axis ticks yet
  
  # Add custom x-axis
  axis(1, at = c(-3, -1, 1, 3), labels = c("-3\n(Disapprove)", "-1\n(Somewhat\nDisapprove)", 
                                           "1\n(Somewhat\nApprove)", "3\n(Approve)"))
  
  # Add sample size and mean
  n_obs <- length(values_clean)
  mean_val <- round(mean(values_clean), 2)
  mtext(paste("N =", n_obs, ", Mean =", mean_val), side = 3, line = 0.5, cex = 0.8)
  
  return(values_clean)
}