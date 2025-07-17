# Simple function to create boxplots for time variables
time_boxplots <- function(data, time_var, title = NULL) {
  if(is.null(title)) {
    title <- deparse(substitute(time_var))
  }
  
  boxplot(data[[time_var]], 
          main = title,
          ylab = "Time (seconds)",
          col = "lightblue",
          notch = FALSE,
          outline = TRUE,  # outline=TRUE shows outliers
          ylim = c(0, max(data[[time_var]], na.rm = TRUE)))  # y-axis starts at 0
  
  # Add mean as a point with label
  mean_val <- mean(data[[time_var]], na.rm = TRUE)
  points(1, mean_val, col = "red", pch = 19, cex = 1.2)
  text(1.3, mean_val, paste("Mean:", round(mean_val, 2)), col = "red")
}