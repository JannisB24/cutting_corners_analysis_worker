mf20_total_score_barchart <- function(data, variable_name = "moral_foundations_20.1.player.moral_foundations_score", custom_title = "Moral Foundations Scores Distribution") {
  # Get the data from the variable name
  scores <- data[[variable_name]]
  
  # Create a default title if none provided
  if(is.null(custom_title)) {
    custom_title <- paste("Distribution of", variable_name)
  }
  
  # Create histogram with appropriate bins
  hist_result <- hist(scores, 
                      breaks = seq(-0.5, 60.5, by = 1),  # Bins centered on integers 0-60
                      plot = FALSE)
  
  # Extract counts and bin centers
  counts <- hist_result$counts
  bin_centers <- hist_result$mids
  
  # Create the bar chart
  barplot(counts, 
          main = custom_title,
          names.arg = 0:60,  # Show all scores from 0-60
          xlab = "Score (0-60)", 
          ylab = "Frequency",
          col = "steelblue",
          xaxt = "n")  # Suppress x-axis labels initially
  
  # Add custom x-axis labels (showing every 5th value to avoid crowding)
  axis(1, at = seq(0, 60, by = 5) * 1.2, labels = seq(0, 60, by = 5))
  
  # Add summary statistics
  mean_score <- mean(scores, na.rm = TRUE)
  median_score <- median(scores, na.rm = TRUE)
  
  legend("topright", 
         legend = c(paste("Mean:", round(mean_score, 1)),
                    paste("Median:", round(median_score, 1))),
         bty = "n")  # No box around legend
}