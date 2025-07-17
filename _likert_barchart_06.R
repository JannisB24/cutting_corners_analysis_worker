likert_barchart_06 <- function(data, variable_name, custom_title = NULL) {
  # Get the data from the variable name
  responses <- data[[variable_name]]
  
  # Create a default title if none provided
  if(is.null(custom_title)) {
    custom_title <- paste("Distribution of Responses for", variable_name)
  }
  
  # Create a vector for all possible ratings (0-6)
  all_ratings <- 0:6
  counts <- numeric(length(all_ratings))
  names(counts) <- as.character(all_ratings)
  
  # Count occurrences of each rating
  if(length(responses) > 0) {
    for(r in responses) {
      if(!is.na(r) && r >= 0 && r <= 6) {
        counts[as.character(r)] <- counts[as.character(r)] + 1
      }
    }
  }
  
  # Create the bar chart
  bar_positions <- barplot(counts, 
                           main = custom_title,
                           names.arg = 0:6,  # Explicitly use 1:6 as names
                           xlab = "Rating (0-6)", 
                           ylab = "Frequency",
                           col = "steelblue",
                           ylim = c(0, max(c(counts, 1)) * 1.2))  # Ensure ylim is always positive
  
  # Add count labels on top of each bar
  for (i in 1:length(counts)) {
    text(x = bar_positions[i], 
         y = counts[i] + max(c(counts, 1))/50,  # Position slightly above each bar
         labels = counts[i],
         cex = 0.8)
  }
}