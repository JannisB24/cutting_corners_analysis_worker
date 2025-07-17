els_single_item_table <- function(data, items) {
  # items should be a named vector or list: names are variable names, values are headlines
  summary_df <- data.frame(
    Headline = character(),
    Mean = numeric(),
    SD = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (item in names(items)) {
    mean_val <- mean(data[[item]], na.rm = TRUE)
    sd_val <- sd(data[[item]], na.rm = TRUE)
    summary_df <- rbind(
      summary_df,
      data.frame(
        Headline = items[[item]],
        Mean = round(mean_val, 2),
        SD = round(sd_val, 2),
        stringsAsFactors = FALSE
      )
    )
  }
  
  return(summary_df)
}