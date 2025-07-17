slider_hist <- function(data, count_column, title, x_label) {
  ggplot(data, aes(x = .data[[count_column]])) +
    geom_histogram(aes(y = after_stat(density)), 
                   binwidth = 1, 
                   fill = "#4A90E2", 
                   color = "white", 
                   alpha = 0.8) + 
    scale_x_continuous(breaks = seq(0, 48, by = 5), 
                       limits = c(0, 48), 
                       expand = expansion(mult = 0.02)) +
    labs(title = title, x = x_label, y = "Count") +
    geom_vline(xintercept = median(data[[count_column]], na.rm = TRUE),
               color = "black", 
               linetype = "dashed", 
               linewidth = 0.8) +
    theme_classic()
}