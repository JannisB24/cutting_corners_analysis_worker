# Function to create LaTeX quantile regression table
quantile_regression_tables <- function(rank_results, taus, 
                                    file_path = "../Masterthesis-Overleaf/4_Results/quantile_regression.tex",
                                    caption = "Quantile Regression Results with Rank-Based Confidence Intervals",
                                    label = "tab:qreg_results") {
  
  # Get coefficient names
  coef_names <- rownames(rank_results[[1]]$summary$coefficients)
  
  # Clean variable names for publication
  clean_names <- c(
    "(Intercept)" = "Constant",
    "els_slider.1.player.treatmentlow" = "Treatment (Low)",
    "els_slider.1.player.slider_50_difficulty" = "Slider Difficulty", 
    "ability_sliders_50" = "Ability"
  )
  
  # Start LaTeX table
  col_spec <- paste0("l", paste0(rep("c", length(taus)), collapse = ""))
  
  latex_code <- paste0(
    "\\begin{table}[htbp]\n",
    "\\centering\n",
    "\\caption{", caption, "}\n",
    "\\label{", label, "}\n",
    "\\begin{tabular}{", col_spec, "}\n",
    "\\toprule\n"
  )
  
  # Add header
  tau_headers <- paste0("$\\tau = ", taus, "$", collapse = " & ")
  latex_code <- paste0(latex_code, "Variable & ", tau_headers, " \\\\\n\\midrule\n")
  
  # Add coefficients and confidence intervals
  for(i in 1:length(coef_names)) {
    # Clean variable name
    var_name <- ifelse(coef_names[i] %in% names(clean_names), 
                       clean_names[coef_names[i]], 
                       coef_names[i])
    
    # Coefficient row
    coef_row <- var_name
    ci_row <- ""
    
    for(j in 1:length(taus)) {
      coefs <- rank_results[[j]]$summary$coefficients
      coef_val <- round(coefs[i, "coefficients"], 4)
      lower_ci <- round(coefs[i, "lower bd"], 4) 
      upper_ci <- round(coefs[i, "upper bd"], 4)
      
      # Check significance
      is_significant <- (lower_ci > 0 & upper_ci > 0) | (lower_ci < 0 & upper_ci < 0)
      
      # Format coefficient with significance
      if(is_significant) {
        coef_cell <- paste0(coef_val, "$^{**}$")
      } else {
        coef_cell <- as.character(coef_val)
      }
      
      # Format confidence interval
      ci_cell <- paste0("[", lower_ci, ", ", upper_ci, "]")
      
      coef_row <- paste0(coef_row, " & ", coef_cell)
      ci_row <- paste0(ci_row, " & ", ci_cell)
    }
    
    # Add rows to table
    latex_code <- paste0(latex_code, coef_row, " \\\\\n")
    latex_code <- paste0(latex_code, ci_row, " \\\\\n")
    
    # Add some space between variables (except after last one)
    if(i < length(coef_names)) {
      latex_code <- paste0(latex_code, "\\addlinespace[0.1cm]\n")
    }
  }
  
  # Close table
  latex_code <- paste0(
    latex_code,
    "\\bottomrule\n",
    "\\end{tabular}\n",
    "\\begin{tablenotes}\n",
    "\\footnotesize\n",
    "\\item \\textbf{Notes:} Rank-based confidence intervals reported in brackets below coefficients. ",
    "$^{**}$ indicates that the 95\\% confidence interval does not include zero (statistically significant).\n",
    "\\end{tablenotes}\n",
    "\\end{table}\n"
  )
  
  # Create directory if it doesn't exist
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  
  # Write to file
  writeLines(latex_code, file_path)
  
  # Success message
  cat("✓ LaTeX quantile regression table saved to:", file_path, "\n")
  cat("✓ Ready to \\input{} in your thesis!\n")
  
  # Return the LaTeX code (optional, for viewing)
  invisible(latex_code)
}