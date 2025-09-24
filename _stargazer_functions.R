work_experience_summary <- function(table_data, filename, title = NULL, label = NULL, digits = 4,
                                     row_labels = NULL, col_labels = NULL,
                                     notes = NULL) {
  
  folder <- "../Masterthesis-Overleaf/4_Results"
  filepath <- file.path(folder, filename)
  
  stargazer(table_data,
            type = "latex",
            out = filepath,
            title = title,
            label = label,
            summary = FALSE,
            rownames = TRUE,
            digits = digits,
            digit.separator = "",
            table.placement = "H",
            header = FALSE,
            float = TRUE,
            notes = notes,
            notes.align = "l")
  
  cat("LaTeX table saved to:", filepath, "\n")
  return(filepath)
}

single_belief_regression <- function(model, title = NULL, covariate.labels = NULL) {
  stargazer(
    model,
    type = "latex",
    title = title,
    covariate.labels = covariate.labels,
    single.row = TRUE
  )
}

treatment_basic_regression <- function(model1, model2, title = NULL, covariate.labels = NULL) {
  folder <- "../Masterthesis-Overleaf/4_Results"
  filename <- "treatment_basic_regression.tex"
  filepath <- file.path(folder, filename)
  
  stargazer(model1, model2,
            type = "latex",
            out = filepath,
            title = title,
            covariate.labels = covariate.labels,
            summary = FALSE,
            rownames = TRUE,
            digits = 3,
            digit.separator = "",
            table.placement = "h",
            header = FALSE,
            float = TRUE,
            notes.align = "l")
  
  cat("LaTeX table saved to:", filepath, "\n")
  return(filepath)
}

treatment_distance_regression <- function(model1, model2, title = NULL, covariate.labels = NULL) {
  folder <- "../Masterthesis-Overleaf/4_Results"
  filename <- "treatment_distance_regression.tex"
  filepath <- file.path(folder, filename)
  
  stargazer(model1, model2,
            type = "latex",
            out = filepath,
            title = title,
            covariate.labels = covariate.labels,
            summary = FALSE,
            rownames = TRUE,
            digits = 3,
            digit.separator = "",
            table.placement = "h",
            header = FALSE,
            float = TRUE,
            notes.align = "l")
  
  cat("LaTeX table saved to:", filepath, "\n")
  return(filepath)
}