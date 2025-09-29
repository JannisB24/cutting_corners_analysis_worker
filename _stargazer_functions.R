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

personal_belief_regression <- function(model1, model2, title = NULL, covariate.labels = NULL) {
  folder <- "../Masterthesis-Overleaf/4_Results"
  filename <- "personal_belief_regression.tex"
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

group_compliance_regression <- function(model1, model2, title = NULL, covariate.labels = NULL) {
  folder <- "../Masterthesis-Overleaf/4_Results"
  filename <- "group_compliance_regression.tex"
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

group_noncompliance_regression <- function(model1, model2, title = NULL, covariate.labels = NULL) {
  folder <- "../Masterthesis-Overleaf/4_Results"
  filename <- "group_noncompliance_regression.tex"
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

empirical_belief_regression <- function(model1, model2, title = NULL, covariate.labels = NULL) {
  folder <- "../Masterthesis-Overleaf/4_Results"
  filename <- "empirical_belief_regression.tex"
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

moved_ratio_50_work_exp <- function(model1, model2, title = NULL, covariate.labels = NULL) {
  folder <- "../Masterthesis-Overleaf/4_Results"
  filename <- "moved_ratio_50_work_exp.tex"
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

moved_ratio_50_all_work <- function(model1, model2, title = NULL, covariate.labels = NULL) {
  folder <- "../Masterthesis-Overleaf/4_Results"
  filename <- "moved_ratio_50_all_work.tex"
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

treatment_demographics <- function(model1, model2, title = NULL, covariate.labels = NULL) {
  folder <- "../Masterthesis-Overleaf/4_Results"
  filename <- "treatment_demographics.tex"
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

all_beliefs_without_interaction <- function(model1, model2, title = NULL, covariate.labels = NULL) {
  folder <- "../Masterthesis-Overleaf/4_Results"
  filename <- "all_beliefs_without_interaction.tex"
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

cognitive_conflict_regression <- function(model1, model2, title = NULL, covariate.labels = NULL) {
  folder <- "../Masterthesis-Overleaf/4_Results"
  filename <- "cognitive_conflict_regression.tex"
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

mf_ros_regression <- function(model1, model2, title = NULL, covariate.labels = NULL) {
  folder <- "../Masterthesis-Overleaf/4_Results"
  filename <- "mf_ros_regression.tex"
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

honesty_regression <- function(model1, title = NULL, covariate.labels = NULL) {
  folder <- "../Masterthesis-Overleaf/4_Results"
  filename <- "honesty_regression.tex"
  filepath <- file.path(folder, filename)
  
  stargazer(model1,
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

all_variables_on_beliefs <- function(model1, model2, model3, model4, title = NULL, covariate.labels = NULL) {
  folder <- "../Masterthesis-Overleaf/4_Results"
  filename <- "all_variables_on_beliefs.tex"
  filepath <- file.path(folder, filename)
  
  stargazer(model1, model2, model3, model4,
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