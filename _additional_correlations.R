library(psych)
library(dplyr)

# 1) Select your variables (exactly those you listed)
vars <- c(
  "moved_ratio_50_all",
  "els_slider.1.player.manager_els",
  "sliders_at_50_count",
  "els_slider.1.player.els_score",
  "moral_foundations_20.1.player.moral_foundations_score",
  "rule_orientation.1.player.rule_orientation_score",
  "honesty.1.player.mother_birth_year"
)

df <- dplyr::select(data, any_of(vars))

if (ncol(df) == 0) {
  stop("None of the requested variables were found in `data`.")
}

# If any variables are not numeric, corr.test will error.
# Convert manually if appropriate (uncomment next line), or fix upstream.
# df <- dplyr::mutate(df, dplyr::across(everything(), ~ suppressWarnings(as.numeric(.))))

# 2) Compute Pearson correlations (pairwise deletion)
# Set to "BH" if you want FDR-adjusted significance stars
adjust_method <- "none"
ct <- psych::corr.test(df, use = "pairwise", method = "pearson", adjust = adjust_method)

r <- as.matrix(ct$r)
p <- as.matrix(ct$p)

# 3) Helpers
fmt_num <- function(x, digits = 2) sprintf(paste0("%.", digits, "f"), x)
starify <- function(p) ifelse(p < 0.001, "***",
                              ifelse(p < 0.01,  "**",
                                     ifelse(p < 0.05,  "*", "")))

latex_escape <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("&", "\\\\&", x, fixed = TRUE)
  x <- gsub("%", "\\\\%", x, fixed = TRUE)
  x <- gsub("\\$", "\\\\$", x)
  x <- gsub("#", "\\\\#", x, fixed = TRUE)
  x <- gsub("_", "\\\\_", x, fixed = TRUE)
  x <- gsub("\\{", "\\\\{", x)
  x <- gsub("\\}", "\\\\}", x)
  x <- gsub("\\^", "\\\\^{}", x)
  x <- gsub("~", "\\\\~{}", x)
  x
}

# 4) Build LaTeX (lower triangle, 1.00 on diagonal, stars for significance)
corr_to_latex <- function(r, p,
                          title = "Pearson Correlations",
                          label = "tab:correlations",
                          show_upper = FALSE,
                          show_diag = TRUE,
                          digits = 2,
                          add_notes = TRUE,
                          note_adjust = "none") {
  
  r <- as.matrix(r); p <- as.matrix(p)
  stopifnot(identical(dim(r), dim(p)))
  k <- ncol(r)
  
  # Ensure names exist and align
  if (is.null(colnames(r))) colnames(r) <- paste0("V", seq_len(k))
  if (is.null(rownames(r))) rownames(r) <- colnames(r)
  dimnames(p) <- dimnames(r)
  
  var_labels <- latex_escape(colnames(r))
  
  # Precompute formatted pieces
  vals  <- matrix(fmt_num(r, digits), nrow = k, ncol = k, dimnames = dimnames(r))
  stars <- matrix(starify(p),        nrow = k, ncol = k, dimnames = dimnames(r))
  
  # Build cells explicitly to preserve dimensions
  cells <- matrix("", nrow = k, ncol = k, dimnames = dimnames(r))
  for (i in seq_len(k)) {
    for (j in seq_len(k)) {
      if (i == j) {
        cells[i, j] <- if (show_diag) fmt_num(1, digits) else ""
      } else if (!show_upper && j > i) {
        cells[i, j] <- ""
      } else {
        if (is.na(r[i, j])) {
          cells[i, j] <- ""
        } else {
          # If p is NA (e.g., due to zero variance), omit stars
          s <- stars[i, j]
          if (is.na(s)) s <- ""
          cells[i, j] <- paste0(vals[i, j], s)
        }
      }
    }
  }
  
  # Column spec: 1 left column for row names + k centered columns
  colspec <- paste0("l", paste(rep("c", k), collapse = ""))
  
  # Header row
  header <- paste(c("", var_labels), collapse = " & ")
  
  # Data rows
  rows <- vapply(seq_len(k), function(i) {
    paste(c(var_labels[i], cells[i, ]), collapse = " & ")
  }, character(1))
  rows <- paste0(rows, " \\\\")
  
  notes <- if (add_notes) {
    paste0(
      "\\begin{flushleft}\\footnotesize\n",
      "Notes: Entries are Pearson correlation coefficients. ",
      "Significance: $\\ast$ p < 0.05, $\\ast\\ast$ p < 0.01, $\\ast\\ast\\ast$ p < 0.001. ",
      if (identical(note_adjust, "BH")) "P-values adjusted using the Benjamini--Hochberg method. ",
      "Pairwise deletion used for missing data.\n",
      "\\end{flushleft}\n"
    )
  } else ""
  
  paste0(
    "\\begin{table}[!htbp]\\centering\n",
    "\\caption{", latex_escape(title), "}\\label{", latex_escape(label), "}\n",
    "\\begin{tabular}{", colspec, "}\n",
    "\\toprule\n",
    header, " \\\\\n",
    "\\midrule\n",
    paste(rows, collapse = "\n"), "\n",
    "\\bottomrule\n",
    "\\end{tabular}\n",
    notes,
    "\\end{table}\n"
  )
}

# 5) Generate LaTeX and save to Overleaf repo
latex_code <- corr_to_latex(
  r, p,
  title = "Correlations among key variables",
  label = "tab:correlation-table",
  show_upper = FALSE,  # lower triangle only
  show_diag = TRUE,
  digits = 2,
  note_adjust = adjust_method
)

out_file <- "../Masterthesis-Overleaf/4_Results/correlation_table.tex"
dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
writeLines(latex_code, out_file)

cat("Wrote LaTeX correlation table to: ", normalizePath(out_file), "\n")