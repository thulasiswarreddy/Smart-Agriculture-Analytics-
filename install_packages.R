packages <- c(
  "shiny",
  "bslib",
  "DT",
  "dplyr",
  "tidyr",
  "ggplot2",
  "plotly",
  "readxl",
  "readr",
  "scales"
)

missing <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}

cat("All required packages are ready!\n")