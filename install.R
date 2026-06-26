# Run this script once to install all required packages:
#   Rscript install.R
#   — or source("install.R") from within RStudio

required_packages <- c(
  "shiny",
  "dplyr",
  "tableHTML",
  "magrittr",
  "stringr",
  "tidyverse",
  "tools",
  "devtools",
  "DT"
)

missing <- required_packages[!required_packages %in% installed.packages()[, "Package"]]

if (length(missing) > 0) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  message("All required packages are already installed.")
}
