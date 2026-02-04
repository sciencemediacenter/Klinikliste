# install_requirements.R
# ======================
# Installs all R packages required to run the Klinikliste scripts.
# Run this script once before using the repository.

# Required packages
packages <- c(
  # Core data manipulation

  "tidyverse",

  # XML/HTML parsing
  "xml2",

  # Utilities
  "glue",
  "tictoc",

  # Parallel processing
  "furrr",
  "future"
)

# Install missing packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing:", pkg))
    install.packages(pkg)
  } else {
    message(paste("Already installed:", pkg))
  }
}

message("Checking and installing required packages...\n")

for (pkg in packages) {
  install_if_missing(pkg)
}

message("\nDone! All required packages are installed.")
message("You can now run the scripts in the scripts/ folder.")
