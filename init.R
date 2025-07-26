# Install required packages
my_packages = c("shiny", "survival", "survminer", "tidyverse", "lubridate", 
                "gridExtra", "grid", "data.table", "grDevices")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
}

invisible(sapply(my_packages, install_if_missing))