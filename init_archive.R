# This ensures all required packages are installed on shinyapps.io:

my_packages = c("shiny", "survival", "survminer", "tidyverse", 
                "lubridate", "gridExtra", "grid")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}