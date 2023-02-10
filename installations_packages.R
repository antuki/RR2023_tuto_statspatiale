
# Packages du CRAN
packages <- c("dplyr", "tidygeocoder", "mapview", "sf", "osmdata",
              "RColorBrewer", "ggplot2", "readr",
              "ggspatial", "knitr", "sfnetworks", "tidygraph", "remotes", "rmdformats")

install.packages(setdiff(packages, rownames(installed.packages()))) 

# Packages hors CRAN
remotes::install_github("joelgombin/banR")
devtools::install_github("koncina/unilur")
