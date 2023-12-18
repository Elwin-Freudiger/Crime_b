#############################################
## The following loads the needed packages ##
#############################################

# load the required packages
packages <- c(
  "here", # for the project's organization
  "tidyverse", "readxl", # for wrangling
  "scales", "corrplot", "plotly", 
  "gganimate", "gifski", "sf", "ggspatial", "leaflet", "leaflet.extras", "treemap", "scales", "RColorBrewer",# for plotting
  "cluster", #for clustering
  "sf",#geojson reading
  "caret", "lmtest", "car", "ggcorrplot", "stargazer", "gtsummary", #for regressions
  "DT"# interactive tables
)
purrr::walk(packages, library, character.only = TRUE)

######################################################
## The following sets a few option for nice reports ##
######################################################


# general options
options(
  digits = 4,
  str = strOptions(strict.width = "cut"),
  width = 69,
  tibble.width = 69,
  cli.unicode = FALSE
)

# ggplot options
theme_set(theme_light())

# knitr options
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.align = "center",
  fig.retina = 0.8,
  dpi = 300,
  out.width = "70%", 
  fig.show = "hold"
)