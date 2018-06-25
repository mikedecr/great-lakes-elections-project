# --------------------------------------
#  Manages building for...
#  EAVS, Wait times, VEP turnout, and normalized measures
# --------------------------------------

# --- setup packages -----------------------
# packages to load include here, magrittr, tidyverse, ggplot2
# install but don't load some other packages

requires <- c("here", "magrittr", "tidyverse", "ggplot2", "readxl", "readr", 
              "R.utils", "haven", "labelled", "gtools", "beepr", "readr", 
              "gghighlight")

to_install <- requires %in% rownames(installed.packages()) == FALSE

cloud_url <- "https://cloud.r-project.org/"

# install if we need to
if (sum(to_install) > 0) {
  install.packages(requires[to_install], repos = cloud_url)
}

# attach only what we want
library("here")
library("magrittr")
library("tidyverse")
library("ggplot2")





# --- run scripts -----------------------

source(here("R/EAVS/eavs-2016-build.R"), echo = TRUE)
beepr::beep(4)
source(here("R/wait-times/waits.R"), echo = TRUE)
beepr::beep(4)
source(here("R/VEP/county-turnout.R"), echo = TRUE)
beepr::beep(4)

# This should be in normalized-measures.R
# source(here("R/normalized/eavs-2016-vote-methods.R"), echo = TRUE)
# beepr::beep(4)

source(here("R/normalized/normalized-measures.R"), echo = TRUE)
beepr::beep(4)
