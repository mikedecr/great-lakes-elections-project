# --------------------------------------
#  Manages building for...
#  EAVS, Wait times, VEP turnout, and normalized measures
# --------------------------------------

# --- setup packages -----------------------
# packages to load include here, magrittr, tidyverse, ggplot2
# install but don't load some other packages



# currently installed packages
pkgs <- as.data.frame(installed.packages()) 

# packages we need, and a logical for loading
pkg_array <- 
  rbind(cbind(pkg = "here", load = TRUE),
        cbind(pgg = "magrittr", load = TRUE),
        cbind(pgg = "tidyverse", load = TRUE),
        cbind(pgg = "ggplot2", load = TRUE),
        cbind(pgg = "readxl", load = FALSE),
        cbind(pgg = "readr", load = FALSE),
        cbind(pgg = "R.utils", load = FALSE),
        cbind(pgg = "haven", load = FALSE),
        cbind(pgg = "labelled", load = FALSE),
        cbind(pgg = "gtools", load = FALSE),
        cbind(pgg = "beepr", load = FALSE),
        cbind(pgg = "readr", load = FALSE),
        cbind(pgg = "gghighlight", load = FALSE)
        )

req_pkgs <- as.data.frame(pkg_array, stringsAsFactors = FALSE)
req_pkgs$message <- NA

# for each package: 
# checks if package is installed, if not: install and add a message
# if we need to load it, load it
for (p in seq_along(req_pkgs$pkg)) {

  if (req_pkgs$pkg[p] %in% pkgs$Package == FALSE) {
    install.packages(req_pkgs$pkg[p], repos = "https://cloud.r-project.org/")
    req_pkgs$message[p] <- paste0("'", req_pkgs$pkg[p], "' was installed")
  }

  if (req_pkgs$load[p] == TRUE){
    library(req_pkgs$pkg[p], character.only = TRUE)
  }

}

# which packages were installed?
cbind(na.omit(req_pkgs$message))



# --- run scripts -----------------------

source(here("R/EAVS/eavs-2016-build.R"), echo = TRUE)
beepr::beep(4)
source(here("R/wait-times/waits.R"), echo = TRUE)
beepr   ::beep(4)
source(here("R/VEP/county-turnout.R"), echo = TRUE)
beepr::beep(4)

# This should be in normalized-measures.R
# source(here("R/normalized/eavs-2016-vote-methods.R"), echo = TRUE)
# beepr::beep(4)

source(here("R/normalized/normalized-measures.R"), echo = TRUE)
beepr::beep(4)
