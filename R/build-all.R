# --------------------------------------
#  Manages building for...
#  EAVS, Wait times, VEP turnout, and normalized measures
# --------------------------------------

# --- setup packages -----------------------
# packages include here, magrittr, tidyverse, ggplot2
# checks if package is installed, installs from R cloud repos if not
# set real-time warnings for this process
options(warn = 1)

if (library("here", logical.return = TRUE) == FALSE)  {
  install.packages("here", repos = "https://cloud.r-project.org/")
  print(paste0("Package '", "here", "' was installed"))
  library("here")
}
if (library("magrittr", logical.return = TRUE) == FALSE)  {
  install.packages("magrittr", repos = "https://cloud.r-project.org/")
  print(paste0("Package '", "magrittr", "' was installed"))
  library("magrittr")
}
if (library("tidyverse", logical.return = TRUE) == FALSE)  {
  install.packages("tidyverse", repos = "https://cloud.r-project.org/")
  print(paste0("Package '", "tidyverse", "' was installed"))
  library("tidyverse")
}
if (library("ggplot2", logical.return = TRUE) == FALSE)  {
  install.packages("ggplot2", repos = "https://cloud.r-project.org/")
  print(paste0("Package '", "ggplot2", "' was installed"))
  library("ggplot2")
}


# remove.packages('here')
options(warn = 0)


source("R/EAVS/eavs-2016-build.R", echo = TRUE)
beepr::beep(4)
source("R/waits.R", echo = TRUE)
beepr   ::beep(4)
source("R/VEP/county-turnout.R", echo = TRUE)
beepr::beep(4)

# This should be in normalized-measures.R
# source("R/normalized/eavs-2016-vote-methods.R", echo = TRUE)
# beepr::beep(4)

source("R/normalized/normalized-measures-MGD.R", echo = TRUE)
beepr::beep(4)
