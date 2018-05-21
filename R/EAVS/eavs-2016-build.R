#----------------------------------------
#   build file: EAVS 2016
#----------------------------------------

# 1. set up directories
# 2. Run files A through C
# 3. Evan has D through F

# master build file loads packages already
library("magrittr")
library("tidyverse")
library("here")



#----------------------------------------
#   directories
#----------------------------------------

here()

# You should ALREADY be in the project root directory
# (via .Rprof file or by opening an R session in the project root)
# you can setwd() but this project isn't designed for that

# here() will always create a path relative to the root
# *even if you set the working directory lower in the tree*

# output and local variable directories
dir.create(here("output"))
dir.create(here("output/eavs"))
dir.create(here("output/eavs/state"))
dir.create(here("output/eavs/state/A"))
dir.create(here("output/eavs/state/B"))
dir.create(here("output/eavs/state/C"))
dir.create(here("output/eavs/jurisdiction"))



#----------------------------------------
#   global settings
#----------------------------------------
options(scipen = 99999)





# --------------------------------------
#  Data
# --------------------------------------


# --- extract EAVS data -----------------

# we check if the unzipped file already exists, else unzip it

# (making file paths easier)
(where_eavs <- here("data/eavs/"))
eavs_filename <- "EAVS 2016 Final Data for Public Release v.3.dta"
eavs_zipname <- "EAVS_2016_Final_Data_for_Public_Release_v2.dta.zip"

# check for unzipped
if (eavs_filename %in% list.files(where_eavs)) {
  print("EAVS already unzipped")
} else {
  unzip(paste0(where_eavs, eavs_zipname), exdir = where_eavs)
}




# --- unlabel and resave -----------------

eavs <- haven::read_dta(paste0(where_eavs, eavs_filename)) %>%
  mutate_all(labelled::remove_labels) %>%
  filter(State %in% c(state.abb, "DC")) %>%
  print() %>%
  saveRDS(paste0(where_eavs, "eavs-2016-unlabelled.RDS"))





# --------------------------------------
#  aggregation
# --------------------------------------

# other files:
# 1. aggregate (calls the tabulation file)
# 2. jurisdiction data using columns from (1)
source("R/EAVS/EAVS-aggregate-A-C.R", echo = TRUE)
source("R/EAVS/EAVS-joyce-munic.R", echo = TRUE)


# --- confirm -----------------
print("EAVS code completed without error")
