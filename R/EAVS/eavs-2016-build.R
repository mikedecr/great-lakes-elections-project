#----------------------------------------
#   build file: EAVS 2016
#----------------------------------------

# 1. set up directories
# 2. Run files A through C
# 3. Evan has D through F


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
  print() %>%
  saveRDS(paste0(where_eavs, "eavs-2016-unlabelled.RDS"))





# --------------------------------------
#  aggregation
# --------------------------------------

# other files:
source("R/EAVS/EAVS-aggregate-A-C.R", echo = TRUE)
# aggregate file calls the tabulate file

source("R/EAVS/EAVS-joyce-munic.R", echo = TRUE)



print("EAVS code completed without error")



# --- things we don't need anymore? -----------------


#
#
# #----------------------------------------
# #   execute
# #----------------------------------------
#
# # Begin by trimming states to Joyce
# (joyce_states <- c("IL", "IN", "MI", "MN", "OH", "WI"))
#
# # would include the FIPS codes for these states
# # but the Wisconsin FIPS is borked
#
#
# # removes stata labels, trim to US states
# d <- haven::read_dta("data/eavs-2016.dta") %>%
#      filter(State %in% state.abb) %>%
#      mutate_all(labelled::remove_labels) %>%
#      print
#
# saveRDS(d, "data/eavs-2016-unlabelled.RDS")
#
#
#
# # limits to Great Lakes only
#
# d %>%
# filter(State %in% joyce_states) %>%
# saveRDS("data/joyce-eavs-2016-unlabelled.RDS")
#





# source("R/EAVS-2016-A.R")
# source("R/EAVS-2016-B.R")
# source("R/EAVS-2016-C.R")
# source("R/eavs-2016-munic.R")

# ----------------------------------------------------
#   aggregation
# ----------------------------------------------------

# not clear how to do missingness
# Not applicable == 0 seems sensible
# Not available == ?
# Uncoded missing == ?
