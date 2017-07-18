#----------------------------------------
#   build file: EAVS 2016
#----------------------------------------


# 1. set up directories
# 2. Run files A through C
# 3. Evan has D through F

rm(list = ls())

library("magrittr")
library("tidyverse")
library("ggplot2")
library("forcats")
library("stringr")




#  custom ggtheme, import from Github
source("https://raw.githubusercontent.com/mikedecr/theme-mgd/master/theme_mgd.R")
theme_set(theme_mgd()) # set default theme
library("extrafont")   # load font faces into R



#----------------------------------------
#   directories
#----------------------------------------

parent_dir <- "/Users/michaeldecrescenzo/box sync/pa/mit/eavs/2016"

setwd(parent_dir)

# output and local variable directories

dir.create("data/clean")
dir.create("output")
dir.create("output/jurisdiction")
dir.create("output/state")
dir.create("output/graphics")


#----------------------------------------
#   global settings
#----------------------------------------

options(scipen = 99999)



#----------------------------------------
#   execute
#----------------------------------------

source("R/EAVS-2016-A.R")
source("R/EAVS-2016-B.R")
source("R/EAVS-2016-C.R")
