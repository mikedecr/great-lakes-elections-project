#----------------------------------------
#   EAVS 2016
#   Aggregate sections A through C
#----------------------------------------

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



d <- list(A = readRDS("data/clean/clean-EAVS-2016-A.RDS"),
          B = readRDS("data/clean/clean-EAVS-2016-B.RDS"),
          C = readRDS("data/clean/clean-EAVS-2016-C.RDS")) %>%
     bind_cols %>%
     as_data_frame %>% 
     print

names(d)


d %>%
group_by(JurisdictionName) %>%
summarize(n = n()) %$% 
table(n) 



d %>%
select(-FIPSCode, -FIPS_2Digit, -JurisdictionName) %>% 
group_by(State) %>% 
summarize_all(mean, na.rm = TRUE) 