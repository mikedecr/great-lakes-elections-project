#----------------------------------------
#   EAVS 2016, section B
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


#----------------------------------------
#   begin
#----------------------------------------

todo <- 0


# directory already set in build file.

d <- haven::read_dta("data/eavs-2016.dta") %>%
					select(FIPSCode:JurisdictionName, starts_with("C"))

names(d)

# variable names to be removed from DF later
drop_names <- select(d, -(FIPSCode:JurisdictionName)) %>%
              names




#----------------------------------------
#   naming scheme
#----------------------------------------

beepr::beep(2)
todo <- todo + 1
# almost every topic set (A2, A3, etc) has comment variables
# Perhaps should only muck through our states?


#----------------------------------------
#   SECTION C: domestic absentee ballots
#----------------------------------------



#----------------------------------------
#   C1: topline absentee stats
#----------------------------------------

## C1a: abs_sent_total
## C1b: abs_returned_total
## C1c: abs_result_undeliver
## C1d: abs_result_spoiled
## C1e: abs_result_unknown
## C1f: abs_result_other1
## C1g: abs_result_other2
## C1h: abs_result_other3

## calc: abs_result


#----------------------------------------
#   C2: permanent absentee registration
#----------------------------------------

## C2: abs_permareg


#----------------------------------------
#   C3: number sent to permanent absentees
#----------------------------------------

## C3: abs_perma_sentcount


#----------------------------------------
#   C4: fate of returned ballots
#----------------------------------------

## C4a: abs_return_counted
## C4b: abs_return_reject
## C4c: abs_return_other1
## C4d: abs_return_other2

## calc: abs_return_total_calc (== C1b ,abs_returned_total)


#----------------------------------------
#   C5: reason absentee ballot rejected
#----------------------------------------

## C5a: abs_rejwhy_date
## C5b: abs_rejwhy_votersig
## C5c: abs_rejwhy_witsig
## C5d: abs_rejwhy_sigmatch
## C5e: abs_rejwhy_eosig
## C5f: abs_rejwhy_envelope
## C5g: abs_rejwhy_noballot
## C5h: abs_rejwhy_envseal
## C5i: abs_rejwhy_address
## C5j: abs_rejwhy_multballots
## C5k: abs_rejwhy_deceased
## C5l: abs_rejwhy_inperson
## C5m: abs_rejwhy_id
## C5n: abs_rejwhy_noapp
## C5o: abs_rejwhy_other1
## C5p: abs_rejwhy_other2
## C5q: abs_rejwhy_other3
## C5r: abs_rejwhy_other4
## C5s: abs_rejwhy_other5
## C5t: abs_rejwhy_other6
## C5u: abs_rejwhy_other7
## C5v: abs_rejwhy_other8

## calc: abs_rejwhy_total_calc ( == C4b, abs_return_reject)






#----------------------------------------
#   doing it
#----------------------------------------





#----------------------------------------
#   C1: topline absentee stats
#----------------------------------------

head(table(d$C1a, exclude = NULL))
head(table(d$C1b, exclude = NULL))
head(table(d$C1c, exclude = NULL))
head(table(d$C1d, exclude = NULL))
head(table(d$C1e, exclude = NULL))
head(table(d$C1f, exclude = NULL))
head(table(d$C1g, exclude = NULL))
head(table(d$C1h, exclude = NULL))

table(is.na(d$C1a), exclude = NULL)
table(is.na(d$C1b), exclude = NULL)
table(is.na(d$C1c), exclude = NULL)
table(is.na(d$C1d), exclude = NULL)
table(is.na(d$C1e), exclude = NULL)
table(is.na(d$C1f), exclude = NULL)
table(is.na(d$C1g), exclude = NULL)
table(is.na(d$C1h), exclude = NULL)

table(d$C1a %% 1, exclude = NULL)
table(d$C1b %% 1, exclude = NULL)
table(d$C1c %% 1, exclude = NULL)
table(d$C1d %% 1, exclude = NULL)
table(d$C1e %% 1, exclude = NULL)
table(d$C1f %% 1, exclude = NULL)
table(d$C1g %% 1, exclude = NULL)
table(d$C1h %% 1, exclude = NULL)




d <- 
  mutate(d,
         abs_sent_total = ifelse(C1a < 0, NA, C1a),
         na_abs_sent_total = case_when(C1a >= 0 ~ "Reported",
                                       C1a == -888888 ~ "Not applicable",
                                       C1a == -999999 ~ "Not available",
                                       is.na(C1a) ~ "Missing"),
         abs_returned_total = ifelse(C1b < 0, NA, C1b),
         na_abs_returned_total = case_when(C1b >= 0 ~ "Reported",
                                           C1b == -888888 ~ "Not applicable",
                                           C1b == -999999 ~ "Not available",
                                           is.na(C1b) ~ "Missing"),
         abs_result_undeliver = ifelse(C1c < 0, NA, C1c),
         na_abs_result_undeliver = case_when(C1c >= 0 ~ "Reported",
                                             C1c == -888888 ~ "Not applicable",
                                             C1c == -999999 ~ "Not available",
                                             is.na(C1c) ~ "Missing"),
         abs_result_spoiled = ifelse(C1d < 0, NA, C1d),
         na_abs_result_spoiled = case_when(C1d >= 0 ~ "Reported",
                                           C1d == -888888 ~ "Not applicable",
                                           C1d == -999999 ~ "Not available",
                                           is.na(C1d) ~ "Missing"),
         abs_result_unknown = ifelse(C1e < 0, NA, C1e),
         na_abs_result_unknown = case_when(C1e >= 0 ~ "Reported",
                                           C1e == -888888 ~ "Not applicable",
                                           C1e == -999999 ~ "Not available",
                                           is.na(C1e) ~ "Missing"),
         abs_result_other1 = ifelse(C1f < 0, NA, C1f),
         na_abs_result_other1 = case_when(C1f >= 0 ~ "Reported",
                                          C1f == -888888 ~ "Not applicable",
                                          C1f == -999999 ~ "Not available",
                                          is.na(C1f) ~ "Missing"),
         abs_result_other2 = ifelse(C1g < 0, NA, C1g),
         na_abs_result_other2 = case_when(C1g >= 0 ~ "Reported",
                                          C1g == -888888 ~ "Not applicable",
                                          C1g == -999999 ~ "Not available",
                                          is.na(C1g) ~ "Missing"),
         abs_result_other3 = ifelse(C1h < 0, NA, C1h),
         na_abs_result_other3 = case_when(C1h >= 0 ~ "Reported",
                                          C1h == -888888 ~ "Not applicable",
                                          C1h == -999999 ~ "Not available",
                                          is.na(C1h) ~ "Missing"),
         abs_result_total_calc = abs_result_undeliver + 
                                 abs_result_spoiled +
                                 abs_result_unknown +
                                 abs_result_other1 +
                                 abs_result_other2 +
                                 abs_result_other3)



#----------------------------------------
#   C2 and C3: permanent absentee registration
#----------------------------------------

head(table(d$C2, exclude = NULL))
head(table(d$C3, exclude = NULL))

table(is.na(d$C2), exclude = NULL)
table(is.na(d$C3), exclude = NULL)

table(d$C2 %% 1, exclude = NULL)
table(d$C3 %% 1, exclude = NULL)


d <- 
  mutate(d, 
         abs_permareg = ifelse(C2 < 0, NA, C2),
         na_abs_permareg = case_when(C2 >= 0 ~ "Reported",
                                     C2 == -888888 ~ "Not applicable",
                                     C2 == -9999999 ~ "Not available",
                                     is.na(C2) ~ "Missing"),
         abs_perma_sentcount = ifelse(C3 < 0, NA, C3),
         na_abs_perma_sentcount = case_when(C3 >= 0 ~ "Reported",
                                            C3 == -888888 ~ "Not applicable",
                                            C3 == -999999 ~ "Not available",
                                            is.na(C3) ~ "Missing"))


#----------------------------------------
#   C4: fate of returned ballots
#----------------------------------------

head(table(d$C4a, exclude = NULL))
head(table(d$C4b, exclude = NULL))
head(table(d$C4c, exclude = NULL))
head(table(d$C4d, exclude = NULL))

table(is.na(d$C4a), exclude = NULL)
table(is.na(d$C4b), exclude = NULL)
table(is.na(d$C4c), exclude = NULL)
table(is.na(d$C4d), exclude = NULL)

table(d$C4a %% 1, exclude = NULL)
table(d$C4b %% 1, exclude = NULL)
table(d$C4c %% 1, exclude = NULL)
table(d$C4d %% 1, exclude = NULL)

d <- 
  mutate(d, 
         abs_return_counted = ifelse(C4a < 0, NA, C4a),
         na_abs_return_counted = case_when(C4a >= 0 ~ "Reported",
                                           C4a == -888888 ~ "Not applicable",
                                           C4a == -999999 ~ "Not available",
                                           is.na(C4a) ~ "Missing"),
         abs_return_reject = ifelse(C4b < 0, NA, C4b),
         na_abs_return_reject = case_when(C4b >= 0 ~ "Reported",
                                          C4b == -888888 ~ "Not applicable",
                                          C4b == -999999 ~ "Not available",
                                          is.na(C4b) ~ "Missing"),
         abs_return_other1 = ifelse(C4c < 0, NA, C4c),
         na_abs_return_other1 = case_when(C4c >= 0 ~ "Reported",
                                          C4c == -888888 ~ "Not applicable",
                                          C4c == -999999 ~ "Not available",
                                          is.na(C4c) ~ "Missing"),
         abs_return_other2 = ifelse(C4d < 0, NA, C4d),
         na_abs_return_other2 = case_when(C4d >= 0 ~ "Reported",
                                          C4d == -888888 ~ "Not applicable",
                                          C4d == -999999 ~ "Not available",
                                          is.na(C4d) ~ "Missing"),
         abs_return_total_calc = abs_return_counted +
                                 abs_return_reject +
                                 abs_return_other1 +
                                 abs_return_other2)


#----------------------------------------
#   C5: reason absentee ballot rejected
#----------------------------------------


head(table(d$C5a, exclude = NULL))
head(table(d$C5b, exclude = NULL))
head(table(d$C5c, exclude = NULL))
head(table(d$C5d, exclude = NULL))
head(table(d$C5e, exclude = NULL))
head(table(d$C5f, exclude = NULL))
head(table(d$C5g, exclude = NULL))
head(table(d$C5h, exclude = NULL))
head(table(d$C5i, exclude = NULL))
head(table(d$C5j, exclude = NULL))
head(table(d$C5k, exclude = NULL))
head(table(d$C5l, exclude = NULL))
head(table(d$C5m, exclude = NULL))
head(table(d$C5n, exclude = NULL))
head(table(d$C5o, exclude = NULL))
head(table(d$C5p, exclude = NULL))
head(table(d$C5q, exclude = NULL))
head(table(d$C5r, exclude = NULL))
head(table(d$C5s, exclude = NULL))
head(table(d$C5t, exclude = NULL))
head(table(d$C5u, exclude = NULL))
head(table(d$C5v, exclude = NULL))

table(is.na(d$C5a), exclude = NULL)
table(is.na(d$C5b), exclude = NULL)
table(is.na(d$C5c), exclude = NULL)
table(is.na(d$C5d), exclude = NULL)
table(is.na(d$C5e), exclude = NULL)
table(is.na(d$C5f), exclude = NULL)
table(is.na(d$C5g), exclude = NULL)
table(is.na(d$C5h), exclude = NULL)
table(is.na(d$C5i), exclude = NULL)
table(is.na(d$C5j), exclude = NULL)
table(is.na(d$C5k), exclude = NULL)
table(is.na(d$C5l), exclude = NULL)
table(is.na(d$C5m), exclude = NULL)
table(is.na(d$C5n), exclude = NULL)
table(is.na(d$C5o), exclude = NULL)
table(is.na(d$C5p), exclude = NULL)
table(is.na(d$C5q), exclude = NULL)
table(is.na(d$C5r), exclude = NULL)
table(is.na(d$C5s), exclude = NULL)
table(is.na(d$C5t), exclude = NULL)
table(is.na(d$C5u), exclude = NULL)
table(is.na(d$C5v), exclude = NULL)

table(d$C5a %% 1, exclude = NULL)
table(d$C5b %% 1, exclude = NULL)
table(d$C5c %% 1, exclude = NULL)
table(d$C5d %% 1, exclude = NULL)
table(d$C5e %% 1, exclude = NULL)
table(d$C5f %% 1, exclude = NULL)
table(d$C5g %% 1, exclude = NULL)
table(d$C5h %% 1, exclude = NULL)
table(d$C5i %% 1, exclude = NULL)
table(d$C5j %% 1, exclude = NULL)
table(d$C5k %% 1, exclude = NULL)
table(d$C5l %% 1, exclude = NULL)
table(d$C5m %% 1, exclude = NULL)
table(d$C5n %% 1, exclude = NULL)
table(d$C5o %% 1, exclude = NULL)
table(d$C5p %% 1, exclude = NULL)
table(d$C5q %% 1, exclude = NULL)
table(d$C5r %% 1, exclude = NULL)
table(d$C5s %% 1, exclude = NULL)
table(d$C5t %% 1, exclude = NULL)
table(d$C5u %% 1, exclude = NULL)
table(d$C5v %% 1, exclude = NULL)

d <- 
  mutate(d, 
         abs_rejwhy_date = ifelse(C5a < 0, NA, C5a),
         na_abs_rejwhy_date = case_when(C5a >= 0 ~ "Reported",
                                        C5a == -888888 ~ "Not applicable",
                                        C5a == -999999 ~ "Not available",
                                        is.na(C5a) ~ "Missing"),
         abs_rejwhy_votersig = ifelse(C5b < 0, NA, C5b),
         na_abs_rejwhy_votersig = case_when(C5b >= 0 ~ "Reported",
                                            C5b == -888888 ~ "Not applicable",
                                            C5b == -999999 ~ "Not available",
                                            is.na(C5b) ~ "Missing"),
         abs_rejwhy_witsig = ifelse(C5c < 0, NA, C5c),
         na_abs_rejwhy_witsig = case_when(C5c >= 0 ~ "Reported",
                                          C5c == -888888 ~ "Not applicable",
                                          C5c == -999999 ~ "Not available",
                                          is.na(C5c) ~ "Missing"),
         abs_rejwhy_sigmatch = ifelse(C5d < 0, NA, C5d),
         na_abs_rejwhy_sigmatch = case_when(C5d >= 0 ~ "Reported",
                                            C5d == -888888 ~ "Not applicable",
                                            C5d == -999999 ~ "Not available",
                                            is.na(C5d) ~ "Missing"),
         abs_rejwhy_eosig = ifelse(C5e < 0, NA, C5e),
         na_abs_rejwhy_eosig = case_when(C5e >= 0 ~ "Reported",
                                         C5e == -888888 ~ "Not applicable",
                                         C5e == -999999 ~ "Not available",
                                         is.na(C5e) ~ "Missing"),
         abs_rejwhy_envelope = ifelse(C5f < 0, NA, C5f),
         na_abs_rejwhy_envelope = case_when(C5f >= 0 ~ "Reported",
                                            C5f == -888888 ~ "Not applicable",
                                            C5f == -999999 ~ "Not available",
                                            is.na(C5f) ~ "Missing"),
         abs_rejwhy_noballot = ifelse(C5g < 0, NA, C5g),
         na_abs_rejwhy_noballot = case_when(C5g >= 0 ~ "Reported",
                                            C5g == -888888 ~ "Not applicable",
                                            C5g == -999999 ~ "Not available",
                                            is.na(C5g) ~ "Missing"),
         abs_rejwhy_envseal = ifelse(C5h < 0, NA, C5h),
         na_abs_rejwhy_envseal = case_when(C5h >= 0 ~ "Reported",
                                           C5h == -888888 ~ "Not applicable",
                                           C5h == -999999 ~ "Not available",
                                           is.na(C5h) ~ "Missing"),
         abs_rejwhy_address = ifelse(C5i < 0, NA, C5i),
         na_abs_rejwhy_address = case_when(C5i >= 0 ~ "Reported",
                                           C5i == -888888 ~ "Not applicable",
                                           C5i == -999999 ~ "Not available",
                                           is.na(C5i) ~ "Missing"),
         abs_rejwhy_multballots = ifelse(C5j < 0, NA, C5j),
         na_abs_rejwhy_multballots = case_when(C5j >= 0 ~ "Reported",
                                               C5j == -888888 ~ "Not applicable",
                                               C5j == -999999 ~ "Not available",
                                               is.na(C5j) ~ "Missing"),
         abs_rejwhy_deceased = ifelse(C5k < 0, NA, C5k),
         na_abs_rejwhy_deceased = case_when(C5k >= 0 ~ "Reported",
                                            C5k == -888888 ~ "Not applicable",
                                            C5k == -999999 ~ "Not available",
                                            is.na(C5k) ~ "Missing"),
         abs_rejwhy_inperson = ifelse(C5l < 0, NA, C5l),
         na_abs_rejwhy_inperson = case_when(C5l >= 0 ~ "Reported",
                                            C5l == -888888 ~ "Not applicable",
                                            C5l == -999999 ~ "Not available",
                                            is.na(C5l) ~ "Missing"),
         abs_rejwhy_id = ifelse(C5m < 0, NA, C5m),
         na_abs_rejwhy_id = case_when(C5m >= 0 ~ "Reported",
                                      C5m == -888888 ~ "Not applicable",
                                      C5m == -999999 ~ "Not available",
                                      is.na(C5m) ~ "Missing"),
         abs_rejwhy_noapp = ifelse(C5n < 0, NA, C5n),
         na_abs_rejwhy_noapp = case_when(C5n >= 0 ~ "Reported",
                                         C5n == -888888 ~ "Not applicable",
                                         C5n == -999999 ~ "Not available",
                                         is.na(C5n) ~ "Missing"),
         abs_rejwhy_other1 = ifelse(C5o < 0, NA, C5o),
         na_abs_rejwhy_other1 = case_when(C5o >= 0 ~ "Reported",
                                          C5o == -888888 ~ "Not applicable",
                                          C5o == -999999 ~ "Not available",
                                          is.na(C5o) ~ "Missing"),
         abs_rejwhy_other2 = ifelse(C5p < 0, NA, C5p),
         na_abs_rejwhy_other2 = case_when(C5p >= 0 ~ "Reported",
                                          C5p == -888888 ~ "Not applicable",
                                          C5p == -999999 ~ "Not available",
                                          is.na(C5p) ~ "Missing"),
         abs_rejwhy_other3 = ifelse(C5q < 0, NA, C5q),
         na_abs_rejwhy_other3 = case_when(C5q >= 0 ~ "Reported",
                                          C5q == -888888 ~ "Not applicable",
                                          C5q == -999999 ~ "Not available",
                                          is.na(C5q) ~ "Missing"),
         abs_rejwhy_other4 = ifelse(C5r < 0, NA, C5r),
         na_abs_rejwhy_other4 = case_when(C5r >= 0 ~ "Reported",
                                          C5r == -888888 ~ "Not applicable",
                                          C5r == -999999 ~ "Not available",
                                          is.na(C5r) ~ "Missing"),
         abs_rejwhy_other5 = ifelse(C5s < 0, NA, C5s),
         na_abs_rejwhy_other5 = case_when(C5s >= 0 ~ "Reported",
                                          C5s == -888888 ~ "Not applicable",
                                          C5s == -999999 ~ "Not available",
                                          is.na(C5s) ~ "Missing"),
         abs_rejwhy_other6 = ifelse(C5t < 0, NA, C5t),
         na_abs_rejwhy_other6 = case_when(C5t >= 0 ~ "Reported",
                                          C5t == -888888 ~ "Not applicable",
                                          C5t == -999999 ~ "Not available",
                                          is.na(C5t) ~ "Missing"),
         abs_rejwhy_other7 = ifelse(C5u < 0, NA, C5u),
         na_abs_rejwhy_other7 = case_when(C5u >= 0 ~ "Reported",
                                          C5u == -888888 ~ "Not applicable",
                                          C5u == -999999 ~ "Not available",
                                          is.na(C5u) ~ "Missing"),
         abs_rejwhy_other8 = ifelse(C5v < 0, NA, C5v),
         na_abs_rejwhy_other8 = case_when(C5v >= 0 ~ "Reported",
                                          C5v == -888888 ~ "Not applicable",
                                          C5v == -999999 ~ "Not available",
                                          is.na(C5v) ~ "Missing"),
         abs_rejwhy_total_calc = abs_rejwhy_date + +
                                 abs_rejwhy_votersig +
                                 abs_rejwhy_witsig +
                                 abs_rejwhy_sigmatch +
                                 abs_rejwhy_eosig +
                                 abs_rejwhy_envelope +
                                 abs_rejwhy_noballot +
                                 abs_rejwhy_envseal +
                                 abs_rejwhy_address +
                                 abs_rejwhy_multballots +
                                 abs_rejwhy_deceased +
                                 abs_rejwhy_inperson +
                                 abs_rejwhy_id +
                                 abs_rejwhy_noapp +
                                 abs_rejwhy_other1 +
                                 abs_rejwhy_other2 +
                                 abs_rejwhy_other3 +
                                 abs_rejwhy_other4 +
                                 abs_rejwhy_other5 +
                                 abs_rejwhy_other6 +
                                 abs_rejwhy_other7 +
                                 abs_rejwhy_other8)



# remove original variables and save
d %>%
select(-one_of(drop_names)) %>% 
print %>% 
saveRDS("data/clean/clean-EAVS-2016-C.RDS")