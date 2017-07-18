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
					select(FIPSCode:JurisdictionName, starts_with("B")) 


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
#   B1: how many sent to whom
#----------------------------------------

## B1a: ava_sent_total
## B1b: ava_sent_unif
## B1c: ava_sent_civ
## B1d: ava_sent_other1
## B1e: ava_sent_other2

## calc: ava_sent_total_calc


#----------------------------------------
#   B2: what happened when sent
#----------------------------------------

## B2a: ava_result_returned
## B2b: ava_result_undeliver
## B2c: ava_result_spoiled
## B2d: ava_result_unknown
## B2e: ava_result_other1
## B2f: ava_result_other2
## B2g: ava_result_other3

## calc: ava_result_total_calc


#----------------------------------------
#   B8: total counted
#----------------------------------------

## B8a: ava_counted_total


#----------------------------------------
#   B9: counted from whom
#----------------------------------------

## B9a: ava_countwho_unif
## B9b: ava_countwho_civ
## B9c: ava_countwho_other

## calc: ava_countwho_total_calc


#----------------------------------------
#   B10: absentee ballots counted from whom
#----------------------------------------

## B10a: ava_countabswho_unif
## B10b: ava_countabswho_civ
## B10c: ava_countabswho_other

## calc: ava_countabswho_total_calc


#----------------------------------------
#   B11: FWA ballots counted from whom
#----------------------------------------

## B11a: ava_countfwabwho_unif
## B11b: ava_countfwabwho_civ
## B11c: ava_countfwabwho_other

## calc: ava_countfwabwho_total_calc


#----------------------------------------
#   B12: other types of ballots counted from whom
#----------------------------------------

## B12a: ava_countotherwho_unif
## B12b: ava_countotherwho_civ
## B12c: ava_countotherwho_other

## calc: ava_countotherwho_total_calc


#----------------------------------------
#   B13: total rejected ballots (abs, fwab, and other)
#----------------------------------------

## B13a: ava_rej_total


#----------------------------------------
#   B14: reasons rejected
#----------------------------------------

## B14a: ava_rejwhy_time
## B14b: ava_rejwhy_signature
## B14c: ava_rejwhy_postmark
## B14d: ava_rejwhy_other1
## B14e: ava_rejwhy_other2
## B14f: ava_rejwhy_other3

## calc: ava_rejwhy_total_calc (should == B13, ava_rej_total)



#----------------------------------------
#   B15: rejected ballots from whom
#----------------------------------------

## B15a: ava_rejwho_unif
## B15b: ava_rejwho_civ
## B15c: ava_rejwho_other

## calc: ava_rejwho_total_calc (should == B13, ava_rej_total)


#----------------------------------------
#   B16: rejected absentee ballots from whom
#----------------------------------------

## B16a: ava_rejabswho_unif
## B16b: ava_rejabswho_civ
## B16c: ava_rejabswho_other

## calc: ava_rejabswho_total_calc

#----------------------------------------
#   B17: rejected fwabs from whom
#----------------------------------------

## B17a: ava_rejfwabwho_unif
## B17b: ava_rejfwabwho_civ
## B17c: ava_rejfwabwho_other

## calc: ava_rejfwabwho_total_calc


#----------------------------------------
#   B18: rejected 'other' ballots from whom
#----------------------------------------

## B18a: ava_rejotherwho_unif
## B18b: ava_rejotherwho_civ
## B18c: ava_rejotherwho_other

## calc: ava_rejotherwho_total_calc


todo	<- todo + 1
beepr::beep(2)
# ava_rejabswho_total_calc + ava_rejfwabwho_total_calc + ava_rejotherwho_total_calc = { ava_rejwho_total_calc , ava_rej_total }? 






#----------------------------------------
#   DOING IT
#----------------------------------------


#----------------------------------------
#   B1: how many sent to whom
#----------------------------------------

head(table(d$B1a, exclude = NULL))
head(table(d$B1b, exclude = NULL))
head(table(d$B1c, exclude = NULL))
head(table(d$B1d, exclude = NULL))
head(table(d$B1e, exclude = NULL))

table(is.na(d$B1a), exclude = NULL)
table(is.na(d$B1b), exclude = NULL)
table(is.na(d$B1c), exclude = NULL)
table(is.na(d$B1d), exclude = NULL)
table(is.na(d$B1e), exclude = NULL)

table(d$B1a %% 1, exclude = NULL)
table(d$B1b %% 1, exclude = NULL)
table(d$B1c %% 1, exclude = NULL)
table(d$B1d %% 1, exclude = NULL)
table(d$B1e %% 1, exclude = NULL)



d <- 
  mutate(d,
         ava_sent_total = ifelse(B1a < 0, NA, B1a),
         na_ava_sent_total = case_when(B1a >= 0 ~ "Reported",
                                       B1a == -888888 ~ "Not applicable",
                                       B1a == -999999 ~ "Not available",
                                       is.na(B1a) ~ "Missing"),
         ava_sent_unif = ifelse(B1b < 0, NA, B1b),
         na_ava_sent_unif = case_when(B1b >= 0 ~ "Reported",
                                      B1b == -888888 ~ "Not applicable",
                                      B1b == -999999 ~ "Not available",
                                      is.na(B1b) ~ "Missing"),
         ava_sent_civ = ifelse(B1c < 0, NA, B1c),
         na_ava_sent_civ = case_when(B1c >= 0 ~ "Reported",
                                     B1c == -888888 ~ "Not applicable",
                                     B1c == -999999 ~ "Not available",
                                     is.na(B1c) ~ "Missing"),
         ava_sent_other1 = ifelse(B1d < 0, NA, B1d),
         na_ava_sent_other1 = case_when(B1d >= 0 ~ "Reported",
                                        B1d == -888888 ~ "Not applicable",
                                        B1d == -999999 ~ "Not available",
                                        is.na(B1d) ~ "Missing"),
         ava_sent_other2 = ifelse(B1e < 0, NA, B1e),
         na_ava_sent_other2 = case_when(B1e >= 0 ~ "Reported",
                                        B1e == -888888 ~ "Not applicable",
                                        B1e == -999999 ~ "Not available",
                                        is.na(B1e) ~ "Missing"),
         ava_sent_total_calc = ava_sent_unif + ava_sent_civ +
                               ava_sent_other1 + ava_sent_other2)




#----------------------------------------
#   b2: what happened when sent:
#----------------------------------------

head(table(d$B2a, exclude = NULL))
head(table(d$B2b, exclude = NULL))
head(table(d$B2c, exclude = NULL))
head(table(d$B2d, exclude = NULL))
head(table(d$B2e, exclude = NULL))
head(table(d$B2f, exclude = NULL))
head(table(d$B2g, exclude = NULL))

table(is.na(d$B2a), exclude = NULL)
table(is.na(d$B2b), exclude = NULL)
table(is.na(d$B2c), exclude = NULL)
table(is.na(d$B2d), exclude = NULL)
table(is.na(d$B2e), exclude = NULL)
table(is.na(d$B2f), exclude = NULL)
table(is.na(d$B2g), exclude = NULL)

table(d$B2a %% 1, exclude = NULL)
table(d$B2b %% 1, exclude = NULL)
table(d$B2c %% 1, exclude = NULL)
table(d$B2d %% 1, exclude = NULL)
table(d$B2e %% 1, exclude = NULL)
table(d$B2f %% 1, exclude = NULL)
table(d$B2g %% 1, exclude = NULL)




d <- 
  mutate(d,
         ava_result_returned = ifelse(B2a < 0, NA, B2a),
         na_ava_result_returned = case_when(B2a >= 0 ~ "Reported",
                                            B2a == -888888 ~ "Not applicable",
                                            B2a == -999999 ~ "Not available",
                                            is.na(B2a) ~ "Missing"),
         ava_result_undeliver = ifelse(B2b < 0, NA, B2b),
         na_ava_result_undeliver = case_when(B2b >= 0 ~ "Reported",
                                             B2b == -888888 ~ "Not applicable",
                                             B2b == -999999 ~ "Not available",
                                             is.na(B2b) ~ "Missing"),
         ava_result_spoiled = ifelse(B2c < 0, NA, B2c),
         na_ava_result_spoiled = case_when(B2c >= 0 ~ "Reported",
                                           B2c == -888888 ~ "Not applicable",
                                           B2c == -999999 ~ "Not available",
                                           is.na(B2c) ~ "Missing"),
         ava_result_unknown = ifelse(B2d < 0, NA, B2d),
         na_ava_result_unknown = case_when(B2d >= 0 ~ "Reported",
                                           B2d == -888888 ~ "Not applicable",
                                           B2d == -999999 ~ "Not available",
                                           is.na(B2d) ~ "Missing"),
         ava_result_other1 = ifelse(B2e < 0, NA, B2e),
         na_ava_result_other1 = case_when(B2e >= 0 ~ "Reported",
                                          B2e == -888888 ~ "Not applicable",
                                          B2e == -999999 ~ "Not available",
                                          is.na(B2e) ~ "Missing"),
         ava_result_other2 = ifelse(B2f < 0, NA, B2f),
         na_ava_result_other2 = case_when(B2f >= 0 ~ "Reported",
                                          B2f == -888888 ~ "Not applicable",
                                          B2f == -999999 ~ "Not available",
                                          is.na(B2f) ~ "Missing"),
         ava_result_other3 = ifelse(B2g < 0, NA, B2g),
         na_ava_result_other3 = case_when(B2g >= 0 ~ "Reported",
                                          B2g == -888888 ~ "Not applicable",
                                          B2g == -999999 ~ "Not available",
                                          is.na(B2g) ~ "Missing"),
         ava_result_total_calc = ava_result_returned + ava_result_undeliver +
                                 ava_result_spoiled + ava_result_unknown +
                                 ava_result_other1 + ava_result_other2 +
                                 ava_result_other3)



#----------------------------------------
#  B3:  total UOCAVA ballots returned
#----------------------------------------

head(table(d$B3a, exclude = NULL))

table(is.na(d$B3a), exclude = NULL)

table(d$B3a %% 1, exclude = NULL)

d <- 
  mutate(d, 
         ava_returned_total = ifelse(B3a < 0, NA, B3a),
         na_ava_returned_total = case_when(B3a >= 0 ~ "Reported",
                                           B3a == -888888 ~ "Not applicable",
                                           B3a == -999999 ~ "Not available",
                                           is.na(B3a) ~ "Missing"))



#----------------------------------------
#   B4: returned ballots from whom
#----------------------------------------

head(table(d$B4a, exclude = NULL))
head(table(d$B4b, exclude = NULL))
head(table(d$B4c, exclude = NULL))

table(is.na(d$B4a), exclude = NULL)
table(is.na(d$B4b), exclude = NULL)
table(is.na(d$B4c), exclude = NULL)

table(d$B4a %% 1, exclude = NULL)
table(d$B4b %% 1, exclude = NULL)
table(d$B4c %% 1, exclude = NULL)


d <- 
  mutate(d,
         ava_subwho_unif = ifelse(B4a < 0, NA, B4a),
         na_ava_subwho_unif = case_when(B4a >= 0 ~ "Reported",
                                        B4a == -888888 ~ "Not applicable",
                                        B4a == -999999 ~ "Not available",
                                        is.na(B4a) ~ "Missing"),
         ava_subwho_civ = ifelse(B4b < 0, NA, B4b),
         na_ava_subwho_civ = case_when(B4b >= 0 ~ "Reported",
                                       B4b == -888888 ~ "Not applicable",
                                       B4b == -999999 ~ "Not available",
                                       is.na(B4b) ~ "Missing"),
         ava_subwho_other = ifelse(B4c < 0, NA, B4c),
         na_ava_subwho_other = case_when(B4c >= 0 ~ "Reported",
                                         B4c == -888888 ~ "Not applicable",
                                         B4c == -999999 ~ "Not available",
                                         is.na(B4c) ~ "Missing"),
         ava_subwho_total_calc = ava_subwho_unif + ava_subwho_civ +
                                 ava_subwho_other)


#----------------------------------------
#   B5: absentee ballots from whom
#----------------------------------------

head(table(d$B5a, exclude = NULL))
head(table(d$B5b, exclude = NULL))
head(table(d$B5c, exclude = NULL))

table(is.na(d$B5a), exclude = NULL)
table(is.na(d$B5b), exclude = NULL)
table(is.na(d$B5c), exclude = NULL)

table(d$B5a %% 1, exclude = NULL)
table(d$B5b %% 1, exclude = NULL)
table(d$B5c %% 1, exclude = NULL)


d <- 
  mutate(d,
         ava_subabswho_unif = ifelse(B5a < 0, NA, B5a),
         na_ava_subabswho_unif = case_when(B5a >= 0 ~ "Reported",
                                           B5a == -888888 ~ "Not applicable",
                                           B5a == -999999 ~ "Not available",
                                           is.na(B5a) ~ "Missing"),
         ava_subabswho_civ = ifelse(B5b < 0, NA, B5b),
         na_ava_subabswho_civ = case_when(B5b >= 0 ~ "Reported",
                                          B5b == -888888 ~ "Not applicable",
                                          B5b == -999999 ~ "Not available",
                                          is.na(B5b) ~ "Missing"),
         ava_subabswho_other = ifelse(B5c < 0, NA, B5c),
         na_ava_subabswho_other = case_when(B5c >= 0 ~ "Reported",
                                            B5c == -888888 ~ "Not applicable",
                                            B5c == -999999 ~ "Not available",
                                            is.na(B5c) ~ "Missing"),
         ava_subabswho_total_calc = ava_subabswho_unif + 
                                    ava_subabswho_civ +
                                    ava_subabswho_other)



#----------------------------------------
#   B6: FWAB ballots from whom
#----------------------------------------

head(table(d$B6a, exclude = NULL))
head(table(d$B6b, exclude = NULL))
head(table(d$B6c, exclude = NULL))

table(is.na(d$B6a), exclude = NULL)
table(is.na(d$B6b), exclude = NULL)
table(is.na(d$B6c), exclude = NULL)

table(d$B6a %% 1, exclude = NULL)
table(d$B6b %% 1, exclude = NULL)
table(d$B6c %% 1, exclude = NULL)


d <- 
  mutate(d, 
         ava_subfwabwho_unif = ifelse(B6a < 0, NA, B6a),
         na_ava_subfwabwho_unif = case_when(B6a >= 0 ~ "Reported",
                                            B6a == -888888 ~ "Not applicable",
                                            B6a == -999999 ~ "Not available",
                                            is.na(B6a) ~ "Missing"),
         ava_subfwabwho_civ = ifelse(B6b < 0, NA, B6b),
         na_ava_subfwabwho_civ = case_when(B6b >= 0 ~ "Reported",
                                           B6b == -888888 ~ "Not applicable",
                                           B6b == -999999 ~ "Not available",
                                           is.na(B6b) ~ "Missing"),
         ava_subfwabwho_other = ifelse(B6c < 0, NA, B6c),
         na_ava_subfwabwho_other = case_when(B6c >= 0 ~ "Reported",
                                             B6c == -888888 ~ "Not applicable",
                                             B6c == -999999 ~ "Not available",
                                             is.na(B6c) ~ "Missing"),
         ava_subfwabwho_total_calc = ava_subfwabwho_unif +
                                     ava_subfwabwho_civ + 
                                     ava_subfwabwho_other)






#----------------------------------------
#   B7: 'other' ballots fromw hom
#----------------------------------------

head(table(d$B7a, exclude = NULL))
head(table(d$B7b, exclude = NULL))
head(table(d$B7c, exclude = NULL))

table(is.na(d$B7a), exclude = NULL)
table(is.na(d$B7b), exclude = NULL)
table(is.na(d$B7c), exclude = NULL)

table(d$B7a %% 1, exclude = NULL)
table(d$B7b %% 1, exclude = NULL)
table(d$B7c %% 1, exclude = NULL)



d <- 
  mutate(d,
         ava_subotherwho_unif = ifelse(B7a < 0, NA, B7a),
         na_ava_subotherwho_unif = case_when(B7a >= 0 ~ "Reported",
                                             B7a == -888888 ~ "Not applicable",
                                             B7a == -999999 ~ "Not available",
                                             is.na(B7a) ~ "Missing"),
         ava_subotherwho_civ = ifelse(B7b < 0, NA, B7b),
         na_ava_subotherwho_civ = case_when(B7b >= 0 ~ "Reported",
                                            B7b == -888888 ~ "Not applicable",
                                            B7b == -999999 ~ "Not available",
                                            is.na(B7b) ~ "Missing"),
         ava_subotherwho_other = ifelse(B7c < 0, NA, B7c),
         na_ava_subotherwho_other = case_when(B7c >= 0 ~ "Reported",
                                              B7c == -888888 ~ "Not applicable",
                                              B7c == -999999 ~ "Not available",
                                              is.na(B7c) ~ "Missing"),
         ava_subotherwho_total_calc = ava_subotherwho_unif + 
                                      ava_subotherwho_civ + 
                                      ava_subotherwho_other)





#----------------------------------------
#   B8 and B9: total actually counted and whom
#----------------------------------------

head(table(d$B8a, exclude = NULL))
head(table(d$B9a, exclude = NULL))
head(table(d$B9b, exclude = NULL))
head(table(d$B9c, exclude = NULL))

table(is.na(d$B8a), exclude = NULL)
table(is.na(d$B9a), exclude = NULL)
table(is.na(d$B9b), exclude = NULL)
table(is.na(d$B9c), exclude = NULL)

table(d$B8a %% 1, exclude = NULL)
table(d$B9a %% 1, exclude = NULL)
table(d$B9b %% 1, exclude = NULL)
table(d$B9c %% 1, exclude = NULL)




d <- 
  mutate(d, 
         ava_counted_total = ifelse(B8a < 0, NA, B8a),
         na_ava_counted_total = case_when(B8a >= 0 ~ "Reported",
                                          B8a == -888888 ~ "Not applicable",
                                          B8a == -999999 ~ "Not available",
                                          is.na(B8a) ~ "Missing"),
         ava_countwho_unif = ifelse(B9a < 0, NA, B9a),
         na_ava_countwho_unif = case_when(B9a >= 0 ~ "Reported",
                                          B9a == -888888 ~ "Not applicable",
                                          B9a == -999999 ~ "Not available",
                                          is.na(B9a) ~ "Missing"),
         ava_countwho_civ = ifelse(B9b < 0, NA, B9b),
         na_ava_countwho_civ = case_when(B9b >= 0 ~ "Reported",
                                         B9b == -888888 ~ "Not applicable",
                                         B9b == -999999 ~ "Not available",
                                         is.na(B9b) ~ "Missing"),
         ava_countwho_other = ifelse(B9c < 0, NA, B9c),
         na_ava_countwho_other = case_when(B9c >= 0 ~ "Reported",
                                           B9c == -888888 ~ "Not applicable",
                                           B9c == -999999 ~ "Not available",
                                           is.na(B9c) ~ "Missing"),
         ava_countwho_total_calc = ava_countwho_unif + 
                                   ava_countwho_civ + 
                                   ava_countwho_other)




#----------------------------------------
#   B10: absentee ballots counted from whom
#----------------------------------------

head(table(d$B10a, exclude = NULL))
head(table(d$B10b, exclude = NULL))
head(table(d$B10c, exclude = NULL))

table(is.na(d$B10a), exclude = NULL)
table(is.na(d$B10b), exclude = NULL)
table(is.na(d$B10c), exclude = NULL)

table(d$B10a %% 1, exclude = NULL)
table(d$B10b %% 1, exclude = NULL)
table(d$B10c %% 1, exclude = NULL)


d <- 
  mutate(d, 
         ava_countabswho_unif = ifelse(B10a < 0, NA, B10a),
         na_ava_countabswho_unif = case_when(B10a >= 0 ~ "Reported",
                                             B10a == -888888 ~ "Not applicable",
                                             B10a == -999999 ~ "Not available",
                                             is.na(B10a) ~ "Missing"),
         ava_countabswho_civ = ifelse(B10b < 0, NA, B10b),
         na_ava_countabswho_civ = case_when(B10b >= 0 ~ "Reported",
                                            B10b == -888888 ~ "Not applicable",
                                            B10b == -999999 ~ "Not available",
                                            is.na(B10b) ~ "Missing"),
         ava_countabswho_other = ifelse(B10c < 0, NA, B10c),
         na_ava_countabswho_other = case_when(B10c >= 0 ~ "Reported",
                                              B10c == -888888 ~ "Not applicable",
                                              B10c == -999999 ~ "Not available",
                                              is.na(B10c) ~ "Missing"),
         ava_countabswho_total_calc = ava_countabswho_unif + 
                                      ava_countabswho_civ + 
                                      ava_countabswho_other)



#----------------------------------------
#   B11: FWA ballots counted from whom
#----------------------------------------

head(table(d$B11a, exclude = NULL))
head(table(d$B11b, exclude = NULL))
head(table(d$B11c, exclude = NULL))

table(is.na(d$B11a), exclude = NULL)
table(is.na(d$B11b), exclude = NULL)
table(is.na(d$B11c), exclude = NULL)

table(d$B11a %% 1, exclude = NULL)
table(d$B11b %% 1, exclude = NULL)
table(d$B11c %% 1, exclude = NULL)



d <- 
  mutate(d, 
         ava_countfwabwho_unif = ifelse(B11a < 0, NA, B11a),
         na_ava_countfwabwho_unif = case_when(B11a >= 0 ~ "Reported",
                                              B11a == -888888 ~ "Not applicable",
                                              B11a == -999999 ~ "Not available",
                                              is.na(B11a) ~ "Missing"),
         ava_countfwabwho_civ = ifelse(B11b < 0, NA, B11b),
         na_ava_countfwabwho_civ = case_when(B11b >= 0 ~ "Reported",
                                             B11b == -888888 ~ "Not applicable",
                                             B11b == -999999 ~ "Not available",
                                             is.na(B11b) ~ "Missing"),
         ava_countfwabwho_other = ifelse(B11c < 0, NA, B11c),
         na_ava_countfwabwho_other = case_when(B11c >= 0 ~ "Reported",
                                               B11c == -888888 ~ "Not applicable",
                                               B11c == -999999 ~ "Not available",
                                               is.na(B11c) ~ "Missing"),
         ava_countfwabwho_total_calc = ava_countfwabwho_unif + 
                                       ava_countfwabwho_civ +
                                       ava_countfwabwho_other)









#----------------------------------------
#   B12: other types of ballots counted from whom
#----------------------------------------

head(table(d$B12a, exclude = NULL))
head(table(d$B12b, exclude = NULL))
head(table(d$B12c, exclude = NULL))

table(is.na(d$B12a), exclude = NULL)
table(is.na(d$B12b), exclude = NULL)
table(is.na(d$B12c), exclude = NULL)

table(d$B12a %% 1, exclude = NULL)
table(d$B12b %% 1, exclude = NULL)
table(d$B12c %% 1, exclude = NULL)

d <- 
  mutate(d,
         ava_countotherwho_unif = ifelse(B12a < 0, NA, B12a),
         na_ava_countotherwho_unif = case_when(B12a >= 0 ~ "Reported",
                                               B12a == -888888 ~ "Not applicable",
                                               B12a == -999999 ~ "Not available",
                                               is.na(B12a) ~ "Missing"),
         ava_countotherwho_civ = ifelse(B12b < 0, NA, B12b),
         na_ava_countotherwho_civ = case_when(B12b >= 0 ~ "Reported",
                                              B12b == -888888 ~ "Not applicable",
                                              B12b == -999999 ~ "Not available",
                                              is.na(B12b) ~ "Missing"),
         ava_countotherwho_other = ifelse(B12c < 0, NA, B12c),
         na_ava_countotherwho_other = case_when(B12c >= 0 ~ "Reported",
                                                B12c == -888888 ~ "Not applicable",
                                                B12c == -999999 ~ "Not available",
                                                is.na(B12c) ~ "Missing"),
         ava_countotherwho_total_calc = ava_countotherwho_unif + 
                                        ava_countotherwho_civ + 
                                        ava_countotherwho_other)



#----------------------------------------
#   B13 and B14: total rejected ballots (abs, fwab, and other) and why
#----------------------------------------

head(table(d$B13a, exclude = NULL))
head(table(d$B14a, exclude = NULL))
head(table(d$B14b, exclude = NULL))
head(table(d$B14c, exclude = NULL))
head(table(d$B14d, exclude = NULL))
head(table(d$B14e, exclude = NULL))
head(table(d$B14f, exclude = NULL))

table(is.na(d$B13a), exclude = NULL)
table(is.na(d$B14a), exclude = NULL)
table(is.na(d$B14b), exclude = NULL)
table(is.na(d$B14c), exclude = NULL)
table(is.na(d$B14d), exclude = NULL)
table(is.na(d$B14e), exclude = NULL)
table(is.na(d$B14f), exclude = NULL)

table(d$B13a %% 1, exclude = NULL)
table(d$B14a %% 1, exclude = NULL)
table(d$B14b %% 1, exclude = NULL)
table(d$B14c %% 1, exclude = NULL)
table(d$B14d %% 1, exclude = NULL)
table(d$B14e %% 1, exclude = NULL)
table(d$B14f %% 1, exclude = NULL)

d <- 
  mutate(d, 
         ava_rej_total = ifelse(B13a < 0, NA, B13a),
         na_ava_rej_total = case_when(B13a >= 0 ~ "Reported",
                                      B13a == -888888 ~ "Not applicable",
                                      B13a == -999999 ~ "Not available",
                                      is.na(B13a) ~ "Missing"),
         ava_rejwhy_time = ifelse(B14a < 0, NA, B14a),
         na_ava_rejwhy_time = case_when(B14a >= 0 ~ "Reported",
                                        B14a == -888888 ~ "Not applicable",
                                        B14a == -999999 ~ "Not available",
                                        is.na(B14a) ~ "Missing"),
         ava_rejwhy_signature = ifelse(B14b < 0, NA, B14b),
         na_ava_rejwhy_signature = case_when(B14b >= 0 ~ "Reported",
                                             B14b == -888888 ~ "Not applicable",
                                             B14b == -999999 ~ "Not available",
                                             is.na(B14b) ~ "Missing"),
         ava_rejwhy_postmark = ifelse(B14c < 0, NA, B14c),
         na_ava_rejwhy_postmark = case_when(B14c >= 0 ~ "Reported",
                                            B14c == -888888 ~ "Not applicable",
                                            B14c == -999999 ~ "Not available",
                                            is.na(B14c) ~ "Missing"),
         ava_rejwhy_other1 = ifelse(B14d < 0, NA, B14d),
         na_ava_rejwhy_other1 = case_when(B14d >= 0 ~ "Reported",
                                          B14d == -888888 ~ "Not applicable",
                                          B14d == -999999 ~ "Not available",
                                          is.na(B14d) ~ "Missing"),
         ava_rejwhy_other2 = ifelse(B14e < 0, NA, B14e),
         na_ava_rejwhy_other2 = case_when(B14e >= 0 ~ "Reported",
                                          B14e == -888888 ~ "Not applicable",
                                          B14e == -999999 ~ "Not available",
                                          is.na(B14e) ~ "Missing"),
         ava_rejwhy_other3 = ifelse(B14f < 0, NA, B14f),
         na_ava_rejwhy_other3 = case_when(B14f >= 0 ~ "Reported",
                                          B14f == -888888 ~ "Not applicable",
                                          B14f == -999999 ~ "Not available",
                                          is.na(B14f) ~ "Missing"),
         ava_rejwhy_total_calc = ava_rejwhy_time + 
                                 ava_rejwhy_signature + 
                                 ava_rejwhy_postmark + 
                                 ava_rejwhy_other1 + 
                                 ava_rejwhy_other2 + 
                                 ava_rejwhy_other3)


#----------------------------------------
#   B15: rejected ballots from whom
#----------------------------------------

head(table(d$B15a, exclude = NULL))
head(table(d$B15b, exclude = NULL))
head(table(d$B15c, exclude = NULL))

table(is.na(d$B15a), exclude = NULL)
table(is.na(d$B15b), exclude = NULL)
table(is.na(d$B15c), exclude = NULL)

table(d$B15a %% 1, exclude = NULL)
table(d$B15b %% 1, exclude = NULL)
table(d$B15c %% 1, exclude = NULL)


d <- 
  mutate(d,
         ava_rejwho_unif = ifelse(B15a < 0, NA, B15a),
         na_ava_rejwho_unif = case_when(B15a >= 0 ~ "Reported",
                                        B15a == -888888 ~ "Not applicable",
                                        B15a == -999999 ~ "Not available",
                                        is.na(B15a) ~ "Missing"),
         ava_rejwho_civ = ifelse(B15b < 0, NA, B15b),
         na_ava_rejwho_civ = case_when(B15b >= 0 ~ "Reported",
                                       B15b == -888888 ~ "Not applicable",
                                       B15b == -999999 ~ "Not available",
                                       is.na(B15b) ~ "Missing"),
         ava_rejwho_other = ifelse(B15c < 0, NA, B15c),
         na_ava_rejwho_other = case_when(B15c >= 0 ~ "Reported",
                                         B15c == -888888 ~ "Not applicable",
                                         B15c == -999999 ~ "Not available",
                                         is.na(B15c) ~ "Missing"),
         ava_rejwho_total_calc = ava_rejwho_unif + 
                                 ava_rejwho_civ + ava_rejwho_other)



#----------------------------------------
#   B16: rejected absentee ballots from whom
#----------------------------------------

head(table(d$B16a, exclude = NULL))
head(table(d$B16b, exclude = NULL))
head(table(d$B16c, exclude = NULL))

table(is.na(d$B16a), exclude = NULL)
table(is.na(d$B16b), exclude = NULL)
table(is.na(d$B16c), exclude = NULL)

table(d$B16a %% 1, exclude = NULL)
table(d$B16b %% 1, exclude = NULL)
table(d$B16c %% 1, exclude = NULL)



d <- 
  mutate(d, 
         ava_rejabswho_unif = ifelse(B16a < 0, NA, B16a),
         na_ava_rejabswho_unif = case_when(B16a >= 0 ~ "Reported",
                                           B16a == -888888 ~ "Not applicable",
                                           B16a == -999999 ~ "Not available",
                                           is.na(B16a) ~ "Missing"),
         ava_rejabswho_civ = ifelse(B16b < 0, NA, B16b),
         na_ava_rejabswho_civ = case_when(B16b >= 0 ~ "Reported",
                                          B16b == -888888 ~ "Not applicable",
                                          B16b == -999999 ~ "Not available",
                                          is.na(B16b) ~ "Missing"),
         ava_rejabswho_other = ifelse(B16c < 0, NA, B16c),
         na_ava_rejabswho_other = case_when(B16c >= 0 ~ "Reported",
                                            B16c == -888888 ~ "Not applicable",
                                            B16c == -999999 ~ "Not available",
                                            is.na(B16c) ~ "Missing"),
         ava_rejabswho_total_calc = ava_rejabswho_unif + 
                                    ava_rejabswho_civ + 
                                    ava_rejabswho_other)


#----------------------------------------
#   B17: rejected fwabs from whom
#----------------------------------------

head(table(d$B17a, exclude = NULL))
head(table(d$B17b, exclude = NULL))
head(table(d$B17c, exclude = NULL))

table(is.na(d$B17a), exclude = NULL)
table(is.na(d$B17b), exclude = NULL)
table(is.na(d$B17c), exclude = NULL)

table(d$B17a %% 1, exclude = NULL)
table(d$B17b %% 1, exclude = NULL)
table(d$B17c %% 1, exclude = NULL)


d <- 
  mutate(d, 
         ava_rejfwabwho_unif = ifelse(B17a < 0, NA, B17a),
         na_ava_rejfwabwho_unif = case_when(B17a >= 0 ~ "Reported",
                                            B17a == -888888 ~ "Not applicable",
                                            B17a == -999999 ~ "Not available",
                                            is.na(B17a) ~ "Missing"),
         ava_rejfwabwho_civ = ifelse(B17b < 0, NA, B17b),
         na_ava_rejfwabwho_civ = case_when(B17b >= 0 ~ "Reported",
                                           B17b == -888888 ~ "Not applicable",
                                           B17b == -999999 ~ "Not available",
                                           is.na(B17b) ~ "Missing"),
         ava_rejfwabwho_other = ifelse(B17c < 0, NA, B17c),
         na_ava_rejfwabwho_other = case_when(B17c >= 0 ~ "Reported",
                                             B17c == -888888 ~ "Not applicable",
                                             B17c == -999999 ~ "Not available",
                                             is.na(B17c) ~ "Missing"),
         ava_rejfwabwho_total_calc = ava_rejfwabwho_unif + 
                                     ava_rejfwabwho_civ + 
                                     ava_rejfwabwho_other)



#----------------------------------------
#   B18: rejected 'other' ballots from whom
#----------------------------------------

head(table(d$B18a, exclude = NULL))
head(table(d$B18b, exclude = NULL))
head(table(d$B18c, exclude = NULL))

table(is.na(d$B18a), exclude = NULL)
table(is.na(d$B18b), exclude = NULL)
table(is.na(d$B18c), exclude = NULL)

table(d$B18a %% 1, exclude = NULL)
table(d$B18b %% 1, exclude = NULL)
table(d$B18c %% 1, exclude = NULL)


d <- 
  mutate(d, 
         ava_rejotherwho_unif = ifelse(B18a < 0, NA, B18a),
         na_ava_rejotherwho_unif = case_when(B18a >= 0 ~ "Reported",
                                             B18a == -888888 ~ "Not applicable",
                                             B18a == -999999 ~ "Not available",
                                             is.na(B18a) ~ "Missing"),
         ava_rejotherwho_civ = ifelse(B18b < 0, NA, B18b),
         na_ava_rejotherwho_civ = case_when(B18b >= 0 ~ "Reported",
                                            B18b == -888888 ~ "Not applicable",
                                            B18b == -999999 ~ "Not available",
                                            is.na(B18b) ~ "Missing"),
         ava_rejotherwho_other = ifelse(B18c < 0, NA, B18c),
         na_ava_rejotherwho_other = case_when(B18c >= 0 ~ "Reported",
                                              B18c == -888888 ~ "Not applicable",
                                              B18c == -999999 ~ "Not available",
                                              is.na(B18c) ~ "Missing"),
         ava_rejotherwho_total_calc = ava_rejotherwho_unif + 
                                      ava_rejotherwho_civ + 
                                      ava_rejotherwho_other)





# remove original variables and save
d %>%
select(-one_of(drop_names)) %>% 
print %>% 
saveRDS("data/clean/clean-EAVS-2016-B.RDS")

