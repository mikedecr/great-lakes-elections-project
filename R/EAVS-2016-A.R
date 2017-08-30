#----------------------------------------
#   EAVS 2016, section A
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
					select(FIPSCode:JurisdictionName, starts_with("A"))


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
#   A1--A3: Topline registration stats
#----------------------------------------

## A1a: reg_total_eligible

## A2: reg_includes
## A2c_Other: reg_includes_spec

## A3a: reg_active
## A3b: reg_inactive

#----------------------------------------
#   A4: sameday registration
#----------------------------------------

## A4a: reg_sameday_new
## A4b: reg_sameday_allow
## A4b_Other: reg_sameday_allow_spec

#----------------------------------------
#   A5: registration forms received
#----------------------------------------

## A5a: reg_forms_total
## A5b: reg_forms_newvalid
## A5c: reg_forms_prereg
## A5d: reg_forms_duplicate
## A5e: reg_forms_invrej
## A5f: reg_forms_minorchange
## A5g: reg_forms_newjuris
## A5h: reg_forms_other1
## A5i: reg_forms_other2
## A5j: reg_forms_other3
## A5k: reg_forms_other4
## A5l: reg_forms_other5

## create your own sum? reg_forms_total_calc


todo	<- todo + 1
beepr::beep(2)
# merge "Other" counts into other categories or aggregate in some way?
# Perhaps sum all "Other" into one "Other" category


#----------------------------------------
#   A6: registration submission type
#----------------------------------------

## A6a: reg_formsub_mail
## A6b: reg_formsub_inperson
## A6c: reg_formsub_online
## A6d: reg_formsub_dmv
## A6e: reg_formsub_nvra_office
## A6f: reg_formsub_state_disable
## A6g: reg_formsub_recruitment
## A6h: reg_formsub_other_non_nvra
## A6i: reg_formsub_regdrive
## A6j: reg_formsub_other1
## A6k: reg_formsub_other2
## A6l: reg_formsub_other3
## A6m: reg_formsub_other4
## A6n: reg_formsub_other5
## A6o: reg_formsub_other6
## calc own total:: reg_formsub_total (== { A5a,  reg_forms_total,  reg_forms_total_calc} )


#----------------------------------------
#   A7: new registration form type
#----------------------------------------

## A7a: reg_newform_mail
## A7b: reg_newform_inperson
## A7c: reg_newform_online
## A7d: reg_newform_dmv
## A7e: reg_newform_nvra_office
## A7f: reg_newform_state_disable
## A7g: reg_newform_recruitment
## A7h: reg_newform_other_non_nvra
## A7i: reg_newform_regdrive
## A7j: reg_newform_other1
## A7k: reg_newform_other2
## A7l: reg_newform_other3
## A7m: reg_newform_other4
## A7n: reg_newform_other5
## A7o: reg_newform_other6
## total: reg_newform_total ( == A5b, reg_forms_newvalid)



#----------------------------------------
#   A8: duplicate registration form type
#----------------------------------------

## A8a: reg_duplic_mail
## A8b: reg_duplic_inperson
## A8c: reg_duplic_online
## A8d: reg_duplic_dmv
## A8e: reg_duplic_nvra_office
## A8f: reg_duplic_state_disable
## A8g: reg_duplic_recruitment
## A8h: reg_duplic_other_non_nvra
## A8i: reg_duplic_regdrive
## A8j: reg_duplic_other1
## A8k: reg_duplic_other2
## A8l: reg_duplic_other3
## A8m: reg_duplic_other4
## A8n: reg_duplic_other5
## A8o: reg_duplic_other6
## total: reg_duplic_total ( == A5d, reg_forms_duplicate)





#----------------------------------------
#   A9: invalid/rejected registration form type
#----------------------------------------

## A9a: reg_invrej_mail
## A9b: reg_invrej_inperson
## A9c: reg_invrej_online
## A9d: reg_invrej_dmv
## A9e: reg_invrej_nvra_office
## A9f: reg_invrej_state_disable
## A9g: reg_invrej_recruitment
## A9h: reg_invrej_other_non_nvra
## A9i: reg_invrej_regdrive
## A9j: reg_invrej_other1
## A9k: reg_invrej_other2
## A9l: reg_invrej_other3
## A9m: reg_invrej_other4
## A9n: reg_invrej_other5
## A9o: reg_invrej_other6
## total: reg_invrej_total ( == A5e, reg_forms_invrej)


#----------------------------------------
#   A10: mailed confirmation notices sent
#----------------------------------------

## A10a: reg_notice_total
## A10b: reg_notice_confirmreg
## A10c: reg_notice_confirmnonreg
## A10d: reg_notice_undeliver
## A10e: reg_notice_unknown
## A10f: reg_notice_other1
## A10g: reg_notice_other2
## A10h: reg_notice_other3
## total: reg_notice_total



#----------------------------------------
#   A11: voters removed from rolls
#----------------------------------------

## A11a: reg_drop_total
## A11b: reg_drop_moved
## A11c: reg_drop_deceased
## A11d: reg_drop_felony
## A11e: reg_drop_inactive
## A11f: reg_drop_incompetent
## A11g: reg_drop_request
## A11h: reg_drop_other1
## A11i: reg_drop_other2
## A11j: reg_drop_other3
## A11k: reg_drop_other4
## A11_Total: reg_drop_total2 





#----------------------------------------
#   recode
#----------------------------------------




#----------------------------------------
#   A1--A3: Topline registration stats
#----------------------------------------


head(table(d$A1a, exclude = NULL))
table(is.na(d$A1a), exclude = NULL)

head(table(d$A2, exclude = NULL))
table(is.na(d$A2), exclude = NULL)

# who is reported, "other" codes
head(table(d$A2c_Other, exclude = NULL)) 

# all "None" are State == ME:
filter(d, State == "ME") %$% 
table(A2, A2c_Other, exclude = NULL)
d %$% table(A2, A2c_Other, exclude = NULL)

# and the only missing jurisdiction is also in Maine

head(table(d$A3a, exclude = NULL))
table(is.na(d$A3a), exclude = NULL)

head(table(d$A3b, exclude = NULL))
table(is.na(d$A3b), exclude = NULL)

table(d$A1a %% 1, exclude = NULL)
table(d$A3a %% 1, exclude = NULL)
table(d$A3b %% 1, exclude = NULL)



d <- mutate(d, 
            reg_total_eligible = ifelse(A1a < 0, NA, A1a), 
            na_reg_total_eligible = case_when(A1a >= 0 ~ "Reported", 
                                               A1a == -999999 ~ "Not available", 
                                               A1a == -888888 ~ "Not applicable", 
                                               is.na(A1a) ~ "Missing"), 
            reg_includes = case_when(A2 == 1 ~ "Active Only", 
                                     A2 == 2 ~ "Active and Inactive", 
                                     A2 == 3 ~ "Other", 
                                     is.na(A2) ~ "Missing"), 
            reg_includes_spec = ifelse(trimws(A2c_Other) == "", NA, A2c_Other), 
            reg_active = ifelse(A3a < 0, NA, A3a), 
            na_reg_active = case_when(A3a >= 0 ~ "Reported", 
                                      A3a == -888888 ~ "Not applicable", 
                                      A3a == -999999 ~ "Not available", 
                                      is.na(A3a) ~ "Missing"), 
            reg_inactive = ifelse(A3b < 0, NA, A3b), 
            na_reg_inactive = case_when(A3b >= 0 ~ "Reported", 
                                        A3b == -888888 ~ "Not applicable", 
                                        A3b == -999999 ~ "Not available", 
                                        is.na(A3b) ~ "Missing"))




#----------------------------------------
#   A4: sameday registration
#----------------------------------------

head(table(d$A4a, exclude = NULL))
table(is.na(d$A4a), exclude = NULL)

head(table(d$A4b, exclude = NULL))
table(is.na(d$A4b), exclude = NULL)

beepr::beep(2)
todo <- todo + 1
## there are zeroes here that don't have codes.
## and also a fudged NA code but that's straightforward to figure out

head(table(d$A4b_Other, exclude = NULL))
table(is.na(d$A4b_Other), exclude = NULL)



d <- 
  mutate(d,
         reg_sameday_new = ifelse(A4a < 0, NA, A4a),
         na_reg_sameday_new = case_when(A4a >= 0 ~ "Reported",
                                        A4a == -888888 ~ "Not applicable",
                                        A4a == -999999 ~ "Not available",
                                        is.na(A4a) ~ "Missing"),
         reg_sameday_allow = case_when(is.na(A4b) ~ "Missing",
                                       A4b == -888888 ~ "Not applicable",
                                       A4b == -88888 ~ "Not applicable",
                                       A4b == -999999 ~ "Not available",
                                       A4b == 0 ~ "UNKNOWN",
                                       A4b == 1 ~ "Allowed for all voters",
                                       A4b == 2 ~ "Allowed for some voters",
                                       A4b == 3 ~ "Other"),
         reg_sameday_allow_spec = A4b_Other)


todo <- todo + 1
beepr::beep(2)
# only 1 "other," a few with "0" but 0 isn't in the codebook.




#----------------------------------------
#   A5: registration forms received
#----------------------------------------


head(table(d$A5a, exclude = NULL))
head(table(d$A5b, exclude = NULL))
head(table(d$A5c, exclude = NULL))
head(table(d$A5d, exclude = NULL))
head(table(d$A5e, exclude = NULL))
head(table(d$A5f, exclude = NULL))
head(table(d$A5g, exclude = NULL))
head(table(d$A5h, exclude = NULL))
head(table(d$A5i, exclude = NULL))
head(table(d$A5j, exclude = NULL))
head(table(d$A5k, exclude = NULL))
head(table(d$A5l, exclude = NULL))

table(d$A5a %% 1, exclude = NULL)
table(d$A5b %% 1, exclude = NULL)
table(d$A5c %% 1, exclude = NULL)
table(d$A5d %% 1, exclude = NULL)
table(d$A5e %% 1, exclude = NULL)
table(d$A5f %% 1, exclude = NULL)
table(d$A5g %% 1, exclude = NULL)
table(d$A5h %% 1, exclude = NULL)
table(d$A5i %% 1, exclude = NULL)
table(d$A5j %% 1, exclude = NULL)
table(d$A5k %% 1, exclude = NULL)
table(d$A5l %% 1, exclude = NULL)

table(is.na(d$A5a), exclude = NULL)
table(is.na(d$A5b), exclude = NULL)
table(is.na(d$A5c), exclude = NULL)
table(is.na(d$A5d), exclude = NULL)
table(is.na(d$A5e), exclude = NULL)
table(is.na(d$A5f), exclude = NULL)
table(is.na(d$A5g), exclude = NULL)
table(is.na(d$A5h), exclude = NULL)
table(is.na(d$A5i), exclude = NULL)
table(is.na(d$A5j), exclude = NULL)
table(is.na(d$A5k), exclude = NULL)
table(is.na(d$A5l), exclude = NULL)



d <- 
  mutate(d, 
         reg_forms_total = ifelse(A5a < 0, NA, A5a),
         na_reg_forms_total = case_when(A5a >= 0 ~ "Reported",
                                        is.na(A5a) ~ "Missing",
                                        A5a == -888888 ~ "Not applicable",
                                        A5a == -999999 ~ "Not available"),
         reg_forms_newvalid = ifelse(A5b < 0, NA, A5b),
         na_reg_forms_newvalid = case_when(A5b >= 0 ~ "Reported",
                                           is.na(A5b) ~ "Missing",
                                           A5b == -888888 ~ "Not applicable",
                                           A5b == -999999 ~ "Not available"),
         reg_forms_prereg = ifelse(A5c < 0, NA, A5c),
         na_reg_forms_prereg = case_when(A5c >= 0 ~ "Reported",
                                         is.na(A5c) ~ "Missing",
                                         A5c == -888888 ~ "Not applicable",
                                         A5c == -999999 ~ "Not available"),
         reg_forms_duplicate = ifelse(A5d < 0, NA, A5d),
         na_reg_forms_duplicate = case_when(A5d >= 0 ~ "Reported",
                                            is.na(A5d) ~ "Missing",
                                            A5d == -888888 ~ "Not applicable",
                                            A5d == -999999 ~ "Not available"),
         reg_forms_invrej = ifelse(A5e < 0, NA, A5e),
         na_reg_forms_invrej = case_when(A5e >= 0 ~ "Reported",
                                         is.na(A5e) ~ "Missing",
                                         A5e == -888888 ~ "Not applicable",
                                         A5e == -999999 ~ "Not available"),
         reg_forms_minorchange = ifelse(A5f < 0, NA, A5f),
         na_reg_forms_minorchange = case_when(A5f >= 0 ~ "Reported",
                                              is.na(A5f) ~ "Missing",
                                              A5f == -888888 ~ "Not applicable",
                                              A5f == -999999 ~ "Not available"),
         reg_forms_newjuris = ifelse(A5g < 0, NA, A5g),
         na_reg_forms_newjuris = case_when(A5g >= 0 ~ "Reported",
                                           is.na(A5g) ~ "Missing",
                                           A5g == -888888 ~ "Not applicable",
                                           A5g == -999999 ~ "Not available"),
         reg_forms_other1 = ifelse(A5h < 0, NA, A5h),
         na_reg_forms_other1 = case_when(A5h >= 0 ~ "Reported",
                                         is.na(A5h) ~ "Missing",
                                         A5h == -888888 ~ "Not applicable",
                                         A5h == -999999 ~ "Not available"),
         reg_forms_other2 = ifelse(A5i < 0, NA, A5i),
         na_reg_forms_other2 = case_when(A5i >= 0 ~ "Reported",
                                         is.na(A5i) ~ "Missing",
                                         A5i == -888888 ~ "Not applicable",
                                         A5i == -999999 ~ "Not available"),
         reg_forms_other3 = ifelse(A5j < 0, NA, A5j),
         na_reg_forms_other3 = case_when(A5j >= 0 ~ "Reported",
                                         is.na(A5j) ~ "Missing",
                                         A5j == -888888 ~ "Not applicable",
                                         A5j == -999999 ~ "Not available"),
         reg_forms_other4 = ifelse(A5k < 0, NA, A5k),
         na_reg_forms_other4 = case_when(A5k >= 0 ~ "Reported",
                                         is.na(A5k) ~ "Missing",
                                         A5k == -888888 ~ "Not applicable",
                                         A5k == -999999 ~ "Not available"),
         reg_forms_other5 = ifelse(A5l < 0, NA, A5l),
         na_reg_forms_other5 = case_when(A5l >= 0 ~ "Reported",
                                         is.na(A5l) ~ "Missing",
                                         A5l == -888888 ~ "Not applicable",
                                         A5l == -999999 ~ "Not available"),
         reg_forms_total_calc = reg_forms_newvalid + reg_forms_prereg + 
                                reg_forms_duplicate + reg_forms_invrej + 
                                reg_forms_minorchange + reg_forms_newjuris + 
                                reg_forms_other1 + reg_forms_other2 +
                                reg_forms_other3 + reg_forms_other4 +
                                reg_forms_other5)

todo <- todo + 1
beepr::beep(2)
# calculated total is probably going to be saturated with NAs
# without some kind zero rule. what do?







#----------------------------------------
#   A6: registration submission type
#----------------------------------------

## A6a: reg_formsub_mail
## A6b: reg_formsub_inperson
## A6c: reg_formsub_online
## A6d: reg_formsub_dmv
## A6e: reg_formsub_nvra_office
## A6f: reg_formsub_state_disable
## A6g: reg_formsub_recruitment
## A6h: reg_formsub_other_non_nvra
## A6i: reg_formsub_regdrive
## A6j: reg_formsub_other1
## A6k: reg_formsub_other2
## A6l: reg_formsub_other3
## A6m: reg_formsub_other4
## A6n: reg_formsub_other5
## A6o: reg_formsub_other6


head(table(d$A6a, exclude = NULL))
head(table(d$A6b, exclude = NULL))
head(table(d$A6c, exclude = NULL))
head(table(d$A6d, exclude = NULL))
head(table(d$A6e, exclude = NULL))
head(table(d$A6f, exclude = NULL))
head(table(d$A6g, exclude = NULL))
head(table(d$A6h, exclude = NULL))
head(table(d$A6i, exclude = NULL))
head(table(d$A6j, exclude = NULL))
head(table(d$A6k, exclude = NULL))
head(table(d$A6l, exclude = NULL))
head(table(d$A6m, exclude = NULL))
head(table(d$A6n, exclude = NULL))
head(table(d$A6o, exclude = NULL))

table(is.na(d$A6a), exclude = NULL)
table(is.na(d$A6b), exclude = NULL)
table(is.na(d$A6c), exclude = NULL)
table(is.na(d$A6d), exclude = NULL)
table(is.na(d$A6e), exclude = NULL)
table(is.na(d$A6f), exclude = NULL)
table(is.na(d$A6g), exclude = NULL)
table(is.na(d$A6h), exclude = NULL)
table(is.na(d$A6i), exclude = NULL)
table(is.na(d$A6j), exclude = NULL)
table(is.na(d$A6k), exclude = NULL)
table(is.na(d$A6l), exclude = NULL)
table(is.na(d$A6m), exclude = NULL)
table(is.na(d$A6n), exclude = NULL)
table(is.na(d$A6o), exclude = NULL)

table(d$A6a %% 1, exclude = NULL)
table(d$A6b %% 1, exclude = NULL)
table(d$A6c %% 1, exclude = NULL)
table(d$A6d %% 1, exclude = NULL)
table(d$A6e %% 1, exclude = NULL)
table(d$A6f %% 1, exclude = NULL)
table(d$A6g %% 1, exclude = NULL)
table(d$A6h %% 1, exclude = NULL)
table(d$A6i %% 1, exclude = NULL)
table(d$A6j %% 1, exclude = NULL)
table(d$A6k %% 1, exclude = NULL)
table(d$A6l %% 1, exclude = NULL)
table(d$A6m %% 1, exclude = NULL)
table(d$A6n %% 1, exclude = NULL)
table(d$A6o %% 1, exclude = NULL)



d <- 
  mutate(d, 
         reg_formsub_mail = ifelse(A6a < 0, NA, A6a),
         na_reg_formsub_mail = case_when(A6a >= 0 ~ "Reported",
                                         A6a == -888888 ~ "Not applicable",
                                         A6a == -999999 ~ "Not available",
                                         is.na(A6a) ~ "Missing"),
         reg_formsub_inperson = ifelse(A6b < 0, NA, A6b),
         na_reg_formsub_inperson = case_when(A6b >= 0 ~ "Reported",
                                             A6b == -888888 ~ "Not applicable",
                                             A6b == -999999 ~ "Not available",
                                             is.na(A6b) ~ "Missing"),
         reg_formsub_online = ifelse(A6c < 0, NA, A6c),
         na_reg_formsub_online = case_when(A6c >= 0 ~ "Reported",
                                           A6c == -888888 ~ "Not applicable",
                                           A6c == -999999 ~ "Not available",
                                           is.na(A6c) ~ "Missing"),
         reg_formsub_dmv = ifelse(A6d < 0, NA, A6d),
         na_reg_formsub_dmv = case_when(A6d >= 0 ~ "Reported",
                                        A6d == -888888 ~ "Not applicable",
                                        A6d == -999999 ~ "Not available",
                                        is.na(A6d) ~ "Missing"),
         reg_formsub_nvra_office = ifelse(A6e < 0, NA, A6e),
         na_reg_formsub_nvra_office = case_when(A6e >= 0 ~ "Reported",
                                                A6e == -888888 ~ "Not applicable",
                                                A6e == -999999 ~ "Not available",
                                                is.na(A6e) ~ "Missing"),
         reg_formsub_state_disable = ifelse(A6f < 0, NA, A6f),
         na_reg_formsub_state_disable = case_when(A6f >= 0 ~ "Reported",
                                                  A6f == -888888 ~ "Not applicable",
                                                  A6f == -999999 ~ "Not available",
                                                  is.na(A6f) ~ "Missing"),
         reg_formsub_recruitment = ifelse(A6g < 0, NA, A6g),
         na_reg_formsub_recruitment = case_when(A6g >= 0 ~ "Reported",
                                                A6g == -888888 ~ "Not applicable",
                                                A6g == -999999 ~ "Not available",
                                                is.na(A6g) ~ "Missing"),
         reg_formsub_other_non_nvra = ifelse(A6h < 0, NA, A6h),
         na_reg_formsub_other_non_nvra = case_when(A6h >= 0 ~ "Reported",
                                                   A6h == -888888 ~ "Not applicable",
                                                   A6h == -999999 ~ "Not available",
                                                   is.na(A6h) ~ "Missing"),
         reg_formsub_regdrive = ifelse(A6i < 0, NA, A6i),
         na_reg_formsub_regdrive = case_when(A6i >= 0 ~ "Reported",
                                             A6i == -888888 ~ "Not applicable",
                                             A6i == -999999 ~ "Not available",
                                             is.na(A6i) ~ "Missing"),
         reg_formsub_other1 = ifelse(A6j < 0, NA, A6j),
         na_reg_formsub_other1 = case_when(A6j >= 0 ~ "Reported",
                                           A6j == -888888 ~ "Not applicable",
                                           A6j == -999999 ~ "Not available",
                                           is.na(A6j) ~ "Missing"),
         reg_formsub_other2 = ifelse(A6k < 0, NA, A6k),
         na_reg_formsub_other2 = case_when(A6k >= 0 ~ "Reported",
                                           A6k == -888888 ~ "Not applicable",
                                           A6k == -999999 ~ "Not available",
                                           is.na(A6k) ~ "Missing"),
         reg_formsub_other3 = ifelse(A6l < 0, NA, A6l),
         na_reg_formsub_other3 = case_when(A6l >= 0 ~ "Reported",
                                           A6l == -888888 ~ "Not applicable",
                                           A6l == -999999 ~ "Not available",
                                           is.na(A6l) ~ "Missing"),
         reg_formsub_other4 = ifelse(A6m < 0, NA, A6m),
         na_reg_formsub_other4 = case_when(A6m >= 0 ~ "Reported",
                                           A6m == -888888 ~ "Not applicable",
                                           A6m == -999999 ~ "Not available",
                                           is.na(A6m) ~ "Missing"),
         reg_formsub_other5 = ifelse(A6n < 0, NA, A6n),
         na_reg_formsub_other5 = case_when(A6n >= 0 ~ "Reported",
                                           A6n == -888888 ~ "Not applicable",
                                           A6n == -999999 ~ "Not available",
                                           is.na(A6n) ~ "Missing"),
         reg_formsub_other6 = ifelse(A6o < 0, NA, A6o),
         na_reg_formsub_other6 = case_when(A6o >= 0 ~ "Reported",
                                           A6o == -888888 ~ "Not applicable",
                                           A6o == -999999 ~ "Not available",
                                           is.na(A6o) ~ "Missing"),
         reg_formsub_total = reg_formsub_mail + reg_formsub_inperson + 
                             reg_formsub_online + reg_formsub_dmv + 
                             reg_formsub_nvra_office + reg_formsub_state_disable +
                             reg_formsub_recruitment + reg_formsub_other_non_nvra +
                             reg_formsub_regdrive + reg_formsub_other1 +
                             reg_formsub_other2 + reg_formsub_other3 + 
                             reg_formsub_other4 + reg_formsub_other5 + 
                             reg_formsub_other6)







#----------------------------------------
#   A7: new registration form type
#----------------------------------------

head(table(d$A7a, exclude = NULL))
head(table(d$A7b, exclude = NULL))
head(table(d$A7c, exclude = NULL))
head(table(d$A7d, exclude = NULL))
head(table(d$A7e, exclude = NULL))
head(table(d$A7f, exclude = NULL))
head(table(d$A7g, exclude = NULL))
head(table(d$A7h, exclude = NULL))
head(table(d$A7i, exclude = NULL))
head(table(d$A7j, exclude = NULL))
head(table(d$A7k, exclude = NULL))
head(table(d$A7l, exclude = NULL))
head(table(d$A7m, exclude = NULL))
head(table(d$A7n, exclude = NULL))
head(table(d$A7o, exclude = NULL))

table(is.na(d$A7a), exclude = NULL)
table(is.na(d$A7b), exclude = NULL)
table(is.na(d$A7c), exclude = NULL)
table(is.na(d$A7d), exclude = NULL)
table(is.na(d$A7e), exclude = NULL)
table(is.na(d$A7f), exclude = NULL)
table(is.na(d$A7g), exclude = NULL)
table(is.na(d$A7h), exclude = NULL)
table(is.na(d$A7i), exclude = NULL)
table(is.na(d$A7j), exclude = NULL)
table(is.na(d$A7k), exclude = NULL)
table(is.na(d$A7l), exclude = NULL)
table(is.na(d$A7m), exclude = NULL)
table(is.na(d$A7n), exclude = NULL)
table(is.na(d$A7o), exclude = NULL)

table(d$A7a %% 1, exclude = NULL)
table(d$A7b %% 1, exclude = NULL)
table(d$A7c %% 1, exclude = NULL)
table(d$A7d %% 1, exclude = NULL)
table(d$A7e %% 1, exclude = NULL)
table(d$A7f %% 1, exclude = NULL)
table(d$A7g %% 1, exclude = NULL)
table(d$A7h %% 1, exclude = NULL)
table(d$A7i %% 1, exclude = NULL)
table(d$A7j %% 1, exclude = NULL)
table(d$A7k %% 1, exclude = NULL)
table(d$A7l %% 1, exclude = NULL)
table(d$A7m %% 1, exclude = NULL)
table(d$A7n %% 1, exclude = NULL)
table(d$A7o %% 1, exclude = NULL)


d <- 
  mutate(d, 
         reg_newform_mail = ifelse(A7a < 0, NA, A7a),
         na_reg_newform_mail = case_when(A7a >= 0 ~ "Reported",
                                         A7a == -888888 ~ "Not applicable",
                                         A7a == -999999 ~ "Not available",
                                         is.na(A7a) ~ "Missing"),
         reg_newform_inperson = ifelse(A7b < 0, NA, A7b),
         na_reg_newform_inperson = case_when(A7b >= 0 ~ "Reported",
                                             A7b == -888888 ~ "Not applicable",
                                             A7b == -999999 ~ "Not available",
                                             is.na(A7b) ~ "Missing"),
         reg_newform_online = ifelse(A7c < 0, NA, A7c),
         na_reg_newform_online = case_when(A7c >= 0 ~ "Reported",
                                           A7c == -888888 ~ "Not applicable",
                                           A7c == -999999 ~ "Not available",
                                           is.na(A7c) ~ "Missing"),
         reg_newform_dmv = ifelse(A7d < 0, NA, A7d),
         na_reg_newform_dmv = case_when(A7d >= 0 ~ "Reported",
                                        A7d == -888888 ~ "Not applicable",
                                        A7d == -999999 ~ "Not available",
                                        is.na(A7d) ~ "Missing"),
         reg_newform_nvra_office = ifelse(A7e < 0, NA, A7e),
         na_reg_newform_nvra_office = case_when(A7e >= 0 ~ "Reported",
                                                A7e == -888888 ~ "Not applicable",
                                                A7e == -999999 ~ "Not available",
                                                is.na(A7e) ~ "Missing"),
         reg_newform_state_disable = ifelse(A7f < 0, NA, A7f),
         na_reg_newform_state_disable = case_when(A7f >= 0 ~ "Reported",
                                                  A7f == -888888 ~ "Not applicable",
                                                  A7f == -999999 ~ "Not available",
                                                  is.na(A7f) ~ "Missing"),
         reg_newform_recruitment = ifelse(A7g < 0, NA, A7g),
         na_reg_newform_recruitment = case_when(A7g >= 0 ~ "Reported",
                                                A7g == -888888 ~ "Not applicable",
                                                A7g == -999999 ~ "Not available",
                                                is.na(A7g) ~ "Missing"),
         reg_newform_other_non_nvra = ifelse(A7h < 0, NA, A7h),
         na_reg_newform_other_non_nvra = case_when(A7h >= 0 ~ "Reported",
                                                   A7h == -888888 ~ "Not applicable",
                                                   A7h == -999999 ~ "Not available",
                                                   is.na(A7h) ~ "Missing"),
         reg_newform_regdrive = ifelse(A7i < 0, NA, A7i),
         na_reg_newform_regdrive = case_when(A7i >= 0 ~ "Reported",
                                             A7i == -888888 ~ "Not applicable",
                                             A7i == -999999 ~ "Not available",
                                             is.na(A7i) ~ "Missing"),
         reg_newform_other1 = ifelse(A7j < 0, NA, A7j),
         na_reg_newform_other1 = case_when(A7j >= 0 ~ "Reported",
                                           A7j == -888888 ~ "Not applicable",
                                           A7j == -999999 ~ "Not available",
                                           is.na(A7j) ~ "Missing"),
         reg_newform_other2 = ifelse(A7k < 0, NA, A7k),
         na_reg_newform_other2 = case_when(A7k >= 0 ~ "Reported",
                                           A7k == -888888 ~ "Not applicable",
                                           A7k == -999999 ~ "Not available",
                                           is.na(A7k) ~ "Missing"),
         reg_newform_other3 = ifelse(A7l < 0, NA, A7l),
         na_reg_newform_other3 = case_when(A7l >= 0 ~ "Reported",
                                           A7l == -888888 ~ "Not applicable",
                                           A7l == -999999 ~ "Not available",
                                           is.na(A7l) ~ "Missing"),
         reg_newform_other4 = ifelse(A7m < 0, NA, A7m),
         na_reg_newform_other4 = case_when(A7m >= 0 ~ "Reported",
                                           A7m == -888888 ~ "Not applicable",
                                           A7m == -999999 ~ "Not available",
                                           is.na(A7m) ~ "Missing"),
         reg_newform_other5 = ifelse(A7n < 0, NA, A7n),
         na_reg_newform_other5 = case_when(A7n >= 0 ~ "Reported",
                                           A7n == -888888 ~ "Not applicable",
                                           A7n == -999999 ~ "Not available",
                                           is.na(A7n) ~ "Missing"),
         reg_newform_other6 = ifelse(A7o < 0, NA, A7o),
         na_reg_newform_other6 = case_when(A7o >= 0 ~ "Reported",
                                           A7o == -888888 ~ "Not applicable",
                                           A7o == -999999 ~ "Not available",
                                           is.na(A7o) ~ "Missing"),
         reg_newform_total = reg_newform_mail + reg_newform_inperson + 
                             reg_newform_online + reg_newform_dmv + 
                             reg_newform_nvra_office + reg_newform_state_disable + 
                             reg_newform_recruitment + reg_newform_other_non_nvra +
                             reg_newform_regdrive + reg_newform_other1 +
                             reg_newform_other2 + reg_newform_other3 + 
                             reg_newform_other4 + reg_newform_other5 + reg_newform_other6)
         
         






#----------------------------------------
#   A8: duplicate registration form type
#----------------------------------------

head(table(d$A8a, exclude = NULL))
head(table(d$A8b, exclude = NULL))
head(table(d$A8c, exclude = NULL))
head(table(d$A8d, exclude = NULL))
head(table(d$A8e, exclude = NULL))
head(table(d$A8f, exclude = NULL))
head(table(d$A8g, exclude = NULL))
head(table(d$A8h, exclude = NULL))
head(table(d$A8i, exclude = NULL))
head(table(d$A8j, exclude = NULL))
head(table(d$A8k, exclude = NULL))
head(table(d$A8l, exclude = NULL))
head(table(d$A8m, exclude = NULL))
head(table(d$A8n, exclude = NULL))
head(table(d$A8o, exclude = NULL))

table(is.na(d$A8a), exclude = NULL)
table(is.na(d$A8b), exclude = NULL)
table(is.na(d$A8c), exclude = NULL)
table(is.na(d$A8d), exclude = NULL)
table(is.na(d$A8e), exclude = NULL)
table(is.na(d$A8f), exclude = NULL)
table(is.na(d$A8g), exclude = NULL)
table(is.na(d$A8h), exclude = NULL)
table(is.na(d$A8i), exclude = NULL)
table(is.na(d$A8j), exclude = NULL)
table(is.na(d$A8k), exclude = NULL)
table(is.na(d$A8l), exclude = NULL)
table(is.na(d$A8m), exclude = NULL)
table(is.na(d$A8n), exclude = NULL)
table(is.na(d$A8o), exclude = NULL)

table(d$A8a %% 1, exclude = NULL)
table(d$A8b %% 1, exclude = NULL)
table(d$A8c %% 1, exclude = NULL)
table(d$A8d %% 1, exclude = NULL)
table(d$A8e %% 1, exclude = NULL)
table(d$A8f %% 1, exclude = NULL)
table(d$A8g %% 1, exclude = NULL)
table(d$A8h %% 1, exclude = NULL)
table(d$A8i %% 1, exclude = NULL)
table(d$A8j %% 1, exclude = NULL)
table(d$A8k %% 1, exclude = NULL)
table(d$A8l %% 1, exclude = NULL)
table(d$A8m %% 1, exclude = NULL)
table(d$A8n %% 1, exclude = NULL)
table(d$A8o %% 1, exclude = NULL)




d <- 
  mutate(d,
         reg_duplic_mail = ifelse(A8a < 0, NA, A8a),
         na_reg_duplic_mail = case_when(A8a >= 0 ~ "Reported",
                                        A8a == -888888 ~ "Not applicable",
                                        A8a == -999999 ~ "Not available",
                                        is.na(A8a) ~ "Missing"),
         reg_duplic_inperson = ifelse(A8b < 0, NA, A8b),
         na_reg_duplic_inperson = case_when(A8b >= 0 ~ "Reported",
                                            A8b == -888888 ~ "Not applicable",
                                            A8b == -999999 ~ "Not available",
                                            is.na(A8b) ~ "Missing"),
         reg_duplic_online = ifelse(A8c < 0, NA, A8c),
         na_reg_duplic_online = case_when(A8c >= 0 ~ "Reported",
                                          A8c == -888888 ~ "Not applicable",
                                          A8c == -999999 ~ "Not available",
                                          is.na(A8c) ~ "Missing"),
         reg_duplic_dmv = ifelse(A8d < 0, NA, A8d),
         na_reg_duplic_dmv = case_when(A8d >= 0 ~ "Reported",
                                       A8d == -888888 ~ "Not applicable",
                                       A8d == -999999 ~ "Not available",
                                       is.na(A8d) ~ "Missing"),
         reg_duplic_nvra_office = ifelse(A8e < 0, NA, A8e),
         na_reg_duplic_nvra_office = case_when(A8e >= 0 ~ "Reported",
                                               A8e == -888888 ~ "Not applicable",
                                               A8e == -999999 ~ "Not available",
                                               is.na(A8e) ~ "Missing"),
         reg_duplic_state_disable = ifelse(A8f < 0, NA, A8f),
         na_reg_duplic_state_disable = case_when(A8f >= 0 ~ "Reported",
                                                 A8f == -888888 ~ "Not applicable",
                                                 A8f == -999999 ~ "Not available",
                                                 is.na(A8f) ~ "Missing"),
         reg_duplic_recruitment = ifelse(A8g < 0, NA, A8g),
         na_reg_duplic_recruitment = case_when(A8g >= 0 ~ "Reported",
                                               A8g == -888888 ~ "Not applicable",
                                               A8g == -999999 ~ "Not available",
                                               is.na(A8g) ~ "Missing"),
         reg_duplic_other_non_nvra = ifelse(A8h < 0, NA, A8h),
         na_reg_duplic_other_non_nvra = case_when(A8h >= 0 ~ "Reported",
                                                  A8h == -888888 ~ "Not applicable",
                                                  A8h == -999999 ~ "Not available",
                                                  is.na(A8h) ~ "Missing"),
         reg_duplic_regdrive = ifelse(A8i < 0, NA, A8i),
         na_reg_duplic_regdrive = case_when(A8i >= 0 ~ "Reported",
                                            A8i == -888888 ~ "Not applicable",
                                            A8i == -999999 ~ "Not available",
                                            is.na(A8i) ~ "Missing"),
         reg_duplic_other1 = ifelse(A8j < 0, NA, A8j),
         na_reg_duplic_other1 = case_when(A8j >= 0 ~ "Reported",
                                          A8j == -888888 ~ "Not applicable",
                                          A8j == -999999 ~ "Not available",
                                          is.na(A8j) ~ "Missing"),
         reg_duplic_other2 = ifelse(A8k < 0, NA, A8k),
         na_reg_duplic_other2 = case_when(A8k >= 0 ~ "Reported",
                                          A8k == -888888 ~ "Not applicable",
                                          A8k == -999999 ~ "Not available",
                                          is.na(A8k) ~ "Missing"),
         reg_duplic_other3 = ifelse(A8l < 0, NA, A8l),
         na_reg_duplic_other3 = case_when(A8l >= 0 ~ "Reported",
                                          A8l == -888888 ~ "Not applicable",
                                          A8l == -999999 ~ "Not available",
                                          is.na(A8l) ~ "Missing"),
         reg_duplic_other4 = ifelse(A8m < 0, NA, A8m),
         na_reg_duplic_other4 = case_when(A8m >= 0 ~ "Reported",
                                          A8m == -888888 ~ "Not applicable",
                                          A8m == -999999 ~ "Not available",
                                          is.na(A8m) ~ "Missing"),
         reg_duplic_other5 = ifelse(A8n < 0, NA, A8n),
         na_reg_duplic_other5 = case_when(A8n >= 0 ~ "Reported",
                                          A8n == -888888 ~ "Not applicable",
                                          A8n == -999999 ~ "Not available",
                                          is.na(A8n) ~ "Missing"),
         reg_duplic_other6 = ifelse(A8o < 0, NA, A8o),
         na_reg_duplic_other6 = case_when(A8o >= 0 ~ "Reported",
                                          A8o == -888888 ~ "Not applicable",
                                          A8o == -999999 ~ "Not available",
                                          is.na(A8o) ~ "Missing"),
         reg_ruplic_total = reg_duplic_mail + reg_duplic_inperson + 
                            reg_duplic_online + reg_duplic_dmv + 
                            reg_duplic_nvra_office + reg_duplic_state_disable + 
                            reg_duplic_recruitment + reg_duplic_other_non_nvra + 
                            reg_duplic_regdrive + reg_duplic_other1 + 
                            reg_duplic_other2 + reg_duplic_other3 + 
                            reg_duplic_other4 + reg_duplic_other5 + reg_duplic_other6)




#----------------------------------------
#   A9: invalid/rejected registration form type
#----------------------------------------

head(table(d$A9a, exclude = NULL))
head(table(d$A9b, exclude = NULL))
head(table(d$A9c, exclude = NULL))
head(table(d$A9d, exclude = NULL))
head(table(d$A9e, exclude = NULL))
head(table(d$A9f, exclude = NULL))
head(table(d$A9g, exclude = NULL))
head(table(d$A9h, exclude = NULL))
head(table(d$A9i, exclude = NULL))
head(table(d$A9j, exclude = NULL))
head(table(d$A9k, exclude = NULL))
head(table(d$A9l, exclude = NULL))
head(table(d$A9m, exclude = NULL))
head(table(d$A9n, exclude = NULL))
head(table(d$A9o, exclude = NULL))

table(is.na(d$A9a), exclude = NULL)
table(is.na(d$A9b), exclude = NULL)
table(is.na(d$A9c), exclude = NULL)
table(is.na(d$A9d), exclude = NULL)
table(is.na(d$A9e), exclude = NULL)
table(is.na(d$A9f), exclude = NULL)
table(is.na(d$A9g), exclude = NULL)
table(is.na(d$A9h), exclude = NULL)
table(is.na(d$A9i), exclude = NULL)
table(is.na(d$A9j), exclude = NULL)
table(is.na(d$A9k), exclude = NULL)
table(is.na(d$A9l), exclude = NULL)
table(is.na(d$A9m), exclude = NULL)
table(is.na(d$A9n), exclude = NULL)
table(is.na(d$A9o), exclude = NULL)

table(d$A9a %% 1, exclude = NULL)
table(d$A9b %% 1, exclude = NULL)
table(d$A9c %% 1, exclude = NULL)
table(d$A9d %% 1, exclude = NULL)
table(d$A9e %% 1, exclude = NULL)
table(d$A9f %% 1, exclude = NULL)
table(d$A9g %% 1, exclude = NULL)
table(d$A9h %% 1, exclude = NULL)
table(d$A9i %% 1, exclude = NULL)
table(d$A9j %% 1, exclude = NULL)
table(d$A9k %% 1, exclude = NULL)
table(d$A9l %% 1, exclude = NULL)
table(d$A9m %% 1, exclude = NULL)
table(d$A9n %% 1, exclude = NULL)
table(d$A9o %% 1, exclude = NULL)


d <- 
  mutate(d,
         reg_invrej_mail = ifelse(A9a < 0, NA, A9a),
         na_reg_invrej_mail = case_when(A9a >= 0 ~ "Reported",
                                        A9a == -888888 ~ "Not applicable",
                                        A9a == -999999 ~ "Not available",
                                        is.na(A9a) ~ "Missing"),
         reg_invrej_inperson = ifelse(A9b < 0, NA, A9b),
         na_reg_invrej_inperson = case_when(A9b >= 0 ~ "Reported",
                                            A9b == -888888 ~ "Not applicable",
                                            A9b == -999999 ~ "Not available",
                                            is.na(A9b) ~ "Missing"),
         reg_invrej_online = ifelse(A9c < 0, NA, A9c),
         na_reg_invrej_online = case_when(A9c >= 0 ~ "Reported",
                                          A9c == -888888 ~ "Not applicable",
                                          A9c == -999999 ~ "Not available",
                                          is.na(A9c) ~ "Missing"),
         reg_invrej_dmv = ifelse(A9d < 0, NA, A9d),
         na_reg_invrej_dmv = case_when(A9d >= 0 ~ "Reported",
                                       A9d == -888888 ~ "Not applicable",
                                       A9d == -999999 ~ "Not available",
                                       is.na(A9d) ~ "Missing"),
         reg_invrej_nvra_office = ifelse(A9e < 0, NA, A9e),
         na_reg_invrej_nvra_office = case_when(A9e >= 0 ~ "Reported",
                                               A9e == -888888 ~ "Not applicable",
                                               A9e == -999999 ~ "Not available",
                                               is.na(A9e) ~ "Missing"),
         reg_invrej_state_disable = ifelse(A9f < 0, NA, A9f),
         na_reg_invrej_state_disable = case_when(A9f >= 0 ~ "Reported",
                                                 A9f == -888888 ~ "Not applicable",
                                                 A9f == -999999 ~ "Not available",
                                                 is.na(A9f) ~ "Missing"),
         reg_invrej_recruitment = ifelse(A9g < 0, NA, A9g),
         na_reg_invrej_recruitment = case_when(A9g >= 0 ~ "Reported",
                                               A9g == -888888 ~ "Not applicable",
                                               A9g == -999999 ~ "Not available",
                                               is.na(A9g) ~ "Missing"),
         reg_invrej_other_non_nvra = ifelse(A9h < 0, NA, A9h),
         na_reg_invrej_other_non_nvra = case_when(A9h >= 0 ~ "Reported",
                                                  A9h == -888888 ~ "Not applicable",
                                                  A9h == -999999 ~ "Not available",
                                                  is.na(A9h) ~ "Missing"),
         reg_invrej_regdrive = ifelse(A9i < 0, NA, A9i),
         na_reg_invrej_regdrive = case_when(A9i >= 0 ~ "Reported",
                                            A9i == -888888 ~ "Not applicable",
                                            A9i == -999999 ~ "Not available",
                                            is.na(A9i) ~ "Missing"),
         reg_invrej_other1 = ifelse(A9j < 0, NA, A9j),
         na_reg_invrej_other1 = case_when(A9j >= 0 ~ "Reported",
                                          A9j == -888888 ~ "Not applicable",
                                          A9j == -999999 ~ "Not available",
                                          is.na(A9j) ~ "Missing"),
         reg_invrej_other2 = ifelse(A9k < 0, NA, A9k),
         na_reg_invrej_other2 = case_when(A9k >= 0 ~ "Reported",
                                          A9k == -888888 ~ "Not applicable",
                                          A9k == -999999 ~ "Not available",
                                          is.na(A9k) ~ "Missing"),
         reg_invrej_other3 = ifelse(A9l < 0, NA, A9l),
         na_reg_invrej_other3 = case_when(A9l >= 0 ~ "Reported",
                                          A9l == -888888 ~ "Not applicable",
                                          A9l == -999999 ~ "Not available",
                                          is.na(A9l) ~ "Missing"),
         reg_invrej_other4 = ifelse(A9m < 0, NA, A9m),
         na_reg_invrej_other4 = case_when(A9m >= 0 ~ "Reported",
                                          A9m == -888888 ~ "Not applicable",
                                          A9m == -999999 ~ "Not available",
                                          is.na(A9m) ~ "Missing"),
         reg_invrej_other5 = ifelse(A9n < 0, NA, A9n),
         na_reg_invrej_other5 = case_when(A9n >= 0 ~ "Reported",
                                          A9n == -888888 ~ "Not applicable",
                                          A9n == -999999 ~ "Not available",
                                          is.na(A9n) ~ "Missing"),
         reg_invrej_other6 = ifelse(A9o < 0, NA, A9o),
         na_reg_invrej_other6 = case_when(A9o >= 0 ~ "Reported",
                                          A9o == -888888 ~ "Not applicable",
                                          A9o == -999999 ~ "Not available",
                                          is.na(A9o) ~ "Missing"),
         reg_invrej_total = reg_invrej_mail + reg_invrej_inperson + 
                            reg_invrej_online + reg_invrej_dmv + 
                            reg_invrej_nvra_office + reg_invrej_state_disable + 
                            reg_invrej_recruitment + reg_invrej_other_non_nvra + 
                            reg_invrej_regdrive + reg_invrej_other1 + 
                            reg_invrej_other2 + reg_invrej_other3 + 
                            reg_invrej_other4 + reg_invrej_other5 + reg_invrej_other6)




#----------------------------------------
#   A10: mailed confirmation notices sent
#----------------------------------------

head(table(d$A10a, exclude = NULL))
head(table(d$A10b, exclude = NULL))
head(table(d$A10c, exclude = NULL))
head(table(d$A10d, exclude = NULL))
head(table(d$A10e, exclude = NULL))
head(table(d$A10f, exclude = NULL))
head(table(d$A10g, exclude = NULL))
head(table(d$A10h, exclude = NULL))

table(is.na(d$A10a), exclude = NULL)
table(is.na(d$A10b), exclude = NULL)
table(is.na(d$A10c), exclude = NULL)
table(is.na(d$A10d), exclude = NULL)
table(is.na(d$A10e), exclude = NULL)
table(is.na(d$A10f), exclude = NULL)
table(is.na(d$A10g), exclude = NULL)
table(is.na(d$A10h), exclude = NULL)


table(d$A10a %% 1, exclude = NULL)
table(d$A10b %% 1, exclude = NULL)
table(d$A10c %% 1, exclude = NULL)
table(d$A10d %% 1, exclude = NULL)
table(d$A10e %% 1, exclude = NULL)
table(d$A10f %% 1, exclude = NULL)
table(d$A10g %% 1, exclude = NULL)
table(d$A10h %% 1, exclude = NULL)


d <- 
  mutate(d, 
         reg_notice_total = ifelse(A10a < 0, NA, A10a),
         na_reg_notice_total = case_when(A10a >= 0 ~ "Reported",
                                         A10a == -888888 ~ "Not applicable",
                                         A10a == -999999 ~ "Not available",
                                         is.na(A10a) ~ "Missing"),
         reg_notice_confirmreg = ifelse(A10b < 0, NA, A10b),
         na_reg_notice_confirmreg = case_when(A10b >= 0 ~ "Reported",
                                              A10b == -888888 ~ "Not applicable",
                                              A10b == -999999 ~ "Not available",
                                              is.na(A10b) ~ "Missing"),
         reg_notice_confirmnonreg = ifelse(A10c < 0, NA, A10c),
         na_reg_notice_confirmnonreg = case_when(A10c >= 0 ~ "Reported",
                                                 A10c == -888888 ~ "Not applicable",
                                                 A10c == -999999 ~ "Not available",
                                                 is.na(A10c) ~ "Missing"),
         reg_notice_undeliver = ifelse(A10d < 0, NA, A10d),
         na_reg_notice_undeliver = case_when(A10d >= 0 ~ "Reported",
                                             A10d == -888888 ~ "Not applicable",
                                             A10d == -999999 ~ "Not available",
                                             is.na(A10d) ~ "Missing"),
         reg_notice_unknown = ifelse(A10e < 0, NA, A10e),
         na_reg_notice_unknown = case_when(A10e >= 0 ~ "Reported",
                                           A10e == -888888 ~ "Not applicable",
                                           A10e == -999999 ~ "Not available",
                                           is.na(A10e) ~ "Missing"),
         reg_notice_other1 = ifelse(A10f < 0, NA, A10f),
         na_reg_notice_other1 = case_when(A10f >= 0 ~ "Reported",
                                          A10f == -888888 ~ "Not applicable",
                                          A10f == -999999 ~ "Not available",
                                          is.na(A10f) ~ "Missing"),
         reg_notice_other2 = ifelse(A10g < 0, NA, A10g),
         na_reg_notice_other2 = case_when(A10g >= 0 ~ "Reported",
                                          A10g == -888888 ~ "Not applicable",
                                          A10g == -999999 ~ "Not available",
                                          is.na(A10g) ~ "Missing"),
         reg_notice_other3 = ifelse(A10h < 0, NA, A10h),
         na_reg_notice_other3 = case_when(A10h >= 0 ~ "Reported",
                                          A10h == -888888 ~ "Not applicable",
                                          A10h == -999999 ~ "Not available",
                                          is.na(A10h) ~ "Missing"),
         reg_notice_total_calc = reg_notice_confirmreg + 
                                 reg_notice_confirmnonreg +
                                 reg_notice_undeliver +
                                 reg_notice_unknown +
                                 reg_notice_other1 + 
                                 reg_notice_other2 +
                                 reg_notice_other3)



#----------------------------------------
#   A11: voters removed from rolls
#----------------------------------------


head(table(d$A11a, exclude = NULL))
head(table(d$A11b, exclude = NULL))
head(table(d$A11c, exclude = NULL))
head(table(d$A11d, exclude = NULL))
head(table(d$A11e, exclude = NULL))
head(table(d$A11f, exclude = NULL))
head(table(d$A11g, exclude = NULL))
head(table(d$A11h, exclude = NULL))
head(table(d$A11i, exclude = NULL))
head(table(d$A11j, exclude = NULL))
head(table(d$A11k, exclude = NULL))

table(is.na(d$A11a), exclude = NULL)
table(is.na(d$A11b), exclude = NULL)
table(is.na(d$A11c), exclude = NULL)
table(is.na(d$A11d), exclude = NULL)
table(is.na(d$A11e), exclude = NULL)
table(is.na(d$A11f), exclude = NULL)
table(is.na(d$A11g), exclude = NULL)
table(is.na(d$A11h), exclude = NULL)
table(is.na(d$A11i), exclude = NULL)
table(is.na(d$A11j), exclude = NULL)
table(is.na(d$A11k), exclude = NULL)

table(d$A11a %% 1, exclude = NULL)
table(d$A11b %% 1, exclude = NULL)
table(d$A11c %% 1, exclude = NULL)
table(d$A11d %% 1, exclude = NULL)
table(d$A11e %% 1, exclude = NULL)
table(d$A11f %% 1, exclude = NULL)
table(d$A11g %% 1, exclude = NULL)
table(d$A11h %% 1, exclude = NULL)
table(d$A11i %% 1, exclude = NULL)
table(d$A11j %% 1, exclude = NULL)
table(d$A11k %% 1, exclude = NULL)



d <- 
  mutate(d,
         reg_drop_total = ifelse(A11a < 0, NA, A11a),
         na_reg_drop_total = case_when(A11a >= 0 ~ "Reported",
                                       A11a == -888888 ~ "Not applicable",
                                       A11a == -999999 ~ "Not available",
                                       is.na(A11a) ~ "Missing"),
         reg_drop_moved = ifelse(A11b < 0, NA, A11b),
         na_reg_drop_moved = case_when(A11b >= 0 ~ "Reported",
                                       A11b == -888888 ~ "Not applicable",
                                       A11b == -999999 ~ "Not available",
                                       is.na(A11b) ~ "Missing"),
         reg_drop_deceased = ifelse(A11c < 0, NA, A11c),
         na_reg_drop_deceased = case_when(A11c >= 0 ~ "Reported",
                                          A11c == -888888 ~ "Not applicable",
                                          A11c == -999999 ~ "Not available",
                                          is.na(A11c) ~ "Missing"),
         reg_drop_felony = ifelse(A11d < 0, NA, A11d),
         na_reg_drop_felony = case_when(A11d >= 0 ~ "Reported",
                                        A11d == -888888 ~ "Not applicable",
                                        A11d == -999999 ~ "Not available",
                                        is.na(A11d) ~ "Missing"),
         reg_drop_inactive = ifelse(A11e < 0, NA, A11e),
         na_reg_drop_inactive = case_when(A11e >= 0 ~ "Reported",
                                          A11e == -888888 ~ "Not applicable",
                                          A11e == -999999 ~ "Not available",
                                          is.na(A11e) ~ "Missing"),
         reg_drop_incompetent = ifelse(A11f < 0, NA, A11f),
         na_reg_drop_incompetent = case_when(A11f >= 0 ~ "Reported",
                                             A11f == -888888 ~ "Not applicable",
                                             A11f == -999999 ~ "Not available",
                                             is.na(A11f) ~ "Missing"),
         reg_drop_request = ifelse(A11g < 0, NA, A11g),
         na_reg_drop_request = case_when(A11g >= 0 ~ "Reported",
                                         A11g == -888888 ~ "Not applicable",
                                         A11g == -999999 ~ "Not available",
                                         is.na(A11g) ~ "Missing"),
         reg_drop_other1 = ifelse(A11h < 0, NA, A11h),
         na_reg_drop_other1 = case_when(A11h >= 0 ~ "Reported",
                                        A11h == -888888 ~ "Not applicable",
                                        A11h == -999999 ~ "Not available",
                                        is.na(A11h) ~ "Missing"),
         reg_drop_other2 = ifelse(A11i < 0, NA, A11i),
         na_reg_drop_other2 = case_when(A11i >= 0 ~ "Reported",
                                        A11i == -888888 ~ "Not applicable",
                                        A11i == -999999 ~ "Not available",
                                        is.na(A11i) ~ "Missing"),
         reg_drop_other3 = ifelse(A11j < 0, NA, A11j),
         na_reg_drop_other3 = case_when(A11j >= 0 ~ "Reported",
                                        A11j == -888888 ~ "Not applicable",
                                        A11j == -999999 ~ "Not available",
                                        is.na(A11j) ~ "Missing"),
         reg_drop_other4 = ifelse(A11k < 0, NA, A11k),
         na_reg_drop_other4 = case_when(A11k >= 0 ~ "Reported",
                                        A11k == -888888 ~ "Not applicable",
                                        A11k == -999999 ~ "Not available",
                                        is.na(A11k) ~ "Missing"),
         reg_drop_total_calc = reg_drop_moved + reg_drop_deceased +
                               reg_drop_felony + reg_drop_inactive +
                               reg_drop_incompetent + reg_drop_request +
                               reg_drop_other1 + reg_drop_other2 + 
                               reg_drop_other3 + reg_drop_other4)






todo <- todo + 1
beepr::beep(2)
# definitely need to tabulate everything
# to see if missing codes are exhaustive



# remove original variables and save
d %>%
select(-one_of(drop_names)) %>% 
print %>% 
saveRDS("data/clean/clean-EAVS-2016-A.RDS")

