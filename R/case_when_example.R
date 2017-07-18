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

a <- haven::read_dta("eavs-2016.dta") %>%
					select(FIPSCode, FIPS_2Digit, State, JurisdictionName, starts_with("A")) %>% 
					print

names(a)




#----------------------------------------
#   naming scheme
#----------------------------------------

beepr::beep(2)
todo <- todo + 1
# almost every topic set (A2, A3, etc) has comment variables



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


head(table(a$A1a, exclude = NULL))
table(is.na(a$A1a), exclude = NULL)

head(table(a$A2, exclude = NULL))
table(is.na(a$A2), exclude = NULL)

# who is reported, "other" codes
head(table(a$A2c_Other, exclude = NULL)) 

# all "None" are State == ME:
filter(a, State == "ME") %$% 
table(A2, A2c_Other, exclude = NULL)
a %$% table(A2, A2c_Other, exclude = NULL)

# and the only missing jurisdiction is also in Maine

head(table(a$A3a, exclude = NULL))
table(is.na(a$A3a), exclude = NULL)

head(table(a$A3b, exclude = NULL))
table(is.na(a$A3b), exclude = NULL)

table(a$A1a %% 1, exclude = NULL)
table(a$A3a %% 1, exclude = NULL)
table(a$A3b %% 1, exclude = NULL)



a %>%
mutate(reg_total_eligible = ifelse(A1a < 0, NA, A1a),
       na_reg_total_eligible1 = case_when(A1a >= 0 ~ "Reported", 
                                          A1a == -999999 ~ "Not Available", 
                                          A1a == -888888 ~ "Not Applicable", 
                                          is.na(A1a) ~ "Missing"),
       reg_includes = case_when(A2 == 1 ~ "Active Only",
                                A2 == 2 ~ "Active and Inactive",
                                A2 == 3 ~ "Other",
                                is.na(A2) ~ "Missing"),
       reg_includes_spec = ifelse(trimws(A2c_Other) == "", NA, A2c_Other),
       reg_active = ifelse(A3a < 0, NA, A3a),
       na_reg_active = case_when(A3a >= 0 ~ "Reported", 
                                 A3a == -888888 ~ "Not Applicable", 
                                 A3a == -999999 ~ "Not Available", 
                                 is.na(A3a) ~ "Missing"),
       reg_inactive = ifelse(A3b < 0, NA, A3b),
       na_reg_inactive = case_when(A3b >= 0 ~ "Reported",
                                    A3b == -888888 ~ "Not Applicable",
                                    A3b == -999999 ~ "Not Available",
                                    is.na(A3b) ~ "Missing"))

