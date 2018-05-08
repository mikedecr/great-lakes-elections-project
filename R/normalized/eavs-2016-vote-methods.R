# ----------------------------------------------------
#   EAVS 2016
#   Relative usage of voting methods
#   (in-person, early, absentee (UOCAVA? Provisional?))
# ----------------------------------------------------


library("magrittr")
library("tidyverse")
library("here")
library("ggplot2")


todo <- 0

(joyce_states <- c("IL", "IN", "MI", "MN", "OH", "WI"))



# readRDS("data/joyce-eavs-2016-unlabelled.RDS") %>%

d <- readRDS("data/eavs/eavs-2016-unlabelled.RDS") %>%
     print()



# F1a - total
# F1b - election day in-person
# F1c - UOCAVA / FWAB
# F1d - civilian absentee
# F1e - provisional
# F1f - early vote center
# F1g - mail
# F1h, F1i, F1j - other



# --- calculate usage -----------------------

# do this also using the MAE aggregation method
raw_methods <- d %>%
  mutate(jurisdictions = 1) %>%
  # filter(State %in% joyce_states) %>%
  select(State, jurisdictions, A1a, F1a: F1j, -contains("Other")) %>%
  mutate_if(is.numeric, function(x) ifelse(x < 0, NA, x)) %>%
  group_by(State) %>%
  summarize_if(is.numeric, function(x) {
                 total_reg <- sum(.$A1a, na.rm = TRUE)
                 represented <- sum(.$A1a[!is.na(x)], na.rm = TRUE)
                 frac_represented <- represented / total_reg
                 ifelse(frac_represented >= 0.85, sum(x, na.rm = TRUE), NA)
                }
    ) %>%
  mutate(F1other = F1h + F1i + F1j) %>%
  select(-A1a, -F1h, -F1i, -F1j) %>%
  mutate(calc_total = rowSums(select(., F1b:F1other)),
         unknown = F1a - calc_total) %>%
  print()


as.data.frame(raw_methods)

# --- NOTE -----------------------
# Several states have some errors in this calculation method
# Great Lakes states happen to have their stuff together pretty well
# "unknown" category measures error between REPORTED and CALCULATED votes
# Some states have a negative number of unknown, meaning that they may be double-counting certain votes when they go through each method
# (e.g. maybe early and absentee are being confused in certain areas)
# future looks at these data will want to engage with this more explicitly
# this method messes up Oregon unintentionally, so I fix it during aggregation
# but in general Oregon is double counted
# (it codes civ_absentee and provisional separately, but all are by mail(?))

prop_methods <- raw_methods %>%
  mutate(total = F1a,
         in_person = F1b / total,
         uocava_fwab = F1c / total,
         civ_absentee = F1d / total,
         provisional = F1e / total,
         early = F1f / total,
         mail = F1g / total,
         other = F1other / total,
         unaccounted = unknown / total) %>%
  mutate_if(is.numeric, round, 5) %>%
  mutate(mail = ifelse(State == "OR", 0, mail)) %>%
  rename(total_votes = total) %>%
  select(-contains("F1"), -calc_total, -unknown) %>%
  mutate(total_accounted = round(rowSums(select(., in_person:other), na.rm = TRUE), 5)) %>%
  print()

as.data.frame(prop_methods)



# everything is behaving?
prop_methods %>%
  gather(key = method, value = prop, -(State:total_votes)) %>%
  filter(prop > 1 | prop < 0)






# --- of the GL states: why is MI off? -----------------------

d %>%
filter(State == "MI") %>%
select(contains("F1")) %>%
select(contains("Other"), contains("Comments")) %>%
as.data.frame()



d %>%
filter(State == "MI") %>%
select(contains("F2")) %>%
as.data.frame()

d %>%
filter(State == "MI") %>%
select(contains("F2")) %$%
table(F2, exclude = NULL)

# all entry methods marked as "other" but with no additional comments
# not sure what to make of this



# ----------------------------------------------------
#   save table
# ----------------------------------------------------


prop_methods %>%
  print() %>%
  write_csv(., "output/normalized/voting-methods.csv")
