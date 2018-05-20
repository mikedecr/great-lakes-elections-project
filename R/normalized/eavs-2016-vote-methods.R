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
# B8a - UOCAVA / FWAB
# C4a - civilian absentee
# F1e - provisional
# F1f - early vote center
# F1g - mail
# F1h, F1i, F1j - other



# --- calculate usage -----------------------

# do this also using the MAE aggregation method
raw_methods <- d %>%
  mutate(jurisdictions = 1) %>%
  # filter(State %in% joyce_states) %>%
  select(State, jurisdictions, A1a,
         F1a, F1b, B8a, C4a, F1e, F1f,
         # F1g,
         F1h, F1i, F1j, -contains("Other")) %>%
  group_by(State) %>%
  mutate_if(is.numeric, function(x) ifelse(x < 0, NA, x)) %>%
  # MAE method
  # summarize_if(is.numeric, function(x) {
  #                total_reg <- sum(.$A1a, na.rm = TRUE)
  #                represented <- sum(.$A1a[!is.na(x)], na.rm = TRUE)
  #                frac_represented <- represented / total_reg
  #                ifelse(frac_represented >= 0.85, sum(x, na.rm = TRUE), NA)
  #               }
  #   ) %>%
  # raw method
  summarize_if(is.numeric, sum, na.rm = TRUE) %>%
  mutate(F1other = F1h + F1i + F1j) %>%
  select(-A1a, -F1h, -F1i, -F1j) %>%
  mutate(calc_total = rowSums(select(., F1b:F1other), na.rm = TRUE),
         unknown = F1a - calc_total) %>%
  print(n = nrow(.))


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
         prop_vote_inperson = F1b / total,
         prop_vote_uocava = B8a / total,
         prop_vote_civabs = C4a / total,
         prop_vote_prov = F1e / total,
         prop_vote_early = F1f / total,
         # mail = F1g / total,
         prop_vote_other = F1other / total,
         prop_vote_unknown = unknown / total) %>%
  select(-contains("F1"), -B8a, -C4a, -total, -calc_total, -unknown) %>%
  mutate_if(is.numeric, function(x) x * 100) %>%
  rename_if(is.numeric, function(x) str_replace_all(x, "prop", "perc")) %>%
  # mutate(mail = ifelse(State == "OR", 0, mail)) %>%
  mutate(perc_vote_accounted =
           select(., contains("perc_v"), -contains("unknown")) %>%
           rowSums(., na.rm = TRUE),
         perc_vote_unaccounted = 100 - perc_vote_accounted) %>%
  # mutate_if()
  print(n = nrow(.))

as.data.frame(prop_methods)

prop_methods %>%
  select(State, contains("total")) %>%
  print(n = nrow(.))

# which states are not behaving
prop_methods %>%
  filter(perc_vote_accounted > 100 | perc_vote_accounted < 95) %>%
  select(State, perc_vote_accounted)

prop_methods %>%
  select(State, perc_vote_unaccounted) %>%
  filter(perc_vote_unaccounted < 0 | perc_vote_unaccounted > 5) %>%
  print(n = nrow(.))


# especially weird ones: AL, IA, UT, VT, (ID, WY, MN)






# ----------------------------------------------------
#   save table
# ----------------------------------------------------


prop_methods %>%
  print() %>%
  write_csv(., "output/normalized/voting-methods.csv")


print("EAVS vote methods code completed without error")
