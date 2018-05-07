# ----------------------------------------------------
#   EAVS 2016
#   Relative usage of voting methods
#   (in-person, early, absentee (UOCAVA? Provisional?))
# ----------------------------------------------------


rm(list = ls())



todo <- 0

(joyce_states <- c("IL", "IN", "MI", "MN", "OH", "WI"))



# readRDS("data/joyce-eavs-2016-unlabelled.RDS") %>%

d <- readRDS("data/eavs-2016-unlabelled.RDS") %>%
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

raw_methods <- d %>%
  mutate(jurisdictions = 1) %>%
  filter(State %in% joyce_states) %>%
  select(State, jurisdictions, F1a: F1j, -contains("Other")) %>%
  mutate_if(is.numeric, function(x) case_when(x < 0 ~ 0,
                                              TRUE ~ x)) %>%
  group_by(State) %>%
  summarize_if(is.numeric, sum, na.rm = TRUE) %>%
  mutate(F1other = F1h + F1i + F1j) %>%
  select(-F1h, -F1i, -F1j) %>%
  mutate(calc_total = rowSums(select(., F1b:F1other)),
         unknown = F1a - calc_total) %>%
  print()



# --- NOTE -----------------------
# Several states have lots of error in this calculation method
# Great Lakes states happen to have their stuff together pretty well
# the "unknown" category essentially measures error between REPORTED and CALCULATED vote totals
# Some states have a negative number of unknown, meaning that they may be double-counting certain votes when they go through each method
# (e.g. maybe early and absentee are being confused in certain areas)
# -
# future looks at these data will want to engage with this more explicitly

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
  rename(total_votes = total) %>%
  select(-contains("F1"), -calc_total, -unknown) %>%
  print()



# --- why is MI slightly off? -----------------------

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
  write_csv(., "output/state/great-lakes-voting-methods.csv")
