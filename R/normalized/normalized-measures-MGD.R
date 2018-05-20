# ----------------------------------------------------
#   JOYCE PROJECT
#   SUMMARY/NORMALIZED MEASURES
# ----------------------------------------------------

# navigate to PA/MIT

library("here")
library("skimr")
library("janitor")
library("magrittr")
library("tidyverse")
library("ggplot2")
library("ipumsr")




# --- Registration -----------------------

# New registrations received (EAVS A5a)
# New valid registrations received (EAVS A5b)
# Total registered voters (EAVS A1a)

# Invalid or rejected registration applications (EAVS A5e)
# Percentage of new voter registrations rejected
#   (EAVS (A5e) / ((A5a)-(A5d+A5f))) (minus duplicates and COA)

(eavs_state <- here("output/eavs/state/"))

# EAVS A5a
regs <- left_join(read_csv(paste0(eavs_state, "A/A-1-4-reg-total.csv")),
                  read_csv(paste0(eavs_state, "A/A-5-reg-sub.csv"))) %>%
  select(State:joyce, A1a, A5a, A5b, A5e, A5f, A5d) %>%
  rename(new_registrations_received = A5a,
         total_registered_voters = A1a,
         new_valid_registrations = A5b,
         new_rejected_registrations = A5e) %>%
  mutate(pct_registrations_rejected =
           new_rejected_registrations /
           (new_registrations_received - A5d - A5f)) %>%
  select(-(starts_with("A5"))) %>%
  print()





# --- UOCAVA -----------------------

# UOCAVA ballots transmitted to voters (EAVS B1a)
# UOCAVA ballots returned for counting (EAVS B2a)
# UOCAVA ballots counted (EAVS B8a)
# UOCAVA ballots rejected (EAVS B13a)

# Percentage of UOCAVA ballots returned for counting
#   that are rejected ((B13a) / (B2a))
# Percentage of UOCAVA ballots transmitted
#   that are not returned for counting ((B1a - B2a) / (B1a))




uocava <-
  left_join(
    read_csv(paste0(eavs_state, "B/B-1-2-uocava-sent.csv")),
    read_csv(paste0(eavs_state, "B/B-8-9-10-11-12-uocava-counted.csv"))) %>%
  left_join(
    read_csv(paste0(eavs_state, "B/B-13-14-15-16-17-18-uocava-rejected.csv"))) %>%
  select(State:joyce, B1a, B2a, B8a, B13a) %>%
  rename(uocava_ballots_sent = B1a,
         uocava_ballots_returned = B2a,
         uocava_ballots_counted = B8a,
         uocava_ballots_rejected = B13a) %>%
  mutate(pct_uocava_ballots_rejected =
           uocava_ballots_rejected / uocava_ballots_returned,
         pct_uocava_ballots_not_returned =
           (uocava_ballots_sent - uocava_ballots_returned) /
             uocava_ballots_sent) %>%
  print()



# --- Absentee -----------------------

# Civilian absentee ballots transmitted to voters (EAVS C1a)
# Civilian absentee ballots returned for counting (EAVS C1b)
# Civilian absentee ballots accepted for counting (EAVS C4a)

# Absentee ballots rejected (EAVS C4b)

# Percentage of civilian absentee ballots returned for counting that are rejected ((C4b) / (C1b))

# Percentage of civilian absentee ballots transmitted that are not returned for counting  ((C1a - C1b) / (C1a))

absentee <-
  left_join(read_csv(paste0(eavs_state, "C/C-1-3-absentee-sent-fate.csv")),
            read_csv(paste0(eavs_state, "C/C-4-absentee-returned-fate.csv"))) %>%
  rename(absentee_ballots_sent = C1a,
         absentee_ballots_returned = C1b,
         absentee_ballots_accepted = C4a,
         absentee_ballots_rejected = C4b) %>%
  mutate(pct_absentee_ballots_rejected =
           (absentee_ballots_rejected / absentee_ballots_returned),
         pct_absentee_ballots_not_returned =
           (absentee_ballots_sent - absentee_ballots_returned) /
             absentee_ballots_sent) %>%
  select(State:joyce, contains("absentee")) %>%
  print()




# --- relative usage of voting methods -----------------

d <- readRDS("data/eavs/eavs-2016-unlabelled.RDS") %>%
     print()

# --- note -----------------------
# this section contains commented out code to do one of two things:
# 1: aggregate using MAE method (seemed unnecessary here)
# 2: calculate "totals" (and thus "errors") along the way
# why not 2? It seems like these categories are not mutually exclusive
# e.g. Oregon is all mail,
#     so this double counts mail with provisional, civialian absentee, etc.
# e.g. MN appears to double count in-person and early voting
# people using these data will want to be careful with this

# F1a - total
# F1b - election day in-person
# B8a - UOCAVA / FWAB
# C4a - civilian absentee
# F1e - provisional
# F1f - early vote center
# F1g - mail
# F1h, F1i, F1j - other

# calculate usage (MAE method or not?)
raw_methods <- d %>%
  mutate(jurisdictions = 1) %>%
  select(State, jurisdictions, A1a, # A1a needed only for MAE method
         F1a, F1b, B8a, C4a, F1e, F1f, F1g, F1h, F1i, F1j,
          -contains("Other")) %>%
  group_by(State) %>%
  mutate_if(is.numeric, function(x) ifelse(x < 0, NA, x)) %>%
  # raw method ----
  summarize_if(is.numeric, sum, na.rm = TRUE) %>%
  # ----
  # MAE method ----
  # summarize_if(is.numeric, function(x) {
  #                total_reg <- sum(.$A1a, na.rm = TRUE)
  #                represented <- sum(.$A1a[!is.na(x)], na.rm = TRUE)
  #                frac_represented <- represented / total_reg
  #                ifelse(frac_represented >= 0.85, sum(x, na.rm = TRUE), NA)
  #               }
  #   ) %>%
  # -----
  mutate(F1other = F1h + F1i + F1j) %>%
  select(-A1a, -F1h, -F1i, -F1j) %>%
  # mutate(calc_total = rowSums(select(., F1b:F1other), na.rm = TRUE),
  #        unknown = F1a - calc_total) %>%
  print(n = nrow(.))



prop_methods <- raw_methods %>%
  mutate(total = F1a,
         prop_vote_inperson = F1b / total,
         prop_vote_uocava = B8a / total,
         prop_vote_civabs = C4a / total,
         prop_vote_prov = F1e / total,
         prop_vote_early = F1f / total,
         prop_vote_mail = F1g / total,
         prop_vote_other = F1other / total,
         # prop_vote_unknown = unknown / total
        ) %>%
  select(-contains("F1"), -B8a, -C4a, -total,
         # -calc_total, -unknown
        ) %>%
  mutate_at(vars(contains("prop_v")), function(x) x * 100) %>%
  rename_if(is.numeric, function(x) str_replace_all(x, "prop", "perc")) %>%
  # mutate(perc_vote_accounted =
  #          select(., contains("perc_v"), -contains("unknown")) %>%
  #          rowSums(., na.rm = TRUE),
  #        perc_vote_unaccounted = 100 - perc_vote_accounted) %>%
  # mutate_if()
  print(n = nrow(.))








# --- Wait times (SPAE) -----------------------

# dropping MOEs because these are non-normal outcomes
# so MOEs don't make sense
# we do have them in the other results though
waits <- read_csv("output/waits/state-avg-waits.csv") %>%
  rename(State = state_abb) %>%
  select(-inputstate, -MOE) %>%
  print()



# --- SPAE extra stuff -----------------------

# Percentage of voters who say polling place was easy to find (Q5)
# Percentage of voters confident their vote was counted as cast (Q33)

census <- read_csv("data/census/census-state-fips.csv") %>%
  rename(inputstate = state_FIPS) %>%
  print()

spae <- read_tsv("data/spae/MITU0022_OUTPUT.tab") %>%
  mutate(wt = weight / max(weight, na.rm = TRUE)) %>%
  left_join(., census, by = "inputstate") %>%
  print()




spae %>%
  group_by(state) %>%
  skim(weight)


polling_place <- spae %>%
  filter(!is.na(Q5)) %>%
  group_by(state_abb) %>%
  summarize(pct_easy_polling_place = 100 * mean(Q5 %in% c(3, 4), na.rm = TRUE)) %>%
  rename(State = state_abb) %>%
  print()


ballot_confidence <- spae %>%
  filter(!is.na(Q33)) %>%
  group_by(state_abb) %>%
  summarize(pct_confident = 100 * mean(Q33 %in% c(1, 2))) %>%
  rename(State = state_abb) %>%
  print()



# --- US Elections Project -----------------------

usep <- read_csv("data/vep/usep-state-vep-2016.csv") %>%
  select(State, `VEP Total Ballots Counted`) %>%
  setNames(c("state", "vep_turnout")) %>%
  filter(!(state %in% c("United States", "District of Columbia"))) %>%
  mutate(vep_turnout = as.numeric(str_replace(vep_turnout, "%", ""))) %>%
  print()



# --- CPS -----------------------

list.files(here("data/cps/vrs-2016"))
# unzip CPS if it isn't already
if ("cps_00002.dat" %in% list.files(here("data/cps/vrs-2016"))) {
  print("CPS Data already uncompressed")
} else {
   R.utils::gunzip(here('data/cps/vrs-2016/cps_00002.dat.gz'),
                   remove = FALSE, skip = TRUE)
}


cps_ddi <- here("data/cps/vrs-2016/cps_00002.xml") %>%
  read_ipums_ddi() %>%
  print()

cps_data <- here("data/cps/vrs-2016/cps_00002.dat") %>%
  read_ipums_micro(ddi = cps_ddi, data_file = .) %>%
  print()



#
# cps_ddi <- read_ipums_ddi(cps_ddi_file) # Contains metadata, nice to have as separate object
# cps_data <- read_ipums_micro(cps_ddi_file, data_file = cps_data_file)
#
# ipums_conditions()


# --- CPS old -----------------

# codebook: https://cps.ipums.org/cps-action/variables/group?id=voter_voter


# unzip CPS if it isn't already
if ("IPUMPS-cps_00001.csv" %in% list.files(here("data/cps"))) {
  print("CPS Data already uncompressed")
} else {
  R.utils::gunzip(here('data/cps/IPUMPS-cps_00001.csv.gz'), remove = FALSE)
}


# Voting age and race demographics (??)
# Percent of VEP registered to vote
#   (Eligible, registered)
# Pct of nonvoters who failed to vote because of registration problems (8)
#   (nonvote, registration problem)
# Pct of nonvoters who didn't vote because of polling place problems (10)
#   (nonvote, polling place problem)
# Pct of nonvoters who didn't vote because of permanent illness/disability (1)
#   (nonvote, illness/disability)



# read CPS data and do simple recoding
cps16 <- read_csv(here("data/cps/IPUMPS-cps_00001.csv")) %>%
  filter(YEAR == 2016) %>%
  print()

cps <- cps16 %>%
  mutate(voted = case_when(VOTED %in% c(1, 96, 97, 98) ~ 0,
                           VOTED == 2 ~ 1),
         registered = case_when(voted == 1 ~ 1,
                                VOREG == 2 ~ 1,
                                VOREG == 1 ~ 0),
         # eligible if you vote/register
         # some regwhynot imply ineligibility
         # what to do about the rest?
         eligible = case_when(voted == 1 ~ 1,
                              registered == 1 ~ 1,
                              !(VOYNOTREG %in% c(3, 8)) ~ 0,
                              VOYNOTREG %in% c(3, 8, 99) ~ 0,
                              TRUE ~ -1),
         # what counts as a registration problem?
         reg_problem = case_when((VOWHYNOT == 8) |
                                 VOYNOTREG %in% c(1, 2, 3) ~ 1),
         pollingplace_problem = case_when(VOWHYNOT == 10 ~ 1),
         ill_disabled = case_when(VOWHYNOT == 1 ~ 1)) %>%
  print()

names(cps)

count(cps, STATEFIP, YEAR) %>%
  print(n = nrow(.))

count(cps2, registered, voted, eligible)
count(cps2, voted, registered, reg_problem)


# aggregate by state (percentages)
cps_norms <- cps %>%
  group_by(STATEFIP) %>%
  summarize(prop_vep_registered =
              sum(registered == 1, na.rm = TRUE) /
              sum(eligible == 1, na.rm = TRUE),
            prop_nv_reg_problem =
              sum(reg_problem == 1, na.rm = TRUE) /
              sum(voted == 0, na.rm = TRUE),
            prop_nv_place_problem =
              sum(pollingplace_problem, na.rm = TRUE) /
              sum(voted == 0, na.rm = TRUE),
            prop_nv_ill_disabled =
              sum(ill_disabled == 1, na.rm = TRUE) /
              sum(voted == 0, na.rm = TRUE),
            n = n()) %>%
  mutate_at(vars(starts_with("prop_")), function(x) x * 100) %>%
  rename_at(vars(starts_with("prop_")),
            function(x) str_replace(x, "prop", "perc")) %>%
  rename(inputstate = STATEFIP) %>%
  left_join(select(census, inputstate, state_abb)) %>%
  rename(State = state_abb) %>%
  select(State, matches("."), -inputstate) %>%
  print(n = nrow(.))


## ??????? Is this working right?
## Check the book again?






# --- normalized table -----------------------



evan_file <- readxl::read_excel("~/Dropbox/Great Lakes Election Project/normalized measures/normalized measures.xlsx") %>%
  rename(State = state) %>%
  print()

ls()

evan_file
absentee
regs
total_absentee
uocava

#renames
ballot_confidence
cps_norms
polling_place
waits

# merging
normalized_final <- evan_file %>%
  left_join(regs) %>%
  left_join(uocava) %>%
  left_join(absentee) %>%
  left_join(total_absentee) %>%
  left_join(ballot_confidence) %>%
  left_join(cps_norms) %>%
  left_join(polling_place) %>%
  left_join(waits) %>%
  print()

write_csv(normalized_final, here("output/normalized/normalized-mgd.csv"))



print("Normalized measures code completed without error")

# --- Don't forget to check county turnout again -----------------------
