# ----------------------------------------------------
#   JOYCE PROJECT
#   SUMMARY/NORMALIZED MEASURES
# ----------------------------------------------------

# navigate to PA/MIT

library("skimr")
library("janitor")
library("magrittr")
library("tidyverse")
library("ggplot2")


# --- DATA -----------------------

# EAVS

# (eavs <- readRDS("eavs/2016/data/joyce-eavs-2016-unlabelled.RDS"))






# --- Registration -----------------------

# how do these get "normalized"

# New registrations received (EAVS A5a)
# New valid registrations received (EAVS A5b)
# Total registered voters (EAVS A1a)

# Invalid or rejected registration applications (EAVS A5e)
# Percentage of new voter registrations rejected
#   (EAVS (A5e) / ((A5a)-(A5d+A5f))) (minus duplicates and COA)

# EAVS A5a
regs <- left_join(read_csv("eavs/2016/output/state/A/A-1-4-reg-total.csv"),
                  read_csv("eavs/2016/output/state/A/A-5-reg-sub.csv")) %>%
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
    read_csv("eavs/2016/output/state/B/B-1-2-uocava-sent.csv"),
    read_csv("eavs/2016/output/state/B/B-8-9-10-11-12-uocava-counted.csv")) %>%
  left_join(
    read_csv("eavs/2016/output/state/B/B-13-14-15-16-17-18-uocava-rejected.csv")) %>%
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
  left_join(read_csv("eavs/2016/output/state/C/C-1-3-absentee-sent-fate.csv"),
            read_csv("eavs/2016/output/state/C/C-4-absentee-returned-fate.csv")) %>%
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



# --- Wait times (SPAE) -----------------------

# dropping MOEs because these are non-normal outcomes
# and MOEs don't make sense
waits <- read_csv("waits/output/state-avg-waits.csv") %>%
  rename(State = state_abb) %>%
  select(-inputstate, -MOE) %>%
  print()



# --- SPAE extra stuff -----------------------

# Percentage of voters who say polling place was easy to find (Q5)
# Percentage of voters confident their vote was counted as cast (Q33)

census <- read_csv("waits/census/census-state-fips.csv") %>%
  rename(inputstate = state_FIPS) %>%
  print()

spae <- read_tsv("waits/spae/MITU0022_OUTPUT.tab") %>%
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

usep <- read_csv("vep/data/usep-state-vep-2016.csv") %>%
  select(State, `VEP Total Ballots Counted`) %>%
  setNames(c("state", "vep_turnout")) %>%
  filter(!(state %in% c("United States", "District of Columbia"))) %>%
  mutate(vep_turnout = as.numeric(str_replace(vep_turnout, "%", ""))) %>%
  print()



# --- CPS -----------------------

# Voting age and race demographics (??)
# Percent of VEP registered to vote

# Percentage of nonvoters who state they failed to vote because of registration problems (8)
# Percentage of nonvoters who say they didn't vote because of polling place problems (10)
# Percentage of nonvoters who say they didn't vote because of a permanent illness or disability (1)


if ("IPUMPS-cps_00001.csv" %in% list.files("cps")) {
  print("CPS Data already uncompressed")
} else {
  R.utils::gunzip('cps/IPUMPS-cps_00001.csv.gz', remove = FALSE)
}

cps <- read_csv("cps/IPUMPS-cps_00001.csv") %>%
  mutate(nonvote = case_when(VOTED == 1 ~ 1),
         registered = case_when(VOTED == 2 | VOREG == 2 ~ 1),
         eligible = case_when(VOYNOTREG != 8 & VOTED %in% c(1, 2) ~ 1,
                              registered == 1 ~ 1),
         reg_problem = case_when(VOWHYNOT == 8 ~ 1),
         pp_problem = case_when(VOWHYNOT == 10 ~ 1),
         ill_disabled = case_when(VOWHYNOT == 1 ~ 1)) %>%
  print()






cps_norms <- cps %>%
  group_by(STATEFIP) %>%
  summarize(pct_vep_registered =
              sum(registered == 1, na.rm = TRUE) /
              sum(eligible == 1, na.rm = TRUE),
            pct_nv_reg_problem =
              sum(reg_problem == 1, na.rm = TRUE) /
              sum(nonvote == 1, na.rm = TRUE),
            pct_nv_place_problem =
              sum(pp_problem, na.rm = TRUE) /
              sum(nonvote == 1, na.rm = TRUE),
            pct_nv_ill_disabled =
              sum(ill_disabled == 1, na.rm = TRUE) /
              sum(nonvote == 1, na.rm = TRUE)) %>%
  rename(inputstate = STATEFIP) %>%
  left_join(select(census, inputstate, state_abb)) %>%
  rename(State = state_abb) %>%
  select(State, matches("."), -inputstate) %>%
  print()


## ??????? Is this working right?
## Check the book again?




# --- Fixing things re: Evan's email -----------------------

ls()

evan_file <- readxl::read_excel("~/Dropbox/Great Lakes Election Project/normalized measures/normalized measures.xlsx") %>%
  rename(State = state) %>%
  filter(State != "DC") %>%
  print()



total_absentee <-
  evan_file %>%
  select(State, total_ballots_cast) %>%
  left_join(select(absentee, State:joyce, absentee_ballots_accepted)) %>%
  left_join(select(uocava, State:joyce, uocava_ballots_counted)) %>%
  mutate(perc_civ_absentee = 100 * (absentee_ballots_accepted /
                                    total_ballots_cast),
         perc_uocava = 100 * (uocava_ballots_counted /
                              total_ballots_cast),
         perc_all_absentee =
           100 * ((absentee_ballots_accepted + uocava_ballots_counted) /
                  total_ballots_cast)) %>%
  select(State, jurisdictions, joyce, contains("perc")) %>%
  print()





# --- normalized table -----------------------

evan_file %>%
  left_join()

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

write_csv(normalized_final, "normalized/normalized-mgd.csv")

# --- Don't forget to check county turnout again -----------------------
