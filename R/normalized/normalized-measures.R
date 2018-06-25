# ----------------------------------------------------
#   JOYCE PROJECT
#   SUMMARY/NORMALIZED MEASURES
# ----------------------------------------------------


# master build file loads packages already
library("here")
library("magrittr")
library("tidyverse")
library("ggplot2")




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
         new_registrations_rejected = A5e) %>%
  mutate(pct_registrations_rejected =
           new_registrations_rejected /
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
         absentee_ballots_counted = C4a,
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
  # summarize_if(is.numeric, sum, na.rm = TRUE) %>%
  # ----
  # MAE method ----
  summarize_if(is.numeric, function(x) {
                 total_reg <- sum(.$A1a, na.rm = TRUE)
                 represented <- sum(.$A1a[!is.na(x)], na.rm = TRUE)
                 frac_represented <- represented / total_reg
                 ifelse(frac_represented >= 0.85, sum(x, na.rm = TRUE), NA)
                }
    ) %>%
  # -----
  mutate(F1other = F1h + F1i + F1j) %>%
  select(-A1a, -F1h, -F1i, -F1j) %>%
  print(n = nrow(.))



prop_methods <- raw_methods %>%
  mutate(total = F1a,
         prop_vote_inperson = F1b / total,
         prop_vote_uocava = B8a / total,
         prop_vote_civabs = C4a / total,
         prop_vote_prov = F1e / total,
         prop_vote_early = F1f / total,
         prop_vote_mail = F1g / total,
         prop_vote_other = F1other / total
        ) %>%
  select(-contains("F1"), -B8a, -C4a, -total) %>%
  mutate_at(vars(contains("prop_v")), function(x) x * 100) %>%
  rename_if(is.numeric, function(x) str_replace_all(x, "prop", "perc")) %>%
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
  print(n = nrow(.))


spae <- read_tsv("data/spae/MITU0022_OUTPUT.tab") %>%
  mutate(wt = weight / max(weight, na.rm = TRUE)) %>%
  left_join(., census, by = "inputstate") %>%
  print()





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

# state turnout: drops DC?
usep <- read_csv("data/vep/usep-state-vep-2016.csv") %>%
  select(State, `VEP Total Ballots Counted`) %>%
  setNames(c("state", "vep_turnout")) %>% 
  left_join(census) %>% 
  select(state_abb, vep_turnout) %>% 
  rename(State = state_abb) %>% 
  filter(State %in% c(state.abb, "District of Columbia")) %>%
  mutate(vep_turnout = as.numeric(str_replace(vep_turnout, "%", ""))) %>%
  print()



# --- CPS -----------------------

# codebook: https://cps.ipums.org/cps-action/variables/group?id=voter_voter


cps <- haven::read_dta(here("data/cps/vrs/vrs_extract_2016.dta")) %>%
  print()

# Voting age and race demographics (??)
# Percent of VEP registered to vote
#   (Eligible, registered)
# Pct of nonvoters who failed to vote because of registration problems (8)
#   (nonvote, registration problem)
# Pct of nonvoters who didn't vote because of polling place problems (10)
#   (nonvote, polling place problem)
# Pct of nonvoters who didn't vote because of permanent illness/disability (1)
#   (nonvote, illness/disability)

# examine variable labels
cps %>%
  mutate_all(labelled::var_label) %>% 
  distinct() %>% 
  gather() %>%
  print(n = nrow(.))

# variables
# - pes1 (voted)
# - pes2 (registered)
# - pes3 (reason not registered)
# - pes4 (main reason not vote)
# - pes5 (in person, by mail)
# - pes6 (election day or early)
# - pes5 (how registered)
# - pwsswgt (weight)
# - gestfips (FIPS)

cps_norms <- cps %>%
  mutate_all(labelled::remove_labels) %>% 
  mutate(voted = case_when(pes1 == 1 ~ 1,
                           pes1 == 2 ~ 0),
         registered = case_when(pes2 == 1 ~  1,
                                pes2 == 2 ~ 0,
                                voted == 1 ~ 1),
         eligible = case_when(pes3 %in% c(3, 8) ~ 0, 
                              pes3 > 0 ~ 1,
                              registered == 1 ~ 1,
                              voted == 1 ~ 1),
         nonvote_regproblems = case_when(pes4 == 8 ~ 1,
                                         pes4 %in% c(1:7, 9:11) ~ 0),
         nonvote_placeproblems = case_when(pes4 == 10 ~ 1,
                                         pes4 %in% c(1:9, 1) ~ 0),
         nonvote_illdisable = case_when(pes4 == 1 ~ 1,
                                         pes4 %in% c(2:11, 1) ~ 0)) %>%
  group_by(gestfips) %>%
  summarize(prop_vep_registered = sum(registered * pwsswgt, na.rm = TRUE) / 
                            sum(eligible * pwsswgt, na.rm = TRUE),
            prop_nv_reg_problem = sum((nonvote_regproblems == 1) * pwsswgt, 
                                 na.rm = TRUE) / 
                             sum((nonvote_regproblems == 0) * pwsswgt, 
                                 na.rm = TRUE),
            prop_nv_place_problem = sum((nonvote_placeproblems == 1) * pwsswgt, 
                                 na.rm = TRUE) / 
                             sum((nonvote_placeproblems == 0) * pwsswgt, 
                                 na.rm = TRUE),
            prop_nv_ill_disabled = sum((nonvote_illdisable == 1) * pwsswgt, 
                                 na.rm = TRUE) / 
                             sum((nonvote_illdisable == 0) * pwsswgt, 
                                 na.rm = TRUE)) %>%
  mutate_at(vars(starts_with("prop_")), function(x) x * 100) %>%
  rename_at(vars(starts_with("prop_")),
            function(x) str_replace(x, "prop", "perc")) %>%
  rename(inputstate = gestfips) %>%
  mutate(inputstate = as.integer(inputstate)) %>%
  left_join(select(census, inputstate, state_abb), .) %>%
  rename(State = state_abb) %>%
  select(State, matches("."), -inputstate) %>%
  print(n = nrow(.))


## ??????? Is this working right?
## Check the book again?
## check w/ Barry about coding decisions (force code to zero or trust NA)




# --- final normalized table -----------------------


evan_file <- readxl::read_excel("data/normalized-evan/normalized measures.xlsx") %>%
  rename(State = state) %>%
  print()

ls()


normalized <- evan_file %>%
  left_join(regs) %>%
  left_join(uocava) %>% 
  left_join(absentee) %>%
  mutate(ballots_cast_absentee = 
           absentee_ballots_returned + uocava_ballots_returned) %>% 
  left_join(prop_methods) %>%
  left_join(waits) %>% 
  left_join(polling_place) %>% 
  left_join(ballot_confidence) %>%
  left_join(usep) %>%
  left_join(cps_norms) %>% 
  select(State, jurisdictions, joyce, 
         new_registrations_received, new_valid_registrations, total_registered_voters,
         contains("prov_ballots"), 
         total_ballots_cast,
         contains("ballots_cast"),
         absentee_ballots_sent, absentee_ballots_returned, absentee_ballots_counted,
         uocava_ballots_sent, uocava_ballots_returned, uocava_ballots_counted,
         new_registrations_rejected, absentee_ballots_rejected, uocava_ballots_rejected,
         vep_turnout,
         perc_vote_early, perc_vote_civabs, perc_vote_uocava, perc_vote_inperson, perc_vote_prov, perc_vote_mail, perc_vote_other,
         # need residual votes
         percent_electronic_sign_in:paper_ballot_booths,
         vep_turnout, perc_vep_registered, 
         pct_registrations_rejected, perc_nv_reg_problem,
         perc_turnout_prov, perc_prov_rej,
         pct_absentee_ballots_rejected, pct_absentee_ballots_not_returned,
         pct_uocava_ballots_rejected, pct_uocava_ballots_not_returned, 
         mean_wait,
         perc_nv_place_problem, pct_easy_polling_place, perc_nv_ill_disabled, pct_confident,
         # data completeness???
         matches(".")) %>% 
  print()

names(normalized)

# some potentially inconsistent/redundant data?
select(normalized, perc_vote_early, perc_early_vote, perc_vote_inperson, perc_ed_vote)

dir.create(here("output/normalized"))
write_csv(normalized, here("output/normalized/normalized-measures.csv"))



print("Normalized measures code completed without error")

