# ----------------------------------------------------
#   JOYCE PROJECT
#   SUMMARY/NORMALIZED MEASURES
# ----------------------------------------------------


# master build file loads packages already

eavs <- readRDS("data/eavs/eavs-recode.RDS") %>%
  mutate(joyce = ifelse(State %in% joyce_states, 1, 0)) %>%
  select(FIPSCode:State, joyce, everything()) %>%
  group_by(State) %>% 
  mutate(wt = A1a / sum(A1a, na.rm = TRUE)) %>%
  ungroup() %>%
  print()

# --- Registration -----------------------

# New registrations received (EAVS A5a)
# New valid registrations received (EAVS A5b)
# Total registered voters (EAVS A1a)

# Invalid or rejected registration applications (EAVS A5e)
# Percentage of new voter registrations rejected
#   (EAVS (A5e) / ((A5a)-(A5d+A5f))) (minus duplicates and COA)

# we pull in state-level figures and then re-claculate the ratio
# using 85% threshold for numerators and denominators
# so the ratio may not match the ratio of state-level figures

# naming scheme: 
# old ratios contain "_ballots_"

(eavs_state <- here("output/eavs/state/"))

regs <- left_join(read_csv(paste0(eavs_state, "A/A-1-4-reg-total.csv")),
                  read_csv(paste0(eavs_state, "A/A-5-reg-sub.csv"))) %>%
  select(State:joyce, A1a, A5a, A5b, A5e, A5f, A5d) %>%
  rename(new_registrations_received = A5a,
         total_registered_voters = A1a,
         new_valid_registrations = A5b,
         new_registrations_rejected = A5e) %>%
  # mutate(pct_registrations_rejected_old = 
  #             100 * new_registrations_rejected / 
  #             (new_registrations_received - (A5d + A5f))) %>%
  select(-(starts_with("A5"))) %>%
  print()


# calculate ratio, save over regs
regs <- eavs %>%
  group_by(State) %>% 
  summarize(received = sum(A5a, na.rm = TRUE),
            rejected = sum(A5e, na.rm = TRUE),
            duplicates = sum(A5d, na.rm = TRUE),
            changes = sum(A5f, na.rm = TRUE),
            pct_registrations_rejected = 
              rejected / 
              (received - (duplicates + changes)),
            rej_wt = sum(wt[!is.na(A5e) & !is.na(A5a) & 
                            !is.na(A5d) & !is.na(A5f)], 
                         na.rm = TRUE)) %>%
  mutate(pct_registrations_rejected = 
           ifelse(rej_wt >= 0.85, 100 * pct_registrations_rejected, NA)) %>%
  select(State, pct_registrations_rejected) %>%
  left_join(regs, .) %>%
  print()


as.data.frame(regs)





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
  rename(uocava_sent = B1a,
         uocava_returned = B2a,
         uocava_counted = B8a,
         uocava_rejected = B13a) %>%
  # mutate(pct_absentee_ballots_rejected =
  #          100 * (uocava_rejected / uocava_returned),
  #        pct_uocava_ballots_not_returned =
  #          100 * (uocava_sent - uocava_returned) /
  #            uocava_sent) %>%
  print()


uocava <- eavs %>%
  group_by(State) %>%
  summarize(sent = sum(B1a, na.rm = TRUE),
            returned = sum(B2a, na.rm = TRUE),
            rejected = sum(B13a, na.rm = TRUE),
            prop_uocava_rejected = rejected / returned,
            rej_wt = sum(wt[!is.na(B13a) & !is.na(B2a)], na.rm = TRUE),
            prop_uocava_not_returned = (sent - returned) / sent,
            nr_wt = sum(wt[!is.na(B1a) & !is.na(B2a)])) %>%
  mutate(pct_uocava_rejected = 
           ifelse(rej_wt >= 0.85, 100 * prop_uocava_rejected, NA),
         pct_uocava_not_returned = 
           ifelse(nr_wt >= 0.85, 100 * prop_uocava_not_returned, NA)) %>%
  select(State, contains("pct_")) %>%
  left_join(uocava, .) %>%
  print()

as.data.frame(uocava)



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
  rename(absentee_sent = C1a,
         absentee_returned = C1b,
         absentee_counted = C4a,
         absentee_rejected = C4b) %>%
  # mutate(pct_absentee_ballots_rejected =
  #          100 * (absentee_rejected / absentee_returned),
  #        pct_absentee_ballots_not_returned =
  #          100 * (absentee_sent - absentee_returned) /
  #            absentee_sent) %>%
  select(State:joyce, contains("absentee")) %>%
  print()


absentee <- eavs %>%
  group_by(State) %>%
  summarize(sent = sum(C1a, na.rm = TRUE),
            returned = sum(C1b, na.rm = TRUE),
            rejected = sum(C4b, na.rm = TRUE),
            prop_absentee_rejected = rejected / returned,
            rej_wt = sum(wt[!is.na(C4b) & !is.na(C1b)], na.rm = TRUE),
            nr_wt = sum(wt[!is.na(C1a) & !is.na(C1b)]),
            prop_absentee_not_returned = (sent - returned) / sent) %>%
  mutate(pct_absentee_rejected = 
           ifelse(rej_wt >= 0.85, 100 * prop_absentee_rejected, NA),
         pct_absentee_not_returned = 
           ifelse(nr_wt >= 0.85, 100 * prop_absentee_not_returned, NA)) %>%
  select(State, contains("pct_")) %>%
  left_join(absentee, .) %>%
  print()





# --- relative usage of voting methods -----------------

eavs <- readRDS("data/eavs/eavs-2016-unlabelled.RDS") %>%
  mutate(joyce = ifelse(State %in% joyce_states, 1, 0)) %>%
  select(FIPSCode:State, joyce, everything()) %>%
  group_by(State) %>% 
  mutate(wt = A1a / sum(A1a, na.rm = TRUE)) %>%
  ungroup() %>%
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

# start by calculating fractions, then go back and check for 85% weights

# F1a - total
# F1b - election day in-person
# B8a - UOCAVA / FWAB
# C4a - civilian absentee
# F1e - provisional
# F1f - early vote center
# F1g - mail
# F1h, F1i, F1j - other

raw_methods <- eavs %>%
  mutate(jurisdictions = 1) %>%
  select(State, jurisdictions, A1a, # A1a needed only for MAE method
         F1a, F1b, B8a, C4a, F1e, F1f, F1g, F1h, F1i, F1j,
          -contains("Other")) %>%
  group_by(State) %>%
  mutate_if(is.numeric, function(x) ifelse(x < 0, NA, x)) %>%
  summarize_if(is.numeric, sum, na.rm = TRUE) %>%
  mutate(F1other = F1h + F1i + F1j) %>%
  select(-A1a, -F1h, -F1i, -F1j) %>%
  mutate(prop_vote_inperson = F1b / F1a,
         prop_vote_uocava = B8a / F1a,
         prop_vote_civabs = C4a / F1a,
         prop_vote_prov = F1e / F1a,
         prop_vote_early = F1f / F1a,
         prop_vote_mail = F1g / F1a,
         prop_vote_other = F1other / F1a
        ) %>%
  select(-contains("F1"), -B8a, -C4a) %>%
  print(n = nrow(.))

# calculate by-item weights
# then adjust proportions as needed
pct_methods <- eavs %>%
  group_by(State) %>%
  mutate_if(is.numeric, function(x) ifelse(x < 0, NA, x)) %>%
  mutate(F1other = F1h + F1i + F1j) %>%
  summarize(inperson_wt = sum(wt[!is.na(F1b) * !is.na(F1a)], na.rm = TRUE), 
            uocava_wt = sum(wt[!is.na(B8a) * !is.na(F1a)], na.rm = TRUE), 
            civabs_wt = sum(wt[!is.na(C4a) * !is.na(F1a)], na.rm = TRUE), 
            prov_wt = sum(wt[!is.na(F1e) * !is.na(F1a)], na.rm = TRUE), 
            early_wt = sum(wt[!is.na(F1f) * !is.na(F1a)], na.rm = TRUE), 
            mail_wt = sum(wt[!is.na(F1g) * !is.na(F1a)], na.rm = TRUE), 
            other_wt = sum(wt[!is.na(F1other) * !is.na(F1a)], na.rm = TRUE)) %>%
  left_join(raw_methods, .) %>%
  mutate(pct_vote_inperson = 
           ifelse(inperson_wt >= .85, 100 * prop_vote_inperson, NA), 
         pct_vote_uocava =
           ifelse(uocava_wt >= .85, 100 * prop_vote_uocava, NA), 
         pct_vote_civabs =
           ifelse(civabs_wt >= .85, 100 * prop_vote_civabs, NA), 
         pct_vote_prov =
           ifelse(prov_wt >= .85, 100 * prop_vote_prov, NA), 
         pct_vote_early =
           ifelse(early_wt >= .85, 100 * prop_vote_early, NA), 
         pct_vote_mail =
           ifelse(mail_wt >= .85, 100 * prop_vote_mail, NA), 
         pct_vote_other =
           ifelse(other_wt >= .85, 100 * prop_vote_other, NA)) %>%
  select(State, jurisdictions, contains("pct_")) %>%
  as.data.frame()



# calculate usage (MAE method or not?)
# raw_methods_old <- eavs %>%
#   mutate(jurisdictions = 1) %>%
#   select(State, jurisdictions, A1a, # A1a needed only for MAE method
#          F1a, F1b, B8a, C4a, F1e, F1f, F1g, F1h, F1i, F1j,
#           -contains("Other")) %>%
#   group_by(State) %>%
#   mutate_if(is.numeric, function(x) ifelse(x < 0, NA, x)) %>%
#   # raw method ----
#   # summarize_if(is.numeric, sum, na.rm = TRUE) %>%
#   # ----
#   # MAE method ----
#   summarize_if(is.numeric, function(x) {
#                  total_reg <- sum(.$A1a, na.rm = TRUE)
#                  represented <- sum(.$A1a[!is.na(x)], na.rm = TRUE)
#                  frac_represented <- represented / total_reg
#                  ifelse(frac_represented >= 0.85, sum(x, na.rm = TRUE), NA)
#                 }
#     ) %>%
#   # -----
#   mutate(F1other = F1h + F1i + F1j) %>%
#   select(-A1a, -F1h, -F1i, -F1j) %>%
#   print(n = nrow(.))



# prop_methods_old <- raw_methods_old %>%
#   mutate(total = F1a,
#          prop_vote_inperson = F1b / total,
#          prop_vote_uocava = B8a / total,
#          prop_vote_civabs = C4a / total,
#          prop_vote_prov = F1e / total,
#          prop_vote_early = F1f / total,
#          prop_vote_mail = F1g / total,
#          prop_vote_other = F1other / total
#         ) %>%
#   select(-contains("F1"), -B8a, -C4a, -total) %>%
#   mutate_at(vars(contains("prop_v")), function(x) x * 100) %>%
#   rename_if(is.numeric, function(x) str_replace_all(x, "prop", "perc")) %>%
#   print(n = nrow(.))





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

# codebook in folder

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

count(cps, pes1)
cps %$%table(pes1, pes2)

count(cps, pes4, wt = pwsswgt) %>%
  filter(pes4 > 0) %>%
  add_tally(n) %>%
  mutate(p = n / nn)

# voting: check w/ MAE but probably code refusals and DK as nonvotes
# registration: same story
# double check eligibility with the other data source
# pct registered

cps_norms <- cps %>%
  mutate_all(labelled::remove_labels) %>% 
  mutate(voted = case_when(pes1 == 1 ~ 1,
                           pes1 == -1 ~ as.numeric(NA),
                           TRUE ~ 0),
         registered = case_when(voted == 1 ~ 1,
                                pes2 == 1 ~  1,
                                pes2 == -1 ~ as.numeric(NA),
                                TRUE ~ 0),
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
                             sum((nonvote_regproblems %in% c(0, 1)) * pwsswgt, 
                                 na.rm = TRUE),
            prop_nv_place_problem = sum((nonvote_placeproblems == 1) * pwsswgt, 
                                 na.rm = TRUE) / 
                             sum((nonvote_placeproblems %in% c(0, 1)) * pwsswgt, 
                                 na.rm = TRUE),
            prop_nv_ill_disabled = sum((nonvote_illdisable == 1) * pwsswgt, 
                                 na.rm = TRUE) / 
                             sum((nonvote_illdisable %in% c(0, 1)) * pwsswgt, 
                                 na.rm = TRUE)) %>%
  mutate_at(vars(starts_with("prop_")), function(x) x * 100) %>%
  rename_at(vars(starts_with("prop_")),
            function(x) str_replace(x, "prop", "pct")) %>%
  rename(inputstate = gestfips) %>%
  mutate(inputstate = as.integer(inputstate)) %>%
  left_join(select(census, inputstate, state_abb), .) %>%
  rename(State = state_abb) %>%
  select(State, matches("."), -inputstate) %>%
  print(n = nrow(.))




# supplement pct VEP registered using EAVS and USEP?
# rejoinder to Burden's MAE chapter conclusion?
mae_vep_registered <- read_csv("data/vep/usep-state-vep-2016.csv") %>%
  select(State, `Voting-Eligible Population (VEP)`) %>%
  setNames(c("state", "VEP")) %>% 
  left_join(census) %>% 
  select(state_abb, VEP) %>% 
  rename(State = state_abb) %>% 
  filter(State %in% c(state.abb, "District of Columbia")) %>%
  left_join(eavs %>% 
              select(State, A1a, wt) %>%
              group_by(State) %>% 
              mutate_if(is.numeric, function(x) ifelse(x < 0, NA, x)) %>%
              summarize(represented = sum(wt[!is.na(A1a)], na.rm = TRUE),
                        registered = sum(A1a, na.rm = TRUE)) %>%
              mutate(registered = ifelse(represented >= .85, registered, NA)) %>%
              select(-represented)) %>%
  mutate(pct_vep_registered_mae = 100 * registered / VEP) %>%
  print(n = nrow(.))


left_join(mae_vep_registered, cps_norms) %>%
  select(contains("registered")) %>%
  ggplot(aes(x = pct_vep_registered, y = pct_vep_registered_mae)) +
    geom_abline() +
    geom_point(aes(color = pct_vep_registered > 100 | 
                     pct_vep_registered_mae > 100),
               show.legend = FALSE) +
    labs(x = "CPS (registrants / eligible)", y = "EAVS (registrants) / USEP (VEP)",
         title = "Registered as Pct. of VEP") +
    coord_cartesian(xlim = c(50, 120), ylim = c(50, 120)) +
    scale_color_manual(values = c("black", "red")) +
    NULL


# also check state by state VRS summary
## Check the book again?


# --- data completeness -----------------------

# - check with Evan's "efficient clerks" - 
# Pew EPI's canonical 15 + 3
# new regs received (A5a), accepted (A5b), total (A1a)
# provisional ballots submitted (F1e),  rejected (E1d):
# total ballots in election (F1a), 
# in person (F1b), early voting center (F1f), cast absentee (C4a) + (B8a)
# civilian abs sent (C1a), returned (C1b), accepted (C4a), 
# UOCAVA sent (B1a), returned (B2a), counted (B8a), 
# 
# rejected registrations (A5e),
# rejected (C4b)
# rejected (B13a)
# 
# calculation:
# for each jurisdiction, the % of questions with valid values
# for each state, the (weighted?) average of jurisdiction values?

n_epi_items <- 17

completeness_jurisdiction <- readRDS(here("data/eavs/eavs-2016-unlabelled.RDS")) %>%
  group_by(State) %>% 
  mutate_if(is.numeric, function(x) ifelse(x < 0, NA, x)) %>%
  mutate(wt = A1a / sum(A1a, na.rm = TRUE)) %>%
  ungroup() %>%
  select(State, FIPSCode, FIPS_2Digit, JurisdictionName, wt,
         A5a, A5b, A1a, F1e, E1d, F1a, F1b, F1f, C4a,
         B8a, C1a, C1b, B1a, B1b, B8a, A5e, C4b, B13a) %>%
  group_by(State, FIPSCode, FIPS_2Digit, JurisdictionName, wt) %>%
  summarize_if(is.numeric, function(x)
               sum(!is.na(x))) %>%
  ungroup() %>%
  mutate(prop_valid = rowSums(select(., -(State:wt))) / n_epi_items) %>%
  select(State:wt, prop_valid) %>%
  print()


completeness_state <- completeness_jurisdiction %>%
  group_by(State) %>%
  summarize(eavs_data_completeness = sum(prop_valid * wt)) %>%
  print(n = nrow(.))



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
           absentee_returned + uocava_returned) %>% 
  left_join(pct_methods) %>%
  left_join(waits) %>% 
  left_join(polling_place) %>% 
  left_join(ballot_confidence) %>%
  left_join(usep) %>%
  left_join(cps_norms) %>% 
  left_join(completeness_state) %>%
  # names() %>% print()
  select(State, jurisdictions, joyce, 
         new_registrations_received, new_valid_registrations, total_registered_voters,
         contains("prov_ballots"), 
         total_ballots_cast,
         contains("ballots_cast"),
         absentee_sent, absentee_returned, absentee_counted,
         uocava_sent, uocava_returned, uocava_counted,
         new_registrations_rejected, absentee_rejected, uocava_rejected,
         vep_turnout,
         pct_vote_early, pct_vote_civabs, pct_vote_uocava, pct_vote_inperson, pct_vote_prov, pct_vote_mail, pct_vote_other,
         # need residual votes
         percent_electronic_sign_in:paper_ballot_booths,
         vep_turnout, pct_vep_registered, 
         pct_registrations_rejected, pct_nv_reg_problem,
         perc_turnout_prov, perc_prov_rej,
         pct_absentee_rejected, pct_absentee_not_returned,
         pct_uocava_rejected, pct_uocava_not_returned, 
         mean_wait,
         pct_nv_place_problem, pct_easy_polling_place, pct_nv_ill_disabled, pct_confident,
         eavs_data_completeness
         # , matches(".")
         ) %>%
  print()

# standardize names?
names(normalized)

# [x] jurisdictions
# [x] joyce indicator



# some potentially inconsistent/redundant data?
# select(normalized, pct_vote_early, perc_early_vote, pct_vote_inperson, perc_ed_vote) %>%
# print(n = nrow(.))

dir.create(here("output/normalized"))
write_csv(normalized, here("output/normalized/normalized-measures.csv"))

ls()

write_csv(completeness_jurisdiction, here("output/normalized/jurisdiction-eavs-completeness.csv"))


print("Normalized measures code completed without error")



