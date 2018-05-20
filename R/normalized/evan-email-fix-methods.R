
# --- Fixing things re: Evan's email -----------------------

# using correct columns for relative usage of voting methods
# some respondents did not enter duplicate quantities, so we use the original


ls()







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


compare_absentee <- total_absentee %>%
  select(State, contains("perc")) %>%
  print(n = nrow(.))

prop_methods %>%
  select(State, contains("uocava"), contains("absentee")) %>%
  mutate_if(is.numeric, function(x) 100 * x) %>%
  left_join(compare_absentee) %>%
  select(State, contains("uocava"), contains("absentee")) %>%
  print(n = nrow(.))

prop_methods
total_absentee
evan_file

evan_file %>%
  select(State, perc_ed_vote,  perc_turnout_prov, perc_early_vote) %>%
  left_join(total_absentee) %>%
  mutate(total_est =
           rowSums(select(., perc_ed_vote, perc_uocava, perc_civ_absentee,
                          perc_turnout_prov, perc_early_vote),
                   na.rm = TRUE)) %>%
  # select(State, total_est) %>%
  print(n = nrow(.))
