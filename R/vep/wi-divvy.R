# ----------------------------------------------------
#   County-level turnout data using lower-level data
#   and a method for allocating jurisdictions into counties
# ----------------------------------------------------

# intuition:
# EAVS data is nested within counties

library("magrittr")
library("tidyverse")
library("ggplot2")
theme_set(theme_bw())

options(scipen = 99999)



# --- target -----------------------
# filter Census data to only WI
census <-
  read_delim(here("data/census/census-fips-2010.txt"),
             col_names = c("State", "state_fips",
                           "county_fips", "county_name",
                           "fips_class"),
             delim = ",") %>%
  filter(State == "WI") %>%
  mutate(fips = paste0(state_fips, county_fips),
         county_name = tolower(county_name)) %>%
  select(State, fips, state_fips, county_fips, matches(".")) %>%
  print()


# --- Wisconsin EAVS data-----------------------
# calculate ballot total
# (fix strings)
eavs <- readRDS("data/eavs/EAVS-2016-unlabelled.RDS") %>%
  filter(State == "WI") %>%
  select(FIPSCode:JurisdictionName, contains("F1")) %>%
  mutate_if(is.numeric, function(x)
                        case_when(x %in% c(-888888, -999999) ~ 0,
                                  is.na(x) ~ 0,
                                  TRUE ~ x)) %>%
  mutate(votes = F1a,
         votes_calc = F1b + F1c + F1d + F1e + F1g + F1h + F1i + F1j,
         JurisdictionName = tolower(JurisdictionName),
         municipality = sapply(str_split(JurisdictionName, pattern = " - ") , function(x) x[1]),
         county_name = sapply(str_split(JurisdictionName, pattern = " - ") , function(x) x[2])) %>%
  select(-contains("F1"), -contains("FIPS")) %>%
  print()

# some of these counties say "multiple counties"




# --- WI Elections Commission data file (use for weights) -----------

# drop HINDI number; doesn't relate to FIPS
# each reporting unit gets a weight to apply to the eavs data
wec <- readxl::read_excel(here("data/vep/Wisconsin_turnout.xlsx")) %>%
  setNames(str_replace_all(tolower(names(.)), " ", "_")) %>%
  mutate(county_name = tolower(county),
         municipality = tolower(municipality),
         registrants = registrants + late_registrants + election_day_registrants) %>%
  select(county_name, municipality, reporting_unit, registrants, total_ballots) %>%
  group_by(county_name) %>%
  mutate(reg_weight = registrants / sum(registrants),
         ballot_weight = total_ballots / sum(total_ballots)) %>%
  print()


# --- split wisconsin data -----------------------

# split:
# weight each Jurisdiction's data by the WEC weight
# sum within counties to find those units' contribution to the county total
mults <- eavs %>%
  filter(county_name == "multiple counties") %>%
  rename(multiple = county_name) %>%
  inner_join(wec, ., by = "municipality") %>%
  mutate(contrib_ballotwt = votes * ballot_weight,
         contrib_regwt = votes * reg_weight) %>%
  group_by(county_name) %>%
  summarize(contrib_ballotwt = sum(contrib_ballotwt),
            contrib_regwt = sum(contrib_regwt)) %>%
  print()


# nested counties:
# sum within county for contribution to county total
wholes <- eavs %>%
  filter(county_name != "multiple counties") %>%
  group_by(county_name) %>%
  summarize(contrib = sum(votes)) %>%
  print()


# --- combine -----------------------

# merge Wisconsin jurisdictions into the census frame
# add contributions from nested and split munis into one column
target <- census %>%
  left_join(., mults, by = "county_name") %>%
  left_join(., wholes, b = "county_name") %>%
  mutate_at(vars(contains("wt")), function(x) ifelse(is.na(x), 0, x)) %>%
  mutate(contrib_ballotwt = contrib_ballotwt + contrib,
         contrib_regwt = contrib_regwt + contrib) %>%
  select(county_name, contains("contrib")) %>%
  print()


saveRDS(target, here("data/vep/wisconsin-eavs-estimate.RDS"))
