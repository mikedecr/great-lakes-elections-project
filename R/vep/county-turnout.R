# ----------------------------------------------------
#   REVISED: county VEP turnout
# ----------------------------------------------------

# directory at "box\ sync/PA/MIT" using terminal

library("magrittr")
library("tidyverse")
library("ggplot2")
theme_set(theme_bw())

options(scipen = 99999)


# ----------------------------------------------------
#   data sources
# ----------------------------------------------------

(joyce_states <- c("IL", "IN", "MI", "MN", "OH", "WI"))


# --- target -----------------------

# This is the level we want to merge to, overall.
# Contains FIPS codes for all counties

census <-
  read_delim("vep/data/census-fips-2010.txt",
             col_names = c("State", "state_fips",
                           "county_fips", "county_name",
                           "fips_class"),
             delim = ",") %>%
  filter(State %in% joyce_states) %>%
  mutate(fips = paste0(state_fips, county_fips),
         county_name = tolower(county_name)) %>%
  select(State, fips, state_fips, county_fips, matches(".")) %>%
  print()


# FIPS states
joyce_fips <- census %>%
  group_by(State, state_fips) %>%
  summarize() %>%
  print()




# Keep?
census %>%
  filter(State == "IL") %$%
  fips



# --- Numerators -----------------------

# EAVS (for all but WI)
#   create FIPS codes
#   aggregate to JurisdictionName
#   drop WI
# WI uses the administrative file(?) to correct wonky municipality boundaries


# to do tasks: Check each w/ census
# danville City's fips should be 17183 (vermilion county, not wabash)
# bloomington to McLean county (17113)
# peoria city into its county (17143)
#
# readRDS("eavs/2016/data/EAVS-2016-unlabelled.RDS") %>%
#   filter(State %in% joyce_states) %>%
#   filter(State != "WI") %>%
#   filter(str_detect(JurisdictionName, "CITY"))
#
#
# readRDS("eavs/2016/data/EAVS-2016-unlabelled.RDS") %>%
#   filter(State %in% joyce_states) %>%
#   filter(State != "WI") %>%
#   filter(str_detect(JurisdictionName, "MCLEAN"))




# read EAVS data
# keep joyce states minus WI
# Keep data on election participants (F1`x`)
# convert missing data to 0 (same as NA for the sake of aggregation)

# two ways to calculate participants:
#   F1a, or calculate all of the subcomponents (which might have error)

# extract bits of FIPS
# change some oopsies in IL

# Calculate jurisdiction-level votes(?)

eavs <- readRDS("eavs/2016/data/EAVS-2016-unlabelled.RDS") %>%
  filter(State %in% joyce_states) %>%
  filter(!(State %in% "WI")) %>%
  select(FIPSCode:JurisdictionName, contains("F1")) %>%
  mutate_if(is.numeric, function(x)
                        case_when(x %in% c(-888888, -999999) ~ 0,
                                  is.na(x) ~ 0,
                                  TRUE ~ x)) %>%
  mutate(votes = F1a,
         votes_calc = F1b + F1c + F1d + F1e + F1g + F1h + F1i + F1j) %>%
  mutate(fips = str_sub(FIPSCode, 1L, 5L),
         state_fips = str_sub(fips, 1L, 2L),
         county_fips = str_sub(fips, 3L, 5L)) %>%
  mutate(
    fips = case_when(
             State == "IL" & str_detect(JurisdictionName, "DANVILLE") ~ "17183",
             State == "IL" & str_detect(JurisdictionName, "BLOOMINGTON") ~ "17113",
             State == "IL" & str_detect(JurisdictionName, "PEORIA") ~ "17143",
             TRUE ~ fips)) %>%
  select(-contains("F1")) %>%
  group_by(State, fips, state_fips) %>%
  summarize(votes = sum(votes, na.rm = TRUE),
            votes_calc = sum(votes_calc, na.rm = TRUE)) %>%
  ungroup() %>%
  print()


# in no case is the calculates votes greater than the reported total
# so we should use `votes`
eavs %$% plot(votes - votes_calc)
eavs %$% sum(votes_calc > votes)






# --- Wisconsin administrative file -----------------------

# drop HINDI number; doesn't relate to FIPS
# join w/ Census fips code (anti_join shows no failures!)
wi <- readxl::read_excel("vep/data/Wisconsin_turnout.xlsx") %>%
  setNames(str_replace(names(.), " ", "_")) %>%
  select(County, Reporting_Unit, Total_Ballots, Total_Voters) %>%
  rename(county_name = County) %>%
  mutate(county_name = tolower(county_name)) %>%
  full_join(., filter(census, State == "WI"), by = "county_name") %>%
  group_by(State, fips, state_fips) %>%
  summarize(ballots = sum(Total_Ballots, na.rm = TRUE),
            participants = sum(Total_Voters, na.rm = TRUE)) %>%
  print()

# we do find some counties where more ballots than participants
wi %$% sum(ballots > participants)



# --- denominators -----------------------

# mcdonald data
denoms <- haven::read_dta("vep/data/207countyturnout20170629.dta") %>%
  filter(year == 2016) %>%
  mutate(fips = ifelse(nchar(fips2) == 4, paste0("0", fips2), fips2),
         state_fips = str_sub(fips, 1L, 2L),
         county_fips = str_sub(fips, 3L, 5L)) %>%
  rename(mcturnout = turnoutvep, mcvote = tvote) %>%
  select(fips, state_fips, mcvote, vep, vepnov, mcturnout) %>%
  filter(state_fips %in% joyce_fips$state_fips) %>%
  print()



# ----------------------------------------------------
#   combine
# ----------------------------------------------------

# Join McDonald denominators to Census FIPS

# anti_join shows no merging problems for Census and McDonald
anti_join(census, denoms)
anti_join(denoms, census)

# no problems with Census and Wisconsin
anti_join(filter(census, State == "WI"), wi)
anti_join(wi, filter(census, State == "WI"))

# 5 units in EAVS that aren't in Census.
# This means that we won't place them in the appropriate county
# so we will need to find their counties and write over
anti_join(filter(census, State != "WI"), eavs)
anti_join(eavs, filter(census, State != "WI"))



# merge census, McDonald, EAVS, WI file
#   remove fips class from census when joining McDonald
master <-
  left_join(census, denoms) %>%
  select(-fips_class) %>%
  select(State, county_name, fips, state_fips, county_fips,
         mcvote, vep, vepnov, mcturnout) %>%
  left_join(wi) %>%
  left_join(eavs) %>%
  print()












# --- calculate turnouts -----------------------
to_init <- master %>%
  mutate(num = case_when(!is.na(ballots) ~ ballots,
                         !is.na(votes) ~ votes),
         denom = vepnov,
         turnout = num/denom) %>%
  select(-ballots, -votes_calc, -votes) %>%
  print()




# ----------------------------------------------------
#   fixing some bad data
# ----------------------------------------------------

# two problems:
# 1) data that are there are a little sloppy
# 2) non-joined IL jurisdictions will mess up their respective counties

# problem (1) we can see here
ggplot(to_init, aes(x = mcturnout, y = turnout)) +
  geom_point() +
  geom_abline() +
  facet_wrap(~ State)


# problem 2: non-matched EAVS FIPS codes
(missing_fips_eavs <- anti_join(eavs, master)$fips)

il <- readRDS("eavs/2016/data/EAVS-2016-unlabelled.RDS") %>%
  filter(State == "IL") %>%
  select(FIPSCode, JurisdictionName) %>%
  print()

lapply(missing_fips_eavs, function(x) filter(il, str_detect(FIPSCode, x))) %>%
  bind_rows()


# counties w/ data very inconsistent with McDonald
# Here are their 'officially reported' turnout figures

# IL:
  # jasper: 5112
  # putnam: 3112
  # cook: 2180344
  # knox: 22503
  # winnebago: 120055
  # kane: 199687
  # edwards: 3324
  # dekalb: 43600
  # stark: 2743
  # pike: 7530
  # massac: 6697
# IN:
  # clinton: 11936
  # dearborn: 11936
  # switzerland: 3699
# WI:
  # sawyer: 9137

# Counties that contain irregular jurisdictions ---

#   Bloomington city (should be in MCLEAN, n = 46129)
#      !!!! differs wildly from McDonald !!!

#   Galesburg City (should be in VERMILION, n = 20637 out of r = 30430)
#       !! looks like McD has R as numerator? !!

#   East St. Louis City (st clair county: 122936)
#   Peoria City (should be in peoria county?, n = 76952)

#   Aurora City (Kane [done], Dupage [435143], will [304,167], kendall [51,979])
 #   Chicago city (look up Cook [2,180,344]and DuPage separately?)
#   Rockford City (Winnebago [66,898] and Ogle [can't find general results?])

filter(to_init, State == "IL" & str_detect(county_name, "mclean"))
filter(to_init, State == "IL" & str_detect(county_name, "vermilion"))
filter(to_init, State == "IL" & str_detect(county_name, "peoria"))
filter(to_init, State == "IL" & str_detect(county_name, "dupage"))
filter(to_init, State == "IL" & str_detect(county_name, "will"))
filter(to_init, State == "IL" & str_detect(county_name, "kendall"))
filter(to_init, State == "IL" & str_detect(county_name, "cook"))
filter(to_init, State == "IL" & str_detect(county_name, "ogle"))
filter(to_init, State == "IL" & str_detect(county_name, "winnebago"))

filter(to_init, State == "IL") %>%
  count(county_name)

to <- to_init %>%
  mutate(
    num = case_when(
      State == "IL" & str_detect(county_name, "jasper") ~ 5112,
      State == "IL" & str_detect(county_name, "putnam") ~ 3112,
      State == "IL" & str_detect(county_name, "cook") ~ 2180344,
      State == "IL" & str_detect(county_name, "knox") ~ 22503,
      State == "IL" & str_detect(county_name, "winnebago") ~ 120055,
      State == "IL" & str_detect(county_name, "kane") ~ 199687,
      State == "IL" & str_detect(county_name, "edwards") ~ 3324,
      State == "IL" & str_detect(county_name, "dekalb") ~ 43600,
      State == "IL" & str_detect(county_name, "stark") ~ 2743,
      State == "IL" & str_detect(county_name, "pike") ~ 7530,
      State == "IL" & str_detect(county_name, "massac") ~ 6697,
      State == "IN" & str_detect(county_name, "clinton") ~ 11936,
      State == "IN" & str_detect(county_name, "dearborn") ~ 23987,
      State == "IN" & str_detect(county_name, "switzerland") ~ 3699,
      State == "WI" & str_detect(county_name, "sawyer") ~ 9137,
      # -- questionable ones (differ from McDonald)
      State == "IL" & str_detect(county_name, "mclean") ~ 46129,
      State == "IL" & str_detect(county_name, "vermilion") ~ 20637,
      # ----
      State == "IL" & str_detect(county_name, "st. clair") ~ 122936,
      State == "IL" & str_detect(county_name, "peoria") ~ 76952,
      State == "IL" & str_detect(county_name, "dupage") ~ 435143,
      State == "IL" & county_name == "will county" ~ 304167,
      State == "IL" & county_name == "kendall county" ~ 51979,
      State == "IL" & county_name == "cook county" ~ 2180344,
      State == "IL" & county_name == "winnebago county" ~ 66898
      State == "IL" & county_name == "ogle county" ~ 66898
      TRUE ~ num),
    turnout = num/denom) %>%
  print()

filter(to, State == "IN" & str_detect(county_name, "dearborn"))








# ----------------------------------------------------
#   visualizing
# ----------------------------------------------------

ggplot(to, aes(x = mcturnout, y = turnout)) +
  geom_abline() +
  geom_point(shape = 1) +
  facet_wrap(~ State) +
  labs(x = "McDonald Estimate",
       y = "Our Estimate\n(using total ballots and projected November VEP)")


dir.create("vep/graphics")

ggsave("vep/graphics/turnout-comparison-postfix.pdf", height = 5, width = 7)


# in case you want to identify individual counties

to %>%
filter(State == "IL") %>%
plot(turnout ~ mcturnout, data = .,
     ylim = c(0, 1),
     xlim = c(0, 1))
abline(a = 0, b = 1)

to %>%
filter(State == "IL") %$%
identify(turnout ~ mcturnout, labels = county_name)
# must press `escape` when plot window is selected to cancel the identify() command


# do we have turnout estimates for everything?
to %>%







# --- save turnout -----------------

final_to <- to %>%
  rename(mcd_numerator = mcvote,
         GL_numerator = num
         GL_denominator = denom,
         GL_turnout = turnout) %>%
  mutate(mcd_turnout = mcd_numerator / vep,
         mcd_turnout_nov = mcd_numerator / vepnov) %>%
  select(State:mcd_numerator, vep, vepnov,
         contains("mcd_turnout"), contains("GL")) %>%
  print()


list.files("vep")
dir.create("vep/output")
write_csv(to, "vep/output/vep-turnout-joyce.csv")













# ----------------------------------------------------
#   try with Wisconsin EAVS data
# ----------------------------------------------------
to


eavswi <- readRDS("vep/data/wisconsin-eavs-estimate.RDS") %>%
  mutate(State = "WI") %>%
  print()

to_plus <- to %>%
  left_join(., eavswi, by = c("State", "county_name")) %>%
  mutate(eavs = case_when(State == "WI" ~ contrib_regwt,
                          TRUE ~ num),
         turnout_eavs = eavs / denom) %>%
  print()



ggplot(to_plus, aes(x = mcturnout, y = turnout_eavs)) +
  geom_abline() +
  geom_point(shape = 1) +
  facet_wrap(~ State) +
  labs(x = "McDonald Estimate",
       y = "Our Estimate\n(using total ballots and projected November VEP)")

to_plus %>%
  filter(State == "WI") %>%
  gather(key = turnout_var, value = turnout, turnout, turnout_eavs) %>%
  mutate(turnout_var = ifelse(turnout_var == "turnout_eavs",
                              "Allocated EAVS", "Raw WEC File")) %>%
  ggplot(aes(x = mcturnout, turnout)) +
    facet_grid(. ~ turnout_var) +
    geom_abline() +
    geom_point(shape = 1) +
    labs(x = "McDonald Estimate",
         y = "Our Estimates")

ggsave("vep/graphics/compare-wisconsin-methods.pdf", height = 3, width = 6)





to_plus %>%
  filter(State == "WI") %>%
  plot(turnout_eavs ~ mcturnout, data = .)
abline(a = 0, b = 1)



# to_plus %>%
#   filter(State == "WI") %$%
#   identify(turnout_eavs ~ mcturnout, labels = county_name)
