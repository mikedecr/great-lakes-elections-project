# ----------------------------------------------------
#   Trying to figure out county VEP turnout
#   (using McDonald county VEP data)
# ----------------------------------------------------


# run EAVS build file first




setwd("~/box sync/pa/mit")
source("eavs/2016/EAVS-2016-build.R")

setwd("~/box sync/pa/mit")




rm(list = ls())


# EAVS numerator
nums <- readRDS("eavs/2016/data/eavs-2016-unlabelled.RDS") %>% 
        select(FIPSCode:JurisdictionName, contains("F1")) %>% 
        mutate(fips2 = str_sub(FIPSCode, 1L, 5L),
               fips2 = as.numeric(fips2),
               state_fips = str_sub(FIPSCode, 1L, 2L)) %>% 
        mutate_if(is.numeric, function(x) 
                              case_when(x %in% c(-888888, -999999) ~ 0,
                                        is.na(x) ~ 0,
                                        TRUE ~ x)) %>% 
        print()


# wisconsin election commission voting data
# variable names are funky but it's okay, we only want some of these
# make fips for county

wi <- readxl::read_excel("vep/data/Wisconsin_turnout.xlsx") %>% 
  setNames(str_replace(names(.), " ", "_")) %>% 
  select(HINDI, County, Reporting_Unit, Total_Ballots, Total_Voters) %>% 
  mutate(County = tolower(County),
         fips = as.character(HINDI),
         fips = case_when(nchar(fips) == 4 ~ paste0("00", fips),
                          TRUE ~ paste0("0", fips)),
         fips = paste0("55", fips),
         sc_fips = str_sub(fips, 1L, 5L),
         num_fips = as.numeric(sc_fips)) %>%
  print()




# denominators

denoms <- haven::read_dta("vep/data/207countyturnout20170629.dta") %>% 
          filter(year == 2016) %>% 
          mutate(state = str_sub(fips2, 1L, 2L)) %>% 
          select(state, fips2, 
                 # countyname, 
                 tvote, vep, vepnov, turnoutvep) %>% 
          mutate(sc_fips = as.character(fips2)) %>% 
          print()


write.csv(denoms, "vep/data/mcdonald-county-vep.csv")




# use census fips and county names for merging

census_fips <- read_delim("vep/data/census-fips-2010.txt", 
                          col_names = c("State", "state_fips", "county_fips", "county_name", "class_fips"), 
                          delim = ",") %>% 
                mutate(sc_fips = paste0(state_fips, county_fips)) %>% 
                print()



joyce_fips <- filter(census_fips, 
                     State %in% c("IL", "IN", "MI", "MN", "OH", "WI")) %>%
  select(State, state_fips) %>%
  group_by(State) %>%
  summarize(state_fips = unique(state_fips)) %>%  
  print()







# --- maybe don't read ----------------------

# cw <- readxl::read_excel("vep/data/20122020_Election_Data_with_2011_Wards.xlsx") %>%
#   print()

# head(cw$CNTY_FIPS)

# wi_counties <- haven::read_dta("~/box sync/pa/mit/eavs/pettigrew-2012/2012 eavs cleaning code/eavs/sectionA/stata_files/wisconsinfips.dta") %>%
#   distinct(countyfips, countyname) %>%
#   print()


# pwi <- haven::read_dta("~/box sync/pa/mit/eavs/pettigrew-2012/2012 eavs cleaning code/eavs/sectionA/stata_files/wisconsinfips.dta") %>%
#   # mutate(last5 = str_sub(fipscode, start = -5L, end = -1L),
#   #        countyname = paste(countyname, "COUNTY")) %>% 
#   # arrange(last5) %>% 
#   print()


# jurisdiction: cousubfp
# county: cnty_fips
# 
# old process: 
# merge EAVS ward to CW jurisdiction using eavs::fips and cw::cousubfp
# "Allocating the reporting jurisdiction count for a given EAVS question across wards in that reporting jurisiction proportional to the population of the ward 18+ (PERSONS18)." wtf
# aggregate within county


# I'm thinking we should only do this risky merging business
# for multiple-county jurisdictions
# else do clean county aggregation where we can.

# --- end -----------------------












# ----------------------------------------------------
#   Wisconsin Turnout
#   using WI file (numerators) and McDonald file (denominators)
# ----------------------------------------------------

# The Wisconsin data has an identifier column that looks like a FIPS but isn't
# so merge on county name instead (dumb)
# How:  clean the name column in the Census FIPS file 
#       merge Census with Wisconsin to get valid FIPS codes

census_wi <- census_fips %>%
  filter(State == "WI") %>%
  mutate(county_name = tolower(county_name)) %>%
  select(State, sc_fips, county_name) %>% 
  print()

wi_nums <- wi %>%
  group_by(County) %>%
  summarize(total_ballots = sum(Total_Ballots, na.rm = TRUE),
            total_voters = sum(Total_Voters, na.rm = TRUE)) %>%
  rename(county_name = County) %>% 
  left_join(census_wi, ., by = "county_name") %>%
  print()


# combine w/ Wisconsin denominators from McDonald
# calculate different turnouts (ballots vs voters, vep vs vepnov)

# most conceptually accurate is probably november denominator with VotERS numerator
# most *consistent with other data* is probably ballots with november denominator

wi_to <- denoms %>%
  filter(state == "55") %>%
  select(-state, -fips2) %>%
  left_join(wi_nums, ., by = "sc_fips") %>%
  mutate(to_b = total_ballots / vep,
         to_b_nov = total_ballots / vepnov,
         to_v = total_voters / vep,
         to_v_nov = total_voters / vepnov,
         state_fips = "55") %>% 
  print()






wi_to %>%
  gather(key = measure, value = value, contains("to_")) %>% 
  ggplot(aes(x = turnoutvep, y = value)) +
    geom_abline() +
    geom_point(shape = 1) +
    facet_wrap(~ measure) 


# look at outlier counties
wi_to %>%
  mutate(diff = round(to_b_nov - turnoutvep, 3)) %>%
  as.data.frame()

# compare wonky counties to EAVS?




# --- numerators for every other state -----------------------


joyce_votes <- nums %>%
  select(-contains("Other"), -contains("Comments")) %>% 
  filter(state_fips %in% joyce_fips$state_fips) %>%
  filter(State != "WI") %>% 
  mutate(sc_fips = as.character(fips2),
         calc_num = F1b + F1c + F1d + F1e + F1g + F1h + F1i + F1j) %>% 
  select(state_fips, sc_fips, JurisdictionName, F1a, calc_num) %>% 
  print()


nonwi_to <- denoms %>%
  select(-fips2, -tvote) %>% 
  filter(state %in% joyce_fips$state_fips) %>%
  left_join(joyce_votes, ., by = "sc_fips") %>%
  mutate(to_eavs = F1a / vep,
         to_eavs_nov = F1a / vepnov,
         to2 = calc_num / vepnov) %>%
  print()


nonwi_to %$% table(is.na(vep), exclude = NULL)
nonwi_to %$% table(is.na(to_eavs), exclude = NULL)
nonwi_to %$% table(is.na(to_eavs_nov), exclude = NULL)
nonwi_to %$% table(is.na(to2), exclude = NULL)


nonwi_to %>%
  gather(key = measure, value = value, contains("to")) %>%
  ggplot(aes(x = turnoutvep, y = value)) +
    geom_abline() +
    geom_point(shape = 1, aes(color = value > 1)) +
    facet_wrap(~ measure) +
    scale_color_manual(values = c("gray", "red"))



# which cases aren't fitting?
nonwi_to[!complete.cases(nonwi_to), ]



# no longer have a > 100% issue for one county, 
# but using the sum definitely is a huge bias off the line


# --- all great lakes turnout table -----------------------


to_gl <- nonwi_to %>%
  left_join(., joyce_fips, by = "state_fips") %>%
  rename(vepturnout = to_eavs_nov, mcd_turnout = turnoutvep) %>%
  select(State, state_fips, sc_fips, vepturnout, mcd_turnout, to2) %>%
  bind_rows(wi_to %>% 
            rename(vepturnout = to_b_nov, mcd_turnout = turnoutvep) %>%
            select(State, state_fips, sc_fips, vepturnout, mcd_turnout)) %>%
  print()





# error vs McDonald explained by error from column sum?
to_gl %>%
  mutate(vs_mcd = vepturnout - mcd_turnout,
         vs_calc = to2 - vepturnout) %>%
  ggplot(aes(x = vs_calc, y = vs_mcd)) +
    geom_hline(yintercept = 0) +
    geom_point(shape = 1) +
    facet_wrap(~ State, scales = "free") +
    labs(x = "Error from Summing",
         y = "Error from McDonald")
# seems like No



to_gl %>%
  mutate(vs_mcd = vepturnout - mcd_turnout,
         vs_calc = to2 - vepturnout) %>%
  filter(State == "IL") %>% 
  select(sc_fips, vs_mcd) %>% 
  as.data.frame()





# how many counties don't match?
census_fips %>%
filter(State %in% joyce_fips$State) %>%
anti_join(to_gl, ., by = "sc_fips")




#  custom ggtheme, import from Github
source("https://raw.githubusercontent.com/mikedecr/theme-mgd/master/theme_mgd.R")
theme_set(theme_mgd()) # set default theme
library("extrafont")   # load font faces into R


ggplot(data = to_gl, aes(x = mcd_turnout, y = vepturnout)) +
  geom_hline(yintercept = 1) +
  geom_abline(size = 0.25) +
  geom_point(shape = 1) +
  facet_wrap(~ State) +
  scale_y_continuous(breaks = seq(0, 1.25, .25)) +
  labs(x = "McDonald Turnout",
       y = "EAVS Turnout",
       caption = "* Wisconsin numerator from WEC file (not EAVS)")


# Illinois is a hot mess





# ----------------------------------------------------
#   ways to fix IL
# ----------------------------------------------------

# get info from cities and counties in IL and try to fix things ourselves
# same with weird outliers in IN and WI
# maybe call people (like Jasper IL) if we can't find online





 
wi %>%
mutate(county_fips = as.numeric(fips))



wi %>%
left_join(filter(denoms, state == 55)



# --- unclear about how to get these FIPS to work -----------------------

nums %>%
filter(State %in% joyce_states) 

denoms %$% 
table(state)

cw %>%
select(COUSUBFP, CNTY_FIPS, PERSONS18) %>%
mutate(FIPSCode = as.character(COUSUBFP)) %>% 
left_join(., filter(nums, State == "WI"), by = "FIPSCode")






cw %$% length(table(COUSUBFP, exclude = NULL))

nums %>%
filter(State == "WI")

cw %>%
filter(str_detect(NAME, "Abbotsford")) %$% 
table(CNTY_NAME, exclude = NULL)


nums %>%
filter(State == "WI") %>%
filter(str_detect(JurisdictionName, "MULTIPLE"))


length(unique(cw$COUSUBFP))




cw %>%
filter(SPLIT == "YES") %$% 
length(unique(COUSUBFP))






















summary(denoms$fips2)
summary(nums$fips2)


d <- inner_join(nums, denoms, by = "fips2") %>% 
     mutate(vepcalc = F1a / vep,
            vepnovcalc = F1a / vepnov) %>% 
     print()


d %>%
group_by(State) %>%
summarize(n = n()) %>%
left_join(data_frame(State = state.abb), .) %>%
as.data.frame() 


denoms %>%
group_by() 

names(denoms)
YEAR
FIPS2
COUNTYNAME
TVOTE
VEP
VEPNOV



