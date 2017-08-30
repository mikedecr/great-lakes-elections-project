#----------------------------------------
#   EAVS 2016
#   Aggregate sections A through C
#----------------------------------------

rm(list = ls())

library("magrittr")
library("tidyverse")
library("ggplot2")
library("forcats")
library("stringr")


#  custom ggtheme, import from Github
source("https://raw.githubusercontent.com/mikedecr/theme-mgd/master/theme_mgd.R")
theme_set(theme_mgd()) # set default theme
library("extrafont")   # load font faces into R


# ----------------------------------------------------
#   roadmap
# ----------------------------------------------------

# read data
# fix FIPS for WI and IL
# aggregate to state
# aggregate to county and equivalents




# ----------------------------------------------------
#   read data, keep only the Joyce states
# ----------------------------------------------------

joyce_states <- c("IL", "IN", "MI", "MN", "OH", "WI") %>% print


d <- list(A = readRDS("data/clean/clean-EAVS-2016-A.RDS"),
          B = readRDS("data/clean/clean-EAVS-2016-B.RDS"),
          C = readRDS("data/clean/clean-EAVS-2016-C.RDS")) %>%
     reduce(full_join, by = c("FIPSCode", "FIPS_2Digit", "State", "JurisdictionName")) %>% 
     filter(State %in% joyce_states) %>% 
     as_data_frame() %>% 
     print() 

names(d)




# ----------------------------------------------------
#   Auxiliary County-level FIPS file
# ----------------------------------------------------

# maybe this will come in handy
# if only for checking that the counties in the EAVS are correct FIPS

# county FIPS comes from 
# https://www.census.gov/geo/reference/codes/cou.html
# which was then saved in a text file

list.files("data")
census_fips <- read_delim("data/census-fips-2010.txt", 
                          col_names = c("State", "state_fips", "county_fips", "county_name", "class_fips"), 
                          delim = ",") %>% 
                as.data.frame() %>%
                print()




# ----------------------------------------------------
#   load Pettigrew data
# ----------------------------------------------------

# Pettigrew data has a match for fips counties in Wisconsin

# Wisconsin FIPS supplement
pwi <- 
  haven::read_dta("~/box sync/pa/mit/eavs/pettigrew-2012/2012 eavs cleaning code/eavs/sectionA/stata_files/wisconsinfips.dta") %>%
  mutate(last5 = str_sub(fipscode, start = -5L, end = -1L),
         countyname = paste(countyname, "COUNTY")) %>% 
  arrange(last5) %>% 
  print


# Section "A" from Pettigrew
pa <- 
  haven::read_dta("~/box sync/pa/mit/eavs/pettigrew-2012/2012 eavs cleaning code/eavs/sectionA/stata_files/2012_eavs_section_a_original.dta") %>% 
  filter(state %in% joyce_states) %>% 
  print



# ----------------------------------------------------
#   understand dirty fips codes
# ----------------------------------------------------


# find "dirty" fips codes in each state: Pettigrew data
# (sub-county detail, doesn't just end with "00000") 

pa %>%
mutate(last5 = str_sub(fipscode, start = -5L, end = -1L)) %>%
select(state, jurisdiction, fipscode, last5) %$% 
table(state, last5 == "00000", exclude = NULL)


# In 2016 data...

d %>%
mutate(good_ending = str_detect(FIPSCode, "00000")) %>% 
select(State, FIPSCode, good_ending) %$% 
table(State, good_ending, exclude = NULL)


# we both find Wisconsin to be a problem
# but Wisconsin is aggregated at different levels in each year
# 2012 has more wards than 2016 
# 2016 appears to have things aggregated slightly higher
filter(pa, state == "WI")
filter(d, State == "WI")


# plus these five jurisdictions in Illinois
filter(d, State == "IL" & str_detect(FIPSCode, "00000") == FALSE)
filter(pa, state == "IL" & str_detect(fipscode, "00000") == FALSE)




# ----------------------------------------------------
#   Fix 2016 Wisconsin counties
# ----------------------------------------------------

# create countynames from Wisconsin data
county_wi <- 
	filter(d, State == "WI") %>% 
	select(State, JurisdictionName, FIPSCode) %>% 
	mutate(countyname = str_split(JurisdictionName, " - ") %>% 
	             lapply(., function(x) x[2]) %>%
	             as.character) %>%
	print()

# which jurisdictions split counties	
multiple_wi <- 
  filter(county_wi, countyname == "MULTIPLE COUNTIES") %>% 
  print()


# I have one extra countyname than Pettigrew: "MULTIPLE COUNTIES"
nlevels(as.factor(pwi$countyname))
nlevels(as.factor(county_wi$countyname))


# can we match FIPS to countyfips in Pettigrew data?
filter(pwi, last5 %in% county_wi$FIPSCode)
filter(pwi, last5 %in% multiple_wi$FIPSCode)
# seemingly not. 
# the FIPS in 2016 Wisconsin are sub-county. 
# Not sure how this happened but it's trash.

# but can probably match county names
# this drops some cases, but they are "multiple"
filter(county_wi, countyname %in% pwi$countyname)


filter(county_wi, !(countyname %in% pwi$countyname)) %$% 
table(countyname, exclude = NULL)

# do "multiple" counties make up the difference in these dataset matches?
nrow(filter(county_wi, countyname %in% pwi$countyname)) + nrow(filter(county_wi, !(countyname %in% pwi$countyname))) == nrow(county_wi)
# yes if TRUE




# this blows up. Do we need this?

county_wi_fips <- 
		pwi %>%
		select(countyfips, countyname) %>% 
		left_join(county_wi, ., by = "countyname") %>%
		print()

# includes 72 + multiple
county_wi_fips %$% table(countyname, exclude = NULL)





# add potential counties to the frame of "multiples"

multiple_wi <- 
		multiple_wi %>%
		mutate(counties = 
		         case_when(
		           str_detect(JurisdictionName, "ABBOTSFORD") ~ "CLARK, MARATHON", 
		           str_detect(JurisdictionName, "APPLETON") ~ "OUTAGAMIE, CALUMET, WINNEBAGO", 
		           str_detect(JurisdictionName, "ASHLAND") ~ "ASHLAND, BAYFIELD", 
		           str_detect(JurisdictionName, "BAYSIDE") ~ "MILWAUKEE, OZAUKEE",
		           str_detect(JurisdictionName, "BELLEVILLE") ~ "DANE, GREEN",
		           str_detect(JurisdictionName, "BERLIN") ~ "GREEN LAKE, WAUSHARA",
		           str_detect(JurisdictionName, "BIRNAMWOOD") ~ "MARATHON, SHAWANO",
		           str_detect(JurisdictionName, "BLANCHARDVILLE") ~ "IOWA, LAFAYETTE",
		           str_detect(JurisdictionName, "BRODHEAD") ~ "GREEN, ROCK",
		           str_detect(JurisdictionName, "BROOKLYN") ~ "DANE, GREEN",
		           str_detect(JurisdictionName, "BURLINGTON") ~ "RACINE, WALWORTH",
		           str_detect(JurisdictionName, "CAMBRIDGE") ~ "DANE, JEFFERSON",
		           str_detect(JurisdictionName, "CAZENOVIA") ~ "RICHLAND, SAUK",
		           str_detect(JurisdictionName, "COLBY") ~ "CLARK, MARATHON",
		           str_detect(JurisdictionName, "COLUMBUS") ~ "COLUMBIA, DODGE",
		           str_detect(JurisdictionName, "CUBA CITY") ~ "GRANT, LAFAYETTE",
		           str_detect(JurisdictionName, "DE SOTO") ~ "CRAWFORD, VERNON",
		           str_detect(JurisdictionName, "DORCHESTER") ~ "CLARK, MARATHON",
		           str_detect(JurisdictionName, "EAU CLAIRE") ~ "CHIPPEWA, EAU CLAIRE",
		           str_detect(JurisdictionName, "EDGERTON") ~ "ROCK, DANE",
		           str_detect(JurisdictionName, "GENOA CITY") ~ "KENOSHA, WALWORTH",
		           str_detect(JurisdictionName, "HARRISON") ~ "CALUMET, OUTAGAMIE",
		           str_detect(JurisdictionName, "HARTFORD") ~ "WASHINGTON, DODGE",
		           str_detect(JurisdictionName, "HAZEL GREEN") ~ "GRANT, LAFAYETTE",
		           str_detect(JurisdictionName, "HOWARD") ~ "BROWN, OUTAGAMIE",
		           str_detect(JurisdictionName, "KAUKAUNA") ~ "OUTAGAMIE, CALUMET",
		           str_detect(JurisdictionName, "KEWASKUM") ~ "WASHINGTON, FOND DU LAC",
		           str_detect(JurisdictionName, "KIEL") ~ "CALUMET, MANITOWOC",
		           str_detect(JurisdictionName, "LAC LA BELLE") ~ "WAUKESHA, JEFFERSON",
		           str_detect(JurisdictionName, "LIVINGSTON") ~ "GRANT, IOWA",
		           str_detect(JurisdictionName, "MARION") ~ "SHAWANO, WAUPACA",
		           str_detect(JurisdictionName, "MARSHFIELD") ~ "WOOD, MARATHON",
		           str_detect(JurisdictionName, "MENASHA") ~ "WINNEBAGO",
		           str_detect(JurisdictionName, "MILLADORE") ~ "WOOD, PORTAGE",
		           str_detect(JurisdictionName, "MILWAUKEE") ~ "MILWAUKEE, WASHINGTON, WAUKESHA",
		           str_detect(JurisdictionName, "MONTFORT") ~ "GRANT, IOWA",
		           str_detect(JurisdictionName, "MUKWONAGO") ~ "WAUKESHA, WALWORTH",
		           str_detect(JurisdictionName, "MUSCODA") ~ "GRANT, IOWA",
		           str_detect(JurisdictionName, "NEW AUBURN") ~ "BARRON, CHIPPEWA",
		           str_detect(JurisdictionName, "NEWBURG") ~ "OZAUKEE, WASHINGTON",
		           str_detect(JurisdictionName, "NEW LONDON") ~ "OUTAGAMIE, WAUPACA", 
		           str_detect(JurisdictionName, "ONTARIO") ~ "VERNON, MONROE",
		           str_detect(JurisdictionName, "PULASKI") ~ "BROWN, OCONTO, SHAWANO",
		           str_detect(JurisdictionName, "RANDOLPH") ~ "COLUMBIA, DODGE",
		           str_detect(JurisdictionName, "RIVER FALLS") ~ "PIERCE, ST. CROIX",
		           str_detect(JurisdictionName, "ROCKLAND") ~ "LA CROSSE, MONROE",
		           str_detect(JurisdictionName, "SPRING VALLEY") ~ "PIERCE, ST. CROIX",
		           str_detect(JurisdictionName, "STANLEY") ~ "CHIPPEWA, CLARK",
		           str_detect(JurisdictionName, "TURTLE LAKE") ~ "BARRON, POLK",
		           str_detect(JurisdictionName, "UNITY") ~ "CLARK, MARATHON",
		           str_detect(JurisdictionName, "VIOLA") ~ "RICHLAND, VERNON",
		           str_detect(JurisdictionName, "WATERTOWN") ~ "DODGE, JEFFERSON",
		           str_detect(JurisdictionName, "WAUPUN") ~ "DODGE, FOND DU LAC",
		           str_detect(JurisdictionName, "WHITEWATER") ~ "WALWORTH, JEFFERSON",
		           str_detect(JurisdictionName, "WISCONSIN DELLS") ~ "COLUMBIA, SAUK, ADAMS, JUNEAU",
		           str_detect(JurisdictionName, "WRIGHTSTOWN") ~ "BROWN, OUTAGAMIE"))

multiple_wi$counties																			

multiple_wi %$% 
data_frame(JurisdictionName, counties)
																			
#  THERE IS SOME MESSED UP STUFF HERE
#  LIKE...ONE PERSON FROM LAC LA BELLE IN JEFFERSON CO BUT THE REST IN WAUKESHA
#  LIVINGSTON: BARELY
#  MENASHA: WINNEBAGO ONLY? BUT A WEIRD INCORPORATION?
#  NEW AUBURN
#  VIOLA



# ----------------------------------------------------
#   attribute voters in multiple counties proportionally
# ----------------------------------------------------

# merge multiple county names into Wisconsin data

d %>%
filter(JurisdictionName %in% multiple_wi$JurisdictionName) %>%
select(-starts_with("na"), 
       -contains("reg_includes"), 
       -contains("allow")) %>%
left_join(., multiple_wi, by = c("State", "JurisdictionName", "FIPSCode")) %>%
select(-countyname) %>%
mutate(denominator = strsplit(counties, split = ", ") %>%
                     lengths()) %>%
split(.$JurisdictionName)



# how to actually divide...
# split into jurisdictions, each has a DF
# divide each column by however many





# ----------------------------------------------------
#   state level
# ----------------------------------------------------

# do numeric ones

d %>%
select(-starts_with("na"), -contains("reg_includes"), -contains("allow"), 
       -JurisdictionName, -FIPSCode, -FIPS_2Digit) %>%
replace(is.na(.), 0) %>%
group_by(State) %>%
summarize_all(sum)



# are there any "na" columns that have NAs?
d %>%
select(State, starts_with("na")) %>%
group_by(State) %>%  
summarize_all(function(x) sum(is.na(x))) %>%
select(-State) %>% 
apply(2, sum) %>%
table(., exclude = NULL)

# doesn't appear so



# how many MISSING codes per state
d %>%
select(State, starts_with("na")) %>%
group_by(State) %>% 
summarize_all(function(x) length(unique(x))) %>%
apply(2, function(x) table(.$State, x))


# paste missing codes
d %>%
select(State, starts_with("na")) %>% 
group_by(State) %>%  
summarize_all(function(x) str_c(levels(as.factor(x)), collapse = ", ")) %>%
as.data.frame()





# ----------------------------------------------------
#   County level
# ----------------------------------------------------

# start without IL or WI
r <- filter(d, !(State %in% c("IL", "WI"))) %>% 
     print



## make a just-in-case county-level FIPS variable
## then aggregate


county_digits <- 10000


# numeric

r %>%
mutate(countyfips = as.numeric(FIPSCode),
       countyfips = county_digits * floor(countyfips / county_digits),
       countyfips = as.character(countyfips)) %>%
select(-starts_with("na"), -contains("reg_includes"), -contains("allow"), 
       -FIPS_2Digit, -FIPSCode, -JurisdictionName) %>%
replace(is.na(.), 0) %>%
group_by(State, countyfips) %>%  
summarize_all(sum)



# are there any "na" columns that have NAs?
r %>%
select(State, starts_with("na")) %>%
group_by(State) %>%  
summarize_all(function(x) sum(is.na(x))) %>%
select(-State) %>% 
apply(2, sum) %>%
table(., exclude = NULL)

# doesn't appear so



# how many MISSING codes per state
r %>%
select(State, starts_with("na")) %>%
group_by(State) %>% 
summarize_all(function(x) length(unique(x))) %>%
apply(2, function(x) table(.$State, x))

# paste missing codes
r %>%
select(State, starts_with("na")) %>% 
group_by(State) %>%  
summarize_all(function(x) str_c(levels(as.factor(x)), collapse = ", "))


