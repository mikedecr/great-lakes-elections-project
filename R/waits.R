# ----------------------------------------------------
#   SPAE for wait times
# ----------------------------------------------------


# --- NOTES -----------------

# how many items to aggregate
#
# - just the estimated wait?
# - or other items about the locus of the blockage?
#
# # charles:
#
# Here’s the cookbook version:
# 1. For respondents who give replies 1-4 to the wait-time question (q13 in 2016), impute minutes using the midpoint of the response intervals.  I.e., 1à0 minutes, 2à 5 minutes, 3à20 minutes, 4à45 minutes.
# 2.       For respondents who give replied # 5 (> 60 minutes), do the following:
# a.       For respondents who respond to the open-ended prompt (q13_t in 2016), use that response to code the number of minutes.
# b.      For respondents who do not respond to the open-ended prompt, take the average of the responses from step 2.a.



library("magrittr")
library("tidyverse")
library("ggplot2")

library("here")


# ----------------------------------------------------
#   Joyce parameters
# ----------------------------------------------------
joyce_states <- c("IL", "IN", "MI", "MN", "OH", "WI")

(state_fips <- read_csv(here("data/census/census-state-fips.csv")))

(joyce_fips <- filter(state_fips, state_abb %in% joyce_states))


# ----------------------------------------------------
#   SPAE
# ----------------------------------------------------

# --- data -----------------

# check for unzipped
if ("SPAE" %in% list.files(here("data"))) {
  print("SPAE already unzipped")
} else {
  dir.create(here("data/SPAE"))
  unzip(here("data/spae.zip"), exdir = here("data/SPAE"))
}


# read data

# read.table(., sep = "\t") fails to get the correct number of obs
read.table(here("data/SPAE/MITU0022_OUTPUT.tab"),
           sep = "\t", header = TRUE) %>%
as_data_frame() %>%
print()

# going the read_tsv route despite small number of parsing failues
# recode the wait variable
sp <- read_tsv(here("data/SPAE/MITU0022_OUTPUT.tab")) %>%
      as_data_frame() %>%
      mutate(wait_time = case_when(Q13 == 1 ~ "1. Not at all",
                                   Q13 == 2 ~ "2. Less than 10 minutes",
                                   Q13 == 3 ~ "3. 10-30 minutes",
                                   Q13 == 4 ~ "4. 31 minutes to 1 hour",
                                   Q13 == 5 ~ "5. More than 1 hour (specify)",
                                   Q13 == 6 ~ "9. I don't know")) %>%
      print

# check if the recoding is good
sp %$% table(wait_time, Q13, exclude = NULL)


# plot by state

sp %>%
filter(inputstate %in% joyce_fips$state_FIPS) %>%
mutate(state = case_when(inputstate == 17 ~ "Illinois",
                         inputstate == 18 ~ "Indiana",
                         inputstate == 26 ~ "Michigan",
                         inputstate == 27 ~ "Minnesota",
                         inputstate == 39 ~ "Ohio",
                         inputstate == 55 ~ "Wisconsin")) %>%
group_by(state, wait_time) %>%
summarize(n = n()) %>%
group_by(state) %>%
mutate(pct = n / sum(n)) %>%
ggplot(aes(x = wait_time, y = pct)) +
  geom_col() +
  scale_y_continuous(labels = ) +
  facet_wrap(~ state) +
  coord_flip()





table(sp$Q13, exclude = NULL)
levels(as.factor(sp$Q13_t))


# ----------------------------------------------------
#   export SPAE "specify" answers to hand-code
# ----------------------------------------------------

dir.create(here("data/waits"))

sp %>%
filter(Q13 == 5) %>%
select(caseid, Q13, Q13_t, inputstate) %>%
print() %>%
write_csv(here("data/waits/specify-to-do.csv"))

#   hand-coding of text responses happens here

#   re-import hand-coded stuff

sp_specify <- read_csv(here("data/waits/specify-recode.csv")) %>%
              mutate(Q13_oe = as.double(Q13_oe)) %>%
              print


# merge and recode to integer (minutes)
sp <-
  left_join(sp, sp_specify, by = c("caseid", "Q13")) %>%
  mutate(wait_int = case_when(Q13 == 1 ~ 0,
                              Q13 == 2 ~ 5 ,
                              Q13 == 3 ~ 15,
                              Q13 == 4 ~ 45),
         wait_int = ifelse(Q13 == 5, Q13_oe, wait_int)) %>%
         print


# if "more than 1 hour" but not specified, impute mean from those who did specify
mean_specified <- filter(sp, Q13 == 5) %$%
                  mean(wait_int, na.rm = TRUE) %>%
                  round() %>%
                  print

sp <- sp %>%
      mutate(wait_int = ifelse(Q13 == 5 & is.na(wait_int),
                               mean_specified,
                               wait_int)) %>%
      print

table(sp$wait_int, exclude = NULL)


# ----------------------------------------------------
#   CCES
#   contains no "specify" column, so no hand-coding required
# ----------------------------------------------------

# check for unzipped
if ("CCES" %in% list.files(here("data"))) {
  print("CCES already unzipped")
} else {
  dir.create(here("data/CCES"))
  unzip(here("data/cces.zip"), exdir = here("data/CCES"))
}


cc <- readr::read_tsv(here("data/cces/CCES16_Common_OUTPUT_Jul2017_VV.tab")) %>%
      mutate(wait_time = case_when(CC16_404 == 1 ~ "1. Not at all",
                                   CC16_404 == 2 ~ "2. Less than 10 minutes",
                                   CC16_404 == 3 ~ "3. 10-30 minutes",
                                   CC16_404 == 4 ~ "4. 31 minutes to 1 hour",
                                   CC16_404 == 5 ~ "5. More than 1 hour (specify)",
                                   CC16_404 == 6 ~ "9. I don't know")) %>%
      print()


# recode to integer (minutes)
cc <- cc %>%
      mutate(wait_int = case_when(CC16_404 == 1 ~ 0,
                                  CC16_404 == 2 ~ 5 ,
                                  CC16_404 == 3 ~ 15,
                                  CC16_404 == 4 ~ 45,
                                  CC16_404 == 5 ~ mean_specified)) %>%
      print


cc %$% table(CC16_404, wait_time, exclude = NULL)
cc %$% table(wait_time, wait_int, exclude = NULL)


# plot
cc %>%
filter(inputstate %in% joyce_fips$state_FIPS) %>%
mutate(state = case_when(inputstate == 17 ~ "Illinois",
                         inputstate == 18 ~ "Indiana",
                         inputstate == 26 ~ "Michigan",
                         inputstate == 27 ~ "Minnesota",
                         inputstate == 39 ~ "Ohio",
                         inputstate == 55 ~ "Wisconsin")) %>%
group_by(state, wait_time) %>%
summarize(n = n()) %>%
group_by(state) %>%
mutate(pct = n / sum(n)) %>%
ggplot(aes(x = wait_time, y = pct)) +
  geom_col() +
  scale_y_continuous(labels = ) +
  facet_wrap(~ state) +
  coord_flip()


# ----------------------------------------------------
#   combine into one dataset
# ----------------------------------------------------


census_fips <- read_csv(here("data/census/census-state-fips.csv")) %>%
               rename(inputstate = state_FIPS) %>%
               print


spcc <-
  bind_rows(select(sp, inputstate, countyname, wait_time, wait_int) %>%
              mutate(dataset = "SPAE"),
            select(cc, inputstate, wait_time, countyname, wait_int) %>%
              mutate(dataset = "CCES")) %>%
  left_join(., census_fips, by = "inputstate") %>%
  mutate(joyce_state = ifelse(state_abb %in% joyce_fips$state_abb, 1, 0)) %>%
  select(dataset, state_abb, inputstate, joyce_state,
         countyname, wait_time, wait_int) %>%
  print()







# ----------------------------------------------------
#   aggregate to state
# ----------------------------------------------------

# all states
state_waits <- spcc %>%
               group_by(state_abb) %>%
               summarize(inputstate = unique(inputstate),
                         mean_wait = mean(wait_int, na.rm = TRUE) %>% round(1),
                         se_wait = sqrt(var(wait_int, na.rm = TRUE) / n()),
                         MOE = se_wait * qt(.975, n() - 1),
                         MOE = round(MOE, 1)) %>%
               select(inputstate, state_abb, mean_wait, MOE) %>%
               print()


dir.create(here("output/waits"))
write_csv(state_waits, here("output/waits/state-avg-waits.csv"))





# ----------------------------------------------------
#   examine both datasets together
# ----------------------------------------------------

spcc %>%
group_by(dataset, state_abb, wait_int) %>%
tally() %>%
group_by(dataset, state_abb) %>%
mutate(prop = n / sum(n)) %>%
ggplot(aes(x = wait_int, y = prop)) +
  geom_col(aes(fill = dataset),
           position = "dodge",
           size = 3) +
  facet_wrap(~ state_abb) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Time in Line to Vote (Minutes)",
       y = "Percent",
       fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, vjust=0.75)) +
  geom_vline(data = state_waits, aes(xintercept = mean_wait), color = "red")



spcc %>%
group_by(dataset, state_abb, wait_int) %>%
tally() %>%
filter(!is.na(wait_int)) %>%
group_by(dataset, state_abb) %>%
mutate(prop = n / sum(n)) %>%
ggplot(aes(x = as.factor(wait_int), y = prop)) +
  geom_col(aes(fill = dataset),
           position = "dodge",
           size = 3,
           show.legend = FALSE) +
  facet_wrap(~ state_abb, scales = "free_y") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Time in Line to Vote (Minutes)",
       y = "Percent",
       fill = NULL,
       caption = "Note: X-axis scale not proportional") +
  theme(axis.text.x = element_text(angle = 45, vjust=0.75))







spcc %>%
group_by(dataset, state_abb, wait_int) %>%
tally() %>%
group_by(dataset, state_abb) %>%
mutate(prop = n / sum(n)) %>%
ggplot(aes(x = wait_int, y = prop)) +
  geom_pointrange(aes(color = dataset, ymax = prop, ymin = 0),
                  position = position_dodge(width = 10)) +
  facet_wrap(~ state_abb) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Time in Line to Vote (Minutes)",
       y = "Percent",
       color = NULL) +
  theme(axis.text.x = element_text(angle = 45, vjust=0.75)) +
  geom_vline(data = state_waits, aes(xintercept = mean_wait),
             color = "black")




# ----------------------------------------------------
#   County level
#   set threshold: n = 50
# ----------------------------------------------------

threshold <- 50

keep_counties <-
  spcc %>%
  group_by(dataset, countyname, state_abb) %>%
  tally() %>%
  spread(key = dataset, value = n) %>%
  mutate(CCES = ifelse(is.na(CCES), 0, CCES),
         SPAE = ifelse(is.na(SPAE), 0, SPAE),
         total = CCES + SPAE) %>%
  arrange(desc(total)) %>%
  filter(total >= threshold) %>%
  print()



spcc_county <-
  spcc %>%
  filter(countyname %in% keep_counties$countyname) %>%
  group_by(inputstate, state_abb, countyname) %>%
  summarize(mean_wait = mean(wait_int, na.rm = TRUE) %>% round(1),
            MOE = sqrt(var(wait_int, na.rm = TRUE) / n()) %>% round(1)) %>%
  print


write_csv(spcc_county, here("output/waits/county-avg-waits.csv"))


print("Wait times code completed without error")
