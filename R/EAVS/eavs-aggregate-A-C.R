# ----------------------------------------------------
#   EAVS 2016
#   attempt to clean&aggregate A through C
#   "new" method for dealing with NA
# ----------------------------------------------------

library("magrittr")
library("tidyverse")

alerts <- 0


# ----------------------------------------------------
#   data
# ----------------------------------------------------

# directory already set

d <- readRDS(here("data/eavs/eavs-2016-unlabelled.RDS")) %>%
     select(FIPSCode:JurisdictionName, starts_with("A"),
            starts_with("B"), starts_with("C")) %>%
     print()

names(d)

(joyce_states <- c("IL", "IN", "MI", "MN", "OH", "WI"))






# ----------------------------------------------------
#   inspecting and tabulating
# ----------------------------------------------------

# --- open-ended questions ("other", "comment") -----------------

d %>%
  summarize_all(class) %>%
  gather() %>%
  filter(value == "character") %>%
  as.data.frame()

# limit to Joyce
# convert things to NA
# look at other and comment
# examine responses
# * * * not really sure what to do with this
joyce_comments <- d %>%
  filter(State %in% joyce_states) %>%
  mutate_if(is.character,
            function(x) ifelse(x %in% c("N/A", " ", "N/AN/A"), NA, x)) %>%
  select(State, ends_with("other"), ends_with("comments")) %>%
  gather(key = var, value = comment, -State) %>%
  filter(!is.na(comment)) %>%
  count(State, var, comment) %>%
  print()



# --- tabulating -----------------

# here is the strategy
lapply(d, function(x) head(table(x, exclude = NULL)))
lapply(d, function(x) table(is.na(x), exclude = NULL))

# we could put these into an object for digging through,
# but practically speaking, we do want to see this stuff up close
# so might as well print it out

# should go into the file one-by-one if you want to get the details
source("R/EAVS/eavs-tabulate-A-C.R", echo = TRUE)






# ----------------------------------------------------
#   Recoding
# ----------------------------------------------------

# recode variables with categorical meanings to non-numeric values
# fix messed up missingness categories

d <- d %>%
  mutate_if(is.numeric,
            function(x) case_when(x %in% c(-999998, -999991) ~ -999999,
                                  TRUE ~ x)) %>%
  mutate(A2 = case_when(A2 == 1 ~ "1. Active voters only",
                        A2 == 2 ~ "2. Active and inactive registered voters",
                        A2 == 3 ~ "3. Other"),
         A4b = case_when(A4b == 1 ~ "1. Yes",
                         A4b == 2 ~ "2. No, but some other voters were able to register and vote in the same day",
                         A4b == 3 ~ "3. Other",
                         A4b == -999999 ~ "9. Data Not Available",
                         A4b %in% c(-888888, -88888) ~ "8. Not Applicable"),
         C2 = case_when(C2 == 1 ~ "Yes",
                        C2 == 2 ~ "No",
                        C2 == - 9999999 ~ "9. Data Not Available",
                        C2 == -888888 ~ "8. Not Applicable"),
         C4d_Other = as.character(C4d_Other)) %>%
  print


# ----------------------------------------------------
#   Aggregating by state
# ----------------------------------------------------

# Charles Stewart instructions for aggregation with NAs
# jurisdictions may have valid values or NA
# if state reports valid for jurisdictions containing >= 85% of registrants
#   then aggregate, else NA


# operationalization:
# include a jurisdiction counter: sum to save number of jurisdictions/state
# recode all negative or missing values as NA
# summarizing using anonymous function(x) that does the following:
#   - Group by state
#   - calculate total registrants in state
#   - calculate registrants in jurisdictions where x is reported
#   - if >= .85, then aggregate, else NA
#   - results of ifelse() are automatically returned

# prep dataset
#   used for aggregating and counting jurisdictions
#   jurisdiction counter, recode missingness codes to NA
pre_state <- d %>%
  mutate(jurisdictions = 1) %>%
  mutate_if(is.numeric, function(x) ifelse(x < 0 | is.na(x), NA, x)) %>%
  print()


# do aggregation for 85% coverage states
#   calc total registrants within a state
#   calc total registrants where x is reported (i.e. x is not NA)
#   calculate fraction of registrants reported in x
#   aggregate if > 85%
# then reorder columns

state_agg <- pre_state %>%
  group_by(State) %>%
  summarize_if(is.numeric, function(x) {
                 total_reg <- sum(.$A1a, na.rm = TRUE)
                 represented <- sum(.$A1a[!is.na(x)], na.rm = TRUE)
                 frac_represented <- represented / total_reg
                 ifelse(frac_represented >= 0.85, sum(x, na.rm = TRUE), NA)
                }
               ) %>%
  select(State, jurisdictions, matches(".")) %>%
  print()

# also: total jurisdictions represented in each state by each item,
# use sum(!is.na(x)) instead of sum(x)
state_N <- pre_state %>%
  group_by(State) %>%
  summarize_if(is.numeric, function(x) {
                 total_reg <- sum(.$A1a, na.rm = TRUE)
                 represented <- sum(.$A1a[!is.na(x)], na.rm = TRUE)
                 frac_represented <- represented / total_reg
                 ifelse(frac_represented >= 0.85, sum(!is.na(x), na.rm = TRUE), NA)
                }
               ) %>%
  setNames(paste0(names(.), "_N")) %>%
  rename(State = State_N, jurisdictions = jurisdictions_N) %>%
  print()


# join agg and Ns tables
# calculate "other" columns as sum of extra categories
#   and "other_N"
# create "other" variables with "zzz" string for alphabetical ordering,
#   then remove the "zzz" pattern once sorted
#   hacky but it works
statelevel <-
  inner_join(state_agg, state_N, by = c("State", "jurisdictions")) %>%
  mutate(joyce = ifelse(State %in% joyce_states, 1, 0)) %>%
  select(gtools::mixedorder(names(.))) %>%
  select(State, jurisdictions, joyce, matches(".")) %>%
  mutate(A5zzzother = rowSums(select(., A5h:A5l, -contains("_N"))),
         A5zzzother_N = rowSums(select(., A5h:A5l_N) %>%
                             select(contains("_N"))),

         A6zzzother = rowSums(select(., A6j:A6o, -contains("_N"))),
         A6zzzother_N = rowSums(select(., A6j:A6o_N) %>%
                             select(contains("_N"))),

         A7zzzother = rowSums(select(., A7j:A7o, -contains("_N"))),
         A7zzzother_N = rowSums(select(., A7j:A7o_N) %>%
                             select(contains("_N"))),

         A8zzzother = rowSums(select(., A8j:A8o, -contains("_N"))),
         A8zzzother_N = rowSums(select(., A8j:A8o_N) %>%
                             select(contains("_N"))),

         A9zzzother = rowSums(select(., A9j:A9o, -contains("_N"))),
         A9zzzother_N = rowSums(select(., A9j:A9o_N) %>%
                             select(contains("_N"))),

         A10zzzother = rowSums(select(., A10f:A10h, -contains("_N"))),
         A10zzzother_N = rowSums(select(., A10f:A10h_N) %>%
                              select(contains("_N"))),

         A11zzzother = rowSums(select(., A11h:A11k, -contains("_N"))),
         A11zzzother_N = rowSums(select(., A11h:A11k_N) %>%
                              select(contains("_N"))),

         B1zzzother = rowSums(select(., B1d:B1e, -contains("_N"))),
         B1zzzother_N = rowSums(select(., B1d:B1e_N) %>%
                             select(contains("_N"))),

         B2zzzother = rowSums(select(., B2e:B2g, -contains("_N"))),
         B2zzzother_N = rowSums(select(., B2e:B2g_N) %>%
                             select(contains("_N"))),

         B4zzzother = B4c,
         B4zzzother_N = B4c_N,

         B5zzzother = B5c,
         B5zzzother_N = B5c_N,

         B6zzzother = B6c,
         B6zzzother_N = B6c_N,

         B7zzzother = B7c,
         B7zzzother_N = B7c_N,

         B9zzzother = B9c,
         B9zzzother_N = B9c_N,

         B10zzzother = B10c,
         B10zzzother_N = B10c_N,

         B11zzzother = B11c,
         B11zzzother_N = B11c_N,

         B12zzzother = B12c,
         B12zzzother_N = B12c_N,

         B14zzzother = rowSums(select(., B14d:B14f, -contains("_N"))),
         B14zzzother_N = rowSums(select(., B14d:B14f_N) %>%
                              select(contains("_N"))),

         B15zzzother = B15c,
         B15zzzother_N = B15c_N,

         B16zzzother = B16c,
         B16zzzother_N = B16c_N,

         B17zzzother = B17c,
         B17zzzother_N = B17c_N,

         B18zzzother = B18c,
         B18zzzother_N = B18c_N,

         C1zzzother = rowSums(select(., C1f:C1h, -contains("_N"))),
         C1zzzother_N = rowSums(select(., C1f:C1h_N) %>%
                             select(contains("_N"))),

         C4zzzother = rowSums(select(., C4c:C4d, -contains("_N"))),
         C4zzzother_N = rowSums(select(., C4c:C4d_N) %>%
                             select(contains("_N"))),

         C5zzzother = rowSums(select(., C5o:C5v, -contains("_N"))),
         C5zzzother_N = rowSums(select(., C5o:C5v_N) %>%
                             select(contains("_N")))) %>%
  select(gtools::mixedorder(names(.))) %>%
  select(State, jurisdictions, joyce, matches(".")) %>%
  setNames(., str_replace(names(.), "zzz", "")) %>%
  print()



select(statelevel, starts_with("B2"))

# potential source of error: whether "other" columns are correct
alerts <- alerts + 1
beepr::beep(2)





# ----------------------------------------------------
#   Saving tables
# ----------------------------------------------------



# --- A -----------------------

# total registered and eligible (a1), active, inactive, sameday
statelevel %>%
  select(State:joyce, A1a:A4a_N) %>%
  print() %>%
  write_csv(here("output/eavs/state/A/A-1-4-reg-total.csv"))


# A5: type of forms received, sum other
statelevel %>%
  select(State:joyce, starts_with("A5")) %>%
  select(-(A5h:A5l_N)) %>%
  print() %>%
  write_csv(here("output/eavs/state/A/A-5-reg-sub.csv"))


# A6: method of form submission (all)
statelevel %>%
  select(State:joyce, starts_with("A6")) %>%
  select(-(A6j:A6o_N)) %>%
  print() %>%
  write_csv(here("output/eavs/state/A/A-6-reg-total-methods.csv"))


# A7: method of form submission (new only)
statelevel %>%
  select(State:joyce, starts_with("A7")) %>%
  select(-(A7j:A7o_N)) %>%
  print() %>%
  write_csv(here("output/eavs/state/A/A-7-reg-sub-methods.csv"))



# A8: Duplicates by method
statelevel %>%
  select(State:joyce, starts_with("A8")) %>%
  select(-(A8j:A8o_N)) %>%
  print() %>%
  write_csv(here("output/eavs/state/A/A-8-reg-dup-methods.csv"))



# A9: invalid or rejected by method
statelevel %>%
  select(State:joyce, starts_with("A9")) %>%
  select(-(A9j:A9o_N)) %>%
  print() %>%
  write_csv(here("output/eavs/state/A/A-9-reg-invalid-methods.csv"))


# A10 mailed confirmation notices
statelevel %>%
  select(State:joyce, starts_with("A10")) %>%
  select(-(A10f:A10h_N)) %>%
  print() %>%
  write_csv(here("output/eavs/state/A/A-10-reg-notices.csv"))



# A11 removals
statelevel %>%
  select(State:joyce, starts_with("A11")) %>%
  select(-(A11h:A11k_N)) %>%
  print() %>%
  write_csv(here("output/eavs/state/A/A-11-reg-removals.csv"))



# --- B -----------------------

# B1: UOCAVA ballots sent to voters
# B2: how mail processed
statelevel %>%
  select(State:joyce, B1a:B1other_N, starts_with("B2")) %>%
  select(-(B1d:B1e_N), -(B2e:B2g_N)) %>%
  print() %>%
  write_csv(here("output/eavs/state/B/B-1-2-uocava-sent.csv"))


# B3 returned for counting
# B4: returned from whom,
# B5 through 7: returned type and from whom
statelevel %>%
  select(State:joyce, starts_with("B3"), starts_with("B4"),
         starts_with("B5"), starts_with("B6"), starts_with("B7")) %>%
  print() %>%
  write_csv(here("output/eavs/state/B/B-3-4-5-6-7-uocava-returned.csv"))


# B8: counted
# B9: counted from whom
# B10-12: type counted from whom
statelevel %>%
  select(State:joyce, starts_with("B8"), starts_with("B9"),
         starts_with("B10"), starts_with("B11"), starts_with("B12")) %>%
  print() %>%
  write_csv(here("output/eavs/state/B/B-8-9-10-11-12-uocava-counted.csv"))


# B13: rejected
# B14: rejected reason
# B15: rejected from whom
# B16-18:rejected type from whom
statelevel %>%
  select(State:joyce,
         starts_with("B13"), starts_with("B14"), starts_with("B15"),
         starts_with("B16"), starts_with("B17"), starts_with("B18")) %>%
  select(-(B14d:B14f_N)) %>%
  print() %>%
  write_csv(here("output/eavs/state/B/B-13-14-15-16-17-18-uocava-rejected.csv"))



# --- C -----------------------

# C1: absentee sent to voters and fate
# C3: num sent were permanent
statelevel %>%
  select(State:joyce, starts_with("C1"), starts_with("C2"), starts_with("C3")) %>%
  select(-(C1f:C1h_N)) %>%
  print() %>%
  write_csv(here("output/eavs/state/C/C-1-3-absentee-sent-fate.csv"))

# C4: returned and fate
statelevel %>%
  select(State:joyce, starts_with("C4")) %>%
  select(-(C4c:C4d_N)) %>%
  print() %>%
  write_csv(here("output/eavs/state/C/C-4-absentee-returned-fate.csv"))

# C5: rejected reasons
statelevel %>%
  select(State:joyce, starts_with("C5")) %>%
  select(-(C5o:C5v_N)) %>%
  print() %>%
  write_csv(here("output/eavs/state/C/C-5-absentee-rejections.csv"))




# --- move to master Great Lakes folder -----------------------
