---
title: "Questions about EAVS"
---


# To do for cleaning the directory

fix availability of turnout data?

Do this as linearly as possible

- [x] start with EAVS
- [x] SPAE/waits
- [x] VEP
- [x] Vote methods
  - [ ] ask about doing 85% threshold for vote methods
- [ ] CPS
  - [ ] Get Barry's help with downloading it?
  - [ ] not clear why the CPS that I have lacks weights
  - [ ] how to code turnout (count refusals/DK as nonvotes?)
  - [ ] how to code eligibility
- [ ] Normalized measures
  - Build it all into one file
  - sort columns
- [ ] exhaustive checks for comments, todos, alerts
- [ ] Whatever Evan has for you?
- [ ] Build-all file
- [ ] number of states? 50, 51, 54? (ask?)
- [ ] check that final file contains all normalized measures


Codebook notes

- [ ] make a codebook
- [ ] voting methods figures aren't always mutually exclusive



# Evan call 2/22

Questions

- normalizing
  + not clear signal from anybody
- EPI measures?
  + we don't know what they want?
  + cover EPI, Book, Table 1


to-do

- fixed agg tables
- make some proportions using judgment and available data in EAVS sections A_C
- table of measures from EPI, Book, Table 1 in proposal


# evan call

questions

- what's the deal with Measures on page 300
- what are we doing
  + For each item for each state, construct a weight based on the percentage of responses that are not NA
  + then you can aggregate with narm = TRUE
  + if not at the threshold, missing at the state level
- normalized measures (fractions)
  + what's the sum? total reported or sum of the components?
  + for each fraction only take into account non-missing from the numerator when calculating the denominator?
  + Do we want to report this now, or just provide enough material to for someone to do it
  + 100 may report denominator, but if 90 report numerator, you can only calculate the ratio using those 90. And even then, it only makes it to the state data if the 90 jurisdictions are 85% of registered
- Questions for Barry (or Charles)
  + is "N" jurisdictions or fraction of registrants, or is this necessary
  + are we actually supposed to be doing the normalizing ourselves and if so, for what variables
  + what's the deal with the stuff about the end of the Measure of American Elections, since they aren't all in EAVS and etc.




# Evan call

to do:

- wait for Evan to upload F
- do turnout
- wait for Evan to send code
- prep repositories




to do

- send Evan the municipal CODE
  + talk to Barry if necessary
- codebook cleaning
- directory organization
- relative usage table
  + check Evan's PDFs


Organizing output

- [x] directory
  + do residual votes come from EAVS or something else?
    * (different directory?)
  + project-specific?
    * wait times maybe separate?
- [x] directory structure
  + state
    * a, b, c...
  + jurisdiction
    * a, b, c...
- what should we do with codebooks?
- create a repository online with source and everything?
  + just in case
  + I have a Github for this
  + I could sync your folder (or you can mail me a zip when we're TOTALLY FINISHED) and then create one for you?



ME:

- NEW TABLE: relative usage of voting methods
  + in-person, early in-person, absentee (UOCAVA?, Provisional?)
  + check grant
  + GL states only
- EXISTING TABLES:
  + add number of jurisdictions
  + fix anything in NA tables?
    * (see below)


NA tables

- currently I just go column by column
- percent of jurisdictions reporting data?
- percent type of missingness (all missingness?)


Check weird intra-state inconsistencies with categorical variables

- might be worth pointing out on the website that the EAVS has unreliable data about certain legal facts?






Distribution:

- keep R scripts online but don't distribute?
- honestly not my preference but whatever



joint readme?

- I have one started for my stuff
- weirdness
- other notes











# Super urgent shit

TRUE ~ variable (else becomes NA)

# Meeting with Evan, July 18

Evan is dealing with complicated text fields in section F

You should think about cleaning up the "Other" categories

Probably doing wait times from the SPAE


# Question for Barry

Does it make sense for us to be duplicating this?

- there should probably be one system across all states?
- Only the stuff particular to each state should be handled by sub-teams?


# Goals

- Voting age and race demographics
- Voting turnout
- Recent election returns for federal and statewide office
- Relative usage of convenience voting
	+ early voting
	+ absentee voting
	+ Election Day voting
- Election administration volume measures
	+ absentee ballots
	+ UOCAVA ballots (military/overseas)
	+ provisional ballots
- **Voter registration statistics (A)**
	+ **new registration**
	+ **cancellations**
- Average wait times
- Residual vote rates
- Post-election auditing requirements
- Voting identification requirements
- Voting technology use
- Chief election officer
- Information about basic unit of responsibility for election administration (county vs. municipality).




# Section Outlines

Section A: Registration

- registration numbers and practices
- active and inactive registration
- Same-day registration numbers and legality
- Errors in registration forms (duplications, invalids)
- Change-of-address forms
- address changes -> jurisdiction changes
- types/sources of registration applications
- sources of duplicated or invalid/rejected registration forms
- data on confirmation notices
- removals from rolls
- reasons for removals


Section B: UOCAVA absentee ballots

- number of UOCAVA absentee ballots
- categories of UOCAVA ballots
- types of UOCAVA ballot submissions (delivered, returned as undeliverable, spoiled or replaced)
- successfully delivered UOCAVA ballots
- types of delivered ballots
- types of undelivered ballots?
- total UOCAVA ballots counted toward election
- division of UOCAVA ballots counted
- Rejected UOCAVA ballots: number, reasons for rejecting, sources
- absentee vs military vs FWAB


Section C: Civilian absentee ballots

- number of civilian absentee ballots
- types of submission (accepted, returned, spoiled/replaced, undeliverable or not returned)
- permanent absentee preference
- mailings by permanent status
- number and fate of returned ballots
- reasons for rejecting returned ballots



Section D: Polling places and poll workers

- number of precincts in jurisdiction
- number of physical polling places
- categorization of physical polling places
- total number of poll workers
- age categories of poll workers
- difficulty of obtaining workers


Section E: Provisional Balloting

- n. provisional ballots
- fate of provisional ballots
- reasons to reject provisional ballot



Section F: Voting, location, equipment

- total number voted
- how voted (physical, provisional, absentee, UOCAVA/FWAB, early)
- providing ID and voting
- electronic poll books/lists for signing in, updating history, looking up polling places
- printed poll books/lists
- does state or jurisdiction manage book/list printing
- types of voting equipment (DRE, audit trail)
- location of vote tallying, central or jurisdiction/precinct
- availability of special voting devices for voters w/ disability
- location of special voting device tally
- equipment for provisional
- tally location for provisional
- equipment for early vote
- location of early vote and tally
- DRE machine number, make, model, version, vendor
- equipment for regular voting
- location of regular voting
- tally location of regular voting
- similar for DREs for disabled voters
- technology for optimal scan ballot
- number of booths and counters for different technologies
- punch card technologies, who uses, where counted, etc
- lever technologies, who uses, where tallied, etc
- hand-counted paper ballots, &c
- same for "other" technologies

# Form A

A1a: registered voters as reported

- two missing data codes, "not available" and "no response"
