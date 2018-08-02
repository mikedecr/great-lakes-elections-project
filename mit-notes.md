---
title: "Questions about EAVS"
---


# Evan call 7/11

- Where is Evan at
- Where am I at
  + verify with Barry that I'm doing one CPS thing right
  + calculate EAVS data completeness
  + *do I need residual votes from Evan?* No
  + *redundant columns*
- What we need to do
  + use 85% threshold for normalized measures
  + Make a codebook
    * EAVS
      - no need for variable descriptions
      - but we need to describe aggregation
    * Wait times
      - SPAE and CCES
    * Normalized measures
  + sigfigs

# To do

fix availability of turnout data?

Do this as linearly as possible

- Do we need graphics?
  + [ ] EAVS
  + [ ] wait times
  + [ ] VEP
  + [ ] Vote methods
  + [ ] CPS registration
  + [ ] Normalized measures
- [ ] VEP data
  + [ ] Remove data from Git/Repo among publication
  + [ ] save notes about revisions
- [ ] CPS coding
  - [ ] *Get the rawest data format possible with codebook?*
  - [ ] *how to code turnout (count refusals/DK as nonvotes?)*
  - [ ] *how to code eligibility*
    + [ ] add EAVS/USEP
- [ ] Normalized measures
  + [ ] *"data completeness rate"* (Charles email)
  + [ ] get residual votes from Evan (for normalized table)
  + [ ] Redundant columns
    * [ ] *This is because you need to divide only valid numerators by valid denominators* (start with the calculation, and then convert to NA if not enough)
  + [ ] sort columns
  + [ ] *Confirm w/ Evan that he's using 85% threshold for anything aggregated to state?*
  + [ ] *"ballots cast absentee" does "cast" mean submitted or accepted?* (check MAE)
  + [ ] *"usability of state elections websites" and "website capabilities"*
  + [ ] Make a codebook
    * [ ] *Google doc*
      * [ ] naming scheme
      * [ ] operationalization of each variable
      * [ ] data sources for each
      * [ ] potential issues/errors
        - [ ] vote methods categories aren't always mutually exclusive
        - [ ] EAVS has intra-state discrepancies in responses about election law
  + [ ] *sig figs* **(5?)** 
- [ ] *Final version to send to Charles/Dataverse*
  + [ ] Check old notes
  + [ ] exhaustive checks for comments, todos, alerts
  + [ ] Collect Evan's code (buildable? `make`? link to his?)
  + [ ] Remove McDonald data
  + [ ] One codebook
  



# Old notes?

- Post-election auditing requirements
- Voting identification requirements
- Voting technology use
- Chief election officer
- Information about basic unit of responsibility for election administration (county vs. municipality)
- normalized measures (fractions)
  + for each fraction only take into account non-missing from the numerator when calculating the denominator?
  + 100 may report denominator, but if 90 report numerator, you can only calculate the ratio using those 90. And even then, it only makes it to the state data if the 90 jurisdictions are 85% of registered





