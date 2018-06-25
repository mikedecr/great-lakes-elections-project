---
title: "Questions about EAVS"
---


# To do

fix availability of turnout data?

Do this as linearly as possible

- [x] start with EAVS
  + [ ] any graphics?
- [x] SPAE/waits
  + [ ] save more graphics
- [x] VEP
  + [ ] *how to deal with data availability?* 
  + [ ] *Just keep the file offline and purposefully break the code?*
  + [ ] *Include a warning at the top that it doesn't work on purpose?*
  + [ ] save more graphics
- [x] Vote methods
  - [x] *ask about doing 85% threshold for vote methods* (Yes)
    + [ ] this currently uses the raw data, not the aggregated
    + [ ] but could make one with aggregated data? Shouldn't make any difference?
  - [x] *old note says GL states only. This would be easier, but is the restriction necessary anymore?* (no)
  - [x] *do we care about "total absentee?"* (no)
- [ ] CPS
  - [ ] *Get Barry's help with downloading it?*
  - [ ] *not clear why the CPS that I have lacks weights*
  - [ ] *how to code turnout (count refusals/DK as nonvotes?)*
  - [ ] *how to code eligibility*
  - [ ] any graphics?
  - [ ] **must simply be patient in Firefox**
- [ ] Normalized measures
  + [x] fix Evan file merge
  + [ ] get residual votes from Evan
  + [x] sort columns using normalized table
  + [ ] Codebook notes:
    * [ ] vote methods categories aren't always mutually exclusive
    * [ ] assemblage of data sources
- [ ] Consult with Evan
  + [ ] resolve "redundant columns"
  + [ ] *"ballots cast absentee" submitted or accepted?*
  + [ ] *"data completeness rate"*
  + [ ] *"usability of state elections websites" and "website capabilities"*
  + [ ] Make a codebook 
      * [ ] naming scheme
      * [ ] data sources
      * [ ] potential errors
- [ ] *sig figs* (5?)
- [ ] Check old notes
- [ ] check that final file contains all normalized measures
- [ ] Collect Evan's final code has for you? (or not?)
  + [ ] is it worth incorporating?
  + [ ] include a link to his resources?
- [ ] exhaustive checks for comments, todos, alerts
- [ ] always be testing the Build file
- [ ] `make`?
- [ ] codebook
  + [ ] joint or per-person? Both?
  + [ ] might be worth pointing out on the website that the EAVS has intra-state discrepancies in responses about election law
- [ ] Delivery of other data (waits, county VEP)



# Old notes?

- Post-election auditing requirements
- Voting identification requirements
- Voting technology use
- Chief election officer
- Information about basic unit of responsibility for election administration (county vs. municipality)
- normalized measures (fractions)
  + for each fraction only take into account non-missing from the numerator when calculating the denominator?
  + 100 may report denominator, but if 90 report numerator, you can only calculate the ratio using those 90. And even then, it only makes it to the state data if the 90 jurisdictions are 85% of registered





