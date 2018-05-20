# --------------------------------------
#  Manages building for...
#  EAVS, Wait times, VEP turnout, and normalized measures
# --------------------------------------




source("R/EAVS/eavs-2016-build.R", echo = TRUE)
beepr::beep(4)
source("R/waits.R", echo = TRUE)
beepr   ::beep(4)
source("R/VEP/county-turnout.R", echo = TRUE)
beepr::beep(4)

# This should be in normalized-measures.R
# source("R/normalized/eavs-2016-vote-methods.R", echo = TRUE)
# beepr::beep(4)

source("R/normalized/normalized-measures-MGD.R", echo = TRUE)
beepr::beep(4)
