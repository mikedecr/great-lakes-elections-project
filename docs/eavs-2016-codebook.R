

#----------------------------------------
#   naming scheme
#----------------------------------------

beepr::beep(2)
todo <- todo + 1
# almost every topic set (A2, A3, etc) has comment variables











#----------------------------------------
#   SECTION A: registration
#----------------------------------------



#----------------------------------------
#   A1--A3: Topline registration stats
#----------------------------------------

## A1a: reg_total_eligible

## A2: reg_includes
## A2c_Other: reg_includes_spec

## A3a: reg_active
## A3b: reg_inactive

#----------------------------------------
#   A4: sameday registration
#----------------------------------------

## A4a: reg_sameday_new
## A4b: reg_sameday_allow
## A4b_Other: reg_sameday_allow_spec

#----------------------------------------
#   A5: registration forms received
#----------------------------------------

## A5a: reg_forms_total
## A5b: reg_forms_newvalid
## A5c: reg_forms_prereg
## A5d: reg_forms_duplicate
## A5e: reg_forms_invrej
## A5f: reg_forms_minorchange
## A5g: reg_forms_newjuris
## A5h: reg_forms_other1
## A5i: reg_forms_other2
## A5j: reg_forms_other3
## A5k: reg_forms_other4
## A5l: reg_forms_other5


## create your own sum? reg_forms_total_calc



todo	<- todo + 1
beepr::beep(2)
# merge "Other" counts into other categories or aggregate in some way?
# Perhaps sum all "Other" into one "Other" category


#----------------------------------------
#   A6: registration submission type
#----------------------------------------

## A6a: reg_formsub_mail
## A6b: reg_formsub_inperson
## A6c: reg_formsub_online
## A6d: reg_formsub_dmv
## A6e: reg_formsub_nvra_office
## A6f: reg_formsub_state_disable
## A6g: reg_formsub_recruitment
## A6h: reg_formsub_other_non_nvra
## A6i: reg_formsub_regdrive
## A6j: reg_formsub_other1
## A6k: reg_formsub_other2
## A6l: reg_formsub_other3
## A6m: reg_formsub_other4
## A6n: reg_formsub_other5
## A6o: reg_formsub_other6
## calc own total:: reg_formsub_total (== { A5a,  reg_forms_total,  reg_forms_total_calc} )


#----------------------------------------
#   A7: new registration form type
#----------------------------------------

## A7a: reg_newform_mail
## A7b: reg_newform_inperson
## A7c: reg_newform_online
## A7d: reg_newform_dmv
## A7e: reg_newform_nvra_office
## A7f: reg_newform_state_disable
## A7g: reg_newform_recruitment
## A7h: reg_newform_other_non_nvra
## A7i: reg_newform_regdrive
## A7j: reg_newform_other1
## A7k: reg_newform_other2
## A7l: reg_newform_other3
## A7m: reg_newform_other4
## A7n: reg_newform_other5
## A7o: reg_newform_other6
## total: reg_newform_total ( == A5b, reg_forms_newvalid)



#----------------------------------------
#   A8: duplicate registration form type
#----------------------------------------

## A8a: reg_duplic_mail
## A8b: reg_duplic_inperson
## A8c: reg_duplic_online
## A8d: reg_duplic_dmv
## A8e: reg_duplic_nvra_office
## A8f: reg_duplic_state_disable
## A8g: reg_duplic_recruitment
## A8h: reg_duplic_other_non_nvra
## A8i: reg_duplic_regdrive
## A8j: reg_duplic_other1
## A8k: reg_duplic_other2
## A8l: reg_duplic_other3
## A8m: reg_duplic_other4
## A8n: reg_duplic_other5
## A8o: reg_duplic_other6
## total: reg_duplic_total ( == A5d, reg_forms_duplicate)





#----------------------------------------
#   A9: invalid/rejected registration form type
#----------------------------------------

## A9a: reg_invrej_mail
## A9b: reg_invrej_inperson
## A9c: reg_invrej_online
## A9d: reg_invrej_dmv
## A9e: reg_invrej_nvra_office
## A9f: reg_invrej_state_disable
## A9g: reg_invrej_recruitment
## A9h: reg_invrej_other_non_nvra
## A9i: reg_invrej_regdrive
## A9j: reg_invrej_other1
## A9k: reg_invrej_other2
## A9l: reg_invrej_other3
## A9m: reg_invrej_other4
## A9n: reg_invrej_other5
## A9o: reg_invrej_other6
## total: reg_invrej_total ( == A5e, reg_forms_invrej)


#----------------------------------------
#   A10: mailed confirmation notices sent
#----------------------------------------

## A10a: reg_notice_total
## A10b: reg_notice_confirmreg
## A10c: reg_notice_confirmnonreg
## A10d: reg_notice_undeliver
## A10e: reg_notice_unknown
## A10f: reg_notice_other1
## A10g: reg_notice_other2
## A10h: reg_notice_other3
## total: reg_notice_total



#----------------------------------------
#   A11: voters removed from rolls
#----------------------------------------

## A11a: reg_drop_total
## A11b: reg_drop_moved
## A11c: reg_drop_deceased
## A11d: reg_drop_felony
## A11e: reg_drop_inactive
## A11f: reg_drop_incompetent
## A11g: reg_drop_request
## A11h: reg_drop_other1
## A11i: reg_drop_other2
## A11j: reg_drop_other3
## A11k: reg_drop_other4
## A11_Total: reg_drop_total2 













#----------------------------------------
#   SECTION B: foreign workers absentee ballots
#----------------------------------------




#----------------------------------------
#   B1: how many sent to whom
#----------------------------------------

## B1a: ava_sent_total
## B1b: ava_sent_unif
## B1c: ava_sent_civ
## B1d: ava_sent_other1
## B1e: ava_sent_other2

## calc: ava_sent_total_calc


#----------------------------------------
#   B2: what happened when sent
#----------------------------------------

## B2a: ava_result_returned
## B2b: ava_result_undeliver
## B2c: ava_result_spoiled
## B2d: ava_result_unknown
## B2e: ava_result_other1
## B2f: ava_result_other2
## B2g: ava_result_other3

## calc: ava_result_total_calc


#----------------------------------------
#  B3:  total UOCAVA ballots returned
#----------------------------------------

## B3: ava_returned_total



#----------------------------------------
#   B4: returned ballots from whom
#----------------------------------------

## B4a: ava_subwho_unif
## B4b: ava_subwho_civ
## B4c: ava_subwho_other

## calc: ava_subwho_total_calc


#----------------------------------------
#   B5: absentee ballots from whom
#----------------------------------------

## B5a: ava_subabswho_unif
## B5b:ava_subabswho_civ
## B5c: ava_subabswho_other

## calc: ava_subabswho_total_calc



#----------------------------------------
#   B6: FWAB ballots from whom
#----------------------------------------

## B6a: ava_subfwabwho_unif
## B6b: ava_subfwabwho_civ
## B6c: ava_subfwabwho_other

## calc: ava_subfwab_total_calc



#----------------------------------------
#   B7: 'other' ballots fromw hom
#----------------------------------------

## B7a: ava_subotherwho_unif
## B7b: ava_subotherwho_civ
## B7c: ava_subotherwho_other

## calc: ava_subotherwho_total_calc 



#----------------------------------------
#   B8: total actually counted
#----------------------------------------

## B8a: ava_counted_total


#----------------------------------------
#   B9: counted from whom
#----------------------------------------

## B9a: ava_countwho_unif
## B9b: ava_countwho_civ
## B9c: ava_countwho_other

## calc: ava_countwho_total_calc


#----------------------------------------
#   B10: absentee ballots counted from whom
#----------------------------------------

## B10a: ava_countabswho_unif
## B10b: ava_countabswho_civ
## B10c: ava_countabswho_other

## calc: ava_countabswho_total_calc


#----------------------------------------
#   B11: FWA ballots counted from whom
#----------------------------------------

## B11a: ava_countfwabwho_unif
## B11b: ava_countfwabwho_civ
## B11c: ava_countfwabwho_other

## calc: ava_countfwabwho_total_calc


#----------------------------------------
#   B12: other types of ballots counted from whom
#----------------------------------------

## B12a: ava_countotherwho_unif
## B12b: ava_countotherwho_civ
## B12c: ava_countotherwho_other

## calc: ava_countotherwho_total_calc


#----------------------------------------
#   B13: total rejected ballots (abs, fwab, and other)
#----------------------------------------

## B13a: ava_rej_total


#----------------------------------------
#   B14: reasons rejected
#----------------------------------------

## B14a: ava_rejwhy_time
## B14b: ava_rejwhy_signature
## B14c: ava_rejwhy_postmark
## B14d: ava_rejwhy_other1
## B14e: ava_rejwhy_other2
## B14f: ava_rejwhy_other3

## calc: ava_rejwhy_total_calc (should == B13, ava_rej_total)



#----------------------------------------
#   B15: rejected ballots from whom
#----------------------------------------

## B15a: ava_rejwho_unif
## B15b: ava_rejwho_civ
## B15c: ava_rejwho_other

## calc: ava_rejwho_total_calc (should == B13, ava_rej_total)


#----------------------------------------
#   B16: rejected absentee ballots from whom
#----------------------------------------

## B16a: ava_rejabswho_unif
## B16b: ava_rejabswho_civ
## B16c: ava_rejabswho_other

## calc: ava_rejabswho_total_calc

#----------------------------------------
#   B17: rejected fwabs from whom
#----------------------------------------

## B17a: ava_rejfwabwho_unif
## B17b: ava_rejfwabwho_civ
## B17c: ava_rejfwabwho_other

## calc: ava_rejfwabwho_total_calc


#----------------------------------------
#   B18: rejected 'other' ballots from whom
#----------------------------------------

## B18a: ava_rejotherwho_unif
## B18b: ava_rejotherwho_civ
## B18c: ava_rejotherwho_other

## calc: ava_rejotherwho_total_calc


todo	<- todo + 1
beepr::beep(2)
# ava_rejabswho_total_calc + ava_rejfwabwho_total_calc + ava_rejotherwho_total_calc = { ava_rejwho_total_calc , ava_rej_total }? 



















#----------------------------------------
#   SECTION C: domestic absentee ballots
#----------------------------------------



#----------------------------------------
#   C1: topline absentee stats
#----------------------------------------

## C1a: abs_sent_total
## C1b: abs_returned_total
## C1c: abs_result_undeliver
## C1d: abs_result_spoiled
## C1e: abs_result_unknown
## C1f: abs_result_other1
## C1g: abs_result_other2
## C1h: abs_result_other3

#-# calc: abs_result


#----------------------------------------
#   C2: permanent absentee registration
#----------------------------------------

## C2: abs_permareg


#----------------------------------------
#   C3: number sent to permanent absentees
#----------------------------------------

## C3: abs_perma_sentcount


#----------------------------------------
#   C4: fate of returned ballots
#----------------------------------------

## C4a: abs_return_counted
## C4b: abs_return_reject
## C4c: abs_return_other1
## C4d: abs_return_other2

#-# calc: abs_return_total_calc (== C1b ,abs_returned_total)


#----------------------------------------
#   C5: reason absentee ballot rejected
#----------------------------------------

## C5a: abs_rejwhy_date
## C5b: abs_rejwhy_votersig
## C5c: abs_rejwhy_witsig
## C5d: abs_rejwhy_sigmatch
## C5e: abs_rejwhy_eosig
## C5f: abs_rejwhy_envelope
## C5g: abs_rejwhy_noballot
## C5h: abs_rejwhy_envseal
## C5i: abs_rejwhy_address
## C5j: abs_rejwhy_multballots
## C5k: abs_rejwhy_deceased
## C5l: abs_rejwhy_inperson
## C5m: abs_rejwhy_id
## C5n: abs_rejwhy_noapp
## C5o: abs_rejwhy_other1
## C5p: abs_rejwhy_other2
## C5q: abs_rejwhy_other3
## C5r: abs_rejwhy_other4
## C5s: abs_rejwhy_other5
## C5t: abs_rejwhy_other6
## C5u: abs_rejwhy_other7
## C5v: abs_rejwhy_other8

#-# calc: abs_rejwhy_total_calc ( == C4b, abs_return_reject)





