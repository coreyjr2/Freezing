##############################
### Freezing EFA Analysis ###
##############################

freezing_raw<-read.csv(file.choose())

###############################
## install relevant packages ##
###############################
install.packages("tidyverse")
y
library(tidyverse)

######################################
## data cleaning - removing columns ##
## the following lines are separated by chunks of cleaning for accessibility##
######################################
freezing_raw<-select(freezing_raw,-c(redcap_survey_identifier,consent_form_timestamp,sona_id,con_fn,con_ln,con_date,con_sig,con_email,consent_form_complete,asi_timestamp,asi_complete,pswq_timestamp,pswq_complete,rrq_timestamp,rrq_complete,sias_timestamp,sias_complete,masq_timestamp,masq_complete,lec_timestamp,lec_complete,siasii_timestamp,siasii_complete,afq_timestamp,afq_complete,afqs_timestamp,afqs_complete,panas_timestamp,panas_complete,atq_timestamp,atq_complete,brief_timestamp,brief_complete,debriefing_form_timestamp,debriefing_form_complete))

