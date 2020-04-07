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
install.packages("psych")
library(psych)
install.packages("GPArotation")
library(GPArotation)
install.packages("qdap")
library(qdap)

######################################
## data cleaning - removing columns ##
######################################
freezing_raw<-select(freezing_raw,-c(redcap_survey_identifier,consent_form_timestamp,sona_id,con_date,con_sig,con_email,consent_form_complete,asi_timestamp,asi_complete,pswq_timestamp,pswq_complete,rrq_timestamp,rrq_complete,sias_timestamp,sias_complete,masq_timestamp,masq_complete,lec_timestamp,lec_complete,siasii_timestamp,siasii_complete,afq_timestamp,afq_complete,afqs_timestamp,afqs_complete,panas_timestamp,panas_complete,atq_timestamp,atq_complete,brief_timestamp,brief_complete,debriefing_form_timestamp,debriefing_form_complete))
freezing_raw$con_fn <- as.character(freezing_raw$con_fn)
freezing_raw$con_ln <- as.character(freezing_raw$con_ln)

### Make sure conversion worked - diagnostic steps ###
typeof(freezing_raw$con_fn)
typeof(freezing_raw$con_ln)
str(freezing_raw$con_fn)
str(freezing_raw$con_ln)


## specifying column index 390 = exclusion of any incomplete data ##
freezing_raw<-freezing_raw[complete.cases(freezing_raw[ , 390]),]


### extract repeat participants ###
duplicates(freezing_raw$con_fn)
duplicates(freezing_raw$con_ln)
grepl()
t2_raw<-freezing_raw %>%
  filter(con_fn==grepl(con_fn))


## cutting out AFQ endorsement section ##
freezing_raw<-select(freezing_raw,-c(afq_1:afq_24))
complete.cases(freezing_raw)                     

###################################################
##  Traditional EFA for Freezing (MASQ & AFQ)    ##
###################################################                   
efa_freeze<-freezing_raw %>%
  dplyr::select(afqs_1:afqs_70)
fa.parallel(efa_freeze, fm = 'minres', fa = 'fa', plot = FALSE)
fa(efa_freeze,nfactors=9,rotate="oblimin",fm="minres")


efa_df<-freezing_raw %>%
  dplyr::select(masq_01:masq_89,afqs_1:afqs_70)
fa.parallel(efa_df, fm = 'minres', fa = 'fa', plot = FALSE)
fa(efa_df,nfactors=11,rotate="oblimin",fm="minres")
