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

## Remove unneeded columns from the dataset, refer to data dictionary for more info 
freezing_raw<-dplyr::select(freezing_raw,-c(redcap_survey_identifier,consent_form_timestamp,
                                            sona_id,con_date,con_sig,con_email,consent_form_complete,
                                            asi_timestamp,asi_complete,pswq_timestamp,pswq_complete,
                                            rrq_timestamp,rrq_complete,sias_timestamp,sias_complete,
                                            masq_timestamp,masq_complete,lec_timestamp,lec_complete,
                                            siasii_timestamp,siasii_complete,afq_timestamp,afq_complete,
                                            afqs_timestamp,afqs_complete,panas_timestamp,panas_complete,
                                            atq_timestamp,atq_complete,brief_timestamp,brief_complete,
                                            debriefing_form_timestamp,debriefing_form_complete))

## Convert first and last name columns to characters
freezing_raw$con_fn <- as.character(freezing_raw$con_fn)
freezing_raw$con_ln <- as.character(freezing_raw$con_ln)

### Make sure conversion worked - diagnostic steps ###
## examine the class of first and last name columns 
typeof(freezing_raw$con_fn)
typeof(freezing_raw$con_ln)
## examine the structure of first and last name columns 
str(freezing_raw$con_fn)
str(freezing_raw$con_ln)


## specifying column index 390 = exclusion of any incomplete data ##
freezing_raw<-freezing_raw[complete.cases(freezing_raw[ , 390]),]
#Column 390 is the last item of data collection, if the participant did not make it that far in the survey, they did not complete all measures, and they are excluded with this line of code 


### extract repeat participants ###
duplicates(freezing_raw$con_fn)
duplicates(freezing_raw$con_ln)
grepl()
duplicate_fn<-duplicates(freezing_raw$con_fn)
duplicate_ln<-duplicates(freezing_raw$con_ln)
str(duplicate_fn)
duplicate_names<-append(duplicate_fn, duplicate_ln)
duplicates(duplicate_names)
##a = c(5,7,2,9)
##ifelse(a == c(2,4,8,10),"even","odd")



#Generate a new data set for Time 2
t2_raw<-freezing_raw %>%
  dplyr::filter(con_ln %in% duplicate_ln & con_fn %in% duplicate_fn)

duplicate_t2_fn<-duplicates(t2_raw$con_fn)
duplicate_t2_ln<-duplicates(t2_raw$con_ln)
nd_t2_ln<-duplicates(t2_raw$con_ln)

t2_raw<-t2_raw %>%
  dplyr::filter(con_ln %in% duplicate_t2_ln & con_fn %in% duplicate_t2_fn) %>%
  dplyr::filter(con_fn %in% duplicate_t2_fn & con_fn %in% t2_raw) %>%
  !duplicated(t2_raw$con_fn)

ifelse(t2_raw$con_fn==t2_raw$con_fn & t2_raw$con_fn==t2_raw$con_ln,)

'%!in%' = Negate(`%in%`)
#t2_raw<-t2_raw[(t2_raw$con_ln %!in% duplicate_ln & t2_raw$con_fn %!in% duplicate_fn),]
  
#grepl(pattern, x, ignore.case = FALSE, perl = FALSE,fixed = FALSE, useBytes = FALSE)

## cutting out AFQ endorsement section ##
freezing_raw<-dplyr::select(freezing_raw,-c(afq_1:afq_24))
#Check to see if we only have complete cases in the raw dataset
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
