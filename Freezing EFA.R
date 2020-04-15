##############################
### Freezing EFA Analysis ###
##############################


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

##Import data##
freezing_raw<-read.csv(file.choose())


######################################
## data cleaning - removing columns ##
######################################

## Remove unneeded columns from the dataset, refer to data dictionary for more info 
freezing_raw<-dplyr::select(freezing_raw,-c(redcap_survey_identifier,consent_form_timestamp,
                                            sona_id,con_sig,con_email,consent_form_complete,
                                            asi_timestamp,asi_complete,pswq_timestamp,pswq_complete,
                                            rrq_timestamp,rrq_complete,sias_timestamp,sias_complete,
                                            masq_timestamp,masq_complete,lec_timestamp,lec_complete,
                                            siasii_timestamp,siasii_complete,afq_timestamp,afq_complete,
                                            afqs_timestamp,afqs_complete,panas_timestamp,panas_complete,
                                            atq_timestamp,atq_complete,brief_timestamp,brief_complete,
                                            debriefing_form_timestamp,debriefing_form_complete))

## clear white space in first and last name columns 
freezing_raw$con_fn<-trimws(freezing_raw$con_fn)
freezing_raw$con_ln<-trimws(freezing_raw$con_ln)

## Uniting first name and last name into one column ##
freezing_raw<-tidyr::unite(freezing_raw, name, con_fn:con_ln, sep = " ", remove = TRUE)


## Convert name column to character
freezing_raw$name <- as.character(freezing_raw$name)


### Make sure conversion worked - diagnostic steps ###
## examine the class of name column
typeof(freezing_raw$name)

## examine the structure of name column
str(freezing_raw$name)


## specifying column index 390 = exclusion of any incomplete data ##
freezing_raw<-freezing_raw[complete.cases(freezing_raw[ , 390]),]
#Column 390 is the last item of data collection, if the participant did not make it that far in the survey, they did not complete all measures, and they are excluded with this line of code 



### extract repeat participants & Generate a new data set for Time 2 ###
T2_data<-freezing_raw %>%
  dplyr::filter(duplicated(name))

freezing_raw_d<-freezing_raw %>%
  dplyr::distinct(name, .keep_all = TRUE)
                



###################################################
##  Traditional EFA for Freezing (MASQ & AFQ)    ##
###################################################   


## cutting out AFQ endorsement section ##
freezing_raw<-dplyr::select(freezing_raw,-c(afq_1:afq_24))
#Check to see if we only have complete cases in the raw dataset
complete.cases(freezing_raw)     


efa_freeze<-freezing_raw %>%
  dplyr::select(afqs_1:afqs_70)
fa.parallel(efa_freeze, fm = 'minres', fa = 'fa', plot = FALSE)
fa(efa_freeze,nfactors=9,rotate="oblimin",fm="minres")


efa_df<-freezing_raw %>%
  dplyr::select(masq_01:masq_89,afqs_1:afqs_70)
fa.parallel(efa_df, fm = 'minres', fa = 'fa', plot = FALSE)
fa(efa_df,nfactors=11,rotate="oblimin",fm="minres")
