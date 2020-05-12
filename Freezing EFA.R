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
install.packages("ggplot2")
library(ggplot2)
install.packages("corrplot")
library(corrplot)
install.packages("GPArotation")
library(GPArotation)
install.packages("qdap")
library(qdap)
install.packages("nFactors")
library(nFactors)

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
##     Traditional EFA for Freezing (AFQ)        ##
###################################################   

## The following steps will only test the distinct entries, no participant duplicates are included ##


## cutting out AFQ endorsement section ##
freezing_raw_d<-dplyr::select(freezing_raw_d,-c(afq_1:afq_24))
#Check to see if we only have complete cases in the raw dataset
complete.cases(freezing_raw_d)     




## Set data to include anxious freezing symptom values ##
efa_freeze<-freezing_raw_d %>%
  dplyr::select(afqs_1:afqs_70)

## Determine number of factors to extract ##

## Get eigenvalues ##
ev<-eigen(cor(efa_freeze))

## conduct parallel analysis ##
ap<-parallel(subject = nrow(efa_freeze), var=ncol(efa_freeze), rep=100, cent=0.05)

## Show analysis of factors to retain ##
nS<-nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

## Look for the number of components associated to the last severe slope change (i.e. what is the number of components before the slope flattens) ##
## Remember to stay flexible. Scree plot indicates anywhere from 3-7 factors in our case ##




## After # of factors has been decided upon, rotate your data ##

## orthogonal rotation - assumes no correlation between components/factors ##
efa.ort<-fa(efa_freeze, nfactors = 4, rotate = "varimax")
print(efa.ort)

## Let's make this easier to read and sort by loading above .3 for each factor##
print(efa.ort, cut=.4)


## How about oblique rotation - assumes correlation between components/factors ##
efa.obl.1<-fa(efa_freeze, nfactors = 1, rotate = "oblimin")
print(efa.obl.1)


efa.obl.2<-fa(efa_freeze, nfactors = 2, rotate = "oblimin")
print(efa.obl.2, cut=.5, sort=T)

efa.obl.3<-fa(efa_freeze, nfactors = 3, rotate = "oblimin")
print(efa.obl.3, cut=.5, sort = T)

factor.congruence(efa.obl.2,efa.obl.3)

efa.obl.4<-fa(efa_freeze, nfactors = 4, rotate = "oblimin")
print(efa.obl.4, cut=.5, sort = T)

factor.congruence(efa.obl.3,efa.obl.4)

efa.obl.5<-fa(efa_freeze, nfactors = 5, rotate = "oblimin")
print(efa.obl.5, cut=.5, sort=T)

efa.obl.6<-fa(efa_freeze, nfactors = 6, rotate = "oblimin")
print(efa.obl.6, cut=.5, sort=T)

efa.obl.7<-fa(efa_freeze, nfactors = 7, rotate = "oblimin")
print(efa.obl.7, cut=.4, sort=T)

## Let's make this easier to read and sort by loading above .3 for each factor ##
print(efa.obl, cut=.5)
print(efa.obl.5, cut=.5)

# how similar are these solutions? Let's check with a congruence coefficient ##
factor.congruence(efa.obl.5,efa.obl.1)






fa.parallel(efa_freeze, fm = 'minres', fa = 'fa', plot = FALSE)
fa(efa_freeze,nfactors=9,rotate="oblimin",fm="minres")


#efa_df<-freezing_raw %>%
  #dplyr::select(masq_01:masq_89,afqs_1:afqs_70)
#fa.parallel(efa_df, fm = 'minres', fa = 'fa', plot = FALSE)
#fa(efa_df,nfactors=11,rotate="oblimin",fm="minres")
