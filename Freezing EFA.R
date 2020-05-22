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
install.packages("lavaan")
library(lavaan)
install.packages("ggthemes")
library(ggthemes)
install.packages("ggpubr")
library(ggpubr)
install.packages("janitor")
library(janitor)

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



### ############################################# ###
### Generate data sets of all repeat participants ###
### ############################################# ###

duplicated<-freezing_raw %>%
  janitor::get_dupes(name)

## Clean duplicated data set: remove duplicates that had less than a week between entries **clean up and dont include names, use con_date

duplicated<-duplicated %>%
  dplyr::filter(duplicated$name != "Alexis Campos",duplicated$name != "Charlie Trost",
                duplicated$name != "Chris Chang", duplicated$name != "Grace Kuehn",
                duplicated$name != "Jonathan Xue", duplicated$name != "Jose Navarro", 
                duplicated$name != "Matthew Boyd", duplicated$name != "maryam shaikh",
                duplicated$name != "Melanie Alarcon", duplicated$name != "Nicholas Severin",
                duplicated$name != "Niv Ostroff", duplicated$name != "othmane ezzabdi", 
                duplicated$name != "Safia Tayyabi", duplicated$name != "Sean Gardiner")


## Generate Time 1 entry of duplicated participants
T1_dup<-duplicated %>%
  dplyr::filter(!duplicated(name))

## Generate Time 2 entry of duplicated participants
T2_dup<-duplicated %>%
  dplyr::filter(duplicated(name))

## Remove repeated entries within Time 2 due to questionnaire setup issue
T2_dup<-T2_dup %>%
  dplyr::filter(!duplicated(name))

## Test for Normality & check descriptive stats
ggqqplot(T1_dup$asi_total)
summary(T1_dup$asi_total)
ggqqplot(T1_dup$masq_aa_total)
summary(T1_dup$masq_aa_total)
ggqqplot(T1_dup$masq_ad_total)
summary(T1_dup$masq_ad_total)
ggqqplot(T1_dup$pswq_total)
summary(T1_dup$pswq_total)

ggqqplot(T2_dup$asi_total)
summary(T2_dup$asi_total)
ggqqplot(T2_dup$masq_aa_total)
summary(T2_dup$masq_aa_total)
ggqqplot(T2_dup$masq_ad_total)
summary(T2_dup$masq_ad_total)
ggqqplot(T2_dup$pswq_total)
summary(T2_dup$pswq_total)

## Test for Variance
var.test(T1_dup$asi_total, T2_dup$asi_total, ratio = 1, alternative = "two.sided")
var.test(T1_dup$masq_aa_total, T2_dup$masq_aa_total, ratio = 1, alternative = "two.sided")
var.test(T1_dup$masq_ad_total, T2_dup$masq_ad_total, ratio = 1, alternative = "two.sided")
var.test(T1_dup$pswq_total, T2_dup$pswq_total, ratio = 1, alternative = "two.sided")

## Anxiety T Tests, unequal variances, T1 & T2 ##

t.test(T1_dup$asi_total, T2_dup$asi_total, alternative = "less", paired=TRUE, var.equal = FALSE)
t.test(T1_dup$masq_aa_total, T2_dup$masq_aa_total, alternative = "less", paired=TRUE, var.equal = FALSE)
t.test(T1_dup$masq_ad_total, T2_dup$masq_ad_total, alternative = "less", paired=TRUE, var.equal = FALSE)
t.test(T1_dup$pswq_total, T2_dup$pswq_total, alternative = "less", paired=TRUE, var.equal = FALSE)

###############################################################################
##        Rational Scale Development - Item Test Correlations                ##
############################################################################### 
## Extract Freezing Endorsement Reponses ##
afq_endorsement<-freezing_raw_d %>%
  dplyr::select(afq_1:afq_24)

psych::describe(afq_endorsement$afq_1)
psych::describe(afq_endorsement$afq_3)
psych::describe(afq_endorsement$afq_5)
psych::describe(afq_endorsement$afq_7)
psych::describe(afq_endorsement$afq_9)
psych::describe(afq_endorsement$afq_11)
psych::describe(afq_endorsement$afq_13)
psych::describe(afq_endorsement$afq_15)
psych::describe(afq_endorsement$afq_17)
psych::describe(afq_endorsement$afq_19)
psych::describe(afq_endorsement$afq_21)
psych::describe(afq_endorsement$afq_23)






### Generate data set for only distinct entries
freezing_raw_d<-freezing_raw %>%
  dplyr::distinct(name, .keep_all = TRUE)

## Examine Cognitive Totals ##
hist(freezing_raw_d$afq_cog_total)
summary(freezing_raw_d$afq_cog_total)
describe(freezing_raw_d$afq_cog_total)


## Examine Physical Totals ##
hist(freezing_raw_d$afq_phys_total)
summary(freezing_raw_d$afq_phys_total)
describe(freezing_raw_d$afq_phys_total)


## Examine Social-Evaluative Totals ##
hist(freezing_raw_d$afq_soc_total)
summary(freezing_raw_d$afq_soc_total)
describe(freezing_raw_d$afq_soc_total)

## Extract Freezing Item Reponses ##
afq<-freezing_raw_d %>%
  dplyr::select(afqs_1:afq_soc_total)

## Visualize Correlations - each item to total factor score ##
?pairs.panels

pairs.panels(afq[,c(1,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(2,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(3,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(4,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(5,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(6,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(7,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(8,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(9,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(10,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(11,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(12,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(13,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(14,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(15,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(16,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(17,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(18,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(19,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(20,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(21,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(22,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(23,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(24,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(25,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(26,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(27,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(28,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(29,70:72)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

## Generate list of pearson coefficients between items and totals ##

## make correlation matrices objects

afq_1<-as.data.frame(cor(afq[, c("afqs_1", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_2<-as.data.frame(cor(afq[, c("afqs_2", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_3<-as.data.frame(cor(afq[, c("afqs_3", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_4<-as.data.frame(cor(afq[, c("afqs_4", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_5<-as.data.frame(cor(afq[, c("afqs_5", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_6<-as.data.frame(cor(afq[, c("afqs_6", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_7<-as.data.frame(cor(afq[, c("afqs_7", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_8<-as.data.frame(cor(afq[, c("afqs_8", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_9<-as.data.frame(cor(afq[, c("afqs_9", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_10<-as.data.frame(cor(afq[, c("afqs_10", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_11<-as.data.frame(cor(afq[, c("afqs_11", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_12<-as.data.frame(cor(afq[, c("afqs_12", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_13<-as.data.frame(cor(afq[, c("afqs_13", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_14<-as.data.frame(cor(afq[, c("afqs_14", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_15<-as.data.frame(cor(afq[, c("afqs_15", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_16<-as.data.frame(cor(afq[, c("afqs_16", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_17<-as.data.frame(cor(afq[, c("afqs_17", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_18<-as.data.frame(cor(afq[, c("afqs_18", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_19<-as.data.frame(cor(afq[, c("afqs_19", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_20<-as.data.frame(cor(afq[, c("afqs_20", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_21<-as.data.frame(cor(afq[, c("afqs_21", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_22<-as.data.frame(cor(afq[, c("afqs_22", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_23<-as.data.frame(cor(afq[, c("afqs_23", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_24<-as.data.frame(cor(afq[, c("afqs_24", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_25<-as.data.frame(cor(afq[, c("afqs_25", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_26<-as.data.frame(cor(afq[, c("afqs_26", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_27<-as.data.frame(cor(afq[, c("afqs_27", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_28<-as.data.frame(cor(afq[, c("afqs_28", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_29<-as.data.frame(cor(afq[, c("afqs_30", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_30<-as.data.frame(cor(afq[, c("afqs_31", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_31<-as.data.frame(cor(afq[, c("afqs_32", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_32<-as.data.frame(cor(afq[, c("afqs_33", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_33<-as.data.frame(cor(afq[, c("afqs_34", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_34<-as.data.frame(cor(afq[, c("afqs_35", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_35<-as.data.frame(cor(afq[, c("afqs_36", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_36<-as.data.frame(cor(afq[, c("afqs_37", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_37<-as.data.frame(cor(afq[, c("afqs_38", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_38<-as.data.frame(cor(afq[, c("afqs_39", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_39<-as.data.frame(cor(afq[, c("afqs_40", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_40<-as.data.frame(cor(afq[, c("afqs_41", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_41<-as.data.frame(cor(afq[, c("afqs_42", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_42<-as.data.frame(cor(afq[, c("afqs_43", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_43<-as.data.frame(cor(afq[, c("afqs_44", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_44<-as.data.frame(cor(afq[, c("afqs_45", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_45<-as.data.frame(cor(afq[, c("afqs_46", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_46<-as.data.frame(cor(afq[, c("afqs_47", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_47<-as.data.frame(cor(afq[, c("afqs_48", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_48<-as.data.frame(cor(afq[, c("afqs_49", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_49<-as.data.frame(cor(afq[, c("afqs_50", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_50<-as.data.frame(cor(afq[, c("afqs_51", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_51<-as.data.frame(cor(afq[, c("afqs_52", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_52<-as.data.frame(cor(afq[, c("afqs_53", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_53<-as.data.frame(cor(afq[, c("afqs_54", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_54<-as.data.frame(cor(afq[, c("afqs_55", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_55<-as.data.frame(cor(afq[, c("afqs_56", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_56<-as.data.frame(cor(afq[, c("afqs_57", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_57<-as.data.frame(cor(afq[, c("afqs_58", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_58<-as.data.frame(cor(afq[, c("afqs_59", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_59<-as.data.frame(cor(afq[, c("afqs_60", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_60<-as.data.frame(cor(afq[, c("afqs_61", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_61<-as.data.frame(cor(afq[, c("afqs_62", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_62<-as.data.frame(cor(afq[, c("afqs_63", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_63<-as.data.frame(cor(afq[, c("afqs_64", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_64<-as.data.frame(cor(afq[, c("afqs_65", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_65<-as.data.frame(cor(afq[, c("afqs_66", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_66<-as.data.frame(cor(afq[, c("afqs_67", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_67<-as.data.frame(cor(afq[, c("afqs_68", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_68<-as.data.frame(cor(afq[, c("afqs_69", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))
afq_69<-as.data.frame(cor(afq[, c("afqs_70", "afq_cog_total", "afq_phys_total", "afq_soc_total")], method = "pearson"))

## renaming column title 

afq_1<-afq_1 %>%
  rename(item = afqs_1)

afq_2<-afq_2 %>%
  rename(item = afqs_2)

afq_3<-afq_3 %>%
  rename(item = afqs_3)

afq_4<-afq_4 %>%
  rename(item = afqs_4)

afq_5<-afq_5 %>%
  rename(item = afqs_5)

afq_6<-afq_6 %>%
  rename(item = afqs_6)

afq_7<-afq_7 %>%
  rename(item = afqs_7)

afq_8<-afq_8 %>%
  rename(item = afqs_8)

afq_9<-afq_9 %>%
  rename(item = afqs_9)

afq_10<-afq_10 %>%
  rename(item = afqs_10)

afq_11<-afq_11 %>%
  rename(item = afqs_11)

afq_12<-afq_12 %>%
  rename(item = afqs_12)

afq_13<-afq_13 %>%
  rename(item = afqs_13)

afq_14<-afq_14 %>%
  rename(item = afqs_14)

afq_15<-afq_15 %>%
  rename(item = afqs_15)

afq_16<-afq_16 %>%
  rename(item = afqs_16)

afq_17<-afq_17 %>%
  rename(item = afqs_17)

afq_18<-afq_18 %>%
  rename(item = afqs_18)

afq_19<-afq_19 %>%
  rename(item = afqs_19)

afq_20<-afq_20 %>%
  rename(item = afqs_20)

afq_21<-afq_21 %>%
  rename(item = afqs_21)

afq_22<-afq_22 %>%
  rename(item = afqs_22)

afq_23<-afq_23 %>%
  rename(item = afqs_23)

afq_24<-afq_24 %>%
  rename(item = afqs_24)

afq_25<-afq_25 %>%
  rename(item = afqs_25)

afq_26<-afq_26 %>%
  rename(item = afqs_26)

afq_27<-afq_27 %>%
  rename(item = afqs_27)

afq_28<-afq_28 %>%
  rename(item = afqs_28)

afq_29<-afq_29 %>%
  rename(item = afqs_30)

afq_30<-afq_30 %>%
  rename(item = afqs_31)

afq_31<-afq_31 %>%
  rename(item = afqs_32)

afq_32<-afq_32 %>%
  rename(item = afqs_33)

afq_33<-afq_33 %>%
  rename(item = afqs_34)

afq_34<-afq_34 %>%
  rename(item = afqs_35)

afq_35<-afq_35 %>%
  rename(item = afqs_36)

afq_36<-afq_36 %>%
  rename(item = afqs_37)

afq_37<-afq_37 %>%
  rename(item = afqs_38)

afq_38<-afq_38 %>%
  rename(item = afqs_39)

afq_39<-afq_39 %>%
  rename(item = afqs_40)

afq_40<-afq_40 %>%
  rename(item = afqs_41)

afq_41<-afq_41 %>%
  rename(item = afqs_42)

afq_42<-afq_42 %>%
  rename(item = afqs_43)

afq_43<-afq_43 %>%
  rename(item = afqs_44)

afq_44<-afq_44 %>%
  rename(item = afqs_45)

afq_45<-afq_45 %>%
  rename(item = afqs_46)

afq_46<-afq_46 %>%
  rename(item = afqs_47)

afq_47<-afq_47 %>%
  rename(item = afqs_48)

afq_48<-afq_48 %>%
  rename(item = afqs_49)

afq_49<-afq_49 %>%
  rename(item = afqs_50)

afq_50<-afq_50 %>%
  rename(item = afqs_51)

afq_51<-afq_51 %>%
  rename(item = afqs_52)

afq_52<-afq_52 %>%
  rename(item = afqs_53)

afq_53<-afq_53 %>%
  rename(item = afqs_54)

afq_54<-afq_54 %>%
  rename(item = afqs_55)

afq_55<-afq_55 %>%
  rename(item = afqs_56)

afq_56<-afq_56 %>%
  rename(item = afqs_57)

afq_57<-afq_57 %>%
  rename(item = afqs_58)

afq_58<-afq_58 %>%
  rename(item = afqs_59)

afq_59<-afq_59 %>%
  rename(item = afqs_60)

afq_60<-afq_60 %>%
  rename(item = afqs_61)

afq_61<-afq_61 %>%
  rename(item = afqs_62)

afq_62<-afq_62 %>%
  rename(item = afqs_63)

afq_63<-afq_63 %>%
  rename(item = afqs_64)

afq_64<-afq_64 %>%
  rename(item = afqs_65)

afq_65<-afq_65 %>%
  rename(item = afqs_66)

afq_66<-afq_66 %>%
  rename(item = afqs_67)

afq_67<-afq_67 %>%
  rename(item = afqs_68)

afq_68<-afq_68 %>%
  rename(item = afqs_69)

afq_69<-afq_69 %>%
  rename(item = afqs_70)

## Preserve first row of each data set

afq_1<-afq_1[ -c(2:4), ]
afq_2<-afq_2[ -c(2:4), ]
afq_3<-afq_3[ -c(2:4), ]
afq_4<-afq_4[ -c(2:4), ]
afq_5<-afq_5[ -c(2:4), ]
afq_6<-afq_6[ -c(2:4), ]
afq_7<-afq_7[ -c(2:4), ]
afq_8<-afq_8[ -c(2:4), ]
afq_9<-afq_9[ -c(2:4), ]
afq_10<-afq_10[ -c(2:4), ]
afq_11<-afq_11[ -c(2:4), ]
afq_12<-afq_12[ -c(2:4), ]
afq_13<-afq_13[ -c(2:4), ]
afq_14<-afq_14[ -c(2:4), ]
afq_15<-afq_15[ -c(2:4), ]
afq_16<-afq_16[ -c(2:4), ]
afq_17<-afq_17[ -c(2:4), ]
afq_18<-afq_18[ -c(2:4), ]
afq_19<-afq_19[ -c(2:4), ]
afq_20<-afq_20[ -c(2:4), ]
afq_21<-afq_21[ -c(2:4), ]
afq_22<-afq_22[ -c(2:4), ]
afq_23<-afq_23[ -c(2:4), ]
afq_24<-afq_24[ -c(2:4), ]
afq_25<-afq_25[ -c(2:4), ]
afq_26<-afq_26[ -c(2:4), ]
afq_27<-afq_27[ -c(2:4), ]
afq_28<-afq_28[ -c(2:4), ]
afq_29<-afq_29[ -c(2:4), ]
afq_30<-afq_30[ -c(2:4), ]
afq_31<-afq_31[ -c(2:4), ]
afq_32<-afq_32[ -c(2:4), ]
afq_33<-afq_33[ -c(2:4), ]
afq_34<-afq_34[ -c(2:4), ]
afq_35<-afq_35[ -c(2:4), ]
afq_36<-afq_36[ -c(2:4), ]
afq_37<-afq_37[ -c(2:4), ]
afq_38<-afq_38[ -c(2:4), ]
afq_39<-afq_39[ -c(2:4), ]
afq_40<-afq_40[ -c(2:4), ]
afq_41<-afq_41[ -c(2:4), ]
afq_42<-afq_42[ -c(2:4), ]
afq_43<-afq_43[ -c(2:4), ]
afq_44<-afq_44[ -c(2:4), ]
afq_45<-afq_45[ -c(2:4), ]
afq_46<-afq_46[ -c(2:4), ]
afq_47<-afq_47[ -c(2:4), ]
afq_48<-afq_48[ -c(2:4), ]
afq_49<-afq_49[ -c(2:4), ]
afq_50<-afq_50[ -c(2:4), ]
afq_51<-afq_51[ -c(2:4), ]
afq_52<-afq_52[ -c(2:4), ]
afq_53<-afq_53[ -c(2:4), ]
afq_54<-afq_54[ -c(2:4), ]
afq_55<-afq_55[ -c(2:4), ]
afq_56<-afq_56[ -c(2:4), ]
afq_57<-afq_57[ -c(2:4), ]
afq_58<-afq_58[ -c(2:4), ]
afq_59<-afq_59[ -c(2:4), ]
afq_60<-afq_60[ -c(2:4), ]
afq_61<-afq_61[ -c(2:4), ]
afq_62<-afq_62[ -c(2:4), ]
afq_63<-afq_63[ -c(2:4), ]
afq_64<-afq_64[ -c(2:4), ]
afq_65<-afq_65[ -c(2:4), ]
afq_66<-afq_66[ -c(2:4), ]
afq_67<-afq_67[ -c(2:4), ]
afq_68<-afq_68[ -c(2:4), ]
afq_69<-afq_69[ -c(2:4), ]



## append all data frames into one

afq_cor_table<- dplyr::bind_rows(afq_1,afq_2,afq_3,afq_4,afq_5,afq_6,afq_7,afq_8,afq_9,afq_10,afq_11,afq_12,afq_13,afq_14,afq_15,afq_16,afq_17,afq_18,afq_19,
                                 afq_20,afq_21,afq_22,afq_23,afq_24,afq_25,afq_26,afq_27,afq_28,afq_29,afq_30,afq_31,afq_32,afq_33,afq_34,afq_35,afq_36,afq_37,
                                 afq_38,afq_39,afq_40,afq_41,afq_42,afq_43,afq_44,afq_45,afq_46,afq_47,afq_48,afq_49,afq_50,afq_51,afq_52,afq_53,afq_54,afq_55,
                                 afq_56,afq_57,afq_58,afq_59,afq_60,afq_61,afq_62,afq_63,afq_64,afq_65,afq_66,afq_67,afq_68,afq_69)

## clear item column

afq_cor_table<-afq_cor_table[,-1]

write_tsv(afq_cor_table, "AFQ_Correlations")



###############################################################################
##         Anxiety Pre-Spring Break vs Post-Spring Break Testing             ##
############################################################################### 

## Create pre-spring break & post-spring break groups
pre_sb<-freezing_raw_d %>%
  dplyr::filter(freezing_raw_d$record_id < 540)

post_sb<-freezing_raw_d %>%
  dplyr::filter(freezing_raw_d$record_id >= 540)

## Although our sample size is large enough, test for normality ##
ggqqplot(pre_sb$asi_total)
ggqqplot(pre_sb$masq_aa_total)
ggqqplot(pre_sb$masq_ad_total)
ggqqplot(pre_sb$pswq_total)


ggqqplot(post_sb$asi_total)
ggqqplot(post_sb$masq_aa_total)
ggqqplot(post_sb$masq_ad_total)
ggqqplot(post_sb$pswq_total)

## Check for variance ##

var.test(pre_sb$asi_total, post_sb$asi_total, alternative =  "two.sided")
hist(pre_sb$asi_total)
hist(post_sb$asi_total)

var.test(pre_sb$masq_aa_total, post_sb$masq_aa_total, alternative =  "two.sided")
hist(pre_sb$masq_aa_total)
hist(post_sb$masq_aa_total)

var.test(pre_sb$masq_ad_total, post_sb$masq_ad_total, alternative =  "two.sided")
par(mfrow=c(1,2))
hist(pre_sb$masq_ad_total)
hist(post_sb$masq_ad_total)

var.test(pre_sb$pswq_total, post_sb$pswq_total, alternative =  "two.sided")

## T-Test for Anxiety Totals ##

t.test(pre_sb$pswq_total, post_sb$pswq_total, alternative = "less", var.equal = FALSE)
t.test(pre_sb$masq_aa_total, post_sb$masq_aa_total, alternative = "less", var.equal = FALSE)
t.test(pre_sb$masq_ad_total, post_sb$masq_ad_total, alternative = "less", var.equal = FALSE)
t.test(pre_sb$asi_total, post_sb$asi_total, alternative = "less", var.equal = FALSE)


save(afq, file = "afq.RData")
save(afq_endorsement, file = "afq_endorsement.Rdata")

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
