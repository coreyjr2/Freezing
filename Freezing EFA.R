#######################################################
### Freezing Questionnaire and Spring Data Analysis ###
#######################################################




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
install.packages("lubridate")
library(lubridate)
install.packages("psy")
library(psy)


##Import data##
freezing_raw<-read.csv(file.choose())

## Import Age of Participants

Age_1053<-read.csv(file.choose())

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
Age_1053$first_name<-trimws(Age_1053$first_name)
Age_1053$last_name<-trimws(Age_1053$last_name)

## Uniting first name and last name into one column ##
freezing_raw<-tidyr::unite(freezing_raw, name, con_fn:con_ln, sep = " ", remove = TRUE)
Age_1053<-tidyr::unite(Age_1053, name, first_name:last_name, sep = " ", remove = TRUE)

## Convert name column to character
freezing_raw$name <- as.character(freezing_raw$name)
Age_1053$name <- as.character(Age_1053$name)

### Make sure conversion worked - diagnostic steps ###
## examine the class of name column
typeof(freezing_raw$name)
typeof(Age_1053$name)

## examine the structure of name column
str(freezing_raw$name)



## specifying column index 390 = exclusion of any incomplete data ##
freezing_raw<-freezing_raw[complete.cases(freezing_raw[ , 390]),]
#Column 390 is the last item of data collection, if the participant did not make it that far in the survey, they did not complete all measures, and they are excluded with this line of code 


### Generate data set for only distinct entries
freezing_raw_d<-freezing_raw %>%
  dplyr::distinct(name, .keep_all = TRUE)


## Add Age column to Distinct Entry Data Set

freezing_raw_d$age<-Age_1053$age

## Reorder columns

freezing_raw_d<- freezing_raw_d[,c(1,2,395,3:394)]


## Depression scores by calendar date

# convert format of con date
freezing_raw_d$con_date<-mdy(freezing_raw_d$con_date)
typeof(freezing_raw_d$con_date)

# Remove outliers
freezing_raw_d_age <- freezing_raw_d %>%
  dplyr::filter (con_date >= "2020-01-31") %>%
  dplyr::filter (con_date <= "2020-05-31")

typeof(freezing_raw_d_age$age)

## Add variable specifying pre and post spring break Separate groups by consent date

freezing_raw_d_age<-freezing_raw_d_age %>%
  mutate(timepoint=case_when(
    record_id %in% 1:539 ~ "pre-spring break",
    record_id %in% 540:758 ~ "post-spring break",
  ))
## Reorder columns
freezing_raw_d_age<- freezing_raw_d_age[,c(1:4,396, 5:395)]

## Add variable to recode age

freezing_raw_d_age<-freezing_raw_d_age %>%
  mutate(age_new=case_when(
    age %in% 18 ~ "18",
    age %in% 19 ~ "19",
    age %in% 20 ~ "20",
    age %in% 21 ~ "21",
    age %in% 22 ~ "22",
    age %in% 23:27 ~ "23+"
  ))


## Add duplicated column
T1_dup_yes<-T1_dup
T1_dup_yes<-T1_dup_yes %>%
  dplyr::mutate(duplicated = "yes")

freezing_raw_d_age<-dplyr::full_join(freezing_raw_d_age, T1_dup_yes, by = "record_id")
freezing_raw_d_age<-freezing_raw_d_age[,c(1:396,791)]
freezing_raw_d_age<-freezing_raw_d_age[,c(1:5,397,6:396)]

## Replace NA with "no"
freezing_raw_d_age$duplicated<-replace_na(freezing_raw_d_age$duplicated, replace = "no")

## Plot Depression Scores
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=masq_ad_total.x, color = duplicated)) +
  geom_point(shape=10) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

dep_plot <- freezing_raw_d_age %>%
  dplyr::filter(timepoint == "post-spring break" & duplicated == "yes"| timepoint == "pre-spring break")
 

## Plot Depression Scores
ggplot(dep_plot, aes(x=con_date.x, y=masq_ad_total.x, color = duplicated)) +
  geom_point(shape=10) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)



ggplot(T1_dup, aes(x=con_date, y=masq_ad_total)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

## Create Linear Model
fit<-lm(freezing_raw_d_age$con_date~freezing_raw_d_age$masq_ad_total)
plot(fit)
anova(fit)
coefficients(fit)

## Plot
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=masq_ad_total.x, color=age_new)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)


freezing_raw_d_age$con_date<-as.Date(freezing_raw_d_age$con_date)
freezing_raw_d_age$age<-as.factor(freezing_raw_d_age$age)


pairs.panels(freezing_raw_d_age[,c(4,127)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
library(psych)

## Plot Age vs Various Scores

plot(freezing_raw_d$pswq_total, freezing_raw_d$age, main="Age & Worry",
     xlab="PSWQ Total", ylab="Age", pch=19)

plot(freezing_raw_d$masq_aa_total, freezing_raw_d$age, main="Age & Anxious Arousal",
     xlab="Anxious Arousal Total", ylab="Age", pch=19)

plot(freezing_raw_d$masq_ad_total, freezing_raw_d$age, main="Age & Anhedonic Depression",
     xlab="Anhedonic Depression Total", ylab="Age", pch=19)


## Group participants by age in separate data frames

age_18<-freezing_raw_d %>%
  dplyr::filter(freezing_raw_d$age==18)
age_19<-freezing_raw_d %>%
  dplyr::filter(freezing_raw_d$age==19)
age_20<-freezing_raw_d %>%
  dplyr::filter(freezing_raw_d$age==20)
age_21<-freezing_raw_d %>%
  dplyr::filter(freezing_raw_d$age==21)
age_22<-freezing_raw_d %>%
  dplyr::filter(freezing_raw_d$age==22)
age_23plus<-freezing_raw_d %>%
  dplyr::filter(freezing_raw_d$age>=23)


## Check for differences by age

psych::describe(age_18$pswq_total)
psych::describe(age_19$pswq_total)
psych::describe(age_20$pswq_total)
psych::describe(age_21$pswq_total)
psych::describe(age_22$pswq_total)




## Visualize Age & Worry
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=pswq_total.x, color=age_new)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se = FALSE) +
  labs(x= "date of completion", y= "pswq total") +
  labs(color= "Age")


## Seems like there is an increase in worry by time for ages 21-23+, but a decrease for those <20, let's look into this

freezing_raw_d_age<-freezing_raw_d_age %>%
  mutate(age_trend=case_when(
    age %in% 18 ~ "<=20",
    age %in% 19 ~ "<=20",
    age %in% 20 ~ "<=20",
    age %in% 21 ~ ">20",
    age %in% 22 ~ ">20",
    age %in% 23:27 ~ ">20"
  ))

## Let's visualize again
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=pswq_total.x, color=age_trend)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se = FALSE) +
  labs(x= "date of completion", y= "pswq total") +
  labs(color= "Age")

## Can we run a t.test to compare means?

t.test(pswq_total.x ~ age_trend, data = freezing_raw_d_age)

## Although the mean is higher in the >20 group, we fail to reject the null hypothesis, let's check effect size

# First make data set for each trend
age_plus20<-freezing_raw_d_age %>%
  dplyr::filter(freezing_raw_d_age$age_trend==">20")

age_less_eq20<-freezing_raw_d_age %>%
  dplyr::filter(freezing_raw_d_age$age_trend=="<=20")




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

## Create HPE and LPE totals for MASQ_ad scores in T1 & T2
T1_dup$MASQ_HPE_total<- rowSums(T1_dup[, c(86,88,89,92,95,96,98,99,101,105,110,115,118,121)])
T1_dup$MASQ_LPE_total<- rowSums(T1_dup[,c(102,97,94,91,100,107,112,124)])
T2_dup$MASQ_HPE_total<- rowSums(T2_dup[, c(86,88,89,92,95,96,98,99,101,105,110,115,118,121)])
T2_dup$MASQ_LPE_total<- rowSums(T2_dup[,c(102,97,94,91,100,107,112,124)])


psych::describe(T1_dup$MASQ_HPE_total)
psych::describe(T2_dup$MASQ_HPE_total)
psych::describe(T1_dup$MASQ_LPE_total)
psych::describe(T2_dup$MASQ_LPE_total)

## Check approach avoidance for repeated participants

T1_dup$atq_appr<-rowSums(T1_dup[, c(309,311,312,315,317,318)])
T1_dup$atq_avoi<-rowSums(T1_dup[, c(308,310,313,314,316,319)])

pre_sb$atq_appr<-rowSums(pre_sb[, c(309,311,312,315,317,318)])
pre_sb$atq_avoi<-rowSums(pre_sb[, c(308,310,313,314,316,319)])

freezing_raw_d_age$atq_appr<-rowSums(freezing_raw_d_age[, c(311,313,314,317,319,320)])
freezing_raw_d_age$atq_avoi<-rowSums(freezing_raw_d_age[, c(310,312,315,316,318,321)])
  
  
## Calculate Means for each group
library(plyr)
library(dplyr)
atq_avoi_mu <- ddply(freezing_raw_d_age, "duplicated", summarise, grp.mean=mean(atq_avoi)) 
atq_appr_mu <- ddply(freezing_raw_d_age, "duplicated", summarise, grp.mean=mean(atq_appr)) 

##Visualize repeat vs distinct participant avoidance

ggplot(freezing_raw_d_age, aes(x=atq_avoi, color=duplicated)) + 
  geom_histogram(binwidth=1, fill="white") +
  geom_vline(data=atq_avoi_mu, aes(xintercept=grp.mean, color=duplicated),
             linetype="dashed") +
  labs(x= "Avoidance Total")

ggplot(freezing_raw_d_age, aes(x=atq_appr, color=duplicated)) + 
  geom_histogram(binwidth=1, fill="white")+
  geom_vline(data=atq_appr_mu, aes(xintercept=grp.mean, color=duplicated), linetype="dashed") +
  labs(x= "Avoidance Total")

install.packages("effectsize")
library(effectsize)
cohen.D(T1_dup$atq_total, pre_sb$atq_total, method = "pooled", mu = 0, formula = NULL )

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
t.test(T1_dup$MASQ_HPE_total, T2_dup$MASQ_HPE_total, alternative = "less", paired=TRUE, var.equal = FALSE)
t.test(T1_dup$MASQ_LPE_total, T2_dup$MASQ_LPE_total, alternative = "less", paired=TRUE, var.equal = FALSE)
t.test(T1_dup$pswq_total, T2_dup$pswq_total, alternative = "less", paired=TRUE, var.equal = FALSE)


###############################################################################
##     Anxiety/Depression Pre-Spring Break vs Post-Spring Break Testing      ##
############################################################################### 

## Create MASQ High Positive Emotion (HPE) and Low Positive Emotion (LPE) columns

freezing_raw_d$MASQ_HPE_total<- rowSums(freezing_raw_d[, c(86,88,89,92,95,96,98,99,101,105,110,115,118,121)])
freezing_raw_d$MASQ_LPE_total<- rowSums( freezing_raw_d[,c(102,97,94,91,100,107,112,124)])


## Create pre-spring break & post-spring break groups
pre_sb<-freezing_raw_d %>%
  dplyr::filter(freezing_raw_d$record_id < 540)

post_sb<-freezing_raw_d %>%
  dplyr::filter(freezing_raw_d$record_id >= 540)


## Create Low Positive Affect and Depressive Mood for pre and post spring break groups

pre_sb$MASQ_HPE_total<- rowSums(pre_sb[, c(86,88,89,92,95,96,98,99,101,105,110,115,118,121)])
post_sb$MASQ_LPE_total<- rowSums(post_sb[,c(102,97,94,91,100,107,112,124)])

psych::describe(pre_sb$MASQ_HPE_total)
psych::describe(post_sb$MASQ_HPE_total)
psych::describe(pre_sb$MASQ_LPE_total)
psych::describe(post_sb$MASQ_LPE_total)


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
t.test(pre_sb$MASQ_HPE_total, post_sb$MASQ_HPE_total, alternative = "less", var.equal = FALSE)
t.test(pre_sb$MASQ_LPE_total, post_sb$MASQ_LPE_total, alternative = "less", var.equal = FALSE)
t.test(pre_sb$asi_total, post_sb$asi_total, alternative = "less", var.equal = FALSE)


############################################################
## Compare Repeated Participants to Distinct Participants ##
############################################################

t.test(pre_sb$pswq_total, T1_dup$pswq_total, alternative = "two.sided", var.equal = FALSE)
t.test(post_sb$pswq_total, T1_dup$pswq_total, alternative = "two.sided", var.equal = FALSE)
t.test(pre_sb$masq_aa_total, T1_dup$masq_aa_total, alternative = "two.sided", var.equal = FALSE)
t.test(post_sb$masq_aa_total, T1_dup$masq_aa_total, alternative = "two.sided", var.equal = FALSE)
t.test(pre_sb$atq_total, T1_dup$atq_total, alternative = "two.sided", var.equal = FALSE)
t.test(post_sb$atq_total, T1_dup$atq_total, alternative = "two.sided", var.equal = FALSE)


save(afq, file = "afq.RData")
save(afq_endorsement, file = "afq_endorsement.Rdata")

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


## Extract Freezing Item Reponses ##
afq<-freezing_raw_d %>%
  dplyr::select(afqs_1:afq_soc_total)




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
###########################################################################################
## Visualize Correlations between AFQ items & MASQ/PSWQ- each item to total factor score ##
###########################################################################################

## Add AFQ Total, PSWQ Total and MASQ Totals to the AFQ dataset

afq$pswq_total<-freezing_raw_d$pswq_total
afq$masq_aa_total<-freezing_raw_d$masq_aa_total
afq$afq_total<-rowSums(afq[,c(70:72)])

## Reorder columns ##
afq<- afq[,c(1:72,76,73:75)]


## Visualize Correlations

pairs.panels(afq[,c(1,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(2,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(3,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(4,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(5,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(6,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(7,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(8,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(9,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(10,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(11,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(12,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(13,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(14,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(15,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(16,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(17,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(18,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(19,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(20,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(21,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(22,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(23,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(24,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(25,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(26,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(27,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(28,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(29,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(30,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(31,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(32,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(33,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(34,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(35,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(36,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(37,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(38,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(39,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(40,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(41,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(42,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(43,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(44,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(45,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(46,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(47,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(48,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(49,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(50,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(51,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(52,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(53,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(54,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(55,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(56,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(57,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(58,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(59,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(60,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(61,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(62,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(63,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(64,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(65,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(66,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(67,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(68,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(afq[,c(69,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)


##################################################
#### Get comparison of ASI, MASQ AA, and PSWQ ####
##################################################

pairs.panels(freezing_raw_d[,c(22,39,125,282:284,397)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

###################################################################
####   Get comparison of ASI, MASQ AA, and PSWQ to AFQ TOTAL   ####
###################################################################

## Score AFQ total

freezing_raw_d$afq_total <- rowSums(freezing_raw_d[, c(282:284)])

psych::describe(freezing_raw_d$afq_total)
view(freezing_raw_d$afq_total)

## Get Comparison
pairs.panels(freezing_raw_d[,c(22,39,125,397)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

############################
###### Score the brief #####
############################

freezing_raw_d$brief_total <- rowSums(freezing_raw_d[, c(317:392)])



##################################################
#### Get comparison of RRQ, SIAS, and Brief ######
##################################################

pairs.panels(freezing_raw_d[,c(64,85,396,280:282)], 
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

## View Histograms of Correlations
hist(afq_cor_table$afq_cog_total)
hist(afq_cor_table$afq_phys_total)
hist(afq_cor_table$afq_soc_total)

write_tsv(afq_cor_table, "AFQ_Correlations")




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

save(efa_freeze, file = "efa_freeze.RData")
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
print(efa.obl.2, cut=.6, sort=T)

efa.obl.3<-fa(efa_freeze, nfactors = 3, rotate = "oblimin")
print(efa.obl.3, cut=.1, sort = T)

factor.congruence(efa.obl.2,efa.obl.3)

efa.obl.4<-fa(efa_freeze, nfactors = 4, rotate = "oblimin")
print(efa.obl.4, cut=.6, sort = T)

factor.congruence(efa.obl.3,efa.obl.4)

efa.obl.5<-fa(efa_freeze, nfactors = 5, rotate = "oblimin")
print(efa.obl.5, cut=.6, sort=T)

efa.obl.6<-fa(efa_freeze, nfactors = 6, rotate = "oblimin")
print(efa.obl.6, cut=.6, sort=T)

efa.obl.7<-fa(efa_freeze, nfactors = 7, rotate = "oblimin")
print(efa.obl.7, cut=.6, sort=T)

efa.obl.8<-fa(efa_freeze, nfactors = 8, rotate = "oblimin")
print(efa.obl.8, cut=.6, sort=T)



## Let's make this easier to read and sort by loading above .3 for each factor ##
print(efa.obl, cut=.5)
print(efa.obl.5, cut=.5)

# how similar are these solutions? Let's check with a congruence coefficient ##
factor.congruence(efa.obl.5,efa.obl.7)






fa.parallel(efa_freeze, fm = 'minres', fa = 'fa', plot = FALSE)
fa(efa_freeze,nfactors=9,rotate="oblimin",fm="minres")


#efa_df<-freezing_raw %>%
  #dplyr::select(masq_01:masq_89,afqs_1:afqs_70)
#fa.parallel(efa_df, fm = 'minres', fa = 'fa', plot = FALSE)
#fa(efa_df,nfactors=11,rotate="oblimin",fm="minres")




## Check Reliability Measure for Random Item Sets

# Total
afq.items<-afq[,c(1:69)]
alpha(afq.items)

# Cognitive
afq.cog.items<-afq[,c(1:28)]
alpha(afq.cog.items)

# Physical
afq.phys.items<-afq[,c(29:56)]
cronbach(afq.phys.items)

# Physical + Cognitive
afq.cogphys.items<-afq[,c(1:56)]
cronbach(afq.cogphys.items)

# Social
afq.soc.items<-afq[,c(57:69)]
cronbach(afq.soc.items)

# Phys + Soc
afq.physoc.items<-afq[,c(29:69)]
cronbach(afq.physoc.items)


afq.5fm<-afq[,c(2,8,9,18,25,26,27,31:37,43:45,52,57,58,60:69)]
cronbach(afq.5fm)

###################################
##   Pure Question Set Testing   ##
###################################

## These items were extracted based on the following criteria: 
## Pearson Correlation higher than .5 to the AFQ Total Score &
## Pearson Correlation lower than .35 to the PSWQ Total Score &
## Pearson Correlation lower than .35 to the MASQ_aa Total Score

afq.pure.items<-afq[,c(1,3,7,8,9,11,12,14,15,18,21,24,29,30,37,38,39,41,42,44,50,52,53,57,58,59,63:69)]

## Make Pure Total
afq$pure_total<-rowSums(afq[,c(1,3,7,8,9,11,12,14,15,18,21,24,29,30,37,38,39,41,42,44,50,52,53,57,58,59,63:69)])

## Generate list of pearson coefficients between items and totals ##
## make correlation matrices objects
afq_1_cor<-as.data.frame(cor(afq[, c("afqs_1", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_3_cor<-as.data.frame(cor(afq[, c("afqs_3", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_7_cor<-as.data.frame(cor(afq[, c("afqs_7", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_8_cor<-as.data.frame(cor(afq[, c("afqs_8", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_9_cor<-as.data.frame(cor(afq[, c("afqs_9", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_11_cor<-as.data.frame(cor(afq[, c("afqs_11", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_12_cor<-as.data.frame(cor(afq[, c("afqs_12", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_14_cor<-as.data.frame(cor(afq[, c("afqs_14", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_15_cor<-as.data.frame(cor(afq[, c("afqs_15", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_18_cor<-as.data.frame(cor(afq[, c("afqs_18", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_21_cor<-as.data.frame(cor(afq[, c("afqs_21", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_24_cor<-as.data.frame(cor(afq[, c("afqs_24", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_29_cor<-as.data.frame(cor(afq[, c("afqs_30", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_30_cor<-as.data.frame(cor(afq[, c("afqs_31", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_37_cor<-as.data.frame(cor(afq[, c("afqs_38", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_38_cor<-as.data.frame(cor(afq[, c("afqs_39", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_39_cor<-as.data.frame(cor(afq[, c("afqs_40", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_41_cor<-as.data.frame(cor(afq[, c("afqs_42", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_42_cor<-as.data.frame(cor(afq[, c("afqs_43", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_44_cor<-as.data.frame(cor(afq[, c("afqs_45", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_50_cor<-as.data.frame(cor(afq[, c("afqs_51", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_52_cor<-as.data.frame(cor(afq[, c("afqs_53", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_53_cor<-as.data.frame(cor(afq[, c("afqs_54", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_57_cor<-as.data.frame(cor(afq[, c("afqs_58", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_58_cor<-as.data.frame(cor(afq[, c("afqs_59", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_59_cor<-as.data.frame(cor(afq[, c("afqs_60", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_63_cor<-as.data.frame(cor(afq[, c("afqs_64", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_64_cor<-as.data.frame(cor(afq[, c("afqs_65", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_65_cor<-as.data.frame(cor(afq[, c("afqs_66", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_66_cor<-as.data.frame(cor(afq[, c("afqs_67", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_67_cor<-as.data.frame(cor(afq[, c("afqs_68", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_68_cor<-as.data.frame(cor(afq[, c("afqs_69", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_69_cor<-as.data.frame(cor(afq[, c("afqs_70", "pure_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))

## Remove Item Colum
afq_1_cor<-afq_1_cor[,-1]
afq_3_cor<-afq_3_cor[,-1]
afq_7_cor<-afq_7_cor[,-1]
afq_8_cor<-afq_8_cor[,-1]
afq_9_cor<-afq_9_cor[,-1]
afq_11_cor<-afq_11_cor[,-1]
afq_12_cor<-afq_12_cor[,-1]
afq_14_cor<-afq_14_cor[,-1]
afq_15_cor<-afq_15_cor[,-1]
afq_18_cor<-afq_18_cor[,-1]
afq_21_cor<-afq_21_cor[,-1]
afq_24_cor<-afq_24_cor[,-1]
afq_29_cor<-afq_29_cor[,-1]
afq_30_cor<-afq_30_cor[,-1]
afq_37_cor<-afq_37_cor[,-1]
afq_38_cor<-afq_38_cor[,-1]
afq_39_cor<-afq_39_cor[,-1]
afq_41_cor<-afq_41_cor[,-1]
afq_42_cor<-afq_42_cor[,-1]
afq_44_cor<-afq_44_cor[,-1]
afq_50_cor<-afq_50_cor[,-1]
afq_52_cor<-afq_52_cor[,-1]
afq_53_cor<-afq_53_cor[,-1]
afq_57_cor<-afq_57_cor[,-1]
afq_58_cor<-afq_58_cor[,-1]
afq_59_cor<-afq_59_cor[,-1]
afq_63_cor<-afq_63_cor[,-1]
afq_64_cor<-afq_64_cor[,-1]
afq_65_cor<-afq_65_cor[,-1]
afq_66_cor<-afq_66_cor[,-1]
afq_67_cor<-afq_67_cor[,-1]
afq_68_cor<-afq_68_cor[,-1]
afq_69_cor<-afq_69_cor[,-1]


## append all data frames into one

afqpure_cor_table<- dplyr::bind_rows(afq_1_cor,afq_3_cor,afq_7_cor,afq_8_cor,afq_9_cor,afq_11_cor,afq_12_cor,
                                     afq_14_cor,afq_15_cor,afq_18_cor,afq_21_cor,afq_24_cor,afq_29_cor,
                                     afq_30_cor,afq_37_cor,afq_38_cor,afq_39_cor,afq_41_cor,afq_42_cor,afq_44_cor,
                                     afq_50_cor,afq_52_cor,afq_53_cor,afq_57_cor,afq_58_cor,afq_59_cor,
                                     afq_63_cor,afq_64_cor,afq_65_cor,afq_66_cor,afq_67_cor,afq_68_cor,afq_69_cor)

## clear total score rows
ind <- seq(1, nrow(afqpure_cor_table), by=5)
afqpure_cor_table<-afqpure_cor_table[ind, ]

## Rename Columns

afqpure_cor_table<-afqpure_cor_table %>%
  rename(pure_cor = pure_total)
afqpure_cor_table<-afqpure_cor_table %>%
  rename(afq_total_cor = afq_total)
afqpure_cor_table<-afqpure_cor_table %>%
  rename(pswq_cor = pswq_total)
afqpure_cor_table<-afqpure_cor_table %>%
  rename(masq_aa_cor = masq_aa_total)

## View Histograms of Correlations
ggplot(afqpure_cor_table, aes(x=pure_cor)) + geom_histogram(color="darkblue", fill="lightblue")
ggplot(afqpure_cor_table, aes(x=afq_total_cor)) + geom_histogram(color="darkblue", fill="lightblue")
ggplot(afqpure_cor_table, aes(x=pswq_cor)) + geom_histogram(color="darkblue", fill="lightblue")
ggplot(afqpure_cor_table, aes(x=masq_aa_cor)) + geom_histogram(color="darkblue", fill="lightblue")


## Visualize Correlations between Pure_total and other totals in AFQ df
pairs.panels(afq[,c(77,73,74,75)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)


## Check reliability of Pure Items
## reminder we made this df: afq.pure.items<-afq[,c(1,3,7,8,9,11,12,14,15,18,21,24,29,30,37,38,39,41,42,44,50,52,53,57,58,59,63:69)]
alpha(afq.pure.items)











########################################
###   Confirmatory Factor Analysis   ###
########################################

## Based on theoretical development
Freeze.model <- ' 
cognitive  =~ afqs_1 + afqs_2 + afqs_3 + afqs_4 + afqs_5 + afqs_6 + afqs_7 + afqs_8 + afqs_9 +
              afqs_10 + afqs_11 + afqs_12 + afqs_13 + afqs_14 + afqs_15 + afqs_16 + afqs_17 + 
              afqs_18 + afqs_19 + afqs_20 + afqs_21 + afqs_22 + afqs_23 + afqs_24 + afqs_25 +
              afqs_26 + afqs_27 + afqs_28
physical =~ afqs_30 + afqs_31 + afqs_32 + afqs_33 + afqs_34 + afqs_35 + afqs_36 + afqs_37 + afqs_38 +
              afqs_39 + afqs_40 + afqs_41 + afqs_42 + afqs_43 + afqs_44 + afqs_45 + afqs_46 + 
              afqs_47 + afqs_48 + afqs_49 + afqs_50 + afqs_51 + afqs_52 + afqs_53 + afqs_54 +
              afqs_55 + afqs_56 + afqs_57
social   =~ afqs_58 + afqs_59 + afqs_60 + afqs_61 + afqs_62 + afqs_63 + afqs_64 + afqs_65 + afqs_66 +
              afqs_67 + afqs_68 + afqs_69 + afqs_70
'
cfa.freeze<-cfa(Freeze.model, data=efa_freeze, std.lv=TRUE, missing = 'fiml')
summary(cfa.freeze, fit.measures = T, standardized = T)
 
## Let's try the theoretical 2 factor model
Freeze.model.2 <- ' 
cogphys  =~ afqs_1 + afqs_2 + afqs_3 + afqs_4 + afqs_5 + afqs_6 + afqs_7 + afqs_8 + afqs_9 +
              afqs_10 + afqs_11 + afqs_12 + afqs_13 + afqs_14 + afqs_15 + afqs_16 + afqs_17 + 
              afqs_18 + afqs_19 + afqs_20 + afqs_21 + afqs_22 + afqs_23 + afqs_24 + afqs_25 +
              afqs_26 + afqs_27 + afqs_28 + afqs_30 + afqs_31 + afqs_32 + afqs_33 + afqs_34 + afqs_35 + afqs_36 + afqs_37 + afqs_38 +
              afqs_39 + afqs_40 + afqs_41 + afqs_42 + afqs_43 + afqs_44 + afqs_45 + afqs_46 + 
              afqs_47 + afqs_48 + afqs_49 + afqs_50 + afqs_51 + afqs_52 + afqs_53 + afqs_54 +
              afqs_55 + afqs_56 + afqs_57
social   =~ afqs_58 + afqs_59 + afqs_60 + afqs_61 + afqs_62 + afqs_63 + afqs_64 + afqs_65 + afqs_66 +
              afqs_67 + afqs_68 + afqs_69 + afqs_70
'

cfa.2.fit<-cfa(Freeze.model.2, data=efa_freeze, std.lv=TRUE, missing = 'fiml')
summary(cfa.2.fit, fit.measures = T, standardized = T)


## Now, let's listen to the data. According to our EFA....

## Let's try a 3 factor model, (note the .6 cutoff)
freeze.model.3 <- '
physical stopping =~ afqs_32 + afqs_33 + afqs_34 + afqs_35 + afqs_36 + afqs_37 + afqs_51
cognitive overload =~ afqs_2 + afqs_8 + afqs_9 + afqs_10 + afqs_11 + afqs_12 + afqs_15 + afqs_16 + afqs_17 + afqs_18
                      + afqs_28
social =~ afqs_58 + afqs_59 + afqs_61 + afqs_62 + afqs_63 + afqs_64 + afqs_65 + afqs_66 +
          afqs_67 + afqs_68 + afqs_69 + afqs_70
'
cfa.3.fit<-cfa(freeze.model.3, data=efa_freeze, std.lv=TRUE, missing = 'fiml')
summary(cfa.3.fit, fit.measures = T, standardized = T)

## Let's try a 5 factor model, (note the .6 cutoff)
freeze.model.5 <- '
panic =~ afqs_38 + afqs_44 + afqs_45 + afqs_46 + afqs_53
cognitive overload =~ afqs_2 + afqs_8 + afqs_9 + afqs_18
physical stopping =~ afqs_32 + afqs_33 + afqs_34 + afqs_35 + afqs_36 + afqs_37 
recovery =~ afqs_25 + afqs_26 + afqs_27 
social =~ afqs_58 + afqs_59 + afqs_61 + afqs_62 + afqs_63 + afqs_64 + afqs_65 + afqs_66 +
          afqs_67 + afqs_68 + afqs_69 + afqs_70
'
cfa.5.fit<-cfa(freeze.model.5, data=efa_freeze, std.lv=TRUE, missing = 'fiml')
summary(cfa.5.fit, fit.measures = T, standardized = T)

## Let's try a 7 factor model, (note the .6 cutoff)
freeze.model.7<- '
cognitive  =~ afqs_2 + afqs_8 + afqs_9 + afqs_18 
physical =~ afqs_32 + afqs_33 + afqs_34 + afqs_35 + afqs_36 + afqs_37 
panic =~ afqs_38 + afqs_45
recovery =~ afqs_25 + afqs_26 + afqs_27 
social   =~ afqs_58 + afqs_59 + afqs_61 + afqs_62 + afqs_63 + afqs_64 + afqs_65 + afqs_66 +
  afqs_67 + afqs_68 + afqs_69 + afqs_70
  '
cfa.7.fit<-cfa(freeze.model.7, data=efa_freeze, std.lv=TRUE, missing = 'fiml')
summary(cfa.7.fit, fit.measures = T, standardized = T)


## Save for Markdown
save(freezing_raw_d, file = "freezing_raw_d.RData")
save(age_18,file = "age_18.RData")
save(age_19,file = "age_19.RData")
save(age_20,file = "age_20.RData")
save(age_21,file = "age_21.RData")
save(age_22,file = "age_22.RData")
save(age_23plus,file = "age_23plus.RData")
save(dep_plot, file = "dep_plot.RData")
save(freezing_raw_d_age, file = "freezing_raw_d_age.RData")
save(T1_dup, file = "T1_dup.RData")

