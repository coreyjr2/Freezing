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
library(plyr)
library(dplyr)
library(tidyr)

##Import data **be sure to order by name before importing so the order of participants matches the Age file
freezing_raw<-read.csv(file.choose())

## Import Age of Participants **be sure to order by name before importing so the order of participants matches the full data file

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

## specifying column index 390 = exclusion of any incomplete data ##
freezing_raw<-freezing_raw[complete.cases(freezing_raw[ , 390]),]
#Column 390 is the last item of data collection, if the participant did not make it that far in the survey, they did not complete all measures, and they are excluded with this line of code 

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




################################################################################
### Let's separate and generate data sets for repeat & distinct participants ###
################################################################################


## Create Duplicated df
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
## Duplicated groups and dfs will be used later to analyze within subject findings

### Now, let's generate a working data set for only the distinct entries
freezing_raw_d<-freezing_raw %>%
  dplyr::distinct(name, .keep_all = TRUE)


## Add Age column to Distinct Entry Data Set

freezing_raw_d$age<-Age_1053$age

## Reorder columns

freezing_raw_d<- freezing_raw_d[,c(1,2,395,3:394)]

# convert format of con date
freezing_raw_d$con_date<-mdy(freezing_raw_d$con_date)
typeof(freezing_raw_d$con_date)

# Remove outliers
freezing_raw_d_age <- freezing_raw_d %>%
  dplyr::filter (con_date >= "2020-01-31") %>%
  dplyr::filter (con_date <= "2020-05-31")



#########################################
### Add columns for grouping analysis ###
#########################################

## Timepoint
## Add variable specifying pre and post spring break Separate groups by consent date

freezing_raw_d_age<-freezing_raw_d_age %>%
  mutate(timepoint=case_when(
    record_id %in% 1:539 ~ "pre-spring break",
    record_id %in% 540:758 ~ "post-spring break",
  ))

## Reorder columns
freezing_raw_d_age<- freezing_raw_d_age[,c(1:4,396, 5:395)]


## Duplicated
## Add variable specifying whether or not they repeated our questionnaire

T1_dup_yes<-T1_dup
T1_dup_yes<-T1_dup_yes %>%
  dplyr::mutate(duplicated = "yes")

freezing_raw_d_age<-dplyr::full_join(freezing_raw_d_age, T1_dup_yes, by = "record_id")

## Replace NA with "no"
freezing_raw_d_age$duplicated<-replace_na(freezing_raw_d_age$duplicated, replace = "no")

## Reorder Columns
freezing_raw_d_age<-freezing_raw_d_age[,c(1:396,791)]
freezing_raw_d_age<-freezing_raw_d_age[,c(1:5,397,6:396)]

## age_new
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

## age_trend
## Add variable to recode for age trends, separately

freezing_raw_d_age<-freezing_raw_d_age %>%
  mutate(age_trend=case_when(
    age %in% 18 ~ "<=20",
    age %in% 19 ~ "<=20",
    age %in% 20 ~ "<=20",
    age %in% 21 ~ ">20",
    age %in% 22 ~ ">20",
    age %in% 23:27 ~ ">20"
  ))


## Group participants by age in separate data frames for ease when extracting descriptive stats of particular scores by age

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




#########################################
###  Visualize Depression & Anxiety   ###
#########################################

###########
# MASQ AD #
###########

## Plot Depression Scores by Duplicated vs Distinct
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=masq_ad_total.x, color = duplicated)) +
  geom_point(shape=10) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)


## Plot Depression scores by Age
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=masq_ad_total.x, color=age_new)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

## Plot Depression scores by Age Trend
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=masq_ad_total.x, color=age_trend)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

## Create Linear Model
fit<-lm(freezing_raw_d_age$con_date~freezing_raw_d_age$masq_ad_total)
plot(fit)
anova(fit)
coefficients(fit)

## Let's also check for patterns in Anhedonic Depression subsections, Low Positive Affect & Depressive Mood


## Create Subsections for all relevant data sets
freezing_raw_d_age$masq_lpa_total<- rowSums(freezing_raw_d_age[, c(89,91,92,95,98,99,101,102,104,108,113,118,121,124)])
freezing_raw_d_age$masq_dm_total<- rowSums(freezing_raw_d_age[,c(94,97,100,103,105,110,115,127)])


##Low Positive Affect

## Plot Low Positive Affect Scores by Duplicated vs Distinct
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=masq_lpa_total, color = duplicated)) +
  geom_point(shape=10) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

## Plot Low Positive Affect scores by Age
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=masq_lpa_total, color=age_new)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

## Plot Low Positive Affect scores by Age Trend
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=masq_lpa_total, color=age_trend)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)


## Depressive Mood

## Plot Depressive Mood Scores by Duplicated vs Distinct
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=masq_dm_total, color = duplicated)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

## Plot Depressive Mood scores by Age
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=masq_dm_total, color=age_new)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

## Plot Depressive Mood scores by Age Trend
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=masq_dm_total, color=age_trend)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

###########
# MASQ AA #
###########

## Plot Arousal by Duplicated vs Distinct
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=masq_aa_total.x, color = duplicated)) +
  geom_point(shape=10) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)


## Plot Arousal scores by Age
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=masq_aa_total.x, color=age_new)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

## Plot Arousal by Age Trend
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=masq_aa_total.x, color=age_trend)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

###########
#  PSWQ   #
###########

## Plot Worry Scores by Duplicated vs Distinct
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=pswq_total.x, color = duplicated)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

## Plot Worry scores by Age
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=pswq_total.x, color=age_new)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm) +
  labs(x= "date of completion", y= "pswq total") +
  labs(color= "Age")

## Plot Worry scores by Age Trend
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=pswq_total.x, color=age_trend)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm) +
  labs(x= "date of completion", y= "pswq total") +
  labs(color= "Age")

###########
#   ASI   #
###########

## Plot Anxiety Sensitivity Scores by Duplicated vs Distinct
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=asi_total, color = duplicated)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

## Plot Anxiety Sensitivity scores by Age
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=asi_total.x, color=age_new)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm) +
  labs(x= "date of completion", y= "pswq total") +
  labs(color= "Age")

## Plot Anxiety Sensitivity scores by Age Trend
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=asi_total.x, color=age_trend)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

###########
#  BRIEF  #
###########

## First, create subtotals for various sections of the brief (working memory, inhibit, shift, emotional control)
freezing_raw_d_age$brief_wm_total<- rowSums(freezing_raw_d_age[, c(326,333,339,348,357,368,378,390)])
freezing_raw_d_age$brief_inh_total<- rowSums(freezing_raw_d_age[,c(327,338,351,358,365,377,380,395)])
freezing_raw_d_age$brief_shft_total<- rowSums(freezing_raw_d_age[,c(330,344,354,366,383,389)])
freezing_raw_d_age$brief_emctrl_total<- rowSums(freezing_raw_d_age[,c(323,334,341,350,355,364,373,379,391,394)])


## Working Memory

## Plot Working Memory scores by Duplicated
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=brief_wm_total, color=duplicated)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)


## Plot Working Memory scores by Age
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=brief_wm_total, color=age_new)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)


## Plot Working Memory scores by Age Trend
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=brief_wm_total, color=age_trend)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)


## Inhibit

## Plot Inhibit scores by Duplicated
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=brief_inh_total, color=duplicated)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

ggplot(freezing_raw_d_age, aes(x=brief_inh_total, color=duplicated)) + 
  geom_histogram(binwidth=5, fill="white", position = "dodge")

## Plot Inhibit scores by Age
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=brief_inh_total, color=age_new)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

ggplot(freezing_raw_d_age, aes(x=brief_inh_total, color=age_new)) + 
  geom_histogram(binwidth=5, fill="white", position = "dodge")

## Plot Inhibit scores by Age Trend
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=brief_inh_total, color=age_trend)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

ggplot(freezing_raw_d_age, aes(x=brief_inh_total, color=age_trend)) + 
  geom_histogram(binwidth=5, fill="white", position = "dodge")

## Shift

## Plot Shift scores by Duplicated
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=brief_shft_total, color=duplicated)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

ggplot(freezing_raw_d_age, aes(x=brief_shft_total, color=duplicated)) + 
  geom_histogram(binwidth=5, fill="white", position = "dodge")

## Plot Shift scores by Age
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=brief_shft_total, color=age_new)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

ggplot(freezing_raw_d_age, aes(x=brief_shft_total, color=age_new)) + 
  geom_histogram(binwidth=5, fill="white", position = "dodge")

## Plot Shift scores by Age Trend
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=brief_shft_total, color=age_trend)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

ggplot(freezing_raw_d_age, aes(x=brief_shft_total, color=age_trend)) + 
  geom_histogram(binwidth=5, fill="white", position = "dodge")


## Emotional Control

## Plot Shift scores by Duplicated
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=brief_emctrl_total, color=duplicated)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

ggplot(freezing_raw_d_age, aes(x=brief_emctrl_total, color=duplicated)) + 
  geom_histogram(binwidth=5, fill="white", position = "dodge")

## Plot Shift scores by Age
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=brief_emctrl_total, color=age_new)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

ggplot(freezing_raw_d_age, aes(x=brief_emctrl_total, color=age_new)) + 
  geom_histogram(binwidth=5, fill="white", position = "dodge")

## Plot Shift scores by Age Trend
ggplot(freezing_raw_d_age, aes(x=con_date.x, y=brief_emctrl_total, color=age_trend)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

ggplot(freezing_raw_d_age, aes(x=brief_shft_total, color=age_trend)) + 
  geom_histogram(binwidth=5, fill="white", position = "dodge")


##########
## LEC  ##
##########

ggplot(freezing_raw_d_age, aes(x=lec_1.x)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Natural Disaster")

ggplot(freezing_raw_d_age, aes(x=lec_2.x)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Fire or Explosion")

ggplot(freezing_raw_d_age, aes(x=lec_3.x)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Transportation Accident")

ggplot(freezing_raw_d_age, aes(x=lec_4.x)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Serious Accident at Work, Home or Rec Activity")

ggplot(freezing_raw_d_age, aes(x=lec_5.x)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Exposure to Toxic Substance")

ggplot(freezing_raw_d_age, aes(x=lec_6.x)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Physical Assault")

ggplot(freezing_raw_d_age, aes(x=lec_7.x)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Assault with a Weapon")

ggplot(freezing_raw_d_age, aes(x=lec_8.x)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Sexual Assault")

ggplot(freezing_raw_d_age, aes(x=lec_9.x)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Other Unwanted or Uncomfotable Sexual Exp")

ggplot(freezing_raw_d_age, aes(x=lec_10.x)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Combat or Exposure to War")

ggplot(freezing_raw_d_age, aes(x=lec_11.x)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Captivity")

ggplot(freezing_raw_d_age, aes(x=lec_12.x)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Life-threatening illness or injury")

ggplot(freezing_raw_d_age, aes(x=lec_13.x)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Severe Human Suffering")

ggplot(freezing_raw_d_age, aes(x=lec_14.x)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Sudden Violent Death")

ggplot(freezing_raw_d_age, aes(x=lec_15.x)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Sudden Accidental Death")

ggplot(freezing_raw_d_age, aes(x=lec_16.x)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Causing Serious Injury or Harm")

ggplot(freezing_raw_d_age, aes(x=lec_17.x)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Any Other")

###########
#   ATQ   #
###########

freezing_raw_d_age$atq_appr<-rowSums(freezing_raw_d_age[, c(311,313,314,317,319,320)])
freezing_raw_d_age$atq_avoi<-rowSums(freezing_raw_d_age[, c(310,312,315,316,318,321)])
  
 
## Calculate Means for each group

atq_avoi_mu <- ddply(freezing_raw_d_age, "duplicated", summarise, grp.mean=mean(atq_avoi)) 
atq_appr_mu <- ddply(freezing_raw_d_age, "duplicated", summarise, grp.mean=mean(atq_appr)) 

##Visualize repeat vs distinct participant avoidance

ggplot(freezing_raw_d_age, aes(x=atq_avoi, color=duplicated)) + 
  geom_histogram(binwidth=1, fill="white") +
  geom_vline(data=atq_avoi_mu, aes(xintercept=grp.mean, color=duplicated), linetype="dashed") +
  labs(x= "Avoidance Total")

##Visualize repeat vs distinct participant approach 
ggplot(freezing_raw_d_age, aes(x=atq_appr, color=duplicated)) + 
  geom_histogram(binwidth=1, fill="white")+
  geom_vline(data=atq_appr_mu, aes(xintercept=grp.mean, color=duplicated), linetype="dashed") +
  labs(x= "Approach Total")


#############################################
##  Analyze Depression & Anxiety Scores    ##
#############################################

## Since the plots show various trends over time, let's split up the data by date of completion in relation to time (pre vs post spring break)


## Create pre-spring break & post-spring break groups
pre_sb<-freezing_raw_d_age %>%
  dplyr::filter(freezing_raw_d_age$record_id < 540)

post_sb<-freezing_raw_d_age %>%
  dplyr::filter(freezing_raw_d_age$record_id >= 540)


## Now, let's look at these scores. 


###########
#  PSWQ   #
###########

## First test comparison of all participants separated by pre vs post spring break

t.test(pre_sb$pswq_total.x, post_sb$pswq_total.x, alternative = "two.sided", var.equal = FALSE)
## No significant difference for general sample


## T test Worry scores by age trend and calendar date

t.test(pswq_total.x ~ age_trend, data = pre_sb)
t.test(pswq_total.x ~ age_trend, data = post_sb)

## No significant difference before spring break
## Significant difference AFTER spring break! Tell this story!!

## Let's run a two-way ANOVA for unbalanced design
library(car)
pswq_anova <- aov(pswq_total.x ~ timepoint * age_trend, data = freezing_raw_d_age)
Anova(pswq_anova, type = "III")

install.packages("pander")
library(pander)

fit.lm2<- aov(pswq_total.x ~ timepoint * age_trend, data = freezing_raw_d_age) 
thsd<-TukeyHSD(fit.lm2)
pander(thsd$`timepoint:age_trend`)

## T test Worry scores by duplicated and calendar date

t.test(pswq_total.x ~ duplicated, data = pre_sb)
t.test(pswq_total.x ~ duplicated, data = post_sb)

## No Significance


############
#  MASQ AA #
############

## First test comparison of all participants separated by pre vs post spring break

t.test(pre_sb$masq_aa_total, post_sb$masq_aa_total, alternative = "two.sided", var.equal = FALSE)
## No significant difference for general sample

## T test Arousal scores by age trend and calendar date

t.test(masq_aa_total.x ~ age_trend, data = pre_sb)
t.test(masq_aa_total.x ~ age_trend, data = post_sb)

## No significance

## T test Arousal scores by duplicated and calendar date

t.test(masq_aa_total.x ~ duplicated, data = pre_sb)
t.test(masq_aa_total.x ~ duplicated, data = post_sb)

## No Significance


############
#  MASQ AD #
############

## First test comparison of all participants separated by pre vs post spring break

t.test(pre_sb$masq_ad_total, post_sb$masq_ad_total, alternative = "two.sided", var.equal = FALSE)
t.test(pre_sb$masq_lpa_total, post_sb$masq_lpa_total, alternative = "two.sided", var.equal = FALSE)
t.test(pre_sb$masq_dm_total, post_sb$masq_dm_total, alternative = "two.sided", var.equal = FALSE)
## ALL SIGNIFICANT!! Tell this story!

## Test Depression scores by age trend and calendar date

t.test(masq_ad_total.x ~ age_trend, data = pre_sb)
t.test(masq_ad_total.x ~ age_trend, data = post_sb)

t.test(masq_lpa_total ~ age_trend, data = pre_sb)
t.test(masq_lpa_total ~ age_trend, data = post_sb)

t.test(masq_dm_total ~ age_trend, data = pre_sb)
t.test(masq_dm_total ~ age_trend, data = post_sb)

############
#  RRQ     #
############

## First test comparison of all participants separated by pre vs post spring break

t.test(pre_sb$rrq_total, post_sb$rrq_total, alternative = "two.sided", var.equal = FALSE)
## No Significance

## T test RRQ scores by age trend and calendar date

t.test(rrq_total.x ~ age_trend, data = pre_sb)
t.test(rrq_total.x ~ age_trend, data = post_sb)
## No Significance

###############
#  SIAS (ST)  #
###############

# Let's assess the stereotype threat measure used in our data

#create subscale total scores; 1-strongly agree:7-strongly disagree, low scores = high susceptibility, average item score of 2.5 or less denotes high susceptibility

#gender identification
freezing_raw_d_age$sias_gi_total<-rowSums(freezing_raw_d_age[,c(149,152,158,164)])
psych::describe(freezing_raw_d_age$sias_gi_total)

#math identification
freezing_raw_d_age$sias_mi_total<-rowSums(freezing_raw_d_age[,c(150,159,174,177,180,183)])
psych::describe(freezing_raw_d_age$sias_mi_total)

#ethnicity identification
freezing_raw_d_age$sias_ei_total<-rowSums(freezing_raw_d_age[,c(155,165,170,171)])
psych::describe(freezing_raw_d_age$sias_ei_total)

#gender stigma consciousness
freezing_raw_d_age$sias_gsc_total<-rowSums(freezing_raw_d_age[,c(153,161,166,172,178)])
psych::describe(freezing_raw_d_age$sias_gsc_total)

#ethnicity stigma consciousness
freezing_raw_d_age$sias_esc_total<-rowSums(freezing_raw_d_age[,c(157,175,179,181,184)])
psych::describe(freezing_raw_d_age$sias_esc_total)

#Negative Affect
freezing_raw_d_age$sias_na_total<-rowSums(freezing_raw_d_age[,c(185,187,188,189,190,191)])
psych::describe(freezing_raw_d_age$sias_na_total)

#Check Correlation with other SIAS subscales
pairs.panels(freezing_raw_d_age[,c(400:405)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

##############################
## Compare Within Subjects  ##
##############################


t.test(T1_dup$asi_total, T2_dup$asi_total, alternative = "less", paired=TRUE, var.equal = FALSE)
## Not Significant

t.test(T1_dup$masq_aa_total, T2_dup$masq_aa_total, alternative = "less", paired=TRUE, var.equal = FALSE)
## Not Significant

t.test(T1_dup$masq_ad_total, T2_dup$masq_ad_total, alternative = "less", paired=TRUE, var.equal = FALSE)
## Significant

## Quick detour to calculate the totals of the MASQ aa subsections in these datasets
T1_dup$masq_lpa_total<- rowSums(T1_dup[, c(87,89,90,93,96,97,99,100,102,106,111,116,119,122)])
T1_dup$masq_dm_total<- rowSums(T1_dup[,c(92,95,98,101,103,108,113,125)])
T2_dup$masq_lpa_total<- rowSums(T2_dup[, c(87,89,90,93,96,97,99,100,102,106,111,116,119,122)])
T2_dup$masq_dm_total<- rowSums(T2_dup[,c(92,95,98,101,103,108,113,125)])

## Okay...back at it
t.test(T1_dup$masq_lpa_total, T2_dup$masq_lpa_total, alternative = "less", paired=TRUE, var.equal = FALSE)
# Trending towards significant 

t.test(T1_dup$masq_dm_total, T2_dup$masq_dm_total, alternative = "less", paired=TRUE, var.equal = FALSE)
# Trending towards significant 

t.test(T1_dup$pswq_total, T2_dup$pswq_total, alternative = "less", paired=TRUE, var.equal = FALSE)
#Not Significant


## To supplement these findings, let's examine executive functions scores


## Score the various subsection of the brief (relevant here: working memory, inhibit, shift, emotional control)

T1_dup$brief_wm_total<- rowSums(T1_dup[, c(324,331,337,346,355,366,376,388)])
T1_dup$brief_inh_total<- rowSums(T1_dup[,c(325,336,349,356,363,375,378,393)])
T1_dup$brief_shft_total<- rowSums(T1_dup[,c(328,342,352,364,381,387)])
T1_dup$brief_emctrl_total<- rowSums(T1_dup[,c(321,332,339,348,353,362,371,377,389,392)])

## Now, T2
T2_dup$brief_wm_total<- rowSums(T2_dup[, c(324,331,337,346,355,366,376,388)])
T2_dup$brief_inh_total<- rowSums(T2_dup[,c(325,336,349,356,363,375,378,393)])
T2_dup$brief_shft_total<- rowSums(T2_dup[,c(328,342,352,364,381,387)])
T2_dup$brief_emctrl_total<- rowSums(T2_dup[,c(321,332,339,348,353,362,371,377,389,392)])

## Let's test
t.test(T1_dup$brief_wm_total, T2_dup$brief_wm_total, alternative = "less", paired=TRUE, var.equal = FALSE)
#not significant

t.test(T1_dup$brief_inh_total, T2_dup$brief_inh_total, alternative = "less", paired=TRUE, var.equal = FALSE)
#not significant

t.test(T1_dup$brief_shft_total, T2_dup$brief_shft_total, alternative = "less", paired=TRUE, var.equal = FALSE)
#not significant

t.test(T1_dup$brief_emctrl_total, T2_dup$brief_emctrl_total, alternative = "less", paired=TRUE, var.equal = FALSE)
#not significant






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

## Visualize Endorsement Section
ggplot(afq_endorsement, aes(x=afq_1)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Physical Assault")



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
#....The list goes on, one could visualize all items if necessary or desired

###########################################################################################
## Visualize Correlations between AFQ items & MASQ/PSWQ- each item to total factor score ##
###########################################################################################

## Add AFQ Total, PSWQ Total and MASQ Totals to the AFQ dataset

## Extract Freezing Item Reponses ##
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


afq_cor_table<-as.data.frame(afq_cor_table)

write_tsv(afq_cor_table, "AFQ_Correlations")


####################################################
#Make some plots of the distirbution of correlations 
####################################################

#Make a version of the data that works nicer with GG plot (wide vs. long)
afq_cor_table_wide<- melt(afq_cor_table)

#make the plots with ggplot
ggplot(afq_cor_table, aes(x=afq_cog_total)) + geom_histogram(color="black", fill="white") + geom_density(alpha=.4, fill="#FF6666")
  
ggplot(afq_cor_table, aes(x=afq_phys_total)) + geom_histogram(color="black", fill="white") + geom_density(alpha=.4, fill="cyan4")

ggplot(afq_cor_table, aes(x=afq_soc_total)) + geom_histogram(color="black", fill="white") + geom_density(alpha=.5, fill="darkgoldenrod2")

ggplot(afq_cor_table_wide,aes(x=value, fill=variable)) + geom_density(alpha=0.25)
ggplot(afq_cor_table_wide,aes(x=value, fill=variable)) + geom_histogram(alpha=0.7, bins=60)
ggplot(afq_cor_table_wide,aes(x=variable, y=value, fill=variable)) + geom_boxplot()


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
  dplyr::rename(pure_cor = pure_total)
afqpure_cor_table<-afqpure_cor_table %>%
  dplyr::rename(afq_total_cor = afq_total)
afqpure_cor_table<-afqpure_cor_table %>%
  dplyr::rename(pswq_cor = pswq_total)
afqpure_cor_table<-afqpure_cor_table %>%
  dplyr::rename(masq_aa_cor = masq_aa_total)

## View Histograms of Correlations
ggplot(afqpure_cor_table, aes(x=pure_cor)) + geom_histogram(color="darkblue", fill="lightblue")
ggplot(afqpure_cor_table, aes(x=afq_total_cor)) + geom_histogram(color="darkblue", fill="lightblue")
ggplot(afqpure_cor_table, aes(x=pswq_cor)) + geom_histogram(color="darkblue", fill="lightblue")
ggplot(afqpure_cor_table, aes(x=masq_aa_cor)) + geom_histogram(color="darkblue", fill="lightblue")


## Visualize Correlations between Pure_total and other totals in AFQ df
pairs.panels(afq[,c(76,75,74,73)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)


## Check reliability of Pure Items
## reminder we made this df: afq.pure.items<-afq[,c(1,3,7,8,9,11,12,14,15,18,21,24,29,30,37,38,39,41,42,44,50,52,53,57,58,59,63:69)]
alpha(afq.pure.items)

x.x<-afqpure_cor_table$pswq_cor
y.y<-afqpure_cor_table$masq_aa_cor
z.z<-afqpure_cor_table$pure_cor

scatter3d(x = afqpure_cor_table$pswq_cor, y = afqpure_cor_table$masq_aa_cor, z = afqpure_cor_table$pure_cor, surface=FALSE, xlab = "PSWQ", ylab = "MASQ AA",
          zlab = "AFQ Pure Total", labels = TRUE)

scatter3D(x.x, y.y, z.z, phi = 0, bty = "g",  type = "h", 
          ticktype = "detailed", pch = 19, cex = 0.5)

par(mar=c(1,1,1,1))

###top 16 associated to the afq total
#8,9,12,14,18,21,24,42,43,45,53,54,64,65,66,69

### Top 16 pure items, highest correlation with Pure Total

# top 16 items in column form: afq[,c(8,9,18,21,30,41,44,52,53,63:69)]
# top 16 items by item name: 8,9,18,21,31,42,45,53,54,64:70

### 8:I did not feel present in the moment
### 9:I felt detached
### 18: My mind felt stuck
### 21: I wanted to run away, but could not

### 31: My stomach sank
### 42: I felt petrified
### 45: I could not take a deep breath
### 53: I was shaking
### 54: I felt keyed up or on edge

### 64: I felt humiliated
### 65: I felt like there was a spotlight on me during what happened
### 66: I felt like there was a spotlight on me after what happened
### 67: I felt like I was the center of attention
### 68: I felt like people were watching my every move
### 69: I felt like people could see right through me
### 70: I felt pressure from others about how I should respond

## Make Pure Total
afq$pure16_total<-rowSums(afq[,c(8,9,18,21,30,41,44,52,53,63:69)])


## Generate list of pearson coefficients between items and totals ##
## make correlation matrices objects
afq_8_cor<-as.data.frame(cor(afq[, c("afqs_8", "pure16_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_9_cor<-as.data.frame(cor(afq[, c("afqs_9", "pure16_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_18_cor<-as.data.frame(cor(afq[, c("afqs_18", "pure16_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_21_cor<-as.data.frame(cor(afq[, c("afqs_21", "pure16_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_30_cor<-as.data.frame(cor(afq[, c("afqs_31", "pure16_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_41_cor<-as.data.frame(cor(afq[, c("afqs_42", "pure16_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_44_cor<-as.data.frame(cor(afq[, c("afqs_45", "pure16_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_52_cor<-as.data.frame(cor(afq[, c("afqs_53", "pure16_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_53_cor<-as.data.frame(cor(afq[, c("afqs_54", "pure16_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_63_cor<-as.data.frame(cor(afq[, c("afqs_64", "pure16_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_64_cor<-as.data.frame(cor(afq[, c("afqs_65", "pure16_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_65_cor<-as.data.frame(cor(afq[, c("afqs_66", "pure16_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_66_cor<-as.data.frame(cor(afq[, c("afqs_67", "pure16_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_67_cor<-as.data.frame(cor(afq[, c("afqs_68", "pure16_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_68_cor<-as.data.frame(cor(afq[, c("afqs_69", "pure16_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_69_cor<-as.data.frame(cor(afq[, c("afqs_70", "pure16_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))

## Remove Item Colum
afq_8_cor<-afq_8_cor[,-1]
afq_9_cor<-afq_9_cor[,-1]
afq_18_cor<-afq_18_cor[,-1]
afq_21_cor<-afq_21_cor[,-1]
afq_30_cor<-afq_30_cor[,-1]
afq_41_cor<-afq_41_cor[,-1]
afq_44_cor<-afq_44_cor[,-1]
afq_52_cor<-afq_52_cor[,-1]
afq_53_cor<-afq_53_cor[,-1]
afq_63_cor<-afq_63_cor[,-1]
afq_64_cor<-afq_64_cor[,-1]
afq_65_cor<-afq_65_cor[,-1]
afq_66_cor<-afq_66_cor[,-1]
afq_67_cor<-afq_67_cor[,-1]
afq_68_cor<-afq_68_cor[,-1]
afq_69_cor<-afq_69_cor[,-1]



# Make new correlation table for visualization
pure16_cor_table<- dplyr::bind_rows(afq_8_cor,afq_9_cor,afq_18_cor,afq_21_cor,
                                    afq_30_cor,afq_41_cor,afq_44_cor,afq_52_cor,afq_53_cor,
                                    afq_63_cor,afq_64_cor,afq_65_cor,afq_66_cor,afq_67_cor,afq_68_cor,afq_69_cor)

ind <- seq(1, nrow(pure16_cor_table), by=5)
pure16_cor_table<-pure16_cor_table[ind, ]

## Rename Columns

pure16_cor_table<-pure16_cor_table %>%
  dplyr::rename(pure16_cor = pure16_total)
pure16_cor_table<-pure16_cor_table %>%
  dplyr::rename(afq_total_cor = afq_total)
pure16_cor_table<-pure16_cor_table %>%
  dplyr::rename(pswq_cor = pswq_total)
pure16_cor_table<-pure16_cor_table %>%
  dplyr::rename(masq_aa_cor = masq_aa_total)


##make a dataset of the AFQ Pure16
pure16_items<-afq%>%
  dplyr::select(8,9,18,21,30,41,44,52,53,63:69)
pure16_items$pure16_total<-rowSums(pure16_items)
pure16_items$purecog_total<-rowSums(pure16_items[,1:4])
pure16_items$purephys_total<-rowSums(pure16_items[,5:9])
pure16_items$puresoc_total<-rowSums(pure16_items[,10:16])

pure16_items$pswq_total<-afq$pswq_total
pure16_items$masq_aa_total<-afq$masq_aa_total
pure16_items$afq_total<-afq$afq_total

#Visualize
pairs.panels(pure16_items[,17:23], 
             method = "pearson",
             hist.col = "#00AFBB",
             density = TRUE,
             ellipses = TRUE)

#rename rows for easy visualization
library(data.table)
pure16_cor_table<-(setattr(pure16_cor_table,"row.names",c("8","9","18","21","31","42","45","53","54","64","65","66","67","68","69","70")))

#Plot
install.packages("scatterplot3d")
library(scatterplot3d)

#reorder columns for plotting pure, pswq, & masq
pure16_cor_table<-pure16_cor_table[,c(1,3,2,4)]

x <- pure16_cor_table$pure16_cor
y <- pure16_cor_table$masq_aa_cor
z <- pure16_cor_table$pswq_cor

#add grid lines

source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
pure_s3d <- scatterplot3d(pure16_cor_table[,1:3], main="Pure 16 Correlation", xlab= "Pure16 Cor", ylab = "MASQ AA Cor", zlab = "PSWQ Cor",pch = "", grid=FALSE, box = FALSE)
addgrids3d(pure16_cor_table[,1:3], grid = c("xy","xz","yz"))
pure_s3d$points3d(pure16_cor_table[,1:3], pch=" ", type="h")
text(pure_s3d$xyz.convert(pure16_cor_table[, 1:3]), labels = rownames(pure16_cor_table),
     cex= 1.5, col = "red")

#### Now... let's do this for the lowest correlated to PSWQ & MASQ AA

## MASQ
### Top 16 low MASQ items, lowest correlation with MASQ Total of pure items

# top 16 items in column form: afq[,c(1,8,12,14,15,30,37,38,39,53,57,58,59,64,68,69)]
# top 16 items by item name: 1,8,12,14,15,31,38,39,40,54,58,59,60,65,69,70

### 1: My mind went blank
### 8: I did not feel present in the moment
### 12: I could not decide what to do
### 14: I felt I was outside my own body
### 15: I was stuck focusing on one thing

### 31: My stomach sank
### 38: My heart pounded
### 39: I began to sweat
### 40: I felt nauseous
### 54: I felt keyed up or on edge

### 58: I was embarrassed
### 59: I was afraid of being judged
### 60: I felt as though there were consequences to failing
### 65: I felt like there was a spotlight on me during what happened
### 69: I felt like people could see right through me
### 70: I felt pressure from others about how I should respond

# Make new correlation table for visualization
lowMASQ_cor<- dplyr::bind_rows(afq_1_cor,afq_8_cor,afq_12_cor,afq_14_cor,afq_15_cor,
                                    afq_30_cor,afq_37_cor,afq_38_cor,afq_39_cor,afq_53_cor,afq_57_cor,afq_58_cor,afq_59_cor,
                                    afq_64_cor,afq_68_cor,afq_69_cor)

ind <- seq(1, nrow(lowMASQ_cor), by=5)
lowMASQ_cor<-lowMASQ_cor[ind, ]

#rename rows for easy visualization
library(data.table)
lowMASQ_cor<-(setattr(lowMASQ_cor,"row.names",c("1","8","12","14","15","31","38","39","40","54","58","59","60","65","69","70")))

#Plot
install.packages("scatterplot3d")
library(scatterplot3d)
install.packages("addgrids3d")
library(addgrids3d)

#reorder columns for plotting
lowMASQ_cor<-lowMASQ_cor[,c(1,3,4,2)]

x <- lowMASQ_cor$pure_total
y <- lowMASQ_cor$masq_aa_total
z <- lowMASQ_cor$pswq_total

#add grid lines

source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
MASQ_s3d <- scatterplot3d(lowMASQ_cor[,1:3], pch = " ", grid=FALSE, box = FALSE)
addgrids3d(lowMASQ_cor[,1:3], grid = c("xy","xz","yz"))
MASQ_s3d$points3d(lowMASQ_cor[,1:3], pch=" ", type="h")
text(MASQ_s3d$xyz.convert(lowMASQ_cor[, 1:3]), labels = rownames(lowMASQ_cor),
     cex= 1.5, col = "red")

## pswq
### Top 16 low PSWQ items, lowest correlation with PSWQ Total of pure items

# top 16 items in column form: afq[,c(3,7,14,15,21,24,29,38,39,42,50,52,53,57,66,68)]
# top 16 items by item name: 3,7,14,15,21,24,30,39,40,43,51,53,54,58,67,69

### 3: I felt like everything slowed down
### 7: I was too startled/shocked to take action
### 14: I felt I was outside my own body
### 15: I was stuck focusing on one thing
### 21: I wanted to run away, but could not
### 24: I was hyper-aware of being unable to think

### 30: My heart skipped a beat
### 39: I began to sweat
### 40: I felt nauseous
### 43: I felt like everything in my body slowed down
### 51: I was not able to speak/scream
### 53: I was shaking
### 54: I felt keyed up or on edge


### 58: I was embarrassed
### 67: I felt like I was the center of attention
### 69: I felt like people could see right through me


# Make new correlation table for visualization
lowpswq_cor<- dplyr::bind_rows(afq_3_cor,afq_7_cor,afq_14_cor,afq_15_cor,afq_21_cor,afq_24_cor,afq_29_cor,
                               afq_38_cor,afq_39_cor,afq_42_cor,afq_50_cor,afq_52_cor,afq_53_cor,afq_57_cor,
                               afq_66_cor,afq_68_cor)

ind <- seq(1, nrow(lowpswq_cor), by=5)
lowpswq_cor<-lowpswq_cor[ind, ]

#rename rows for easy visualization
library(data.table)
lowpswq_cor<-(setattr(lowpswq_cor,"row.names",c("3","7","14","15","21","24","30","39","40","43","51","53","54","58","67","69")))

#Plot
install.packages("scatterplot3d")
library(scatterplot3d)
install.packages("addgrids3d")
library(addgrids3d)

#reorder columns for plotting
lowpswq_cor<-lowpswq_cor[,c(1,3,4,2)]

x <- lowpswq_cor$pure_total
y <- lowpswq_cor$masq_aa_total
z <- lowpswq_cor$pswq_total

#add grid lines

source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
pswq_s3d <- scatterplot3d(lowpswq_cor[,1:3], pch = " ", grid=FALSE, box = FALSE)
addgrids3d(lowpswq_cor[,1:3], grid = c("xy","xz","yz"))
pswq_s3d$points3d(lowpswq_cor[,1:3], pch=" ", type="h")
text(pswq_s3d$xyz.convert(lowpswq_cor[, 1:3]), labels = rownames(lowpswq_cor),
     cex= 1.5, col = "red")

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





#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,...................,.,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,&
#@,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,..,.....................,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,&
#@,,,*,.,,,,,,,,,,,,,,.,,,,,,,,,,,,,,,,,....,.,,,,,...............,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,&
#@,,,****,,,,,,,,,,,,,,,,..,,,,,,,,,,,,,,,..,*/*(((##(,,,,,,......,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,&
#@,,,,,,,,,,,,,***,,,,,,,,,,,,,,,,*/((((((((//(#(((#//(#**,*.,..,.,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,&
#@,,,,,,,,,,,,,,***,,,**,,,,,,,,/(((%##%#%&%%%%%#%%%((((#/(/**,,.,,,,,,,,,,,,,,,,,,*************,,,,&
#@,,,,,,,,,,,,,,,,,,,,,*,,,,,,*((##(#(%#%&&#####%@#%##%#(#((((/(,,,,,,,,,,,,,*********//////********@
#@,,,,,,,,,,,,,,,,,,,,,,,,,,,((%#((%##(#%#%%##%##%%#(%####%##(#((,,,,,,,,***********/*///******/////@
#@,,,,,,,,,,,,,,,,,,,,,,,,,,(###(#%######%&%####&(##%##%(####((#%##,,,,,,**********////******//////*@
#@,,,,,,,,,,,,,,,,,,,,,,,,,((((##%##(#%##(###%%#%(%##%###(%%#%###%#(/,,,**********////***////////***&
#@,,,,,,,,,,,,,,,,,,,,,,,*###(#%#%#((#(##((%####%%#((#(/(((%##%##%%##*,,***********////////////*****&
#@,,,,,,,,,*,,,,,,,,,,,,*%##(%####(((((((####((#((/(/(/(((((%#%%#(#((/************///////////*******&
#@,,,,,,,,,,,,,,,,,,,,,,/#%%#####(((((/##((##((((/(//*/***//##%%%(##%#,***********////////**********&
#@,,,,,,,,,,,,,,,,,,,,,,*(%###%(#(/////////((/*******/*,,,,*/(##%%####*,**********//////************&
#@,,,,,,,,,,,,,,,,,,,,,,,,##%%(((///*/**********,,,,,,,,,,,,,*####%%###(**********//****************&
#@,,,,,,,,,,,,,,,,,,,,,,,,,(%&%////*************/(&((//##//(%/**(/((#(((*************************,*,&
#@,,,,,,,,,,,,,,,,,,,,,,,,,*%&&/*/(####(#((/(#(/(/#((#((/*/,,,,*/*/(*#((*************************,,,&
#@,,,,,,,,,,,,,,,,,,,,,,,,,(%%####//#%&%%(/**,,(#((///,***,,,****,/,/#((*,**************************@
#@,,,,,,,,,,,,,,,,,,,,,,,,**(#(/*/*////((((**,,,,/((///**,,,,,****,,((#*/*,***************////******@
#@,,,,,,,,,,,,,,,,,,,,,,,,,,,,/*/**///(////**,,,,,******,,,,,,,,**,(#(#(*,,,*********////*****//////@
#@,*,,,,,*,,,,,,,,,,,,,,,,,,,,,,#*******//***,,,,,,*////**,**,****(%%###*,,,,,**////******/*///***,,&
#@,************,,,,,,,,,,,,*,,,,//****//(/*///*/***,,,**//***,,***%%#(#(,,,*************///***,,,,,,&
#@,*************,,,,,,,,,,,,,,,*(****//(//*****/****///*//*,******#@@@@*,,,,,,,,,***//*****,,,,,,,,.&
#@,**********,***,,,,,,,,,,,*,,,*#&***////##(///****,,,***********@@@@&%(,,,,,,,********,,,,,,,,....&
#@**************,,,,,,,,,,,,,*,,,#&@/***//*//////***,,,*******/***&@@@&%%&%/,,,,,,,,,,,,,,,,,,,,,...&
#@*************,,,,,,,,,,,,,,,*,%&@@@&///////****,*,,,,***//******%@&&%%%%&%%&%/....,,,,,,,,,,,,,***&
#@*******************,,*********&&@@%@@@%(#(/////******////******%@&%##@@%%&@#@@@&..................&
#@,************************,,,,#&&&&@@@@@@%#((((#(((///////////#%&&%%&@&%%%%%%%@@@@@,...............&
#@********************,,*#%%%#%%&&&@@&@@@@@%###((##((///////(%%%&&%%@%&%%%%%%%@@@@@@##. ....  ...   &
#@***********/**,,,,#%##%%%%%%%&&&&@&&&&&@@@%##%####((///(###%%&%&&&%%#%#%%%@@@@@@&%%%%####, .......&
#@***////*,,,...,/##%#%%%%%&%&&%%%&&&&&%%%@@@&#((#####(######&&&%%%&%%%%%%%@@&@@@@%%%%#(((((##(*....&
#@**,,.......,,,(##%%%%%%&&%%&&&%&%&&&&&&%%%@@&#(((((#####(%&%%%%%%%%%%%%&@@@@&&&&%&&%%###(#((((#(/.&
#@......,,,....##%%%%%%%&&%%%&&&&&%&%&@@@@&#%&%%#((((////%&%%%%##%%%%&%%@@@@&&&&&&&&&%##########(##(@
#@.,..........#%%%%%%&%%&&&%&&&&&&&&&&%#%%%##%#%&%(((((&&%&%&%#%%%%%%%#%@@&&&&&%%%&&&%##########(#((@
#@ ..........##%%%#%&&%%&&&%%&@&&&&&@@&@&&@@&&&&%%%@%###%%%###%%%%%%%#%#&&@&&%&%%&&&&%########%#####@
#@ .........###%%%%%&&%%&&%&&@@@%%&&%&@@@@&@@@@@%#%#%#%%%#&%&%&&#%%%#%&&&&%%&%%%%&&&%%######%%######@
#@ ......../##%%%%#&&&%%&&&%&&&&&%%%&&&&&@@@@&%%%%%%%%%%%#%%&&&&%##%%#%&%%&%%%%%%&&%%######%%%######@
#@ .......,###%%&&#&&&%%&&%%%%&&&&&@&@&&@@@@@@@&&&%&%&&&&&&&%&&&%######%##%%###%%&%%%####%&%%#######@
#@       .#####%&&%&&&%%&&%%&&&@@@&%%&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@%%###%%####%%%%%%###%&&%########@
#@       /####%%&&%&&&%%%&%%&&&%&&&@@@&&%#%@&%##%%%%&%&&%&@@@&%@@%%###########%%%%%#####@&%%########@
#@       ###%%%%%&%&&&%%%%%@%%%%&#&@@@@@&@&##%#%#&%%%&&&@@@@&@@&%#######%#####%%%%%####@&&%%########@
#@      ####%%%%%%&&@&%%%%%@@#%%%%&%&@@@&%%%%%%%%%&&&&&&&%&@@@%%#######%######%%%%####&@&%%##%%#####@
#@     *####%%%%%%&&@&%%%%&@@@@&%%%&%&%%%&&&&%#%%%@@@%%@@@@@&%########%#######%%%####&@@&%%#%%%#####@
#@    .#####%%%%%%&&@&#%%#@@@@@@@@&%%&&&&&&%#%%%@&&&@@@@@@@%##########%#######%%####%@@&&%#&%%%#####@
#@    (####%%%%%%%&&@&#%%#@@@@@@@@@@&&&&&&&&&&&&@@@@@@@@@%%##########%##############@@@&%%#&%%%#####@
#@   .#####%%%%%%%%&@@#%%%@@@@@@@@@@@@@@@%&@@@@@@@@@@@@@%##########################%@@@&%%#&%%%#####@
#@   ##%#%%%%%%%%&%&@@#%%&@@@@@@@@@@@@@@@@@@@@@@@@@@@@&%###########################&@@&&&%#&%%%%####@
#@  *%%%#%%%%%%%&&%&@@%%%@@@@@@@@@@@@@@@@@@@@@@@@@@@@&%######%%%###################&@@&&&%#@&%%%####@
#@..#%%%%%%%%%%%%&%&@@%%%%##&@@@@@@@@@@@@@@@@@@@@@@@%%######%%%%%%##%##############%@&&&&%#&&&%%####@
#@ ,%%%%%%%%%&&&%%&%&@&%%%##%%#&@@@@@@@@@@@@@@@@@@@&%######%%%%%%%%%###############%&&&&&%##@&%%##%#@
#@##%%%%%%&%&&&&&&%%&@@%%%##%%##%&&@@@@@@@@@@@@@@@%%#######%%%%%%%%%%##############%&&&&&%#(@&%##%%%@
#@&&&@@@@&&&&&&&@@@&%@@#%%%##%##%&&&&@@@@@@@@@@@@%%#######%%%%%%%%%%###############%&&&&&%#(@&%%&&%%@
#@%&%%%%%&&&&&&&&@@@&&@#%%%#%%%#%&&&&@@@@@@@@@@@%%%#######%%%%%%%%%%################%&&&&%#(@%%&&&%%@
#@##%%&&&&&%%%&&&@@@@%@#%%%#%%%#%%&&@@@@@@@@@@@%%########%%%%%%%%%%#################%%&&%%##&@@&&&%%@
#@%&%%%%%&@@@@@@@&@@&&%%#%%##%%##%&&@@@@@@@@@&%%%#######%%%%%%%%%%#################%%%%%%%#&@@@&&&%&@
#@&&%%%%&&&&&&&%%&&&&@@&%%%%#%%%#%&&&@@@@@@@@%%%########%%%%%%%%%#################%%#%%%%##@@@@@&&&%@
#@&%%%%%%%%%%&&&&%#%&@@@%%%%#%%%#%%&&&&@@@@@@%#########%%%%%%%%%%################%%##%%###@@@@&%&&%%@
#@#&&&&&&&&&&&&&&%&@@@@@@&&&&&&&&&&&&@@@@@@@@@%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%&@@@@@@@@@@@@


#################################################################
###### K- Means Clustering for Determining Item quality #########
#################################################################

afq<-freezing_raw_d %>%
  dplyr::select(afqs_1:afq_soc_total)
afq$pswq_total<-freezing_raw_d$pswq_total
afq$masq_aa_total<-freezing_raw_d$masq_aa_total
afq$afq_total<-rowSums(afq[,c(70:72)])


afq_1<-as.data.frame(cor(afq[, c("afqs_1", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_2<-as.data.frame(cor(afq[, c("afqs_2", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_3<-as.data.frame(cor(afq[, c("afqs_3", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_4<-as.data.frame(cor(afq[, c("afqs_4", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_5<-as.data.frame(cor(afq[, c("afqs_5", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_6<-as.data.frame(cor(afq[, c("afqs_6", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_7<-as.data.frame(cor(afq[, c("afqs_7", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_8<-as.data.frame(cor(afq[, c("afqs_8", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_9<-as.data.frame(cor(afq[, c("afqs_9", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_10<-as.data.frame(cor(afq[, c("afqs_10", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_11<-as.data.frame(cor(afq[, c("afqs_11", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_12<-as.data.frame(cor(afq[, c("afqs_12", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_13<-as.data.frame(cor(afq[, c("afqs_13", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_14<-as.data.frame(cor(afq[, c("afqs_14", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_15<-as.data.frame(cor(afq[, c("afqs_15", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_16<-as.data.frame(cor(afq[, c("afqs_16", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_17<-as.data.frame(cor(afq[, c("afqs_17", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_18<-as.data.frame(cor(afq[, c("afqs_18", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_19<-as.data.frame(cor(afq[, c("afqs_19", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_20<-as.data.frame(cor(afq[, c("afqs_20", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_21<-as.data.frame(cor(afq[, c("afqs_21", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_22<-as.data.frame(cor(afq[, c("afqs_22", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_23<-as.data.frame(cor(afq[, c("afqs_23", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_24<-as.data.frame(cor(afq[, c("afqs_24", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_25<-as.data.frame(cor(afq[, c("afqs_25", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_26<-as.data.frame(cor(afq[, c("afqs_26", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_27<-as.data.frame(cor(afq[, c("afqs_27", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_28<-as.data.frame(cor(afq[, c("afqs_28", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_29<-as.data.frame(cor(afq[, c("afqs_30", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_30<-as.data.frame(cor(afq[, c("afqs_31", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_31<-as.data.frame(cor(afq[, c("afqs_32", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_32<-as.data.frame(cor(afq[, c("afqs_33", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_33<-as.data.frame(cor(afq[, c("afqs_34", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_34<-as.data.frame(cor(afq[, c("afqs_35", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_35<-as.data.frame(cor(afq[, c("afqs_36", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_36<-as.data.frame(cor(afq[, c("afqs_37", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_37<-as.data.frame(cor(afq[, c("afqs_38", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_38<-as.data.frame(cor(afq[, c("afqs_39", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_39<-as.data.frame(cor(afq[, c("afqs_40", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_40<-as.data.frame(cor(afq[, c("afqs_41", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_41<-as.data.frame(cor(afq[, c("afqs_42", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_42<-as.data.frame(cor(afq[, c("afqs_43", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_43<-as.data.frame(cor(afq[, c("afqs_44", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_44<-as.data.frame(cor(afq[, c("afqs_45", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_45<-as.data.frame(cor(afq[, c("afqs_46", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_46<-as.data.frame(cor(afq[, c("afqs_47", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_47<-as.data.frame(cor(afq[, c("afqs_48", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_48<-as.data.frame(cor(afq[, c("afqs_49", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_49<-as.data.frame(cor(afq[, c("afqs_50", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_50<-as.data.frame(cor(afq[, c("afqs_51", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_51<-as.data.frame(cor(afq[, c("afqs_52", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_52<-as.data.frame(cor(afq[, c("afqs_53", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_53<-as.data.frame(cor(afq[, c("afqs_54", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_54<-as.data.frame(cor(afq[, c("afqs_55", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_55<-as.data.frame(cor(afq[, c("afqs_56", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_56<-as.data.frame(cor(afq[, c("afqs_57", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_57<-as.data.frame(cor(afq[, c("afqs_58", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_58<-as.data.frame(cor(afq[, c("afqs_59", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_59<-as.data.frame(cor(afq[, c("afqs_60", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_60<-as.data.frame(cor(afq[, c("afqs_61", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_61<-as.data.frame(cor(afq[, c("afqs_62", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_62<-as.data.frame(cor(afq[, c("afqs_63","afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_63<-as.data.frame(cor(afq[, c("afqs_64", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_64<-as.data.frame(cor(afq[, c("afqs_65", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_65<-as.data.frame(cor(afq[, c("afqs_66", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_66<-as.data.frame(cor(afq[, c("afqs_67", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_67<-as.data.frame(cor(afq[, c("afqs_68", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_68<-as.data.frame(cor(afq[, c("afqs_69", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_69<-as.data.frame(cor(afq[, c("afqs_70", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))

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


afq_cor_table_kmc<- dplyr::bind_rows(afq_1,afq_2,afq_3,afq_4,afq_5,afq_6,afq_7,afq_8,afq_9,afq_10,afq_11,afq_12,afq_13,afq_14,afq_15,afq_16,afq_17,afq_18,afq_19,
                                 afq_20,afq_21,afq_22,afq_23,afq_24,afq_25,afq_26,afq_27,afq_28,afq_29,afq_30,afq_31,afq_32,afq_33,afq_34,afq_35,afq_36,afq_37,
                                 afq_38,afq_39,afq_40,afq_41,afq_42,afq_43,afq_44,afq_45,afq_46,afq_47,afq_48,afq_49,afq_50,afq_51,afq_52,afq_53,afq_54,afq_55,
                                 afq_56,afq_57,afq_58,afq_59,afq_60,afq_61,afq_62,afq_63,afq_64,afq_65,afq_66,afq_67,afq_68,afq_69)

afq_cor_table_kmc<-afq_cor_table_kmc[,2:4]

item<-c("afq_1","afq_2","afq_3","afq_4","afq_5","afq_6","afq_7","afq_8","afq_9","afq_10","afq_11","afq_12","afq_13","afq_14","afq_15","afq_16","afq_17","afq_18","afq_19","afq_20","afq_21","afq_22","afq_23","afq_24","afq_25","afq_26","afq_27","afq_28","afq_30","afq_31","afq_32","afq_33","afq_34","afq_35","afq_36","afq_37","afq_38","afq_39","afq_40","afq_41","afq_42","afq_43","afq_44","afq_45","afq_46","afq_47","afq_48","afq_49","afq_50","afq_51","afq_52","afq_53","afq_54","afq_55","afq_56","afq_57","afq_58","afq_59","afq_60","afq_61","afq_62","afq_63","afq_64","afq_65","afq_66","afq_67","afq_68","afq_69", "afq_70")

subscale<-c("cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive", "cognitive", "cognitive", "cognitive", 
            "physical", "physical", "physical", "physical", 
            "physical", "physical", "physical", "physical", 
            "physical", "physical", "physical", "physical", 
            "physical", "physical", "physical", "physical", 
            "physical", "physical", "physical", "physical", 
            "physical", "physical", "physical", "physical", 
            "physical", "physical", "physical", "physical", 
            "social", "social", "social", "social", "social", 
            "social", "social", "social", "social", "social", 
            "social", "social", "social")

afq_kmc<-data.frame(item, afq_cor_table_kmc, subscale)
colnames(afq_kmc)<-(c("item", "afq_total", "masq_aa_total", "pswq_total", "subscale"))

##Rename rows for easy visualization

afq_kmc<-rownames(afq_kmc)[rownames(afq_kmc) == "afqs1"] = "1"

#############################
##### Chopping block ########
#############################

#                       /\_[]_/\
#                      |] _||_ [|
#               ___     \/ || \/
#              /___\       ||
#             (|0 0|)      ||
#           __/{\U/}\_ ___/vvv
#          / \  {~}   / _|_P|
#         | /\  ~   /_/   []
#         |_| (____)        
#         \_]/______\        
#            _\_||_/_           
#           (_,_||_,_)

#### Items 25, 26, and 27 are very bad boys and are off the team 

afq_kmc <- afq_kmc[-c(25:27), ]

############### ################
#### Time to cluster, baby ####
############### ################
library(data.table)
(setattr(afq_kmc, "row.names", c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","28","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69", "70")))

install.packages("plot3D")
library("plot3D")
install.packages("scatterplot3d") # Install
library("scatterplot3d") # load
x <- afq_kmc$afq_total
y <- afq_kmc$masq_aa_total
z <- afq_kmc$pswq_total

#plot3D(afq_kmc[,1:3], col=as.numeric(afq_kmc$subscale))

scatter3D(x, y, z, bty = "g",
          pch = 20, cex = 2, xlab = "MASQ", ylab = "PSWQ", zlab = "AFQ", main = "3D Scatterplot of Item to Scale Correlations", col.var = afq_kmc$Subscale, col = c("#1B9E77", "#D95F02", "#7570B3"), theta = 15, phi = 20)

s3d <- scatterplot3d(afq_kmc[,2:4], pch = 16, color=colors)
legend("top", legend = levels(afq_kmc$subscale),
       col =  c("black", "red", "green"), pch = 16, 
       inset = 0, xpd = TRUE, horiz = TRUE)

#### Let's start clustering #####

###############################
#### What method is best? ####
##############################
intern <- clValid(afq_kmc[,2:4], nClust = 2:7, 
                  clMethods = c("hierarchical","kmeans","pam"), validation = "internal")
# Summary
summary(intern) %>% kable() %>% kable_styling()

kmeans(afq_kmc[,2:4], centers = 2, nstart = 30)
#################################################
###    Let's visualize 2-7 cluster solutions  ###
#################################################

library("stats")
install.packages("factoextra")
library("factoextra")
library("ggplot2")
library("cowplot")

kmean_calc <- function(df, ...){
  kmeans(df, scaled = ..., nstart = 30)
}
km2 <- kmean_calc(afq_kmc[,2:4], 2)
km3 <- kmean_calc(afq_kmc[,2:4], 3)
km4 <- kmeans(afq_kmc[,2:4], 4)
km5 <- kmeans(afq_kmc[,2:4], 5)
km6 <- kmeans(afq_kmc[,2:4], 6)
km7 <- kmeans(afq_kmc[,2:4], 7)

p1 <- fviz_cluster(km2, data = afq_kmc[,2:4], elipse.type = "convex",  pointsize = 1, labelsize = 9) + theme_minimal() + ggtitle("k = 2") 
p2 <- fviz_cluster(km3, data = afq_kmc[,2:4], elipse.type = "convex",  pointsize = 1, labelsize = 9) + theme_minimal() + ggtitle("k = 3")
p3 <- fviz_cluster(km4, data = afq_kmc[,2:4], elipse.type = "convex",  pointsize = 1, labelsize = 9) + theme_minimal() + ggtitle("k = 4")
p4 <- fviz_cluster(km5, data = afq_kmc[,2:4], elipse.type = "convex",  pointsize = 1, labelsize = 9) + theme_minimal() + ggtitle("k = 5")
p5 <- fviz_cluster(km6, data = afq_kmc[,2:4], elipse.type = "convex",  pointsize = 1, labelsize = 9) + theme_minimal() + ggtitle("k = 6")
p6 <- fviz_cluster(km7, data = afq_kmc[,2:4], elipse.type = "convex",  pointsize = 1, labelsize = 9) + theme_minimal() + ggtitle("k = 7")
plot_grid(p1, p2, p3, p4, p5, p6, labels = c("k2", "k3", "k4", "k5", "k6", "k7"))



############################################
##### How many clusters should we use? #####
############################################

#### NB Clust 30 indicies test ######
install.packages("NbClust")
library("NbClust")
res.nbclust <- NbClust(afq_kmc[,2:4], distance = "euclidean",
                       min.nc = 2, max.nc = 7, 
                       method = "complete", index ="all")
factoextra::fviz_nbclust(res.nbclust) + theme_minimal() + ggtitle("NbClust's optimal number of clusters")

###Looks like 2 clusters are best solution

cluster<-c(2,  2,  1,  1,  1,  2,  1,  2,  2,  2,  2,  2,  1,  1,  1,  2,  2,  2,  1,  1,  2,  2,  1,  1,  2,  1,  2,  1,  1,  1,  1,  1,  1,  2,  1,  2, 1,  2,  1,  2,  2,  2,  2,  1,  2,  1,  1,  2,  2,  1,  1,  1,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  1,  2,  2,  2)
afq_kmc<-data.frame(afq_kmc, cluster)
afq_kmc$cluster<-as.factor(afq_kmc$cluster)

scatter3d(x = afq_kmc$pswq_total, y = afq_kmc$masq_aa_total, z = afq_kmc$afq_total, groups = afq_kmc$cluster, surface=FALSE, xlab = "PSWQ", ylab = "MASQ AA",
          zlab = "AFQ Total", ellipsoid = TRUE)

text3D(x, y, z,  labels = rownames(afq_kmc),add = TRUE, colkey = FALSE, cex = 0.5)

################################################
#### Cluster for all items in the scale     #### 
################################################

## Run the analsysis 
kmeans(afq_kmc[,2:4], centers = 2, nstart = 30)
##Split between regular items and social items, lets look at how they operate within subscale

###################################################################
##### How many clusters should we use for cog/phys subscale? #####
###################################################################

#### NB Clust 30 indicies test ######
res.nbclust <- NbClust(afq_kmc[1:53,2:4], distance = "euclidean",
                       min.nc = 2, max.nc = 7, 
                       method = "complete", index ="all")
factoextra::fviz_nbclust(res.nbclust) + theme_minimal() + ggtitle("NbClust's optimal number of clusters")

###Looks like 2 or 4 clusters are best solution

##Generate two or four cluster solutions
kmeans(afq_kmc[1:53,2:4], centers = 2, nstart = 30)
kmeans(afq_kmc[1:53,2:4], centers = 4, nstart = 30)

##Generate plots of two and four cluster solutions
two_cluster<-c(2,  2,  1,  1,  1,  2,  1,  2,  2,  2,  2,  2,  1,  1,  1,  2,  2,  2,  1,  1,  2,  2,  1,  1,  2,  1,  2,  1,  1,  1,  1,  1,  1,  2,  1, 1,  1,  2,  1,  2,  2,  2,  2,  1,  2,  1,  1,  2,  2,  1,  1,  1,  2 )
four_cluster<-c(3,  3,  1,  4,  4,  2,  1,  2,  2,  2,  2,  3,  4,  1,  4,  3,  3,  2,  1,  1,  2,  2,  4,  1,  3,  1,  3,  1,  1,  1,  1,  1,  1,  3,  4, 3, 1,  2,  1,  2,  2,  2,  2,  4,  2,  4,  1,  2,  1,  1,  1,  1,  3)
afq_cp_kmc<-afq_kmc[1:53,]
afq_cp_kmc<-data.frame(afq_cp_kmc, two_cluster, four_cluster)
afq_cp_kmc$two_cluster<-as.factor(afq_cp_kmc$two_cluster)
afq_cp_kmc$four_cluster<-as.factor(afq_cp_kmc$four_cluster)

### make 3d plots of two and four cluster solutions 
scatter3d(x = afq_cp_kmc$pswq_total, y = afq_cp_kmc$masq_aa_total, z = afq_cp_kmc$afq_total, groups = afq_cp_kmc$two_cluster, surface=FALSE, ellipsoid = TRUE)
scatter3d(x = afq_cp_kmc$pswq_total, y = afq_cp_kmc$masq_aa_total, z = afq_cp_kmc$afq_total, groups = afq_cp_kmc$four_cluster, surface=FALSE, ellipsoid = TRUE)

par(mfrow=c(1,1))
scatter3D(afq_cp_kmc$pswq_total, afq_cp_kmc$masq_aa_total, afq_cp_kmc$afq_total, phi = 0, bty = "g", pch = 20, cex = 0.5, xlab = " Correlation to PSWQ", ylab = " Correlation to MASQ AA", zlab = " Correlation to AFQ")
# Add text
text3D(afq_cp_kmc$pswq_total, afq_cp_kmc$masq_aa_total, afq_cp_kmc$afq_total,  labels =afq_cp_kmc$item,
       add = TRUE, cex = 0.5)
library("scatterplot3d") # load
s3d_twoc<-scatterplot3d(afq_cp_kmc[,2:4], color = as.numeric(two_cluster), pch = 16)
text(s3d_twoc$xyz.convert(afq_cp_kmc[, 2:4]), labels = item,
     cex= 0.7)
s3d_fourc<-scatterplot3d(afq_cp_kmc[,2:4], color = as.numeric(four_cluster), pch = 16)
text(s3d_fourc$xyz.convert(afq_cp_kmc[, 2:4]), labels = item,
     cex= 0.7)

###################################################################
##### How many clusters should we use for social subscale? #####
###################################################################

afq_social<-afq_kmc%>%
  dplyr::filter(subscale == "social")

#### NB Clust 30 indicies test ######
res.nbclust <- NbClust(afq_social[,2:4], distance = "euclidean",
                       min.nc = 2, max.nc = 7, 
                       method = "complete", index ="all")
factoextra::fviz_nbclust(res.nbclust) + theme_minimal() + ggtitle("NbClust's optimal number of clusters")

###Looks like 3 clusters are best solution

##Generate three cluster solution for social
kmeans(afq_social[,2:4], centers = 3, nstart = 30)

##Generate plots of three cluster solution of social
social_cluster<-c(1, 1, 1, 2, 2, 1, 3, 3, 3, 3, 3, 3, 1)
afq_social<-data.frame(afq_social, social_cluster)
afq_social$social_cluster<-as.factor(afq_social$social_cluster)

### make 3d plots of three cluster solution of social
scatter3d(x = afq_social$pswq_total, y = afq_social$masq_aa_total, z = afq_social$afq_total, groups = afq_social$social_cluster, surface=TRUE, ellipsoid = TRUE)

####################################################################################
####################################################################################
####################################################################################

##split based off of two cluster solution
afq_kmc_blue_balls<-afq_kmc %>%
  dplyr::filter(cluster == 2)
afq_kmc_pink_balls<-afq_kmc %>%
  dplyr::filter(cluster == 1)

#How many clusters are best?
res.nbclust <- NbClust(afq_kmc_blue_balls[,2:4], distance = "euclidean",
                       min.nc = 2, max.nc = 7, 
                       method = "complete", index ="all")
factoextra::fviz_nbclust(res.nbclust) + theme_minimal() + ggtitle("NbClust's optimal number of clusters")

# k means the blue balls
kmeans(afq_kmc_blue_balls[,2:4], centers = 3, nstart = 30)
cluster_bb<-c(3, 1, 2, 2, 2, 1, 2, 3, 1, 1, 2, 2, 2, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 1, 3, 3, 3, 1, 1, 3, 2, 3, 2, 3, 2, 3)
)
afq_kmc_blue_balls<-data.frame(afq_kmc_blue_balls, cluster_bb)
afq_kmc_blue_balls$cluster_bb<-as.factor(afq_kmc_blue_balls$cluster_bb)

###make a plot
scatter3d(x = afq_kmc_blue_balls$pswq_total, y = afq_kmc_blue_balls$masq_aa_total, z = afq_kmc_blue_balls$afq_total, groups = afq_kmc_blue_balls$cluster_bb, surface=FALSE, xlab = "PSWQ", ylab = "MASQ AA",
          zlab = "AFQ Total", ellipsoid = TRUE)

##make the data driven version of the AFQ
afq_dd<-afq_cp_kmc%>%
  dplyr::filter(four_cluster == 2)
afq_social<-afq_social%>%
  dplyr::filter(social_cluster == 3)
afq_dd<-dplyr::bind_rows(afq_dd, afq_social)
afq_dd<-afq_dd[,1:5]




###Now let's compare the AFQ KMC to the MASQ, PSWQ, and AFQ total 
afq_dd_items<-afq

afq$DD_total<-rowSums(afq[,c(6, 8, 9, 10, 11, 18, 21, 22, 41, 43, 44, 45, 46, 48, 51, 63,64, 65, 66, 67, 68)])
afq$dd_cog<-rowSums(afq[,c(6, 8, 9, 10, 11, 18, 21, 22)])
afq$dd_phys<-rowSums(afq[,c(41, 43, 44, 45, 46, 48, 51)])
afq$dd_social<-rowSums(afq[,c(63,64, 65, 66, 67, 68)])

##make a dataset of the AFQ DD 
afq_dd_items<-afq%>%
  dplyr::select(6, 8, 9, 10, 11, 18, 21, 22, 41, 43, 44, 45, 46, 48, 51, 63, 64, 65, 66, 67, 68)

afq_dd_items$cogdd_total<-rowSums(afq_dd_items[,1:8])
afq_dd_items$physdd_total<-rowSums(afq_dd_items[,9:15])
afq_dd_items$socdd_total<-rowSums(afq_dd_items[,16:21])
afq_dd_items$DD_total<-rowSums(afq_dd_items)
afq_dd_items$afq_total<-afq$afq_total
afq_dd_items$pswq_total<-afq$pswq_total
afq_dd_items$masq_aa_total<-afq$masq_aa_total

#Do some visualization 
pairs.panels(afq_dd_items[,25:28], 
             method = "pearson",
             hist.col = "#00AFBB",
             density = TRUE,
             ellipses = TRUE)
pairs.panels(afq[,c(70:79)], 
             method = "pearson",
             hist.col = "#00AFBB",
             density = TRUE,
             ellipses = TRUE)

## Generate list of pearson coefficients between items and totals ##
## make correlation matrices objects
afq_6_cor<-as.data.frame(cor(afq[, c("afqs_6", "DD_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_8_cor<-as.data.frame(cor(afq[, c("afqs_8", "DD_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_9_cor<-as.data.frame(cor(afq[, c("afqs_9", "DD_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_10_cor<-as.data.frame(cor(afq[, c("afqs_10", "DD_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_11_cor<-as.data.frame(cor(afq[, c("afqs_11", "DD_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_18_cor<-as.data.frame(cor(afq[, c("afqs_18", "DD_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_21_cor<-as.data.frame(cor(afq[, c("afqs_21", "DD_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_22_cor<-as.data.frame(cor(afq[, c("afqs_22", "DD_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_42_cor<-as.data.frame(cor(afq[, c("afqs_42", "DD_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_44_cor<-as.data.frame(cor(afq[, c("afqs_44", "DD_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_45_cor<-as.data.frame(cor(afq[, c("afqs_45", "DD_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_46_cor<-as.data.frame(cor(afq[, c("afqs_46", "DD_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_47_cor<-as.data.frame(cor(afq[, c("afqs_47", "DD_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_49_cor<-as.data.frame(cor(afq[, c("afqs_49", "DD_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_52_cor<-as.data.frame(cor(afq[, c("afqs_52", "DD_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_64_cor<-as.data.frame(cor(afq[, c("afqs_64", "DD_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_65_cor<-as.data.frame(cor(afq[, c("afqs_65", "DD_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_66_cor<-as.data.frame(cor(afq[, c("afqs_66", "DD_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_67_cor<-as.data.frame(cor(afq[, c("afqs_67", "DD_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_68_cor<-as.data.frame(cor(afq[, c("afqs_68", "DD_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))
afq_69_cor<-as.data.frame(cor(afq[, c("afqs_69", "DD_total", "afq_total", "pswq_total", "masq_aa_total")], method = "pearson"))

## Remove Item Colum
afq_6_cor<-afq_6_cor[,-1]
afq_8_cor<-afq_8_cor[,-1]
afq_9_cor<-afq_9_cor[,-1]
afq_10_cor<-afq_10_cor[,-1]
afq_11_cor<-afq_11_cor[,-1]
afq_18_cor<-afq_18_cor[,-1]
afq_21_cor<-afq_21_cor[,-1]
afq_22_cor<-afq_22_cor[,-1]
afq_42_cor<-afq_42_cor[,-1]
afq_44_cor<-afq_44_cor[,-1]
afq_45_cor<-afq_45_cor[,-1]
afq_46_cor<-afq_46_cor[,-1]
afq_47_cor<-afq_47_cor[,-1]
afq_49_cor<-afq_49_cor[,-1]
afq_52_cor<-afq_52_cor[,-1]
afq_64_cor<-afq_64_cor[,-1]
afq_65_cor<-afq_65_cor[,-1]
afq_66_cor<-afq_66_cor[,-1]
afq_67_cor<-afq_67_cor[,-1]
afq_68_cor<-afq_68_cor[,-1]
afq_69_cor<-afq_69_cor[,-1]


## append all data frames into one

dd_cor_table<- dplyr::bind_rows(afq_6_cor,afq_8_cor,afq_9_cor,afq_10_cor,afq_11_cor,afq_18_cor,
                                     afq_21_cor,afq_22_cor,afq_42_cor,afq_44_cor,
                                     afq_45_cor,afq_46_cor,afq_47_cor,afq_49_cor,afq_52_cor,afq_64_cor,
                                     afq_65_cor,afq_66_cor,afq_67_cor,afq_68_cor,afq_69_cor)

## clear total score rows
ind <- seq(1, nrow(dd_cor_table), by=5)
dd_cor_table<-dd_cor_table[ind, ]

## Rename Columns

dd_cor_table<-dd_cor_table%>%
  dplyr::rename(dd_cor = DD_total)
dd_cor_table<-dd_cor_table %>%
  dplyr::rename(afq_cor = afq_total)
dd_cor_table<-dd_cor_table %>%
  dplyr::rename(pswq_cor = pswq_total)
dd_cor_table<-dd_cor_table %>%
  dplyr::rename(masq_aa_cor = masq_aa_total)



x <- dd_cor_table$dd_cor
y <- dd_cor_table$pswq_cor
z <- dd_cor_table$masq_aa_cor

#reorder columns
dd_cor_table<-dd_cor_table[,c(1,4,3,2)]

#rename rows for easy visualization
library(data.table)
dd_cor_table<-(setattr(dd_cor_table,"row.names",c("6", "8", "9", "10", "11", "18", "21", "22", "42", "44", "45", "46", "47", "49", "52", "64", "65","66", "67", "68", "69")))


#add grid lines & Plot
source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
dd_s3d<-scatterplot3d(dd_cor_table[,2:4], main="DD Item Correlations", xlab="DD correlation", ylab="PSWQ Cor", zlab="MASQ AA Cor",pch = "", grid=FALSE, box = FALSE)
addgrids3d(dd_cor_table[,2:4], grid = c("xy","xz","yz"))
dd_s3d$points3d(dd_cor_table[,2:4], pch=" ", type="h")
text(dd_s3d$xyz.convert(dd_cor_table[, 2:4]), labels = rownames(dd_cor_table),
     cex= 1.5, col = "red")


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
save(afq, file = "afq.RData")
save(afq_endorsement, file = "afq_endorsement.Rdata")
save(pre_sb, file = "pre_sb.RData")
save(post_sb, file = "post_sb.RData")
save(afqpure_cor_table, file = "afqpure_cor_table.RData")
save(afq_kmc, file = "afq_kmc.RData")
save(pure16_cor_table, file = "pure16_cor_table.RData")
save(dd_cor_table, file = "dd_cor_table.RData")
save(pure16_items, file="pure16_items.RData")
save(afq_dd_items, file="afq_dd_items.RData")