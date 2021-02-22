#######################################################
### Freezing Questionnaire and Spring Data Analysis ###
#######################################################

## ** NOTE IF YOU ARE NOT APPROVED UNDER THE IRB PROTOCOL 08297 FROM UNIVERSTIY OF ILLINOIS DO NOT USE THIS CODE ** ##

## SEE R CODE ' FREEZING EFA_OPEN.R ' ##

###############################
## install relevant packages ##
###############################
install.packages("tidyverse")
install.packages("PostHocTest")
install.packages("psych")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("GPArotation")
install.packages("qdap")
install.packages("nFactors")
install.packages("lavaan")
install.packages("ggthemes")
install.packages("ggpubr")
install.packages("janitor")
install.packages("lubridate")
install.packages("psy")
install.packages("rgl")
install.packages("pander")
install.packages("ggcorrplot")
install.packages("rockchalk")
install.packages("lmSupport")
install.packages("jmv")
install.packages("formattable")


library(tidyverse)
library(psych)
library(ggplot2)
library(corrplot)
library(GPArotation)
library(qdap)
library(nFactors)
library(lavaan)
library(ggthemes)
library(ggpubr)
library(janitor)
library(lubridate)
library(psy)
library(plyr)
library(dplyr)
library(tidyr)
library(car)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(datarium)
library(pander)
library(ggcorrplot)
library(rockchalk)
library(lmSupport)
library(jmv)
library(formattable)


##Import data **be sure to order by name before importing so the order of participants matches the Age file
freezing_raw<-read.csv(file.choose())

## Import Age of Participants **be sure to order by name before importing so the order of participants matches the full data file

Age_1053<-read.csv(file.choose())

## Import Demographics of Participants

Demographics<-read.csv(file.choose())

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
Demographics$first_name<-trimws(Demographics$first_name)
Demographics$last_name<-trimws(Demographics$last_name)

## Uniting first name and last name into one column ##
freezing_raw<-tidyr::unite(freezing_raw, name, con_fn:con_ln, sep = " ", remove = TRUE)
Age_1053<-tidyr::unite(Age_1053, name, first_name:last_name, sep = " ", remove = TRUE)
Demographics<-tidyr::unite(Demographics, name, first_name:last_name, sep = " ", remove = TRUE)


## Convert name column to character
freezing_raw$name <- as.character(freezing_raw$name)
Age_1053$name <- as.character(Age_1053$name)
Demographics$name <- as.character(Demographics$name)

### Make sure conversion worked - diagnostic steps ###
## examine the class of name column
typeof(freezing_raw$name)
typeof(Age_1053$name)
typeof(Demographics$name)

## examine the structure of name column
str(freezing_raw$name)


################################################################################
### Let's generate data sets for distinct participants                       ###
################################################################################


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

# Remove outliers by consent date
freezing_raw_d_age <- freezing_raw_d %>%
  dplyr::filter (con_date >= "2020-01-31") %>%
  dplyr::filter (con_date <= "2020-05-31")

# Remove age outliers
freezing_raw_d_age<-freezing_raw_d_age %>%
  dplyr::filter(freezing_raw_d_age$age < 24)

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


# Create Subsections for all relevant data sets
freezing_raw_d_age$masq_lpa_total<- rowSums(freezing_raw_d_age[, c("masq_01","masq_14","masq_18","masq_23","masq_27","masq_30","masq_35","masq_36","masq_40","masq_49",
                                                                   "masq_58","masq_72","masq_78","masq_86")])
freezing_raw_d_age$masq_dm_total<- rowSums(freezing_raw_d_age[,c("masq_44","masq_33", "masq_26", "masq_53","masq_66", "masq_21","masq_39","masq_89")])

freezing_raw_d_age$rrq_rum_total<- rowSums(freezing_raw_d_age[, c("rrq_01","rrq_02","rrq_03","rrq_04","rrq_05","rrq_06","rrq_07","rrq_08","rrq_09",
                                                                  "rrq_10","rrq_11","rrq_12")])
freezing_raw_d_age$rrq_refl_total<- rowSums(freezing_raw_d_age[,c("rrq_13","rrq_14", "rrq_15", "rrq_16","rrq_17", "rrq_18","rrq_19","rrq_20","rrq_21"
                                                                  ,"rrq_22","rrq_23","rrq_24")])

freezing_raw_d_age$atq_appr<-rowSums(freezing_raw_d_age[, c("atq_02","atq_04","atq_05","atq_08","atq_10","atq_11")])
freezing_raw_d_age$atq_avoi<-rowSums(freezing_raw_d_age[, c("atq_01","atq_03","atq_06","atq_07","atq_09","atq_12")])


######################################################
### Create dataset to check lifeevent signficance  ###

lec<-freezing_raw_d_age %>%
  select(1:5,41,127,131:147,397:402)

# Recode original LEC data

lec<-lec %>%
  mutate(lec_1=case_when(
    lec_1 == 1 ~ 1,
    lec_1 != 1 ~ 0,
  ))
lec<-lec %>%
  mutate(lec_2=case_when(
    lec_2 == 1 ~ 1,
    lec_2 != 1 ~ 0,
  ))
lec<-lec %>%
  mutate(lec_3=case_when(
    lec_3 == 1 ~ 1,
    lec_3 != 1 ~ 0,
  ))
lec<-lec %>%
  mutate(lec_4=case_when(
    lec_4 == 1 ~ 1,
    lec_4 != 1 ~ 0,
  ))
lec<-lec %>%
  mutate(lec_5=case_when(
    lec_5 == 1 ~ 1,
    lec_5 != 1 ~ 0,
  ))
lec<-lec %>%
  mutate(lec_6=case_when(
    lec_6 == 1 ~ 1,
    lec_6 != 1 ~ 0,
  ))
lec<-lec %>%
  mutate(lec_7=case_when(
    lec_7 == 1 ~ 1,
    lec_7 != 1 ~ 0,
  ))
lec<-lec %>%
  mutate(lec_8=case_when(
    lec_8 == 1 ~ 1,
    lec_8 != 1 ~ 0,
  ))
lec<-lec %>%
  mutate(lec_9=case_when(
    lec_9 == 1 ~ 1,
    lec_9 != 1 ~ 0,
  ))
lec<-lec %>%
  mutate(lec_10=case_when(
    lec_10 == 1 ~ 1,
    lec_10 != 1 ~ 0,
  ))
lec<-lec %>%
  mutate(lec_11=case_when(
    lec_11 == 1 ~ 1,
    lec_11 != 1 ~ 0,
  ))
lec<-lec %>%
  mutate(lec_12=case_when(
    lec_12 == 1 ~ 1,
    lec_12 != 1 ~ 0,
  ))
lec<-lec %>%
  mutate(lec_13=case_when(
    lec_13 == 1 ~ 1,
    lec_13 != 1 ~ 0,
  ))
lec<-lec %>%
  mutate(lec_14=case_when(
    lec_14 == 1 ~ 1,
    lec_14 != 1 ~ 0,
  ))
lec<-lec %>%
  mutate(lec_15=case_when(
    lec_15 == 1 ~ 1,
    lec_15 != 1 ~ 0,
  ))
lec<-lec %>%
  mutate(lec_16=case_when(
    lec_16 == 1 ~ 1,
    lec_16 != 1 ~ 0,
  ))
lec<-lec %>%
  mutate(lec_17=case_when(
    lec_17 == 1 ~ 1,
    lec_17 != 1 ~ 0,
  ))

# Create summed column to assess whether or not participants endorsed no events (0), 1 event (1), or more than 1 event (2+)

lec$lec_score<-rowSums(lec[,c(8:24)])

lec<-lec %>%
  mutate(lifeevent=case_when(
    lec_score == 0 ~ "none",
    lec_score == 1 ~ "single",
    lec_score >= 2 ~ "multiple"
  ))

# Check our sample for each category

count(lec$lifeevent)

# separate endorsement groups

disaster <- lec %>%
  dplyr::filter(lec$lec_1 == 1)
fire <- lec %>%
  dplyr::filter(lec$lec_2 == 1)
car_acc <- lec %>%
  dplyr::filter(lec$lec_3 == 1)
ser_acc <- lec %>%
  dplyr::filter(lec$lec_4 == 1)
tox_exp <- lec %>%
  dplyr::filter(lec$lec_5 == 1)
phys_aslt <- lec %>%
  dplyr::filter(lec$lec_6 == 1)
aslt_weap <- lec %>%
  dplyr::filter(lec$lec_7 == 1)
sxl_aslt <- lec %>%
  dplyr::filter(lec$lec_8 == 1)
unwntd_sx_exp <- lec %>%
  dplyr::filter(lec$lec_9 == 1)
combat <- lec %>%
  dplyr::filter(lec$lec_10 == 1)
captivity <- lec %>%
  dplyr::filter(lec$lec_11 == 1)
life_threat <- lec %>%
  dplyr::filter(lec$lec_12 == 1)
suffering <- lec %>%
  dplyr::filter(lec$lec_13 == 1)
violent_death <- lec %>%
  dplyr::filter(lec$lec_14 == 1 | lec$lec_14 == 2)
acc_death <- lec %>%
  dplyr::filter(lec$lec_15 == 1 | lec$lec_15 == 2)
caused_harm <- lec %>%
  dplyr::filter(lec$lec_16 == 1)
other <- lec %>%
  dplyr::filter(lec$lec_17 == 1)



### Assess for differences in endorsed life events
# try this --> filter(xor(condition1, condition2)), found here: https://suzan.rbind.io/2018/02/dplyr-tutorial-3/


########################################################
####### Hierarchical Multi Regression Analysis #######
########################################################

# Check for multicollinearity between IVs

# Assess Correlation between IVs
IV <- cbind(lec$age, lec$atq_appr,lec$atq_avoi)
cor(IV, method = c("pearson"))


# separate DVs for this process


## Worry

# Build models - note: build each regression for each DV

# The order of this hierarchical regression is theoretically-based to attend to chronological effects and prioritized main effect analysis
#0 ~ 1 --> total SS
#1 ~ --> age
#2 ~ --> timepoint
#3 ~  --> interaction timepoint * age 
#4 ~  --> life event
#5 ~  --> timepoint * life event
#6 ~ --> avoidance
#7 ~  --> timepoint * avoidance
#8 ~ --> approach
#9 ~ timepoint * approach
#10 ~ lifeevent and approach (lower-order constituent step)
#11 ~ timepoint, lifeevent, and approach

#Set Reference levels

levels(lec$timepoint)
lec$timepoint <- relevel(lec$timepoint,"pre-spring break")

# Now, let's implement, start with PSWQ
w00 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint + lifeevent:age + lifeevent:timepoint:age 
          + atq_avoi + atq_avoi:timepoint + atq_avoi:age + atq_avoi:lifeevent + atq_avoi:timepoint:age + atq_avoi:timepoint:lifeevent 
          + atq_avoi:lifeevent:age + atq_avoi:timepoint:age:lifeevent + atq_appr + atq_appr:timepoint + atq_appr:age + atq_appr:lifeevent
          + atq_appr:atq_avoi + atq_appr:timepoint:age + atq_appr:timepoint:lifeevent + atq_appr:timepoint:atq_avoi + atq_appr:lifeevent:age
          + atq_appr:lifeevent:atq_avoi + atq_appr:age:atq_avoi + atq_appr:timepoint:lifeevent:age + atq_appr:timepoint:lifeevent:atq_avoi 
          + atq_appr:age:lifeevent:atq_avoi + atq_appr:age:timepoint:atq_avoi + atq_appr:lifeevent:age:timepoint:atq_avoi, data=lec) # to obtain total R-squared to report to reader
w0 <- lm(pswq_total ~ 1, data=lec)  # to obtain Total SS
w1 <- lm(pswq_total ~ age, data=lec)  # effect of age 
w2 <- lm(pswq_total ~ timepoint + age, data=lec)  # main effect of covid warning onset (spring break divide)
w3 <- lm(pswq_total ~ timepoint + age + timepoint:age, data=lec)   # effect of interaction between spring break and age 
w4 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent, data=lec)   # life event
w5 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint, data=lec)   # effect of interaction between spring break and lifeevent
w6 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi, data=lec)   # avoidance
w7 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint, data=lec)   # effect of interaction between spring break and avoidance
w8 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr, data=lec)   # approach
w9 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr + 
           atq_appr:timepoint, data=lec)   # effect of interaction between spring break and approach
w10 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr + 
            atq_appr:timepoint + atq_appr:lifeevent, data=lec)   # effect of interaction between lifeevent and approach (lower-order constituent step)
w11 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr + 
            atq_appr:timepoint + atq_appr:lifeevent + atq_appr:timepoint:lifeevent, data=lec)   # effect of interaction between spring break, lifeevent, and approach

# Total variance
summary(w00)

# model comparison for PSWQ
anova(w0,w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w00)  
#Res.   Df    RSS Df Sum of Sq        F    Pr(>F)    
#0     652 187471                                    
#1     651 187354  1       118   0.8537  0.356393     #deltaR = 118/187471 = 0.0006294307
#2     650 187347  1         6   0.0446  0.832988     #deltaR = 6/187471 = 3.200495e-05
#3     649 186238  1      1109   8.0481  0.004746 **  #deltaR = 1109/187471 = 0.005915582
#4     648 185441  1       797   5.7852  0.016566 *   #deltaR = 797/187471 = 0.004251324
#5     647 185176  1       265   1.9254  0.166228     #deltaR = 265/187471 = 0.001413552
#6     646  89294  1     95882 695.6976 < 2.2e-16 *** #deltaR = 95882/187471 = 0.5114498
#7     645  89293  1         1   0.0040  0.949353     #deltaR = 1/187471 = 5.334158e-06
#8     644  88711  1       582   4.2197  0.040587 *   #deltaR = 582/187471 = 0.00310448
#9     643  88711  1         0   0.0006  0.980754     #deltaR = 0/187471 = 0.0
#10    642  88645  1        66   0.4790  0.489600     #deltaR = 66/187471 = 0.0003520545
#11    641  88543  1       102   0.7404  0.390398     #deltaR = 102/187471 = 0.0005440842
#12    621  85587 20      2956   1.0725  0.374381     #deltaR = 2956/187471 = 0.01576777
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



#### obtain coefficients and R-squared values between models

summary(w1)
lm.beta (w1)
# coef:  0.3595, Multiple R-squared:  0.0006276, F-statistic: 0.4088 on 1 and 651 DF,  p-value: 0.5228, ∆R = 0.0006276
summary(w2)
lm.beta (w2)
# coef:  -0.230, Multiple R-squared:  0.0006604, F-statistic: 0.2148 on 2 and 650 DF,  p-value: 0.8068, ∆R = 0.0006604 - 0.0006276 = 3.28e-05
summary(w3)
lm.beta (w3)
# coef:  -2.581, Multiple R-squared:  0.006577, F-statistic: 1.432 on 3 and 649 DF,  p-value: 0.2322, ∆R = 0.006577 - 0.0006604 =  0.0059166
summary(w4)
lm.beta (w4)
# coef:  2.543, Multiple R-squared:  0.01083, F-statistic: 1.774 on 4 and 648 DF,  p-value: 0.1324, ∆R = 0.01083 - 0.006577 = 0.004253
summary(w5)
lm.beta(w5)
coef.lm.beta(lmw5)
# coef:  -3.370, Multiple R-squared: 0.01225, F-statistic: 1.604 on 5 and 647 DF,  p-value: 0.1568, ∆R = 0.01225 - 0.01083 = 0.00142
summary(w6)
lm.beta(w6)
# coef:  1.62167, Multiple R-squared: 0.5237, F-statistic: 118.4 on 6 and 646 DF,  p-value: < 2.2e-16, ∆R = 0.5237 - 0.01225 = 0.51145
summary(w7)
lm.beta(w7)
# coef:  -0.009091, Multiple R-squared:  0.5237, F-statistic: 101.3 on 7 and 645 DF,  p-value: < 2.2e-16, ∆R = 0.5237 - 0.5237 = 0.00
summary(w8)
lm.beta(w8)
# coef:   -0.16268, Multiple R-squared:  0.5268, F-statistic: 89.62 on 8 and 644 DF,  p-value: < 2.2e-16, ∆R = 0.5268 - 0.5237 = 0.0031
summary(w9)
lm.beta(w9)
# coef:   -0.004406, Multiple R-squared:  0.5268, F-statistic: 79.54 on 9 and 643 DF,  p-value: < 2.2e-16, ∆R = 0.5268 - 0.5268 = 0.00
summary(w10)
lm.beta(w10)
# coef:   -0.13167, Multiple R-squared:  0.5272, F-statistic: 71.57 on 10 and 642 DF,  p-value: < 2.2e-16, ∆R = 0.5272 - 0.5268 = 0.0004
summary(w11)
lm.beta(w11)
# coef:   -0.39507, Multiple R-squared:  0.5277, F-statistic: 65.11 on 11 and 641 DF,  p-value: < 2.2e-16, ∆R = 0.5277 - 0.5272 = 0.0005

# Visualize
ggplot(lec, aes(x=age, y=pswq_total, color=lifeevent)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F) +
  labs(x= "Age", y= "Worry") +
  labs(color= "Timepoint")

ggplot(lec, aes(x=atq_avoi, y=pswq_total)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F)

ggplot(lec, aes(x=atq_appr, y=pswq_total)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F)

ggplot(lec, aes(x=pswq_total, color=lifeevent)) + 
  geom_histogram(binwidth=5, fill="white", position = "dodge")

ggplot(lec, aes(x=age, y=pswq_total, color = lifeevent)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F)

# "Control" for other DVs
wph0 <- lm(pswq_total ~ 1, data=lec)  # to obtain Total SS
wph1 <- lm(pswq_total ~ rrq_rum_total, data=lec)  # account for shared variance between rumination and worry
wph3 <- lm(pswq_total ~ rrq_rum_total + age, data=lec)  # effect of age 
wph4 <- lm(pswq_total ~ rrq_rum_total + timepoint + age, data=lec)  # main effect of covid warning onset (spring break divide)
wph5 <- lm(pswq_total ~ rrq_rum_total + timepoint + age + timepoint:age, data=lec)   # effect of interaction between spring break and age 
wph6 <- lm(pswq_total ~ rrq_rum_total + timepoint + age + timepoint:age + lifeevent, data=lec)   # life event
wph7 <- lm(pswq_total ~ rrq_rum_total + timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint, data=lec)   # effect of interaction between spring break and lifeevent
wph8 <- lm(pswq_total ~ rrq_rum_total + timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi, data=lec)   # avoidance
wph9 <- lm(pswq_total ~ rrq_rum_total + timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint, data=lec)   # effect of interaction between spring break and avoidance
wph10 <- lm(pswq_total ~ rrq_rum_total  + timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr, data=lec)   # approach

# post hoc model comparison for PSWQ
anova(wph0,wph1, wph2, wph3, wph4, wph5, wph6, wph7, wph8, wph9, wph10)  
# Just controlling for Rumination
#Res.Df    RSS Df Sum of Sq        F  Pr(>F)    
#1     652 187471                                  
#2     651 108858  1     78614 650.7940 < 2e-16 ***
#3     650 108846  1        12   0.0953 0.75770    
#4     649 108845  1         1   0.0098 0.92122    
#5     648 108801  1        44   0.3636 0.54674    
#6     647 108783  1        18   0.1496 0.69902    
#7     646 108412  1       371   3.0693 0.08026 .  
#8     645  78044  1     30368 251.3975 < 2e-16 ***
#9     644  78042  1         2   0.0167 0.89716    
#10    643  77672  1       370   3.0650 0.08047 .  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# controlling for Rumination & Depressed Mood
#Res.Df    RSS Df Sum of Sq        F    Pr(>F)    
#1     652 187471                                    
#2     651 108858  1     78614 650.3793 < 2.2e-16 ***
#3     650 106609  1      2249  18.6067  1.86e-05 ***
#4     649 106592  1        17   0.1385   0.70989    
#5     648 106353  1       239   1.9803   0.15984    
#6     647 106308  1        44   0.3652   0.54586    
#7     646 106272  1        37   0.3039   0.58164    
#8     645 105913  1       359   2.9711   0.08524 .  
#9     644  77870  1     28043 232.0021 < 2.2e-16 ***
#10    643  77865  1         4   0.0356   0.85035    
#11    642  77601  1       265   2.1890   0.13949    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## Rumination

# Build models
rm00 <- lm(rrq_rum_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint + lifeevent:age + lifeevent:timepoint:age 
           + atq_avoi + atq_avoi:timepoint + atq_avoi:age + atq_avoi:lifeevent + atq_avoi:timepoint:age + atq_avoi:timepoint:lifeevent 
           + atq_avoi:lifeevent:age + atq_avoi:timepoint:age:lifeevent + atq_appr + atq_appr:timepoint + atq_appr:age + atq_appr:lifeevent
           + atq_appr:atq_avoi + atq_appr:timepoint:age + atq_appr:timepoint:lifeevent + atq_appr:timepoint:atq_avoi + atq_appr:lifeevent:age
           + atq_appr:lifeevent:atq_avoi + atq_appr:age:atq_avoi + atq_appr:timepoint:lifeevent:age + atq_appr:timepoint:lifeevent:atq_avoi 
           + atq_appr:age:lifeevent:atq_avoi + atq_appr:age:timepoint:atq_avoi + atq_appr:lifeevent:age:timepoint:atq_avoi, data=lec) # to obtain total R-squared to report to reader
rm0 <- lm(rrq_rum_total ~ 1, data=lec)  # to obtain Total SS
rm1 <- lm(rrq_rum_total ~ age, data=lec)  # effect of age 
rm2 <- lm(rrq_rum_total ~ timepoint + age, data=lec)  # main effect of covid warning onset (spring break divide)
rm3 <- lm(rrq_rum_total ~ timepoint + age + timepoint:age, data=lec)   # effect of interaction between spring break and age 
rm4 <- lm(rrq_rum_total ~ timepoint + age + timepoint:age + lifeevent, data=lec)   # life event
rm5 <- lm(rrq_rum_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint, data=lec)   # effect of interaction between spring break and lifeevent
rm6 <- lm(rrq_rum_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi, data=lec)   # avoidance
rm7 <- lm(rrq_rum_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint, data=lec)   # effect of interaction between spring break and avoidance
rm8 <- lm(rrq_rum_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr, data=lec)   # approach
rm9 <- lm(rrq_rum_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr + 
            atq_appr:timepoint, data=lec)   # effect of interaction between spring break and approach
rm10 <- lm(rrq_rum_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr + 
             atq_appr:timepoint + atq_appr:lifeevent, data=lec)   # effect of interaction between lifeevent and approach (lower-order constituent step)
rm11 <- lm(rrq_rum_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr + 
             atq_appr:timepoint + atq_appr:lifeevent + atq_appr:timepoint:lifeevent, data=lec)   # effect of interaction between spring break, lifeevent, and approach

# Total variance
summary(rm00)

# model comparison for Rumination
anova(rm0,rm1, rm2, rm3, rm4, rm5, rm6, rm7, rm8, rm9, rm10, rm11, rm00)  
#Res.Df   RSS Df Sum of Sq        F    Pr(>F)    
#0     652 46126                                    
#1     651 46094  1      32.6   0.7631 0.3800414    # deltaR = 32.6/46126 = 0.0007067597
#2     650 46093  1       1.1   0.0266 0.8697495    # deltaR = 1.1/46126 = 2.384772e-05
#3     649 45674  1     418.8   9.7974 0.0017229 ** # deltaR = 418.8/46126 = 0.009079478
#4     648 45052  1     621.4  14.5373  0.0001385 ***# deltaR = 621.4/46126 = 0.01347179
#5     647 45047  1       5.2   0.1208 0.7268203    # deltaR = 5.2/46126 = 0.0001127347
#6     646 27265  1   17781.9  415.978 < 2.2e-16 ***# deltaR = 17781.9/46126 = 0.3855071
#7     645 27254  1      11.4   0.2664 0.6039181    # deltaR = 11.4/46126 = .0002471491
#8     644 27196  1      58.0   1.3576 0.2417636    # deltaR = 58.0/46126 = 0.001257425
#9     643 27138  1      57.4   1.3417 0.2445419    # deltaR = 57.4/46126 = 0.001244417
#10    642 27131  1       7.8   0.1820 0.6680650    # deltaR = 7.8/46126 = 0.000169102
#11    641 27098  1      32.6   0.7627 0.3801590    # deltaR = 32.6/46126 = 0.0007067597
#12    621 26546 20     552.0   0.6456 0.8788835    # deltaR = 552/46126 = 0.01196722
#---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#### obtain coefficients and R-squared values between models
summary(rm1)
lm.beta(rm1)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(rm2)
lm.beta(rm2)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(rm3)
lm.beta(rm3)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(rm4)
lm.beta(rm4)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(rm5)
lm.beta(rm5)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(rm6)
lm.beta(rm6)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(rm7)
lm.beta(rm7)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(rm8)
lm.beta(rm8)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(rm9)
lm.beta(rm9)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(rm10)
lm.beta(rm10)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(rm11)
lm.beta(rm11)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 

# Visualize
ggplot(lec, aes(x=age, y=rrq_rum_total, color=timepoint)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F) +
  labs(x= "Age", y= "Rumination") +
  labs(color= "Timepoint")

ggplot(lec, aes(x=atq_avoi, y=rrq_rum_total)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F)

ggplot(lec, aes(x=age, y=rrq_rum_total, color = lifeevent)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F)

ggplot(lec, aes(x=rrq_rum_total, color=lifeevent)) + 
  geom_histogram(binwidth=5, fill= "white", position = "dodge")

# "Control" for other DVs
rmph0 <- lm(rrq_rum_total ~ 1, data=lec)  # to obtain Total SS
rmph1 <- lm(rrq_rum_total ~ pswq_total, data=lec)  # account for shared variance between rumination and worry
rmph2 <- lm(rrq_rum_total ~ pswq_total + masq_dm_total, data=lec)  # account for shared variance between rumination and depressed mood
rmph3 <- lm(rrq_rum_total ~ pswq_total + masq_dm_total + age, data=lec)  # effect of age 
rmph4 <- lm(rrq_rum_total ~ pswq_total + masq_dm_total + timepoint + age, data=lec)  # main effect of covid warning onset (spring break divide)
rmph5 <- lm(rrq_rum_total ~ pswq_total + masq_dm_total + timepoint + age + timepoint:age, data=lec)   # effect of interaction between spring break and age 
rmph6 <- lm(rrq_rum_total ~ pswq_total + masq_dm_total + timepoint + age + timepoint:age + lifeevent, data=lec)   # life event
rmph7 <- lm(rrq_rum_total ~ pswq_total + masq_dm_total + timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint, data=lec)   # effect of interaction between spring break and lifeevent
rmph8 <- lm(rrq_rum_total ~ pswq_total + masq_dm_total + timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi, data=lec)   # avoidance

# post hoc model comparison for Rum after PSWQ
anova(rmph0,rmph1, rmph2, rmph3, rmph4,rmph5, rmph6,rmph7, rmph8)  
# Just controlling for PSWQ
#Res.Df   RSS Df Sum of Sq        F    Pr(>F)    
#1    652 46126                                    
#2    651 26784  1   19342.5 523.5298 < 2.2e-16 ***
#3    650 26779  1       5.0   0.1343  0.714086    
#4    649 26779  1       0.1   0.0020  0.964517    
#5    648 26683  1      96.1   2.5998  0.107367    
#6    647 26429  1     254.3   6.8821  0.008912 ** 
#7    646 26373  1      55.4   1.4983  0.221384    
#8    645 23830  1    2542.8  68.8250 6.288e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# controlling for PSWQ & DM
#Res.Df   RSS Df Sum of Sq        F    Pr(>F)    
#1    652 46126                                    
#2    651 26784  1   19342.5 539.9947 < 2.2e-16 ***
#3    650 25415  1    1368.7  38.2102 1.128e-09 ***
#4    649 25408  1       7.3   0.2043   0.65145    
#5    648 25253  1     154.9   4.3248   0.03796 *  
#6    647 25165  1      87.8   2.4521   0.11786    
#7    646 24980  1     185.3   5.1727   0.02327 *  
#8    645 24932  1      48.0   1.3401   0.24744    
#9    644 23068  1    1863.9  52.0366 1.536e-12 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## Reflection

# Build models
rf00 <- lm(rrq_refl_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint + lifeevent:age + lifeevent:timepoint:age 
           + atq_avoi + atq_avoi:timepoint + atq_avoi:age + atq_avoi:lifeevent + atq_avoi:timepoint:age + atq_avoi:timepoint:lifeevent 
           + atq_avoi:lifeevent:age + atq_avoi:timepoint:age:lifeevent + atq_appr + atq_appr:timepoint + atq_appr:age + atq_appr:lifeevent
           + atq_appr:atq_avoi + atq_appr:timepoint:age + atq_appr:timepoint:lifeevent + atq_appr:timepoint:atq_avoi + atq_appr:lifeevent:age
           + atq_appr:lifeevent:atq_avoi + atq_appr:age:atq_avoi + atq_appr:timepoint:lifeevent:age + atq_appr:timepoint:lifeevent:atq_avoi 
           + atq_appr:age:lifeevent:atq_avoi + atq_appr:age:timepoint:atq_avoi + atq_appr:lifeevent:age:timepoint:atq_avoi, data=lec) # to obtain total R-squared to report to reader
rf0 <- lm(rrq_refl_total ~ 1, data=lec)  # to obtain Total SS
rf1 <- lm(rrq_refl_total ~ age, data=lec)  # effect of age 
rf2 <- lm(rrq_refl_total ~ timepoint + age, data=lec)  # main effect of covid warning onset (spring break divide)
rf3 <- lm(rrq_refl_total ~ timepoint + age + timepoint:age, data=lec)   # effect of interaction between spring break and age 
rf4 <- lm(rrq_refl_total ~ timepoint + age + timepoint:age + lifeevent, data=lec)   # life event
rf5 <- lm(rrq_refl_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint, data=lec)   # effect of interaction between spring break and lifeevent
rf6 <- lm(rrq_refl_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi, data=lec)   # avoidance
rf7 <- lm(rrq_refl_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint, data=lec)   # effect of interaction between spring break and avoidance
rf8 <- lm(rrq_refl_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr, data=lec)   # approach
rf9 <- lm(rrq_refl_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr + 
            atq_appr:timepoint, data=lec)   # effect of interaction between spring break and approach
rf10 <- lm(rrq_refl_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr + 
             atq_appr:timepoint + atq_appr:lifeevent, data=lec)   # effect of interaction between lifeevent and approach (lower-order constituent step)
rf11 <- lm(rrq_refl_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr + 
             atq_appr:timepoint + atq_appr:lifeevent + atq_appr:timepoint:lifeevent, data=lec)   # effect of interaction between spring break, lifeevent, and approach

# Total variance
summary(rf00)

# model comparison for Reflection
anova(rf0,rf1, rf2, rf3, rf4, rf5, rf6, rf7, rf8, rf9, rf10, rf11, rf00)  
#Res.Df   RSS Df Sum of Sq       F    Pr(>F)    
#0     652 48740                                   
#1     651 48729  1     11.02  0.1598  0.691381    # deltaR = 11.02/48740 = 0.0002260977
#2     650 48719  1     10.49  0.1521  0.698562    # deltaR = 10.49/48740 = 0.0002152236
#3     649 48708  1     10.76  0.1561  0.694800    # deltaR = 10.76/48740 = 0.0002207632
#4     648 48498  1    209.41  3.0370  0.083858 .  # deltaR = 209.41/48740 = 0.004296471
#5     647 48277  1    221.75  3.2160  0.075272 .  # deltaR = 221.75/48740 = 0.004549651
#6     646 47644  1    632.57  9.1740  0.002722 ** # deltaR = 632.57/48740 = 0.01297846
#7     645 46882  1    762.17 11.0535  0.001009 ** # deltaR = 762.17/48740 = 0.01563746
#8     644 44946  1   1936.27 28.0813 1.917e-07 ***# deltaR = 1936.27/48740 = 0.03972651
#9     643 44946  1      0.03  0.0004  0.984374     # deltaR = 0.03/48740 = 6.155109e-07
#10    642 44937  1      8.74  0.1268  0.723665    # deltaR = 8.74/48740 = 0.0001793188
#11    641 44777  1    160.03  2.3209  0.130625    # deltaR = 160.03/48740 = 0.00328334
#12    621 42820 20   1957.37  1.4194 0.1057310    # deltaR = 1957.37/48740 = 0.04015942
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#### obtain coefficients and R-squared values between models
summary(rf1)
lm.beta(rf1)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(rf2)
lm.beta(rf2)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(rf3)

lm.beta(rf3)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(rf4)
lm.beta(rf4)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(rf5)
lm.beta(rf5)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(rf6)
lm.beta(rf6)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(rf7)
lm.beta(rf7)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(rf8)
lm.beta(rf8)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(rf9)
lm.beta(rf9)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(rf10)
lm.beta(rf10)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(rf11)
lm.beta(rf11)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 

# Visualize

ggplot(lec, aes(x=atq_avoi, y=rrq_refl_total, color=timepoint)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F) +
  labs(x= "Avoidance", y= "Reflection") +
  labs(color= "Timepoint")

ggplot(lec, aes(x=atq_avoi, y=rrq_refl_total)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F)


ggplot(lec, aes(x=atq_appr, y=rrq_refl_total)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F)

# Control for other DVs
rfph0 <- lm(rrq_refl_total ~ 1, data=lec)  # to obtain Total SS
rfph1 <- lm(rrq_refl_total ~ rrq_rum_total, data=lec)  # rumination
rfph1 <- lm(rrq_refl_total ~ rrq_rum_total + age, data=lec)  # effect of age 
rfph2 <- lm(rrq_refl_total ~ rrq_rum_total + timepoint + age, data=lec)  # main effect of covid warning onset (spring break divide)
rfph3 <- lm(rrq_refl_total ~ rrq_rum_total + timepoint + age + timepoint:age, data=lec)   # effect of interaction between spring break and age 
rfph4 <- lm(rrq_refl_total ~ rrq_rum_total + timepoint + age + timepoint:age + lifeevent, data=lec)   # life event
rfph5 <- lm(rrq_refl_total ~ rrq_rum_total + timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint, data=lec)   # effect of interaction between spring break and lifeevent
rfph6 <- lm(rrq_refl_total ~ rrq_rum_total +  timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi, data=lec)   # avoidance
rfph7 <- lm(rrq_refl_total ~ rrq_rum_total + timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint, data=lec)   # effect of interaction between spring break and avoidance
rfph8 <- lm(rrq_refl_total ~ rrq_rum_total + timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr, data=lec)   # approach


## Anxious Arousal

# Build models
aa00 <- lm(masq_aa_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint + lifeevent:age + lifeevent:timepoint:age 
           + atq_avoi + atq_avoi:timepoint + atq_avoi:age + atq_avoi:lifeevent + atq_avoi:timepoint:age + atq_avoi:timepoint:lifeevent 
           + atq_avoi:lifeevent:age + atq_avoi:timepoint:age:lifeevent + atq_appr + atq_appr:timepoint + atq_appr:age + atq_appr:lifeevent
           + atq_appr:atq_avoi + atq_appr:timepoint:age + atq_appr:timepoint:lifeevent + atq_appr:timepoint:atq_avoi + atq_appr:lifeevent:age
           + atq_appr:lifeevent:atq_avoi + atq_appr:age:atq_avoi + atq_appr:timepoint:lifeevent:age + atq_appr:timepoint:lifeevent:atq_avoi 
           + atq_appr:age:lifeevent:atq_avoi + atq_appr:age:timepoint:atq_avoi + atq_appr:lifeevent:age:timepoint:atq_avoi, data=lec) # to obtain total R-squared to report to reader
aa0 <- lm(masq_aa_total ~ 1, data=lec)  # to obtain Total SS
aa1 <- lm(masq_aa_total ~ age, data=lec)  # effect of age 
aa2 <- lm(masq_aa_total ~ timepoint + age, data=lec)  # main effect of covid warning onset (spring break divide)
aa3 <- lm(masq_aa_total ~ timepoint + age + timepoint:age, data=lec)   # effect of interaction between spring break and age 
aa4 <- lm(masq_aa_total ~ timepoint + age + timepoint:age + lifeevent, data=lec)   # life event
aa5 <- lm(masq_aa_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint, data=lec)   # effect of interaction between spring break and lifeevent
aa6 <- lm(masq_aa_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi, data=lec)   # avoidance
aa7 <- lm(masq_aa_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint, data=lec)   # effect of interaction between spring break and avoidance
aa8 <- lm(masq_aa_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr, data=lec)   # approach
aa9 <- lm(masq_aa_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr + 
            atq_appr:timepoint, data=lec)   # effect of interaction between spring break and approach
aa10 <- lm(masq_aa_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr + 
             atq_appr:timepoint + atq_appr:lifeevent, data=lec)   # effect of interaction between lifeevent and approach (lower-order constituent step)
aa11 <- lm(masq_aa_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr + 
             atq_appr:timepoint + atq_appr:lifeevent + atq_appr:timepoint:lifeevent, data=lec)   # effect of interaction between spring break, lifeevent, and approach

#Total variance explained by all IVs
summary(aa00)

# model comparison for anxious arousal
anova(aa0,aa1, aa2, aa3, aa4, aa5, aa6, aa7, aa8, aa9, aa10, aa11, aa00)  
#Res.Df   RSS Df Sum of Sq       F   Pr(>F)    
#0     652 65308                                  
#1     651 65033  1     275.4  3.2120  0.07383 .  # deltaR = 275.4/65308 = 0.004216941
#2     650 65032  1       0.1  0.0007  0.97862    # deltaR = 0.1/65308 =  1.531206e-06
#3     649 65001  1      31.1  0.3625  0.54770    # deltaR = 31.1/65308 = 0.0004762051
#4     648 65001  1       0.4  0.0048  0.94511    # deltaR = 0.4/65308 = 6.124824e-06
#5     647 64803  1     198.2  2.3113  0.12927    # deltaR = 198.2/65308 = 0.00303485
#6     646 59662  1    5140.7 59.9610 3.98e-14 ***# deltaR = 5140.7/65308 = 0.07871471
#7     645 59637  1      24.7  0.2878  0.59216    # deltaR = 24.7/65308 = 0.0003782079
#8     644 57430  1    2207.5 25.7479 5.22e-07 ***# deltaR = 2207.5/65308 = 0.03380137
#9     643 57092  1     337.8  3.9405  0.04776 *  # deltaR = 337.8/65308 = 0.005172414
#10    642 55511  1    1580.9 18.4393 2.06e-05 ***# deltaR = 1580.9/65308 = 0.02420684
#11    641 55053  1     458.0  5.3425  0.02124 *  # deltaR = 458/65308 = 0.007012923
#12    621 42820 20   1957.37  1.0570 0.1057310   # deltaR = 1957.37/65308 = 0.02997137
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#### obtain coefficients and multiple R-squared for each stage of each model
summary(aa1)
lm.beta(aa1)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(aa2)
lm.beta(aa2)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(aa3)
lm.beta(aa3)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(aa4)
lm.beta(aa4)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(aa5)
lm.beta(aa5)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(aa6)
lm.beta(aa6)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(aa7)
lm.beta(aa7)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(aa8)
lm.beta(aa8)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(aa9)
lm.beta(aa9)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(aa10)
lm.beta(aa10)
15# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(aa11)
lm.beta(aa11)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 

#Visualize

ggplot(lec, aes(x=atq_appr, y=masq_aa_total, color = timepoint)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F) +
  labs(x= "Approach", y= "Amxious Arousal") +
  labs(color= "Timepoint")

qplot(x=atq_appr, y=masq_aa_total, facets = ~lifeevent, color = timepoint, data = lec) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F) +
  labs(x= "Approach", y= "Amxious Arousal") +
  labs(facets= "Life Event") +
  labs(color= "Timepoint")



ggplot(lec, aes(x=atq_avoi, y=masq_aa_total)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F)


ggplot(lec, aes(x=atq_appr, y=masq_aa_total)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F)

# "Control" for other DVs
aaph0 <- lm(masq_aa_total ~ 1, data=lec)  # to obtain Total SS
aaph1 <- lm(masq_aa_total ~ masq_dm_total, data=lec)  # effect of dm on aa
aaph2 <- lm(masq_aa_total ~ masq_dm_total + age, data=lec)  # effect of age 
aaph3 <- lm(masq_aa_total ~ masq_dm_total + timepoint + age, data=lec)  # main effect of covid warning onset (spring break divide)
aaph4 <- lm(masq_aa_total ~ masq_dm_total + timepoint + age + timepoint:age, data=lec)   # effect of interaction between spring break and age 
aaph5 <- lm(masq_aa_total ~ masq_dm_total + timepoint + age + timepoint:age + lifeevent, data=lec)   # life event
aaph6 <- lm(masq_aa_total ~ masq_dm_total + timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint, data=lec)   # effect of interaction between spring break and lifeevent
aaph7 <- lm(masq_aa_total ~ masq_dm_total + timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi, data=lec)   # avoidance
aaph8 <- lm(masq_aa_total ~ masq_dm_total + timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint, data=lec)   # effect of interaction between spring break and avoidance
aaph9 <- lm(masq_aa_total ~ masq_dm_total + timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr, data=lec)   # approach
aaph10 <- lm(masq_aa_total ~ masq_dm_total + timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr + 
               atq_appr:timepoint, data=lec)   # effect of interaction between spring break and approach
aaph11 <- lm(masq_aa_total ~ masq_dm_total + timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr + 
               atq_appr:timepoint + atq_appr:lifeevent, data=lec)   # effect of interaction between lifeevent and approach (lower-order constituent step)
aaph12 <- lm(masq_aa_total ~ masq_dm_total + timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr + 
               atq_appr:timepoint + atq_appr:lifeevent + atq_appr:timepoint:lifeevent, data=lec)   # effect of interaction between spring break, lifeevent, and approach

# model comparison for anxious arousal after controlling for dm
anova(aaph0,aaph1, aaph2, aaph3, aaph4, aaph5, aaph6, aaph7, aaph8, aaph9, aaph10, aaph11, aaph12)
# after controlling for dm
#Res.Df   RSS Df Sum of Sq        F    Pr(>F)    
#1     652 65308                                    
#2     651 46727  1   18581.1 285.2079 < 2.2e-16 ***
#3     650 46466  1     261.1   4.0077  0.045713 *  
#4     649 44522  1    1943.7  29.8349 6.741e-08 ***
#5     648 44522  1       0.0   0.0000  0.995653    
#6     647 44374  1     147.9   2.2707  0.132330    
#7     646 44171  1     203.3   3.1210  0.077766 .  
#8     645 43947  1     223.9   3.4361  0.064246 .  
#9     644 43944  1       2.6   0.0398  0.841943    
#10    643 43764  1     180.0   2.7621  0.097010 .  
#11    642 42962  1     802.0  12.3106  0.000482 ***
#12    641 41899  1    1062.9  16.3150 6.015e-05 ***
#13    640 41696  1     203.8   3.1282  0.077424 .  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## Low Positive Affect

# Build models
lpa00 <- lm(masq_lpa_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint + lifeevent:age + lifeevent:timepoint:age 
            + atq_avoi + atq_avoi:timepoint + atq_avoi:age + atq_avoi:lifeevent + atq_avoi:timepoint:age + atq_avoi:timepoint:lifeevent 
            + atq_avoi:lifeevent:age + atq_avoi:timepoint:age:lifeevent + atq_appr + atq_appr:timepoint + atq_appr:age + atq_appr:lifeevent
            + atq_appr:atq_avoi + atq_appr:timepoint:age + atq_appr:timepoint:lifeevent + atq_appr:timepoint:atq_avoi + atq_appr:lifeevent:age
            + atq_appr:lifeevent:atq_avoi + atq_appr:age:atq_avoi + atq_appr:timepoint:lifeevent:age + atq_appr:timepoint:lifeevent:atq_avoi 
            + atq_appr:age:lifeevent:atq_avoi + atq_appr:age:timepoint:atq_avoi + atq_appr:lifeevent:age:timepoint:atq_avoi, data=lec) # to obtain total R-squared to report to reader
lpa0 <- lm(masq_lpa_total ~ 1, data=lec)  # to obtain Total SS
lpa1 <- lm(masq_lpa_total ~ age, data=lec)  # effect of age 
lpa2 <- lm(masq_lpa_total ~ timepoint + age, data=lec)  # main effect of covid warning onset (spring break divide)
lpa3 <- lm(masq_lpa_total ~ timepoint + age + timepoint:age, data=lec)   # effect of interaction between spring break and age 
lpa4 <- lm(masq_lpa_total ~ timepoint + age + timepoint:age + lifeevent, data=lec)   # life event
lpa5 <- lm(masq_lpa_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint, data=lec)   # effect of interaction between spring break and lifeevent
lpa6 <- lm(masq_lpa_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi, data=lec)   # avoidance
lpa7 <- lm(masq_lpa_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint, data=lec)   # effect of interaction between spring break and avoidance
lpa8 <- lm(masq_lpa_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr, data=lec)   # approach
lpa9 <- lm(masq_lpa_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr + 
             atq_appr:timepoint, data=lec)   # effect of interaction between spring break and approach
lpa10 <- lm(masq_lpa_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr + 
              atq_appr:timepoint + atq_appr:lifeevent, data=lec)   # effect of interaction between lifeevent and approach (lower-order constituent step)
lpa11 <- lm(masq_lpa_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr + 
              atq_appr:timepoint + atq_appr:lifeevent + atq_appr:timepoint:lifeevent, data=lec)   # effect of interaction between spring break, lifeevent, and approach

# Total variance explained by all IVs of interest
summary(lpa00)


# model comparison for low positive affect
anova(lpa0,lpa1, lpa2, lpa3, lpa4, lpa5, lpa6, lpa7, lpa8, lpa9, lpa10, lpa11, lpa00)  
#Res.Df   RSS Df Sum of Sq        F    Pr(>F)    
#0     652 82072                                    
#1     651 82052  1      20.3   0.2324   0.63316    # deltaR = 20.3/82072 = 0.0002473438
#2     650 79249  1    2802.8  32.0507 3.044e-08 ***# deltaR = 2802.8/82072 = 0.0341505
#3     649 79178  1      70.6   0.8079   0.37361    # deltaR = 70.6/82072 =  0.0008602203
#4     648 78988  1     190.6   2.1794   0.14413    # deltaR = 190.6/82072 = 0.002322351
#5     647 78984  1       4.3   0.0493   0.82596    # deltaR = 4.3/82072 = 5.239302e-05
#6     646 70248  1    8735.5  99.8926 < 2.2e-16 ***# deltaR = 8735.5/82072 = 0.106437
#7     645 70240  1       8.4   0.0965   0.75836    # deltaR = 8.4/82072 = 0.0001023492
#8     644 57590  1   12649.5 144.6494 < 2.2e-16 ***# deltaR = 12649.5/82072 = 0.1541269
#9     643 57538  1      52.0   0.5944   0.44531     # deltaR = 52/82072 = 0.00063359
#10    642 57198  1     340.2   3.8897   0.05117 .  # deltaR = 340.2/82072 = 0.004145141
#11    641 57125  1      73.4   0.8397   0.36435    # deltaR = 73.4/82072 = 0.0008943367
#12    621 54306 20    2818.5   1.6115   0.04466 *  # deltaR = 2818.5/82072 = 0.0343418
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#### obtain coefficients and R-squared values between models
summary(lpa1)
lm.beta(lpa1)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(lpa2)
lm.beta(lpa2)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(lpa3)
lm.beta(lpa3)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(lpa4)
lm.beta(lpa4)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(lpa5)
lm.beta(lpa5)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(lpa6)
lm.beta(lpa6)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(lpa7)
lm.beta(lpa7)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(lpa8)
lm.beta(lpa8)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(lpa9)
lm.beta(lpa9)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(lpa10)
lm.beta(lpa10)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(lpa11)
lm.beta(lpa11)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 


#Visualize


ggplot(lec, aes(x=atq_appr, y=masq_lpa_total, color = lifeevent)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F)

ggplot(lec, aes(x=atq_appr, y=masq_lpa_total, color = timepoint)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F)

ggplot(lec, aes(x=atq_avoi, y=masq_lpa_total)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F)


ggplot(lec, aes(x=atq_appr, y=masq_lpa_total)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F)

# Control for other DV
lpaph0 <- lm(masq_lpa_total ~ 1, data=lec)  # to obtain Total SS
lpaph0 <- lm(masq_lpa_total ~ masq_dm_total, data=lec)  # dm on lpa
lpaph1 <- lm(masq_lpa_total ~ masq_dm_total + age, data=lec)  # effect of age 
lpaph2 <- lm(masq_lpa_total ~ masq_dm_total + timepoint + age, data=lec)  # main effect of covid warning onset (spring break divide)
lpaph3 <- lm(masq_lpa_total ~ masq_dm_total + timepoint + age + timepoint:age, data=lec)   # effect of interaction between spring break and age 
lpaph4 <- lm(masq_lpa_total ~ masq_dm_total + timepoint + age + timepoint:age + lifeevent, data=lec)   # life event
lpaph5 <- lm(masq_lpa_total ~ masq_dm_total + timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint, data=lec)   # effect of interaction between spring break and lifeevent
lpaph6 <- lm(masq_lpa_total ~ masq_dm_total + timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi, data=lec)   # avoidance
lpaph7 <- lm(masq_lpa_total ~ masq_dm_total + timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint, data=lec)   # effect of interaction between spring break and avoidance
lpaph8 <- lm(masq_lpa_total ~ masq_dm_total + timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr, data=lec)   # approach

# model comparison for anxious arousal after controlling for lpa
anova(lpaph0,lpaph1, lpaph2,lpaph3, lpaph4, lpaph5, lpaph6,lpaph7, lpaph8)
#Res.Df   RSS Df Sum of Sq       F    Pr(>F)    
#1    651 64221                                   
#2    650 64197  1      24.4  0.2932    0.5884    
#3    649 64037  1     159.6  1.9216    0.1662    
#4    648 64024  1      13.3  0.1599    0.6894    
#5    647 64008  1      15.5  0.1867    0.6658    
#6    646 64005  1       3.7  0.0444    0.8331    
#7    645 61688  1    2317.0 27.8941 1.756e-07 ***
#8    644 61627  1      60.4  0.7266    0.3943    
#9    643 53411  1    8216.4 98.9160 < 2.2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## Depressed Mood

# Build models
dm00 <- lm(masq_dm_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint + lifeevent:age + lifeevent:timepoint:age 
           + atq_avoi + atq_avoi:timepoint + atq_avoi:age + atq_avoi:lifeevent + atq_avoi:timepoint:age + atq_avoi:timepoint:lifeevent 
           + atq_avoi:lifeevent:age + atq_avoi:timepoint:age:lifeevent + atq_appr + atq_appr:timepoint + atq_appr:age + atq_appr:lifeevent
           + atq_appr:atq_avoi + atq_appr:timepoint:age + atq_appr:timepoint:lifeevent + atq_appr:timepoint:atq_avoi + atq_appr:lifeevent:age
           + atq_appr:lifeevent:atq_avoi + atq_appr:age:atq_avoi + atq_appr:timepoint:lifeevent:age + atq_appr:timepoint:lifeevent:atq_avoi 
           + atq_appr:age:lifeevent:atq_avoi + atq_appr:age:timepoint:atq_avoi + atq_appr:lifeevent:age:timepoint:atq_avoi, data=lec) # to obtain total R-squared to report to reader
dm0 <- lm(masq_dm_total ~ 1, data=lec)  # to obtain Total SS
dm1 <- lm(masq_dm_total ~ age, data=lec)  # effect of age 
dm2 <- lm(masq_dm_total ~ timepoint + age, data=lec)  # main effect of covid warning onset (spring break divide)
dm3 <- lm(masq_dm_total ~ timepoint + age + timepoint:age, data=lec)   # effect of interaction between spring break and age 
dm4 <- lm(masq_dm_total ~ timepoint + age + timepoint:age + lifeevent, data=lec)   # life event
dm5 <- lm(masq_dm_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint, data=lec)   # effect of interaction between spring break and lifeevent
dm6 <- lm(masq_dm_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi, data=lec)   # avoidance
dm7 <- lm(masq_dm_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint, data=lec)   # effect of interaction between spring break and avoidance
dm8 <- lm(masq_dm_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr, data=lec)   # approach
dm9 <- lm(masq_dm_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr + 
            atq_appr:timepoint, data=lec)   # effect of interaction between spring break and approach
dm10 <- lm(masq_dm_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr + 
             atq_appr:timepoint + atq_appr:lifeevent, data=lec)   # effect of interaction between lifeevent and approach (lower-order constituent step)
dm11 <- lm(masq_dm_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr + 
             atq_appr:timepoint + atq_appr:lifeevent + atq_appr:timepoint:lifeevent, data=lec)   # effect of interaction between spring break, lifeevent, and approach

# Total variance explained by all IVs of interest
summary(dm00)

# model comparison 
anova(dm0,dm1, dm2, dm3, dm4, dm5, dm6, dm7, dm8, dm9, dm10, dm11, dm00)  
#Res.Df   RSS Df Sum of Sq        F    Pr(>F)    
#0     652 24834                                    
#1     651 24833  1       0.3   0.0095   0.92208    # deltaR = 0.3/24834 = 1.208021e-05
#2     650 22505  1    2328.2  87.2509< 2.2e-16 ***# deltaR = 2328.2/24834 = 0.0937505
#3     649 22472  1      33.6   1.2578   0.26110    # deltaR = 33.6/24834 = 0.001352984
#4     648 22327  1     144.7  5.4245   0.01981 *  # deltaR = 144.7/24834 = 0.005826689
#5     647 22327  1       0.0   0.0014   0.97060    # deltaR = 0.0/24834 = 0.0
#6     646 18685  1    3641.7 136.4750 < 2.2e-16 ***# deltaR = 3641.7/24834 = 0.1466417
#7     645 18634  1      51.4   1.9263   0.16442    # deltaR = 51.4/24834 = 0.002069743
#8     644 17255  1    1378.7  51.6660 1.594e-12 ***# deltaR = 1378.7/24834 = 0.05551663
#9     643 17133  1     122.2   4.5810   0.03220 *   # deltaR = 122.2/24834 = 0.004920673
#10    642 17070  1      63.2   2.3674   0.12329    # deltaR = 63.2/24834 = 0.002544898
#11    641 17005  1      64.2   2.4043   0.12042    # deltaR = 64.2/24834 = 0.002585165
#12    621 16571 20     434.6   0.8143   0.69732    # deltaR = 434.6/24834 = 0.0175002
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#### obtain coefficients and R-squared values between models
summary(dm1)
lm.beta(dm1)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(dm2)
lm.beta(dm2)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(dm3)
lm.beta(dm3)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(dm4)
lm.beta(dm4)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(dm5)
lm.beta(dm5)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(dm6)
lm.beta(dm6)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(dm7)
lm.beta(dm7)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(dm8)
lm.beta(dm8)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(dm9)
lm.beta(dm9)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(dm10)
lm.beta(dm10)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 
summary(dm11)
lm.beta(dm11)
# coef: Multiple R-squared: , F-statistic:,  p-value: , ∆R = 


#Visualize


ggplot(lec, aes(x=age, y=masq_dm_total, color = timepoint)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F)

ggplot(lec, aes(x=age, y=masq_dm_total, color = lifeevent)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F)

ggplot(lec, aes(x=atq_appr, y=masq_dm_total, color = timepoint)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F)

ggplot(lec, aes(x=atq_avoi, y=masq_dm_total)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F)


ggplot(lec, aes(x=atq_appr, y=masq_dm_total)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F)

# To control for other correlated DVs
dmph0 <- lm(masq_dm_total ~ 1, data=lec)  # to obtain Total SS
dmph2 <- lm(masq_dm_total ~ rrq_rum_total, data=lec)  # control for rumination
dmph3 <- lm(masq_dm_total ~ rrq_rum_total + age, data=lec)  # effect of age 
dmph4 <- lm(masq_dm_total ~ rrq_rum_total + timepoint + age, data=lec)  # main effect of covid warning onset (spring break divide)
dmph5 <- lm(masq_dm_total ~ rrq_rum_total + timepoint + age + timepoint:age, data=lec)   # effect of interaction between spring break and age 
dmph6 <- lm(masq_dm_total ~ rrq_rum_total + timepoint + age + timepoint:age + lifeevent, data=lec)   # life event
dmph7 <- lm(masq_dm_total ~ rrq_rum_total + timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint, data=lec)   # effect of interaction between spring break and lifeevent
dmph8 <- lm(masq_dm_total ~ rrq_rum_total + timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi, data=lec)   # avoidance
dmph9 <- lm(masq_dm_total ~ rrq_rum_total + timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint, data=lec)   # effect of interaction between spring break and avoidance
dmph10 <- lm(masq_dm_total ~ rrq_rum_total + timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr, data=lec)   # approach
dmph11<- lm(masq_dm_total ~ rrq_rum_total + timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint +  atq_avoi + atq_avoi:timepoint + atq_appr + 
              atq_appr:timepoint, data=lec)   # effect of interaction between spring break and approach

# model comparison controlling for DVs
anova(dmph0,dmph2, dmph3, dmph4, dmph5, dmph6, dmph7, dmph8, dmph9, dmph10, dmph11)  
# just controlling for lpa
#Res.Df   RSS Df Sum of Sq        F    Pr(>F)    
#1     652 24834                                    
#2     651 19432  1    5401.5 218.0580 < 2.2e-16 ***
#3     650 19429  1       2.8   0.1113 0.7387392    
#4     649 18185  1    1244.2  50.2270 3.617e-12 ***
#5     648 18170  1      14.7   0.5930 0.4415325    
#6     647 18093  1      77.9   3.1435 0.0767056 .  
#7     646 18092  1       0.1   0.0034 0.9534095    
#8     645 16408  1    1684.5  68.0029 9.236e-16 ***
#9     644 16349  1      59.2   2.3892 0.1226655    
#10    643 16003  1     346.1  13.9725 0.0002021 ***
#11    642 15903  1     100.0   4.0351 0.0449817 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# controlling for anxious arousal
#   Res.Df   RSS Df Sum of Sq        F    Pr(>F) 
#1     652 24834                                      
#2     651 17768  1    7065.5 351.8374 < 2.2e-16 ***
#3     650 17743  1      24.6   1.2274 0.2683270    
#4     649 15407  1    2336.1 116.3309 < 2.2e-16 ***
#5     648 15392  1      15.6   0.7780 0.3780845    
#6     647 15242  1     149.9   7.4625 0.0064728 ** 
#7     646 15218  1      23.5   1.1688 0.2800535    
#8     645 13763  1    1455.0  72.4526 < 2.2e-16 ***
#9     644 13730  1      33.0   1.6430 0.2003778    
#10    643 13149  1     581.2  28.9430 1.046e-07 ***
#11    642 12892  1     256.6  12.7765 0.0003773 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# controlling for rumination
#   Res.Df   RSS Df Sum of Sq        F    Pr(>F) 
#1     652 24834                                      
#2     651 17768  1    7065.5 351.8374 < 2.2e-16 ***
#3     650 17743  1      24.6   1.2274 0.2683270    
#4     649 15407  1    2336.1 116.3309 < 2.2e-16 ***
#5     648 15392  1      15.6   0.7780 0.3780845    
#6     647 15242  1     149.9   7.4625 0.0064728 ** 
#7     646 15218  1      23.5   1.1688 0.2800535    
#8     645 13763  1    1455.0  72.4526 < 2.2e-16 ***
#9     644 13730  1      33.0   1.6430 0.2003778    
#10    643 13149  1     581.2  28.9430 1.046e-07 ***
#11    642 12892  1     256.6  12.7765 0.0003773 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Assess Correlation between DVs
Y <- cbind(lec$pswq_total, lec$rrq_rum_total,lec$rrq_refl_total, lec$masq_aa_total,lec$masq_lpa_total, lec$masq_dm_total)
cor(Y, method = c("pearson"))
Y




##############################################################
##           DESCRIPTIVE STATISTICS FOR TABLES              ##
##############################################################

tables<-freezing_raw_d_age[,c("timepoint", "age_trend", "pswq_total", "rrq_refl_total", "rrq_rum_total", "masq_aa_total", "masq_lpa_total", "masq_dm_total", "atq_avoi", "atq_appr")]
tables2<-lec[,c("lifeevent", "timepoint", "age_trend", "pswq_total", "rrq_refl_total", "rrq_rum_total", "masq_aa_total", "masq_lpa_total", "masq_dm_total", "atq_avoi", "atq_appr")]


###
#table 1 - self-report desc stats
###

# rename scores
tables$Worry <- tables$pswq_total
tables$Rumination <- tables$rrq_rum_total
tables$Reflection <- tables$rrq_refl_total
tables$Anxious_Arousal <- tables$masq_aa_total
tables$Low_Positive_Affect <- tables$masq_lpa_total
tables$Depressed_Mood <- tables$masq_dm_total
tables$Avoidance <- tables$atq_avoi
tables$Approach <- tables$atq_appr
tables$Group <- tables$timepoint
tables$Age <- tables$age_trend

# rid old columns
tables <- tables[,c(11:20)]

tables_gpre<-tables %>%
  dplyr::filter(tables$Group == "pre-spring break")
tables_gpre<-tables_gpre[,c(1:8)]

t1<- describe(tables_gpre)
formattable(t1)

tables_gpost<-tables %>%
  dplyr::filter(tables$Group == "post-spring break")
tables_gpost<-tables_gpost[,c(1:8)]

t2<- describe(tables_gpost)
formattable(t2)

tables_o<-tables %>%
  dplyr::filter(tables$Age == "≥20")
tables_o<-tables_o[,c(1:8)]

t3<- describe(tables_o)
formattable(t3)

tables_y<-tables %>%
  dplyr::filter(tables$Age == "<20")
tables_y<-tables_y[,c(1:8)]

t4<- describe(tables_y)
formattable(t4)

tables_yes<-tables2 %>%
  dplyr::filter(tables2$lifeevent == "yes")
tables_yes<-tables_yes[,c(4:11)]

t5<- describe(tables_yes)
formattable(t5)


tables_no <-tables2 %>%
  dplyr::filter(tables2$lifeevent == "no")
tables_no<-tables_no[,c(4:11)]

t6<- describe(tables_no)
formattable(t6)
##################################################################################################
## Let's check correlation between anxious arousal, apprehension, rumination & anhedonic depression scores

pairs.panels(freezing_raw_d_age[,c("masq_lpa_total", "masq_dm_total", "pswq_total", "masq_aa_total", "rrq_total")], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

## Given that some of our data is not normally distributed, let's try spearman

pairs.panels(freezing_raw_d_age[,c("masq_lpa_total", "masq_dm_total", "pswq_total", "masq_aa_total", "rrq_total")], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)


## Let's check continuous covariate btwn anxious arousal, apprehension & anhedonic depression scores

lm_test_1<-lm(masq_aa_total ~ rrq_total + pswq_total, data = freezing_raw_d_age)
summary(lm_test_1)

install.packages ("lm.beta")
library(lm.beta)

lm.beta(lm_test_1)
getDeltaRsquare(lm_test_1)
# There is a predictive effect of rumination and worry on anxious arousal, however, the adjusted r-squared is quite small .06 -> so only 6% of change in our variable could be predicted by rum/worry

lm_test_2<-lm(masq_dm_total ~ pswq_total + rrq_total, data = freezing_raw_d_age)
summary(lm_test_2)
lm.beta(lm_test_2)
getDeltaRsquare(lm_test_2)
# There is a predictive effect of rumination and worry on depressed mood, with an adjusted r-squared = .14, still low but both have an effect

lm_test_3<-lm(masq_lpa_total ~ rrq_total + pswq_total, data = freezing_raw_d_age)
summary(lm_test_3)
lm.beta(lm_test_3)
getDeltaRsquare(lm_test_3)
# There is a predictive effect of only worry on low positive affect, with an adjusted r-squared = .10


lm_test_4<-lm(rrq_total ~ pswq_total, data = freezing_raw_d_age)
summary(lm_test_4)
lm.beta(lm_test_4)

lm_test_5<-lm(pswq_total ~ rrq_total, data = freezing_raw_d_age)
summary(lm_test_5)

cc

# significant predictive reciprocated relationship between worry and rumination, r-squared .26 


cor(freezing_raw_d_age[, c("rrq_total", "pswq_total", "masq_aa_total", "masq_lpa_total", "masq_dm_total")], method = "pearson")

########
# ATQ  #
########


psych::describe(freezing_raw_d_age$atq_appr)
psych::describe(freezing_raw_d_age$atq_avoi)

quantile(freezing_raw_d_age$atq_appr)
quantile(freezing_raw_d_age$atq_avoi)

freezing_raw_d_age<-freezing_raw_d_age %>%
  mutate(temperament=case_when(
    atq_avoi %in% 28:42 & atq_appr %in% 6:31 ~ "high avoi",
    atq_appr %in% 32:42 & atq_avoi %in% 6:27 ~ "high appr",
    atq_avoi %in% 28:42 & atq_appr %in% 32:42 ~ "high dual",
    atq_avoi %in% 6:27 & atq_appr %in% 6:31 ~ "low dual"
  ))

count(freezing_raw_d_age$temperament)

temp_group<- lm(cbind(pswq_total, rrq_refl_total, rrq_rum_total, masq_aa_total, masq_dm_total, masq_lpa_total) ~ timepoint*temperament, freezing_raw_d_age)
temp_man<-Manova(temp_group, test.statistic = "Pillai")
temp_man

gd3 <- freezing_raw_d_age %>%
  gather(key = "variable", value = "value", pswq_total, rrq_total, masq_aa_total, masq_lpa_total, masq_dm_total) %>%
  group_by(variable)

gd3 %>% anova_test(value ~ temperament*timepoint)


pwctemp <- freezing_raw_d_age %>%
  gather(key = "variable", value = "value", pswq_total, rrq_total, masq_aa_total, masq_lpa_total, masq_dm_total) %>%
  group_by(variable) %>%
  games_howell_test(value ~ temperament)
pwctemp



count(freezing_raw_d_age)
cor1<-cor(freezing_raw_d_age[, c("atq_appr", "atq_avoi", "rrq_total", "pswq_total", "masq_aa_total", "masq_lpa_total", "masq_dm_total")], method = "pearson")
ggcorrplot(cor1, hc.order = TRUE, type = "lower",
           lab = TRUE)

pairs.panels(freezing_raw_d_age[,c("atq_appr", "atq_avoi", "rrq_total", "pswq_total", "masq_aa_total", "masq_lpa_total", "masq_dm_total")], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

mlm1<-lm(cbind(pswq_total, rrq_refl_total, rrq_rum_total, masq_aa_total, masq_lpa_total, masq_dm_total) ~ atq_appr+atq_avoi+timepoint , data = freezing_raw_d_age)
summary(mlm1)


head(resid(mlm1))
head(fitted(mlm1))
coef(mlm1)
vcov(mlm1)



x1<-lm(pswq_total ~ atq_avoi + atq_appr, data = freezing_raw_d_age)
getDeltaRsquare(x1)
coef(x1)
x2<-lm(rrq_refl_total ~ atq_avoi + atq_appr, data = freezing_raw_d_age)
getDeltaRsquare(x2)

coef(x2)
x6<-lm(rrq_rum_total ~ atq_avoi + atq_appr, data = freezing_raw_d_age)
getDeltaRsquare(x6)
x3<-lm(masq_lpa_total ~ atq_avoi + atq_appr, data = freezing_raw_d_age)
getDeltaRsquare(x3)
x4<-lm(masq_dm_total ~ atq_avoi + atq_appr, data = freezing_raw_d_age)
getDeltaRsquare(x4)
x5<-lm(masq_aa_total ~ atq_avoi + atq_appr, data = freezing_raw_d_age)
getDeltaRsquare(x5)


mlm2<-lm(cbind(masq_aa_total, masq_lpa_total, masq_dm_total) ~ pswq_total + rrq_total, data = freezing_raw_d_age)
summary(mlm2)

lm.deltaR2(mlm1)
head(resid(mlm1))
head(fitted(mlm1))
coef(mlm2)
vcov(mlm1)

worry<-freezing_raw_d_age[,c(27:42)]
cronbach(worry)

rumination<-freezing_raw_d_age[,c(44:67)]
cronbach(rumination)

test<-lm(pswq_total ~ atq_avoi + atq_appr, data = freezing_raw_d_age)
summary(test)
lm.beta(test)

Anova(mlm1)

grouped.data.mlm <- freezing_raw_d_age %>%
  gather(key = "variable", value = "value", pswq_total, rrq_refl_total, rrq_rum_total, masq_aa_total, masq_lpa_total, masq_dm_total) %>%
  group_by(variable)

grouped.data.mlm %>% anova_test(value ~ atq_avoi*atq_appr)

##############################################################################
# Tests that follow are exploratory and unrelated to Pandemic Trends Paper   #
##############################################################################



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


#visualize pswq
ggplot(freezing_demo, aes(x=con_date, y=pswq_total, color=RACE)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F) +
  labs(x= "date of completion", y= "pswq total") +
  labs(color= "race")

ggplot(freezing_demo, aes(x=con_date, y=pswq_total, color=GENDER)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm) +
  labs(x= "date of completion", y= "pswq total") +
  labs(color= "GENDER")

ggplot(freezing_demo, aes(x=con_date, y=pswq_total, color=CLASS.SELF)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F) +
  labs(x= "date of completion", y= "pswq total") +
  labs(color= "SES")

ggplot(freezing_demo, aes(x=con_date, y=pswq_total, color=ETHNICITY)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F) +
  labs(x= "date of completion", y= "pswq total") +
  labs(color= "Ethnicity")

ggplot(freezing_raw_d_age, aes(x=con_date, y=pswq_total, color=age_trend)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F) +
  labs(x= "date of completion", y= "pswq total") +
  labs(color= "Age")




#########################################
###  Visualize Depression & Anxiety   ###
#########################################

###########
# MASQ AD #
###########

## Plot Depression Scores by Duplicated vs Distinct
ggplot(freezing_raw_d_age, aes(x=con_date, y=masq_ad_total, color = duplicated)) +
  geom_point(shape=10) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)


## Plot Depression scores by Age Trend
ggplot(freezing_raw_d_age, aes(x=con_date, y=masq_ad_total, color=age_trend)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

## Create Linear Model
fit<-lm(freezing_raw_d_age$con_date~freezing_raw_d_age$masq_ad_total)
plot(fit)
anova(fit)
coefficients(fit)

## Let's also check for patterns in Anhedonic Depression subsections, Low Positive Affect & Depressive Mood


##Low Positive Affect

## Plot Low Positive Affect Scores by Duplicated vs Distinct
ggplot(freezing_raw_d_age, aes(x=con_date, y=masq_lpa_total, color = duplicated)) +
  geom_point(shape=10) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)


## Plot Low Positive Affect scores by Age Trend
ggplot(freezing_raw_d_age, aes(x=con_date, y=masq_lpa_total, color=age_trend)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

## Plot Low Positive Affect scores by Timepoint
ggplot(freezing_raw_d_age, aes(x=con_date, y=masq_lpa_total, color=timepoint)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

## Depressive Mood

## Plot Depressive Mood Scores by Duplicated vs Distinct
ggplot(freezing_raw_d_age, aes(x=con_date, y=masq_dm_total, color = duplicated)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)


## Plot Depressive Mood scores by Age Trend
ggplot(freezing_raw_d_age, aes(x=con_date, y=masq_dm_total, color=age_trend)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

## Plot Depressive Mood scores by Timepoint
ggplot(freezing_raw_d_age, aes(x=con_date, y=masq_dm_total, color=timepoint)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

###########
# MASQ AA #
###########

## Plot Arousal by Duplicated vs Distinct
ggplot(freezing_raw_d_age, aes(x=con_date, y=masq_aa_total, color = duplicated)) +
  geom_point(shape=10) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)


## Plot Arousal by Age Trend
ggplot(freezing_raw_d_age, aes(x=con_date, y=masq_aa_total, color=age_trend)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

###########
#  PSWQ   #
###########

## Plot Worry Scores by Duplicated vs Distinct
ggplot(freezing_raw_d_age, aes(x=con_date, y=pswq_total, color = duplicated)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

## Plot Worry scores by Age
ggplot(freezing_raw_d_age, aes(x=con_date, y=pswq_total)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm) +
  labs(x= "date of completion", y= "pswq total")

## Plot Worry scores by Age Trend
ggplot(freezing_raw_d_age, aes(x=con_date, y=pswq_total, color=age_trend)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm) +
  labs(x= "date of completion", y= "pswq total") +
  labs(color= "Age")

## Plot Worry scores by Timepoint
ggplot(freezing_raw_d_age, aes(x=con_date, y=pswq_total, color=timepoint)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm) +
  labs(x= "date of completion", y= "pswq total") +
  labs(color= "Age")

###########
#   ASI   #
###########

## Plot Anxiety Sensitivity Scores by Duplicated vs Distinct
ggplot(freezing_raw_d_age, aes(x=con_date, y=asi_total, color = duplicated)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)


## Plot Anxiety Sensitivity scores by Age Trend
ggplot(freezing_raw_d_age, aes(x=con_date, y=asi_total, color=age_trend)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

###########
#  BRIEF  #
###########

## First, create subtotals for various sections of the brief (working memory, inhibit, shift, emotional control)
freezing_raw_d_age$brief_wm_total<- rowSums(freezing_raw_d_age[, c('brief_04', 'brief_11', 'brief_17', 'brief_26', 'brief_35', 'brief_46', 'brief_56', 'brief_68')])
freezing_raw_d_age$brief_inh_total<- rowSums(freezing_raw_d_age[,c('brief_05', 'brief_16', 'brief_29','brief_36', 'brief_43', 'brief_55','brief_58','brief_73')])
freezing_raw_d_age$brief_shft_total<- rowSums(freezing_raw_d_age[,c('brief_08', 'brief_22', 'brief_32','brief_44', 'brief_61', 'brief_67')])
freezing_raw_d_age$brief_emctrl_total<- rowSums(freezing_raw_d_age[,c('brief_01', 'brief_12', 'brief_19','brief_28', 'brief_33', 'brief_42','brief_51','brief_57','brief_69','brief_72')])

## Working Memory

## Plot Working Memory scores by Duplicated
ggplot(freezing_raw_d_age, aes(x=con_date, y=brief_wm_total, color=duplicated)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)


## Plot Working Memory scores by Age Trend
ggplot(freezing_raw_d_age, aes(x=con_date, y=brief_wm_total, color=age_trend)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)


## Inhibit

## Plot Inhibit scores by Duplicated
ggplot(freezing_raw_d_age, aes(x=con_date, y=brief_inh_total, color=duplicated)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

ggplot(freezing_raw_d_age, aes(x=brief_inh_total, color=duplicated)) + 
  geom_histogram(binwidth=5, fill="white", position = "dodge")

## Plot Inhibit scores by Age Trend
ggplot(freezing_raw_d_age, aes(x=con_date, y=brief_inh_total, color=age_trend)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

ggplot(freezing_raw_d_age, aes(x=brief_inh_total, color=age_trend)) + 
  geom_histogram(binwidth=5, fill="white", position = "dodge")

## Shift

## Plot Shift scores by Duplicated
ggplot(freezing_raw_d_age, aes(x=con_date, y=brief_shft_total, color=duplicated)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

ggplot(freezing_raw_d_age, aes(x=brief_shft_total, color=duplicated)) + 
  geom_histogram(binwidth=5, fill="white", position = "dodge")


## Plot Shift scores by Age Trend
ggplot(freezing_raw_d_age, aes(x=con_date, y=brief_shft_total, color=age_trend)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

ggplot(freezing_raw_d_age, aes(x=brief_shft_total, color=age_trend)) + 
  geom_histogram(binwidth=5, fill="white", position = "dodge")


## Emotional Control

## Plot Shift scores by Duplicated
ggplot(freezing_raw_d_age, aes(x=con_date, y=brief_emctrl_total, color=duplicated)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

ggplot(freezing_raw_d_age, aes(x=brief_emctrl_total, color=duplicated)) + 
  geom_histogram(binwidth=5, fill="white", position = "dodge")


## Plot Shift scores by Age Trend
ggplot(freezing_raw_d_age, aes(x=con_date, y=brief_emctrl_total, color=age_trend)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm)

ggplot(freezing_raw_d_age, aes(x=brief_shft_total, color=age_trend)) + 
  geom_histogram(binwidth=5, fill="white", position = "dodge")


t.test(pswq_total ~ lifeevent, data = lec, alternative = "less", var.equal = FALSE)
#p < .05
t.test(masq_aa_total ~ lifeevent, data = lec, alternative = "less", var.equal = FALSE)
#p > .05
t.test(masq_ad_total ~ lifeevent, data = lec, alternative = "less", var.equal = FALSE)
#p < .05
t.test(masq_lpa_total ~ lifeevent, data = lec, alternative = "less", var.equal = FALSE)
#p > .05
t.test(masq_dm_total ~ lifeevent, data = lec, alternative = "less", var.equal = FALSE)
#p < .05
t.test(rrq_total ~ lifeevent, data = lec, alternative = "less", var.equal = FALSE)
#p < .05


#Now check differences by experience
lec_ND<- lec %>%
  dplyr::filter(lec_1 == 1)

lec_Fire<- lec %>%
  dplyr::filter(lec_2 == 1)

lec_TransAcc<- lec %>%
  dplyr::filter(lec_3 == 1)

lec_SeriousAcc<- lec %>%
  dplyr::filter(lec_4 == 1)

lec_ToxicSub<- lec %>%
  dplyr::filter(lec_5 == 1)

lec_PhysicalAssault<- lec %>%
  dplyr::filter(lec_6 == 1)

lec_ArmedAssault<- lec %>%
  dplyr::filter(lec_7 == 1)

lec_SexualAssault<- lec %>%
  dplyr::filter(lec_8 == 1)

lec_UnwantedSE<- lec %>%
  dplyr::filter(lec_9 == 1)

lec_Combat<- lec %>%
  dplyr::filter(lec_10 == 1)

lec_Captivity<- lec %>%
  dplyr::filter(lec_11 == 1)

lec_LifeThreatIllness<- lec %>%
  dplyr::filter(lec_12 == 1)

lec_SevereHumanSuff<- lec %>%
  dplyr::filter(lec_13 == 1)

lec_SuddenViolentDeath<- lec %>%
  dplyr::filter(lec_14 == 1)

lec_AccidentalDeath<- lec %>%
  dplyr::filter(lec_15 == 1)

lec_CausedInjury<- lec %>%
  dplyr::filter(lec_16 == 1)
psych::describe(lec_CausedInjury$pswq_total)


lec_OtherStress<- lec %>%
  dplyr::filter(lec_17 == 1)
psych::describe(lec_OtherStress$pswq_total)

t.test(lec_UnwantedSE$pswq_total, lec_SexualAssault$pswq_total, alternative = "two.sided", var.equal = FALSE)

#Visualize
ggplot(freezing_raw_d_age, aes(x=lec_1)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Natural Disaster")

ggplot(freezing_raw_d_age, aes(x=lec_2)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Fire or Explosion")

ggplot(freezing_raw_d_age, aes(x=lec_3)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Transportation Accident")

ggplot(freezing_raw_d_age, aes(x=lec_4)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Serious Accident at Work, Home or Rec Activity")

ggplot(freezing_raw_d_age, aes(x=lec_5)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Exposure to Toxic Substance")

ggplot(freezing_raw_d_age, aes(x=lec_6)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Physical Assault")

ggplot(freezing_raw_d_age, aes(x=lec_7)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Assault with a Weapon")

ggplot(freezing_raw_d_age, aes(x=lec_8)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Sexual Assault")

ggplot(freezing_raw_d_age, aes(x=lec_9)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Other Unwanted or Uncomfotable Sexual Exp")

ggplot(freezing_raw_d_age, aes(x=lec_10)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Combat or Exposure to War")

ggplot(freezing_raw_d_age, aes(x=lec_11)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Captivity")

ggplot(freezing_raw_d_age, aes(x=lec_12)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Life-threatening illness or injury")

ggplot(freezing_raw_d_age, aes(x=lec_13)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Severe Human Suffering")

ggplot(freezing_raw_d_age, aes(x=lec_14)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Sudden Violent Death")

ggplot(freezing_raw_d_age, aes(x=lec_15)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Sudden Accidental Death")

ggplot(freezing_raw_d_age, aes(x=lec_16)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Causing Serious Injury or Harm")

ggplot(freezing_raw_d_age, aes(x=lec_17)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Any Other")

###########
#   ATQ   #
###########



## Calculate Means for each group

atq_avoi_mu <- ddply(freezing_raw_d_age, "duplicated", summarise, grp.mean=mean(atq_avoi)) 
atq_appr_mu <- ddply(freezing_raw_d_age, "duplicated", summarise, grp.mean=mean(atq_appr)) 



freezing_demo$atq_appr<-rowSums(freezing_demo[, c(323,325,326,329,331,332)])
freezing_demo$atq_avoi<-rowSums(freezing_demo[, c(322,324,327,328,330,333)])

freezing_demo_twogender$atq_appr<-rowSums(freezing_demo_twogender[, c(323,325,326,329,331,332)])
freezing_demo_twogender$atq_avoi<-rowSums(freezing_demo_twogender[, c(322,324,327,328,330,333)])

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

## Calculate totals of endorsements (how many times did each participant say yes to a freezing context?)

afq_endorsement$endorsed<-rowSums(afq_endorsement[,c(1,3,5,7,9,11,13,15,17,19,21,23)])

psych::describe((afq_endorsement$endorsed))

## make dataset for those that only endorsed one event type

afq_1endorse<- afq_endorsement %>%
  dplyr::filter(afq_endorsement$endorsed == 1)

## Visualize single endorsement


## Visualize endorsed totals
count(afq_1endorse, vars="afq_1")
count(afq_1endorse, vars="afq_3")
count(afq_1endorse, vars="afq_5")
count(afq_1endorse, vars="afq_7")
count(afq_1endorse, vars="afq_9")
count(afq_1endorse, vars="afq_11")
count(afq_1endorse, vars="afq_13")
count(afq_1endorse, vars="afq_15")



ggplot(afq_endorsement, aes(x=endorsed)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Contexts endorsed")

count(afq_endorsement, vars="endorsed")

## Calculate time of endorsements (when did each participant experience a freezing context?)

afq_endorsement$afq_2[is.na(afq_endorsement$afq_2)] = 0 
afq_endorsement$afq_4[is.na(afq_endorsement$afq_4)] = 0 
afq_endorsement$afq_6[is.na(afq_endorsement$afq_6)] = 0 
afq_endorsement$afq_8[is.na(afq_endorsement$afq_8)] = 0 
afq_endorsement$afq_10[is.na(afq_endorsement$afq_10)] = 0 
afq_endorsement$afq_12[is.na(afq_endorsement$afq_12)] = 0 
afq_endorsement$afq_14[is.na(afq_endorsement$afq_14)] = 0 
afq_endorsement$afq_16[is.na(afq_endorsement$afq_16)] = 0 
afq_endorsement$afq_18[is.na(afq_endorsement$afq_18)] = 0 
afq_endorsement$afq_20[is.na(afq_endorsement$afq_20)] = 0 
afq_endorsement$afq_22[is.na(afq_endorsement$afq_22)] = 0 
afq_endorsement$afq_24[is.na(afq_endorsement$afq_24)] = 0 

afq_endorsement$time<-rowSums(afq_endorsement[,c(2,4,6,8,10,12,14,16,18,20,22,24)])
afq_endorsement$avgtime<-afq_endorsement$time/afq_endorsement$endorsed

afq_endorsement<-afq_endorsement[,c(1:25,27,26)]

afq_endorsement$avgtime[is.nan(afq_endorsement$avgtime)] = 0

typeof(afq_endorsement$avgtime)
as.numeric(afq_endorsement$avgtime)
psych::describe((afq_endorsement$avgtime))

ggplot(afq_endorsement, aes(x=avgtime) + 
         geom_histogram(color="black", fill="white") +
         labs(x = "Avg length of time since freeze"))


## Visualize Endorsement Section
ggplot(afq_endorsement, aes(x=afq_1)) + 
  geom_histogram(color="black", fill="white") +
  labs(x = "Couldn't Move or Think")

ggplot(afq_endorsement, aes(x=afq_3)) + 
  geom_histogram(color="black", fill="white") +
  labs (x = "Violent Situation")


## Extract Freezing Item Reponses ##
afq<-freezing_raw_d_age %>%
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

###########################################################################################
## Visualize Correlations between AFQ items & MASQ/PSWQ- each item to total factor score ##
###########################################################################################

## Add AFQ Total, PSWQ Total and MASQ Totals to the AFQ dataset

## Extract Freezing Item Reponses ##
afq$afq_total<-rowSums(afq[,c(70:72)])
afq$pswq_total<-freezing_raw_d_age$pswq_total
afq$masq_aa_total<-freezing_raw_d_age$masq_aa_total
afq$masq_ad_total<-freezing_raw_d_age$masq_ad_total

# Remove outliers of afqs in AFQ set
#psych::describe(afq$afq_total)
#afq_no_outlier<-afq %>%
# dplyr::filter(afq_total <= 293)

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

x<-afqpure_cor_table$pswq_cor
y.y<-afqpure_cor_table$masq_aa_cor
z.z<-afqpure_cor_table$pure_cor

scatter3d(x = afqpure_cor_table$pswq_cor, y = afqpure_cor_table$masq_aa_cor, z = afqpure_cor_table$pure_cor, surface=FALSE, xlab = "PSWQ", ylab = "MASQ AA",
          zlab = "AFQ Pure Total", labels = TRUE)

scatter3D(x, y.y, z.z, phi = 0, bty = "g",  type = "h", 
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
print(efa.obl.3, cut=.6, sort = T)

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
afq$masq_ad_total<-freezing_raw_d$masq_ad_total
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

text3D(x, y, z,  labels = rownames(afq_kmc),add = TRUE, colkey = FALSE, cex =  0.5)

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

################################################################
####### Attempt 2 for kmc, removing some items a priori ########
################################################################

afq_dd_2<-freezing_raw_d %>%
  dplyr::select(afqs_1:afqs_70)
#Cut the items we don't want
#Cut 25, 26, 27
afq_dd_2<- afq_dd_2[,-c(25:27)]
afq_dd_2$afqs_44 <- NULL
afq_dd_2$afqs_46 <- NULL
afq_dd_2$afqs_47 <- NULL
afq_dd_2$afqs_49 <- NULL
afq_dd_2$afqs_52 <- NULL

##Add the new total columns 
afq_dd_2$afq_cog_total<-rowSums(afq_dd_2[,(1:25)])
afq_dd_2$afq_phys_total<-rowSums(afq_dd_2[,(26:49)])
afq_dd_2$afq_cogandphys_total<-rowSums(afq_dd_2[,c(1:49)])

##Add the totals from the MASQ and the PSWQ
afq_dd_2$pswq_total<-freezing_raw_d$pswq_total
afq_dd_2$masq_aa_total<-freezing_raw_d$masq_aa_total

## Generate list of pearson coefficients between items and totals ##
## make correlation matrices objects

afq_1_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_1", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_2_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_2", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_3_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_3", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_4_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_4", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_5_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_5", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_6_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_6", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_7_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_7", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_8_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_8", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_9_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_9", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_10_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_10", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_11_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_11", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_12_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_12", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_13_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_13", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_14_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_14", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_15_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_15", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_16_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_16", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_17_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_17", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_18_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_18", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_19_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_19", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_20_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_20", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_21_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_21", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_22_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_22", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_23_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_23", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_24_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_24", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_28_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_28", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_30_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_30", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_31_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_31", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_32_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_32", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_33_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_33", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_34_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_34", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_35_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_35", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_36_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_36", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_37_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_37", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_38_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_38", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_39_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_39", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_40_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_40", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_41_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_41", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_42_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_42", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_43_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_43", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_45_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_45", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_48_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_48", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_50_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_50", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_51_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_51", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_53_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_53", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_54_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_54", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_55_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_55", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_56_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_56", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_57_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_57", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_58_dd_cor<-as.data.frame(cor(afq_dd_2[, c("afqs_58", "afq_cogandphys_total", "afq_cog_total", "afq_phys_total","pswq_total", "masq_aa_total")], method = "pearson"))


## Remove Item Colum
afq_1_dd_cor<-afq_1_dd_cor[,-1]
afq_2_dd_cor<-afq_2_dd_cor[,-1]
afq_3_dd_cor<-afq_3_dd_cor[,-1]
afq_4_dd_cor<-afq_4_dd_cor[,-1]
afq_5_dd_cor<-afq_5_dd_cor[,-1]
afq_6_dd_cor<-afq_6_dd_cor[,-1]
afq_7_dd_cor<-afq_7_dd_cor[,-1]
afq_8_dd_cor<-afq_8_dd_cor[,-1]
afq_9_dd_cor<-afq_9_dd_cor[,-1]
afq_10_dd_cor<-afq_10_dd_cor[,-1]
afq_11_dd_cor<-afq_11_dd_cor[,-1]
afq_12_dd_cor<-afq_12_dd_cor[,-1]
afq_13_dd_cor<-afq_13_dd_cor[,-1]
afq_14_dd_cor<-afq_14_dd_cor[,-1]
afq_15_dd_cor<-afq_15_dd_cor[,-1]
afq_16_dd_cor<-afq_16_dd_cor[,-1]
afq_17_dd_cor<-afq_17_dd_cor[,-1]
afq_18_dd_cor<-afq_18_dd_cor[,-1]
afq_19_dd_cor<-afq_19_dd_cor[,-1]
afq_20_dd_cor<-afq_20_dd_cor[,-1]
afq_21_dd_cor<-afq_21_dd_cor[,-1]
afq_22_dd_cor<-afq_22_dd_cor[,-1]
afq_23_dd_cor<-afq_23_dd_cor[,-1]
afq_24_dd_cor<-afq_24_dd_cor[,-1]
afq_28_dd_cor<-afq_28_dd_cor[,-1]
afq_30_dd_cor<-afq_30_dd_cor[,-1]
afq_31_dd_cor<-afq_31_dd_cor[,-1]
afq_32_dd_cor<-afq_32_dd_cor[,-1]
afq_33_dd_cor<-afq_33_dd_cor[,-1]
afq_34_dd_cor<-afq_34_dd_cor[,-1]
afq_35_dd_cor<-afq_35_dd_cor[,-1]
afq_36_dd_cor<-afq_36_dd_cor[,-1]
afq_37_dd_cor<-afq_37_dd_cor[,-1]
afq_38_dd_cor<-afq_38_dd_cor[,-1]
afq_39_dd_cor<-afq_39_dd_cor[,-1]
afq_40_dd_cor<-afq_40_dd_cor[,-1]
afq_41_dd_cor<-afq_41_dd_cor[,-1]
afq_42_dd_cor<-afq_42_dd_cor[,-1]
afq_43_dd_cor<-afq_43_dd_cor[,-1]
afq_45_dd_cor<-afq_45_dd_cor[,-1]
afq_48_dd_cor<-afq_48_dd_cor[,-1]
afq_50_dd_cor<-afq_50_dd_cor[,-1]
afq_51_dd_cor<-afq_51_dd_cor[,-1]
afq_53_dd_cor<-afq_53_dd_cor[,-1]
afq_54_dd_cor<-afq_54_dd_cor[,-1]
afq_55_dd_cor<-afq_55_dd_cor[,-1]
afq_56_dd_cor<-afq_56_dd_cor[,-1]
afq_57_dd_cor<-afq_57_dd_cor[,-1]
afq_58_dd_cor<-afq_58_dd_cor[,-1]

## append all data frames into one

afq_dd_cor_table<- dplyr::bind_rows(afq_1_dd_cor,afq_2_dd_cor,afq_3_dd_cor, afq_4_dd_cor, afq_5_dd_cor, afq_6_dd_cor, afq_7_dd_cor, afq_8_dd_cor, afq_9_dd_cor, afq_10_dd_cor, afq_11_dd_cor, afq_12_dd_cor, afq_13_dd_cor, afq_14_dd_cor, afq_15_dd_cor, afq_16_dd_cor, afq_17_dd_cor, afq_18_dd_cor, afq_19_dd_cor, afq_20_dd_cor, afq_21_dd_cor, afq_22_dd_cor, afq_23_dd_cor, afq_24_dd_cor, afq_28_dd_cor, afq_30_dd_cor, afq_31_dd_cor, afq_32_dd_cor, afq_33_dd_cor, afq_34_dd_cor, afq_35_dd_cor, afq_36_dd_cor, afq_37_dd_cor, afq_38_dd_cor, afq_39_dd_cor, afq_40_dd_cor, afq_41_dd_cor, afq_42_dd_cor, afq_43_dd_cor, afq_45_dd_cor, afq_48_dd_cor, afq_50_dd_cor, afq_51_dd_cor, afq_53_dd_cor, afq_54_dd_cor, afq_55_dd_cor, afq_56_dd_cor, afq_57_dd_cor, afq_58_dd_cor)

## clear total score rows
ind <- seq(1, nrow(afq_dd_cor_table), by=6)
afq_dd_cor_table<-afq_dd_cor_table[ind, ]

item_dd_2<-c("afq_1","afq_2","afq_3","afq_4","afq_5","afq_6","afq_7","afq_8","afq_9","afq_10","afq_11","afq_12","afq_13","afq_14","afq_15","afq_16","afq_17","afq_18","afq_19","afq_20","afq_21","afq_22","afq_23","afq_24","afq_28","afq_30","afq_31","afq_32","afq_33","afq_34","afq_35","afq_36","afq_37","afq_38","afq_39","afq_40","afq_41","afq_42","afq_43","afq_45","afq_48","afq_50","afq_51","afq_53","afq_54","afq_55","afq_56","afq_57","afq_58")

subscale<-c("cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive", 
            "physical", "physical", "physical", "physical", 
            "physical", "physical", "physical", "physical", 
            "physical", "physical", "physical", "physical", 
            "physical", "physical", "physical", "physical", 
            "physical", "physical", "physical", "physical", 
            "physical", "physical", "physical", "physical")

afq_dd_cor_table<-data.frame(item_dd_2, afq_dd_cor_table, subscale)
colnames(afq_dd_cor_table)<-(c("item", "afq_cogandphys_total", "afq_cog_total","afq_phys_total","pswq_total","masq_aa_total", "subscale"))

afq_dd_cor_table <- afq_dd_cor_table[, c(1,3,4,2,5,6,7)]


########################################
###### Let's move on to clustering #####
########################################

##Let's cluster and sort out what makes the most sense for the cognitive and physical subscale 

##How many clusters are best?

library("NbClust")
res.nbclust <- NbClust(afq_dd_cor_table[,(4:6)], distance = "euclidean",
                       min.nc = 2, max.nc = 7, 
                       method = "complete", index ="all")
factoextra::fviz_nbclust(res.nbclust) + theme_minimal() + ggtitle("NbClust's optimal number of clusters")

# let's do the clustering
kmeans(afq_dd_cor_table[,4:6], centers = 3, nstart = 30)
cluster_dd_cogphys<-c(2,   2,   1,   3,   3,   2,   1,   2,   2,   2,   2,   2,   3,   1,   3,   2,   2,   2,   1,   1,   2,   1,   3,   1,  2,   1,   2,   1,   1,   1,   1,   1,   1,   2,   3,   2,   1,   2,   1,   2,   3,   3,   1,   1,   1,   1,   1,   2, 3)
)

##Append a column with each itme's cluster index
afq_dd_cor_table<-data.frame(afq_dd_cor_table, cluster_dd_cogphys)
afq_dd_cor_table$cluster_dd_cogphys<-as.factor(afq_dd_cor_table$cluster_dd_cogphys)

###make a plot
scatter3d(x = afq_dd_cor_table$pswq_total, y = afq_dd_cor_table$masq_aa_total, z = afq_dd_cor_table$afq_cogandphys_total, groups = afq_dd_cor_table$cluster_dd_cogphys, surface=FALSE, xlab = "PSWQ", ylab = "MASQ AA", zlab = "AFQ Cog and phys Total", ellipsoid = TRUE)

Identify3d(x = afq_dd_cor_table$pswq_total, y = afq_dd_cor_table$masq_aa_total, z = afq_dd_cor_table$afq_cogandphys_total, groups = afq_dd_cor_table$cluster_dd_cogphys, labels = 1:length(afq_dd_cor_table$pswq_total),
           offset = ((100/length(afq_dd_cor_table$pswq_total))^(1/3)) * 0.02)



##############################################################################
###### Clustering with the whole scale after certain items were removed ######
##############################################################################

afq_dd_total<-freezing_raw_d %>%
  dplyr::select(afqs_1:afqs_70)
#Cut the items we don't want
#Cut 25, 26, 27
afq_dd_total<- afq_dd_total[,-c(25:27)]
afq_dd_total$afqs_44 <- NULL
afq_dd_total$afqs_46 <- NULL
afq_dd_total$afqs_47 <- NULL
afq_dd_total$afqs_49 <- NULL
afq_dd_total$afqs_52 <- NULL

##Add the new total columns 
afq_dd_total$afq_cog_total<-rowSums(afq_dd_total[,(1:28)])
afq_dd_total$afq_phys_total<-rowSums(afq_dd_total[,(29:47)])
afq_dd_total$afq_soc_total<-rowSums(afq_dd_total[,(48:61)])
afq_dd_total$afq_total<-rowSums(afq_dd_total[,c(1:61)])

##Add the totals from the MASQ and the PSWQ
afq_dd_total$pswq_total<-freezing_raw_d$pswq_total
afq_dd_total$masq_aa_total<-freezing_raw_d$masq_aa_total


## Generate list of pearson coefficients between items and totals ##
## make correlation matrices objects

afq_1_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_1", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_2_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_2", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_3_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_3", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_4_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_4", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_5_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_5", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_6_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_6", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_7_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_7", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_8_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_8", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_9_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_9", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_10_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_10", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_11_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_11", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_12_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_12", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_13_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_13", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_14_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_14", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_15_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_15", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_16_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_16", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_17_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_17", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_18_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_18", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_19_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_19", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_20_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_20", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_21_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_21", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_22_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_22", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_23_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_23", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_24_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_24", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_28_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_28", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_30_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_30", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_31_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_31", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_32_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_32", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_33_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_33", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_34_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_34", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_35_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_35", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_36_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_36", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_37_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_37", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_38_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_38", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_39_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_39", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_40_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_40", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_41_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_41", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_42_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_42", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_43_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_43", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_45_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_45", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_48_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_48", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_50_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_50", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_51_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_51", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_53_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_53", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_54_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_54", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_55_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_55", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_56_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_56", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_57_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_57", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_58_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_58", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_59_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_59", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_60_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_60", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_61_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_61", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_62_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_62", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_63_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_63", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_64_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_64", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_65_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_65", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_66_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_66", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_67_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_67", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_68_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_68", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_69_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_69", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_70_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_70", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))

## Remove Item Colum
afq_1_dd_cor<-afq_1_dd_cor[,-1]
afq_2_dd_cor<-afq_2_dd_cor[,-1]
afq_3_dd_cor<-afq_3_dd_cor[,-1]
afq_4_dd_cor<-afq_4_dd_cor[,-1]
afq_5_dd_cor<-afq_5_dd_cor[,-1]
afq_6_dd_cor<-afq_6_dd_cor[,-1]
afq_7_dd_cor<-afq_7_dd_cor[,-1]
afq_8_dd_cor<-afq_8_dd_cor[,-1]
afq_9_dd_cor<-afq_9_dd_cor[,-1]
afq_10_dd_cor<-afq_10_dd_cor[,-1]
afq_11_dd_cor<-afq_11_dd_cor[,-1]
afq_12_dd_cor<-afq_12_dd_cor[,-1]
afq_13_dd_cor<-afq_13_dd_cor[,-1]
afq_14_dd_cor<-afq_14_dd_cor[,-1]
afq_15_dd_cor<-afq_15_dd_cor[,-1]
afq_16_dd_cor<-afq_16_dd_cor[,-1]
afq_17_dd_cor<-afq_17_dd_cor[,-1]
afq_18_dd_cor<-afq_18_dd_cor[,-1]
afq_19_dd_cor<-afq_19_dd_cor[,-1]
afq_20_dd_cor<-afq_20_dd_cor[,-1]
afq_21_dd_cor<-afq_21_dd_cor[,-1]
afq_22_dd_cor<-afq_22_dd_cor[,-1]
afq_23_dd_cor<-afq_23_dd_cor[,-1]
afq_24_dd_cor<-afq_24_dd_cor[,-1]
afq_28_dd_cor<-afq_28_dd_cor[,-1]
afq_30_dd_cor<-afq_30_dd_cor[,-1]
afq_31_dd_cor<-afq_31_dd_cor[,-1]
afq_32_dd_cor<-afq_32_dd_cor[,-1]
afq_33_dd_cor<-afq_33_dd_cor[,-1]
afq_34_dd_cor<-afq_34_dd_cor[,-1]
afq_35_dd_cor<-afq_35_dd_cor[,-1]
afq_36_dd_cor<-afq_36_dd_cor[,-1]
afq_37_dd_cor<-afq_37_dd_cor[,-1]
afq_38_dd_cor<-afq_38_dd_cor[,-1]
afq_39_dd_cor<-afq_39_dd_cor[,-1]
afq_40_dd_cor<-afq_40_dd_cor[,-1]
afq_41_dd_cor<-afq_41_dd_cor[,-1]
afq_42_dd_cor<-afq_42_dd_cor[,-1]
afq_43_dd_cor<-afq_43_dd_cor[,-1]
afq_45_dd_cor<-afq_45_dd_cor[,-1]
afq_48_dd_cor<-afq_48_dd_cor[,-1]
afq_50_dd_cor<-afq_50_dd_cor[,-1]
afq_51_dd_cor<-afq_51_dd_cor[,-1]
afq_53_dd_cor<-afq_53_dd_cor[,-1]
afq_54_dd_cor<-afq_54_dd_cor[,-1]
afq_55_dd_cor<-afq_55_dd_cor[,-1]
afq_56_dd_cor<-afq_56_dd_cor[,-1]
afq_57_dd_cor<-afq_57_dd_cor[,-1]
afq_58_dd_cor<-afq_58_dd_cor[,-1]
afq_59_dd_cor<-afq_59_dd_cor[,-1]
afq_60_dd_cor<-afq_60_dd_cor[,-1]
afq_61_dd_cor<-afq_61_dd_cor[,-1]
afq_62_dd_cor<-afq_62_dd_cor[,-1]
afq_63_dd_cor<-afq_63_dd_cor[,-1]
afq_64_dd_cor<-afq_64_dd_cor[,-1]
afq_65_dd_cor<-afq_65_dd_cor[,-1]
afq_66_dd_cor<-afq_66_dd_cor[,-1]
afq_67_dd_cor<-afq_67_dd_cor[,-1]
afq_68_dd_cor<-afq_68_dd_cor[,-1]
afq_69_dd_cor<-afq_69_dd_cor[,-1]
afq_70_dd_cor<-afq_70_dd_cor[,-1]

## append all data frames into one

afq_dd_cor_table<- dplyr::bind_rows(afq_1_dd_cor,afq_2_dd_cor,afq_3_dd_cor, afq_4_dd_cor, afq_5_dd_cor, afq_6_dd_cor, afq_7_dd_cor, afq_8_dd_cor, afq_9_dd_cor, afq_10_dd_cor, afq_11_dd_cor, afq_12_dd_cor, afq_13_dd_cor, afq_14_dd_cor, afq_15_dd_cor, afq_16_dd_cor, afq_17_dd_cor, afq_18_dd_cor, afq_19_dd_cor, afq_20_dd_cor, afq_21_dd_cor, afq_22_dd_cor, afq_23_dd_cor, afq_24_dd_cor, afq_28_dd_cor, afq_30_dd_cor, afq_31_dd_cor, afq_32_dd_cor, afq_33_dd_cor, afq_34_dd_cor, afq_35_dd_cor, afq_36_dd_cor, afq_37_dd_cor, afq_38_dd_cor, afq_39_dd_cor, afq_40_dd_cor, afq_41_dd_cor, afq_42_dd_cor, afq_43_dd_cor, afq_45_dd_cor, afq_48_dd_cor, afq_50_dd_cor, afq_51_dd_cor, afq_53_dd_cor, afq_54_dd_cor, afq_55_dd_cor, afq_56_dd_cor, afq_57_dd_cor, afq_58_dd_cor, afq_59_dd_cor, afq_60_dd_cor, afq_61_dd_cor, afq_62_dd_cor, afq_63_dd_cor, afq_64_dd_cor, afq_65_dd_cor, afq_66_dd_cor, afq_67_dd_cor, afq_68_dd_cor, afq_69_dd_cor, afq_70_dd_cor)

## clear total score rows
ind <- seq(1, nrow(afq_dd_cor_table), by=7)
afq_dd_cor_table<-afq_dd_cor_table[ind, ]

item_dd_2<-c("afq_1","afq_2","afq_3","afq_4","afq_5","afq_6","afq_7","afq_8","afq_9","afq_10","afq_11","afq_12","afq_13","afq_14","afq_15","afq_16","afq_17","afq_18","afq_19","afq_20","afq_21","afq_22","afq_23","afq_24","afq_28","afq_30","afq_31","afq_32","afq_33","afq_34","afq_35","afq_36","afq_37","afq_38","afq_39","afq_40","afq_41","afq_42","afq_43","afq_45","afq_48","afq_50","afq_51","afq_53","afq_54","afq_55","afq_56","afq_57","afq_58","afq_59","afq_60","afq_61","afq_62","afq_63","afq_64","afq_65","afq_66","afq_67","afq_68","afq_69", "afq_70")

subscale<-c("cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive", 
            "physical", "physical", "physical", "physical", 
            "physical", "physical", "physical", "physical", 
            "physical", "physical", "physical", "physical", 
            "physical", "physical", "physical", "physical", 
            "physical", "physical", "physical", "physical", 
            "physical", "physical", "physical", 
            "social", "social", "social", "social", "social", 
            "social", "social", "social", "social", "social", 
            "social", "social", "social")

afq_dd_cor_table<-data.frame(item_dd_2, afq_dd_cor_table, subscale)
colnames(afq_dd_cor_table)<-(c("item", "afq_total", "afq_cog_total","afq_phys_total","afq_soc_total","pswq_total","masq_aa_total", "subscale"))

afq_dd_cor_table <- afq_dd_cor_table[, c(1,3,4,5,2,6,7,8)]

########################################
###### Let's move on to clustering #####
########################################

##Let's cluster and sort out what makes the most sense for the cognitive and physical subscale 

##How many clusters are best?

library("NbClust")
res.nbclust <- NbClust(afq_dd_cor_table[,(5:7)], distance = "euclidean",
                       min.nc = 2, max.nc = 7, 
                       method = "complete", index ="all")
factoextra::fviz_nbclust(res.nbclust) + theme_minimal() + ggtitle("NbClust's optimal number of clusters")

# let's do the clustering
kmeans(afq_dd_cor_table[,5:7], centers = 3, nstart = 50)
cluster_dd_total<-c(1,   1,   2,   3,   2,   1,   2,   1,   1,   1,   1,   1,   3,   2,   3,   1,   1,   1,   2,   2,   1,   1,   3,   2, 3,   2,   1,   2,   2,   2,   2,   2,   2,   1,   3,   3,   2,   1,   2,   1,   2,   2  , 2,   2,   2,   2,   2,   1, 3,   3,   3,   1,   1,   1,   1,   1,   1,   2,   1,   1,   1 )
)

##Append a column with each itme's cluster index
afq_dd_cor_table<-data.frame(afq_dd_cor_table, cluster_dd_total)
afq_dd_cor_table$cluster_dd_total<-as.factor(afq_dd_cor_table$cluster_dd_total)



###make a plot

library('rgl')
library('car')
scatter3d(x = afq_dd_cor_table$pswq_total, y = afq_dd_cor_table$masq_aa_total, z = afq_dd_cor_table$afq_total, groups = afq_dd_cor_table$cluster_dd_total, surface=FALSE, xlab = "PSWQ", ylab = "MASQ AA", zlab = "AFQ Cog and phys Total", ellipsoid = TRUE)
Identify3d(x = afq_dd_cor_table$pswq_total, y = afq_dd_cor_table$masq_aa_total, z = afq_dd_cor_table$afq_total, groups = afq_dd_cor_table$cluster_dd_total,offset = ((100/length(afq_dd_cor_table$pswq_total))^(1/3)) * 0.02)


##############################################################################
###### Removing more items after last clustering attempt (removed cluster 3 ######
##############################################################################
afq_dd_total<-freezing_raw_d %>%
  dplyr::select(afqs_1:afqs_70)
#Cut the items we don't want
afq_dd_total$afqs_4 <- NULL
afq_dd_total$afqs_5 <- NULL
afq_dd_total$afqs_13 <- NULL
afq_dd_total$afqs_15 <- NULL
afq_dd_total$afqs_20 <- NULL
afq_dd_total$afqs_23 <- NULL
afq_dd_total$afqs_25 <- NULL
afq_dd_total$afqs_26 <- NULL
afq_dd_total$afqs_27 <- NULL
afq_dd_total$afqs_28 <- NULL
afq_dd_total$afqs_39 <- NULL
afq_dd_total$afqs_40 <- NULL
afq_dd_total$afqs_44 <- NULL
afq_dd_total$afqs_46 <- NULL
afq_dd_total$afqs_47 <- NULL
afq_dd_total$afqs_48 <- NULL
afq_dd_total$afqs_49 <- NULL
afq_dd_total$afqs_50 <- NULL
afq_dd_total$afqs_52 <- NULL
afq_dd_total$afqs_54 <- NULL
afq_dd_total$afqs_55 <- NULL
afq_dd_total$afqs_56 <- NULL
afq_dd_total$afqs_57 <- NULL
afq_dd_total$afqs_58 <- NULL
afq_dd_total$afqs_59 <- NULL
afq_dd_total$afqs_60 <- NULL
afq_dd_total$afqs_61 <- NULL
afq_dd_total$afqs_63 <- NULL

##Add the new total columns 
afq_dd_total$afq_cog_total<-rowSums(afq_dd_total[,(1:18)])
afq_dd_total$afq_phys_total<-rowSums(afq_dd_total[,(19:33)])
afq_dd_total$afq_soc_total<-rowSums(afq_dd_total[,(34:41)])
afq_dd_total$afq_total<-rowSums(afq_dd_total[,c(1:41)])

##Add the totals from the MASQ and the PSWQ
afq_dd_total$pswq_total<-freezing_raw_d$pswq_total
afq_dd_total$masq_aa_total<-freezing_raw_d$masq_aa_total


## Generate list of pearson coefficients between items and totals ##
## make correlation matrices objects

afq_1_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_1", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_2_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_2", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_3_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_3", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_6_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_6", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_7_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_7", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_8_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_8", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_9_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_9", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_10_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_10", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_11_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_11", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_12_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_12", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_14_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_14", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_16_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_16", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_17_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_17", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_18_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_18", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_19_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_19", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_21_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_21", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_22_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_22", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_24_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_24", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_30_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_30", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_31_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_31", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_32_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_32", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_33_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_33", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_34_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_34", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_35_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_35", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_36_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_36", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_37_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_37", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_38_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_38", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_41_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_41", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_42_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_42", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_43_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_43", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_45_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_45", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_51_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_51", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_53_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_53", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_62_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_62", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_64_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_64", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_65_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_65", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_66_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_66", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_67_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_67", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_68_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_68", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_69_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_69", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))
afq_70_dd_cor<-as.data.frame(cor(afq_dd_total[, c("afqs_70", "afq_total", "afq_cog_total", "afq_phys_total","afq_soc_total","pswq_total", "masq_aa_total")], method = "pearson"))

## Remove Item Colum
afq_1_dd_cor<-afq_1_dd_cor[,-1]
afq_2_dd_cor<-afq_2_dd_cor[,-1]
afq_3_dd_cor<-afq_3_dd_cor[,-1]
afq_6_dd_cor<-afq_6_dd_cor[,-1]
afq_7_dd_cor<-afq_7_dd_cor[,-1]
afq_8_dd_cor<-afq_8_dd_cor[,-1]
afq_9_dd_cor<-afq_9_dd_cor[,-1]
afq_10_dd_cor<-afq_10_dd_cor[,-1]
afq_11_dd_cor<-afq_11_dd_cor[,-1]
afq_12_dd_cor<-afq_12_dd_cor[,-1]
afq_14_dd_cor<-afq_14_dd_cor[,-1]
afq_16_dd_cor<-afq_16_dd_cor[,-1]
afq_17_dd_cor<-afq_17_dd_cor[,-1]
afq_18_dd_cor<-afq_18_dd_cor[,-1]
afq_19_dd_cor<-afq_19_dd_cor[,-1]
afq_21_dd_cor<-afq_21_dd_cor[,-1]
afq_22_dd_cor<-afq_22_dd_cor[,-1]
afq_24_dd_cor<-afq_24_dd_cor[,-1]
afq_30_dd_cor<-afq_30_dd_cor[,-1]
afq_31_dd_cor<-afq_31_dd_cor[,-1]
afq_32_dd_cor<-afq_32_dd_cor[,-1]
afq_33_dd_cor<-afq_33_dd_cor[,-1]
afq_34_dd_cor<-afq_34_dd_cor[,-1]
afq_35_dd_cor<-afq_35_dd_cor[,-1]
afq_36_dd_cor<-afq_36_dd_cor[,-1]
afq_37_dd_cor<-afq_37_dd_cor[,-1]
afq_38_dd_cor<-afq_38_dd_cor[,-1]
afq_41_dd_cor<-afq_41_dd_cor[,-1]
afq_42_dd_cor<-afq_42_dd_cor[,-1]
afq_43_dd_cor<-afq_43_dd_cor[,-1]
afq_45_dd_cor<-afq_45_dd_cor[,-1]
afq_51_dd_cor<-afq_51_dd_cor[,-1]
afq_53_dd_cor<-afq_53_dd_cor[,-1]
afq_62_dd_cor<-afq_62_dd_cor[,-1]
afq_64_dd_cor<-afq_64_dd_cor[,-1]
afq_65_dd_cor<-afq_65_dd_cor[,-1]
afq_66_dd_cor<-afq_66_dd_cor[,-1]
afq_67_dd_cor<-afq_67_dd_cor[,-1]
afq_68_dd_cor<-afq_68_dd_cor[,-1]
afq_69_dd_cor<-afq_69_dd_cor[,-1]
afq_70_dd_cor<-afq_70_dd_cor[,-1]

## append all data frames into one

afq_dd_cor_table<- dplyr::bind_rows(afq_1_dd_cor,afq_2_dd_cor, afq_3_dd_cor, afq_6_dd_cor, afq_7_dd_cor, afq_8_dd_cor, afq_9_dd_cor, afq_10_dd_cor, afq_11_dd_cor, afq_12_dd_cor,  afq_14_dd_cor,  afq_16_dd_cor, afq_17_dd_cor, afq_18_dd_cor, afq_19_dd_cor,  afq_21_dd_cor, afq_22_dd_cor, afq_24_dd_cor,  afq_30_dd_cor, afq_31_dd_cor, afq_32_dd_cor, afq_33_dd_cor, afq_34_dd_cor, afq_35_dd_cor, afq_36_dd_cor, afq_37_dd_cor, afq_38_dd_cor,  afq_41_dd_cor, afq_42_dd_cor, afq_43_dd_cor, afq_45_dd_cor, afq_51_dd_cor, afq_53_dd_cor, afq_62_dd_cor, afq_64_dd_cor, afq_65_dd_cor, afq_66_dd_cor, afq_67_dd_cor, afq_68_dd_cor, afq_69_dd_cor, afq_70_dd_cor)

## clear total score rows
ind <- seq(1, nrow(afq_dd_cor_table), by=7)
afq_dd_cor_table<-afq_dd_cor_table[ind, ]

item_dd_2<-c("afq_1","afq_2","afq_3","afq_6","afq_7","afq_8","afq_9","afq_10","afq_11","afq_12","afq_14","afq_16","afq_17","afq_18","afq_19","afq_21","afq_22","afq_24","afq_30","afq_31","afq_32","afq_33","afq_34","afq_35","afq_36","afq_37","afq_38","afq_41","afq_42","afq_43","afq_45","afq_51","afq_53","afq_62","afq_64","afq_65","afq_66","afq_67","afq_68","afq_69", "afq_70")

subscale<-c("cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive", "cognitive",
            "physical", "physical", "physical", "physical", 
            "physical", "physical", "physical", "physical", 
            "physical", "physical", "physical", "physical", 
            "physical", "physical", "physical",
            "social", "social", "social", "social", "social", 
            "social", "social", "social")

afq_dd_cor_table<-data.frame(item_dd_2, afq_dd_cor_table, subscale)
colnames(afq_dd_cor_table)<-(c("item", "afq_total", "afq_cog_total","afq_phys_total","afq_soc_total","pswq_total","masq_aa_total", "subscale"))

afq_dd_cor_table <- afq_dd_cor_table[, c(1,3,4,5,2,6,7,8)]

########################################
###### Let's move on to clustering #####
########################################

##Let's cluster and sort out what makes the most sense for the cognitive and physical subscale 

##How many clusters are best?

library("NbClust")
res.nbclust <- NbClust(afq_dd_cor_table[,(5:7)], distance = "euclidean",
                       min.nc = 2, max.nc = 7, 
                       method = "complete", index ="all")

factoextra::fviz_nbclust(res.nbclust) + theme_minimal() + ggtitle("NbClust's optimal number of clusters")

# let's do the clustering
kmeans(afq_dd_cor_table[,5:7], centers = 3, nstart = 50)
cluster_dd_total<-c(2,   2,   3,   1,   3,   1,   1,   1,   1,   2,   3,   2, 2,   1,   3,   1,   1,   3,   3,   2,   3,   3,   3,   3,   3,   3,  2,   3,   1,   3,   1,   3,   3,   2,   1,   2,   1,   3,   2,   1,   2 )

##Append a column with each itme's cluster index
afq_dd_cor_table<-data.frame(afq_dd_cor_table, cluster_dd_total)
afq_dd_cor_table$cluster_dd_total<-as.factor(afq_dd_cor_table$cluster_dd_total)



###make a plot

library('rgl')
library('car')
scatter3d(x = afq_dd_cor_table$pswq_total, y = afq_dd_cor_table$masq_aa_total, z = afq_dd_cor_table$afq_total, groups = afq_dd_cor_table$cluster_dd_total, surface=FALSE, xlab = "PSWQ", ylab = "MASQ AA", zlab = "AFQ Total", ellipsoid = TRUE)
Identify3d(x = afq_dd_cor_table$pswq_total, y = afq_dd_cor_table$masq_aa_total, z = afq_dd_cor_table$afq_total, groups = afq_dd_cor_table$cluster_dd_total,offset = ((100/length(afq_dd_cor_table$pswq_total))^(1/3)) * 0.02)


##Get some descriptives of the reduced clustered survey

afq_dd_cor_table %>%
  dplyr::filter(cluster_dd_total == 1) %>%
  psych::describe(afq_dd_cor_table$afq_total)
afq_dd_cor_table %>%
  dplyr::filter(cluster_dd_total == 2) %>%
  psych::describe(afq_dd_cor_table$afq_total)
afq_dd_cor_table %>%
  dplyr::filter(cluster_dd_total == 3) %>%
  psych::describe(afq_dd_cor_table$afq_total)











#######################################################################################################
###### Removing EVEN MORE items after last clustering attempt (removed items via rationale cutting) ###
#######################################################################################################
afq_final<-freezing_raw_d %>%
  dplyr::select(afqs_1:afqs_70)
#Cut the items we don't want
afq_final$afqs_3 <- NULL
afq_final$afqs_4 <- NULL
afq_final$afqs_5 <- NULL
afq_final$afqs_11 <- NULL
afq_final$afqs_12 <- NULL
afq_final$afqs_13 <- NULL
afq_final$afqs_14 <- NULL
afq_final$afqs_15 <- NULL
afq_final$afqs_17 <- NULL
afq_final$afqs_19 <- NULL
afq_final$afqs_20 <- NULL
afq_final$afqs_21 <- NULL
afq_final$afqs_23 <- NULL
afq_final$afqs_24 <- NULL
afq_final$afqs_25 <- NULL
afq_final$afqs_26 <- NULL
afq_final$afqs_27 <- NULL
afq_final$afqs_28 <- NULL
afq_final$afqs_30 <- NULL
afq_final$afqs_31 <- NULL
afq_final$afqs_32 <- NULL
afq_final$afqs_33 <- NULL
afq_final$afqs_34 <- NULL
afq_final$afqs_35 <- NULL
afq_final$afqs_38 <- NULL
afq_final$afqs_39 <- NULL
afq_final$afqs_40 <- NULL
afq_final$afqs_44 <- NULL
afq_final$afqs_45 <- NULL
afq_final$afqs_46 <- NULL
afq_final$afqs_47 <- NULL
afq_final$afqs_48 <- NULL
afq_final$afqs_49 <- NULL
afq_final$afqs_50 <- NULL
afq_final$afqs_52 <- NULL
afq_final$afqs_53 <- NULL
afq_final$afqs_54 <- NULL
afq_final$afqs_55 <- NULL
afq_final$afqs_56 <- NULL
afq_final$afqs_57 <- NULL
afq_final$afqs_58 <- NULL
afq_final$afqs_59 <- NULL
afq_final$afqs_60 <- NULL
afq_final$afqs_61 <- NULL
afq_final$afqs_63 <- NULL


##Add the items and totals from the MASQ and the PSWQ
afq_final$pswq_01<-freezing_raw_d$pswq_01
afq_final$pswq_02<-freezing_raw_d$pswq_02
afq_final$pswq_03<-freezing_raw_d$pswq_03
afq_final$pswq_04<-freezing_raw_d$pswq_04
afq_final$pswq_05<-freezing_raw_d$pswq_05
afq_final$pswq_06<-freezing_raw_d$pswq_06
afq_final$pswq_07<-freezing_raw_d$pswq_07
afq_final$pswq_08<-freezing_raw_d$pswq_08
afq_final$pswq_09<-freezing_raw_d$pswq_09
afq_final$pswq_10<-freezing_raw_d$pswq_10
afq_final$pswq_11<-freezing_raw_d$pswq_11
afq_final$pswq_12<-freezing_raw_d$pswq_12
afq_final$pswq_13<-freezing_raw_d$pswq_13
afq_final$pswq_14<-freezing_raw_d$pswq_14
afq_final$pswq_15<-freezing_raw_d$pswq_15
afq_final$pswq_16<-freezing_raw_d$pswq_16
afq_final$pswq_total<-freezing_raw_d$pswq_total
afq_final$masq_01<-freezing_raw_d$masq_01
afq_final$masq_03<-freezing_raw_d$masq_03
afq_final$masq_14<-freezing_raw_d$masq_14
afq_final$masq_18<-freezing_raw_d$masq_18
afq_final$masq_19<-freezing_raw_d$masq_19
afq_final$masq_21<-freezing_raw_d$masq_21
afq_final$masq_23<-freezing_raw_d$masq_23
afq_final$masq_25<-freezing_raw_d$masq_25
afq_final$masq_26<-freezing_raw_d$masq_26
afq_final$masq_27<-freezing_raw_d$masq_27
afq_final$masq_30<-freezing_raw_d$masq_30
afq_final$masq_33<-freezing_raw_d$masq_33
afq_final$masq_35<-freezing_raw_d$masq_35
afq_final$masq_36<-freezing_raw_d$masq_36
afq_final$masq_39<-freezing_raw_d$masq_39
afq_final$masq_40<-freezing_raw_d$masq_40
afq_final$masq_44<-freezing_raw_d$masq_44
afq_final$masq_45<-freezing_raw_d$masq_45
afq_final$masq_48<-freezing_raw_d$masq_48
afq_final$masq_49<-freezing_raw_d$masq_49
afq_final$masq_52<-freezing_raw_d$masq_52
afq_final$masq_53<-freezing_raw_d$masq_53
afq_final$masq_55<-freezing_raw_d$masq_55
afq_final$masq_57<-freezing_raw_d$masq_57
afq_final$masq_58<-freezing_raw_d$masq_58
afq_final$masq_61<-freezing_raw_d$masq_61
afq_final$masq_66<-freezing_raw_d$masq_66
afq_final$masq_67<-freezing_raw_d$masq_67
afq_final$masq_69<-freezing_raw_d$masq_69
afq_final$masq_72<-freezing_raw_d$masq_72
afq_final$masq_73<-freezing_raw_d$masq_73
afq_final$masq_75<-freezing_raw_d$masq_75
afq_final$masq_78<-freezing_raw_d$masq_78
afq_final$masq_79<-freezing_raw_d$masq_79
afq_final$masq_85<-freezing_raw_d$masq_85
afq_final$masq_86<-freezing_raw_d$masq_86
afq_final$masq_87<-freezing_raw_d$masq_87
afq_final$masq_88<-freezing_raw_d$masq_88
afq_final$masq_89<-freezing_raw_d$masq_89
afq_final$masq_aa_total<-freezing_raw_d$masq_aa_total
afq_final$masq_ad_total<-freezing_raw_d$masq_ad_total
afq_final$masq_lpa_total<-freezing_raw_d$masq_lpa_total
afq_final$masq_dm_total<-freezing_raw_d$masq_dm_total

##Add new total column
afq_final$afq_total<-rowSums(afq_final[,c(1:28)])
psych::describe(afq_final$afq_total)
summary(afq_final$afq_total)




afq_high <- afq_final %>%
  dplyr::filter(afq_total >= 74)
psych::describe(afq_high$afq_total)
summary(afq_high$afq_total)

afq_low <- afq_final %>%
  dplyr::filter(afq_total <= 56)


t.test(afq_high$pswq_total, afq_low$pswq_total, alternative = "two.sided", var.equal = FALSE)
t.test(afq_high$masq_aa_total, afq_low$masq_aa_total, alternative = "two.sided", var.equal = FALSE)
t.test(afq_high$masq_ad_total, afq_low$masq_ad_total, alternative = "two.sided", var.equal = FALSE)
t.test(afq_high$masq_lpa_total, afq_low$masq_lpa_total, alternative = "two.sided", var.equal = FALSE)
t.test(afq_high$masq_dm_total, afq_low$masq_dm_total, alternative = "two.sided", var.equal = FALSE)

## Visualize
ggplot(afq_final, aes(x=afq_total, y=pswq_total)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F) +
  labs(x= "afq", y= "pswq") 

ggplot(afq_final, aes(x=afq_total, y=masq_aa_total)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F) +
  labs(x= "afq", y= "arousal") 

ggplot(afq_final, aes(x=afq_total, y=masq_ad_total)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F) +
  labs(x= "afq", y= "anhedonic depression") 


## Generate list of pearson coefficients between items and totals ##
## make correlation matrices objects

afq_1_cor<-as.data.frame(cor(afq_final[, c("afqs_1", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
afq_2_cor<-as.data.frame(cor(afq_final[, c("afqs_2", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
afq_3_cor<-as.data.frame(cor(afq_final[, c("afqs_3", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
afq_6_cor<-as.data.frame(cor(afq_final[, c("afqs_6", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
afq_7_cor<-as.data.frame(cor(afq_final[, c("afqs_7", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
afq_8_cor<-as.data.frame(cor(afq_final[, c("afqs_8", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
afq_9_cor<-as.data.frame(cor(afq_final[, c("afqs_9", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
afq_10_cor<-as.data.frame(cor(afq_final[, c("afqs_10", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
afq_14_cor<-as.data.frame(cor(afq_final[, c("afqs_14", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
afq_16_cor<-as.data.frame(cor(afq_final[, c("afqs_16", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
afq_18_cor<-as.data.frame(cor(afq_final[, c("afqs_18", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
afq_22_cor<-as.data.frame(cor(afq_final[, c("afqs_22", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
afq_24_cor<-as.data.frame(cor(afq_final[, c("afqs_24", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
afq_30_cor<-as.data.frame(cor(afq_final[, c("afqs_30", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
afq_36_cor<-as.data.frame(cor(afq_final[, c("afqs_36", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
afq_37_cor<-as.data.frame(cor(afq_final[, c("afqs_37", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
afq_41_cor<-as.data.frame(cor(afq_final[, c("afqs_41", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
afq_42_cor<-as.data.frame(cor(afq_final[, c("afqs_42", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
afq_43_cor<-as.data.frame(cor(afq_final[, c("afqs_43", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
afq_51_cor<-as.data.frame(cor(afq_final[, c("afqs_51", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
afq_62_cor<-as.data.frame(cor(afq_final[, c("afqs_62", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
afq_64_cor<-as.data.frame(cor(afq_final[, c("afqs_64", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
afq_65_cor<-as.data.frame(cor(afq_final[, c("afqs_65", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
afq_66_cor<-as.data.frame(cor(afq_final[, c("afqs_66", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
afq_67_cor<-as.data.frame(cor(afq_final[, c("afqs_67", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
afq_68_cor<-as.data.frame(cor(afq_final[, c("afqs_68", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
afq_69_cor<-as.data.frame(cor(afq_final[, c("afqs_69", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
afq_70_cor<-as.data.frame(cor(afq_final[, c("afqs_70", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))

## Remove Item Colum
afq_1_cor<-afq_1_cor[,-1]
afq_2_cor<-afq_2_cor[,-1]
afq_3_cor<-afq_3_cor[,-1]
afq_6_cor<-afq_6_cor[,-1]
afq_7_cor<-afq_7_cor[,-1]
afq_8_cor<-afq_8_cor[,-1]
afq_9_cor<-afq_9_cor[,-1]
afq_10_cor<-afq_10_cor[,-1]
afq_14_cor<-afq_14_cor[,-1]
afq_16_cor<-afq_16_cor[,-1]
afq_18_cor<-afq_18_cor[,-1]
afq_22_cor<-afq_22_cor[,-1]
afq_24_cor<-afq_24_cor[,-1]
afq_30_cor<-afq_30_cor[,-1]
afq_36_cor<-afq_36_cor[,-1]
afq_37_cor<-afq_37_cor[,-1]
afq_41_cor<-afq_41_cor[,-1]
afq_42_cor<-afq_42_cor[,-1]
afq_43_cor<-afq_43_cor[,-1]
afq_51_cor<-afq_51_cor[,-1]
afq_62_cor<-afq_62_cor[,-1]
afq_64_cor<-afq_64_cor[,-1]
afq_65_cor<-afq_65_cor[,-1]
afq_66_cor<-afq_66_cor[,-1]
afq_67_cor<-afq_67_cor[,-1]
afq_68_cor<-afq_68_cor[,-1]
afq_69_cor<-afq_69_cor[,-1]
afq_70_cor<-afq_70_cor[,-1]

## append all data frames into one

afq_cor_table<- dplyr::bind_rows(afq_1_cor,afq_2_cor, afq_3_cor, afq_6_cor, afq_7_cor, afq_8_cor, 
                                 afq_9_cor, afq_10_cor, afq_14_cor,  afq_16_cor, afq_18_cor, afq_22_cor, 
                                 afq_24_cor,  afq_30_cor, afq_36_cor, afq_37_cor, afq_41_cor, afq_42_cor, 
                                 afq_43_cor, afq_51_cor, afq_62_cor, afq_64_cor, afq_65_cor, afq_66_cor, 
                                 afq_67_cor, afq_68_cor, afq_69_cor, afq_70_cor)

## clear total score rows
ind <- seq(1, nrow(afq_cor_table), by=5)
afq_cor_table<-afq_cor_table[ind, ]

item_final<-c("afq_1","afq_2","afq_3","afq_6","afq_7","afq_8","afq_9","afq_10",
              "afq_14","afq_16","afq_18","afq_22","afq_24","afq_30","afq_36","afq_37",
              "afq_41","afq_42","afq_43","afq_51","afq_62","afq_64","afq_65","afq_66","afq_67",
              "afq_68","afq_69", "afq_70")

subscale<-c("cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive",
            "physical", "physical", "physical", "physical", 
            "physical", "physical", "physical", 
            "social", "social", "social", "social", "social", 
            "social", "social", "social")


afq_cor_table<-data.frame(item_final, afq_cor_table, subscale)
colnames(afq_cor_table)<-(c("item", "afq_total","pswq_total","masq_aa_total", "masq_ad_total", "subscale"))


########################################
###### Let's move on to clustering #####
########################################


##How many clusters are best?

library("NbClust")
res.nbclust <- NbClust(afq_cor_table[,(2:4)], distance = "euclidean",
                       min.nc = 2, max.nc = 7, 
                       method = "complete", index ="all")

factoextra::fviz_nbclust(res.nbclust) + theme_minimal() + ggtitle("NbClust's optimal number of clusters")

# let's do the clustering
kmeans(afq_cor_table[,2:4], centers = 2, nstart = 50)
cluster_final<-c(2,2,1,2,1,2,2,2,1,2,2,2,1,1,1,1,1,2,1,1,2,2,2,2,1,2,2,2)

##Append a column with each itme's cluster index
afq_cor_table<-data.frame(afq_cor_table, cluster_final)
afq_cor_table$cluster_final<-as.factor(afq_cor_table$cluster_final)



###make a plot

library('rgl')
library('car')
minScale<-min(0)
maxScale<-max(1)
scatter3d(x = afq_cor_table$pswq_total, y = afq_cor_table$masq_aa_total, z = afq_cor_table$afq_total, 
          axis.scales = TRUE, xlim = c(minScale, maxScale), ylim = c(minScale, maxScale), zlim = c(minScale, maxScale),groups = afq_cor_table$cluster_final, surface=FALSE,
          xlab = "PSWQ", ylab = "MASQ AA", zlab = "AFQ Total", ellipsoid = TRUE)

Identify3d(x = afq_cor_table$pswq_total, y = afq_cor_table$masq_aa_total, z = afq_cor_table$afq_cogandphys_total, 
           groups = afq_cor_table$cluster_final,labels = 1:length(0:1))


##Get some descriptives of the reduced clustered survey

afq_cor_table %>%
  dplyr::filter(cluster_final == 1) %>%
  psych::describe(afq_cor_table$afq_total)
afq_cor_table %>%
  dplyr::filter(cluster_final == 2) %>%
  psych::describe(afq_cor_table$afq_total)

######################################################################
##### Now, let's see how PSWQ and MASQ items map onto AFQ total  ##### 
######################################################################


## PSWQ
pswq_01<-as.data.frame(cor(afq_final[, c("pswq_01", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_02<-as.data.frame(cor(afq_final[, c("pswq_02", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_03<-as.data.frame(cor(afq_final[, c("pswq_03", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_04<-as.data.frame(cor(afq_final[, c("pswq_04", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_05<-as.data.frame(cor(afq_final[, c("pswq_05", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_06<-as.data.frame(cor(afq_final[, c("pswq_06", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_07<-as.data.frame(cor(afq_final[, c("pswq_07", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_08<-as.data.frame(cor(afq_final[, c("pswq_08", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_09<-as.data.frame(cor(afq_final[, c("pswq_09", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_10<-as.data.frame(cor(afq_final[, c("pswq_10", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_11<-as.data.frame(cor(afq_final[, c("pswq_11", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_12<-as.data.frame(cor(afq_final[, c("pswq_12", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_13<-as.data.frame(cor(afq_final[, c("pswq_13", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_14<-as.data.frame(cor(afq_final[, c("pswq_14", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_15<-as.data.frame(cor(afq_final[, c("pswq_15", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_16<-as.data.frame(cor(afq_final[, c("pswq_16", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))


## Remove Item Colum
pswq_01<-pswq_01[,-1]
pswq_02<-pswq_02[,-1]
pswq_03<-pswq_03[,-1]
pswq_04<-pswq_04[,-1]
pswq_05<-pswq_05[,-1]
pswq_06<-pswq_06[,-1]
pswq_07<-pswq_07[,-1]
pswq_08<-pswq_08[,-1]
pswq_09<-pswq_09[,-1]
pswq_10<-pswq_10[,-1]
pswq_11<-pswq_11[,-1]
pswq_12<-pswq_12[,-1]
pswq_13<-pswq_13[,-1]
pswq_14<-pswq_14[,-1]
pswq_15<-pswq_15[,-1]
pswq_16<-pswq_16[,-1]


## append all data frames into one

pswq_cor_table<- dplyr::bind_rows(pswq_01,pswq_02,pswq_03,pswq_04,pswq_05,pswq_06,pswq_07,
                                  pswq_08,pswq_09,pswq_10,pswq_11,pswq_12,pswq_13,pswq_14,
                                  pswq_15,pswq_16)

## clear total score rows
ind <- seq(1, nrow(pswq_cor_table), by=5)
pswq_cor_table<-pswq_cor_table[ind, ]


## MASQ
masq_01<-as.data.frame(cor(afq_final[, c("masq_01", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_03<-as.data.frame(cor(afq_final[, c("masq_03", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_14<-as.data.frame(cor(afq_final[, c("masq_14", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_18<-as.data.frame(cor(afq_final[, c("masq_18", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_19<-as.data.frame(cor(afq_final[, c("masq_19", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_21<-as.data.frame(cor(afq_final[, c("masq_21", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_23<-as.data.frame(cor(afq_final[, c("masq_23", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_25<-as.data.frame(cor(afq_final[, c("masq_25", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_26<-as.data.frame(cor(afq_final[, c("masq_26", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_27<-as.data.frame(cor(afq_final[, c("masq_27", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_30<-as.data.frame(cor(afq_final[, c("masq_30", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_33<-as.data.frame(cor(afq_final[, c("masq_33", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_35<-as.data.frame(cor(afq_final[, c("masq_35", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_36<-as.data.frame(cor(afq_final[, c("masq_36", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_39<-as.data.frame(cor(afq_final[, c("masq_39", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_40<-as.data.frame(cor(afq_final[, c("masq_40", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_44<-as.data.frame(cor(afq_final[, c("masq_44", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_45<-as.data.frame(cor(afq_final[, c("masq_45", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_48<-as.data.frame(cor(afq_final[, c("masq_48", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_49<-as.data.frame(cor(afq_final[, c("masq_49", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_52<-as.data.frame(cor(afq_final[, c("masq_52", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_53<-as.data.frame(cor(afq_final[, c("masq_53", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_55<-as.data.frame(cor(afq_final[, c("masq_55", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_57<-as.data.frame(cor(afq_final[, c("masq_57", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_58<-as.data.frame(cor(afq_final[, c("masq_58", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_61<-as.data.frame(cor(afq_final[, c("masq_61", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_66<-as.data.frame(cor(afq_final[, c("masq_66", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_67<-as.data.frame(cor(afq_final[, c("masq_67", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_69<-as.data.frame(cor(afq_final[, c("masq_69", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_72<-as.data.frame(cor(afq_final[, c("masq_72", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_73<-as.data.frame(cor(afq_final[, c("masq_73", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_75<-as.data.frame(cor(afq_final[, c("masq_75", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_78<-as.data.frame(cor(afq_final[, c("masq_78", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_79<-as.data.frame(cor(afq_final[, c("masq_79", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_85<-as.data.frame(cor(afq_final[, c("masq_85", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_86<-as.data.frame(cor(afq_final[, c("masq_86", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_87<-as.data.frame(cor(afq_final[, c("masq_87", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_88<-as.data.frame(cor(afq_final[, c("masq_88", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_89<-as.data.frame(cor(afq_final[, c("masq_89", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))


## Remove Item Colum

masq_01<-masq_01[,-1]
masq_03<-masq_03[,-1]
masq_14<-masq_14[,-1]
masq_18<-masq_18[,-1]
masq_19<-masq_19[,-1]
masq_21<-masq_21[,-1]
masq_23<-masq_23[,-1]
masq_25<-masq_25[,-1]
masq_26<-masq_26[,-1]
masq_27<-masq_27[,-1]
masq_30<-masq_30[,-1]
masq_33<-masq_33[,-1]
masq_35<-masq_35[,-1]
masq_36<-masq_36[,-1]
masq_39<-masq_39[,-1]
masq_40<-masq_40[,-1]
masq_44<-masq_44[,-1]
masq_45<-masq_45[,-1]
masq_48<-masq_48[,-1]
masq_49<-masq_49[,-1]
masq_52<-masq_52[,-1]
masq_53<-masq_53[,-1]
masq_55<-masq_55[,-1]
masq_57<-masq_57[,-1]
masq_58<-masq_58[,-1]
masq_61<-masq_61[,-1]
masq_66<-masq_66[,-1]
masq_67<-masq_67[,-1]
masq_69<-masq_69[,-1]
masq_72<-masq_72[,-1]
masq_73<-masq_73[,-1]
masq_75<-masq_75[,-1]
masq_78<-masq_78[,-1]
masq_79<-masq_79[,-1]
masq_85<-masq_85[,-1]
masq_86<-masq_86[,-1]
masq_87<-masq_87[,-1]
masq_88<-masq_88[,-1]
masq_89<-masq_89[,-1]




## append all data frames into one

masq_cor_table<- dplyr::bind_rows(masq_01,masq_03,masq_14,masq_18,masq_19,masq_21,masq_23,masq_25,masq_26,
                                  masq_27,masq_30,masq_33,masq_35,masq_36,masq_39,masq_40,masq_44,masq_45,
                                  masq_48,masq_49,masq_52,masq_53,masq_55,masq_57,masq_58,masq_61,masq_66,
                                  masq_67,masq_69,masq_72,masq_73,masq_75,masq_78,masq_79,masq_85,masq_86,
                                  masq_87,masq_88,masq_89)

## clear total score rows
ind <- seq(1, nrow(masq_cor_table), by=5)
masq_cor_table<-masq_cor_table[ind, ]


#################################################################
## Collect correlations to MASQ/PSWQ totals of "high-freezers" ##
#################################################################


afq_1_cor<-as.data.frame(cor(afq_high[, c("afqs_1", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_2_cor<-as.data.frame(cor(afq_high[, c("afqs_2", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_3_cor<-as.data.frame(cor(afq_high[, c("afqs_3", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_6_cor<-as.data.frame(cor(afq_high[, c("afqs_6", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_7_cor<-as.data.frame(cor(afq_high[, c("afqs_7", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_8_cor<-as.data.frame(cor(afq_high[, c("afqs_8", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_9_cor<-as.data.frame(cor(afq_high[, c("afqs_9", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_10_cor<-as.data.frame(cor(afq_high[, c("afqs_10", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_14_cor<-as.data.frame(cor(afq_high[, c("afqs_14", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_16_cor<-as.data.frame(cor(afq_high[, c("afqs_16", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_18_cor<-as.data.frame(cor(afq_high[, c("afqs_18", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_22_cor<-as.data.frame(cor(afq_high[, c("afqs_22", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_24_cor<-as.data.frame(cor(afq_high[, c("afqs_24", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_30_cor<-as.data.frame(cor(afq_high[, c("afqs_30", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_36_cor<-as.data.frame(cor(afq_high[, c("afqs_36", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_37_cor<-as.data.frame(cor(afq_high[, c("afqs_37", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_41_cor<-as.data.frame(cor(afq_high[, c("afqs_41", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_42_cor<-as.data.frame(cor(afq_high[, c("afqs_42", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_43_cor<-as.data.frame(cor(afq_high[, c("afqs_43", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_51_cor<-as.data.frame(cor(afq_high[, c("afqs_51", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_62_cor<-as.data.frame(cor(afq_high[, c("afqs_62", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_64_cor<-as.data.frame(cor(afq_high[, c("afqs_64", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_65_cor<-as.data.frame(cor(afq_high[, c("afqs_65", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_66_cor<-as.data.frame(cor(afq_high[, c("afqs_66", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_67_cor<-as.data.frame(cor(afq_high[, c("afqs_67", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_68_cor<-as.data.frame(cor(afq_high[, c("afqs_68", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_69_cor<-as.data.frame(cor(afq_high[, c("afqs_69", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_70_cor<-as.data.frame(cor(afq_high[, c("afqs_70", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))

## Remove Item Colum
afq_1_cor<-afq_1_cor[,-1]
afq_2_cor<-afq_2_cor[,-1]
afq_3_cor<-afq_3_cor[,-1]
afq_6_cor<-afq_6_cor[,-1]
afq_7_cor<-afq_7_cor[,-1]
afq_8_cor<-afq_8_cor[,-1]
afq_9_cor<-afq_9_cor[,-1]
afq_10_cor<-afq_10_cor[,-1]
afq_14_cor<-afq_14_cor[,-1]
afq_16_cor<-afq_16_cor[,-1]
afq_18_cor<-afq_18_cor[,-1]
afq_22_cor<-afq_22_cor[,-1]
afq_24_cor<-afq_24_cor[,-1]
afq_30_cor<-afq_30_cor[,-1]
afq_36_cor<-afq_36_cor[,-1]
afq_37_cor<-afq_37_cor[,-1]
afq_41_cor<-afq_41_cor[,-1]
afq_42_cor<-afq_42_cor[,-1]
afq_43_cor<-afq_43_cor[,-1]
afq_51_cor<-afq_51_cor[,-1]
afq_62_cor<-afq_62_cor[,-1]
afq_64_cor<-afq_64_cor[,-1]
afq_65_cor<-afq_65_cor[,-1]
afq_66_cor<-afq_66_cor[,-1]
afq_67_cor<-afq_67_cor[,-1]
afq_68_cor<-afq_68_cor[,-1]
afq_69_cor<-afq_69_cor[,-1]
afq_70_cor<-afq_70_cor[,-1]

## append all data frames into one

afq_cor_table<- dplyr::bind_rows(afq_1_cor,afq_2_cor, afq_3_cor, afq_6_cor, afq_7_cor, afq_8_cor, 
                                 afq_9_cor, afq_10_cor, afq_14_cor,  afq_16_cor, afq_18_cor, afq_22_cor, 
                                 afq_24_cor,  afq_30_cor, afq_36_cor, afq_37_cor, afq_41_cor, afq_42_cor, 
                                 afq_43_cor, afq_51_cor, afq_62_cor, afq_64_cor, afq_65_cor, afq_66_cor, 
                                 afq_67_cor, afq_68_cor, afq_69_cor, afq_70_cor)

## clear total score rows
ind <- seq(1, nrow(afq_cor_table), by=7)
afq_cor_table<-afq_cor_table[ind, ]

item_final<-c("afq_1","afq_2","afq_3","afq_6","afq_7","afq_8","afq_9","afq_10",
              "afq_14","afq_16","afq_18","afq_22","afq_24","afq_30","afq_36","afq_37",
              "afq_41","afq_42","afq_43","afq_51","afq_62","afq_64","afq_65","afq_66","afq_67",
              "afq_68","afq_69", "afq_70")

subscale<-c("cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive",
            "physical", "physical", "physical", "physical", 
            "physical", "physical", "physical", 
            "social", "social", "social", "social", "social", 
            "social", "social", "social")


afq_cor_table<-data.frame(item_final, afq_cor_table, subscale)
colnames(afq_cor_table)<-(c("item", "afq_total","pswq_total","masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total","subscale"))

######################################################################################
##### Now, let's see how PSWQ and MASQ items map onto AFQ total in high freezers ##### 
######################################################################################


## PSWQ
pswq_01<-as.data.frame(cor(afq_high[, c("pswq_01", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_02<-as.data.frame(cor(afq_high[, c("pswq_02", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_03<-as.data.frame(cor(afq_high[, c("pswq_03", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_04<-as.data.frame(cor(afq_high[, c("pswq_04", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_05<-as.data.frame(cor(afq_high[, c("pswq_05", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_06<-as.data.frame(cor(afq_high[, c("pswq_06", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_07<-as.data.frame(cor(afq_high[, c("pswq_07", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_08<-as.data.frame(cor(afq_high[, c("pswq_08", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_09<-as.data.frame(cor(afq_high[, c("pswq_09", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_10<-as.data.frame(cor(afq_high[, c("pswq_10", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_11<-as.data.frame(cor(afq_high[, c("pswq_11", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_12<-as.data.frame(cor(afq_high[, c("pswq_12", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_13<-as.data.frame(cor(afq_high[, c("pswq_13", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_14<-as.data.frame(cor(afq_high[, c("pswq_14", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_15<-as.data.frame(cor(afq_high[, c("pswq_15", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_16<-as.data.frame(cor(afq_high[, c("pswq_16", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))


## Remove Item Colum
pswq_01<-pswq_01[,-1]
pswq_02<-pswq_02[,-1]
pswq_03<-pswq_03[,-1]
pswq_04<-pswq_04[,-1]
pswq_05<-pswq_05[,-1]
pswq_06<-pswq_06[,-1]
pswq_07<-pswq_07[,-1]
pswq_08<-pswq_08[,-1]
pswq_09<-pswq_09[,-1]
pswq_10<-pswq_10[,-1]
pswq_11<-pswq_11[,-1]
pswq_12<-pswq_12[,-1]
pswq_13<-pswq_13[,-1]
pswq_14<-pswq_14[,-1]
pswq_15<-pswq_15[,-1]
pswq_16<-pswq_16[,-1]


## append all data frames into one

pswq_cor_table<- dplyr::bind_rows(pswq_01,pswq_02,pswq_03,pswq_04,pswq_05,pswq_06,pswq_07,
                                  pswq_08,pswq_09,pswq_10,pswq_11,pswq_12,pswq_13,pswq_14,
                                  pswq_15,pswq_16)

## clear total score rows
ind <- seq(1, nrow(pswq_cor_table), by=5)
pswq_cor_table<-pswq_cor_table[ind, ]


## MASQ
masq_01<-as.data.frame(cor(afq_high[, c("masq_01", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_03<-as.data.frame(cor(afq_high[, c("masq_03", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_14<-as.data.frame(cor(afq_high[, c("masq_14", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_18<-as.data.frame(cor(afq_high[, c("masq_18", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_19<-as.data.frame(cor(afq_high[, c("masq_19", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_21<-as.data.frame(cor(afq_high[, c("masq_21", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_23<-as.data.frame(cor(afq_high[, c("masq_23", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_25<-as.data.frame(cor(afq_high[, c("masq_25", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_26<-as.data.frame(cor(afq_high[, c("masq_26", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_27<-as.data.frame(cor(afq_high[, c("masq_27", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_30<-as.data.frame(cor(afq_high[, c("masq_30", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_33<-as.data.frame(cor(afq_high[, c("masq_33", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_35<-as.data.frame(cor(afq_high[, c("masq_35", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_36<-as.data.frame(cor(afq_high[, c("masq_36", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_39<-as.data.frame(cor(afq_high[, c("masq_39", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_40<-as.data.frame(cor(afq_high[, c("masq_40", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_44<-as.data.frame(cor(afq_high[, c("masq_44", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_45<-as.data.frame(cor(afq_high[, c("masq_45", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_48<-as.data.frame(cor(afq_high[, c("masq_48", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_49<-as.data.frame(cor(afq_high[, c("masq_49", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_52<-as.data.frame(cor(afq_high[, c("masq_52", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_53<-as.data.frame(cor(afq_high[, c("masq_53", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_55<-as.data.frame(cor(afq_high[, c("masq_55", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_57<-as.data.frame(cor(afq_high[, c("masq_57", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_58<-as.data.frame(cor(afq_high[, c("masq_58", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_61<-as.data.frame(cor(afq_high[, c("masq_61", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_66<-as.data.frame(cor(afq_high[, c("masq_66", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_67<-as.data.frame(cor(afq_high[, c("masq_67", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_69<-as.data.frame(cor(afq_high[, c("masq_69", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_72<-as.data.frame(cor(afq_high[, c("masq_72", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_73<-as.data.frame(cor(afq_high[, c("masq_73", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_75<-as.data.frame(cor(afq_high[, c("masq_75", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_78<-as.data.frame(cor(afq_high[, c("masq_78", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_79<-as.data.frame(cor(afq_high[, c("masq_79", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_85<-as.data.frame(cor(afq_high[, c("masq_85", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_86<-as.data.frame(cor(afq_high[, c("masq_86", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_87<-as.data.frame(cor(afq_high[, c("masq_87", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_88<-as.data.frame(cor(afq_high[, c("masq_88", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_89<-as.data.frame(cor(afq_high[, c("masq_89", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))


## Remove Item Colum

masq_01<-masq_01[,-1]
masq_03<-masq_03[,-1]
masq_14<-masq_14[,-1]
masq_18<-masq_18[,-1]
masq_19<-masq_19[,-1]
masq_21<-masq_21[,-1]
masq_23<-masq_23[,-1]
masq_25<-masq_25[,-1]
masq_26<-masq_26[,-1]
masq_27<-masq_27[,-1]
masq_30<-masq_30[,-1]
masq_33<-masq_33[,-1]
masq_35<-masq_35[,-1]
masq_36<-masq_36[,-1]
masq_39<-masq_39[,-1]
masq_40<-masq_40[,-1]
masq_44<-masq_44[,-1]
masq_45<-masq_45[,-1]
masq_48<-masq_48[,-1]
masq_49<-masq_49[,-1]
masq_52<-masq_52[,-1]
masq_53<-masq_53[,-1]
masq_55<-masq_55[,-1]
masq_57<-masq_57[,-1]
masq_58<-masq_58[,-1]
masq_61<-masq_61[,-1]
masq_66<-masq_66[,-1]
masq_67<-masq_67[,-1]
masq_69<-masq_69[,-1]
masq_72<-masq_72[,-1]
masq_73<-masq_73[,-1]
masq_75<-masq_75[,-1]
masq_78<-masq_78[,-1]
masq_79<-masq_79[,-1]
masq_85<-masq_85[,-1]
masq_86<-masq_86[,-1]
masq_87<-masq_87[,-1]
masq_88<-masq_88[,-1]
masq_89<-masq_89[,-1]




## append all data frames into one

masq_cor_table<- dplyr::bind_rows(masq_01,masq_03,masq_14,masq_18,masq_19,masq_21,masq_23,masq_25,masq_26,
                                  masq_27,masq_30,masq_33,masq_35,masq_36,masq_39,masq_40,masq_44,masq_45,
                                  masq_48,masq_49,masq_52,masq_53,masq_55,masq_57,masq_58,masq_61,masq_66,
                                  masq_67,masq_69,masq_72,masq_73,masq_75,masq_78,masq_79,masq_85,masq_86,
                                  masq_87,masq_88,masq_89)

## clear total score rows
ind <- seq(1, nrow(masq_cor_table), by=5)
masq_cor_table<-masq_cor_table[ind, ]




#################################################################
## Collect correlations to MASQ/PSWQ totals of "low-freezers" ##
#################################################################


afq_1_cor<-as.data.frame(cor(afq_low[, c("afqs_1", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_2_cor<-as.data.frame(cor(afq_low[, c("afqs_2", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_3_cor<-as.data.frame(cor(afq_low[, c("afqs_3", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_6_cor<-as.data.frame(cor(afq_low[, c("afqs_6", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_7_cor<-as.data.frame(cor(afq_low[, c("afqs_7", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_8_cor<-as.data.frame(cor(afq_low[, c("afqs_8", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_9_cor<-as.data.frame(cor(afq_low[, c("afqs_9", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_10_cor<-as.data.frame(cor(afq_low[, c("afqs_10", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_14_cor<-as.data.frame(cor(afq_low[, c("afqs_14", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_16_cor<-as.data.frame(cor(afq_low[, c("afqs_16", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_18_cor<-as.data.frame(cor(afq_low[, c("afqs_18", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_22_cor<-as.data.frame(cor(afq_low[, c("afqs_22", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_24_cor<-as.data.frame(cor(afq_low[, c("afqs_24", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_30_cor<-as.data.frame(cor(afq_low[, c("afqs_30", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_36_cor<-as.data.frame(cor(afq_low[, c("afqs_36", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_37_cor<-as.data.frame(cor(afq_low[, c("afqs_37", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_41_cor<-as.data.frame(cor(afq_low[, c("afqs_41", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_42_cor<-as.data.frame(cor(afq_low[, c("afqs_42", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_43_cor<-as.data.frame(cor(afq_low[, c("afqs_43", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_51_cor<-as.data.frame(cor(afq_low[, c("afqs_51", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_62_cor<-as.data.frame(cor(afq_low[, c("afqs_62", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_64_cor<-as.data.frame(cor(afq_low[, c("afqs_64", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_65_cor<-as.data.frame(cor(afq_low[, c("afqs_65", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_66_cor<-as.data.frame(cor(afq_low[, c("afqs_66", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_67_cor<-as.data.frame(cor(afq_low[, c("afqs_67", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_68_cor<-as.data.frame(cor(afq_low[, c("afqs_68", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_69_cor<-as.data.frame(cor(afq_low[, c("afqs_69", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))
afq_70_cor<-as.data.frame(cor(afq_low[, c("afqs_70", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total")], method = "pearson"))

## Remove Item Colum
afq_1_cor<-afq_1_cor[,-1]
afq_2_cor<-afq_2_cor[,-1]
afq_3_cor<-afq_3_cor[,-1]
afq_6_cor<-afq_6_cor[,-1]
afq_7_cor<-afq_7_cor[,-1]
afq_8_cor<-afq_8_cor[,-1]
afq_9_cor<-afq_9_cor[,-1]
afq_10_cor<-afq_10_cor[,-1]
afq_14_cor<-afq_14_cor[,-1]
afq_16_cor<-afq_16_cor[,-1]
afq_18_cor<-afq_18_cor[,-1]
afq_22_cor<-afq_22_cor[,-1]
afq_24_cor<-afq_24_cor[,-1]
afq_30_cor<-afq_30_cor[,-1]
afq_36_cor<-afq_36_cor[,-1]
afq_37_cor<-afq_37_cor[,-1]
afq_41_cor<-afq_41_cor[,-1]
afq_42_cor<-afq_42_cor[,-1]
afq_43_cor<-afq_43_cor[,-1]
afq_51_cor<-afq_51_cor[,-1]
afq_62_cor<-afq_62_cor[,-1]
afq_64_cor<-afq_64_cor[,-1]
afq_65_cor<-afq_65_cor[,-1]
afq_66_cor<-afq_66_cor[,-1]
afq_67_cor<-afq_67_cor[,-1]
afq_68_cor<-afq_68_cor[,-1]
afq_69_cor<-afq_69_cor[,-1]
afq_70_cor<-afq_70_cor[,-1]

## append all data frames into one

afq_cor_table<- dplyr::bind_rows(afq_1_cor,afq_2_cor, afq_3_cor, afq_6_cor, afq_7_cor, afq_8_cor, 
                                 afq_9_cor, afq_10_cor, afq_14_cor,  afq_16_cor, afq_18_cor, afq_22_cor, 
                                 afq_24_cor,  afq_30_cor, afq_36_cor, afq_37_cor, afq_41_cor, afq_42_cor, 
                                 afq_43_cor, afq_51_cor, afq_62_cor, afq_64_cor, afq_65_cor, afq_66_cor, 
                                 afq_67_cor, afq_68_cor, afq_69_cor, afq_70_cor)

## clear total score rows
ind <- seq(1, nrow(afq_cor_table), by=7)
afq_cor_table<-afq_cor_table[ind, ]

item_final<-c("afq_1","afq_2","afq_3","afq_6","afq_7","afq_8","afq_9","afq_10",
              "afq_14","afq_16","afq_18","afq_22","afq_24","afq_30","afq_36","afq_37",
              "afq_41","afq_42","afq_43","afq_51","afq_62","afq_64","afq_65","afq_66","afq_67",
              "afq_68","afq_69", "afq_70")

subscale<-c("cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive", "cognitive", "cognitive", "cognitive", 
            "cognitive",
            "physical", "physical", "physical", "physical", 
            "physical", "physical", "physical", 
            "social", "social", "social", "social", "social", 
            "social", "social", "social")


afq_cor_table<-data.frame(item_final, afq_cor_table, subscale)
colnames(afq_cor_table)<-(c("item", "afq_total","pswq_total","masq_aa_total", "masq_ad_total", "masq_lpa_total", "masq_dm_total","subscale"))

######################################################################################
##### Now, let's see how PSWQ and MASQ items map onto AFQ total in high freezers ##### 
######################################################################################


## PSWQ
pswq_01<-as.data.frame(cor(afq_low[, c("pswq_01", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_02<-as.data.frame(cor(afq_low[, c("pswq_02", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_03<-as.data.frame(cor(afq_low[, c("pswq_03", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_04<-as.data.frame(cor(afq_low[, c("pswq_04", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_05<-as.data.frame(cor(afq_low[, c("pswq_05", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_06<-as.data.frame(cor(afq_low[, c("pswq_06", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_07<-as.data.frame(cor(afq_low[, c("pswq_07", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_08<-as.data.frame(cor(afq_low[, c("pswq_08", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_09<-as.data.frame(cor(afq_low[, c("pswq_09", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_10<-as.data.frame(cor(afq_low[, c("pswq_10", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_11<-as.data.frame(cor(afq_low[, c("pswq_11", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_12<-as.data.frame(cor(afq_low[, c("pswq_12", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_13<-as.data.frame(cor(afq_low[, c("pswq_13", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_14<-as.data.frame(cor(afq_low[, c("pswq_14", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_15<-as.data.frame(cor(afq_low[, c("pswq_15", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
pswq_16<-as.data.frame(cor(afq_low[, c("pswq_16", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))


## Remove Item Colum
pswq_01<-pswq_01[,-1]
pswq_02<-pswq_02[,-1]
pswq_03<-pswq_03[,-1]
pswq_04<-pswq_04[,-1]
pswq_05<-pswq_05[,-1]
pswq_06<-pswq_06[,-1]
pswq_07<-pswq_07[,-1]
pswq_08<-pswq_08[,-1]
pswq_09<-pswq_09[,-1]
pswq_10<-pswq_10[,-1]
pswq_11<-pswq_11[,-1]
pswq_12<-pswq_12[,-1]
pswq_13<-pswq_13[,-1]
pswq_14<-pswq_14[,-1]
pswq_15<-pswq_15[,-1]
pswq_16<-pswq_16[,-1]


## append all data frames into one

pswq_cor_table<- dplyr::bind_rows(pswq_01,pswq_02,pswq_03,pswq_04,pswq_05,pswq_06,pswq_07,
                                  pswq_08,pswq_09,pswq_10,pswq_11,pswq_12,pswq_13,pswq_14,
                                  pswq_15,pswq_16)

## clear total score rows
ind <- seq(1, nrow(pswq_cor_table), by=5)
pswq_cor_table<-pswq_cor_table[ind, ]


## MASQ
masq_01<-as.data.frame(cor(afq_low[, c("masq_01", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_03<-as.data.frame(cor(afq_low[, c("masq_03", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_14<-as.data.frame(cor(afq_low[, c("masq_14", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_18<-as.data.frame(cor(afq_low[, c("masq_18", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_19<-as.data.frame(cor(afq_low[, c("masq_19", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_21<-as.data.frame(cor(afq_low[, c("masq_21", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_23<-as.data.frame(cor(afq_low[, c("masq_23", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_25<-as.data.frame(cor(afq_low[, c("masq_25", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_26<-as.data.frame(cor(afq_low[, c("masq_26", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_27<-as.data.frame(cor(afq_low[, c("masq_27", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_30<-as.data.frame(cor(afq_low[, c("masq_30", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_33<-as.data.frame(cor(afq_low[, c("masq_33", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_35<-as.data.frame(cor(afq_low[, c("masq_35", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_36<-as.data.frame(cor(afq_low[, c("masq_36", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_39<-as.data.frame(cor(afq_low[, c("masq_39", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_40<-as.data.frame(cor(afq_low[, c("masq_40", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_44<-as.data.frame(cor(afq_low[, c("masq_44", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_45<-as.data.frame(cor(afq_low[, c("masq_45", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_48<-as.data.frame(cor(afq_low[, c("masq_48", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_49<-as.data.frame(cor(afq_low[, c("masq_49", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_52<-as.data.frame(cor(afq_low[, c("masq_52", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_53<-as.data.frame(cor(afq_low[, c("masq_53", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_55<-as.data.frame(cor(afq_low[, c("masq_55", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_57<-as.data.frame(cor(afq_low[, c("masq_57", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_58<-as.data.frame(cor(afq_low[, c("masq_58", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_61<-as.data.frame(cor(afq_low[, c("masq_61", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_66<-as.data.frame(cor(afq_low[, c("masq_66", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_67<-as.data.frame(cor(afq_low[, c("masq_67", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_69<-as.data.frame(cor(afq_low[, c("masq_69", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_72<-as.data.frame(cor(afq_low[, c("masq_72", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_73<-as.data.frame(cor(afq_low[, c("masq_73", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_75<-as.data.frame(cor(afq_low[, c("masq_75", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_78<-as.data.frame(cor(afq_low[, c("masq_78", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_79<-as.data.frame(cor(afq_low[, c("masq_79", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_85<-as.data.frame(cor(afq_low[, c("masq_85", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_86<-as.data.frame(cor(afq_low[, c("masq_86", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_87<-as.data.frame(cor(afq_low[, c("masq_87", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_88<-as.data.frame(cor(afq_low[, c("masq_88", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))
masq_89<-as.data.frame(cor(afq_low[, c("masq_89", "afq_total", "pswq_total", "masq_aa_total", "masq_ad_total")], method = "pearson"))


## Remove Item Colum

masq_01<-masq_01[,-1]
masq_03<-masq_03[,-1]
masq_14<-masq_14[,-1]
masq_18<-masq_18[,-1]
masq_19<-masq_19[,-1]
masq_21<-masq_21[,-1]
masq_23<-masq_23[,-1]
masq_25<-masq_25[,-1]
masq_26<-masq_26[,-1]
masq_27<-masq_27[,-1]
masq_30<-masq_30[,-1]
masq_33<-masq_33[,-1]
masq_35<-masq_35[,-1]
masq_36<-masq_36[,-1]
masq_39<-masq_39[,-1]
masq_40<-masq_40[,-1]
masq_44<-masq_44[,-1]
masq_45<-masq_45[,-1]
masq_48<-masq_48[,-1]
masq_49<-masq_49[,-1]
masq_52<-masq_52[,-1]
masq_53<-masq_53[,-1]
masq_55<-masq_55[,-1]
masq_57<-masq_57[,-1]
masq_58<-masq_58[,-1]
masq_61<-masq_61[,-1]
masq_66<-masq_66[,-1]
masq_67<-masq_67[,-1]
masq_69<-masq_69[,-1]
masq_72<-masq_72[,-1]
masq_73<-masq_73[,-1]
masq_75<-masq_75[,-1]
masq_78<-masq_78[,-1]
masq_79<-masq_79[,-1]
masq_85<-masq_85[,-1]
masq_86<-masq_86[,-1]
masq_87<-masq_87[,-1]
masq_88<-masq_88[,-1]
masq_89<-masq_89[,-1]




## append all data frames into one

masq_cor_table<- dplyr::bind_rows(masq_01,masq_03,masq_14,masq_18,masq_19,masq_21,masq_23,masq_25,masq_26,
                                  masq_27,masq_30,masq_33,masq_35,masq_36,masq_39,masq_40,masq_44,masq_45,
                                  masq_48,masq_49,masq_52,masq_53,masq_55,masq_57,masq_58,masq_61,masq_66,
                                  masq_67,masq_69,masq_72,masq_73,masq_75,masq_78,masq_79,masq_85,masq_86,
                                  masq_87,masq_88,masq_89)

## clear total score rows
ind <- seq(1, nrow(masq_cor_table), by=5)
masq_cor_table<-masq_cor_table[ind, ]

write.csv(afq_cor_table,"high_afq_cor_table.csv", row.names = TRUE)
write.csv(pswq_cor_table,"high_pswq_cor_table.csv", row.names = TRUE)
write.csv(masq_cor_table,"high_masq_cor_table.csv", row.names = TRUE)


########################################################################################
##### Predicting Freeze using PSWQ and MASQ items - high freeze & full sample test ##### 
########################################################################################


#6-items .38 correlation threshold
fullsample_trio_set6<-afq_final
fullsample_trio_set6<-fullsample_trio_set6[,c(1:28,30,42,43,54,67,72,87)]
fullsample_trio_set6$trio_total<-rowSums(fullsample_trio_set6[, c(29:34)])

cor.test(fullsample_trio_set6$afq_total,fullsample_trio_set6$trio_total, method="pearson")



#10-items

fullsample_trio_set10<-afq_final
fullsample_trio_set10<-fullsample_trio_set10[,c(1:28,30,35,42,43,54,63,67,69,72,77,87)]
fullsample_trio_set10$trio_total<-rowSums(fullsample_trio_set10[, c(29:38)])

cor.test(fullsample_trio_set10$afq_total,fullsample_trio_set10$trio_total, method="pearson")

#11-items .36 correlation threshold
fullsample_trio_set11<-afq_final
fullsample_trio_set11<-fullsample_trio_set11[,c(1:28,30,33,35,42,43,54,63,67,69,72,77,87)]
fullsample_trio_set11$trio_total<-rowSums(fullsample_trio_set11[, c(29:39)])

cor.test(fullsample_trio_set11$afq_total,fullsample_trio_set11$trio_total, method="pearson")

#13-items
fullsample_trio_set13<-afq_final
fullsample_trio_set13<-fullsample_trio_set13[,c(1:28,30,33,35,42,43,53,54,63,67,69,72,77,79,87)]
fullsample_trio_set13$trio_total<-rowSums(fullsample_trio_set13[, c(29:41)])

cor.test(fullsample_trio_set13$afq_total,fullsample_trio_set13$trio_total, method="pearson")

#14-items .34 correlation threshold
fullsample_trio_set14<-afq_final
fullsample_trio_set14<-fullsample_trio_set14[,c(1:28,30,33,35,41,42,43,53,54,63,67,69,72,77,79,87)]
fullsample_trio_set14$trio_total<-rowSums(fullsample_trio_set14[, c(29:42)])

cor.test(fullsample_trio_set14$afq_total,fullsample_trio_set14$trio_total, method="pearson")

#17-items
fullsample_trio_set17<-afq_final
fullsample_trio_set17<-fullsample_trio_set17[,c(1:28,30,32,33,35,40,41,42,43,53,54,60,63,67,69,72,77,79,87)]
fullsample_trio_set17$trio_total<-rowSums(fullsample_trio_set17[, c(29:45)])

cor.test(fullsample_trio_set17$afq_total,fullsample_trio_set17$trio_total, method="pearson")


####################################################
#######    Run EFA on final AFQ item set    ########
####################################################


## Set data to include anxious freezing symptom values ##
efa_frzfin<-afq_final


## Determine number of factors to extract ##

## Get eigenvalues ##
ev<-eigen(cor(efa_frzfin))

## conduct parallel analysis ##
ap<-parallel(subject = nrow(efa_frzfin), var=ncol(efa_frzfin), rep=100, cent=0.05)


## Show analysis of factors to retain ##
nS<-nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

## Look for the number of components associated to the last severe slope change (i.e. what is the number of components before the slope flattens) ##
## Remember to stay flexible ##


## After # of factors has been decided upon, rotate your data ##

## orthogonal rotation - assumes no correlation between components/factors ##
efa.ort<-fa(efa_frzfin, nfactors = 3, rotate = "varimax")
print(efa.ort)

## Let's make this easier to read and sort by loading above .3 for each factor##
print(efa.ort, cut=.4)


## How about oblique rotation - assumes correlation between components/factors ##
efa.obl.1<-fa(efa_frzfin, nfactors = 1, rotate = "oblimin")
print(efa.obl.1)

efa.obl.2<-fa(efa_frzfin, nfactors = 2, rotate = "oblimin")
print(efa.obl.2, cut=.4, sort=T)

efa.obl.3<-fa(efa_frzfin, nfactors = 3, rotate = "oblimin")
print(efa.obl.3)
print(efa.obl.3, cut=.4, sort = T)



factor.congruence(efa.obl.2,efa.obl.3)

efa.obl.4<-fa(efa_frzfin, nfactors = 4, rotate = "oblimin")
print(efa.obl.4, cut=.4, sort = T)
factor.congruence(efa.obl.3,efa.obl.4)

efa.obl.5<-fa(efa_frzfin, nfactors = 5, rotate = "oblimin")
print(efa.obl.5, cut=.4, sort=T)

efa.obl.6<-fa(efa_frzfin, nfactors = 6, rotate = "oblimin")
print(efa.obl.6, cut=.4, sort=T)

efa.obl.7<-fa(efa_frzfin, nfactors = 7, rotate = "oblimin")
print(efa.obl.7, cut=.6, sort=T)

efa.obl.8<-fa(efa_frzfin, nfactors = 8, rotate = "oblimin")
print(efa.obl.8, cut=.6, sort=T)


## Let's make this easier to read and sort by loading above .3 for each factor ##
print(efa.obl, cut=.5)
print(efa.obl.5, cut=.5)

# how similar are these solutions? Let's check with a congruence coefficient ##
factor.congruence(efa.obl.5,efa.obl.7)



psych::describe(freezing_raw_d$afqs_3)
psych::describe(freezing_raw_d$afqs_14)
psych::describe(freezing_raw_d$afqs_30)
plot(freezing_raw_d$afqs_3)

hist(freezing_raw_d$afqs_3)
hist(freezing_raw_d$afqs_14)
hist(freezing_raw_d$afqs_30)

hist(freezing_raw_d$afqs_7)
efa_frzfin


efa.frz.3<-fa(efa_frzfin, nfactors = 3, rotate = "oblimin")
print(efa.frz.3, cut=.4, sort = T)

frz.cfa<-'
S  =~ afqs_62 + afqs_64 + afqs_65 + afqs_66 + afqs_67 + afqs_68 + afqs_69 + afqs_70
C =~ afqs_1 + afqs_2 + afqs_6 + afqs_8 + afqs_9 + afqs_10 + afqs_16 + afqs_18
P  =~ afqs_7 + afqs_22 + afqs_36 + afqs_37 + afqs_41 + afqs_42 + afqs_43 + afqs_51
'
## Get factor totals and extract pertinent data for SEM proposal

freeze_data<-freezing_raw_d_age

freeze_data$afq_soc<-rowSums(freezing_raw_d_age[,c("afqs_62","afqs_64","afqs_65","afqs_66","afqs_67","afqs_68","afqs_69","afqs_70")])
freeze_data$afq_phy<-rowSums(freezing_raw_d_age[,c("afqs_7","afqs_22","afqs_36","afqs_37","afqs_41","afqs_42","afqs_43","afqs_51")])
freeze_data$afq_cog<-rowSums(freezing_raw_d_age[,c("afqs_1","afqs_2","afqs_6","afqs_8","afqs_9","afqs_10","afqs_16","afqs_18")])



freeze_data <- freeze_data[,c("afqs_62","afqs_64","afqs_65","afqs_66","afqs_67","afqs_68","afqs_69","afqs_70",
                              "afqs_7","afqs_22","afqs_36","afqs_37","afqs_41","afqs_42","afqs_43","afqs_51",
                              "afqs_1","afqs_2","afqs_6","afqs_8","afqs_9","afqs_10","afqs_16","afqs_18",
                              "masq_lpa_total", "masq_dm_total", "masq_aa_total", "pswq_total", "rrq_rum_total", "afq_soc", "afq_phy", "afq_cog")]



freeze_data_totals <- freeze_data[,c("masq_lpa_total", "masq_dm_total", "masq_aa_total", "pswq_total", "rrq_rum_total", "afq_soc", "afq_phy", "afq_cog")]


freeze_socitems <- freeze_data[,c("afqs_62","afqs_64","afqs_65","afqs_66","afqs_67","afqs_68","afqs_69","afqs_70",
                                  "afq_soc", "afq_phy", "afq_cog")]

freeze_cogitems <- freeze_data[,c("afqs_1","afqs_2","afqs_6","afqs_8","afqs_9","afqs_10","afqs_16","afqs_18",
                                  "afq_soc", "afq_phy", "afq_cog")]

freeze_physitems <- freeze_data[,c("afqs_7","afqs_22","afqs_36","afqs_37","afqs_41","afqs_42","afqs_43","afqs_51","afq_soc", "afq_phy", "afq_cog")]

install.packages("Hmisc")
library(Hmisc)

rcorr(as.matrix(freeze_data_totals))
x2$P


soc_factor<-freeze_data[,c("afqs_62","afqs_64","afqs_65","afqs_66","afqs_67","afqs_68","afqs_69","afqs_70")]
cronbach(soc_factor)

cog_factor<-freeze_data[,c("afqs_1","afqs_2","afqs_6","afqs_8","afqs_9","afqs_10","afqs_16","afqs_18")]
cronbach(cog_factor)

phys_factor<-freeze_data[,c("afqs_7","afqs_22","afqs_36","afqs_37","afqs_41","afqs_42","afqs_43","afqs_51")]
cronbach(phys_factor)


write.csv(freeze_data, "Freeze_data.csv")

#Lets fit this model and check on how it performed

frz.cfa.fit<-cfa(frz.cfa, data = efa_frzfin, missing = 'fiml')
summary(frz.cfa.fit, fit.measures = T, standardized = T)
write.csv(efa_frzfin, "EFA_Freeze_HW2.csv")

model1<-'afq_soc ~~ afq_soc
         afq_cog ~~ afq_cog
         afq_phy ~~ afq_phy
         rrq_rum_total ~~ rrq_rum_total
         pswq_total ~~ pswq_total
         masq_lpa_total ~~ masq_lpa_total
         masq_dm_total ~~ masq_dm_total
         masq_aa_total ~~ masq_aa_total
         rrq_rum_total ~~ pswq_total
         masq_lpa_total ~~ masq_dm_total
         afq_phy ~ rrq_rum_total + pswq_total + masq_aa_total
         afq_soc ~ rrq_rum_total + pswq_total + masq_aa_total
         afq_cog ~ rrq_rum_total + pswq_total + masq_aa_total
         masq_lpa_total ~ rrq_rum_total + pswq_total
         masq_dm_total ~ rrq_rum_total + pswq_total + masq_aa_total
         masq_aa_total ~ rrq_rum_total + pswq_total
         '

m.fit<-sem(model1, data = freeze_data, missing = 'fiml')

install.packages("semPlot")
library(semPlot)
install.packages("qgraph")
library(qgraph)
install.packages("tidySEM")
library(tidySEM)

graph_sem(model = m.fit)
describe(m.fit)

summary(m.fit, fit.measures = T, standardized = T)





























##############################################
##    Blow up Latent Variable Theory Study  ## 
##############################################

# Step 1: Assess correlations between totals across questionnaires in our Spring 2020 data


pairs.panels(freezing_raw_d_age[,c("masq_ad_total", "pswq_total", "masq_aa_total",
                                   "rrq_total", "atq_appr", "atq_avoi", "panas_pos_total", 
                                   "panas_neg_total", "asi_total", "sias_total", "brief_wm_total", 
                                   "brief_inh_total", "brief_shft_total","brief_emctrl_total")], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB"
)



#################################################################
#################################################################

## OLD ANALYSES NEVER USED ON SPRING 2020 DATA - DEMOGRAPHICS  ##

#################################################################
#################################################################

freezing_demo %>%
  count(life, GENDER)

count(freezing_demo$timepoint,freezing_demo$gender)


## Create pre-spring break & post-spring break groups for discrete testing later
pre_sb<-freezing_raw_d_age %>%
  dplyr::filter(freezing_raw_d_age$record_id < 540)

post_sb<-freezing_raw_d_age %>%
  dplyr::filter(freezing_raw_d_age$record_id >= 540)

## Clean Demographics before importing into freezing raw set
Demographics<- Demographics[,c(2,9:19)]

freezing_demo<-freezing_raw_d_age
names(freezing_demo)[names(freezing_demo) == "name"] <- "name"
freezing_demo<- merge(freezing_demo,Demographics,by="name")

#reorder columns for visibility
freezing_demo$masq_lpa_total<- rowSums(freezing_demo[, c("masq_01","masq_14","masq_18","masq_23","masq_27","masq_30","masq_35","masq_36","masq_40","masq_49",
                                                         "masq_58","masq_72","masq_78","masq_86")])
freezing_demo$masq_dm_total<- rowSums(freezing_demo[,c("masq_44","masq_33", "masq_26", "masq_53","masq_66", "masq_21","masq_39","masq_89")])

freezing_demo_pre_sb<-freezing_demo %>%
  dplyr::filter(freezing_demo$record_id < 540)

freezing_demopost_sb<-freezing_demo %>%
  dplyr::filter(freezing_demo$record_id >= 540)


#quickly assess sample sizes of various variables
count(freezing_demo, vars=RACE)
#should we remove small samples?
freezing_demo_race <- freezing_demo %>%
  dplyr::filter(RACE != "Native Hawaiian or Other Pacific Islander") 

count(freezing_demo, vars="GENDER")
#should we remove small samples? 
#if yes..then
freezing_demo_twogender <- freezing_demo %>%
  dplyr::filter(record_id != 509) %>%
  dplyr::filter(record_id != 81)


count(freezing_demo, vars=CLASS.SELF)
#should we compare extremes (i.e. lower vs upper class?)
#if yes..then
freezing_demo_class <- freezing_demo %>%
  dplyr::filter(CLASS.SELF != "[Decline to Answer]")


count(freezing_demo, vars=ETHNICITY)
#should we remove small samples? 
#if yes..then
freezing_demo_twoethn <- freezing_demo %>%
  dplyr::filter(record_id != 689)

## Create upper and lower class
freezing_demo<-freezing_demo %>%
  mutate(college_class=case_when(
    YEAR.COLLEGE %in% "Freshman or first-year student" ~ "lower class",
    YEAR.COLLEGE %in% "Sophomore" ~ "lower class",
    YEAR.COLLEGE %in% "Junior" ~ "upper class",
    YEAR.COLLEGE %in% "Senior" ~ "upper class",
  ))
freezing_demo_upper<-freezing_demo %>%
  filter(college_class == "upper class")
freezing_demo_lower<-freezing_demo %>%
  filter(college_class == "lower class")

describe(freezing_demo_lower$age)
describe(freezing_demo_upper$age)

#############################################
##      MANOVA - Demographics              ##
#############################################

#Checking Assumptions for freezing_demo dataframe
#Let's check for levels of multicollinearity
YD <- cbind(freezing_demo$pswq_total, freezing_demo$masq_aa_total,freezing_demo$masq_ad_total, freezing_demo$masq_dm_total, freezing_demo$masq_lpa_total)
cor(YD, method = c("pearson"))

# Okay, it seems as if we have very high correlation between masq-ad and masq-ad14 (r = .93), this is a problem, adjust.
# Proposed solution is to assess just the two-factor model of AD? ie utilize masq-AD8 & masq-AD14, but not the general score

box_res_d<-box_m(freezing_demo[,c("pswq_total", "masq_aa_total", "masq_dm_total", "masq_lpa_total")],freezing_demo[,"timepoint"])

# The assumption that of equality of covariance matrices is met
# Test for homogeneity of variance

leveneTest(freezing_demo[,"pswq_total"],freezing_demo[,"timepoint"], location = "median", correction = "zero.connection")
leveneTest(freezing_demo[,"masq_aa_total"],freezing_demo[,"timepoint"], location = "median", correction = "zero.connection")
leveneTest(freezing_demo[,"masq_dm_total"],freezing_demo[,"timepoint"], location = "median", correction = "zero.connection")
leveneTest(freezing_demo[,"masq_lpa_total"],freezing_demo[,"timepoint"], location = "median", correction = "zero.connection")

# The assumption that of equality of variance is met
# Now, let's check for univariate normality, could use shapiro test, but because of high sample size, we will prioritize qq plots

freezing_demo %>%
  group_by(timepoint) %>%
  shapiro_test(pswq_total, masq_aa_total, masq_dm_total, masq_lpa_total) %>%
  arrange(variable)

# look at qq plots
ggqqplot(freezing_demo, "pswq_total", facet.by = "timepoint",
         ylab = "worry", ggtheme = theme_bw())
ggqqplot(freezing_demo, "masq_aa_total", facet.by = "timepoint",
         ylab = "arousal", ggtheme = theme_bw())
ggqqplot(freezing_demo, "masq_dm_total", facet.by = "timepoint",
         ylab = "depressed mood", ggtheme = theme_bw())
ggqqplot(freezing_demo, "masq_lpa_total", facet.by = "timepoint",
         ylab = "low positive affect", ggtheme = theme_bw())

# Now, check multivariate normality
freezing_demo %>%
  select(pswq_total, masq_aa_total, masq_dm_total, masq_lpa_total) %>%
  mshapiro_test()


#################################################
#Checking Assumptions for freezing_demo_twogender dataframe
#Let's check for levels of multicollinearity
YDG <- cbind(freezing_demo_twogender$pswq_total, freezing_demo_twogender$masq_aa_total,freezing_demo_twogender$masq_ad_total, freezing_demo_twogender$masq_dm_total, freezing_demo_twogender$masq_lpa_total)
cor(YDG, method = c("pearson"))

# Okay, it seems as if we have very high correlation between masq-ad and masq-ad14 (r = .93), this is a problem, adjust.
# Proposed solution is to assess just the two-factor model of AD? ie utilize masq-AD8 & masq-AD14, but not the general score

box_res_dg<-box_m(freezing_demo_twogender[,c("pswq_total", "masq_aa_total", "masq_dm_total", "masq_lpa_total")],freezing_demo_twogender[,"timepoint"])

# The assumption that of equality of covariance matrices is met
# Test for homogeneity of variance

leveneTest(freezing_demo_twogender[,"pswq_total"],freezing_demo_twogender[,"timepoint"], location = "median", correction = "zero.connection")
leveneTest(freezing_demo_twogender[,"masq_aa_total"],freezing_demo_twogender[,"timepoint"], location = "median", correction = "zero.connection")
leveneTest(freezing_demo_twogender[,"masq_dm_total"],freezing_demo_twogender[,"timepoint"], location = "median", correction = "zero.connection")
leveneTest(freezing_demo_twogender[,"masq_lpa_total"],freezing_demo_twogender[,"timepoint"], location = "median", correction = "zero.connection")

# The assumption that of equality of variance is met
# Now, let's check for univariate normality, could use shapiro test, but because of high sample size, we will prioritize qq plots

freezing_demo_twogender %>%
  group_by(timepoint) %>%
  shapiro_test(pswq_total, masq_aa_total, masq_dm_total, masq_lpa_total) %>%
  arrange(variable)

# look at qq plots
ggqqplot(freezing_demo_twogender, "pswq_total", facet.by = "timepoint",
         ylab = "worry", ggtheme = theme_bw())
ggqqplot(freezing_demo_twogender, "masq_aa_total", facet.by = "timepoint",
         ylab = "arousal", ggtheme = theme_bw())
ggqqplot(freezing_demo_twogender, "masq_dm_total", facet.by = "timepoint",
         ylab = "depressed mood", ggtheme = theme_bw())
ggqqplot(freezing_demo_twogender, "masq_lpa_total", facet.by = "timepoint",
         ylab = "low positive affect", ggtheme = theme_bw())

# Now, check multivariate normality
freezing_demo_twogender %>%
  select(pswq_total, masq_aa_total, masq_dm_total, masq_lpa_total) %>%
  mshapiro_test()


#################################################
#Checking Assumptions for freezing_demo_twoethn dataframe
#Let's check for levels of multicollinearity
YDE <- cbind(freezing_demo_twoethn$pswq_total, freezing_demo_twoethn$masq_aa_total,freezing_demo_twoethn$masq_ad_total, freezing_demo_twoethn$masq_dm_total, freezing_demo_twoethn$masq_lpa_total)
cor(YDE, method = c("pearson"))

# Okay, it seems as if we have very high correlation between masq-ad and masq-ad14 (r = .93), this is a problem, adjust.
# Proposed solution is to assess just the two-factor model of AD? ie utilize masq-AD8 & masq-AD14, but not the general score

box_res_de<-box_m(freezing_demo_twoethn[,c("pswq_total", "masq_aa_total", "masq_dm_total", "masq_lpa_total")],freezing_demo_twoethn[,"timepoint"])

# The assumption that of equality of covariance matrices is met
# Test for homogeneity of variance

leveneTest(freezing_demo_twoethn[,"pswq_total"],freezing_demo_twoethn[,"timepoint"], location = "median", correction = "zero.connection")
leveneTest(freezing_demo_twoethn[,"masq_aa_total"],freezing_demo_twoethn[,"timepoint"], location = "median", correction = "zero.connection")
leveneTest(freezing_demo_twoethn[,"masq_dm_total"],freezing_demo_twoethn[,"timepoint"], location = "median", correction = "zero.connection")
leveneTest(freezing_demo_twoethn[,"masq_lpa_total"],freezing_demo_twoethn[,"timepoint"], location = "median", correction = "zero.connection")

# The assumption that of equality of variance is met
# Now, let's check for univariate normality, could use shapiro test, but because of high sample size, we will prioritize qq plots

freezing_demo_twoethn %>%
  group_by(timepoint) %>%
  shapiro_test(pswq_total, masq_aa_total, masq_dm_total, masq_lpa_total) %>%
  arrange(variable)

# look at qq plots
ggqqplot(freezing_demo_twoethn, "pswq_total", facet.by = "timepoint",
         ylab = "worry", ggtheme = theme_bw())
ggqqplot(freezing_demo_twoethn, "masq_aa_total", facet.by = "timepoint",
         ylab = "arousal", ggtheme = theme_bw())
ggqqplot(freezing_demo_twoethn, "masq_dm_total", facet.by = "timepoint",
         ylab = "depressed mood", ggtheme = theme_bw())
ggqqplot(freezing_demo_twoethn, "masq_lpa_total", facet.by = "timepoint",
         ylab = "low positive affect", ggtheme = theme_bw())

# Now, check multivariate normality
freezing_demo_twoethn %>%
  select(pswq_total, masq_aa_total, masq_dm_total, masq_lpa_total) %>%
  mshapiro_test()


#################################################
#Checking Assumptions for freezing_demo_race dataframe
#Let's check for levels of multicollinearity
YDR <- cbind(freezing_demo_race$pswq_total, freezing_demo_race$masq_aa_total,freezing_demo_race$masq_ad_total, freezing_demo_race$masq_dm_total, freezing_demo_race$masq_lpa_total)
cor(YDR, method = c("pearson"))

# Okay, it seems as if we have very high correlation between masq-ad and masq-ad14 (r = .93), this is a problem, adjust.
# Proposed solution is to assess just the two-factor model of AD? ie utilize masq-AD8 & masq-AD14, but not the general score

box_res_dr<-box_m(freezing_demo_race[,c("pswq_total", "masq_aa_total", "masq_dm_total", "masq_lpa_total")],freezing_demo_race[,"timepoint"])

# The assumption that of equality of covariance matrices is met
# Test for homogeneity of variance

leveneTest(freezing_demo_race[,"pswq_total"],freezing_demo_race[,"timepoint"], location = "median", correction = "zero.connection")
leveneTest(freezing_demo_race[,"masq_aa_total"],freezing_demo_race[,"timepoint"], location = "median", correction = "zero.connection")
leveneTest(freezing_demo_race[,"masq_dm_total"],freezing_demo_race[,"timepoint"], location = "median", correction = "zero.connection")
leveneTest(freezing_demo_race[,"masq_lpa_total"],freezing_demo_race[,"timepoint"], location = "median", correction = "zero.connection")

# The assumption that of equality of variance is met
# Now, let's check for univariate normality, could use shapiro test, but because of high sample size, we will prioritize qq plots

freezing_demo_race %>%
  group_by(timepoint) %>%
  shapiro_test(pswq_total, masq_aa_total, masq_dm_total, masq_lpa_total) %>%
  arrange(variable)

# look at qq plots
ggqqplot(freezing_demo_race, "pswq_total", facet.by = "timepoint",
         ylab = "worry", ggtheme = theme_bw())
ggqqplot(freezing_demo_race, "masq_aa_total", facet.by = "timepoint",
         ylab = "arousal", ggtheme = theme_bw())
ggqqplot(freezing_demo_race, "masq_dm_total", facet.by = "timepoint",
         ylab = "depressed mood", ggtheme = theme_bw())
ggqqplot(freezing_demo_race, "masq_lpa_total", facet.by = "timepoint",
         ylab = "low positive affect", ggtheme = theme_bw())

# Now, check multivariate normality
freezing_demo_race %>%
  select(pswq_total, masq_aa_total, masq_dm_total, masq_lpa_total) %>%
  mshapiro_test()


#################
# Gender Identity
#################

count(freezing_demo_twogender, c("GENDER", "timepoint"))

A <- lm(cbind(pswq_total, masq_aa_total, masq_dm_total, masq_lpa_total) ~ GENDER*timepoint, freezing_demo_twogender)
Manova(A, test.statistic = "Pillai")

# Report these results in the following manner: "There was a statistically significant difference between the 
# binary gender identities on the combined dependent variables (pswq, masq-aa, masq-lpa & masq-dm), Pillai's Trace = .123, F(4,182) = 6.37, p< 0.0001

# A significant Manova can be followed by univariate one-way ANOVA to examine each dependent variable

grouped.data <- freezing_demo_twogender %>%
  gather(key = "variable", value = "value", pswq_total, masq_aa_total, masq_lpa_total, masq_dm_total) %>%
  group_by(variable)

grouped.data %>% anova_test(value ~ GENDER)

## Report these results in the following manner: "Follow-up univariate ANOVAs, using a Bonferroni adjusted alpha level of 0.0125, showed that there was a 
#statistically significant difference in PSWQ (F(1,185)= 19, p < .00001) between binary gender identity

# Now, we need to check pairwise comparisons

pwcg <- freezing_demo_twogender %>%
  gather(key = "variable", value = "value", pswq_total, masq_aa_total, masq_lpa_total, masq_dm_total) %>%
  group_by(variable) %>%
  games_howell_test(value ~ GENDER)
pwcg

#Report these results in the following manner: "Pairwise comparisons between groups confirmed the significant difference between groups such that cisgender females
# had higher pswq scores


#################
# Ethnicity
#################

count(freezing_demo_twoethn, c("ETHNICITY", "timepoint"))


B <- lm(cbind(pswq_total, masq_aa_total, masq_dm_total, masq_lpa_total) ~ ETHNICITY*timepoint, freezing_demo_twoethn)
Manova(B, test.statistic = "Pillai")

# Report these results in the following manner: "There exists no significant difference between ethnicity endorsement
# on the combined dependent variables (pswq, masq-aa, masq-lpa & masq-dm), Pillai's Trace = .03, F(4,183) = 1.24, p = .295



#################
# College Class
#################

C <- lm(cbind(pswq_total, masq_aa_total, masq_dm_total, masq_lpa_total) ~ YEAR.COLLEGE*timepoint, freezing_demo)
Manova(C, test.statistic = "Pillai")

# Report these results in the following manner: "There exists no significant difference between college year
# on the combined dependent variables (pswq, masq-aa, masq-lpa & masq-dm).


#################
# RACE
#################

count(freezing_demo_race, c("RACE", "timepoint"))
summary(freezing_demo_race)

D <- lm(cbind(pswq_total, masq_aa_total, masq_dm_total, masq_lpa_total) ~ RACE*timepoint, freezing_demo_race)
Manova(D, test.statistic = "Pillai")

# Report these results in the following manner: "There exists no significant difference between college year
# on the combined dependent variables (pswq, masq-aa, masq-lpa & masq-dm).

#################
# SES
#################
count(freezing_demo_class, c("CLASS.SELF", "timepoint"))

E <- lm(cbind(pswq_total, masq_aa_total, masq_dm_total, masq_lpa_total) ~ CLASS.SELF*timepoint, freezing_demo_class)
Manova(E, test.statistic = "Pillai")

# Report these results in the following manner: "There exists no significant difference between socioeconomic statuses
# on the combined dependent variables (pswq, masq-aa, masq-lpa & masq-dm).

#################
# SEX ORIENTATION
#################

count(freezing_demo$SEX.ORIENTATION)

G <- lm(cbind(pswq_total, masq_aa_total, masq_dm_total, masq_lpa_total) ~ SEX.ORIENTATION*timepoint, freezing_demo)
Manova(G, test.statistic = "Pillai")

# Report these results in the following manner: "There exists no significant difference between socioeconomic statuses
# on the combined dependent variables (pswq, masq-aa, masq-lpa & masq-dm).

#################
# INTERNATIONAL
#################

count(freezing_demo, vars=INTERNATIONAL)


H <- lm(cbind(pswq_total, masq_aa_total, masq_dm_total, masq_lpa_total) ~ INTERNATIONAL*timepoint, freezing_demo)
Manova(H, test.statistic = "Pillai")

# Report these results in the following manner: "There exists no significant difference between INTERNATIONAL status
# on the combined dependent variables (pswq, masq-aa, masq-lpa & masq-dm).

# Computation - do not use, does not include all DVs

#Y1 <- lm(cbind(pswq_total, masq_aa_total, masq_dm_total, masq_lpa_total) ~ timepoint*age_trend, freezing_raw_d_age)
#age_man<-Manova(Y1, test.statistic = "Pillai")
#age_man

#count(freezing_raw_d_age, vars=age)

# Report these results in the following manner: "There was a statistically significant difference between the 
#timepoints on the combined dependent variables (pswq, masq-aa, masq-lpa & masq-dm), Pillai's Trace = .142, F(4,646) = 26.89, p< 0.0001

# A significant Manova can be followed by univariate one-way ANOVA to examine each dependent variable

#grouped.data <- freezing_raw_d_age %>%
#gather(key = "variable", value = "value", pswq_total, masq_aa_total, masq_lpa_total, masq_dm_total) %>%
#group_by(variable)

#grouped.data %>% anova_test(value ~ timepoint)

## Report these results in the following manner: "Follow-up univariate ANOVAs, using a Bonferroni adjusted alpha level of 0.0125, showed that there was a 
#statistically significant difference in MASQ-DM (F(1,651)= 67.2, p < .00001) and MASQ-LPA (F(1,651) = 23.1, p<.00001) between T1 and T2 groups

# Now, we need to check pairwise comparisons

#pwc <- freezing_raw_d_age %>%
#gather(key = "variable", value = "value", pswq_total, masq_aa_total, masq_lpa_total, masq_dm_total) %>%
#group_by(variable) %>%
#games_howell_test(value ~ timepoint)
#pwc

#Report these results in the following manner: "Pairwise comparisons between groups confirmed the significant difference between groups such that T2 had higher scores
#for masq-dm and masq-lpa 


##############################################################
##  MANOVA - By Group (T1 & T2)  & Age Trend (<20, ≥ 20)    ##
##############################################################

## Group is either pre-spring break (T1) or post-spring break (T2)

count(freezing_raw_d_age,c("timepoint", "age_trend"))

#Checking Assumptions
#Let's check for levels of multicollinearity
Y <- cbind(freezing_raw_d_age$pswq_total, freezing_raw_d_age$masq_aa_total,freezing_raw_d_age$masq_ad_total, freezing_raw_d_age$masq_dm_total, freezing_raw_d_age$masq_lpa_total)
cor(Y, method = c("pearson"))

# Okay, it seems as if we have very high correlation between masq-ad and masq-ad14 (r = .93), this is a problem, adjust.
# Proposed solution is to assess just the two-factor model of AD? ie utilize masq-AD8 & masq-AD14, but not the general score

box_res<-box_m(freezing_raw_d_age[,c("pswq_total", "masq_aa_total", "masq_dm_total", "masq_lpa_total")],freezing_raw_d_age[,"timepoint"])

# The assumption that of equality of covariance matrices is met
# Test for homogeneity of variance

leveneTest(freezing_raw_d_age[,"pswq_total"],freezing_raw_d_age[,"timepoint"], location = "median", correction = "zero.connection")
leveneTest(freezing_raw_d_age[,"masq_aa_total"],freezing_raw_d_age[,"timepoint"], location = "median", correction = "zero.connection")
leveneTest(freezing_raw_d_age[,"masq_dm_total"],freezing_raw_d_age[,"timepoint"], location = "median", correction = "zero.connection")
leveneTest(freezing_raw_d_age[,"masq_lpa_total"],freezing_raw_d_age[,"timepoint"], location = "median", correction = "zero.connection")

# The assumption that of equality of variance is met
# Now, let's check for univariate normality, could use shapiro test, but because of high sample size, we will prioritize qq plots

freezing_raw_d_age %>%
  group_by(timepoint) %>%
  shapiro_test(pswq_total, masq_aa_total, masq_dm_total, masq_lpa_total) %>%
  arrange(variable)

# look at qq plots
ggqqplot(freezing_raw_d_age, "pswq_total", facet.by = "timepoint",
         ylab = "worry", ggtheme = theme_bw())
ggqqplot(freezing_raw_d_age, "masq_aa_total", facet.by = "timepoint",
         ylab = "arousal", ggtheme = theme_bw())
ggqqplot(freezing_raw_d_age, "masq_dm_total", facet.by = "timepoint",
         ylab = "depressed mood", ggtheme = theme_bw())
ggqqplot(freezing_raw_d_age, "masq_lpa_total", facet.by = "timepoint",
         ylab = "low positive affect", ggtheme = theme_bw())

# Now, check multivariate normality
freezing_raw_d_age %>%
  select(pswq_total, masq_aa_total, masq_dm_total, masq_lpa_total) %>%
  mshapiro_test()




## Computation 1

Y2<- lm(cbind(pswq_total, rrq_refl_total, rrq_rum_total, masq_aa_total, masq_dm_total, masq_lpa_total) ~ timepoint*age, freezing_raw_d_age)
age2_man<-Manova(Y2, test.statistic = "Pillai")
age2_man


grouped.data <- freezing_raw_d_age %>%
  gather(key = "variable", value = "value", pswq_total, rrq_refl_total, rrq_rum_total, masq_aa_total, masq_lpa_total, masq_dm_total) %>%
  group_by(variable)

gd<-grouped.data %>% anova_test(value ~ age_trend*timepoint)
formattable(gd)

pwc <- freezing_raw_d_age %>%
  gather(key = "variable", value = "value", pswq_total, rrq_refl_total, rrq_rum_total, masq_aa_total, masq_lpa_total, masq_dm_total) %>%
  group_by(variable) %>%
  games_howell_test(value ~ timepoint)
pwc


t.test(pswq_total ~ age_trend, data = post_sb, alternative = "less", var.equal = FALSE)
t.test(pswq_total ~ age_trend, data = pre_sb, alternative = "less", var.equal = FALSE)


## MANCOVA to speak to 2nd derivative, rate of rate of change between groups having controlled for anxiety

mancova(data = freezing_raw_d_age,  
        deps = vars(masq_lpa_total, masq_dm_total),
        factors = vars(timepoint, age_trend),
        covs = vars(pswq_total, rrq_refl_total, rrq_rum_total, masq_aa_total),
        multivar = list("pillai", "wilks", "hotel", "roy"),
        boxM = FALSE,
        shapiro = FALSE,
        qqPlot = TRUE
)

# Great, timepoint remains significant after we control for anxiety measures. Now, let's take a look at unique variance of each depression subscale

res.aov <- freezing_raw_d_age %>% 
  anova_test(masq_dm_total ~ cbind(masq_lpa_total, pswq_total, rrq_refl_total, rrq_rum_total, masq_aa_total) + timepoint*age_trend)
get_anova_table(res.aov)


res.aov2 <- freezing_raw_d_age %>% 
  anova_test(masq_lpa_total ~ cbind(masq_dm_total, pswq_total, rrq_refl_total, rrq_rum_total, masq_aa_total) + timepoint*age_trend)
get_anova_table(res.aov2)

####### MANOVA Omnibus computation ####### 

# separating contributions of avoidance and approach by running two separate omnibus tests with the inclusion of either avoidance or approach in the model 

L1 <- lm(cbind(pswq_total, rrq_refl_total, rrq_rum_total, masq_aa_total, masq_dm_total, masq_lpa_total) 
         ~ (lifeevent*timepoint*atq_appr*age), lec)
lec1_man<-Manova(L1, test.statistic = "Pillai")
lec1_man



L2 <- lm(cbind(pswq_total, rrq_refl_total, rrq_rum_total, masq_aa_total, masq_dm_total, masq_lpa_total) 
         ~ (lifeevent*timepoint*atq_avoi*age), lec)
lec2_man<-Manova(L2, test.statistic = "Pillai")
lec2_man



## Shifting followup tests - make sure to input variables of interest where applicable

grouped.data1 <- lec %>%
  gather(key = "variable", value = "value", pswq_total, rrq_refl_total, rrq_rum_total, masq_aa_total, masq_lpa_total, masq_dm_total) %>%
  group_by(variable)
grouped.data1

gd2<-grouped.data1 %>% anova_test(value ~ lifeevent*atq_appr)
gd2

pwclife <- lec %>%
  gather(key = "variable", value = "value", pswq_total, rrq_refl_total, rrq_rum_total, masq_aa_total, masq_lpa_total, masq_dm_total) %>%
  group_by(variable) %>%
  games_howell_test(value ~ lifeevent)
pwclife

### Visualize results from followup univariate tests on significant omnibus MANOVA results

## MASQ_AA

ggplot(lec, aes(x=atq_appr, y=masq_aa_total, color=lifeevent)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F) +
  labs(x= "Approach", y= "Arousal") +
  labs(color= "Life Event")

ggplot(lec, aes(x=atq_avoi, y=masq_aa_total, color=lifeevent)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F) +
  labs(x= "Avoidance", y= "Arousal") +
  labs(color= "Life Event")

## MASQ_DM

ggplot(lec, aes(x=atq_appr, y=masq_dm_total, color=lifeevent)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F) +
  labs(x= "Approach", y= "Depressed Mood") +
  labs(color= "Life Event")

ggplot(lec, aes(x=atq_appr, y=masq_dm_total, color=timepoint)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F) +
  labs(x= "Approach", y= "Depressed Mood") +
  labs(color= "Timepoint")

## RRQ_Reflection

ggplot(lec, aes(x=atq_avoi, y=rrq_refl_total, color=lifeevent)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F) +
  labs(x= "Avoidance", y= "Reflection") +
  labs(color= "Life Event")

ggplot(lec, aes(x=atq_avoi, y=rrq_refl_total, color=timepoint)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=F) +
  labs(x= "Avoidance", y= "Reflection") +
  labs(color= "Timepoint")


#### Unused hierarchical regression steps

m12 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint + lifeevent:age + lifeevent:timepoint:age 
          + atq_avoi + atq_avoi:timepoint + atq_avoi:age + atq_avoi:lifeevent + atq_avoi:timepoint:age, data=lec)   # Model 12
m13 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint + lifeevent:age + lifeevent:timepoint:age 
          + atq_avoi + atq_avoi:timepoint + atq_avoi:age + atq_avoi:lifeevent + atq_avoi:timepoint:age + atq_avoi:timepoint:lifeevent, 
          data=lec)   # Model 13
m14 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint + lifeevent:age + lifeevent:timepoint:age
          + atq_avoi + atq_avoi:timepoint + atq_avoi:age + atq_avoi:lifeevent + atq_avoi:timepoint:age + atq_avoi:timepoint:lifeevent 
          + atq_avoi:lifeevent:age, data=lec)   # Model 14
m15 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint + lifeevent:age + lifeevent:timepoint:age 
          + atq_avoi + atq_avoi:timepoint + atq_avoi:age + atq_avoi:lifeevent + atq_avoi:timepoint:age + atq_avoi:timepoint:lifeevent 
          + atq_avoi:lifeevent:age + atq_avoi:timepoint:age:lifeevent, data=lec)   # Model 15
m16 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint + lifeevent:age + lifeevent:timepoint:age 
          + atq_avoi + atq_avoi:timepoint + atq_avoi:age + atq_avoi:lifeevent + atq_avoi:timepoint:age + atq_avoi:timepoint:lifeevent 
          + atq_avoi:lifeevent:age + atq_avoi:timepoint:age:lifeevent + atq_appr, data=lec)   # Model 16
m17 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint + lifeevent:age + lifeevent:timepoint:age 
          + atq_avoi + atq_avoi:timepoint + atq_avoi:age + atq_avoi:lifeevent + atq_avoi:timepoint:age + atq_avoi:timepoint:lifeevent 
          + atq_avoi:lifeevent:age + atq_avoi:timepoint:age:lifeevent + atq_appr + atq_appr:timepoint, data=lec)   # Model 17
m18 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint + lifeevent:age + lifeevent:timepoint:age 
          + atq_avoi + atq_avoi:timepoint + atq_avoi:age + atq_avoi:lifeevent + atq_avoi:timepoint:age + atq_avoi:timepoint:lifeevent 
          + atq_avoi:lifeevent:age + atq_avoi:timepoint:age:lifeevent + atq_appr + atq_appr:timepoint + atq_appr:age, data=lec)   # Model 18
m19 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint + lifeevent:age + lifeevent:timepoint:age 
          + atq_avoi + atq_avoi:timepoint + atq_avoi:age + atq_avoi:lifeevent + atq_avoi:timepoint:age + atq_avoi:timepoint:lifeevent 
          + atq_avoi:lifeevent:age + atq_avoi:timepoint:age:lifeevent + atq_appr + atq_appr:timepoint + atq_appr:age + atq_appr:lifeevent,
          data=lec)   # Model 19
m20 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint + lifeevent:age + lifeevent:timepoint:age 
          + atq_avoi + atq_avoi:timepoint + atq_avoi:age + atq_avoi:lifeevent + atq_avoi:timepoint:age + atq_avoi:timepoint:lifeevent 
          + atq_avoi:lifeevent:age + atq_avoi:timepoint:age:lifeevent + atq_appr + atq_appr:timepoint + atq_appr:age + atq_appr:lifeevent
          + atq_appr:atq_avoi, data=lec)   # Model 20
m21 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint + lifeevent:age + lifeevent:timepoint:age 
          + atq_avoi + atq_avoi:timepoint + atq_avoi:age + atq_avoi:lifeevent + atq_avoi:timepoint:age + atq_avoi:timepoint:lifeevent 
          + atq_avoi:lifeevent:age + atq_avoi:timepoint:age:lifeevent + atq_appr + atq_appr:timepoint + atq_appr:age + atq_appr:lifeevent
          + atq_appr:atq_avoi + atq_appr:timepoint:age, data=lec)   # Model 21
m22 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint + lifeevent:age + lifeevent:timepoint:age 
          + atq_avoi + atq_avoi:timepoint + atq_avoi:age + atq_avoi:lifeevent + atq_avoi:timepoint:age + atq_avoi:timepoint:lifeevent 
          + atq_avoi:lifeevent:age + atq_avoi:timepoint:age:lifeevent + atq_appr + atq_appr:timepoint + atq_appr:age + atq_appr:lifeevent
          + atq_appr:atq_avoi + atq_appr:timepoint:age + atq_appr:timepoint:lifeevent, data=lec)   # Model 22
m23 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint + lifeevent:age + lifeevent:timepoint:age 
          + atq_avoi + atq_avoi:timepoint + atq_avoi:age + atq_avoi:lifeevent + atq_avoi:timepoint:age + atq_avoi:timepoint:lifeevent 
          + atq_avoi:lifeevent:age + atq_avoi:timepoint:age:lifeevent + atq_appr + atq_appr:timepoint + atq_appr:age + atq_appr:lifeevent
          + atq_appr:atq_avoi + atq_appr:timepoint:age + atq_appr:timepoint:lifeevent + atq_appr:timepoint:atq_avoi, data=lec)   # Model 23
m24 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint + lifeevent:age + lifeevent:timepoint:age 
          + atq_avoi + atq_avoi:timepoint + atq_avoi:age + atq_avoi:lifeevent + atq_avoi:timepoint:age + atq_avoi:timepoint:lifeevent 
          + atq_avoi:lifeevent:age + atq_avoi:timepoint:age:lifeevent + atq_appr + atq_appr:timepoint + atq_appr:age + atq_appr:lifeevent
          + atq_appr:atq_avoi + atq_appr:timepoint:age + atq_appr:timepoint:lifeevent + atq_appr:timepoint:atq_avoi + atq_appr:lifeevent:age,
          data=lec)   # Model 24
m25 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint + lifeevent:age + lifeevent:timepoint:age 
          + atq_avoi + atq_avoi:timepoint + atq_avoi:age + atq_avoi:lifeevent + atq_avoi:timepoint:age + atq_avoi:timepoint:lifeevent 
          + atq_avoi:lifeevent:age + atq_avoi:timepoint:age:lifeevent + atq_appr + atq_appr:timepoint + atq_appr:age + atq_appr:lifeevent
          + atq_appr:atq_avoi + atq_appr:timepoint:age + atq_appr:timepoint:lifeevent + atq_appr:timepoint:atq_avoi + atq_appr:lifeevent:age
          + atq_appr:lifeevent:atq_avoi, data=lec)   # Model 25
m26 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint + lifeevent:age + lifeevent:timepoint:age 
          + atq_avoi + atq_avoi:timepoint + atq_avoi:age + atq_avoi:lifeevent + atq_avoi:timepoint:age + atq_avoi:timepoint:lifeevent 
          + atq_avoi:lifeevent:age + atq_avoi:timepoint:age:lifeevent + atq_appr + atq_appr:timepoint + atq_appr:age + atq_appr:lifeevent
          + atq_appr:atq_avoi + atq_appr:timepoint:age + atq_appr:timepoint:lifeevent + atq_appr:timepoint:atq_avoi + atq_appr:lifeevent:age
          + atq_appr:lifeevent:atq_avoi + atq_appr:age:atq_avoi, data=lec)   # Model 26
m27 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint + lifeevent:age + lifeevent:timepoint:age 
          + atq_avoi + atq_avoi:timepoint + atq_avoi:age + atq_avoi:lifeevent + atq_avoi:timepoint:age + atq_avoi:timepoint:lifeevent 
          + atq_avoi:lifeevent:age + atq_avoi:timepoint:age:lifeevent + atq_appr + atq_appr:timepoint + atq_appr:age + atq_appr:lifeevent
          + atq_appr:atq_avoi + atq_appr:timepoint:age + atq_appr:timepoint:lifeevent + atq_appr:timepoint:atq_avoi + atq_appr:lifeevent:age
          + atq_appr:lifeevent:atq_avoi + atq_appr:age:atq_avoi + atq_appr:timepoint:lifeevent:age, data=lec)   # Model 27
m28 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint + lifeevent:age + lifeevent:timepoint:age 
          + atq_avoi + atq_avoi:timepoint + atq_avoi:age + atq_avoi:lifeevent + atq_avoi:timepoint:age + atq_avoi:timepoint:lifeevent 
          + atq_avoi:lifeevent:age + atq_avoi:timepoint:age:lifeevent + atq_appr + atq_appr:timepoint + atq_appr:age + atq_appr:lifeevent
          + atq_appr:atq_avoi + atq_appr:timepoint:age + atq_appr:timepoint:lifeevent + atq_appr:timepoint:atq_avoi + atq_appr:lifeevent:age
          + atq_appr:lifeevent:atq_avoi + atq_appr:age:atq_avoi + atq_appr:timepoint:lifeevent:age + atq_appr:timepoint:lifeevent:atq_avoi, 
          data=lec)   # Model 28
m29 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint + lifeevent:age + lifeevent:timepoint:age 
          + atq_avoi + atq_avoi:timepoint + atq_avoi:age + atq_avoi:lifeevent + atq_avoi:timepoint:age + atq_avoi:timepoint:lifeevent 
          + atq_avoi:lifeevent:age + atq_avoi:timepoint:age:lifeevent + atq_appr + atq_appr:timepoint + atq_appr:age + atq_appr:lifeevent
          + atq_appr:atq_avoi + atq_appr:timepoint:age + atq_appr:timepoint:lifeevent + atq_appr:timepoint:atq_avoi + atq_appr:lifeevent:age
          + atq_appr:lifeevent:atq_avoi + atq_appr:age:atq_avoi + atq_appr:timepoint:lifeevent:age + atq_appr:timepoint:lifeevent:atq_avoi 
          + atq_appr:age:lifeevent:atq_avoi, data=lec)   # Model 29
m30 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint + lifeevent:age + lifeevent:timepoint:age 
          + atq_avoi + atq_avoi:timepoint + atq_avoi:age + atq_avoi:lifeevent + atq_avoi:timepoint:age + atq_avoi:timepoint:lifeevent 
          + atq_avoi:lifeevent:age + atq_avoi:timepoint:age:lifeevent + atq_appr + atq_appr:timepoint + atq_appr:age + atq_appr:lifeevent
          + atq_appr:atq_avoi + atq_appr:timepoint:age + atq_appr:timepoint:lifeevent + atq_appr:timepoint:atq_avoi + atq_appr:lifeevent:age
          + atq_appr:lifeevent:atq_avoi + atq_appr:age:atq_avoi + atq_appr:timepoint:lifeevent:age + atq_appr:timepoint:lifeevent:atq_avoi 
          + atq_appr:age:lifeevent:atq_avoi + atq_appr:age:timepoint:atq_avoi, data=lec)   # Model 30
m31 <- lm(pswq_total ~ timepoint + age + timepoint:age + lifeevent + lifeevent:timepoint + lifeevent:age + lifeevent:timepoint:age 
          + atq_avoi + atq_avoi:timepoint + atq_avoi:age + atq_avoi:lifeevent + atq_avoi:timepoint:age + atq_avoi:timepoint:lifeevent 
          + atq_avoi:lifeevent:age + atq_avoi:timepoint:age:lifeevent + atq_appr + atq_appr:timepoint + atq_appr:age + atq_appr:lifeevent
          + atq_appr:atq_avoi + atq_appr:timepoint:age + atq_appr:timepoint:lifeevent + atq_appr:timepoint:atq_avoi + atq_appr:lifeevent:age
          + atq_appr:lifeevent:atq_avoi + atq_appr:age:atq_avoi + atq_appr:timepoint:lifeevent:age + atq_appr:timepoint:lifeevent:atq_avoi 
          + atq_appr:age:lifeevent:atq_avoi + atq_appr:age:timepoint:atq_avoi + atq_appr:lifeevent:age:timepoint:atq_avoi, data=lec)   # Model 31
