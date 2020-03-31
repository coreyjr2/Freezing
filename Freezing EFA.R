##############################
### Freezing EFA Analysis ###
##############################

freezing_raw<-read.csv(file.choose())

## install relevant packages ##
install.packages("tidyverse")
y
library(tidyverse)

## data cleaning - removing columns ##
freezing_raw<-select(freezing_raw,-c(redcap_survey_identifier,consent_form_timestamp,sona_id))
