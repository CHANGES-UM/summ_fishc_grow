### Lake Summary fish presence/absence workflow cleaning ###
## written by Katelyn King 

#### load libraries #### 
library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
library(rqdatatable)
library(data.table)

#read in urls that have been completed as of AUG and have the bad urls removed already 
urls<-read.csv("urls/new_urls.csv") 

#### fish info presence question_SUMM ####
SUMM_fish_q<-read.csv("Lake_SUMM/SUMM_data/question_reducer_SUMM_fishinfo.csv") %>% 
  select(-c(workflow_id, reducer, task, none_listed, 'data.aggregation_version')) # don't need these categories, so task not necessary 
#SUMM_fish_tasks<-read.csv("Lake_SUMM/SUMM_data/SUMM_fishinfo_key.csv", header=TRUE) # don't need these categories, so task not necessary 

fish_wide<-melt(SUMM_fish_q, id.vars = "subject_id") %>% #shifts data from wide to long format so that each column is now in one column called 'variable'
  drop_na('value') %>% 
  pivot_wider(id_cols = c(subject_id), #shift data table back to wide format, so each subject has one row only  
              names_from = variable, 
              values_from = c(value), 
              values_fill =  0,
              values_fn= sum)

fish_wide <- subset(fish_wide, select=c(1,2,11, 3:10, 12:38)) #move the yes column to be beside the no data column
fish_wide$reviewers<-fish_wide$no_data + fish_wide$yes_data #add up the number of reviews 
fish_wide$review<-ifelse(fish_wide$yes_data == fish_wide$no_data, 'Yes', "No") #if reviewers are split 

####fish info text_SUMM- extra fish species  ####
fish_extras<-read.csv("Lake_SUMM/SUMM_data/text_reducer_SUMM_fishinfo.csv") %>%
  select(subject_id, 'data.number_views', 'data.consensus_text') %>%
  drop_na('data.number_views') %>% 
  select(subject_id, 'data.consensus_text') %>%
  rename(fish_extras = 'data.consensus_text')

#merge all of the extras into one row 
comments <- aggregate(fish_extras[2], fish_extras[-2], 
                      FUN = function(X) paste(unique(X), collapse=", "))

#merge fish selection and the extras 
fish_choices<-left_join(fish_wide, comments)

# manually review cards where reviewers were split, or there was only 1 review. ###
fishinfo_bad<-filter(fish_choices, fish_choices$review == "Yes" | fish_choices$reviewers == 1)
#write.csv(fishinfo_bad, "/Users/katelynking/Desktop/fish_for_review.csv", row.names = FALSE)
fish_manual<-read.csv("Lake_SUMM/SUMM_data/manual_review_data/fish_for_review_kk.csv") %>% 
  select(-c(front, back, comments, checked, review, reviewers)) %>%
  mutate_all(as.character)

fishinfo_good<-filter(fish_choices, fish_choices$review == "No" & fish_choices$reviewers != 1) %>% 
  select(-c(review, reviewers)) %>%
  mutate_all(as.character)

fish_info<-rbind(fishinfo_good, fish_manual)

#change all numbers to 0s and 1s 
helperFunction <- function(x){
  ifelse(x >=1, 1,0)
}

fish_info <- fish_info %>% 
  select(-c(no_data, yes_data)) %>%
  mutate_at(c(2:36), as.numeric) %>% #target specific columns 
  mutate_at(c(2:36),helperFunction)

#write.csv(fish_info, 'Lake_SUMM/SUMM_data/clean_data/clean_fish_pres_abs_summ_cards.csv', row.names = FALSE)

#### merge fish data with new_keys and comments and tags #### 
fish<-read.csv('Lake_SUMM/SUMM_data/clean_data/clean_fish_pres_abs_summ_cards.csv') 

fish_pres_absence<-left_join(dates, fish) %>%
  left_join(new_keys)

#reorder columns 
fish_pres_absence <- fish_pres_absence[, c(1,48,47,46,2,3,4,5,6,7,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,
                                           40,41,42,43,44,45,8,9)]

#remove subjects 63264926 (58687958), 63265573 (59071125), 63265689 (59071255),  and 63265642 (59071199) because they have 2 or 3 lakes on one card. 
fish_pres_absence<-fish_pres_absence[!(fish_pres_absence$subject_id=="58687958" | fish_pres_absence$subject_id=="59071125" | fish_pres_absence$subject_id=="59071255" | fish_pres_absence$subject_id=="59071199"),]

#write final dataset 
write.csv(fish_pres_absence, 'Lake_SUMM/SUMM_data/clean_data/lake_summ_fish_pres_qaqc.csv', row.names = FALSE)

#### December Data #### 
#change all numbers to 0s and 1s 
helperFunction <- function(x){
  ifelse(x >=1, 1,0)
}

SUMM_fish_q_dec<-read.csv("Lake_SUMM/SUMM_data/dec_data/question_reducer_summ_fishchoices_dec.csv") %>% 
  select(-c(workflow_id, task, 'data.none.listed')) # don't need these categories, so task not necessary 

fish_wide<-melt(SUMM_fish_q_dec, id.vars = "subject_id") %>% #shifts data from wide to long format so that each column is now in one column called 'variable'
  drop_na('value') %>% 
  pivot_wider(id_cols = c(subject_id), #shift data table back to wide format, so each subject has one row only  
              names_from = variable, 
              values_from = c(value), 
              values_fill =  0,
              values_fn= sum) %>% 
  mutate(review = ifelse(data.yes < data.no & data.no != 3, 'Yes', "No")) %>%  #if 2 reviewers said no data and one said yes,#checked one, and entries are correct so keep 
  select(-c(data.yes, data.no, review)) %>% 
  mutate_at(c(2:34),helperFunction)

#rename columns to match other pres/abs
names(fish_wide)[2:34] <- c('yellow_perch', 'bluegill', 'largemouth_bass', 'northern_pike', 'smallmouth_bass', 
                            'golden_shiner', 'bullhead_not_specified', 'white_sucker', 'black_bullhead', 'pumpkinseed', 'black_crappie', 'rock_bass', 'walleye',
                            'bluntnose_minnow', 'brown_bullhead', 'common_shiner', 'iowa_darter', 'banded_killifish', 
                            'mimic_shiner', 'blackchin_shiner', 'brook_silverside', 'yellow_bullhead', 'common_carp', 'longnose_gar',
                            'bowfin_dogfish', 'lake_chubsucker', 'cisco_herring_whitefish', 'blacknose_shiner', 'spotted_gar', 'johnny_darter',
                            'brown_trout', 'blacknose_minnow', 'rainbow_trout')


#add extra fish species 
fish_extras<-read.csv("Lake_SUMM/SUMM_data/dec_data/text_reducer_SUMM_fishchoices_dec.csv") %>%
  select(subject_id, 'data.number_views', 'data.consensus_text', 'data.consensus_score') %>%
  drop_na('data.number_views') %>% 
  select(subject_id, 'data.consensus_text') %>%
  rename(fish_extras = 'data.consensus_text')

#merge all of the extras into one row 
comments <- aggregate(fish_extras[2], fish_extras[-2], 
                      FUN = function(X) paste(unique(X), collapse=", ")) %>% 
  mutate(fish_extras = gsub(",", ";", fish_extras))


fish_pres_dec<-left_join(fish_wide, comments) 

write.csv(fish_pres_dec, 'Lake_SUMM/lake_summ_fish_pres_qaqc_dec.csv', row.names = FALSE)

