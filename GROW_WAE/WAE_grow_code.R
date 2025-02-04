### Walleye GROW CARDS ###
## written by Katelyn King 

#### load libraries #### 
library(dplyr)
library(tidyr)
library(stringr)
library(rqdatatable)

#### load data #### 
#read in lake match data from Faye and Michael's work  
lake_match <-read.csv("GROW_general/grow_card_lake_match_13Sep2021.csv", header=TRUE, na.strings = c("", "NA")) %>%
  select(subject_id, county, lakename, 'new.key') %>%
  rename(new_key = 'new.key') %>%
  drop_na('new_key')

#read in dates from basic workflow   
grow_dates <-read.csv("GROW_general/grow_dates.csv", header=TRUE, na.strings = c("", "NA"))

#task values 
grow_task_values<-read.csv("GROW_BLG/BLG_data/GROW_task_values.csv", header=TRUE) %>% 
  select(task, column_name, old_name) %>%
  mutate(old_name = trimws(old_name, which = c("both"))) #trim white space 

#read in urls 
urls<-read.csv("urls/basic_GROW_URLs.csv", na.strings = c("", "NA"))  %>% 
  distinct(subject_id, .keep_all = TRUE) 

#3 files, dropdown (age info), question (this is if a sp is present on the card), text (length info)

#### blgrow_text (length info) cleaning ####
#cards are in inches unless otherwise noted - none noted mm this time? get tags from Justin 
#don't want anything in parenthesis which are weights. 
text<-read.csv("GROW_WAE/WAE_data/text_reducer_walleye_texts.csv") %>%
  select(subject_id, task, 'data.aligned_text', 'data.number_views', 'data.consensus_score','data.consensus_text') %>%
  drop_na('data.number_views') %>% 
  left_join(grow_task_values) %>% # join with the task values to get the actual names
  left_join(urls) %>%
  naniar::replace_with_na(replace = list(data.consensus_text = c('-', '--', "---", '...', '. . .', "-----", "...."))) %>% #function that replaces all of a value with NA 
  separate(data.consensus_text, c("data", "trash"), sep = "\\(", remove = FALSE) %>% #new column for units 
  separate(data, into = c("data", "trash2"), sep = " ") %>% 
  mutate(across(where(is.character), ~ na_if(.,""))) %>% # get rid of blank cell 
  drop_na('data.consensus_text') #drop no data


#*manual check consensus scores <2 #### 
#data consensus was updates so that if 2 people agreed than a 3rd did not have to look at the card. 
#So only what was viewed by >= 2 people but half didnt agree 
text_bad<- filter(text, data.consensus_score < 2) %>%
  drop_na(URL_front) #drop no data
#write.csv(text_bad,  "/Users/katelynking/Desktop/grow_walleye_review.csv", row.names = FALSE )
#good data where consensus is >50% 
text_good<-filter(text, data.consensus_score >= 2 ) %>%
  select(subject_id, column_name, data) 

#merging back in manually reviewed data #check comments for multiple groups of the species on a card
manual_length<-read.csv("GROW_WAE/WAE_data/grow_walleye_text_review.csv", na.strings = c("", "NA")) %>%
  mutate(data= ifelse(data == "NA ", NA, data)) %>% 
  drop_na(subject_id, data) %>%
  left_join(grow_task_values, by=c("task")) %>%
  rename(column_name = column_name.y) %>% 
  select(subject_id, column_name, data)

lengths_clean<-gtools::smartbind(text_good, manual_length) %>%
  mutate(column_name = trimws(column_name, which = c("both"))) %>% #trim white space that is making too many colums
  pivot_wider( #rotate so that each of the tasks is a column 
    id_cols = c(subject_id),
    names_from = column_name, 
    values_from = c(data), 
    values_fill = list(data = NA) )

#### dropdown ages cleaning #### 
#read in files (these have hash values)
#separate into four columns, one for each of the reviews if there are different answers
grow_age_values<-read.csv("GROW_BLG/BLG_data/GROW_age_values.csv", header=TRUE) %>% 
  mutate(code = as.factor(code))

dropdown<-read.csv("GROW_WAE/WAE_data/dropdown_reducer_walleye_dropdowns.csv", na.strings = c("", "NA")) %>%
  left_join(grow_task_values) %>% 
  select(subject_id, column_name, 'data.value') %>%
  drop_na('data.value') %>% 
  rename(data_value = 'data.value') %>% 
  mutate(sep = gsub("[[:punct:]]", "", data_value)) %>% 
  mutate(sep=  gsub("\\s", "_", sep) ) %>% #separate into four columns, one for each of the reviews if there are different answers
  separate(col=sep, c("age_hash", "review", "age_hash2", "review2", "age_hash3", "review3", "age_hash4", "review4" ), sep = "_") %>% 
  mutate(age_hash = as.factor(age_hash), 
         age_hash2 = as.factor(age_hash2),
         age_hash3 = as.factor(age_hash3),
         age_hash4 = as.factor(age_hash4)) %>% 
  left_join(grow_age_values, by=c('age_hash' = 'code')) %>% #get age values for the first review
  left_join(grow_age_values, by=c('age_hash2' = 'code'))  %>% #get age values for the 2nd review
  left_join(grow_age_values, by=c('age_hash3' = 'code')) %>% #get age values for the 3rd review
  left_join(grow_age_values, by=c('age_hash4' = 'code')) %>% #get age values for the 4th review
  rename(age1=age_group.x, age2=age_group.y, age3=age_group.x.x, age4=age_group.y.y) %>% 
  mutate(age1= ifelse(age_hash =='9f71969064233', 5, age1),  #hash code is not matching 9f71969064233
         age2=ifelse(age_hash2 =='9f71969064233', 5, age2),
         age3=ifelse(age_hash3 =='9f71969064233', 5, age3) 
  ) %>%
  mutate(age1= ifelse(age_hash =='f9428135d6777', 11, age1),  #hash code is not matching #f9428135d6777
         age2=ifelse(age_hash2 =='f9428135d6777', 11, age2),
         age3=ifelse(age_hash3 =='f9428135d6777', 11, age3)
  )%>%
  select(-c(age_hash, age_hash2, age_hash3, age_hash4))

# keep >2s OR any that were 2 but don't have disagreements (NAs) 
age_good<-filter(dropdown,  review >= 2 | review2 >= 2 | (review == 2 & is.na(review2)) ) %>%
  mutate(age= case_when(
    is.na(review2) ~ age1, # (total agreement)
    review2 > review ~ age2, # (majority selected)
    review > review2 ~ age1 # (majority selected)
  )) %>%
  select(subject_id, column_name, age)

#*filter out the ones for manual review  ####
age_bad<-filter(dropdown, (review < 2  & review2 <2) | (review == 1 & is.na(review2))) %>%
  select(-c(review3, review4)) %>% 
  mutate(age = '') %>% 
  left_join(urls)

#write.csv(age_bad, "/Users/katelynking/Desktop/grow_walleye_ages_review.csv", row.names = FALSE)
age_manual<-read.csv("GROW_WAE/WAE_data/grow_walleye_ages_review.csv", na.strings = c("", "NA")) %>% #after manual review 
            select(subject_id, column_name, age) %>% 
  mutate(age= ifelse(age == "NA ", NA, age)) %>%
  drop_na(age) %>%
  mutate(column_name = trimws(column_name, which = c("both"))) %>% #trim white space 
  left_join(grow_task_values, by=c("column_name" = "old_name")) %>%
  select(subject_id, column_name.y, age)  %>% 
  rename(column_name = column_name.y)

#join manually reviewed and good datasets 
age_data<-rbind(age_good, age_manual)%>%
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(age), 
              values_fill = list(age = NA) )

#*join age data: dropdown ages and length text  ####
all_grow<-left_join(age_data, lengths_clean) %>% 
  pivot_longer(-subject_id,
               names_to =c("row", ".value"),
               names_sep ="_row") %>% 
  rename(age_group = " age group ", fish_count = " Number of Fish", length_range = " Length Range", length_mean = " Mean Length") %>% 
  filter(if_any(c("age_group", "fish_count", "length_range", "length_mean"), complete.cases)) %>%
  mutate(length_range = gsub("mm" , '', length_range), #remove units from some cells 
         length_mean = gsub("mm" , '', length_mean), 
         length_range = gsub("cm" , '', length_range), #remove units from some cells 
         length_mean = gsub("cm" , '', length_mean),
         length_range = gsub("\"" , '', length_range),
         length_mean = gsub("\"" , '', length_mean)
  ) %>% 
  separate(length_range, c("length_min", "length_max"), sep = "-")  %>% 
  mutate(across(where(is.character), ~ na_if(.,""))) %>% # get rid of blank cell 
  select(-c(row)) %>% 
  mutate(fish_count = as.numeric(as.character(fish_count)), 
         length_min = as.numeric(as.character(length_min)), 
         length_max = as.numeric(as.character(length_max)),
         length_mean = as.numeric(as.character(length_mean)),
         age_group = as.numeric(as.character(age_group))
  ) %>% #QAQC CHECK NAs where there is no age group listed! 
  mutate(age_group = case_when(  
   is.na(age_group) & subject_id==59962576 ~ 20, 
   is.na(age_group) & subject_id==59962651 ~ 2, 
   is.na(age_group) & subject_id==59687876 ~ 4, 
   is.na(age_group) & subject_id==59962647 ~ 6, 
   is.na(age_group) & subject_id==59688091 ~ 6, #use distinct() to remove the duplicate 
   is.na(age_group) & subject_id==59688092 ~ 4, #lengths are wrong tho.change these below
   is.na(age_group) & subject_id==59962488 & length_min == 17.0 ~ 5, 
   is.na(age_group) & subject_id==59962488 & length_min == 19.2 ~ 6, 
   is.na(age_group) & subject_id==59962488 & length_min == 19.8 ~ 8, # use distinct() to remove the duplicate row
   is.na(age_group) & subject_id==59963995 ~ 2, 
   subject_id==58754443 & age_group == 5 & length_min == 15.7 ~ 14, #update the rest of the data below with this age group
   is.na(age_group) & subject_id==58754443 ~ 15, #update the rest of the data below with this age group
   is.na(age_group) & subject_id==58754758 & fish_count == 102 ~ 8,
   is.na(age_group) & subject_id==58754758 & fish_count == 14 ~ 9,
   is.na(age_group) & subject_id==58754840 ~ 8,
   is.na(age_group) & subject_id== 59963314 ~ 9,
   TRUE ~ age_group))  %>% 
  mutate(length_min = ifelse(subject_id==59688092 & age_group == 4, NA, length_min),
         length_max = ifelse(subject_id==59688092 & age_group == 4, NA, length_max),
         length_mean = ifelse(subject_id==59688092 & age_group == 4, NA, length_mean),
         length_min = ifelse(subject_id==58754443 & age_group == 14, 18.9, length_min),
         length_max = ifelse(subject_id==58754443 & age_group == 14, 19.8, length_max),
         fish_count = ifelse(subject_id==58754443 & age_group == 14, 2, fish_count),
         length_mean = ifelse(subject_id==58754443 & age_group == 14, 19.4, length_mean),
         length_min = ifelse(subject_id==58754443 & age_group == 15, 18.7, length_min),
         length_max = ifelse(subject_id==58754443 & age_group == 15, 20.2, length_max),
         fish_count = ifelse(subject_id==58754443 & age_group == 15, 3, fish_count),
         length_mean = ifelse(subject_id==58754443 & age_group == 15, 19.5, length_mean),
         length_mean = ifelse(subject_id==59688091 & age_group == 6, NA, length_mean), 
         age_group = ifelse(subject_id==58754754 & length_min == 20.5, NA, age_group), #no age group given
         age_group = ifelse(subject_id==58746258, NA, age_group), #no age group given
         age_group = ifelse(subject_id== 58642664 & length_min == 23.0, 14, age_group) #correction 
  ) %>% 
  distinct() 

#58754676 supposed to be NA - no group listed on card 
#58746585supposed to be NA - no group listed on card 

#add new rows of data that are missing from subjects 
new_row1 <- c(58754443, 17, 1, 18.2, NA, 18.2)  
new_row2 <- c(58754443, 18, 1, 21.0, NA, 21.0)  
new_row3 <- c(58642664, 15, 1, 24.0, NA, 24.0) 
new_row4 <- c(58642664, 16, 1, 26.0, NA, 26.0) 
new_row5 <- c(58642664, 17, 1, 26.5, NA, 26.5) 
new_row6 <- c(58642664, 20, 1, 29.1, NA, 29.1) 

all_grow<-rbind(all_grow, new_row1, new_row2, new_row3, new_row4, new_row5, new_row6)

#### join all data #### 
#join with basic dates and with lake matching info (which include already basic text)

#get units
grow_units<-read.csv("/Users/katelynking/Desktop/grow_card_units.csv")  

GROW<-left_join(all_grow, grow_dates) %>%
  left_join(lake_match) %>%
  left_join(grow_units) %>% #units from manual review and tags
  drop_na('url_front') %>% #drop cards without a url, these were removed from workflows 
  mutate(units= case_when(
    unit == "centimeters" ~ "centimeters",
    unit == "millimeters" ~ "millimeters",
    TRUE ~ 'inches'   )) %>%
  mutate(length_min_mm = case_when( #convert everything to millimeters 
    units == 'inches' ~ length_min*25.4, 
    units == 'centimeters' ~ length_min*10, 
    TRUE ~ length_min # else keep as mm 
  )) %>%
  mutate(length_max_mm = case_when(
    units == 'inches' ~ length_max*25.4, 
    units == 'centimeters' ~ length_max*10, 
    TRUE ~ length_max # else keep as mm 
  ))  %>%
  mutate(length_mean_mm = case_when(
    units == 'inches' ~ length_mean*25.4, 
    units == 'centimeters' ~ length_mean*10, 
    TRUE ~ length_mean # else keep as mm 
  )) %>%
  select(-c(length_min, length_max, length_mean, units, unit)) %>% 
  mutate(length_mean_mm = ifelse(is.na(length_mean_mm) & fish_count == 1, length_min_mm, length_mean_mm)) #if only one fish, fill in length mean with the same as the min length 

#check for outliers 
plot(GROW$length_max_mm) 
plot(GROW$length_min_mm)
plot(GROW$length_mean_mm)


#re-order columns 
GROW <- GROW[, c(1, 15,14,13, 2, 3, 16,17,18, 4,5,6, 7, 8,9,12, 10,11)]

#final data set 
write.csv(GROW, "GROW_WAE/wae_grow_qaqc.csv", row.names = FALSE)

#### add in DECEMBER data #### 
#note after removing subjects that had already been run and removing subjects without wae, only pages were left. no new cards. 
#no new dec data added 
text<-read.csv("GROW_WAE/WAE_data/text_reducer_walleye_dec.csv") %>%
  select(subject_id, task, 'data.aligned_text', 'data.number_views', 'data.consensus_score','data.consensus_text') %>%
  drop_na('data.number_views') %>% 
  naniar::replace_with_na(replace = list(data.consensus_text = c('-', '--', "---", '...', '. . .', "....", "----", "-----", "... . ."))) %>% #function that replaces all of a value with NA 
  drop_na('data.consensus_text') %>%
  left_join(grow_task_values) %>% # join with the task values to get the actual names
  separate(data.consensus_text, c("data", "trash"), sep = "\\(", remove = FALSE) %>% #remove parenthesis which are weights
  separate(data, into = c("data", "trash2"), sep = " ") %>% 
  left_join(grow_dates)

#some of these subjects were already done 
#read in the other data 
grow_wae<-read.csv("GROW_WAE/wae_grow_qaqc.csv")
other_lakes<-distinct(grow_wae, subject_id) 
unique_lakes_text<-anti_join(text, other_lakes)

#*manual check consensus scores <2 #### 
text_bad<- filter(unique_lakes_text, data.consensus_score < 2) %>% 
  drop_na(url_front) #anything without url is a page 
#none of these have walleye on them, so leave out 

#pivot good text
text_good<-filter(unique_lakes_text, data.consensus_score >= 2) %>%
  select(subject_id, column_name, data)%>%
  pivot_wider( #rotate so that each of the tasks is a column 
    id_cols = c(subject_id),
    names_from = column_name, 
    values_from = c(data), 
    values_fill = list(data = NA) )
 

#* dropdown #### 
grow_age_values<-read.csv("GROW_BLG/BLG_data/GROW_age_values.csv", header=TRUE)
#separate into four columns, one for each of the reviews if there are different answers
dropdown<-read.csv("GROW_WAE/WAE_data/dropdown_reducer_walleye_dec.csv", na.strings = c("", "NA")) %>%
  left_join(grow_task_values) %>% 
  select(subject_id, column_name, 'data.value') %>%
  drop_na('data.value') %>% 
  rename(data_value = 'data.value') %>% 
  mutate(sep = gsub("[[:punct:]]", "", data_value)) %>% 
  mutate(sep=  gsub("\\s", "_", sep) ) %>% #separate into four columns, one for each of the reviews if there are different answers
  separate(col=sep, c("age_hash", "review", "age_hash2", "review2", "age_hash3", "review3", "age_hash4", "review4" ), sep = "_") %>% 
  mutate(age_hash = as.factor(age_hash), 
         age_hash2 = as.factor(age_hash2),
         age_hash3 = as.factor(age_hash3),
         age_hash4 = as.factor(age_hash4)) %>% 
  left_join(grow_age_values, by=c('age_hash' = 'code')) %>% #get age values for the first review
  left_join(grow_age_values, by=c('age_hash2' = 'code'))  %>% #get age values for the 2nd review
  left_join(grow_age_values, by=c('age_hash3' = 'code')) %>% #get age values for the 3rd review
  left_join(grow_age_values, by=c('age_hash4' = 'code')) %>% #get age values for the 4th review
  rename(age1=age_group.x, age2=age_group.y, age3=age_group.x.x, age4=age_group.y.y) %>% 
  mutate(age1= ifelse(age_hash =='9f71969064233', 5, age1),  #hash code is not matching 9f71969064233
         age2=ifelse(age_hash2 =='9f71969064233', 5, age2),
         age3=ifelse(age_hash3 =='9f71969064233', 5, age3) 
  ) %>%
  mutate(age1= ifelse(age_hash =='f9428135d6777', 11, age1),  #hash code is not matching #f9428135d6777
         age2=ifelse(age_hash2 =='f9428135d6777', 11, age2),
         age3=ifelse(age_hash3 =='f9428135d6777', 11, age3)
  )%>%
  select(-c(age_hash, age_hash2, age_hash3, age_hash4))

unique_lakes_dropdown<-anti_join(dropdown, other_lakes)

#*filter out the ones for manual review  ####
age_bad<-filter(unique_lakes_dropdown, review < 2 ) %>%
  left_join(grow_dates) %>%  #these that match are cards, some are pages 
  drop_na(url_front)
#the same cards, no WAE - leave out 

# keep >=2s 
age_good<-filter(unique_lakes_dropdown, review >= 2 ) %>%
  mutate(age= case_when(
    is.na(review2) ~ age1, # (total agreement)
    review2 > review ~ age2, # (majority selected)
    review > review2 ~ age1 # (majority selected)
  ))  %>% 
  select(subject_id, column_name, age)%>%
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(age), 
              values_fill = list(age = NA) )

#*join age data: dropdown ages and length text  ####
all_grow<-full_join(age_good, text_good) %>% 
  pivot_longer(-subject_id,
               names_to =c("row", ".value"),
               names_sep ="_row") %>% 
  rename(age_group = " age group ", fish_count = " Number of Fish ", length_range = " Length Range ", length_mean = " Mean Length ") %>% 
  filter(if_any(c("age_group", "fish_count", "length_range", "length_mean"), complete.cases)) %>%
  separate(length_range, c("length_min", "length_max"), sep = "-") %>% 
  mutate(fish_count = as.numeric(as.character(fish_count)), 
         length_min = as.numeric(as.character(length_min)), 
         length_max = as.numeric(as.character(length_max)),
         length_mean = as.numeric(as.character(length_mean))
  )%>% 
  select(-c(row)) 

#check NAs or if anything coerced to NA

#### clean up units and join all data #### 
grow_units<-read.csv("/Users/katelynking/Desktop/grow_card_units.csv")  

GROW<-left_join(all_grow, grow_dates) %>%
  left_join(lake_match) %>%
  left_join(grow_units) %>% #units from manual review and tags
  drop_na('url_front') %>% #drop cards without a url, these did not match basic 
  mutate(units= case_when(
    unit == "centimeters" ~ "centimeters",
    unit == "millimeters" ~ "millimeters",
    TRUE ~ 'inches'   )) %>%
  mutate(length_min_mm = case_when( #convert everything to millimeters 
    units == 'inches' ~ length_min*25.4, 
    units == 'centimeters' ~ length_min*10, 
    TRUE ~ length_min # else keep as mm 
  )) %>%
  mutate(length_max_mm = case_when(
    units == 'inches' ~ length_max*25.4, 
    units == 'centimeters' ~ length_max*10, 
    TRUE ~ length_max # else keep as mm 
  ))  %>%
  mutate(length_mean_mm = case_when(
    units == 'inches' ~ length_mean*25.4, 
    units == 'centimeters' ~ length_mean*10, 
    TRUE ~ length_mean # else keep as mm 
  )) %>%
  select(-c(length_min, length_max, length_mean, units, unit)) %>% 
  mutate(length_mean_mm = ifelse(is.na(length_mean_mm) & fish_count == 1, length_min_mm, length_mean_mm)) %>% 
  filter(fish_count != 0) #remove 1 obs where fish count is 0, so no information for that age group 



