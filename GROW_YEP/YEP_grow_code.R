### Yellow Perch GROW CARDS ###
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

#### text (length info) cleaning ####
#cards are in inches unless otherwise noted - none noted mm this time? get tags from Justin 
#don't want anything in parenthesis which are weights. 
text<-read.csv("GROW_YEP/YEP_data/text_reducer_perch_texts.csv") %>%
  select(subject_id, task, 'data.aligned_text', 'data.number_views', 'data.consensus_score','data.consensus_text') %>%
  drop_na('data.number_views') %>% 
  left_join(grow_task_values) %>% # join with the task values to get the actual names
  left_join(urls) %>%
  naniar::replace_with_na(replace = list(data.consensus_text = c('-', '--', "---","——", "----", "-----", '.', '...', '. . .', "..."))) %>% #function that replaces all of a value with NA 
  separate(data.consensus_text, c("data", "trash"), sep = "\\(", remove = FALSE) %>% #new column for units 
  separate(data, into = c("data", "trash2"), sep = " ") %>% 
  drop_na('data.consensus_text') #drop no data

#*manual check consensus scores <2 #### 
#data consensus was updates so that if 2 people agreed than a 3rd did not have to look at the card. 
#So only what was viewed by >= 2 people but half didnt agree 
text_bad<- filter(text, data.consensus_score < 2) %>% 
  drop_na(URL_front)
#write.csv(text_bad,  "/Users/katelynking/Desktop/grow_yellowperch_review.csv", row.names = FALSE )
manual_length<-read.csv("GROW_YEP/YEP_data/grow_yellowperch_text_review.csv", na.strings = c("", "NA")) %>%
  drop_na(subject_id, data) %>%
  left_join(grow_task_values, by=c("task")) %>%
  rename(column_name = column_name.y) %>% 
  select(subject_id, column_name, data)

#good data where consensus is >50% 
text_good<-filter(text, data.consensus_score >= 2 ) %>%
  select(subject_id, column_name, data) %>% 
  mutate(data = case_when(subject_id==59962993 & (column_name == "7th row Number of Fish " | column_name == "7th row Mean Length ") ~ NA_character_,  #59962993 - use the all sex section only - only 6 rows, remove 7th 
         subject_id==61362195 & column_name == "First row Number of Fish " ~ "6",  #61362195 - all only  # need to manually change everything 
         subject_id==61362195 & column_name == "First row Length Range "~ "4.7-5.8", 
         subject_id==61362195 & column_name == "First row Mean Length "~ "5.4", 
         subject_id==61362195 & column_name == "Second row Number of Fish "~ "44",
         subject_id==61362195 & column_name == "Second row Length Range "~ "5.5-7.3", 
         subject_id==61362195 & column_name == "Second row Mean Length "~ "6.2",
         subject_id==61362195 & column_name == "Third row Number of Fish "~ "12", 
         subject_id==61362195 & column_name == "Third row Length Range "~ "6.0-7.7", 
         subject_id==61362195 & column_name == "Third row Mean Length "~ "6.7", 
         subject_id==61362195 & column_name == "Fourth row Number of Fish "~ "15", 
         subject_id==61362195 & column_name == "Fourth row Length Range "~ "6.1-9.2", 
         subject_id==61362195 & column_name == "Fourth row Mean Length "~ "7.2", 
         subject_id==61362195 & column_name == "5th row Number of Fish "~ "3", 
         subject_id==61362195 & column_name == "5th row Length Range "~ "6.5-8.1", 
         subject_id==61362195 & column_name == "5th row Mean Length " ~ "7.4", 
         subject_id==61362136 & column_name == "First row Number of Fish "~ "23", #61362136 - keep all only #change everything
         subject_id==61362136 & column_name == "First row Length Range "~ "4.6-6.3",
         subject_id==61362136 & column_name == "First row Mean Length "~ "5.4", 
         subject_id==61362136 & column_name == "Second row Number of Fish "~ "110", 
         subject_id==61362136 & column_name == "Second row Length Range "~ "5.4-7.8", 
         subject_id==61362136 & column_name == "Second row Mean Length "~ "6.3", 
         subject_id==61362136 & column_name == "Third row Number of Fish "~ "8", 
         subject_id==61362136 & column_name == "Third row Length Range "~ "6.2-7.8", 
         subject_id==61362136 & column_name == "Third row Mean Length "~ "7.1", 
         subject_id==61362136 & column_name == "Fourth row Number of Fish "~ "4", 
         subject_id==61362136 & column_name == "Fourth row Length Range "~ "7.7-8.1", 
         subject_id==61362136 & column_name == "Fourth row Mean Length " ~ "8.0", 
         subject_id==61362136 & column_name == "5th row Number of Fish "~ "1", 
         subject_id==61362136 & column_name == "5th row Length Range "~ "9.5",
         subject_id==61362136 & column_name == "5th row Mean Length "~ "9.5", 
         subject_id==61362136 & column_name == "6th row Number of Fish "~ NA_character_, 
         subject_id==61362136 & column_name == "6th row Length Range "~ NA_character_, 
         subject_id==61362136 & column_name == "6th row Mean Length "~ NA_character_, 
         subject_id==61362136 & column_name == "7th row Number of Fish "~ NA_character_, 
         subject_id==61362136 & column_name == "7th row Length Range "~ NA_character_, 
         subject_id==61362136 & column_name == "7th row Mean Length "~ NA_character_, 
         subject_id==61362136 & column_name == "8th row Number of Fish "~ NA_character_, 
         subject_id==61362136 & column_name == "8th row Length Range "~ NA_character_, 
         subject_id==61362136 & column_name == "8th row Mean Length "~ NA_character_, 
         subject_id==61362136 & column_name == "9th row Number of Fish "~ NA_character_, 
         subject_id==61362136 & column_name == "9th row Length Range "~ NA_character_, 
         subject_id==61362136 & column_name == "9th row Mean Length "~ NA_character_, 
         subject_id==61362136 & column_name == "10th row Number of Fish "~ NA_character_, 
         subject_id==61362136 & column_name == "10th row Length Range "~ NA_character_, 
         subject_id==61362136 & column_name == "10th row Mean Length "~ NA_character_, 
         subject_id==61362136 & column_name == "11th row Number of Fish "~ NA_character_, 
         subject_id==61362136 & column_name == "11th row Length Range "~ NA_character_, 
         subject_id==61362136 & column_name == "11th row Mean Length "~ NA_character_, 
         subject_id==61362136 & column_name == "12th row Number of Fish "~ NA_character_, 
         subject_id==61362136 & column_name == "12th row Length Range "~ NA_character_, 
         subject_id==61362136 & column_name == "12th row Mean Length "~ NA_character_, 
         subject_id==61362136 & column_name == "13th row Number of Fish "~ NA_character_,
         subject_id==61362136 & column_name == "13th row Mean Length "~ NA_character_,
         subject_id==59687971 & column_name == "First row Number of Fish " ~ "9",  #59687971 - first row incorrect
         subject_id==59687971 & column_name == "First row Length Range "~ "4.8-5.9", 
         subject_id==59687971 & column_name == "First row Mean Length "~ "5.4", 
                       TRUE ~ data)
      )

#merging back in manually reviewed data 
#QAQC look at cards with multiple sections (change in code above)
#58755272 - use all only - 4 rows # good 
#58755273 use all only - 5 rows #  good 
#59964030 - two gear types #  good - keep all 


lengths_clean<-gtools::smartbind(text_good, manual_length) %>%
  mutate(column_name = trimws(column_name, which = c("both"))) %>% #trim white space that is making too many column
  drop_na(data) %>%
  pivot_wider( #rotate so that each of the tasks is a column 
    id_cols = c(subject_id),
    names_from = column_name, 
    values_from = c(data), 
    values_fill = list(data = NA) ) %>% 
  filter(subject_id != 59152228 & subject_id != 58642640)  #59152228 and 58642640 are duplicate cards
  

#### dropdown ages cleaning #### 
#read in files 
#separate into four columns, one for each of the reviews if there are different answers
grow_age_values<-read.csv("GROW_BLG/BLG_data/GROW_age_values.csv", header=TRUE) %>% 
  mutate(code = as.factor(code))

dropdown<-read.csv("GROW_YEP/YEP_data/dropdown_reducer_perch_dropdowns.csv", na.strings = c("", "NA")) %>%
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
         age3=ifelse(age_hash3 =='9f71969064233', 5, age3),
         ) %>%         
         select(-c(age_hash, age_hash2, age_hash3, age_hash4))

# keep >2s OR any that were 2 but don't have disagreements (NAs) 
age_good<-filter(dropdown,  review >= 2 | review2 >= 2 | (review == 2 & is.na(review2)) ) %>%
  mutate(age= case_when(
    is.na(review2) ~ age1, # (total agreement)
    review2 > review ~ age2, # (majority selected)
    review > review2 ~ age1 # (majority selected)
  )) %>%
  select(subject_id, column_name, age) %>% 
  mutate(age = case_when(
        subject_id==59962993 & (column_name == "7th row age group " | column_name == "8th row age group ") ~ NA_real_,
        subject_id==61362136 & column_name == "Fourth row age group " ~ 5,
        subject_id==61362136 & column_name == "5th row age group " ~ 6,
        subject_id==61362136 & column_name == "6th row age group " ~ NA_real_,
        subject_id==61362136 & column_name == "7th row age group " ~ NA_real_,
        subject_id==61362136 & column_name == "8th row age group " ~ NA_real_,
        subject_id==61362136 & column_name == "9th row age group " ~ NA_real_,
        subject_id==61362136 & column_name == "10th row age group " ~ NA_real_,
        subject_id==61362136 & column_name == "11th row age group " ~ NA_real_,
        subject_id==61362136 & column_name == "12th row age group " ~ NA_real_,
        subject_id==61362136 & column_name == "13th row age group " ~ NA_real_,
        TRUE ~ age)
  )%>% 
  drop_na(age)
        

#*filter out the ones for manual review  ####
age_bad<-filter(dropdown, (review < 2  & review2 <2) | (review == 1 & is.na(review2))) %>%
  select(-c(review3, review4)) %>% 
  mutate(age = '') %>% 
  left_join(urls) %>% 
  drop_na(URL_front)

#write.csv(age_bad, "/Users/katelynking/Desktop/grow_yellowperch_ages_review.csv", row.names = FALSE)
age_manual<-read.csv( "GROW_YEP/YEP_data/grow_yellowperch_ages_review.csv") %>% #after manual review 
            drop_na(age)  %>%
  mutate(column_name = trimws(column_name, which = c("both"))) %>% #trim white space 
  left_join(grow_task_values, by=c("column_name" = "old_name")) %>%
  select(subject_id, column_name.y, age)  %>% 
  rename(column_name = column_name.y)

#join manually reviewed and good datasets 
age_data<-rbind(age_good, age_manual)%>%
  mutate(column_name = trimws(column_name, which = c("both"))) %>% #trim white space that is making too many colums
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(age), 
              values_fill = list(age = NA) ) %>% 
  filter(subject_id != 59152228 & subject_id != 58642640) #59152228 and 58642640 are duplicate cards

#*join age data: dropdown ages and length text  ####

all_grow<-left_join(age_data, lengths_clean) %>% 
  pivot_longer(-subject_id,
               names_to =c("row", ".value"),
               names_sep ="_row") %>% 
  rename(age_group = " age group", fish_count = " Number of Fish", length_range = " Length Range", length_mean = " Mean Length") %>% 
  filter(if_any(c("age_group", "fish_count", "length_range", "length_mean"), complete.cases)) %>%
  mutate(length_range = gsub("mm" , '', length_range), #remove units from some cells 
         length_mean = gsub("mm" , '', length_mean), 
         length_range = gsub("cm" , '', length_range), #remove units from some cells 
         length_mean = gsub("cm" , '', length_mean), 
         length_range = gsub("," , '-', length_range), #replace some commas with dashes
  ) %>% 
  separate(length_range, c("length_min", "length_max"), sep = "-")  %>% 
  mutate(across(where(is.character), ~ na_if(.,""))) %>% # get rid of blank cell 
  select(-c(row)) %>% 
  mutate(length_mean = gsub("-" , '', length_mean)) %>% #remove negative sign 
  mutate(fish_count = as.numeric(as.character(fish_count)), 
         length_min = as.numeric(as.character(length_min)), 
         length_max = as.numeric(as.character(length_max)),
         length_mean = as.numeric(as.character(length_mean)),
         age_group = as.numeric(as.character(age_group))
  ) %>% 
  mutate(age_group = case_when(  #CHECK NAs where there is no age group listed! 
    is.na(age_group) & subject_id==59687971 ~ 2, 
    is.na(age_group) & subject_id==58753948 ~ 11,
    is.na(age_group) & subject_id== 58754189 ~ 7, 
    is.na(age_group) & subject_id== 58754257 ~ 6, 
    is.na(age_group) & subject_id== 61361999 & fish_count == 4 ~ 6,
    is.na(age_group) & subject_id== 61361999 & fish_count == 2 ~ 7,
    is.na(age_group) & subject_id== 58754096 ~ 6,
    TRUE ~ age_group), 
    age_group = ifelse(subject_id==59963134 & length_min == 13.50, 6, age_group),#age has a ? by it, think it should be 6
    length_min = ifelse(subject_id==59687971 & age_group == 2, 4.8, length_min), #correction 
    length_max = ifelse(subject_id==59687971 & age_group == 2, 5.9, length_max),
    length_mean = ifelse(subject_id==59687971 & age_group == 2, 5.4, length_mean)
    )  %>% 
  drop_na(age_group) %>% #the rest should be NA: 58746420 no fish in this group, #58754732 wrong line, #58642646 
  distinct()  #use distinct() removes the duplicate row
  

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
  mutate(length_mean_mm = ifelse(is.na(length_mean_mm) & fish_count == 1, length_min_mm, length_mean_mm)) %>%#if only one fish, fill in length mean with the same as the min length 
  mutate(length_mean_mm= ifelse(subject_id == 59963891, 213.36, length_mean_mm ), #fix outlier / typo
         length_max_mm= ifelse(subject_id == 59963140, 193.04, length_max_mm )) #fix outlier / typo


#check for outliers 
plot(GROW$length_max_mm) 
plot(GROW$length_min_mm)
plot(GROW$length_mean_mm)

#re-order columns 
GROW <- GROW[, c(1, 15,14,13, 2, 3, 16,17,18, 4,5,6, 7, 8,9,12, 10,11)]

#final data set 
write.csv(GROW, "GROW_YEP/yep_grow_qa_qc.csv", row.names = FALSE)

#### add in DECEMBER data #### 
#*text (length info) cleaning ####
text<-read.csv("GROW_YEP/YEP_data/text_reducer_perch_dec.csv") %>%
  select(subject_id, task, 'data.aligned_text', 'data.number_views', 'data.consensus_score','data.consensus_text') %>%
  drop_na('data.number_views') %>% 
  naniar::replace_with_na(replace = list(data.consensus_text = c('-', '--', "---", '...', '. . .', "----", "-----"))) %>% #function that replaces all of a value with NA 
  drop_na('data.consensus_text') %>%
  left_join(grow_task_values) %>% # join with the task values to get the actual names
  separate(data.consensus_text, c("data", "trash"), sep = "\\(", remove = FALSE) %>% #remove parenthesis which are weights
  separate(data, into = c("data", "trash2"), sep = " ") 

#read the other data to ensure no overlap, looks like all are unique  
grow_yep<-read.csv("GROW_YEP/yep_grow_qa_qc.csv")
other_lakes<-distinct(grow_yep, subject_id) 
unique_lakes_text<-anti_join(text, other_lakes)

#*manual check consensus scores <2 #### 
#So only what was viewed by >= 2 people but half didn't agree 
text_manual<- filter(text, data.consensus_score < 2) %>% 
  left_join(grow_dates) %>% 
  drop_na(url_front) %>% 
  filter(subject_id != "82939729") %>%  #one obs bad 82939729 other good 
  select(subject_id, column_name, data)

#good text
text_good<-filter(text, data.consensus_score >= 2) %>%
  select(subject_id, column_name, data)

text_clean<-rbind(text_good, text_manual) %>%
  pivot_wider( #rotate so that each of the tasks is a column 
    id_cols = c(subject_id),
    names_from = column_name, 
    values_from = c(data), 
    values_fill = list(data = NA) )


#* dropdown #### 
grow_age_values<-read.csv("GROW_BLG/BLG_data/GROW_age_values.csv", header=TRUE)
#separate into four columns, one for each of the reviews if there are different answers
dropdown<-read.csv("GROW_YEP/YEP_data/dropdown_reducer_perch_dec.csv", na.strings = c("", "NA")) %>%
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

#*filter out the ones for manual review  ####
age_bad<-filter(dropdown, review < 2 ) %>%
  left_join(grow_dates) %>%  #these that match are cards, some are pages 
  drop_na(url_front)
#no bad observations 

# keep >=2s 
age_good<-filter(dropdown, review >= 2 ) %>%
  mutate(age= case_when(
    is.na(review2) ~ age1, # (total agreement)
    review2 > review ~ age2, # (majority selected)
    review > review2 ~ age1 # (majority selected)
  ))  %>% 
  select(subject_id, column_name, age) %>%
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(age), 
              values_fill = list(age = NA) )

#*join age data: dropdown ages and length text  ####
all_grow<-left_join(age_good, text_clean) %>% 
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

GROW<-full_join(all_grow, grow_dates) %>%
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

#QAQC check for outliers 
plot(GROW$length_max_mm) 
plot(GROW$length_min_mm)
plot(GROW$length_mean_mm) 

grow_join<-rbind(grow_yep, GROW)

write.csv(grow_join, "GROW_YEP/yep_grow_all_qaqc.csv", row.names = FALSE)


