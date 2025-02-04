### Cisco GROW CARDS ###
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
grow_task_values<-read.csv("GROW_BLG/BLG_data/GROW_task_values.csv", header=TRUE)

#read in urls 
urls<-read.csv("urls/basic_GROW_URLs.csv", na.strings = c("", "NA"))  %>% 
  distinct(subject_id, .keep_all = TRUE) 

#3 files, dropdown (age info), question (this is if a sp is present on the card), text (length info)

####  (length info) cleaning ####
#cards are in inches unless otherwise noted 
#don't want anything in parenthesis which are weights. 
text<-read.csv("GROW_CIS/CIS_data/text_reducer_cisco_texts.csv") %>%
  select(subject_id, task, 'data.aligned_text', 'data.number_views', 'data.consensus_score','data.consensus_text') %>%
  drop_na('data.number_views') %>% 
  naniar::replace_with_na(replace = list(data.consensus_text = c('-', '--', "---", '...', '. . .', '....', '…', '... . .'))) %>% #function that replaces all of a value with NA 
  drop_na('data.consensus_text') %>%
  left_join(grow_task_values) %>% # join with the task values to get the actual names
  left_join(urls) %>%
  separate(data.consensus_text, c("data", "trash"), sep = "\\(", remove = FALSE) %>% #remove parenthesis which are weights
  separate(data, into = c("data", "trash2"), sep = " ") 

#*manual check consensus scores <2 #### 
#data consensus was updates so that if 2 people agreed than a 3rd did not have to look at the card. 
#So only what was viewed by >= 2 people but half didnt agree 
text_bad<- filter(text, data.consensus_score < 2) 
#write.csv(text_bad,  "/Users/katelynking/Desktop/grow_cis_text_review.csv", row.names = FALSE )
#good data where consensus is >50% 
text_good<-filter(text, data.consensus_score >= 2 ) %>%
  select(subject_id, column_name, data)

#merging back in manually reviewed data 
manual_length1<-read.csv("GROW_CIS/CIS_data/grow_cisco_text_review.csv", na.strings = c("", "NA")) %>%
  select(subject_id, column_name, data) %>% 
  drop_na(data)

lengths_clean<-gtools::smartbind(text_good, manual_length1) %>%
  mutate(column_name = trimws(column_name, which = c("both"))) %>% #trim white space that is making too many colums
  pivot_wider( #rotate so that each of the tasks is a column 
    id_cols = c(subject_id),
    names_from = column_name, 
    values_from = c(data), 
    values_fill = list(data = NA) )

#### dropdown ages cleaning #### 
#read in files (Justin already changed the hash codes here)
#separate into four columns, one for each of the reviews if there are different answers
dropdown<-read.csv("GROW_CIS/CIS_data/dropdown_reducer_cisco_dropdowns.csv", na.strings = c("", "NA")) %>%
  left_join(grow_task_values) %>% 
  select(subject_id, column_name, 'data.value') %>%
  drop_na('data.value') %>% 
  rename(data_value = 'data.value') %>% 
  separate(data_value, c("review1", "review2", "review3", "review4"), sep = ",") %>% #in this case there are only 2 coolumsn with reviews, but for generality leave here 
  mutate(review1 = gsub("[[:punct:]]", "", review1), #remove punctuation 
         review2 = gsub("[[:punct:]]", "", review2), 
         review3 = gsub("[[:punct:]]", "", review3), 
         review4 = gsub("[[:punct:]]", "", review4), 
         review1 = gsub("[^0-9 ]+ \\s", "", review1), #remove characters 
         review2 = gsub("[^0-9 ]+\\s ", "", review2),
         review3 = gsub("[^0-9 ]+\\s ", "", review3),
         review4= gsub("[^0-9 ]+\\s ", "", review4),
         review1 = gsub("\\s", "_", review1), #replace space with _  
         review2 = gsub("\\s", "_", review2), 
         review3 = gsub("\\s", "_", review3),  
         review4 = gsub("\\s", "_", review4), 
         review1 = gsub('^\\D+', '',review1),  #remove everything before the digit 
         review2 = gsub('^\\D+', '',review2),
         review3 = gsub('^\\D+', '',review3),
         review4 = gsub('^\\D+', '',review4)
         ) %>% 
  separate(review1, c("age1", "review"), sep = "_", extra="merge") %>% 
  separate(review2, c("age2", "review2"), sep = "_", extra="merge") %>% 
  separate(review3, c("age3", "review3"), sep = "_", extra="merge") %>% 
  separate(review4, c("age4", "review4"), sep = "_", extra="merge") %>% 
  mutate(review = gsub('^\\D+', '',review), #remove everything before the digit (still have some leading _)
         review2 = gsub('^\\D+', '',review2),
         review3 = gsub('^\\D+', '',review3),
         review4 = gsub('^\\D+', '',review4),
         review = gsub('0_', '',review), #remove leading 0s 
         review2 = gsub('0_', '',review2),
         review3 = gsub('0_', '',review3),
         review4 = gsub('0_', '',review4),
        age1 = as.numeric(age1), 
         age2 = as.numeric(age2), 
        age3 = as.numeric(age3),
        age4 = as.numeric(age4),
        ) 

# keep >2s OR any that were 2 but don't have disagreements (NAs) 
age_good<-filter(dropdown, review >= 2 | review2 >= 2 | (review == 2 & is.na(review2)) ) %>%
  mutate(age= case_when(
    is.na(review2) ~ age1, # (total agreement)
    review2 > review ~ age2, # (majority selected)
    review > review2 ~ age1 # (majority selected)
  )) %>%
  select(subject_id, column_name, age)%>% 
  mutate(age = ifelse((subject_id == "59152224" & column_name == "7th row age group "), 9, age ))

#*filter out the ones for manual review  ####
age_bad<-filter(dropdown, (review < 2  & review2 <2) | (review == 1 & is.na(review2))) %>%
  select(-c(review3, review4)) %>% 
  mutate(age = '') %>% 
  left_join(urls) %>% 
  filter(subject_id == "59962531")%>% #one of these observations is correct
  select(subject_id, column_name, age1) %>% 
  rename(age = age1)

age_data<-rbind(age_good, age_bad)%>%
  mutate(column_name = trimws(column_name, which = c("both")))%>%
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(age), 
              values_fill = list(age = NA) )
  

#*join age data: dropdown ages and length text  ####
age<-left_join(age_data, lengths_clean)
#re-order columns 
age <- age[, c(1, 2,12,13,14, 3,15,16,17, 4,18,20,19, 5,21,22,23, 6, 24,25,26, 7, 27,28,29, 8,30,31,32, 9,33,35,34, 10,36,37,38, 11,39,40,41)]

#pull out each chunk of columns 
first<-age[,c(1:5)] 
colnames(first)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
first<- first %>% 
  filter_at(vars("age_group", "fish_count", "length_range", "length_mean"),any_vars(!is.na(.))) # drop if all 4 are NA 
second<-age[,c(1, 6:9)]
colnames(second)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
second<- second %>% 
  filter_at(vars("age_group", "fish_count", "length_range", "length_mean"),any_vars(!is.na(.))) # drop if all 4 are NA 
third<-age[,c(1, 10:13)]
colnames(third)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
third<- third %>% 
  filter_at(vars("age_group", "fish_count", "length_range", "length_mean"),any_vars(!is.na(.))) # drop if all 4 are NA 
fourth<-age[,c(1, 14:17)]
colnames(fourth)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
fourth<- fourth %>% 
  filter_at(vars("age_group", "fish_count", "length_range", "length_mean"),any_vars(!is.na(.))) # drop if all 4 are NA 
fifth<-age[,c(1, 18:21)]
colnames(fifth)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
fifth<- fifth %>% 
  filter_at(vars("age_group", "fish_count", "length_range", "length_mean"),any_vars(!is.na(.))) # drop if all 4 are NA 
sixth<-age[,c(1, 22:25)]
colnames(sixth)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
sixth<- sixth %>% 
  filter_at(vars("age_group", "fish_count", "length_range", "length_mean"),any_vars(!is.na(.))) # drop if all 4 are NA 
seventh<-age[,c(1, 26:29)]
colnames(seventh)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
seventh<- seventh %>% 
  filter_at(vars("age_group", "fish_count", "length_range", "length_mean"),any_vars(!is.na(.))) # drop if all 4 are NA 
eighth<-age[,c(1, 30:33)]
colnames(eighth)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
eighth<- eighth %>% 
  filter_at(vars("age_group", "fish_count", "length_range", "length_mean"),any_vars(!is.na(.))) # drop if all 4 are NA 
nine<-age[,c(1, 34:37)]
colnames(nine)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
nine<- nine %>% 
  filter_at(vars("age_group", "fish_count", "length_range", "length_mean"),any_vars(!is.na(.))) # drop if all 4 are NA 
ten<-age[,c(1, 38:41)]
colnames(ten)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
ten<- ten %>% 
  filter_at(vars("age_group", "fish_count", "length_range", "length_mean"),any_vars(!is.na(.))) 

#bind them all
all_grow<-rbind(first, second, third, fourth, fifth, sixth, seventh, eighth, nine, ten)

# split out ranges 
all_grow<-all_grow %>%
  mutate(length_range = gsub("mm" , '', length_range), #remove units from some cells 
         length_mean = gsub("mm" , '', length_mean), 
         length_range = gsub("cm" , '', length_range), #remove units from some cells 
         length_mean = gsub("cm" , '', length_mean)
  ) %>% 
  separate(length_range, c("length_min", "length_max"), sep = "-") %>% 
  mutate(length_mean= ifelse(subject_id == 59963301 & age_group == 3, NA,length_mean ) #typo on the card, so putting NA 
         )%>% 
  mutate(fish_count = as.numeric(as.character(fish_count)), #change values to numeric 
         length_min = as.numeric(as.character(length_min)),
         length_max = as.numeric(as.character(length_max)),
         length_mean = as.numeric(as.character(length_mean))
  )

#CHECK NAs where there is no age group listed! 
#59962468 - this is correct, no age group listed 

#### join all data #### 
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
#write.csv(GROW, "GROW_CIS/cis_grow_qaqc.csv", row.names = FALSE)

### add in dec data #### 
#these data were transcribed manually due to error in Zooniverse workflow 

data_dec<-read.csv("GROW_CIS/CIS_data/cisco_manual_transcription_dec.csv")  %>%
  separate(length_range, c("length_min", "length_max"), sep = "-") %>% 
  mutate(fish_count = as.numeric(as.character(fish_count)), 
         length_min = as.numeric(as.character(length_min)), 
         length_max = as.numeric(as.character(length_max)),
         length_mean = as.numeric(as.character(length_mean))
  )

#pull basic date data (was not kept because no url)
basic_task_values<-read.csv("GROW_BLG/BLG_data/Basic_task_value.csv", header=TRUE) %>%
  select(task, column_name)
basic_date_values<-read.csv("GROW_BLG/BLG_data/Basic_date_values.csv", header=TRUE)

basic_dropdown<-read.csv("GROW_BLG/BLG_data/dropdown_reducer_basic_GROW_dropdowns.csv", na.strings = c("", "NA")) %>%
  select(subject_id, task, 'data.value') %>% 
  filter(subject_id == "58754338") %>% 
  mutate(sep= gsub("[[:punct:]]", "", data.value), 
         sep= gsub("\\s", "_",  sep)) %>% 
  separate( col=sep, c("date_hash", "review"), sep = "_") %>%
  left_join(basic_task_values) %>% #get actual column name 
  left_join(basic_date_values, by=c('date_hash' = 'code')) %>% #get date values for the first review
  mutate(month = ifelse(is.na(month), date_hash, month)) %>% 
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(month), 
              values_fill = list(month = NA) ) %>% 
  mutate(end_date_day = ifelse(end_date_day =='Same as Begin Date (Day)', begin_date_day, end_date_day), #change "same as..." to the actual date 
          end_date_month =ifelse(end_date_month =='Same as Begin Date (Month)', begin_date_month, end_date_month),
          end_date_year = ifelse(end_date_year =='Same as Begin Date (Year)', begin_date_year, end_date_year) 
  )


GROW<-left_join(data_dec, grow_dates) %>%
  natural_join(basic_dropdown,  by  = 'subject_id', jointype = "FULL") %>%
  left_join(lake_match) %>%
  left_join(grow_units) %>% #units from manual review and tags
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
  mutate(length_mean_mm = ifelse(is.na(length_mean_mm) & fish_count == 1, length_min_mm, length_mean_mm)) 

#QAQC check for outliers 
plot(GROW$length_max_mm) 
plot(GROW$length_min_mm)
plot(GROW$length_mean_mm) 

#re-order columns 
GROW <- GROW[, c(1, 15,14,13, 2, 10, 16,17,18, 3,4,5,7, 8,9,6,12,11)]

#bind with the other cisco data 
grow_cis<-read.csv("GROW_CIS/cis_grow_qaqc.csv")

grow_join<-rbind(GROW, grow_cis)

write.csv(grow_join, "GROW_CIS/cis_grow_all_qaqc.csv", row.names = FALSE)

