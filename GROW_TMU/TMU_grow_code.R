### Tiger Muskie GROW CARDS ###
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
  select(task, column_name)

#read in urls 
urls<-read.csv("urls/basic_GROW_URLs.csv", na.strings = c("", "NA"))  %>% 
  distinct(subject_id, .keep_all = TRUE) 

#read in subject cross reference table 
cross_ref<-read.csv("GROW_general/grow_subject_xref.csv") 

#3 files: dropdown (age info),text (length info), question (this is if a sp is present on the card),

#### text (length info) cleaning ####
#cards are in inches unless otherwise noted - none noted mm this time? get tags from Justin 
#don't want anything in parenthesis which are weights. 
text<-read.csv("GROW_TMU/TMU_data/text-reducer-TigerMuskie-texts.csv") %>%
  select(subject_id, task, 'data.aligned_text', 'data.number_views', 'data.consensus_score','data.consensus_text', "URL.4", "URL.8") %>%
  drop_na('data.number_views') %>% 
  left_join(cross_ref) %>%
  drop_na('orig_subject_id') %>% #removes the duplicates 
  select(-c(subject_id)) %>% #remove new subjects and keep originals
  rename(subject_id = orig_subject_id) %>%
  naniar::replace_with_na(replace = list(data.consensus_text = c('-', '--', "---", '...', '. . .', "-----"))) %>% #function that replaces all of a value with NA 
  drop_na('data.consensus_text') %>%
  left_join(grow_task_values) %>% # join with the task values to get the actual names
  separate(data.consensus_text, c("data", "trash"), sep = "\\(", remove = FALSE) %>% #remove parenthesis which are weights
  separate(data, into = c("data", "trash2"), sep = " ") %>% 
  rename(url_front = URL.4, url_back = URL.8) %>% 
  select(subject_id, everything()) %>%#move subjects to first column 
  mutate(data.consensus_score = ifelse(subject_id == "72067060" & task == "T32", 2, data.consensus_score)) #this is correct 

#*manual check consensus scores <2 #### 
#data consensus was updates so that if 2 people agreed than a 3rd did not have to look at the card. 
#So only what was viewed by >= 2 people but half didnt agree 
text_bad<- filter(text, data.consensus_score < 2) 
#only 3 bad, two are incorrect and one I incorporated above, leave out these two 

#good data where consensus is >50% 
text_good<-filter(text, data.consensus_score >= 2 ) %>%
  select(subject_id, column_name, data) %>%
  mutate(column_name = trimws(column_name, which = c("both"))) %>% #trim white space that is making too many colums
  pivot_wider( #rotate so that each of the tasks is a column 
    id_cols = c(subject_id),
    names_from = column_name, 
    values_from = c(data), 
    values_fill = list(data = NA) )

#### dropdown ages cleaning #### 
#read in files (Justin already changed the hash codes here)
#separate into four columns, one for each of the reviews if there are different answers
dropdown<-read.csv("GROW_TMU/TMU_data/dropdown-reducer-TigerMuskie-dropdowns.csv", na.strings = c("", "NA")) %>%
  left_join(grow_task_values) %>% 
  select(subject_id, column_name, 'data.value') %>%
  drop_na('data.value') %>% 
  left_join(cross_ref) %>%
  drop_na('orig_subject_id') %>% #drops the duplicates 
  select(-c(subject_id)) %>% #remove new subjects and keep originals
  rename(subject_id = orig_subject_id) %>%
  mutate(data_value = gsub("'0 / 0'", "0", data.value), #fix hex codes 
         data_value = gsub("'I / 1'", "1", data_value),
         data_value = gsub("'II / 2'", "2", data_value),
         data_value = gsub("'III / 3'", "3", data_value),
         data_value = gsub("'IV / 4'", "4", data_value),
         data_value = gsub("'V / 5'", "5", data_value),
         data_value = gsub("'VI / 6'", "6", data_value),
         data_value = gsub("'VII / 7'", "7", data_value),
         data_value = gsub("'VIII / 8'", "8", data_value),
         data_value = gsub("'IX / 9 '", "9", data_value),
         data_value = gsub("'X / 10'", "10", data_value),
         data_value = gsub("X\\+", "10", data_value),
         data_value = gsub("'XI / 11'", "11", data_value),
         data_value = gsub("'XII / 12 '", "12", data_value),
         data_value = gsub("'XIII / 13'", "13", data_value),
         data_value = gsub("'XIV'", "14", data_value),
         data_value = gsub("'XV'", "15", data_value)
  ) %>% 
  separate(data_value, c("review1", "review2", "review3", "review4"), sep = ",") %>% 
  mutate(review1 = gsub("[[:punct:]]", "", review1), #remove punctuation 
         review2 = gsub("[[:punct:]]", "", review2), 
         review3 = gsub("[[:punct:]]", "", review3), 
         review4 = gsub("[[:punct:]]", "", review4),
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
  mutate(
    age1 = as.numeric(age1), 
    age2 = as.numeric(age2), 
    age3 = as.numeric(age3),
    age4 = as.numeric(age4),
  ) 


# keep >=2s because retired if 2 of 3 agreed OR any that were 2 but don't have disagreements (NAs) 
age_good<-filter(dropdown, review >= 2 | review2 >= 2 | (review == 2 & is.na(review2)) ) %>%
  mutate(age= case_when(
    is.na(review2) ~ age1, # (total agreement)
    review2 > review ~ age2, # (majority selected)
    review > review2 ~ age1 # (majority selected)
  )) %>%
  select(subject_id, column_name, age)%>%
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(age), 
              values_fill = list(age = NA))

#*filter out the ones for manual review  ####
age_bad<-filter(dropdown, (review < 2  & review2 <2) | (review == 1 & is.na(review2))) %>%
  select(-c(age3, age4, review3, review4)) %>% 
  mutate(age = '') %>% 
  left_join(urls)
#no bad ones 


#*join age data: dropdown ages and length text  ####
age<-left_join(age_good, text_good)

#re-order columns 
age <- age[, c(1, 2,8,9,10, 3, 11,18,12, 4,13,14,15, 5,16,19,17, 6, 20,21,22, 7, 23,24,25)]

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

#bind them all
all_grow<-rbind(first, second, third, fourth, fifth, sixth)

# split out ranges 
all_grow<-all_grow %>%
  mutate(length_range = gsub("mm" , '', length_range), #remove units from some cells 
         length_mean = gsub("mm" , '', length_mean), 
         length_range = gsub("cm" , '', length_range), #remove units from some cells 
         length_mean = gsub("cm" , '', length_mean)
  ) %>% 
  separate(length_range, c("length_min", "length_max"), sep = "-")%>% 
  mutate(across(where(is.character), ~ na_if(.,""))) %>% # get rid of blank cell 
  mutate(fish_count = as.numeric(as.character(fish_count)), #change values to numeric 
         length_min = as.numeric(as.character(length_min)),
         length_max = as.numeric(as.character(length_max)),
         length_mean = as.numeric(as.character(length_mean))
  )

#check NAs in age group and check if something is coerced to NA from the as.numeric code
#no NAs found 


#### join all data #### 
#join with basic dates and with lake matching info (which include already basic text)
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
write.csv(GROW, "GROW_TMU/tmu_grow_qaqc.csv", row.names = FALSE)

#### add in December data #### 
#*text (length info) cleaning ####
text<-read.csv("GROW_TMU/TMU_data/text_reducer_tigermuskie_dec.csv") %>%
  select(subject_id, task, 'data.aligned_text', 'data.number_views', 'data.consensus_score','data.consensus_text', "URL.4", "URL.8") %>%
  drop_na('data.number_views') %>% 
  naniar::replace_with_na(replace = list(data.consensus_text = c('-', '--', "---", '...', '. . .', "----", "-----"))) %>% #function that replaces all of a value with NA 
  drop_na('data.consensus_text') %>%
  left_join(grow_task_values) %>% # join with the task values to get the actual names
  separate(data.consensus_text, c("data", "trash"), sep = "\\(", remove = FALSE) %>% #remove parenthesis which are weights
  separate(data, into = c("data", "trash2"), sep = " ") %>% 
  rename(url_front = URL.4, url_back = URL.8) 


#*manual check consensus scores <2 #### 
text_bad<- filter(text, data.consensus_score < 2) 
#one subject with incorrect entries - but this is a page, so leave out 

#pivot good text
text_good<-filter(text, data.consensus_score >= 2) %>%
  select(subject_id, column_name, data)%>%
  pivot_wider( #rotate so that each of the tasks is a column 
    id_cols = c(subject_id),
    names_from = column_name, 
    values_from = c(data), 
    values_fill = list(data = NA) )

#* dropdown #### 
grow_age_values<-read.csv("GROW_BLG/BLG_data/GROW_age_values.csv", header=TRUE)
#separate into four columns, one for each of the reviews if there are different answers
dropdown<-read.csv("GROW_TMU/TMU_data/dropdown_reducer_tigermuskie_dec.csv", na.strings = c("", "NA")) %>%
  left_join(grow_task_values) %>% 
  select(subject_id, column_name, 'data.value') %>%
  drop_na('data.value') %>% 
  mutate(data_value = gsub("'0 / 0'", "0", data.value), #fix hex codes 
         data_value = gsub("'I / 1'", "1", data_value),
         data_value = gsub("'II / 2'", "2", data_value),
         data_value = gsub("'III / 3'", "3", data_value),
         data_value = gsub("'IV / 4'", "4", data_value),
         data_value = gsub("'V / 5'", "5", data_value),
         data_value = gsub("'VI / 6'", "6", data_value),
         data_value = gsub("'VII / 7'", "7", data_value),
         data_value = gsub("'VIII / 8'", "8", data_value),
         data_value = gsub("'VIII'", "8", data_value),
         data_value = gsub("'IX / 9 '", "9", data_value),
         data_value = gsub("'X / 10'", "10", data_value),
         data_value = gsub("X\\+", "10", data_value),
         data_value = gsub("'XI / 11'", "11", data_value),
         data_value = gsub("'XII / 12 '", "12", data_value),
         data_value = gsub("'XIII / 13'", "13", data_value),
         data_value = gsub("'XIV'", "14", data_value),
         data_value = gsub("'XV'", "15", data_value)
  ) %>% 
  separate(data_value, c("review1", "review2", "review3", "review4"), sep = ",") %>% 
  mutate(review1 = gsub("[[:punct:]]", "", review1), #remove punctuation 
         review2 = gsub("[[:punct:]]", "", review2), 
         review3 = gsub("[[:punct:]]", "", review3), 
         review4 = gsub("[[:punct:]]", "", review4),
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
  mutate(age1= ifelse(age1 == '9a05e76b99', 7, age1 ), #some of the hash codes were not changed 
         age1= ifelse(age1 =='6cfd34768001f', 4, age1 ), 
         age2= ifelse(age2 =='6cfd34768001f', 4, age2 ), 
         age2= ifelse(age2 == '0466fadfa1b7', 7, age2)
  ) %>% 
  mutate(
    age1 = as.numeric(age1), 
    age2 = as.numeric(age2), 
    age3 = as.numeric(age3),
    age4 = as.numeric(age4),
  ) 

#filter out the ones for manual review  ####
##same subject with incorrect entries, leave out  
age_bad<-filter(dropdown, review < 2 ) %>%
  left_join(grow_dates) #these that match are cards, some are pages 

# keep >=2s 
age_good<-filter(dropdown, review >= 2 ) %>%
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

#QAQC check for outliers 
plot(GROW$length_max_mm) 
plot(GROW$length_min_mm)
plot(GROW$length_mean_mm) 

#bind with the other data 
grow_tmu<-read.csv("GROW_TMU/tmu_grow_qaqc.csv")

grow_join<-rbind(grow_tmu, GROW)

write.csv(grow_join, "GROW_TMU/tmu_grow_all_qaqc.csv", row.names = FALSE)
