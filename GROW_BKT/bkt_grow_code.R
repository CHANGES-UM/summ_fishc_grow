### code for brook trout cleaning 

#### load libraries #### 
library(dplyr) # library for data munging 
library(tidyr) # library for data munging 
library(stringr) #library for regex functions 
library(rqdatatable) #join and replace NAs from one table with data from another 

#### load data #### 
#read in lake match data where lakes were matched to MDNR authority file   
lake_match <-read.csv("GROW_general/grow_card_lake_match_13Sep2021.csv", header=TRUE, na.strings = c("", "NA")) %>%
  select(subject_id, county, lakename, 'new.key') %>%
  rename(new_key = 'new.key') %>%
  drop_na('new_key')

#read in dates from basic workflow   
grow_dates <-read.csv("GROW_general/grow_dates.csv", header=TRUE, na.strings = c("", "NA"))

#task values 
grow_task_values<-read.csv("GROW_BLG/BLG_data/GROW_task_values.csv", header=TRUE) %>% 
  select(task, column_name) %>%
  mutate(column_name = trimws(column_name, which = c("both"))) #trim white space

#read in urls 
urls<-read.csv("urls/basic_GROW_URLs.csv", na.strings = c("", "NA"))  %>% 
  distinct(subject_id, .keep_all = TRUE) 

grow_age_values<-read.csv("GROW_BLG/BLG_data/GROW_age_values.csv", header=TRUE) %>% 
  mutate(code= as.factor(code))


#### (length info) cleaning ####
text<-read.csv("GROW_BKT/bkt_data/text_reducer_brooktrout.csv") %>%
  select(subject_id, task, 'data.aligned_text', 'data.number_views', 'data.consensus_score','data.consensus_text',"URL.4", "URL.8") %>%
  drop_na('data.number_views') %>% 
  left_join(grow_task_values) %>% 
  mutate(data.consensus_text = gsub("mm" , '', data.consensus_text), #remove units
         data.consensus_text = gsub("cm" , '', data.consensus_text), 
         data.consensus_text = gsub("in" , '', data.consensus_text)
  ) %>% 
  naniar::replace_with_na(replace = list(data.consensus_text = c('-', '--', "---", '...', '. . .', "-----", "———"))) %>% #function that replaces all of a value with NA 
  drop_na('data.consensus_text') %>%
  separate(data.consensus_text, c("data", "trash"), sep = "\\(") %>%  #gets rid of extra stuff
  select(-c(trash))%>% 
  rename(url_front = URL.4, url_back = URL.8) 

#read in dec data 
text_dec<-read.csv("GROW_BKT/bkt_data/text_reducer_brooktrout_dec.csv") %>%
  select(subject_id, task, 'data.aligned_text', 'data.number_views', 'data.consensus_score','data.consensus_text',"URL.4", "URL.8") %>%
  drop_na('data.number_views') %>% 
  left_join(grow_task_values) %>% 
  mutate(data.consensus_text = gsub("mm" , '', data.consensus_text), #remove units
         data.consensus_text = gsub("cm" , '', data.consensus_text), 
         data.consensus_text = gsub("in" , '', data.consensus_text)
  ) %>% 
  naniar::replace_with_na(replace = list(data.consensus_text = c('-', '--', "---", '...', '. . .', "-----", "———"))) %>% #function that replaces all of a value with NA 
  drop_na('data.consensus_text') %>%
  separate(data.consensus_text, c("data", "trash"), sep = "\\(") %>%  #gets rid of extra stuff
  select(-c(trash))%>% 
  rename(url_front = URL.4, url_back = URL.8) 

#join original and dec data 
text_bind<-rbind(text, text_dec)

#*manual check consensus scores <2 #### 
#data consensus was updated so that if 2 people agreed than a 3rd did not have to look at the card. 
#So only what was viewed by >= 2 people but half didnt agree 
text_bad<- filter(text_bind, data.consensus_score < 2) 
#write.csv(text_bad,  "/Users/katelynking/Desktop/grow_bkt_text_review.csv", row.names = FALSE )

#good data where consensus is >50% 
text_good<-filter(text_bind, data.consensus_score >= 2 ) %>%
  select(subject_id, column_name, data)

#merging back in manually reviewed data 
manual_length1<-read.csv("GROW_BKT/bkt_data/grow_bkt_text_review.csv", na.strings = c("", "NA")) %>%
  select(subject_id, column_name, data) %>% 
  drop_na(data)

lengths_clean<-gtools::smartbind(text_good, manual_length1) %>%
  mutate(column_name = trimws(column_name, which = c("both"))) %>% #trim white space that is making too many columns
  pivot_wider( #rotate so that each of the tasks is a column 
    id_cols = c(subject_id),
    names_from = column_name, 
    values_from = c(data), 
    values_fill = list(data = NA) )


#### fish age drop downs #### 

#read in file 
dropdown<-read.csv("GROW_BKT/bkt_data/dropdown_reducer_brooktrout.csv", na.strings = c("", "NA")) %>%
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

#read in dec file 
dropdown_dec<-read.csv("GROW_BKT/bkt_data/dropdown_reducer_brooktrout_dec.csv", na.strings = c("", "NA")) %>%
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

age_bind <- rbind(dropdown, dropdown_dec)

# keep >=2s because retired if 2 of 3 agreed OR any that were 2 but don't have disagreements (NAs) 
age_good<-filter(age_bind, review >= 2 | review2 >= 2 | (review == 2 & is.na(review2)) ) %>%
  mutate(age= case_when(
    is.na(review2) ~ age1, # (total agreement)
    review2 > review ~ age2, # (majority selected)
    review > review2 ~ age1 # (majority selected)
  )) %>%
  select(subject_id, column_name, age)

#*filter out the ones for manual review  ####
age_bad<-filter(age_bind, (review < 2  & review2 <2) | (review == 1 & is.na(review2))) %>%
  select(-c(age3, age4, review3, review4)) %>% 
  mutate(age = '') %>% 
  left_join(urls)

#manually checked age_bad and all incorrect, so nothing to join 


#good dataset 
age_data<-age_good%>%
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(age), 
              values_fill = list(age = NA) )


#####join age data: dropdown ages and length text  ####
all_grow<-left_join(age_data, lengths_clean) %>% 
  pivot_longer(-subject_id,
               names_to =c("row", ".value"),
               names_sep ="_row") %>% 
  rename(age_group = " age group", fish_count = " Number of Fish", length_range = " Length Range", length_mean = " Mean Length") %>% 
  filter(if_any(c("age_group", "fish_count", "length_range", "length_mean"), complete.cases)) %>%
  separate(length_range, c("length_min", "length_max"), sep = "-") %>% 
  mutate(fish_count = as.numeric(as.character(fish_count)), 
         length_min = as.numeric(as.character(length_min)), 
         length_max = as.numeric(as.character(length_max)),
         length_mean = as.numeric(as.character(length_mean))
  )%>% 
  select(-c(row)) 
  
  
#### clean up units and join all data #### 
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
  mutate(length_mean_mm = ifelse(is.na(length_mean_mm) & fish_count == 1, length_min_mm, length_mean_mm)) 

#QAQC check for outliers 
plot(GROW$length_max_mm) 
plot(GROW$length_min_mm)
plot(GROW$length_mean_mm) 


#reorder columns so that identifiers are at the beginning, followed by fish info, dates, and urls
GROW <- GROW[, c(1, 15,14,13, 2, 3, 16,17,18, 4,5,6, 7, 8,9,12, 10,11)]

#final data set 
write.csv(GROW, "GROW_BKT/bkt_grow_qaqc.csv", row.names = FALSE)

