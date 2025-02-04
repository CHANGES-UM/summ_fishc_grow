### Largemouth bass GROW CARDS ###
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


#3 files: dropdown (age info),text (length info), question (this is if a sp is present on the card),

#### text (length info) cleaning ####
#cards are in inches unless otherwise noted - none noted mm this time? get tags from Justin 
#don't want anything in parenthesis which are weights. 
text<-read.csv("GROW_LMB/lmb_data/text_reducer_lmb_texts.csv") %>%
  select(subject_id, task, 'data.aligned_text', 'data.number_views', 'data.consensus_score','data.consensus_text', "URL.4", "URL.8") %>%
  drop_na('data.number_views') %>% 
  naniar::replace_with_na(replace = list(data.consensus_text = c('-', '--', "---", '...', '. . .', "----", "-----", "…", "......."))) %>% #function that replaces all of a value with NA 
  drop_na('data.consensus_text') %>%
  left_join(grow_task_values) %>% # join with the task values to get the actual names
  separate(data.consensus_text, c("data", "trash"), sep = "\\(", remove = FALSE) %>% #remove parenthesis which are weights
  separate(data, into = c("data", "trash2"), sep = " ") %>% 
  rename(url_front = URL.4, url_back = URL.8) 
  

#*manual check consensus scores <2 #### 
#data consensus was updates so that if 2 people agreed than a 3rd did not have to look at the card. 
#So only what was viewed by >= 2 people but half didnt agree 
text_bad<- filter(text, data.consensus_score < 2) 
#write.csv(text_bad,  "/Users/katelynking/Desktop/grow_lmb_text_review.csv", row.names = FALSE )

#good data where consensus is >50% 
text_good<-filter(text, data.consensus_score >= 2 ) %>%
  select(subject_id, column_name, data) %>% 
  mutate(data = ifelse((subject_id ==59963012 & column_name == "8th row Number of Fish "), NA, data), #remove back-calculated averages 
         data = ifelse((subject_id ==59963012 & column_name == "8th row Mean Length "), NA, data),
         data = ifelse((subject_id ==59963012 & column_name == "9th row Number of Fish "), NA, data),
         data = ifelse((subject_id ==59963012 & column_name == "9th row Mean Length "), NA, data),
         data = ifelse((subject_id ==59963012 & column_name == "10th row Number of Fish "), NA, data),
         data = ifelse((subject_id ==59963012 & column_name == "10th row Mean Length "), NA, data),
         data = ifelse((subject_id ==59963012 & column_name == "11th row Number of Fish "), NA, data),
         data = ifelse((subject_id ==59963012 & column_name == "11th row Mean Length "), NA, data),
         data = ifelse((subject_id ==59963012 & column_name == "12th row Number of Fish "), NA, data),
         data = ifelse((subject_id ==59963012 & column_name == "12th row Mean Length "), NA, data),
         data = ifelse((subject_id ==58754284 & column_name == "Second row Number of Fish "), NA, data), 
        data = ifelse((subject_id ==58754284 & column_name == "Second row Length Range "), NA, data),
        data = ifelse((subject_id ==58754284 & column_name == "Second row Mean Length "), NA, data)
         ) %>%
  drop_na('data')

#add rows of missing data
text_new<- text_good %>% 
  tibble::add_row(subject_id = 61362176, column_name="14th row Number of Fish", data = "1") %>%
  tibble::add_row(subject_id = 61362176, column_name="14th row Length Range", data = "22.2")


#merging back in manually reviewed data 
manual_length1<-read.csv("GROW_LMB/lmb_data/grow_largemouthbass_text_review.csv", na.strings = c("", "NA")) %>%
  drop_na('data') %>% 
  mutate(column_name = trimws(column_name, which = c("both"))) %>% #trim white space 
  left_join(grow_task_values, by=c("column_name" = "old_name")) %>%
  select(subject_id, column_name.y, data)  %>% 
  rename(column_name = column_name.y)

lengths_clean<-gtools::smartbind(text_new, manual_length1) %>%
  mutate(column_name = trimws(column_name, which = c("both"))) %>% #trim white space that is making too many colums
  pivot_wider( #rotate so that each of the tasks is a column 
    id_cols = c(subject_id),
    names_from = column_name, 
    values_from = c(data), 
    values_fill = list(data = NA) ) %>% 
  filter(subject_id !=58755313)  #remove duplicate 58755313 is a copy of the card 


#### dropdown ages cleaning #### 
#read in files (Justin already changed the hash codes here)
#separate into four columns, one for each of the reviews if there are different answers
dropdown<-read.csv("GROW_LMB/lmb_data/dropdown_reducer_lmb_dropdowns.csv", na.strings = c("", "NA")) %>%
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


# keep >=2s because retired if 2 of 3 agreed OR any that were 2 but don't have disagreements (NAs) 
age_good<-filter(dropdown, review >= 2 | review2 >= 2 | (review == 2 & is.na(review2)) ) %>%
  mutate(age= case_when(
    is.na(review2) ~ age1, # (total agreement)
    review2 > review ~ age2, # (majority selected)
    review > review2 ~ age1 # (majority selected)
  )) %>%
  select(subject_id, column_name, age) %>% 
  mutate(age = ifelse((subject_id ==59963012 & column_name == "8th row age group "), NA, age), #remove back-calculated averages 
         age = ifelse((subject_id ==59963012 & column_name == "9th row age group "), NA, age), 
         age = ifelse((subject_id ==59963012 & column_name == "10th row age group "), NA, age), 
         age = ifelse((subject_id ==59963012 & column_name == "11th row age group "), NA, age),
         age = ifelse((subject_id ==59963012 & column_name == "12th row age group "), NA, age),
         age = ifelse((subject_id ==61362176 & column_name == "12th row age group "), 14, age) #add missing data
         )  %>%
  tibble::add_row(subject_id = 61362176, column_name="13th row age group ", age = 18) %>%
 tibble::add_row(subject_id = 61362176, column_name="14th row age group ", age = 21)


#*filter out the ones for manual review  ####
age_bad<-filter(dropdown, (review < 2  & review2 <2) | (review == 1 & is.na(review2))) %>%
  select(-c(age3, age4, review3, review4)) %>% 
  mutate(age = '') %>% 
  left_join(urls)


#write.csv(age_bad, "/Users/katelynking/Desktop/grow_lmb_age_review.csv", row.names = FALSE)
age_manual<-read.csv( "GROW_LMB/lmb_data/grow_largemouthbass_age_review.csv") %>% 
  select(subject_id, column_name, age)  %>%
  drop_na('age') %>% 
  mutate(column_name = trimws(column_name, which = c("both"))) %>% #trim white space 
  left_join(grow_task_values, by=c("column_name" = "old_name")) %>%
  select(subject_id, column_name.y, age)  %>% 
  rename(column_name = column_name.y)


#join manually reviewed and good datasets 
age_data<-rbind(age_good, age_manual)%>%
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(age), 
              values_fill = list(age = NA) ) %>% 
  filter(subject_id !=58755313)  #remove duplicate 58755313 is a copy of the card 

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
         length_mean = gsub("cm" , '', length_mean)
  ) %>%
  separate(length_range, c("length_min", "length_max"), sep = "-") %>% 
  mutate(fish_count= ifelse(subject_id == 58746220 & fish_count == "No", '0',fish_count ), 
         length_mean= ifelse(subject_id == 58642649 & age_group == 2, '9.0',  length_mean), 
         fish_count= ifelse(subject_id == 58746387 & age_group == 4, NA, fish_count), 
         length_min = ifelse(subject_id == 59962252 & age_group == 0, 0.5, length_min), 
         length_mean = ifelse(subject_id == 59962252 & age_group == 0, 0.5, length_mean),
         length_min = ifelse(subject_id == 59962917 & age_group == 3, 10.2, length_min), #correct typo
         length_max = ifelse(subject_id == 59962917 & age_group == 3, 12.9, length_max),
         length_mean = ifelse(subject_id == 58746158 & age_group == 0, 3.2, length_mean), 
         age_group = ifelse(subject_id == 59688043 &  fish_count == "9", 1, age_group), #fill in missing age groups 
         age_group = ifelse(subject_id == 59688043 &  length_min == "7.0", 2, age_group), 
         age_group = ifelse(subject_id == 59688043 &  fish_count == "5", 3, age_group), 
         age_group = ifelse(subject_id == 59688043 &  fish_count == "4", 4, age_group), 
         age_group = ifelse(subject_id == 59688043 &  length_min == "14", 5, age_group),
        age_group = ifelse(subject_id == 58754364 & fish_count == "2", 4, age_group), 
        age_group = ifelse(subject_id == 58754364 & fish_count == "10", 5, age_group),
       age_group = ifelse(subject_id == 58754364 & fish_count == "3", 6, age_group), 
       age_group = ifelse(subject_id == 58754364 & length_min == "5.1", 1, age_group),
        age_group = ifelse(subject_id == 58746282 & fish_count == "1", 8, age_group),
        age_group = ifelse(subject_id == 58746282 & length_min == "15.7", 7, age_group),
        age_group = ifelse(subject_id == 58754377  & length_min == "13.1", 4, age_group), 
        age_group = ifelse(subject_id == 58754377  & length_min == "15.1", 5, age_group), 
        age_group = ifelse(subject_id == 58754377  & fish_count == 6, 3, age_group),
        age_group = ifelse(subject_id == 58754685 & length_min == "14.2", 3, age_group), 
        age_group = ifelse(subject_id == 58755146 & length_min == "17.0", 6, age_group), 
        age_group = ifelse(subject_id == 58755146 & length_min == "12", 1, age_group), 
        length_min = ifelse(subject_id == 58755146 & age_group == 1, 5.5, length_min), 
        length_max = ifelse(subject_id == 58755146 & age_group == 1, 5.5, length_max), 
        length_mean = ifelse(subject_id == 58755146 & age_group == 1, 5.5, length_mean), 
        age_group = ifelse(subject_id == 59962278 & length_min == "17.3", 7, age_group), 
        age_group = ifelse(subject_id == 59962648 & fish_count == "4", 0, age_group),
        age_group = ifelse(subject_id == 59964404 & length_min == "9.8", 3, age_group),
        age_group = ifelse(subject_id == 59964404 & length_min == "12.7", 4, age_group),
        age_group = ifelse(subject_id == 59964404 & length_min == "14.7", 5, age_group),  
        age_group = ifelse(subject_id == 59964404 & length_min == "7.4", 2, age_group), 
        age_group = ifelse(subject_id == 59964606 & fish_count == "4", 6, age_group), 
        fish_count = ifelse(subject_id == 59964606 & is.na(age_group), "1", fish_count), #change this line 
        length_min = ifelse(subject_id == 59964606 & is.na(age_group), "13.5", length_min), #change this line 
        length_max = ifelse(subject_id == 59964606 & is.na(age_group), "13.5", length_max), 
        length_mean = ifelse(subject_id == 59964606 & is.na(age_group), "13.5", length_mean), 
        age_group = ifelse(subject_id == 59964606 & is.na(age_group), 5, age_group), 
        age_group = ifelse(subject_id == 61362003 & length_min == "9.7", 3, age_group), 
        length_max = ifelse(subject_id == 58746045 & age_group == "6", "13.9", length_max), #typo
       length_min = ifelse(subject_id == 59963582 & age_group == "9", "13.8", length_min), #typo 
       length_mean = ifelse(subject_id == 59964200 & age_group == "11", "18.9", length_mean), #typo 
        age_group = ifelse(subject_id == 59962624 & is.na(age_group), "unknown", age_group), # 59962624 NA ok for this because can't read the age
        age_group = ifelse(subject_id == 59964580 & is.na(age_group), "unknown", age_group),#59964580 NA Ok for this because no age listed  
  )   %>% 
  mutate(across(where(is.character), ~ na_if(.,"")))%>% # get rid of blank cell 
  drop_na(age_group) %>% #remove NA lines (I checked all of these)
  mutate(age_group = ifelse(age_group == "unknown", NA, age_group), #put unknown back to NA 
         age_group = as.numeric(as.character(age_group))
  ) %>%
  mutate(fish_count = as.numeric(as.character(fish_count)), #change values to numeric 
         length_min = as.numeric(as.character(length_min)),
         length_max = as.numeric(as.character(length_max)),
         length_mean = as.numeric(as.character(length_mean))
  ) %>% 
  select(-c(row))   %>% 
  distinct(.keep_all = TRUE)   #remove duplicate rows 
  

#CHECK NAs where there is no age group listed! 
#58754586 - remove NA line  
#58754284 - remove NA line  
# 59962624 NA ok for this because can't read the age
#59964580 NA Ok for this because no age listed  

#### join all data #### 
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
plot(GROW$length_max_mm) #one outlier is as written on the card
plot(GROW$length_min_mm)
plot(GROW$length_mean_mm)

#re-order columns 
GROW <- GROW[, c(1, 15,14,13, 2, 3, 16,17,18, 4,5,6, 7, 8,9,12, 10,11)]

#final data set 
write.csv(GROW, "GROW_LMB/lmb_grow_qaqc.csv", row.names = FALSE)

#### add dec data ####

#### text (length info) cleaning ####
text<-read.csv("GROW_LMB/lmb_data/text_reducer_lmb_dec.csv") %>%
  select(subject_id, task, 'data.aligned_text', 'data.number_views', 'data.consensus_score','data.consensus_text', "URL.4", "URL.8") %>%
  drop_na('data.number_views') %>%
  left_join(grow_task_values) %>% # join with the task values to get the actual names
  separate(data.consensus_text, c("data", "trash"), sep = "\\(", remove = FALSE) %>% #remove parenthesis which are weights
  separate(data, into = c("data", "trash2"), sep = " ") %>% 
  rename(url_front = URL.4, url_back = URL.8) 

#what was viewed by >= 2 people but half didnt agree or only 1 person reviewed 
text_bad<- filter(text, data.consensus_score < 2) 
#checked all of these manually, all correct 

#so keep all 
text_good<-select(text, subject_id,column_name, data) %>% 
  pivot_wider( #rotate so that each of the tasks is a column 
    id_cols = c(subject_id),
    names_from = column_name, 
    values_from = c(data), 
    values_fill = list(data = NA) ) 


#### dropdown ages cleaning #### 
#read in files (Justin already changed the hash codes here)
#separate into four columns, one for each of the reviews if there are different answers
dropdown<-read.csv("GROW_LMB/lmb_data/dropdown_reducer_lmb_dec.csv", na.strings = c("", "NA")) %>%
  left_join(grow_task_values) %>% 
  select(subject_id, column_name, 'data.value') %>%
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
  separate(data_value, c("review1"), sep = ",") %>% 
  mutate(review1 = gsub("[[:punct:]]", "", review1), #remove punctuation 
         review1 = gsub("\\s", "_", review1), #replace space with _  
         review1 = gsub('^\\D+', '',review1),  #remove everything before the digit 
  ) %>% 
  separate(review1, c("age1", "review"), sep = "_", extra="merge") %>% 
  mutate(
    age1 = as.numeric(age1)
  ) 

# keep >=  2 
age_good<-filter(dropdown, review >= 2 ) %>%
  select(subject_id, column_name, age1)

#filter out the ones for manual review ####
url_lmb<-read.csv("GROW_LMB/lmb_data/dropdown_reducer_lmb_dec.csv", na.strings = c("", "NA"))%>% 
  select(subject_id, URL.4, URL.8) %>% 
  rename(url_front = URL.4, url_back = URL.8) %>% 
  distinct(subject_id, .keep_all = TRUE)

age_bad<-filter(dropdown, review < 2 )  %>% 
  left_join(url_lmb)
#these are all good 


#all dropdowns were good so just pivot 
age_data<-select(dropdown, subject_id, column_name, age1) %>%
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(age1), 
              values_fill = list(age1 = NA) )

#*join age data: dropdown ages and length text  ####
all_grow<-left_join(age_data, text_good) %>% 
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

#### clean up units and join all data #### 
grow_units<-read.csv("/Users/katelynking/Desktop/grow_card_units.csv")  

GROW<-left_join(all_grow, grow_dates) %>%
  left_join(lake_match) %>%
  left_join(grow_units) %>% #units from manual review and tags
  drop_na('url_front') %>% #drop cards without a url, these are grow pages 
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


#bind with the other data 
grow_lmb <-read.csv("GROW_LMB/lmb_grow_qaqc.csv")

grow_join<-rbind( grow_lmb, GROW)

write.csv(grow_join, "GROW_LMB/lmb_grow_all_qaqc.csv", row.names = FALSE)

