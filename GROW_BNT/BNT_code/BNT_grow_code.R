### BrownTrout GROW CARDS ###
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

#### blgrow_text (length info) cleaning ####
#cards are in inches unless otherwise noted - none noted mm this time? get tags from Justin 
#don't want anything in parenthesis which are weights. 
text<-read.csv("GROW_BNT/BNT_data/text-reducer-BrownTrout-texts.csv") %>%
  select(subject_id, task, 'data.aligned_text', 'data.number_views', 'data.consensus_score','data.consensus_text') %>%
  drop_na('data.number_views') %>% 
  left_join(grow_task_values) %>% # join with the task values to get the actual names
  left_join(urls) %>%
  separate(data.consensus_text, c("data", "trash"), sep = "\\(", remove = FALSE) %>% #new column for units 
  separate(data, into = c("data", "trash2"), sep = " ") 

#*manual check consensus scores <2 #### 
#data consensus was updated so that if 2 people agreed than a 3rd did not have to look at the card. 
#So only what was viewed by >= 2 people but half didnt agree 
text_bad<- filter(text, data.consensus_score < 2) 
#write.csv(text_bad,  "/Users/katelynking/Desktop/grow_text_review.csv", row.names = FALSE )
#good data where consensus is >50% 
text_good<-filter(text, data.consensus_score >= 2 ) %>%
  select(subject_id, column_name, data)

#manual review
#text_review<-left_join(blg_text_bad2,urls ) %>% 
# select(subject_id, 'data.aligned_text', 'data.consensus_text', column_name, data, units, URL_front, URL_back)
#write.csv(text_review, "/Users/katelynking/Desktop/text_review.csv", row.names = FALSE)

#merging back in manually reviewed data 
manual_length1<-read.csv("GROW_BNT/BNT_data/grow_text_review_kt.csv", na.strings = c("", "NA")) %>%
  select(subject_id, column_name, data) 

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
dropdown<-read.csv("GROW_BNT/BNT_data/dropdown-reducer-BrownTrout-dropdowns.csv", na.strings = c("", "NA")) %>%
  left_join(grow_task_values) %>% 
  select(subject_id, column_name, 'data.value') %>%
  drop_na('data.value') %>% 
  rename(data_value = 'data.value') %>% 
  separate(data_value, c("review1", "review2", "review3", "review4"), sep = ",") %>% #in this case there are only 2 coolumsn with reviews, but for generality leave here 
 mutate(review1 = gsub("[[:punct:]]", "", review1), #remove punctuation 
        review2 = gsub("[[:punct:]]", "", review2), 
        review1 = gsub("[^0-9 ]+ \\s", "", review1), #remove characters 
        review2 = gsub("[^0-9 ]+\\s ", "", review2),
        review1 = gsub("\\s", "_", review1), #replace space with _  
        review2 = gsub("\\s", "_", review2), 
        review2 = gsub('^\\D+', '',review2) #remove everything befor the digit 
        ) %>% 
  separate(review1, c("age1", "review"), sep = "_", extra="merge") %>% 
  separate(review2, c("age2", "review2"), sep = "_", extra="merge") %>% 
  mutate(age1 = as.factor(age1), 
         age2 = as.factor(age2)) %>% 
  mutate(review= ifelse(subject_id == 58754689 & age1 == "0", 3, review)
)

# keep >2s OR any that were 2 but don't have disagreements (NAs) 
age_good<-filter(dropdown, review > 2 | review2 > 2 | (review == 2 & is.na(review2)) ) %>%
  mutate(age= case_when(
    is.na(review2) ~ age1, # (total agreement)
    review2 > review ~ age2, # (majority selected)
    review > review2 ~ age1 # (majority selected)
  )) %>%
  select(subject_id, column_name, age)

#*filter out the ones for manual review  ####
age_bad<-filter(dropdown, (review <= 2  & review2 <=2) | (review == 1 & is.na(review2))) %>%
  select(-c(review3, review4)) %>% 
  mutate(age = '') %>% 
  left_join(urls)

#write.csv(age_bad, "/Users/katelynking/Desktop/grow_ages_review.csv", row.names = FALSE)
age_manual<-read.csv( "GROW_BNT/BNT_data/ages_for_review_kt.csv") %>% #after manual review 
  select(subject_id, column_name, age)

#join manually reviewed and good datasets 
age_data<-rbind(age_good, age_manual)%>%
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(age), 
              values_fill = list(age = NA) )

#*join age data: dropdown ages and length text  ####
bnt_age<-left_join(age_data, lengths_clean)
#re-order columns 
bnt_age <- bnt_age[, c(1, 2,8,9,10, 3, 11,12,13, 4, 14,15,16, 5, 17,18,19, 6, 20,21,22, 7, 23,24,25)]

#pull out each chunk of columns 
first<-bnt_age[,c(1:5)] 
colnames(first)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
first<- first %>% 
  filter_at(vars("age_group", "fish_count", "length_range", "length_mean"),any_vars(!is.na(.))) # drop if all 4 are NA 
second<-bnt_age[,c(1, 6:9)]
colnames(second)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
second<- second %>% 
  filter_at(vars("age_group", "fish_count", "length_range", "length_mean"),any_vars(!is.na(.))) # drop if all 4 are NA 
third<-bnt_age[,c(1, 10:13)]
colnames(third)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
third<- third %>% 
  filter_at(vars("age_group", "fish_count", "length_range", "length_mean"),any_vars(!is.na(.))) # drop if all 4 are NA 
fourth<-bnt_age[,c(1, 14:17)]
colnames(fourth)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
fourth<- fourth %>% 
  filter_at(vars("age_group", "fish_count", "length_range", "length_mean"),any_vars(!is.na(.))) # drop if all 4 are NA 
fifth<-bnt_age[,c(1, 18:21)]
colnames(fifth)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
fifth<- fifth %>% 
  filter_at(vars("age_group", "fish_count", "length_range", "length_mean"),any_vars(!is.na(.))) # drop if all 4 are NA 
sixth<-bnt_age[,c(1, 22:25)]
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
  ) %>% 
  drop_na('age_group') #one should not be there 

#QAQC #check NAs in age group and check if something is coerced to NA from the as.numeric code
#one NA from a cell that says "unknown" ok 


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

#QAQC #check for outliers 
plot(GROW$length_max_mm) 
plot(GROW$length_min_mm)
plot(GROW$length_mean_mm)


#re-order columns 
GROW <- GROW[, c(1, 15,14,13, 2, 3, 16,17,18, 4,5,6, 7, 8,9,12, 10,11)]

#final data set 
write.csv(GROW, "GROW_BNT/bnt_grow_qaqc.csv", row.names = FALSE)


#### add dec data #### 
#### text (length info) cleaning ####
text<-read.csv("GROW_BNT/BNT_data/text_reducer_browntrout_dec.csv") %>%
  select(subject_id, task, 'data.aligned_text', 'data.number_views', 'data.consensus_score','data.consensus_text', "URL.4", "URL.8") %>%
  drop_na('data.number_views') %>%
  left_join(grow_task_values) %>% # join with the task values to get the actual names
  separate(data.consensus_text, c("data", "trash"), sep = "\\(", remove = FALSE) %>% #remove parenthesis which are weights
  separate(data, into = c("data", "trash2"), sep = " ") %>% 
  rename(url_front = URL.4, url_back = URL.8) 

#good data where consensus is >50% (note no bad text)
text_good<-filter(text, data.consensus_score >= 2 ) %>%
  select(subject_id,column_name, data) %>% 
  pivot_wider( #rotate so that each of the tasks is a column 
    id_cols = c(subject_id), #id_cols = c(subject_id, orig_subject_id) change back!! 
    names_from = column_name, 
    values_from = c(data), 
    values_fill = list(data = NA) )  

#### dropdown ages cleaning #### 
#read in files (Justin already changed the hash codes here)
#separate into four columns, one for each of the reviews if there are different answers
dropdown<-read.csv("GROW_BNT/BNT_data/dropdown_reducer_browntrout_dec.csv", na.strings = c("", "NA")) %>%
  left_join(grow_task_values) %>% 
  select(subject_id, column_name, 'data.value') %>%
  mutate(data_value = gsub("'0 / 0'", "0", data.value), #fix hex codes 
         data_value = gsub("'I / 1'", "1", data_value),
         data_value = gsub("'II / 2'", "2", data_value),
         data_value = gsub("'III / 3'", "3", data_value),
  ) %>% 
  separate(data_value, c("review1"), sep = ",") %>% 
  mutate(review1 = gsub("[[:punct:]]", "", review1), #remove punctuation 
         review1 = gsub("\\s", "_", review1), #replace space with _  
         review1 = gsub('^\\D+', '',review1),  #remove everything before the digit 
  ) %>% 
  separate(review1, c("age1", "review"), sep = "_", extra="merge") %>% 
  mutate(
    age = as.numeric(age1)
  ) %>%
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(age), 
              values_fill = list(age = NA) )


# only one observation and all consensus 

#*join age data: dropdown ages and length text  ####
all_grow<-left_join(dropdown, text_good) %>% 
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


#reorder columns so that identifiers are at the beginning, followed by fish info, dates, and urls
GROW <- GROW[, c(1, 15,14,13, 2, 3, 16,17,18, 4,5,6, 7, 8,9,12, 10,11)]

#bind with the other data 
grow_bnt<-read.csv("GROW_BNT/bnt_grow_qaqc.csv")

grow_join<-rbind(GROW, grow_bnt)

write.csv(grow_join, "GROW_BNT/bnt_grow_all_qaqc.csv", row.names = FALSE)



