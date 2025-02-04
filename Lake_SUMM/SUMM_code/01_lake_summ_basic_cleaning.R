### Lake Summary Basic workflow cleaning ###
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

#two missing urls that only have the "old card" subject id but need the original subject ID 
urls[nrow(urls) + 1,] <- c(59070552,  'https://changes-ifr.s3.us-east-2.amazonaws.com/ss2/SUMM/double/b_first/Otsego_Ford_SUMM_10.jpeg', 
                           'https://changes-ifr.s3.us-east-2.amazonaws.com/ss2/SUMM/double/b_first/Otsego_Ford_SUMM_5.jpeg'
                          )

urls[nrow(urls) + 1,] <- c(59070553, 'https://changes-ifr.s3.us-east-2.amazonaws.com/ss2/SUMM/double/b_first/Otsego_Ford_SUMM_6.jpeg',
                           'https://changes-ifr.s3.us-east-2.amazonaws.com/ss2/SUMM/double/b_first/Otsego_Ford_SUMM_7.jpeg'
                           )

urls$subject_id<-as.integer(as.character(urls$subject_id))

#read in files with values to match (e.g. the date hash and task names) 
basic_task_values<-read.csv("GROW_BLG/BLG_data/Basic_task_value.csv", header=TRUE) %>%
  select(task, column_name)

basic_date_values<-read.csv("GROW_BLG/BLG_data/Basic_date_values.csv", header=TRUE)

#update this code with the following changes to remove duplicates also need to add in lakes names 
problem_subjects<-read.csv("whole_database/AFDMF_problem_subjects.csv")
dup_subjects<-read.csv("whole_database/duplicates_annotated.csv") %>% #1198
  filter(Notes != "delete") %>% #1072 select only the ones to keep 
  distinct(filename_1, .keep_all = TRUE) %>% #598
  mutate(duplicate_keep = "Yes") %>%  # create a column that says whether or not to keep 
  select(subject_id, duplicate_keep)

all_subjects<-read.csv("whole_database/duplicates_annotated.csv") %>% #1198
  left_join(dup_subjects) %>% 
  mutate(duplicate_keep = ifelse(is.na(duplicate_keep), "No", duplicate_keep) ) #mark everything else No do not keep 

summ_dat<-read.csv("summary_card_data.csv") %>% 
  anti_join(problem_subjects) %>% #2174
  left_join(all_subjects) %>% 
  mutate(duplicate_keep = ifelse(is.na(duplicate_keep), "Yes", duplicate_keep) ) %>%#if NA then was not a duplicate, so keep 
  filter(duplicate_keep != "No") #2120 


#### BASIC SUMM dropdowns (dates) #### 
#dropdown_reducer_basic_SUMM_dropdowns
basic_dropdown<-read.csv("Lake_SUMM/SUMM_data/dropdown_reducer_basic_SUMM.csv", na.strings = c("", "NA")) %>%
  select(subject_id, task, 'data.value') %>%
  drop_na('data.value') %>% 
  rename(data_value = 'data.value') 

basic_dropdown$sep <- gsub("[[:punct:]]", "", basic_dropdown$data_value)  #remove punctuation 
basic_dropdown$sep<-gsub("\\s", "_", basic_dropdown$sep) #replace space with _
basic_dropdown<- separate(data=basic_dropdown, col=sep, c("date_hash", "review", "date_hash2", "review2", "date_hash3", "review3", "date_hash4", "review4" ), sep = "_")
basic_dropdown <-left_join(basic_dropdown, basic_task_values) %>% #get actual column name 
  left_join(basic_date_values, by=c('date_hash' = 'code')) %>% #get date values for the first review
  left_join(basic_date_values, by=c('date_hash2' = 'code'))  %>% #get date values for the 2nd review
  left_join(basic_date_values, by=c('date_hash3' = 'code')) %>% #get date values for the 3rd review
  left_join(basic_date_values, by=c('date_hash4' = 'code')) %>% #get date values for the 4th review
  rename(month1=month.x, month2=month.y, month3=month.x.x, month4=month.y.y)
basic_dropdown$month1<-ifelse(is.na(basic_dropdown$month1), basic_dropdown$date_hash, basic_dropdown$month1) #if month is NA then replace with the day/year, else keep month 
basic_dropdown$month2<-ifelse(is.na(basic_dropdown$month2), basic_dropdown$date_hash2, basic_dropdown$month2) #if month is NA then replace with the day/year, else keep month 
basic_dropdown$month3<-ifelse(is.na(basic_dropdown$month3), basic_dropdown$date_hash3, basic_dropdown$month3) #if month is NA then replace with the day/year, else keep month 
basic_dropdown$month4<-ifelse(is.na(basic_dropdown$month4), basic_dropdown$date_hash4, basic_dropdown$month4) #if month is NA then replace with the day/year, else keep month 

# keep 4s and 3s OR any that were 3,2,1 but don't have disagreements (NAs)- because you can leave end date blank or pick "same as", we will keep 1s 
dates_good<-filter(basic_dropdown, review >= 3 | is.na(review2) | review2 >= 3) %>%
  mutate(date= case_when(
    is.na(review2) ~ month1, # if ~ then 
    review2 > review ~ month2, # if ~ then 
    review > review2 ~ month1 # if ~ then 
  )) %>%
  select(subject_id, column_name, date)

basic_dates_format<-tidyr::pivot_wider(data= dates_good, 
                                       id_cols = c(subject_id),
                                       names_from = column_name, 
                                       values_from = c(date), 
                                       values_fill = list(date = NA) )

#filter out the ones that didn't make it ( make sure bad + good = original basic_dropdown)
dates_bad<-filter(basic_dropdown, review < 3 & !is.na(review2) & review2 < 3) %>%
  select(-c(task, data_value, date_hash, date_hash2, date_hash3, date_hash4))

#mutate if there is a split because choosing 'same as' 
dates_new<-dates_bad %>%
  mutate(date= case_when( 
    (month2 == 'Same as Begin Date (Month)' | month2 == 'Same as Begin Date (Day)' | month2 == 'Same as Begin Date (Year)') & is.na(month3)   ~ month1, # if ~ then 
    (month1 == 'Same as Begin Date (Month)' | month1 == 'Same as Begin Date (Day)' | month1 == 'Same as Begin Date (Year)') & is.na(month3)   ~ month2
    #else be NA if its not either of these conditions 
  ))

basic_dates_format2<-tidyr::pivot_wider(data= dates_new, 
                                        id_cols = c(subject_id),
                                        names_from = column_name, 
                                        values_from = c(date), 
                                        values_fill = list(date = NA) )

#want to join and replace NAs from one table with data from another if there, need rqdatatable package
dates_clean<-natural_join(basic_dates_format, basic_dates_format2, by  = 'subject_id', jointype = "FULL")

#observations for manual review by Kartik ###
dates_still_na<-filter(dates_new, is.na(date)) #181
dates_for_review<-left_join(dates_still_na,front_back )
#write.csv(dates_for_review, "/Users/katelynking/Desktop/dates_summ_review.csv", row.names = FALSE)
#read in manual dates 
dates_manual<-read.csv("Lake_SUMM/SUMM_data/manual_review_data/dates_summ_review_KT.csv") %>% 
  select(subject_id, column_name, date) %>%
  pivot_wider(
    id_cols = c(subject_id),
    names_from = column_name, 
    values_from = c(date), 
    values_fill = list(date = NA) ) 

#join manual with the other dates data
dates_clean_plus_manual<-natural_join(dates_clean, dates_manual, by  = 'subject_id', jointype = "FULL") %>%
  left_join(urls) %>%
  drop_na('front') 

#change "same as..." to the actual date 
dates_clean_plus_manual$end_date_day<-ifelse(dates_clean_plus_manual$end_date_day =='Same as Begin Date (Day)', dates_clean_plus_manual$begin_date_day, dates_clean_plus_manual$end_date_day) 
dates_clean_plus_manual$end_date_month<-ifelse(dates_clean_plus_manual$end_date_month =='Same as Begin Date (Month)', dates_clean_plus_manual$begin_date_month, dates_clean_plus_manual$end_date_month) 
dates_clean_plus_manual$end_date_year<-ifelse(dates_clean_plus_manual$end_date_year =='Same as Begin Date (Year)', dates_clean_plus_manual$begin_date_year, dates_clean_plus_manual$end_date_year) 

#QA/QC:check begin and end years that don't match #26 obs 
#dates_clean_plus_manual$review<-ifelse(dates_clean_plus_manual$begin_date_year != dates_clean_plus_manual$end_date_year, TRUE, FALSE)
dates_clean_plus_manual$end_date_year<-ifelse(dates_clean_plus_manual$subject_id == 58643121, 1930, dates_clean_plus_manual$end_date_year) #
dates_clean_plus_manual$end_date_year<-ifelse(dates_clean_plus_manual$subject_id == 58688029, 1938, dates_clean_plus_manual$end_date_year) #
dates_clean_plus_manual$end_date_month<-ifelse(dates_clean_plus_manual$subject_id == 58688029, 'July', dates_clean_plus_manual$end_date_month) #
dates_clean_plus_manual$end_date_month<-ifelse(dates_clean_plus_manual$subject_id == 59070482, 'September', dates_clean_plus_manual$end_date_month) #
dates_clean_plus_manual$end_date_day<-ifelse(dates_clean_plus_manual$subject_id == 59070482, 3, dates_clean_plus_manual$end_date_day) #
dates_clean_plus_manual$end_date_year<-ifelse(dates_clean_plus_manual$subject_id == 59070482, 1930, dates_clean_plus_manual$end_date_year) #
dates_clean_plus_manual$end_date_year<-ifelse(dates_clean_plus_manual$subject_id == 59071127, 1953, dates_clean_plus_manual$end_date_year) #
dates_clean_plus_manual$end_date_year<-ifelse(dates_clean_plus_manual$subject_id == 59071276, 1961, dates_clean_plus_manual$end_date_year) #
dates_clean_plus_manual$end_date_month<-ifelse(dates_clean_plus_manual$subject_id == 59071349, 'August', dates_clean_plus_manual$end_date_month) #
dates_clean_plus_manual$end_date_day<-ifelse(dates_clean_plus_manual$subject_id == 59071349, NA, dates_clean_plus_manual$end_date_day) #
dates_clean_plus_manual$end_date_year<-ifelse(dates_clean_plus_manual$subject_id == 59071349, 1959, dates_clean_plus_manual$end_date_year) #
dates_clean_plus_manual$begin_date_day<-ifelse(dates_clean_plus_manual$subject_id == 59071349, NA, dates_clean_plus_manual$begin_date_day) #
dates_clean_plus_manual$end_date_year<-ifelse(dates_clean_plus_manual$subject_id == 61362604, 1938, dates_clean_plus_manual$end_date_year) #
dates_clean_plus_manual$end_date_year<-ifelse(dates_clean_plus_manual$subject_id == 59672349, 1979, dates_clean_plus_manual$end_date_year) #
dates_clean_plus_manual$begin_date_year<-ifelse(dates_clean_plus_manual$subject_id == 59672349, 1979, dates_clean_plus_manual$begin_date_year) #
dates_clean_plus_manual$begin_date_day<-ifelse(dates_clean_plus_manual$subject_id == 59672349, 16, dates_clean_plus_manual$begin_date_day) #

#check again
#dates_clean_plus_manual$review2<-ifelse(dates_clean_plus_manual$begin_date_year != dates_clean_plus_manual$end_date_year, TRUE, FALSE)

#write.csv(dates_clean_plus_manual, 'Lake_SUMM/SUMM_data/clean_data/clean_dates.csv', row.names = FALSE)

#### December data #### 
basic_dropdown<-read.csv("Lake_SUMM/SUMM_data/dec_data/basic_SUMM_dropdown_dec.csv", na.strings = c("", "NA")) %>% #date hash already replaced
  select(subject_id, task, 'data.value', 'URL.4', 'URL.8') %>%
  drop_na('data.value') %>% 
  rename(data_value = 'data.value', url_front = 'URL.4', url_back ='URL.8' ) %>% 
  mutate(sep = gsub("[[:punct:]]", "", data_value),   #remove punctuation 
        sep = gsub("\\s", "_", sep), #replace space with _
        sep = gsub("Same_as_Begin_Date_Year", "same", sep ), 
        sep = gsub("Same_as_Begin_Date_Month", "same", sep ), 
        sep = gsub("Same_as_Begin_Date_Day", "same", sep )
        )%>% 
  separate(col=sep, c("date_hash", "review", "date_hash2", "review2", "date_hash3", "review3", "date_hash4", "review4" ), sep = "_") %>% 
  left_join(basic_task_values)
  
#filter out the ones that dont have consensus (review is split 50:50 or less)
dates_bad<-filter(basic_dropdown, (review < 2 & is.na(review2)) | (review < 2 &review2 < 2)) %>%
  select(-c(task, data_value)) %>% 
  mutate(date = date_hash2, # checked all of these, and choosing review 2 with a few updates 
         date = ifelse(is.na(date), date_hash, date), 
         date = ifelse(subject_id == 82940993 & column_name == "end_date_month", 'June', date ), 
         date = ifelse(subject_id == 82940993 & column_name == "end_date_day", 13, date), 
         date = ifelse(subject_id == 82940974, 1953, date), 
         date = ifelse(subject_id == 82940952 & column_name == "end_date_year", 1977, date) 
  )%>%
  select(subject_id, column_name, date, url_front, url_back)%>% 
  pivot_wider(  id_cols = c(subject_id, url_front, url_back),
                names_from = column_name, 
                values_from = c(date), 
                values_fill = list(date = NA) )

 
# keep consensus 
dates_good<-filter(basic_dropdown, review >= 2 | review2 >= 2) %>%
  mutate(date= case_when(
    is.na(review2) ~ date_hash, # if ~ then 
    review2 > review ~ date_hash2, # if ~ then 
    review > review2 ~ date_hash # if ~ then 
  )) %>%
  select(subject_id, column_name, date, url_front, url_back)%>% 
  pivot_wider(  id_cols = c(subject_id, url_front, url_back),
                names_from = column_name, 
                values_from = c(date), 
                values_fill = list(date = NA) )

#want to join and replace NAs from one table with data from another if there, need rqdatatable package
dates_clean<-natural_join(dates_good, dates_bad, by  = 'subject_id', jointype = "FULL") %>% 
  mutate(end_date_month = ifelse(end_date_month == "same", begin_date_month, end_date_month), #change same to the actual month or year
    end_date_year = ifelse(end_date_year == "same", begin_date_year, end_date_year), 
    begin_date_year =ifelse(subject_id == 82940993, 1984, begin_date_year), #change the "none" values 
    end_date_year =ifelse(subject_id == 82940993, 1984, end_date_year), 
    begin_date_year =ifelse(subject_id == 82940982, 1953, begin_date_year), #change the "none" values 
    end_date_year =ifelse(subject_id == 82940982, 1953, end_date_year), 
    end_date_year =ifelse(subject_id == 82940954, 1982, end_date_year)
  ) 

#QA/QC:check begin and end years that don't match - none! 
#dates_clean$review<-ifelse(dates_clean$begin_date_year != dates_clean$end_date_year, TRUE, FALSE)
#re-order columns 
dates_clean <- dates_clean[, c(1,2,3,4,5,6,7,9,8)]

write.csv(dates_clean, 'Lake_SUMM/SUMM_data/dec_data/clean_dates_dec.csv', row.names = FALSE)
