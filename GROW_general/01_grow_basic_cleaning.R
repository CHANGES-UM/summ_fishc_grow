### GROW CARDS BASIC CLEANING DATES ###
## written by Katelyn King 

#### load libraries #### 
library(dplyr)
library(tidyr)
library(stringr)
library(rqdatatable)


#### load data #### 

#read in urls 
urls<-read.csv("urls/basic_GROW_URLs.csv", na.strings = c("", "NA"))  %>% 
  distinct(subject_id, .keep_all = TRUE) %>%
  rename(url_front = URL_front, url_back = URL_back) 

####basic_dropdown (dates) cleaning #### 
#read in file and remove unwanted columns and NAs
basic_dropdown<-read.csv("GROW_BLG/BLG_data/dropdown_reducer_basic_GROW_dropdowns.csv", na.strings = c("", "NA")) %>%
  select(subject_id, task, 'data.value') %>%
  drop_na('data.value') %>% 
  rename(data_value = 'data.value') 
#read in files with values to match (e.g. the date hash and task names)
basic_task_values<-read.csv("GROW_BLG/BLG_data/Basic_task_value.csv", header=TRUE) %>%
  select(task, column_name)
basic_date_values<-read.csv("GROW_BLG/BLG_data/Basic_date_values.csv", header=TRUE)

basic_dropdown$sep <- gsub("[[:punct:]]", "", basic_dropdown$data_value)  #remove punctuation 
basic_dropdown$sep<-gsub("\\s", "_", basic_dropdown$sep) #replace space with _
basic_dropdown<- separate(data=basic_dropdown, col=sep, c("date_hash", "review", "date_hash2", "review2", "date_hash3", "review3", "date_hash4", "review4" ), sep = "_") %>%
  left_join(basic_task_values) %>% #get actual column name 
  left_join(basic_date_values, by=c('date_hash' = 'code')) %>% #get date values for the first review
  left_join(basic_date_values, by=c('date_hash2' = 'code'))  %>% #get date values for the 2nd review
  left_join(basic_date_values, by=c('date_hash3' = 'code')) %>% #get date values for the 3rd review
  left_join(basic_date_values, by=c('date_hash4' = 'code')) %>% #get date values for the 4th review
  rename(month1=month.x, month2=month.y, month3=month.x.x, month4=month.y.y) %>%
  mutate(month1 = ifelse(is.na(month1), date_hash, month1), #if month is NA then replace with the day/year, else keep month 
         month2 = ifelse(is.na(month2), date_hash2, month2),
         month3 = ifelse(is.na(month3), date_hash3, month3),
         month4 = ifelse(is.na(month4), date_hash4, month4)
  )


#filter out the ones that didn't make it (review is split 50:50 or less)
dates_bad<-filter(basic_dropdown, review < 3 & !is.na(review2) & review2 < 3) %>%
  select(-c(task, data_value, date_hash, date_hash2, date_hash3, date_hash4)) %>%
  mutate(date= case_when( #but if there is a split because choosing 'same as' then don't need to review these 
    (month2 == 'Same as Begin Date (Month)' | month2 == 'Same as Begin Date (Day)' | month2 == 'Same as Begin Date (Year)') & is.na(month3)   ~ month1, # if ~ then 
    (month1 == 'Same as Begin Date (Month)' | month1 == 'Same as Begin Date (Day)' | month1 == 'Same as Begin Date (Year)') & is.na(month3)   ~ month2)
    #else be NA if its not either of these conditions 
  )

#* observations for manual review by Kartik ####
#dates_still_na<-filter(dates_bad, is.na(date))  %>% #714 obs 
#left_join(urls )
#write.csv(dates_still_na, "/Users/katelynking/Desktop/dates_for_review.csv", row.names = FALSE)

#pivot dates to good format
dates_new<- pivot_wider(data = dates_bad, 
                        id_cols = c(subject_id),
                        names_from = column_name, 
                        values_from = c(date), 
                        values_fill = list(date = NA) )


# keep 4s and 3s OR any that were 3,2,1 but don't have disagreements (NAs) 
dates_good<-filter(basic_dropdown, review >= 3 | is.na(review2) | review2 >= 3) %>%
  mutate(date= case_when(
    is.na(review2) ~ month1, # if ~ then 
    review2 > review ~ month2, # if ~ then 
    review > review2 ~ month1 # if ~ then 
  )) %>%
  select(subject_id, column_name, date) %>% 
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(date), 
              values_fill = list(date = NA) ) %>%
  natural_join(dates_new, by  = 'subject_id', jointype = "FULL") %>% #join and replace NAs from one table with data from another if there, need rqdatatable package
  mutate(end_date_day = ifelse(end_date_day =='Same as Begin Date (Day)', begin_date_day, end_date_day), #change "same as..." to the actual date 
         end_date_month =ifelse(end_date_month =='Same as Begin Date (Month)', begin_date_month, end_date_month),
         end_date_year = ifelse(end_date_year =='Same as Begin Date (Year)', begin_date_year, end_date_year) 
  )

#merging back in manually reviewed data 
manual_dates<-read.csv("GROW_BLG/BLG_data/manual_review_data/dates_manual_Kartik.csv", na.strings = c("", "NA")) %>%
  select(subject_id, column_name, date) %>% 
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(date), 
              values_fill = list(date = NA) )

dates_clean_plus_manual<-natural_join(dates_good, manual_dates, by  = 'subject_id', jointype = "FULL")


#QA/QC:
#* check older dates 
dates_clean_plus_manual<-dates_clean_plus_manual %>% 
  mutate(begin_date_year = ifelse(subject_id == 58642717, NA, begin_date_year), 
         begin_date_year = ifelse(subject_id == 58754950, 1988, begin_date_year), 
         begin_date_year = ifelse(subject_id == 58746428, 1980, begin_date_year)
  )

#* check begin and end years that don't match n=351 - review manually 
dates_clean_plus_manual$review<-ifelse(dates_clean_plus_manual$begin_date_year != dates_clean_plus_manual$end_date_year, "TRUE", "FALSE") 
#dates_review<-  filter(dates_clean_plus_manual, review == 'TRUE') %>% 
#              left_join(urls) %>% 
#             drop_na('URL_front')
#write.csv(dates_review, "/Users/katelynking/Desktop/grow_dates_review.csv", row.names = FALSE)

grow_dates_review<-read.csv("GROW_BLG/BLG_data/manual_review_data/grow_dates_review_cb.csv")%>% 
  select(-c("review")) %>%
  rename(url_front = URL_front, url_back = URL_back) 




dates_final<-filter(dates_clean_plus_manual, review == 'FALSE' | is.na(review)) %>% 
  left_join(urls) %>% 
  drop_na('url_front') %>% 
  select(-c("review")) %>%
  mutate (comment = NA)%>%
  rbind(grow_dates_review)


##### add in Dec 2022 Zooniverse basic data (SS6) ####
new_urls<-read.csv("GROW_BLG/BLG_data/SS6_Basic_GROW_Dropdowns.csv", na.strings = c("", "NA")) %>%
  select(subject_id, 'URL.4', 'URL.8' ) %>% 
  rename(url_front=URL.4, url_back=URL.8) %>% 
  distinct(subject_id, .keep_all = TRUE)

basic_dropdown2<-read.csv("GROW_BLG/BLG_data/SS6_Basic_GROW_Dropdowns.csv", na.strings = c("", "NA")) %>%
  select(subject_id, task, 'data.value' ) %>%
  drop_na('data.value') %>% 
  rename(data_value = 'data.value') %>% 
  left_join(basic_task_values) %>% #get actual column name 
  separate(col=data_value, c("review","review2", "review3", "review4"), sep = ",") %>%  #separate different answers by the comma 
  separate(col=review, c("date", "review"), sep = ":") %>%  #separate date response and reviewer number by the :  
  separate(col=review2, c("date2", "review2"), sep = ":") %>% 
  separate(col=review3, c("date3", "review3"), sep = ":") %>% 
  separate(col=review4, c("date4", "review4"), sep = ":")  %>% 
  mutate(review = as.numeric(gsub("[[:punct:]]", "", review)), 
         date = gsub("[[:punct:]]", "", date), 
         review2 = as.numeric(gsub("[[:punct:]]", "", review2)), 
         date2 = gsub("[[:punct:]]", "", date2), 
         review3 = as.numeric(gsub("[[:punct:]]", "", review3)),
         date3 = gsub("[[:punct:]]", "", date3), 
         review4 = as.numeric(gsub("[[:punct:]]", "", review4)), 
         date4 = gsub("[[:punct:]]", "", date4) ) 

# keep >= OR any that were 3,2,1 but don't have disagreements (NAs) 
dates_good2<-filter(basic_dropdown2,  (review >= 2 | review2 >= 2) ) %>%
  mutate(date_new= case_when(
    is.na(review2) ~ date, # if ~ then 
    review2 > review ~ date2, # if ~ then 
    review > review2 ~ date # if ~ then 
  )) %>%
  select(subject_id, column_name, date_new)%>% #basic date format 
  mutate(date_new = str_trim(date_new, side=c("both"))) %>% #remove white space before or after answers 
  naniar::replace_with_na_all(condition = ~.x == 'None')   %>% #function that replaces all of a value with NA, in this case 'None' 
  tidyr::pivot_wider( id_cols = c(subject_id),
                      names_from = column_name, 
                      values_from = c(date_new), 
                      values_fill = list(date_new = NA) )  %>% 
  mutate(end_date_day = ifelse(end_date_day =='Same as Begin Date Day', begin_date_day, end_date_day), #change "same as..." to the actual date 
         end_date_month =ifelse(end_date_month =='Same as Begin Date Month', begin_date_month, end_date_month),
         end_date_year = ifelse(end_date_year =='Same as Begin Date Year', begin_date_year, end_date_year) 
  ) %>% 
  left_join(new_urls) %>%
  drop_na('url_front')  # drop cards that don't have url 

##filter out the ones that are split 
dates_bad2<-filter(basic_dropdown2, (review < 2 & is.na(review2)) | (review < 2 & review2 < 2)) %>%
  left_join(new_urls)

#write.csv(dates_bad2, "/Users/katelynking/Desktop/SS6_grow_dates_review.csv", row.names = FALSE)
dates_review<-read.csv("GROW_BLG/BLG_data/manual_review_data/SS6_grow_dates_review.csv") %>% 
  select(subject_id, column_name, date_new) %>% 
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(date_new), 
              values_fill = list(date_new = NA) ) %>%
  left_join(new_urls)

#want to join and replace NAs from one table with data from another if there, need rqdatatable package
dates_clean2<-natural_join(dates_good2, dates_review, by  = 'subject_id', jointype = "FULL")  %>%
  mutate (comment = NA) # add a comment column to match previous data

#reorder 
dates_clean2 <- dates_clean2[, c(1,2,3,4,5,6,7,9,8,10)]

#QA/QC ##
#check begin and end years that don't match 
#dates_clean2$review<-ifelse(dates_clean2$begin_date_year != dates_clean2$end_date_year, TRUE, FALSE)
dates_clean2$begin_date_year<-ifelse(dates_clean2$subject_id == 82937331, 1980, dates_clean2$begin_date_year)

#check cards without begin year
dates_clean2$begin_date_year<-ifelse(dates_clean2$subject_id == 82937334, 1976, dates_clean2$begin_date_year)
dates_clean2$begin_date_year<-ifelse(dates_clean2$subject_id == 82939722, 1993, dates_clean2$begin_date_year)
dates_clean2$begin_date_year<-ifelse(dates_clean2$subject_id == 82939723, 1992, dates_clean2$begin_date_year)
dates_clean2$begin_date_year<-ifelse(dates_clean2$subject_id == 82939727, 1991, dates_clean2$begin_date_year)
dates_clean2$begin_date_month<-ifelse(dates_clean2$subject_id == 82939727, 'May', dates_clean2$begin_date_month)

#*join early and later datasets #### 
grow_dates<-rbind(dates_final, dates_clean2)

#write dates to be used for all grow cards 
write.csv(grow_dates, 'GROW_general/grow_dates.csv', row.names = FALSE)
