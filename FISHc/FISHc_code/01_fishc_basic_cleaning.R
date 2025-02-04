### FISH Collection (FISHc) basic data cleaning ###
## written by Katelyn King 
### basic information  

#note that more basic cards were run in Oct and Dec of 2022, so 3 dropdowns are needed 

#### load libraries #### 
library(dplyr)
library(tidyr)
library(stringr)
library(rqdatatable)
library(naniar)

urls<-read.csv("urls/new_urls.csv") #have the bad urls removed already 

#subjects that were removed from workflow should be removed from dataset
bad_urls<-read.csv("urls/oldbfirst_subjects_to_remove.csv") 

#read in files with tasks to match (these tasks are the same across basic)
basic_task_values<-read.csv("GROW_BLG/BLG_data/Basic_task_value.csv", header=TRUE) %>%
  select(task, column_name) 

#### BASIC FISHc dropdowns #### 
#note basic text was used for lake matching
#dropdown_reducer_basic_FISHc - this is ALL updated by Justin - note Justin also replaced hash values already 
basic_dropdown<-read.csv("FISHc/FISHc_data/basic_data/basic_FISHc_dropdowns.csv", na.strings = c("", "NA")) %>%
  select(subject_id, task, 'data.value') %>%
  drop_na('data.value') %>% 
  rename(data_value = 'data.value')  %>% 
  anti_join(bad_urls) %>% #remove bad subjects 
  mutate(sep = gsub("[[:punct:]]", "", data_value), #remove punctuation 
         sep=gsub("\\s", "_", sep) #replace space with _
         ) %>% 
  separate(col=sep, c("date", "review", "date2", "review2", "date3", "review3", "date4", "review4" ), sep = "_") %>%
  left_join(basic_task_values) #get actual column name 

# keep >= OR any that were 3,2,1 but don't have disagreements (NAs) 
dates_good<-filter(basic_dropdown, review >= 2 | is.na(review2) | review2 >= 2) %>%
  mutate(date_new= case_when(
    is.na(review2) ~ date, # if ~ then 
    review2 > review ~ date2, # if ~ then 
    review > review2 ~ date # if ~ then 
  )) %>%
  select(subject_id, column_name, date_new)%>% #basic date format 
  tidyr::pivot_wider( id_cols = c(subject_id),
                      names_from = column_name, 
                      values_from = c(date_new), 
                      values_fill = list(date_new = NA) ) %>% 
  naniar::replace_with_na_all(condition = ~.x == 'None') %>% #function that replaces all of a value with NA, in this case 'None' 
  left_join(urls) %>%
  drop_na('front') # drop cards that don't have url 

#* observations for manual review ####
##filter out the ones that are split 
#dates_bad<-filter(basic_dropdown, review < 2 & !is.na(review2) & review2 < 2) %>%
# select(-c(task, data_value)) %>%
#left_join(urls)
#write.csv(dates_bad, "/Users/katelynking/Desktop/dates_review_fishc.csv", row.names = FALSE) #Kartik 

#read in manual dates and join 
manual_dates<-read.csv("Fishc/FISHc_data/manual_reviews/dates_review_fishc_kt.csv") %>%
  anti_join(bad_urls) %>% #remove bad subjects 
  tidyr::pivot_wider( id_cols = c(subject_id),
                      names_from = column_name, 
                      values_from = c(date.1), 
                      values_fill = list(date = NA) )

#want to join and replace NAs from one table with data from another if there, need rqdatatable package
dates_clean<-natural_join(dates_good, manual_dates, by  = 'subject_id', jointype = "FULL")

#reorder 
dates_clean <- dates_clean[, c(1,3,4,5,6,7,8,9,2)]

#*QA/QC:check begin and end years that don't match ####
#dates_clean$review<-ifelse(dates_clean$begin_date_year != dates_clean$end_date_year, TRUE, FALSE)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 58642797, NA, dates_clean$end_date_year)
dates_clean$end_date_day<-ifelse(dates_clean$subject_id == 58642797, NA, dates_clean$end_date_day)
dates_clean$end_date_month<-ifelse(dates_clean$subject_id == 58642797, NA, dates_clean$end_date_month)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 58642806, NA, dates_clean$end_date_year)
dates_clean$end_date_day<-ifelse(dates_clean$subject_id == 58642806, NA, dates_clean$end_date_day)
dates_clean$end_date_month<-ifelse(dates_clean$subject_id == 58642806, NA, dates_clean$end_date_month)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 58749740, 1969, dates_clean$end_date_year)
dates_clean$begin_date_year<-ifelse(dates_clean$subject_id == 58749895, 1941, dates_clean$begin_date_year)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 58750049, 1967, dates_clean$end_date_year)
dates_clean$end_date_day<-ifelse(dates_clean$subject_id == 58750049, 13, dates_clean$end_date_day)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 58750208, 1951, dates_clean$end_date_year)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 58750438, 1962, dates_clean$end_date_year)
dates_clean$begin_date_year<-ifelse(dates_clean$subject_id == 58750475, 1947, dates_clean$begin_date_year)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 58750589, 1960, dates_clean$end_date_year)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 58750740, 1960, dates_clean$end_date_year)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 58750741, 1960, dates_clean$end_date_year)
dates_clean$begin_date_day<-ifelse(dates_clean$subject_id == 58750741, 15, dates_clean$begin_date_day)
dates_clean$begin_date_month<-ifelse(dates_clean$subject_id == 58750741, 'May', dates_clean$begin_date_month)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 58750786, 1960, dates_clean$end_date_year)
dates_clean$begin_date_year<-ifelse(dates_clean$subject_id == 58750909, 1959, dates_clean$begin_date_year)
dates_clean$begin_date_day<-ifelse(dates_clean$subject_id == 58750909, 13, dates_clean$begin_date_day)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 58751219, NA, dates_clean$end_date_year)
dates_clean$end_date_day<-ifelse(dates_clean$subject_id == 58751219, NA, dates_clean$end_date_day)
dates_clean$end_date_month<-ifelse(dates_clean$subject_id == 58751219, NA, dates_clean$end_date_month)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 58752461, 1947, dates_clean$end_date_year)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 58752567, 1940, dates_clean$end_date_year)
dates_clean$begin_date_day<-ifelse(dates_clean$subject_id == 58752567, 13, dates_clean$begin_date_day)
dates_clean$begin_date_year<-ifelse(dates_clean$subject_id == 58753190, 1974, dates_clean$begin_date_year)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 58755207, NA, dates_clean$end_date_year)
dates_clean$end_date_day<-ifelse(dates_clean$subject_id == 58755207, NA, dates_clean$end_date_day)
dates_clean$end_date_month<-ifelse(dates_clean$subject_id == 58755207, NA, dates_clean$end_date_month)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 58755214, NA, dates_clean$end_date_year)
dates_clean$end_date_day<-ifelse(dates_clean$subject_id == 58755214, NA, dates_clean$end_date_day)
dates_clean$end_date_month<-ifelse(dates_clean$subject_id == 58755214, NA, dates_clean$end_date_month)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 58755230, NA, dates_clean$end_date_year)
dates_clean$end_date_day<-ifelse(dates_clean$subject_id == 58755230, NA, dates_clean$end_date_day)
dates_clean$end_date_month<-ifelse(dates_clean$subject_id == 58755230, NA, dates_clean$end_date_month)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 59964066, 1963, dates_clean$end_date_year)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 59965833, 1946, dates_clean$end_date_year)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 59965987, 1957, dates_clean$end_date_year)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 59966026, 1940, dates_clean$end_date_year)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 59967764, 1951, dates_clean$end_date_year)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 59968164, 1954, dates_clean$end_date_year)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 59968748, 1969, dates_clean$end_date_year)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 59968995, 1962, dates_clean$end_date_year)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 59969194, 1955, dates_clean$end_date_year)
dates_clean$end_date_month<-ifelse(dates_clean$subject_id == 59969194, 'June', dates_clean$end_date_month)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 61361186, 1957, dates_clean$end_date_year) #note that card says 1967 but time says 24 hrs
dates_clean$end_date_day<-ifelse(dates_clean$subject_id == 61361186, 7, dates_clean$end_date_day) #note that card says 6 but time says 24 hrs
dates_clean$end_date_month<-ifelse(dates_clean$subject_id == 58750399, 'September', dates_clean$end_date_month)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 58750399, 1956, dates_clean$end_date_year)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 58751462, 1956, dates_clean$end_date_year)
dates_clean$begin_date_year<-ifelse(dates_clean$subject_id == 59965494, 1956, dates_clean$begin_date_year)
dates_clean$begin_date_year<-ifelse(dates_clean$subject_id == 59965500, 1956, dates_clean$begin_date_year)
#check again after changes 
#dates_clean$review2<-ifelse(dates_clean$begin_date_year != dates_clean$end_date_year, TRUE, FALSE)

#other odd date check (early dates )
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 58750953, 1960, dates_clean$end_date_year)
dates_clean$begin_date_year<-ifelse(dates_clean$subject_id == 58750953, 1960, dates_clean$begin_date_year)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 58751233, 1962, dates_clean$end_date_year)
dates_clean$begin_date_year<-ifelse(dates_clean$subject_id == 58751233, 1962, dates_clean$begin_date_year)
dates_clean$begin_date_day<-ifelse(dates_clean$subject_id == 58749800, 26, dates_clean$begin_date_day) 
dates_clean$begin_date_month<-ifelse(dates_clean$subject_id == 58749800, 'July', dates_clean$begin_date_month)
dates_clean$begin_date_year<-ifelse(dates_clean$subject_id == 58749800, 1972, dates_clean$begin_date_year)
dates_clean$end_date_day<-ifelse(dates_clean$subject_id == 58749800, 27, dates_clean$end_date_day) 
dates_clean$end_date_month<-ifelse(dates_clean$subject_id == 58749800, 'July', dates_clean$end_date_month)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 58749800, 1972, dates_clean$end_date_year)
dates_clean$begin_date_year<-ifelse(dates_clean$subject_id == 58751297, 1956, dates_clean$begin_date_year)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 58751297, 1956, dates_clean$end_date_year)
dates_clean$end_date_month<-ifelse(dates_clean$subject_id == 58751455, 'July', dates_clean$end_date_month)
dates_clean$end_date_year<-ifelse(dates_clean$subject_id == 58751455, 1956, dates_clean$end_date_year)
dates_clean$begin_date_year<-ifelse(dates_clean$subject_id == 58751455, 1956, dates_clean$begin_date_year)
dates_clean$begin_date_year<-ifelse(dates_clean$subject_id == 58748990, 1959, dates_clean$begin_date_year)

#if the end date is filled in but not the beginning, put end dates into beginning 
dates_clean<-dates_clean%>%
  mutate(begin_date_year = ifelse(is.na(begin_date_year), end_date_year, begin_date_year), 
         begin_date_month= ifelse(is.na(begin_date_month), end_date_month, begin_date_month), 
         begin_date_day= ifelse(is.na(begin_date_day), end_date_day, begin_date_day)) 

#double check ones that have no year
#no_year<-filter(dates_clean, is.na(begin_date_year))
#write.csv(no_year, "/Users/katelynking/Desktop/fishc_noyear_review.csv", row.names = FALSE)
no_year<-read.csv("FISHc/FISHc_data/manual_reviews/fishc_noyear_review_cb.csv") %>%
  select(-c(notes))

dates_clean<-natural_join(dates_clean, no_year, by  = 'subject_id', jointype = "FULL")

#*read in lake match data from Michael's work #### 
lake_match <-read.csv("FISHc/FISHc_data/FISHc_lake_match_31may22.csv", header=TRUE, na.strings = c("", "NA")) %>%
  select(subject_id, county, lakename, Progress) %>%
  rename(new_key = Progress) %>%
  drop_na('new_key')

fishc_basic_dates<-left_join(dates_clean,lake_match)

#reorder 
fishc_basic_dates <- fishc_basic_dates[, c(1,12,11,10,3,4,5,6,7,8,9,2)]
#write.csv(fishc_basic_dates, "FISHc/FISHc_data/final_data/fishc_basic_dates.csv", row.names = FALSE)

#### add JUNE DATA ####
basic_dropdown2<-read.csv("FISHc/FISHc_data/basic_data/basic_FISHc_dropdowns_June22.csv", na.strings = c("", "NA")) %>%
  select(subject_id, task, 'data.value') %>%
  drop_na('data.value') %>% 
  rename(data_value = 'data.value')  %>% 
  anti_join(bad_urls) %>% #remove bad subjects 
  mutate(data_value = gsub("Same as Begin Date ", "same", data_value), #change these to same so there are not spaces
        sep = gsub("[[:punct:]]", "", data_value), #remove punctuation 
         sep=gsub("\\s", "_", sep) #replace space with _
  ) %>% 
  separate(col=sep, c("date", "review", "date2", "review2", "date3", "review3", "date4", "review4" ), sep = "_") %>%
  left_join(basic_task_values) %>%  #get actual column name 
  mutate(date = ifelse(column_name == "end_date_year" & date == "None", "sameYear", date), 
         date2 = ifelse(column_name == "end_date_year" & date2 == "None", "sameYear", date2), 
         date3 = ifelse(column_name == "end_date_year" & date3 == "None", "sameYear", date3)) #want to treat none the same as 'same year' as these are agreements 

# keep >= 2 OR any that were 3,2 but don't have disagreements (NAs) 
dates_good<-filter(basic_dropdown2, review >= 2 | review2 >= 2) %>%
  mutate(date_new= case_when(
    is.na(review2) ~ date, # if ~ then 
    review > review2 ~ date, 
    review2 > review ~ date2, 
    review2 == review ~ date 
  )) %>%
  select(subject_id, column_name, date_new)%>% #basic date format 
  tidyr::pivot_wider( id_cols = c(subject_id),
                      names_from = column_name, 
                      values_from = c(date_new), 
                      values_fill = list(date_new = NA) ) %>% 
  naniar::replace_with_na_all(condition = ~.x == 'None') %>% #function that replaces all of a value with NA, in this case 'None' 
  left_join(urls) %>%
  drop_na('front') # drop cards that don't have url 

ties<-filter(basic_dropdown2, review == 2 & review2 == 2) %>% 
  mutate(date_new= case_when( 
  (date2 == 'sameMonth' | date2 == 'sameDay' | date2 == 'sameYear') & is.na(date3) ~ date,
  (date == 'sameMonth' | date == 'sameDay' | date == 'sameYear') & is.na(date3)   ~ date2,
 )  #else be NA if its not either of these conditions 
) %>% 
  left_join(urls) %>% #check ones that are still NA 
  mutate(date_new = ifelse(subject_id == '58750578', 25, date_new), 
         date_new = ifelse(subject_id == '58750596', 23, date_new), 
         date_new = ifelse(subject_id == '58750748', 24, date_new), 
         date_new = ifelse(subject_id == '58750797', 23, date_new), 
         date_new = ifelse(subject_id == '58751280', 6, date_new), 
         date_new = ifelse(subject_id == '58751464', 1956, date_new),
         date_new = ifelse(subject_id == '58751464', 1956, date_new),
         date_new = ifelse(subject_id == '58753155', 6, date_new),
         date_new = ifelse(subject_id == '58753219', 30, date_new)
  ) %>% 
  select(subject_id, column_name, date_new, front, back)

#filter out the ones that didn't make it (review is split 50:50 or less), but keep "same as" answers 
dates_bad<-filter(basic_dropdown2, (review < 2 & review2 < 2) | review <2 & is.na(review2) ) %>%
  naniar::replace_with_na_all(condition = ~.x == 'None') %>%
  filter(if_any(c("date", "date2", "date3", "date4"), complete.cases)) %>% #after removing none there are some that become all NA 
  mutate(date_new= case_when( 
    (date2 == 'sameMonth' | date2 == 'sameDay' | date2 == 'sameYear') & is.na(date3) ~ date,
    (date == 'sameMonth' | date == 'sameDay' | date == 'sameYear') & is.na(date3)   ~ date2,
    date3 == 'sameMonth' & date== 'sameMonth' ~ date2, 
    date3 == 'sameYear' & date== 'sameYear' ~ date2,
    date3 == 'sameMonth' & date2== 'sameMonth' ~ date, 
    date3 == 'sameYear' & date2== 'sameYear' ~ date )  #else be NA if its not either of these conditions 
  ) %>% 
  mutate(date = ifelse((is.na(date_new) & date == "sameYear"), NA, date) ) %>%
  filter(if_any(c("date", "date2", "date3", "date4"), complete.cases)) %>%  #remove all NA 
  mutate(column_name = ifelse(is.na(date_new) & 
                                 (column_name == "end_date_year" | column_name == "end_date_day" | column_name == "end_date_month"), NA, column_name) ) %>% 
    #if it is an end date then remove because people had the option not to put an end date if it was the same
  drop_na('column_name')

dates_good2<-filter(dates_bad, !is.na(date_new)) %>% 
  left_join(urls)%>% 
  select(subject_id, column_name, date_new, front, back)
  
#anything that is still NA in date_new can be checked 
dates_review<-filter(dates_bad, is.na(date_new)) %>% 
  left_join(urls) %>% 
  mutate(date_new = ifelse(subject_id == '58642493', 19, date_new),
         date_new = ifelse(subject_id == '58748631', 1959, date_new),
         date_new = ifelse(subject_id == '58748793', 1957, date_new),
         date_new = ifelse(subject_id == '58748807', 'June', date_new),
         date_new = ifelse(subject_id == '58748887' & task =='T0', 'June', date_new),
         date_new = ifelse(subject_id == '58748887' & task =='T1', '30', date_new),
         date_new = ifelse(subject_id == '58748889', 1955, date_new),
         date_new = ifelse(subject_id == '58749318', 14, date_new),
         date_new = ifelse(subject_id == '58749606' & task =='T0', 'June', date_new),
         date_new = ifelse(subject_id == '58749606' & task =='T1', '17', date_new),
         date_new = ifelse(subject_id == '58749606' & task =='T2', '1968', date_new),
         date_new = ifelse(subject_id == '58749787', 1972, date_new),
         date_new = ifelse(subject_id == '58750014', 1, date_new),
         date_new = ifelse(subject_id == '58750333', 27, date_new),
         date_new = ifelse(subject_id == '58750423', 27, date_new),
          date_new = ifelse(subject_id == '58749787', 1972, date_new),
         date_new = ifelse(subject_id == '58750457' & task =='T0', 'August', date_new),
         date_new = ifelse(subject_id == '58750457' & task =='T1', '3', date_new),
         date_new = ifelse(subject_id == '58750581', 28, date_new),
         date_new = ifelse(subject_id == '58750683' & task =='T0', 'August', date_new),
         date_new = ifelse(subject_id == '58750683' & task =='T1', '9', date_new),
         date_new = ifelse(subject_id == '58750683' & task =='T2', '1956', date_new),
         date_new = ifelse(subject_id == '58750781', 17, date_new),
         date_new = ifelse(subject_id == '58750811', 27, date_new),
         date_new = ifelse(subject_id == '58750815', 17, date_new),
         date_new = ifelse(subject_id == '58750821', 12, date_new),
         date_new = ifelse(subject_id == '58750825', 'August', date_new),
         date_new = ifelse(subject_id == '58750857', 1956, date_new),
         date_new = ifelse(subject_id == '58750919', 14, date_new),
         date_new = ifelse(subject_id == '58751110', 1950, date_new),
         date_new = ifelse(subject_id == '58751130', 25, date_new),
         date_new = ifelse(subject_id == '58751218', 14, date_new),
         date_new = ifelse(subject_id == '58751241', 1, date_new),
         date_new = ifelse(subject_id == '58751295', 1956, date_new),
         date_new = ifelse(subject_id == '58751309'& task =='T1', 26 , date_new),
         date_new = ifelse(subject_id == '58751309'& task =='T2', 1956, date_new),
         date_new = ifelse(subject_id == '58751420', 27, date_new),
         date_new = ifelse(subject_id == '58751477', 24, date_new),
         date_new = ifelse(subject_id == '58751618', 1949, date_new),
         date_new = ifelse(subject_id == '58751845', 1954, date_new),
         date_new = ifelse(subject_id == '58751863', 1937, date_new),
         date_new = ifelse(subject_id == '58752006', 'September', date_new),
         date_new = ifelse(subject_id == '58752394', 7, date_new),
         date_new = ifelse(subject_id == '58752539', 18, date_new),
         date_new = ifelse(subject_id == '58751309'& task =='T0', 'July' , date_new),
         date_new = ifelse(subject_id == '58751309'& task =='T1', 19, date_new),
         date_new = ifelse(subject_id == '58752564', 12, date_new),
         date_new = ifelse(subject_id == '58753151', 2, date_new),
         date_new = ifelse(subject_id == '58753429', 1953, date_new),
         date_new = ifelse(subject_id == '58753438', 1953, date_new)
  ) %>% 
  drop_na(date_new) %>%  #everything I left blank was not correct and can be removed 
  select(subject_id, column_name, date_new, front, back)


dates_updated<-rbind(ties, dates_good2, dates_review) %>% 
  tidyr::pivot_wider( id_cols = c(subject_id, front, back),
                      names_from = column_name, 
                      values_from = c(date_new), 
                      values_fill = list(date_new = NA) ) 

clean_dates2<-natural_join(dates_good, dates_updated, by  = 'subject_id', jointype = "FULL") %>% 
  mutate(end_date_year= ifelse(end_date_year == 'sameYear', begin_date_year, end_date_year ),
         end_date_month= ifelse(end_date_month == 'sameMonth', begin_date_month, end_date_month ), 
         end_date_day= ifelse(end_date_day == 'sameDay', begin_date_day, end_date_day ) ) %>% 
  #if the end date is filled in but not the beginning, put end dates into beginning 
  mutate(begin_date_year = ifelse(is.na(begin_date_year), end_date_year, begin_date_year), 
         begin_date_month= ifelse(is.na(begin_date_month), end_date_month, begin_date_month), 
         begin_date_day= ifelse(is.na(begin_date_day), end_date_day, begin_date_day)) 

#QA/QC:check begin and end years that don't match, check no years
clean_dates2<-clean_dates2%>%
  mutate(check = ifelse(begin_date_year != end_date_year, TRUE, FALSE)) %>% 
  mutate(end_date_year = ifelse(check == TRUE, begin_date_year, end_date_year), #checked all of these should be the same as begin date
        begin_date_year = ifelse(subject_id == 58751455, 1956, begin_date_year), #except this one 
        end_date_year = ifelse(subject_id == 58751455, 1956, end_date_year), 
        begin_date_year = ifelse(subject_id == 58750909, 1959, begin_date_year),
        begin_date_year = ifelse(subject_id == 58750558, 1940, begin_date_year), 
        )%>% 
  select(-c(check)) %>% 
  left_join(lake_match) #add new_keys

  
#### add DEC DATA #### 
basic_dropdown3<-read.csv("FISHc/FISHc_data/basic_data/SS6_basic_FISHc_Dropdowns.csv", na.strings = c("", "NA")) %>% 
  select(subject_id, task, 'data.value') %>%
  drop_na('data.value') %>% 
  rename(data_value = 'data.value') %>%
  anti_join(bad_urls) %>% #remove bad subjects 
  mutate(data_value = gsub("Same as Begin Date ", "same", data_value), #change these to same so there are not spaces
         sep = gsub("[[:punct:]]", "", data_value), #remove punctuation 
         sep=gsub("\\s", "_", sep) #replace space with _
  ) %>% 
  separate(col=sep, c("date", "review", "date2", "review2", "date3", "review3", "date4", "review4" ), sep = "_") %>%
  left_join(basic_task_values) %>%  #get actual column name 
  mutate(date = ifelse(column_name == "end_date_year" & date == "None", "sameYear", date), 
         date2 = ifelse(column_name == "end_date_year" & date2 == "None", "sameYear", date2), 
         date3 = ifelse(column_name == "end_date_year" & date3 == "None", "sameYear", date3)) #want to treat none the same as 'same year' as these are agreements 

dec_urls<-read.csv("FISHc/FISHc_data/basic_data/SS6_basic_FISHc_Dropdowns.csv", na.strings = c("", "NA")) %>%
  select(subject_id, 'URL.4', 'URL.8') %>% 
  rename(front= 'URL.4', back= 'URL.8') %>% 
  distinct(subject_id, .keep_all = TRUE)

# keep >= 2 OR any that were 3,2 but don't have disagreements (NAs) 
clean_dates3<-filter(basic_dropdown3, review >= 2 | review2 >= 2) %>%
  mutate(date_new= case_when(
    is.na(review2) ~ date, # if ~ then 
    review > review2 ~ date, 
    review2 > review ~ date2, 
    review2 == review ~ date #ties? 
  )) %>%
  select(subject_id, column_name, date_new)%>% #basic date format 
  tidyr::pivot_wider( id_cols = c(subject_id),
                      names_from = column_name, 
                      values_from = c(date_new), 
                      values_fill = list(date_new = NA) ) %>% 
  naniar::replace_with_na_all(condition = ~.x == 'None') %>%  #function that replaces all of a value with NA, in this case 'None' 
  left_join(dec_urls) %>% 
  mutate(begin_date_day = ifelse(subject_id == 82937372, 29,begin_date_day), 
         end_date_day = ifelse(subject_id == 82937372, 30, end_date_day), # this should be July 29 to July 30 
         end_date_month = ifelse(subject_id == 82937372, 'July', end_date_month),
         )  %>% 
  mutate(end_date_year= ifelse(end_date_year == 'sameYear', begin_date_year, end_date_year ),
         end_date_month= ifelse(end_date_month == 'sameMonth', begin_date_month, end_date_month ), 
         end_date_day= ifelse(end_date_day == 'sameDay', begin_date_day, end_date_day ) ) %>% 
  #if the end date is filled in but not the beginning, put end dates into beginning 
  mutate(begin_date_year = ifelse(is.na(begin_date_year), end_date_year, begin_date_year), 
         begin_date_month= ifelse(is.na(begin_date_month), end_date_month, begin_date_month), 
         begin_date_day= ifelse(is.na(begin_date_day), end_date_day, begin_date_day))%>% 
  left_join(lake_match) #add lake match 
         

#filter out the ones that didn't make it (review is split 50:50 or less)
#these are all end dates that are the same as begin date, so not needed 
dates_bad3<-filter(basic_dropdown3, (review < 2 & review2 < 2) | review <2 & is.na(review2) ) %>%
  left_join(dec_urls) 

#QA/QC:check begin and end years that don't match, check no years
#one card with no date is correct 
#dates_good<- dates_good%>%
 # mutate(check = ifelse(begin_date_year != end_date_year, TRUE, FALSE)) #these are all false 

##### JOIN ALL 3 DATASETS #### 
#see if these already exist
overlap<-semi_join(fishc_basic_dates, clean_dates2, by=c('subject_id')) #return all rows from x with a match in y.

#will need to join them naturally 
fishc_all_join<-natural_join(fishc_basic_dates, clean_dates2, by  = 'subject_id', jointype = "FULL") %>% 
  rbind(clean_dates3)

# clean up lake names, counties, and duplicates  
#subjects that were removed from workflows for one issue or another and should not be included
problem_subjects <- read.csv('whole_database/AFDMF_problem_subjects.csv')

#remove rows that are exactly the same
dup_subjects<-read.csv("whole_database/duplicates_annotated.csv") %>% 
  filter(Notes != "delete") %>% #save the ones that don't say delete 
  distinct(filename_1, .keep_all = TRUE) %>%
  mutate(duplicate_keep = "Yes") %>%  # create a column that says whether or not to keep 
  select(subject_id, duplicate_keep)
all_subjects<-read.csv("whole_database/duplicates_annotated.csv") %>% #1198
  left_join(dup_subjects) %>% 
  mutate(duplicate_keep = ifelse(is.na(duplicate_keep), "No", duplicate_keep) ) %>% 
  select(subject_id, duplicate_keep)

#just pull county/lake info 
subjects_lakenames<-read.csv('whole_database/basic_subjects_Jan2023.csv')%>% #43710
  separate(col=metadata, c("trash1", "trash2", "trash3", "filename", "trash4","trash5", "trash6", "url_front", "trash8", "trash9", "trash10", "url_back", "trash11"), sep = '"') %>% 
  select(subject_id, workflow_id, subject_set_id, filename,url_front, url_back) %>% 
  mutate(filename = gsub(".jpg_", ".jpg;", filename),  
         filename = gsub(".jpeg_", ".jpeg;", filename), 
         filename = gsub(".jpeG_", ".jpeg;", filename)) %>% 
  separate(col=filename,c("filename_1", "filename_2"), sep = ';') %>% 
  mutate(county = str_extract(filename_1, pattern = ".*?_")) %>% #pull out the county from the filename 
  mutate(county = str_sub(county, start = 1, end = -2)) %>% #gets rid of the underscore
  mutate(lakename = str_extract(filename_1, pattern = "_.*?([_\\d]|T\\d)")) %>% # pull out the lake from the filename 
  mutate(lakename = str_sub(lakename, start = 2, end = -2)) %>% #gets rid of the underscore
  mutate(lakename = toupper(lakename)) %>% 
  mutate(county = toupper(county))  %>% 
  select(subject_id, lakename, county)%>% 
  distinct(subject_id, .keep_all = TRUE) %>% 
  mutate(lakename = ifelse(subject_id == "58642842", "REDCEDAR", lakename), #update lake names that are missing 
         county = ifelse(subject_id == "58642842", "INGHAM", county), 
         lakename = ifelse(subject_id == "58642862", "VALHALLAPOND", lakename),
         county = ifelse(subject_id == "58642862", "INGHAM", county),
         lakename = ifelse(subject_id == "59962461", "NINTHSTREET", lakename),
         lakename = ifelse(subject_id == "59962462", "NINTHSTREET", lakename),
         lakename = ifelse(subject_id == "59962463", "NINTHSTREET", lakename),
         lakename = ifelse(subject_id == "82937323", "NINTHSTREET", lakename), 
         lakename = ifelse(subject_id == '59964690', "NINTHSTREET", lakename),
         lakename = ifelse(subject_id == '59964693', "NINTHSTREET", lakename),
         lakename = ifelse(subject_id == '59964695', "NINTHSTREET", lakename),
         lakename = ifelse(subject_id == '59964696', "NINTHSTREET", lakename),
         lakename = ifelse(subject_id == '59964697', "NINTHSTREET", lakename),
         lakename = ifelse(subject_id == '59964698', "NINTHSTREET", lakename),
         lakename = ifelse(subject_id == '59964699', "NINTHSTREET", lakename),
         lakename = ifelse(subject_id == '59964700', "NINTHSTREET", lakename),
         lakename = ifelse(subject_id == '59964701', "NINTHSTREET", lakename),
         lakename = ifelse(subject_id == '59964702', "NINTHSTREET", lakename)
  ) 

fishc_all<-fishc_all_join%>% #starts with 13736
  anti_join(problem_subjects) %>% #remove subjects that were removed from workflows 
  left_join(all_subjects) %>% 
  mutate(duplicate_keep = ifelse(is.na(duplicate_keep), "Yes", duplicate_keep) ) %>%#if NA then was not a duplicate, so keep 
  filter(duplicate_keep == "Yes") %>% 
  select(-c(lakename, county, duplicate_keep)) %>%  #remove lake name and county and replace with info from file name 
  left_join(subjects_lakenames) %>% 
  mutate(lakename = ifelse(subject_id == 59105551, "FORTIETHSTREET", lakename) ) 

fishc_lakenames<-subset(fishc_all,is.na(county))%>% 
  mutate(county = str_extract(front, pattern = "[a-zA-Z]+_") ) %>% 
  mutate(lakename = str_extract(front, pattern = "_.*?([_\\d]|T\\d)")) %>% 
  mutate(county = str_sub(county, start = 1, end = -2)) %>% #gets rid of the underscore
  mutate(lakename = str_sub(lakename, start = 2, end = -2)) %>% #gets rid of the underscore
  mutate(lakename = toupper(lakename)) %>% 
  mutate(county = toupper(county))  %>% 
  mutate(lakename = ifelse(subject_id == '82937349', 'CROOKEDBIG', lakename), #a few that didnt work 
         county = ifelse(subject_id == '59963531', 'ALCONA', county), 
         lakename = ifelse(subject_id == '59963531', 'HARRISVILLLE', lakename)) %>% 
  select(subject_id, lakename, county)



### try to match with database #### 
#first match by subject id 
lake_match2<-read.csv("whole_database/metadatabase_lake_match_20Apr23.csv") %>% 
  select(New_Keys, subject_ids) %>% 
  separate_rows(subject_ids, sep = ";") %>%
  mutate(subject_ids = as.integer(as.character(trimws(subject_ids, which = c("both"))))) %>% #NAs produced in blank spaces
  drop_na(subject_ids) %>% #remove NA
  distinct(subject_ids, .keep_all = TRUE) %>% #remove duplicates (front/back of image) 
  rename(subject_id = subject_ids, new_key = New_Keys)

#next match by lakename and county 
lake_match3<-read.csv("whole_database/metadatabase_lake_match_20Apr23.csv")%>% 
  select(New_Keys, County, Lake) %>% 
  rename(new_key = New_Keys, lakename=Lake,county = County  )%>% 
  mutate(lakename = toupper(lakename)) %>% 
  mutate(county = toupper(county)) %>% 
  distinct(lakename, county, .keep_all = TRUE)

fishc_all<- natural_join(fishc_all, fishc_lakenames, by=("subject_id"), jointype="FULL") %>% 
            natural_join(lake_match2, by = "subject_id", jointype = "LEFT") %>% 
            natural_join(lake_match3, by = c("lakename", "county"), jointype = "LEFT") %>% 
            rename(url_front = front, url_back = back)

#reorder 
fishc_all <- fishc_all[, c(12,11,1,2,4,5,6,7,8,9,10,3)]

write.csv(fishc_all, "FISHc/FISHc_data/final_data/fishc_all_dates.csv", row.names = FALSE)
