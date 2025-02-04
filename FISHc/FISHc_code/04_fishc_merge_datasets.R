library(dplyr)
library(tidyr)
library(stringr)

#subjects that were removed from workflows for one issue or another and should not be included
problem_subjects <- read.csv('whole_database/AFDMF_problem_subjects.csv')

#### combine all the FishC data #### 
basic_dat<-read.csv("FISHc/FISHc_data/final_data/fishc_all_dates.csv")
text_dat<-read.csv("FISHc/FISHc_data/final_data/fishc_text.csv")
abund_dat<-read.csv("FISHc/FISHc_data/final_data/fishc_abund.csv") %>% 
  select(-c(front, back)) #remove card images, these are on basic 

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
         lakename = ifelse(subject_id == "82937323", "NINTHSTREET", lakename)
  ) 


fishc_qa_qc<-left_join(basic_dat, text_dat) %>% 
  left_join(abund_dat) %>% 
  left_join(subjects_lakenames, by="subject_id")  %>% 
  select(subject_id, new_key, lakename.x, county.x, lakename.y, county.y, everything()) %>%  #move names  
  mutate(lakename.y = ifelse(is.na(lakename.y), lakename.x, lakename.y), 
         county.y = ifelse(is.na(county.y), county.x, county.y)) %>%
  select(-c(lakename.x, county.x)) %>% 
  rename( lakename=lakename.y, county = county.y) %>% 
  mutate(lakename = ifelse((new_key == "4-23" & !is.na(new_key)), "NINTHSTREET", lakename), #update lake names that are missing 
         lakename = ifelse((new_key == "77-201"  & !is.na(new_key)), "FORTIETHSTREET", lakename)
  ) %>% 
  anti_join(problem_subjects) #remove subjects that were removed from workflows


#note I tried to fill in new key using metadatabase and removing duplicates, did not change dataset, so removed code

##* correct lakes with mulitple new keys ####
#wrong<-fishc_qa_qc %>% 
 # group_by(lakename,county) %>% 
  #summarise(count = n_distinct(new_key))  %>% 
  #filter(count > 1 ) 

#fishc_check<-left_join(fishc_qa_qc, wrong, by= c('lakename', 'county')) %>% 
 # filter(!is.na(count)) %>% 
  #distinct(subject_id, .keep_all = TRUE) %>% 
  #select(subject_id, new_key, lakename, county, url_front, url_back)

#write.csv(fishc_check, "/Users/katelynking/Desktop/fishc_check.csv", row.names = FALSE)

fishc_updated_new_key<-read.csv("FISHc/FISHc_data/manual_reviews/fishc_new_key_checked.csv") %>% 
  select(subject_id, new_key, lakename, county) #updated some of the lake names and counties as well

#rows_update() modifies existing rows in all_grow with new data from updated_new_key
all_fishc<- rows_update(fishc_qa_qc, fishc_updated_new_key, by = "subject_id", unmatched = "ignore") %>% 
  filter(subject_id != 58642807 & subject_id !=58755211 & subject_id !=58755214 & subject_id !=58755215
         & subject_id !=58755216 & subject_id !=58755217& subject_id != 82941858 & subject_id !=82942119
         & subject_id !=58755237 & subject_id !=58755239) #remove grow cards (these are in the grow dataset)

#fishc_no_new_key<-filter(all_fishc,is.na( new_key)) 
fishc_no_new_key<-read.csv('FISHc/FISHc_data/fishc_nonewkey.csv') %>%
  select(subject_id, new_key)
all_fishc<- rows_update(all_fishc, fishc_no_new_key, by = "subject_id", unmatched = "ignore") #unmatched = "ignore"` if you want to ignore these `y` rows

write.csv(all_fishc, "FISHc/FISHc_data/final_data/fishc_qaqc_Jan2025.csv", row.names = FALSE)


