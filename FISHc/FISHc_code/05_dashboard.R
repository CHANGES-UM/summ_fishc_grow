#### code to prep data for posting in dashboard ### 
library(dplyr) #library for data wrangling 
library(stringr) #library for extracting data 
library(tidyr) #library for data munging 
library(rqdatatable) #library for natural_join

### read in FishC data 
fishc<-read.csv('FISHc/fishc_qaqc_added_lakes.csv')

## read in general files #### 
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

#just pull county/lake info to replace other lake/county 
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



### SEE IF YOU CAN FILL IN NEW KEY USING HUMPHRIES or metadatabase #### 
lake_match2<-read.csv("whole_database/metadatabase_lake_match_20Apr23.csv") %>% 
  select(New_Keys, subject_ids) %>% 
  separate_rows(subject_ids, sep = ";") %>%
  mutate(subject_ids = as.integer(as.character(trimws(subject_ids, which = c("both"))))) %>% #NAs produced in blank spaces
  drop_na(subject_ids) %>% #remove NA
  distinct(subject_ids, .keep_all = TRUE) %>% #remove duplicates (front/back of image) 
  rename(subject_id = subject_ids, new_key = New_Keys)


humphries_modified <-read.csv("/Users/katelynking/Desktop/UofM/CHANGES/lake_matching/HUMPHRIES_IFR_LAKES.csv") %>% 
  rename(new_key = New_Key, county = COUNTY) %>%
  mutate(county = toupper(county)) %>%
  mutate(lakename = toupper(Lake_Name)) %>%
  mutate(lakename = gsub(pattern = "\\s|[\\(\\)',.]|LAKE|POND", replacement = '', x = lakename)) %>%
  mutate(lakename = gsub(pattern = "^(WEST|EAST|NORTH|SOUTH|SOUTHEAST|SOUTHWEST|NORTHEAST|NORTHWEST|SMALL|LITTLE|BIG)?(.*)$", 
                         replacement = "\\2 \\1", x = lakename, perl = TRUE)) %>% #put these names at the end for consistency 
  mutate(lakename = gsub("\\s+", "", lakename)) %>% 
  select(new_key, county, lakename)

lat_lon <-read.csv("/Users/katelynking/Desktop/UofM/CHANGES/lake_matching/HUMPHRIES_IFR_LAKES.csv") %>% 
  rename(new_key = New_Key, county = COUNTY) %>%
  select(new_key, LONG_DD, LAT_DD)


##### clean up fishc table #### 
fishc_clean<-anti_join(fishc, problem_subjects) %>% 
                left_join(all_subjects) %>% 
                mutate(duplicate_keep = ifelse(is.na(duplicate_keep), "Yes", duplicate_keep) ) %>%#if NA then was not a duplicate, so keep 
              filter(duplicate_keep == "Yes") %>% 
              select(-c(lakename,county)) %>% 
              left_join(subjects_lakenames) %>% #add lake names and county from this file  
              natural_join(lake_match2, by = "subject_id", jointype = "LEFT") %>%  #try to add more new_keys from metadatabase 
              natural_join(humphries_modified, by = c("lakename", "county"), jointype = "LEFT") %>% 
  mutate(lakename= ifelse(new_key == '4-23', 'NINTHSTPOND', lakename)) %>%
  select(-c(abund_comments, area_covered, front, back, depth_of_collection, descript_gear1, descript_gear2, descript_gear3, descript_gear4, duplicate_keep, field_no, fish_extras, remarks, temp_surface_max_c, point_of_examination, time, 
            begin_date_day, begin_date_month, end_date_day, end_date_month, end_date_year, gear_used_kind, gear_used_length, gear_used_mesh, subject_id, temp_surface_min_c, type_gear1, type_gear2, type_gear3, type_gear4)) %>% 
  select(new_key, lakename, county, begin_date_year, everything()) %>% #reorder columns 
  mutate_at(c(5:59), as.numeric)  %>% #all text turned to NA 
  group_by(new_key, lakename, county, begin_date_year) %>% 
  pivot_longer(5:59,
               names_to = "species", 
               values_to = "count") %>% 
  mutate(count = ifelse(is.na(count), 0, count)) %>% 
  filter(count > 0) %>%  #remove rows that are 0s
  left_join(lat_lon) %>% 
  drop_na(new_key)#remove anything that doesn't have a new_key 

write.csv(fishc_clean, "/Users/katelynking/Desktop/fishc_clean_gis.csv", row.names = FALSE) 

#how mnay new_key nas #still 136 NAs
#sum(is.na(fishc_clean$new_key)) 
n_distinct(fishc_clean$new_key)

