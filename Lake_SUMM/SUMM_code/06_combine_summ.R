### Combine summary cards and QA QC  #### 
library(tidyr)


#* read in general lakes data ####
#subjects that were removed from workflows for one issue or another and should not be included
problem_subjects <- read.csv('whole_database/AFDMF_problem_subjects.csv')

#duplicate images to remove #remove rows that are exactly the same
dup_subjects<-read.csv("whole_database/duplicates_annotated.csv") %>% 
  filter(Notes != "delete") %>% #save the ones that don't say delete 
  distinct(filename_1, .keep_all = TRUE) %>%
  mutate(duplicate_keep = "Yes") %>%  # create a column that says whether or not to keep 
  select(subject_id, duplicate_keep)
all_subjects<-read.csv("whole_database/duplicates_annotated.csv") %>% #1198
  left_join(dup_subjects) %>% 
  mutate(duplicate_keep = ifelse(is.na(duplicate_keep), "No", duplicate_keep) ) %>% 
  select(subject_id, duplicate_keep)

#meta database for filling in new_keys
lake_match2<-read.csv("whole_database/metadatabase_lake_match_20Apr23.csv") %>% 
  select(New_Keys, subject_ids) %>% 
  separate_rows(subject_ids, sep = ";") %>%
  mutate(subject_ids = as.integer(as.character(trimws(subject_ids, which = c("both"))))) %>% #NAs produced in blank spaces
  drop_na(subject_ids) %>% #remove NA
  distinct(subject_ids, .keep_all = TRUE) %>% #remove duplicates (front/back of image) 
  rename(subject_id = subject_ids, new_key = New_Keys)

#* read in summary card data ####  
fish_pres<-read.csv("Lake_SUMM/lake_summ_fish_pres_qaqc.csv") %>%
  select(-c(new_key, lakename, county, begin_date_day, begin_date_month, begin_date_year, end_date_day, end_date_month, end_date_year, url_front, url_back))
fish_pres_dec<-read.csv("Lake_SUMM/lake_summ_fish_pres_qaqc_dec.csv")
fish_pres_all<-gtools::smartbind(fish_pres, fish_pres_dec)
summ_new<-read.csv("Lake_SUMM/lake_summary_qaqc.csv")  
summ_old<-read.csv("Lake_SUMM/lake_summary_older_cards_qaqc.csv") %>% 
  rename(lake_area_min_ha = lake_area_ha, methylorange_alk_min_ppm=alkalinity_min_ppm, methylorange_alk_max_ppm=alkalinity_max_ppm, do_above_thermo_min_ppm = do_surface_ppm, do_in_thermo_min_ppm = do_thermo_ppm, do_below_thermo_min_ppm = do_below_thermo_ppm,  url_front = front, url_back = back) #rename some columns to join with newer cards 
summ_dec<-read.csv("Lake_SUMM/lake_summary_dec.csv")  %>% 
  rename(max_depth_min_m = max_depth_m) %>% 
  select(-c(filename))

#bind everything
new_old_dec<-natural_join(summ_new, summ_old, jointype = "FULL", by = "subject_id") %>% 
                  natural_join(summ_dec, jointype = "FULL", by = "subject_id") %>% 
  mutate(lake_area_max_ha = as.numeric(lake_area_max_ha), 
         cottages_max_n = as.numeric(cottages_max_n), 
         resorts_max_n = as.numeric(resorts_max_n), 
         hotels_homes_max_n = as.numeric(hotels_homes_max_n)) %>% 
  left_join(fish_pres_all) %>% 
  relocate(url_front, url_back, .after = last_col()) %>%
  select(-c(lakename, county)) %>%  #remove lake name and county and replace with info from file name 
  mutate(filename1 = basename(url_front),
         filename2 = basename(url_back)) %>%
  mutate(county = str_extract(filename1, pattern = ".*?_")) %>% #pull out the county from the filename 
  mutate(county = str_sub(county, start = 1, end = -2)) %>% #gets rid of the underscore
  mutate(lakename = str_extract(filename1, pattern = "_.*?([_\\d]|T\\d)")) %>% # pull out the lake from the filename 
  mutate(lakename = toupper(lakename)) %>% 
  mutate(county = toupper(county)) %>%
  mutate(lakename = ifelse(lakename == "_4", "_WHITMORE_", lakename), 
         lakename = ifelse(lakename == "_9", "_NINTHSTREETDAM_", lakename),
         new_key = ifelse(subject_id == 58687904, "44-278", new_key), #fix new key
         new_key = ifelse(subject_id == 58687719, "38-115", new_key), 
         begin_date_year = ifelse(subject_id == 58687674, 1959, begin_date_year), #fix year 
         )%>% 
  mutate(lakename = str_sub(lakename, start = 2, end = -2)) %>%   #gets rid of the underscore
  mutate(max_depth_m = ifelse(is.na(max_depth_max_m), max_depth_min_m, max_depth_max_m  )) %>% #just keep the max depth 
  select(-c(max_depth_max_m,max_depth_min_m )) %>% 
  relocate(subject_id, new_key, county, lakename, begin_date_day, begin_date_month, begin_date_year, end_date_day, end_date_month, end_date_year, #relocate all variables 
           fishing_intensity_summer,   fishing_intensity_winter, fishing_intensity_summer_text, fishing_intensity_winter_text, public_fishing_text, public_fishing, 
           max_depth_m, secchi_min_m, secchi_max_m, lake_area_min_ha, lake_area_max_ha, min_shoal_pct, max_shoal_pct, liveries_min_n, liveries_max_n, 
           resorts_min_n, resorts_max_n, cottages_min_n, cottages_max_n, hotels_homes_min_n, hotels_homes_max_n, development,
           methylorange_alk_min_ppm, methylorange_alk_max_ppm, flag_alk, shore_dev_index, dam_height_min_m,dam_height_max_m, dam_outlet,
           dam_passable_for_fish,dam_effect_fish, dam_outlet_text, dam_passable_for_fish_text, dam_effect_fish_text,
           thermocline_present,thermocline_loc_min_m,  thermocline_loc_max_m, flag_bottom,
           temp_surface_min_c,temp_surface_max_c, temp_bottom_min_c, temp_bottom_max_c,
           do_above_thermo_min_ppm ,do_above_thermo_max_ppm, do_in_thermo_min_ppm,   do_in_thermo_max_ppm ,do_below_thermo_min_ppm, do_below_thermo_max_ppm ) %>%  #default is to move the columns to the front 
  anti_join(problem_subjects) %>% #remove subjects that were removed from workflows
  left_join(all_subjects) %>% #remove duplicate subjects 
  mutate(duplicate_keep = ifelse(is.na(duplicate_keep), "Yes", duplicate_keep) ) %>%#if NA then was not a duplicate, so keep 
  filter(duplicate_keep == "Yes") %>% 
  select(-c(duplicate_keep)) 


#try to add new_keys by match with database 
new_old_dec_match<-new_old_dec %>%
  natural_join(lake_match2, by = "subject_id", jointype = "LEFT") %>% 
  select(unique(c(names(new_old_dec)))) #this re-orders columns back to original 

#how many still without new key? 
sum(is.na(new_old_dec_match$new_key)) 

#### QAQC ####
#*obs with multiple new keys ###
#wrong<-new_old_dec_match %>% 
  #group_by(lakename,county) %>% 
  #summarise(count = n_distinct(new_key))  %>% 
  #filter(count > 1 ) 

#summ_check<-left_join(new_old_dec_match, wrong, by= c('lakename', 'county')) %>% 
  #filter(!is.na(count)) %>% 
  #distinct(subject_id, .keep_all = TRUE) %>% 
  #select(subject_id, new_key, lakename, county, url_front, url_back)

summ_updated_new_key<-read.csv("Lake_SUMM/SUMM_data/manual_review_data/new_key_summ_checked.csv") %>% 
  select(subject_id, new_key, lakename, county) #updated some of the lake names and counties as well

#rows_update() modifies existing rows in data with new data from updated_new_key
all_summ<- rows_update(new_old_dec_match, summ_updated_new_key, by = "subject_id", unmatched = "ignore") 

#observations with no new_key, examine with TRS and maps to identify 
#summ_no_new_key<-filter(all_summ,is.na( new_key)) %>% 
 # select(subject_id, new_key, lakename, county, url_front, url_back) 

summ_no_new_key<-read.csv("Lake_SUMM/SUMM_data/summ_no_new_key.csv") %>% 
  select(subject_id, new_key) #keep only subject ids and new keys but read in the whole table if you want to see comments 

all_summ<- rows_update(all_summ, summ_no_new_key, by = "subject_id") %>% 
  filter((begin_date_year > 1915 | is.na(begin_date_year)) & subject_id != 58688648 & subject_id != 58688666 & subject_id != 58687292) %>% #remove cards that are referencing older dates and more dups and not summary cards 
  mutate(lakename = ifelse(lakename == "SUMM", "CHARLEVOIX", lakename)) #update lakename that is still wrong 

sum(is.na(all_summ$new_key)) #12 cards left unmatched 

write.csv(all_summ, "/Users/katelynking/Desktop/all_summ_Jan2025.csv", row.names = FALSE)


#*plot enviro variables #### 
plot(all_summ$secchi_max_m)
plot(all_summ$secchi_min_m)
plot(all_summ$min_shoal_pct)
plot(all_summ$max_shoal_pct)
plot(all_summ$resorts_min_n)
plot(all_summ$resorts_max_n)
plot(all_summ$liveries_max_n)
plot(all_summ$liveries_min_n)
plot(all_summ$cottages_max_n)
plot(all_summ$cottages_min_n)
plot(all_summ$hotels_homes_max_n)
plot(all_summ$hotels_homes_min_n)
plot(all_summ$methylorange_alk_min_ppm)
plot(all_summ$methylorange_alk_max_ppm) # check one outlier 
plot(all_summ$temp_bottom_max_c) # check 2 outlier 
plot(all_summ$temp_bottom_min_c)
plot(all_summ$temp_surface_max_c)
plot(all_summ$temp_surface_min_c)# check 3 outlier 
plot(all_summ$do_above_thermo_max_ppm)# check one outlier 
plot(all_summ$do_above_thermo_min_ppm) #check outliers 
plot(all_summ$do_below_thermo_max_ppm)
plot(all_summ$do_below_thermo_min_ppm)
plot(all_summ$do_in_thermo_max_ppm)
plot(all_summ$do_in_thermo_min_ppm)

#link new key to the new MDNR ID
MDNR_xwalk<-read.csv("/Users/katelynking/Desktop/MDNR_xwalk.csv") %>%
  rename(new_key = NewKey) %>% 
  select(MDNRID, new_key)

summ_newkeys<-all_summ%>% 
  select(subject_id, new_key) %>% 
  drop_na(new_key)

all_summ_xwalk<-left_join(summ_newkeys, MDNR_xwalk)

ifr_points<-readxl::read_excel("/Users/katelynking/Desktop/IFR_Lake_Points.xlsx") %>% 
  rename(new_key = NEW_KEY)

ifr_summ_xwalk<-left_join(summ_newkeys, ifr_points)

#new keys -> MDNR keys 



#remove URL at the end 
