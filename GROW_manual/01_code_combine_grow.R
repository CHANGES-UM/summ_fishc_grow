### Join all growth datasets 
library(dplyr) #library for data wrangling 
library(stringr) #library for extracting data 
library(tidyr) #library for data munging 
library(ggplot2)
library(rqdatatable) #library for natural_join

#### read in general files #### 
#subjects that were removed from workflows for one issue or another and should not be included
problem_subjects <- read.csv('whole_database/AFDMF_problem_subjects.csv')

subjects<-read.csv('whole_database/basic_subjects_Jan2023.csv')%>% #43658
  separate(col=metadata, c("trash1", "trash2", "trash3", "filename", "trash4","trash5", "trash6", "url_1", "trash8", "trash9", "trash10", "url_2", "trash11"), sep = '"') %>% 
  select(subject_id, workflow_id, subject_set_id, filename,url_1, url_2) %>% 
  mutate(filename = gsub(".jpg_", ".jpg;", filename),  
         filename = gsub(".jpeg_", ".jpeg;", filename), 
         filename = gsub(".jpeG_", ".jpeg;", filename)) %>% 
  separate(col=filename,c("filename_1", "filename_2"), sep = ';') %>% 
  distinct(filename_1, .keep_all = T) %>% 
  mutate(filename_1 = toupper(filename_1),   #make filenames all CAPS 
         filename_2 = toupper(filename_2)) %>% #remove jpg and jpeg to match with manual spreadsheets where this wasnt always recorded
  mutate(filename_1 = gsub(".JPG", "", filename_1),  
         filename_1 = gsub(".JPEG", "", filename_1),  
         filename_2 = gsub(".JPG", "", filename_2),  
         filename_2 = gsub(".JPEG", "", filename_2)
  ) %>% 
  select(subject_id, filename_1, filename_2) %>% 
  mutate(county = str_extract(filename_1, pattern = ".*?_")) %>% #pull out the county from the filename 
  mutate(county = str_sub(county, start = 1, end = -2)) %>% #gets rid of the underscore
  mutate(lakename = str_extract(filename_1, pattern = "_.*?([_\\d]|T\\d)")) %>% # pull out the lake from the filename 
  mutate(lakename = str_sub(lakename, start = 2, end = -2)) %>% #gets rid of the underscore
  mutate(lakename = toupper(lakename)) %>% 
  mutate(county = toupper(county)) 


grow_dates <-read.csv("GROW_general/grow_dates.csv", header=TRUE, na.strings = c("", "NA"))

lake_match <-read.csv("GROW_general/grow_card_lake_match_13Sep2021.csv", header=TRUE, na.strings = c("", "NA")) %>%
  select(subject_id,'new.key') %>%
  rename(new_key = 'new.key') %>%
  drop_na('new_key')


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

subjects_urls<-read.csv('whole_database/basic_subjects_Jan2023.csv')%>% 
  separate(col=metadata, c("trash1", "trash2", "trash3", "filename", "trash4","trash5", "trash6", "url_front", "trash8", "trash9", "trash10", "url_back", "trash11"), sep = '"') %>% 
  select(subject_id, url_front, url_back) %>% 
  distinct(subject_id, .keep_all = TRUE)

### Zooniverse datasets 16 species ####
bcr<-read.csv("GROW_BCR/bcr_grow_all_qaqc.csv") %>% 
  mutate(species = "black_crappie")
bkt<-read.csv("GROW_BKT/bkt_grow_qaqc.csv")%>% 
  mutate(species = "brook_trout")
blg<-read.csv("GROW_BLG/blg_grow_all_qaqc.csv")  %>% 
  mutate(species = "bluegill")
bnt<-read.csv("GROW_BNT/bnt_grow_all_qaqc.csv")  %>% 
  mutate(species = "brown_trout")
cis<-read.csv("GROW_CIS/cis_grow_all_qaqc.csv")  %>% 
  mutate(species = "lake_herring")
cws<-read.csv("GROW_CWS/cws_grow_qaqc.csv")  %>% 
  mutate(species = "white_sucker")
lat<-read.csv("GROW_LAT/lat_grow_all_qaqc.csv")  %>% 
  mutate(species = "lake_trout")
lmb<-read.csv("GROW_LMB/lmb_grow_all_qaqc.csv")  %>% 
  mutate(species = "largemouth_bass")
nop<-read.csv("GROW_NOP/nop_grow_all_qaqc.csv")  %>% 
  mutate(species = "northern_pike")
psf<-read.csv("GROW_PSF/psf_grow_all_qaqc.csv")  %>% 
  mutate(species = "pumpkinseed")
rbt<-read.csv("GROW_RBT/rbt_grow_all_qaqc.csv")  %>% 
  mutate(species = "rainbow_trout")
rkb<-read.csv("GROW_RKB/rkb_grow_all_qaqc.csv")  %>% 
  mutate(species = "rock_bass")
smb<-read.csv("GROW_SMB/smb_grow_all_qaqc.csv")  %>% 
  mutate(species = "smallmouth_bass")
tmu<-read.csv("GROW_TMU/tmu_grow_all_qaqc.csv") %>% 
  mutate(species = "tiger_muskie")
wae<-read.csv("GROW_WAE/wae_grow_qaqc.csv")  %>% 
  mutate(species = "walleye")
yep<-read.csv("GROW_YEP/yep_grow_all_qaqc.csv")  %>% 
  mutate(species = "yellow_perch")

zoo_growth<-rbind(bcr, bkt,blg, bnt, cis, cws, lat, lmb, nop, psf, rbt, rkb, smb, tmu, wae, yep)%>% 
  select(-c(lakename, county)) %>%  #remove lake name and county and replace with info from file name 
  left_join(subjects_lakenames) %>% 
  anti_join(problem_subjects) %>% #remove subjects that were removed from workflows 
  mutate(across(where(is.character), ~ na_if(.,""))) %>%
  natural_join(subjects_urls, by = "subject_id", jointype = "LEFT") %>% 
  mutate(lakename = ifelse(subject_id == "82937336", "ISLAND", lakename),
         county = ifelse(subject_id == "82937336", "MARQUETTE", county))


#### manual transcription 20 species ####
brb<-read.csv("GROW_manual/brown_bullhead.csv", na.strings = c("", "NA")) %>% 
  mutate(species = "brown_bullhead") %>%
  drop_na('card_name') %>% 
  select(card_name, age_group, number_of_fish, length_range, mean_length, comments, species)
bur<-read.csv("GROW_manual/burbot.csv", na.strings = c("", "NA")) %>% 
  mutate(species = "burbot") %>%
  drop_na('card_name')%>% 
  select(card_name, age_group, number_of_fish, length_range, mean_length, comments, species)
car<-read.csv("GROW_manual/carp.csv", na.strings = c("", "NA")) %>% 
  mutate(species = "common_carp") %>%
  drop_na('card_name')%>% 
  select(card_name, age_group, number_of_fish, length_range, mean_length, comments, species)
chs<-read.csv("GROW_manual/chinook.csv", na.strings = c("", "NA")) %>% 
  mutate(species = "chinook_salmon") %>%
  drop_na('card_name')%>% 
  select(card_name, age_group, number_of_fish, length_range, mean_length, comments, species)
cos<-read.csv("GROW_manual/coho.csv", na.strings = c("", "NA")) %>% 
  mutate(species = "coho_salmon") %>%
  drop_na('card_name')%>% 
  select(card_name, age_group, number_of_fish, length_range, mean_length, comments, species)
dru<-read.csv("GROW_manual/freshwater_drum.csv", na.strings = c("", "NA")) %>% 
  mutate(species = "freshwater_drum") %>%
  drop_na('card_name')%>% 
  select(card_name, age_group, number_of_fish, length_range, mean_length, comments, species)
gsf<-read.csv("GROW_manual/green_sunfish.csv", na.strings = c("", "NA")) %>% 
  mutate(species = "green_sunfish") %>%
  drop_na('card_name')%>% 
  select(card_name, age_group, number_of_fish, length_range, mean_length, comments, species)
hsf<-read.csv("GROW_manual/hybrid_sunfish.csv", na.strings = c("", "NA")) %>% 
  mutate(species = "hybrid_sunfish") %>%
  drop_na('card_name')%>% 
  select(card_name, age_group, number_of_fish, length_range, mean_length, comments, species)
lwf<-read.csv("GROW_manual/lake_whitefish.csv", na.strings = c("", "NA")) %>% 
  mutate(species = "lake_whitefish") %>%
  drop_na('card_name')%>% 
  select(card_name, age_group, number_of_fish, length_range, mean_length, comments, species)
lsf<-read.csv("GROW_manual/longear_sunfish.csv", na.strings = c("", "NA")) %>% 
  mutate(species = "northern_longear_sunfish") %>%
  drop_na('card_name')%>% 
  select(card_name, age_group, number_of_fish, length_range, mean_length, comments, species)
mus<-read.csv("GROW_manual/muskie.csv", na.strings = c("", "NA")) %>% 
  mutate(species = "muskellunge") %>%
  drop_na('card_name')%>% 
  select(card_name, age_group, number_of_fish, length_range, mean_length, comments, species)
rsf<-read.csv("GROW_manual/redear_sunfish.csv", na.strings = c("", "NA")) %>% 
  mutate(species = "redear_sunfish") %>%
  drop_na('card_name')%>% 
  select(card_name, age_group, number_of_fish, length_range, mean_length, comments, species)
rwf<-read.csv("GROW_manual/round_whitefish.csv", na.strings = c("", "NA")) %>% 
  mutate(species = "round_whitefish") %>%
  drop_na('card_name')%>% 
  select(card_name, age_group, number_of_fish, length_range, mean_length, comments, species)
sau<-read.csv("GROW_manual/sauger.csv", na.strings = c("", "NA")) %>% 
  mutate(species = "sauger") %>%
  drop_na('card_name')%>% 
  select(card_name, age_group, number_of_fish, length_range, mean_length, comments, species)
sme<-read.csv("GROW_manual/smelt.csv", na.strings = c("", "NA")) %>% 
  mutate(species = "rainbow_smelt") %>%
  drop_na('card_name')%>% 
  select(card_name, age_group, number_of_fish, length_range, mean_length, comments, species)
spl<-read.csv("GROW_manual/splake.csv", na.strings = c("", "NA")) %>% 
  mutate(species = "splake") %>%
  drop_na('card_name')%>% 
  select(card_name, age_group, number_of_fish, length_range, mean_length, comments, species)
stn<-read.csv("GROW_manual/sturgeon.csv", na.strings = c("", "NA")) %>% 
  mutate(species = "lake_sturgeon") %>%
  drop_na('card_name')%>% 
  select(card_name, age_group, number_of_fish, length_range, mean_length, comments, species)
war<-read.csv("GROW_manual/warmouth_bass.csv", na.strings = c("", "NA")) %>% 
  mutate(species = "warmouth") %>%
  drop_na('card_name')%>% 
  select(card_name, age_group, number_of_fish, length_range, mean_length, comments, species)
whb<-read.csv("GROW_manual/white_bass.csv", na.strings = c("", "NA")) %>% 
  mutate(species = "white_bass") %>%
  drop_na('card_name')%>% 
  select(card_name, age_group, number_of_fish, length_range, mean_length, comments, species)
wcr<-read.csv("GROW_manual/white_crappie.csv", na.strings = c("", "NA")) %>% 
  mutate(species = "white_crappie") %>%
  drop_na('card_name')%>% 
  select(card_name, age_group, number_of_fish, length_range, mean_length, comments, species)

#combine manual and join with dates 
manual_grow_species<-rbind(brb, bur, car, chs, cos, dru, gsf, hsf, lsf, lwf, mus, rsf, rwf, sau, sme, spl, stn, war, wcr, whb) %>% 
  mutate(card_name = toupper(card_name)   #make filenames all CAPS 
       ) %>%
  mutate(card_name = gsub(".JPG", "", card_name),   #remove jpg and jpeg to match 
         card_name = gsub(".JPEG", "", card_name)
  ) %>%
  left_join(subjects, by=c("card_name" = "filename_1")) %>% 
  left_join(subjects, by=c("card_name" = "filename_2")) %>% 
  mutate(subject_id = ifelse(is.na(subject_id.x), subject_id.y, subject_id.x), 
         county = ifelse(is.na(county.x), county.y, county.x), 
         lakename = ifelse(is.na(lakename.x), lakename.y, lakename.x), 
    ) %>% 
  select(-c(card_name, subject_id.x, subject_id.y, lakename.x, lakename.y, county.x, county.y, filename_1, filename_2)) %>% #remove unwanted rows 
  rename(length_mean=mean_length, fish_count = number_of_fish) %>% 
 separate(length_range, c("length_min", "length_max"), sep = "-")%>% # split out ranges 
  mutate(fish_count = as.numeric(as.character(fish_count)), #change values to numeric 
         length_min = as.numeric(as.character(length_min)),
         length_max = as.numeric(as.character(length_max)),
         length_mean = as.numeric(as.character(length_mean))
  )

#join with basic dates and with lake matching info and standardize units 
grow_units<-read.csv("GROW_general/grow_card_units.csv")  

GROW<-left_join(manual_grow_species, grow_dates) %>%
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
  mutate(length_mean_mm = ifelse(is.na(length_mean_mm) & fish_count == 1, length_min_mm, length_mean_mm)) %>%  #if only one fish, fill in length mean with the same as the min length 
  mutate(comment = ifelse(is.na(comment), comments, comment)
  ) %>% 
  select(-c(comments)) %>% 
  anti_join(problem_subjects) #remove problem subjects 



#### combine growth data ####
#and remove duplicates where the image was assigned multiple subject ids 
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


  
#all_grow %>% 
 # group_by(subject_id) %>% 
#  summarise(sum_na= sum(is.na(new_key))) %>% 
 # filter(sum_na > 0
  #       ) #213 cards #139 cards 

### SEE IF YOU CAN FILL IN NEW KEY USING HUMPHRIES or metadatabase #### 
lake_match2<-read.csv("whole_database/metadatabase_lake_match_20Apr23.csv") %>% 
  select(New_Keys, subject_ids) %>% 
  separate_rows(subject_ids, sep = ";") %>%
  mutate(subject_ids = as.integer(as.character(trimws(subject_ids, which = c("both"))))) %>% #NAs produced in blank spaces
  drop_na(subject_ids) %>% #remove NA
  distinct(subject_ids, .keep_all = TRUE) %>% #remove duplicates (front/back of image) 
  rename(subject_id = subject_ids, new_key = New_Keys)

all_grow<-rbind( GROW, zoo_growth) %>% #starts with 62713
  left_join(all_subjects) %>% 
  mutate(duplicate_keep = ifelse(is.na(duplicate_keep), "Yes", duplicate_keep) ) %>%#if NA then was not a duplicate, so keep 
  filter(duplicate_keep == "Yes") %>% #takes it down to 60384
  filter(subject_id != 61362146 & subject_id != 58755316 & subject_id != 58642612 & subject_id != 58642632 & subject_id != 58755141 & subject_id != 58746462 & subject_id !=61360535 & subject_id !=58754506) %>%  # duplicate cards found in QAQC
  distinct(lakename, county, species, age_group, fish_count, length_min_mm, length_max_mm, length_mean_mm, begin_date_day, begin_date_month, begin_date_year, .keep_all = TRUE) %>% #filter again based on column values that are exactly the same 
   select(-c(duplicate_keep)) %>% 
  natural_join(lake_match2, by = "subject_id", jointype = "LEFT") 

#re-order columns 
all_grow <- all_grow[, c(1,16, 12, 7, 17, 2, 11, 15,13,14, 3,4,5,8,9,10,6,19,18)]  

#write.csv(all_grow, "/Users/katelynking/Desktop/all_grow_21NOV2023.csv", row.names = FALSE)
#all_grow<-read.csv("/Users/katelynking/Desktop/all_grow_21NOV2023.csv")

#### QAQC ####
#* look for outliers #### 
##these were checked and updated in the individual cleaning code or confirmed to be true to the card
summary <- all_grow %>%
  group_by(species, age_group) %>%
  summarise(
    Q25 = quantile (length_mean_mm, 0.25, na.rm=TRUE), 
    Q75 = quantile(length_mean_mm, 0.75, na.rm=TRUE), 
    IQR_plus = IQR(length_mean_mm, na.rm=TRUE) * 1.75 
  ) %>% 
  mutate(upper_whisk = Q75 + IQR_plus, 
         lower_whisk = Q25 - IQR_plus) 


grow_IQR<-left_join(all_grow, summary, by = c("species", "age_group")) %>% 
  mutate(check = ifelse(length_mean_mm > upper_whisk | length_mean_mm < lower_whisk, TRUE, FALSE)) %>%
  filter(check == "TRUE") %>% 
  select(subject_id, species, begin_date_month, age_group, fish_count, length_min_mm, length_max_mm, length_mean_mm, url_front, url_back, lower_whisk, upper_whisk, check)



##* correct lakes with mulitple new keys ####
#wrong<-all_grow %>% 
 # group_by(lakename,county) %>% 
  #summarise(count = n_distinct(new_key))  %>% 
  #filter(count > 1 ) 

#grow_check<-left_join(all_grow, wrong, by= c('lakename', 'county')) %>% 
 # filter(!is.na(count)) %>% 
  #distinct(subject_id, .keep_all = TRUE) %>% 
  #select(subject_id, new_key, lakename, county, url_front, url_back)

#write.csv(grow_check, "/Users/katelynking/Desktop/grow_check.csv", row.names = FALSE)
grow_updated_new_key<-read.csv("GROW_general/grow_check_new_key.csv") %>% 
  select(subject_id, new_key, lakename, county) %>% #updated some of the lake names and counties as well
  mutate(new_key = ifelse(lakename== "NINTHSTREET" , "4-20", new_key), 
         new_key = ifelse(subject_id== "59962664" , "8-69", new_key)) #these got changed to dates in excel

#rows_update() modifies existing rows in all_grow with new data from updated_new_key
all_grow<- rows_update(all_grow, grow_updated_new_key, by = "subject_id", unmatched = "ignore")

#grow_no_new_key<-filter(all_grow,is.na( new_key)) %>% 
 # distinct(subject_id, .keep_all = TRUE) %>%  
  #select(subject_id, new_key, lakename, county, url_front, url_back)

#add lakes with manual matching of new key and nhdid 
grow_no_new_key<-read.csv("GROW_general/grow_no_new_key.csv")  %>% 
  select(subject_id, new_key) #keep only subject ids and new keys but read in the whole table if you want to see comments 

#link to nhdid when available  
mdnr_crosswalk<-read.csv("whole_database/mdnr_crosswalk.csv") %>% 
  select(new_key, nhdid) %>% 
  distinct(new_key, .keep_all = TRUE)

all_grow<- rows_update(all_grow, grow_no_new_key, by = "subject_id") %>% 
  mutate(age_group = ifelse(subject_id == 58746421, NA, age_group)) %>% #one update after making figure 
  left_join(mdnr_crosswalk) %>%
  mutate(filename1 = basename(url_front), #pull out the file name 
         filename2 = basename(url_back)) %>% 
  select(-c("url_front", "url_back"))  

write.csv(all_grow, "C:\\Users\\kingk42\\Desktop\\grow_data.csv", row.names = FALSE)
write.csv(all_grow, "GROW_general/grow_qaqc_Feb2025.csv", row.names = FALSE)



