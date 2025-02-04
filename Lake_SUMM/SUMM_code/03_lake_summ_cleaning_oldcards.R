### Lake Summary data cleaning old cards ###
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

#note that basic was for both old/new cards 
dates<-read.csv('Lake_SUMM/SUMM_data/clean_data/clean_dates.csv') 

#### text_SUMM_old: lake variables ######################## 
######################################################################## 
#read in task names 
SUMM_old_tasks<-read.csv("Lake_SUMM/SUMM_data/SUMM_old_key.csv", header=TRUE)
#read in text data 
SUMM_text_old<-read.csv("Lake_SUMM/SUMM_data/text_reducer_SUMM_old.csv") %>%
  select(subject_id, task, 'data.aligned_text', 'data.number_views', 'data.consensus_score','data.consensus_text') %>%
  drop_na('data.number_views') %>% 
  left_join(SUMM_old_tasks) %>%  # join with the task values to get the actual names
  left_join(urls)

summary(as.factor(SUMM_text_old$column_name)) #check out all of the tasks 
#best way to tackle this is to split up by task (13 tasks)

#*lake characteristics ######################################
lake_char<-filter(SUMM_text_old, column_name == "Size_Acres" | column_name == "Maximum Depth" | 
                    column_name == "Turbidity" )

#filter out the ones <50% for manual review 
#lake_char_bad<-filter(lake_char, data.consensus_score <= 2)
#write.csv(lake_char_bad, "/Users/katelynking/Desktop/summ_morpho_turbid.csv", row.names = FALSE)

#clean up the good data 
lake_char_good<-filter(lake_char, data.consensus_score > 2) %>% 
  mutate(data.consensus_text = tolower(data.consensus_text), #text to lower 
         data2= gsub('^\\D+', '', x=data.consensus_text), #remove anything before the number #note this also gets rid of cells that only have text (e.g. "very turbid")
         data2= gsub("'", 'feet', data2), # replace ' with feet 
         data2= gsub("ft\\.", 'feet', data2), # replace with feet
           data2= gsub("ft", 'feet', data2), # replace with feet 
         data2= gsub(' 1/2', '.5', data2), # replace 1/2 with .5 with a space
         data2= gsub('1/2', '0.5', data2) # replace 1/2 with .5 without a space
         ) %>% 
  separate(data2, #extract data and units 
           into = c("data1", "data2"), #separate ranges first
           sep= "-",   
           extra = 'merge') %>%
  mutate(min = str_extract(data1, regex("[\\d\\,\\.]+")), #extract a number with commas and decimals ok
      units1 = str_extract(data1,  regex("[A-Za-z]+")), #extract min units 
      max = str_extract(data2,  regex("[\\d\\,\\.]+")), #extract the max
      units2 = str_extract(data2,  regex("[A-Za-z]+")) #extract max units 
      ) %>% #specific fields noted to still have issues 
  mutate(min = ifelse(subject_id == 63341258 & task == 'T7', 6.5, min), 
         max = ifelse((subject_id == 63341258 | subject_id == 63341312 ) & task == 'T7', NA, max),
         min = ifelse(subject_id ==63341224 & task == 'T7', NA, min),
         min = ifelse(subject_id ==63341371 & task == 'T5', 131, min) 
      ) %>% 
  mutate(min = gsub(',', '', min), #remove commas
         min = as.numeric(min), #change data column to numeric 
         max = as.numeric(max))  %>% #standardize units ; use summary(as.factor(lake_char_good$units1)) to see all units
  mutate(units1 = ifelse(is.na(units1), units2, units1)) %>% #incorporate units2 into units1 column 
  mutate(units= case_when( #standardize units into new column 
    units1 == 'a' ~ 'acres', # if ~ then 
    units1 == 'acre' ~ 'acres',
    units1 == 'approximately' ~ 'acres',
    units1 == 'co' ~ 'feet',
    units1 == 'during' ~ 'acres', 
    units1 == 'm' ~ 'meters',
    units1 == 'map' ~ 'acres',
    units1 == 'meter' ~ 'meters',
    units1 == 'very' ~ NA_character_,
    units1 == 'with' ~ 'acres', 
    TRUE ~ as.character(units1)
  )) %>%
  select(subject_id, column_name, min, max, units)

#check again 
summary(as.factor(lake_char_good$units))

#merging back in manually reviewed data 
manual_lake_char<-read.csv("Lake_SUMM/SUMM_data/manual_review_data/summ_old_lake_char_cb.csv", na.strings = c("", "NA")) %>%
 mutate(min = as.numeric(data_min),
        max=as.numeric(data_max) 
      )  %>%
   select(subject_id, column_name, min, max, units) 

lake_char_clean<-gtools::smartbind(lake_char_good, manual_lake_char)

#change units to final format, need to split out variables
area<-filter(lake_char_clean, column_name == "Size_Acres")
depth<-filter(lake_char_clean, column_name == "Maximum Depth")
secchi<-filter(lake_char_clean, column_name == "Turbidity")

area<-area %>% #area on card is in acres
  mutate(units = ifelse(is.na(units), 'acres', units)) %>% #all NAs put to acres 
  mutate(lake_area_ha= ifelse(units == 'acres', round(min/2.471,3), min)) %>% #convert acres to ha 
  mutate(lake_area_max_ha= ifelse(units == 'acres', round(max/2.471,3), max)) %>% #convert acres to ha 
  select(subject_id, lake_area_ha)

depth<-depth %>% #depth on card is in feet - might make sense to choose the highest depth as the max, instead of having a range
  mutate(units = ifelse(is.na(units), 'feet', units)) %>% #all NAs put to feet 
  mutate(max_depth_min_m= ifelse(units == 'feet', round(min/3.281,3), min)) %>% #convert feet to meters 
  mutate(max_depth_max_m= ifelse(units == 'feet', round(max/3.281,3), max)) %>% #convert feet to meters 
  select(subject_id, max_depth_min_m, max_depth_max_m)

secchi<-secchi %>% #depth on card is in feet - might make sense to choose the highest depth as the max, instead of having a range
  mutate(units = ifelse(is.na(units), 'feet', units)) %>% #all NAs put to feet 
  mutate(secchi_min_m= ifelse(units == 'feet', round(min/3.281,3), min)) %>% #convert feet to meters 
  mutate(secchi_max_m= ifelse(units == 'feet', round(max/3.281,3), max)) %>% #convert feet to meters 
  select(subject_id, secchi_min_m, secchi_max_m)

clean_lake_char<-full_join(area, depth) %>%
  full_join(secchi)

#done! 

#*Recreational dev #####################################################
development<-filter(SUMM_text_old, column_name == "Recreational dev" ) %>% 
  mutate(data.consensus_text = tolower(data.consensus_text), #make all text lowercase
         data2 = gsub('-', '', data.consensus_text)
  ) %>% 
  mutate_all(na_if,"") %>% #turn blank cells into NAs
  pivot_wider( id_cols = c(subject_id), #pivot table so each variable is a column 
               names_from = column_name ,
               values_from = c('data2'),
               values_fill =  NA) %>%  #turn blank cells into NAs
      rename(development = 'Recreational dev')
#development_bad<- filter(development, data.consensus_score < 2) # (only 3 obs, all dashes, just change in R )

#done! 

#* fishing pressure ##### 
#want to consolidate into none, permitted and not permitted #a little different from the newer cards, category is "public fishing") 
fishing_pressure<-filter(SUMM_text_old,column_name == "Public Fishing")  %>% 
  mutate(data.consensus_text = tolower(data.consensus_text), #text to lower 
         data2= gsub('-', '', x=data.consensus_text),
         data2= gsub('\\.', '', data2)
  )  %>% #specific fields noted to still have issues 
  mutate(data2 = ifelse(subject_id == 63341193, 'open to public', data2)
  )  %>% 
  mutate(data2= case_when( 
    data2 == 'discouraged but evidently public*' ~ 'permitted', # if ~ then 
    data2 == 'little due to the fact that very few if any fish are present' ~ 'permitted',
    data2 == 'open to public' ~ 'permitted',
    data2 == 'partly yes' ~ 'permitted',
    data2 == 'permitted (except temporarily)' ~ 'permitted',
    data2 == 'yes' ~ 'permitted',
    data2 == 'yes, it is permitted' ~ 'permitted',
    TRUE ~ as.character(data2)
  )) %>% 
  mutate_all(na_if,"") %>% #turn blank cells into NAs
  pivot_wider( id_cols = c(subject_id), #pivot table so each variable is a column 
               names_from = column_name ,
               values_from = c('data.consensus_text','data2'),
               values_fill =  NA) %>%
  rename(public_fishing_text='data.consensus_text_Public Fishing', public_fishing='data2_Public Fishing')

#fishing_bad<- filter(fishing_pressure, data.consensus_score < 2)  #ALSO ONLY 3 OBS 

#done! 

#*Alkalinity  #################################################
#alk<-filter(SUMM_text_old,column_name == "Alkalinity") 
#a lot of the data has the investigators writing pH in the alkalinity category, 
#alk was only kept if it was listed as M.O. ppm (methyl orange) from either the hardness or the alkalinity category 
#this was all done by reviewing the cards manually 
#write.csv(alk, "/Users/katelynking/Desktop/summ_alk_manual.csv")
alk <-read.csv('Lake_SUMM/SUMM_data/manual_review_data/summ_old_alk_manual.csv') %>%
  mutate(alkalinity_max_ppm =  as.numeric(alkalinity_max)) %>% 
  select(subject_id, alkalinity_min, alkalinity_max_ppm) %>%
  rename(alkalinity_min_ppm='alkalinity_min')

#done! 

#*dam information ########################
#need to split - dam in outlet will be Y/N, effect of fish movement could be Y/N or none, some, yes, height would be numerical  
#kept original column, but converted answers into a binary "yes/no" dam present and passable for fish 
#any partial or passability for fish was a yes
#anything x was a no. 
#anything that was unclear or questionable wrote NA because effect is unknown 
#dams_category_bad<- filter(dams_category, data.consensus_score < 2) #these are all dashes except for 3 changed in R

dams_category<-filter(SUMM_text_old, column_name == "Dam in outlet" | column_name == 'Passable for fish')%>% 
  mutate(data.consensus_text = tolower(data.consensus_text), #text to lower 
         data2= gsub('-', '', x=data.consensus_text),
         data2= gsub('\\.', '', data2),
         data2= gsub('\\?', '', data2),
  )  %>% #specific fields noted to still have issues 
  mutate(data2 = ifelse((subject_id == 63341230 | subject_id ==63341193) & task == 'T13' , NA, data2), 
         data2 = ifelse(subject_id ==63341193 & task == 'T11', 'no outlet', data2)
  )  %>% 
  mutate(data2= case_when( 
    data2 == 'yes, at manistique' ~ 'yes', # if ~ then 
    data2 == 'rough dam on north neck' ~ 'yes',
    data2 == 'probably at high water' ~ 'yes',
    data2 == 'partially at high water only' ~ 'yes',
    data2 == 'outlet usually dry' ~ 'no',
    data2 == 'outlet not examined by party' ~ NA_character_,
    data2 == 'only in very high water' ~ 'yes',
    data2 == 'only in very high water' ~ 'yes',
    data2 == 'only in high water' ~ 'yes',
    data2 == 'only at times' ~ 'yes',
    data2 == 'not examined by survey party' ~ NA_character_,
    data2 == 'not examined' ~ NA_character_,
    data2 == 'none seen or be reported' ~ "no", 
    data2 == 'none seen' ~ "no", 
    data2 == 'none reported outlet not examined' ~ NA_character_,
    data2 == 'none observed' ~ 'no',
    data2 == 'none now' ~ 'no',
    data2 == 'near lake stream not examined' ~ 'no',
    data2 == 'none near lake' ~ 'no',
    data2 == 'none between second & baw beese' ~ 'no',
    data2 == 'none at present' ~ 'no',
    data2 == 'none above elk rapids' ~ 'no',
    data2 == 'none above baw beese outlet' ~ 'no',
    data2 == 'none' ~ 'no',
    data2 == 'no outlet' ~ 'no',
    data2 == 'no inlet or outlet' ~ 'no',
    data2 =='no according to chandelor crow and [] note that [] has seen fish [] over this dam and has also [] h langlois' ~ 'no', 
    data2 =='no (see roland lake)' ~ 'no',
    data2 =='no (see rept)' ~ 'no',
    data2 =='no (old beaver dam)' ~ 'no',
    data2 =='no  beaver at times' ~ 'no',
    data2 =='in high water' ~ 'yes',
    data2 =='generally no outlet' ~ 'no',
    data2 =='during high water' ~ 'yes',
    data2 =='broken logging dam' ~ 'no', 
    data2 =='between boot & fourth' ~ 'yes',
    data2 =='between 4th & 3rd' ~ 'yes',
    data2 =='between 3rd and baw beese' ~ 'yes',
    data2 =='at cambria  3 mi below lake' ~ 'no',
    data2 =='a 3 1/2 foot dam' ~ 'yes',
    data2 =='(see back) creek dammed several places far below lake by beaver; these do not affect water level lake ordinarily not connected with stream' ~ 'no',
    data2 =='(see back) (over) 11 creek dammed several places far below lake by beaver: these do not affect water level lake ordinarily not connected with stream' ~ 'no',
    data2 =='(outlet not examined)' ~ NA_character_,
    data2 =='(outlet not examined by party)' ~ NA_character_,
    data2 == '(not investigated)' ~ NA_character_,
    TRUE ~ as.character(data2)
  )) %>% 
  mutate_all(na_if,"") %>% #turn blank cells into NAs
  pivot_wider( id_cols = c(subject_id), #pivot table so each variable is a column 
             names_from = column_name ,
             values_from = c('data.consensus_text','data2'),
             values_fill =  NA) %>%
  rename(dam_outlet_text='data.consensus_text_Dam in outlet', dam_passable_for_fish_text='data.consensus_text_Passable for fish', 
         dam_outlet='data2_Dam in outlet', dam_passable_for_fish='data2_Passable for fish')

#dam height data numerical 
#dams_height_bad<- filter(dams_height, data.consensus_score < 2)  #these are all dashes except for 1 that is correct 
#dams_height<-filter(SUMM_text_old, column_name == "Height of dam" ) 
#only a few observations, so adjust all manually 
#write.csv(dams_height, "/Users/katelynking/Desktop/summ__old_dam_height.csv", row.names = FALSE)
dams_height<-read.csv('Lake_SUMM/SUMM_data/manual_review_data/summ__old_dam_height.csv') %>% #change units to final format
  mutate(dam_height_min_m= round(data_min/3.281,3), 
          dam_height_max_m= round(data_max/3.281,3)) %>% #convert feet to meters 
  select(subject_id, dam_height_min_m, dam_height_max_m)

 #dams done! 

#*temp  #######################################################
temp<-filter(SUMM_text_old, column_name == "Temperature_Surface" | column_name == "Temperature_Deep Water" )
temp_bad<- filter(temp, data.consensus_score < 2 ) #select out where consensus score is <50% and reviewed by more than 1 
#write.csv(temp_bad, "/Users/katelynking/Desktop/summ_temp.csv", row.names = FALSE)

#clean good data 
temp_good<- filter(temp, data.consensus_score >= 2)  %>% #select out where consensus score is >=50% 
  mutate(data2 = gsub("'", 'feet', data.consensus_text)) %>%
  separate(data2,
           into = c("data1", "data2"), #separate ranges first
           sep= "-",   
           extra = 'merge') %>%
  mutate(min = str_extract(data1, regex("[\\d\\,\\.]+"))) %>% #extract the min; a number with commas and decimals ok
  mutate(units1 = str_extract(data1,  regex("[A-Za-z]+"))) %>% #extract min units 
  mutate(max = str_extract(data2,  regex("[\\d\\,\\.]+"))) %>% #extract the max
  mutate(units2 = str_extract(data2,  regex("[A-Za-z]+"))) %>%  #extract max units 
  mutate(min = ifelse(subject_id == 63341190  & task == 'T3' , 18.9, min), #anything with "feet" or "m" in the units is incorrect 
         min = ifelse(subject_id == 63341194  & task == 'T3' , 22.5, min),
         min = ifelse(subject_id == 63341206  & task == 'T3' , 7.2, min),
         min = ifelse(subject_id == 63341247  & task == 'T3' , 8.3, min),
         min = ifelse(subject_id == 63341279  & task == 'T3' , 26.6, min),
         min = ifelse(subject_id == 63341301  & task == 'T3' , 13.3, min),
         min = ifelse(subject_id == 63341329  & task == 'T3' , 14.8, min),
         min = ifelse(subject_id == 63341335  & task == 'T3' , 23.4, min),
         min = ifelse(subject_id == 63341344  & task == 'T3' , 17.8, min),
         min = ifelse(subject_id == 63341366  & task == 'T3' , 22.5, min),
         min = ifelse(subject_id == 63341205  & task == 'T3' , 21.9, min),
         min = ifelse(subject_id == 63341210  & task == 'T3' , 6.6, min),
         min = ifelse(subject_id == 63341236  & task == 'T3' , 20.0, min),
         min = ifelse(subject_id == 63341285  & task == 'T3' , 17.5, min),
         min = ifelse(subject_id == 63341300  & task == 'T3' , 7.8, min),
         min = ifelse(subject_id == 63341320 & (task == 'T3'|task == 'T1'), NA, min),  
         min = ifelse((subject_id == 63341270 | subject_id ==63341353) & task == 'T3' , 69, min),
         min = ifelse(subject_id == 63341285 & task == 'T1' , 20.3, min),
         min = ifelse(subject_id == 63341291 & task == 'T1' , 70, min),
         max = ifelse(subject_id == 63341302  & task == 'T3' , NA, max),
         min = ifelse(subject_id ==63341291  & task == 'T3' , 52, min), 
         min=as.numeric(min),#data column to numeric 
         max=as.numeric(max)
  ) %>%
  mutate(units= ifelse(min < 32, 'celsius', 'fahrenheit')) %>% #if the number is <32 then Celsius 
  mutate(min_c= ifelse(units == 'fahrenheit', round((min-32)*(5/9),3), min)) %>% ##convert F to C (32°F − 32) × 5/9 = 0°C
  mutate(max_c= ifelse(units == 'fahrenheit', round((max-32)*(5/9),3), max)) %>%
  select(subject_id, min_c, max_c, column_name) %>% #select wanted columns
  pivot_wider( id_cols = c(subject_id), #pivot table so each variable is a column 
               names_from = column_name ,
               values_from = c('min_c','max_c'),
               values_fill =  NA) %>%
  rename(temp_surface_min_c='min_c_Temperature_Surface', temp_surface_max_c='max_c_Temperature_Surface', 
         temp_bottom_min_c='min_c_Temperature_Deep Water', temp_bottom_max_c='max_c_Temperature_Deep Water')

#add manual temperatures 
temp_manual<-read.csv('Lake_SUMM/SUMM_data/manual_review_data/summ_old_temp_calla.csv') %>% 
  select(subject_id, column_name, data_min, data_max, units)%>%
  mutate(min_c= ifelse(units == 'F', round((data_min-32)*(5/9),3), data_min)) %>% ##convert F to C (32°F − 32) × 5/9 = 0°C
  mutate(max_c= ifelse(units == 'F', round((data_max-32)*(5/9),3), data_max)) %>%
  select(subject_id, min_c, max_c, column_name) %>% #select wanted columns
  pivot_wider( id_cols = c(subject_id), #pivot table so each variable is a column 
               names_from = column_name ,
               values_from = c('min_c','max_c'),
               values_fill =  NA) %>%
  rename(temp_surface_min_c='min_c_Temperature_Surface', temp_surface_max_c='max_c_Temperature_Surface', 
         temp_bottom_min_c='min_c_Temperature_Deep Water', temp_bottom_max_c='max_c_Temperature_Deep Water')

#want to join and replace NAs from one table with data from another if there, need rqdatatable package
temperature<-natural_join(temp_good, temp_manual, by  = 'subject_id', jointype = "FULL")

  
#* dissoloved oxygen #### 
# DO need to split into surface, thermocline, and bottom if possible to match the other data 
#might just be easiest to do this manually 
do<-filter(SUMM_text_old, column_name == "Oxygen")
#write.csv(do, "/Users/katelynking/Desktop/do_review.csv", row.names = FALSE)
do<-read.csv('Lake_SUMM/SUMM_data/manual_review_data/summ_old_do_review.csv') %>% 
  select(subject_id, do_surface_ppm, do_thermo_ppm, do_below_thermo_ppm) %>% 
  mutate(do_below_thermo_ppm = ifelse(subject_id == 63341321, 6.5, do_below_thermo_ppm) )

#DO done !


#### merge all tables and join with new_keys #### 
dates<-read.csv("Lake_SUMM/SUMM_data/clean_data/clean_dates.csv")

# read in lake match info from txt file so that the new_key does not change to a date 
new_keys<-read.delim("Lake_SUMM/SUMM_data/SUMM_lakematch_4JAN22.txt",  sep = ",",  na.strings = c("", "NA"), header = TRUE) %>%
  select(subject_id, county, lakename, New_Key) %>%
  rename(new_key = New_Key)
#read in subject ids - original ids used for basic workflow, and then separate workflow used for new vs old cards so additional subject id created 
text_subject<-read.csv('Lake_SUMM/SUMM_data/text_subject_ids_old.csv') %>% 
  distinct(subject_id, .keep_all = TRUE)

lake_summary<-left_join(dates, text_subject, by=c("subject_id" = "subject_id_match")) %>% #this should match the subject to dates
  left_join(clean_lake_char, by=c("subject_id.y" = "subject_id")) %>% # this should match by the second subject id 
  left_join(development, by=c("subject_id.y" = "subject_id")) %>% 
  left_join(alk, by=c("subject_id.y" = "subject_id")) %>% 
  left_join(dams_category, by=c("subject_id.y" = "subject_id")) %>% 
  left_join(dams_height, by=c("subject_id.y" = "subject_id"))  %>% 
  left_join(do, by=c("subject_id.y" = "subject_id")) %>% 
  left_join(temperature, by=c("subject_id.y" = "subject_id"))  %>% 
  left_join(fishing_pressure, by=c("subject_id.y" = "subject_id")) %>% 
  left_join(new_keys) %>%
  select(-c(subject_id.y))  #remove secondary subject id 
  
#re-order columns 
lake_summary <- lake_summary[, c(1,35,34,33,2,3,4,5,6,7,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,8,9)]

#keep only the old data (dates file includes the new data )
lake_summary<-lake_summary %>%
  filter_at(vars(11:33),any_vars(!is.na(.)))  #removes if all are NA (because most of these are dates for new cards)



#write.csv(lake_summary, 'Lake_SUMM/SUMM_data/clean_data/lake_summary_older_cards_qaqc.csv', row.names = FALSE)
