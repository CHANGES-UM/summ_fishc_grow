### Additional summary card cleaning for cards run through the Zooniverse workflow in Dec 2022 #### 

#### load libraries #### 
library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
library(rqdatatable)
library(data.table)

#read in task names 
tasks<-read.csv("Lake_SUMM/SUMM_data/SUMM_new_key.csv")

#### Fishing intensity was a dropdown task ####
#dropdown_reducer_SUMM_new - this file already has urls, and tasks/codes replaced from OpenRefine
new_dropdown<-read.csv("Lake_SUMM/SUMM_data/dec_data/dropdown_reducer_SUMM_newer_dec.csv", na.strings = c("", "NA")) %>% #urls are on here if you want them
  select(original_id, task, 'data.value', 'URL.4', 'URL.8') %>%
  drop_na('data.value') %>% 
  rename(data_value = 'data.value', url_front = 'URL.4', url_back ='URL.8') %>% 
  left_join(tasks) %>% 
  separate(col=data_value, c("review1", "review2" ), sep = ",") %>%
  separate(col=review1, c("answer1", "review1"), sep = ":") %>%
  separate(col=review2, c("answer2", "review2"), sep = ":") %>%
  mutate(answer1=gsub("[[:punct:]]", "", answer1),  #remove punctuation 
        review1 = as.numeric( gsub("[[:punct:]]", "", review1) ), 
      answer2= gsub("[[:punct:]]", "", answer2),  
      review2 = as.numeric(gsub("[[:punct:]]", "", review2) )
    )

#looks like all are good 
summ_drop_good<-dplyr::filter(new_dropdown, review1 >= 2  | review2 >= 2) %>%
  mutate(data= case_when(
    is.na(review2) ~ answer1, # if ~ then 
    review1 > review2 ~ answer1, 
    review2 > review1 ~ answer2 
  )) %>% 
  select(original_id, column_name, data, url_front, url_back)%>% #check column names in other data 
  mutate(data = trimws(data, which = c("both")), #trim white space 
         data = tolower(data), #standardize answers 
         ) %>%  #want to consolidate into none, light, medium, and heavy - if an in between category listed, go with heavier 
  mutate(fishing_intensity = case_when( 
    data == 'heavy' ~ 'heavy', 
    data == 'light' ~ 'light', 
    data == 'lighttomedium' ~ 'medium', 
    data == 'lighttomoderate' ~ 'medium', 
    data == 'lighttonone' ~ 'light', 
    data == 'littleornone' ~ 'light', 
    data == 'medium' ~ 'medium', 
    data == 'mediumtoheavy' ~ 'heavy', 
    data == 'none' ~ 'none', 
    data == 'notopen' ~ 'none', 
    data == 'vlight' ~ 'light', 
    )
  ) %>% 
  pivot_wider(
                id_cols = c(original_id),
                names_from = column_name, 
                values_from = c(data, fishing_intensity), 
                values_fill = list(answer = NA) ) %>% 
  rename(fishing_intensity_summer_text = "data_Intensity of Fishing_Summer", fishing_intensity_winter_text = "data_Intensity of Fishing_Winter", 
         fishing_intensity_summer = "fishing_intensity_Intensity of Fishing_Summer", fishing_intensity_winter = "fishing_intensity_Intensity of Fishing_Winter")
    

#### text_SUMM_new: lake variables ######################## 

SUMM_text_new<-read.csv("Lake_SUMM/SUMM_data/dec_data/text_reducer_SUMM_newer_dec.csv") %>%
  select(subject_id, original_id, task, 'data.aligned_text', 'data.number_views', 'data.consensus_score','data.consensus_text', 'URL.4','URL.8') %>%
  drop_na('data.number_views')%>% 
  left_join(tasks) # join with the task values to get the actual names

#best way to tackle this is to split up by task (21 tasks)
#https://stackoverflow.com/questions/54311359/use-gsub-remove-all-string-before-first-numeric-character
summary(as.factor(SUMM_text_new$column_name)) #check out all of the tasks 

#*lake characteristics ######################################
lake_char<-filter(SUMM_text_new, column_name == "Area" | column_name == "Max Depth" | column_name == "Area of Vegetation" | 
                    column_name == "Secchi disk " ) %>% 
  rename(data_consensus_text= 'data.consensus_text')

#function to convert ft and in to just feet 
ftin2ft <- function(data) {
  feet <- as.numeric(str_extract(data, "(\\d)+(?=feet)"))
  inch <- as.numeric(str_extract(data, "(\\d)+(?=inches)"))
  feet + inch/12
}

#note "area of vegetation" was changed to 'width of shoal' in some cards
#filter out the ones <50% for manual review 
lake_char_manual<-filter(lake_char, data.consensus_score < 2) %>% 
  mutate(data_consensus_text = ifelse(subject_id == 83381077, NA, data_consensus_text), 
         data_consensus_text = ifelse(subject_id == 83381078 & task == "T10", '7.5 m', data_consensus_text),
         data_consensus_text = ifelse(subject_id == 83381078  & task == "T14", '2 m', data_consensus_text),
         data_consensus_text = ifelse(subject_id == 83381083, "8 m", data_consensus_text),
         data_consensus_text = ifelse(subject_id == 83381064, "4 feet", data_consensus_text), 
         data_consensus_text = ifelse(subject_id == 83381074, "50 - 2000 feet", data_consensus_text)
         )%>% 
  separate(data_consensus_text,
           into = c("data1", "data2"), #separate ranges first
           sep= "-",   
           extra = 'merge', 
           remove = FALSE)  %>% 
  mutate(min = str_extract(data1, regex("[\\d\\,\\.]+")),  #extract the min; a number with commas and decimals ok
        units1 = str_extract(data1,  regex("[A-Za-z]+")),  #extract min units 
        max = str_extract(data2,  regex("[\\d\\,\\.]+")),  #extract the max
        units2 = str_extract(data2,  regex("[A-Za-z]+")), 
        min = as.numeric(min) , 
        max = as.numeric(max) 
        )

#clean good data
lake_char_good<-filter(lake_char, data.consensus_score >= 2) %>% 
  mutate(data = tolower(data_consensus_text), #make all text lowercase
         data = gsub('^\\D+', '', data), #remove anything before the number
         data = gsub('to', '-', data), 
         data = gsub("'", 'feet', data), 
         data = gsub("ft\\.", 'feet', data), 
         data = gsub("ft", 'feet', data), 
         data =gsub('"', 'inches', data),
         data =gsub('in\\.', 'inches',data)
        ) %>% 
  separate(data, #extract data and units 
           into = c("data1", "data2"), #separate ranges first
           sep= "-",   
           extra = 'merge') %>%
  mutate(new_min=round(ftin2ft(data1),3), #function to combine ft and in
         new_max=round(ftin2ft(data2),3)) %>% 
  mutate(min = ifelse(is.na(new_min), (str_extract(data1, regex("[\\d\\,\\.]+"))), new_min))%>% #if new_min is NA then extract the min; a number with commas and decimals ok
  mutate(units1 = str_extract(data1,  regex("[A-Za-z]+"))) %>% #extract min units 
  mutate(max = ifelse(is.na(new_max), (str_extract(data2,  regex("[\\d\\,\\.]+"))), new_max)) %>% #extract the max
  mutate(units2 = str_extract(data2,  regex("[A-Za-z]+"))) %>%  #extract max units 
 mutate(max = ifelse(subject_id == "83381050" & task == "T8", NA, max), ##specific fields still have issues 
        max = ifelse(units2 == "map", NA, max),
        max = ifelse(subject_id == "83381059" & task == "T8", NA, max),
        min = ifelse(subject_id == "83381065" & task == "T14", 9.166, min),
        min = ifelse(subject_id == "83381070" & task == "T14", 4.5, min), 
        min =gsub(',', '', min),  #remove commas 
        min = as.numeric(min) , 
        max = as.numeric(max) 
 ) %>% 
  select(-c(new_min, new_max))

#QAQC if max is greater than min 
#lake_char_good$review<-ifelse(lake_char_good$min >= lake_char_good$max, TRUE, FALSE) 
#summary(lake_char_good$review)

#combine manual and good data 
lake_char_clean<-gtools::smartbind(lake_char_good, lake_char_manual) %>% #standardize units ; use summary(as.factor(lake_char_clean$units1)) to see all units ; determine unit based on card image or variable
  mutate(units1 = ifelse(is.na(units1), units2, units1)) %>% #incorporate units2 into units1 column 
  mutate(units= case_when( #standardize units into new column 
    units1 == 'a' ~ 'acres', # if ~ then 
    units1 == 'ac' ~ 'acres',
    units1 == 'from' ~ 'acres',
    units1 == 'ha' ~ 'hectares',
    units1 == 'm' ~ 'meters',
    units1 == 'map' ~ NA_character_,
    units1 == 'Negligible' ~ NA_character_,
    units1 == 'sparse' ~ NA_character_,
    TRUE ~ as.character(units1)
  )) %>%
  select(original_id, column_name, min, max, units)

#change units to final format, need to split out variables
area<-filter(lake_char_clean, column_name == "Area")
veg<-filter(lake_char_clean, column_name == "Area of Vegetation") #this can be in feet, acres, and % so I leave this variable out 
depth<-filter(lake_char_clean, column_name == "Max Depth")
secchi<-filter(lake_char_clean, column_name == "Secchi disk ")

area<-area %>% #area on card is in acres
  mutate(units = ifelse(is.na(units), 'acres', units)) %>% #all NAs put to acres 
  mutate(lake_area_min_ha= ifelse(units == 'acres', round(min/2.471,3), min)) %>% #convert acres to ha 
  mutate(lake_area_max_ha= ifelse(units == 'acres', round(max/2.471,3), max)) %>% #convert acres to ha 
  select(original_id, lake_area_min_ha, lake_area_max_ha)

depth<-depth %>% #depth on card is in feet - might make sense to choose the highest depth as the max, instead of having a range
  mutate(units = ifelse(is.na(units), 'feet', units)) %>% #all NAs put to feet 
  mutate(max_depth_min_m= ifelse(units == 'feet', round(min/3.281,3), min)) %>% #convert feet to meters 
  mutate(max_depth_max_m= ifelse(units == 'feet', round(max/3.281,3), max)) %>% #convert feet to meters 
  select(original_id, max_depth_min_m, max_depth_max_m)

secchi<-secchi %>% #depth on card is in feet - might make sense to choose the highest depth as the max, instead of having a range
  mutate(units = ifelse(is.na(units), 'feet', units)) %>% #all NAs put to feet 
  mutate(secchi_min_m= ifelse(units == 'feet', round(min/3.281,3), min)) %>% #convert feet to meters 
  mutate(secchi_max_m= ifelse(units == 'feet', round(max/3.281,3), max)) %>% #convert feet to meters 
  select(original_id, secchi_min_m, secchi_max_m)

clean_lake_char<-full_join(area, depth) %>%
  full_join(secchi)

#check plots 
plot(clean_lake_char$lake_area_min_ha)
plot(clean_lake_char$max_depth_min_m)
plot(clean_lake_char$secchi_max_m)
plot(clean_lake_char$secchi_min_m)


#* percent shoal #####################################################
pct_shoal<-filter(SUMM_text_new, column_name == "Percent shoal") %>% #consensus is all >2
          mutate(data2=gsub('^\\D+', '', x=data.consensus_text), #remove anything before the number (eg 'approx' or 'about' )
                 data2=gsub('%', '', data2) #everything is % so no need to keep symbol 
                 )%>% 
          separate(data2,
           into = c("min", "max"), 
           sep= "-",  #separate ranges 
           extra = 'merge')  %>% 
  mutate(min = as.numeric(min), 
         max = as.numeric(max) ) %>% 
  pivot_wider(
              id_cols = c(original_id),
              names_from = column_name ,
              values_from = c('min','max'),
              values_fill =  NA) %>%
  rename(min_shoal_pct = 'min_Percent shoal', max_shoal_pct = 'max_Percent shoal')

#*boat resorts, cottages, hotels and homes, liveries #####################################################
houses<-filter(SUMM_text_new, column_name == "Cottages" | column_name =="Boat Resorts " | column_name =="Hotels and Homes" | column_name =="Liveries"  )

houses_bad<- filter(houses, data.consensus_score < 2)  # these are all not needed 

#clean up the good data 
houses_good<-filter(houses, data.consensus_score >= 2) %>% 
  mutate(data.consensus_text = tolower(data.consensus_text), #make all text lowercase
          data2 = gsub('none', '0', data.consensus_text),  #change "one" to numeric 1 and "none" to numeric 0 prior to subbing out text 
         data2=gsub('one', '1', data2), # replace words with numbers 
         data2=gsub('two', '2', data2), # replace words with numbers
         data2 = gsub('^\\D+', '', x=data2),
         data2 = gsub('or', '-', x=data2)
) %>% 
  separate(data2,
           into = c("data1", "data2"), #separate ranges first
           sep= "-",   
           extra = 'merge') %>% 
  mutate(min = as.numeric(str_extract(data1, regex("[\\d\\,\\.]+"))), #pull out number keep decimals and commas 
         max = as.numeric(str_extract(data2,  regex("[\\d\\,\\.]+")))  ) %>% #one warning NA introduced is supposed to be NA 
  pivot_wider(
            id_cols = c(original_id),
            names_from = column_name ,
            values_from = c('min','max'),
            values_fill =  NA) %>%
  rename(resorts_min_n = 'min_Boat Resorts ', resorts_max_n = 'max_Boat Resorts ', 
         liveries_min_n = 'min_Liveries', liveries_max_n = 'max_Liveries',
         cottages_min_n = 'min_Cottages', cottages_max_n = 'max_Cottages',
         hotels_homes_min_n = 'min_Hotels and Homes', hotels_homes_max_n = 'max_Hotels and Homes')

#*methyl alkalinity  #################################################
#note that data consensus <2 was checked and all are correct and cleaned by the below code 
alk<-filter(SUMM_text_new,column_name == "Methyl Orange Alk Range") %>% 
  mutate(data.consensus_text = tolower(data.consensus_text), 
         data.consensus_text = gsub('to', '-', data.consensus_text )
  ) %>% 
  separate(data.consensus_text,
           into = c("data1", "data2"), #separate ranges first
           sep= "-",   
           extra = 'merge') %>% 
  mutate(min = as.numeric(str_extract(data1, regex("[\\d\\,\\.]+"))), #pull out number keep decimals and commas 
         max = as.numeric(str_extract(data2,  regex("[\\d\\,\\.]+")))) %>%
  mutate(min = ifelse(subject_id == 83381062 , 13.6, min), #switch min and max 
         max = ifelse(subject_id == 83381062 , 27.2, max), 
         min = ifelse(subject_id == 83381063 , 200, min), #switch min and max 
         max = ifelse(subject_id == 83381063 , 1161, max) 
         ) %>%
  rename(methylorange_alk_min_ppm = min, methylorange_alk_max_ppm = max) %>% 
  select(original_id, methylorange_alk_min_ppm, methylorange_alk_max_ppm) #everything is in ppm 

#QA/QC if max is the same or greater than min need to review - made these changes above 
#alk$review<-ifelse(alk$min >= alk$max, TRUE, FALSE) 

#*shore development index  #############################################
#some wrote in a % which looks like development houses, others wrote an index value for how round it is 
#We want the shoreline development index of a lake. This the ratio of the length of the lake's shoreline to the circumference of a circle with the same area as the lake.
shore_dev<-filter(SUMM_text_new, column_name == "Shore Development") %>% 
  mutate(data = as.numeric(data.consensus_text)) %>%  #keep only the index, everything else turns to NA
  select(original_id, data)%>% 
  rename(shore_dev_index = data)

#*dam information ########################
#need to split - dam in outlet will be Y/N, effect of fish movement could be Y/N or none, some, yes, height would be numerical  
dams_category<-filter(SUMM_text_new, column_name == "Dam in outlet" | column_name == 'Dam effect on fish movements')
dams_height<-filter(SUMM_text_new, column_name == "Height of dam" )

dams_height_bad<- filter(dams_height, data.consensus_score < 2)  #should be feet, otherwise correct
dams_category_bad<- filter(dams_category, data.consensus_score < 2) # both of these were checked and are correct, so use all

#clean up Category good data - create 4 categories, kept original column,
#converted answers into a binary "yes/no" dam present and effect on fish 
##any partial or seasonal effect on fish was a yes
#anything x was a no. 
#anything that was unclear or questionable wrote NA because effect is unknown
dam_cat_good<-dams_category %>% 
  mutate(data.consensus_text = tolower(data.consensus_text), 
         data.consensus_text = gsub("'", 'feet', data.consensus_text) )%>% 
  pivot_wider(
      id_cols = c(original_id, URL.4, URL.8),
      names_from = column_name, 
      values_from = c(data.consensus_text), 
      values_fill = list(answer = NA) 
  ) %>% 
  rename(dam_outlet_text='Dam in outlet', dam_effect_fish_text='Dam effect on fish movements')%>% 
  mutate(dam_outlet = case_when( 
    dam_outlet_text == 'beaver dam' ~ 'yes', # if ~ then 
    dam_outlet_text == 'no' ~ 'no',
    dam_outlet_text == 'none' ~ 'no',
    dam_outlet_text == 'none, becomes a drainage ditch' ~ 'no',
    dam_outlet_text == 'yes' ~ 'yes',
    dam_outlet_text == 'yes - man made' ~ 'yes',
    dam_outlet_text == 'yes.' ~ 'yes'
  )) %>% 
  mutate(dam_effect_fish = case_when(
    dam_effect_fish_text == 'barrier' ~ 'yes',
    dam_effect_fish_text == 'completely blocks upstream migration of fish' ~ 'yes',
    dam_effect_fish_text == 'effective barrier' ~ 'yes',
    dam_effect_fish_text == 'no fish barrier in spring - 15feet cement apron with 6" head above.' ~ 'yes',
    dam_effect_fish_text == 'prevents movement of fish to and from the lake' ~ 'yes',
    dam_effect_fish_text == 'stops fish movement, except during high water.' ~ 'yes',
    dam_effect_fish_text == 'stops some fish' ~ 'yes',
    dam_effect_fish_text == 'unknown' ~ NA_character_, 
    dam_effect_fish_text == 'none' ~ "no", 
    dam_effect_fish_text == 'barrier to upstream movement'~ 'yes'
  )) %>% 
  dplyr::select(-c("URL.4", "URL.8"))

#dam height 
dam_height_good<-dams_height %>% 
  mutate(data.consensus_text = tolower(data.consensus_text),  #clean up answers
         data.consensus_text = gsub("'", 'feet', data.consensus_text), 
         data.consensus_text = gsub('none', NA, data.consensus_text), 
         data.consensus_text = gsub('"', 'inches', data.consensus_text), 
         data.consensus_text = gsub(' 1/2', '.5', data.consensus_text ), 
         data.consensus_text =gsub("ft", 'feet',data.consensus_text )
         )%>% 
  pivot_wider(
    id_cols = c(original_id, URL.4, URL.8),
    names_from = column_name, 
    values_from = c(data.consensus_text), 
    values_fill = list(answer = NA) 
  )  %>% #separate the numbers from feet and then convert units to M
  separate('Height of dam',
           into = c("data1", "data2"), #separate ranges first
           sep= "-",   
           extra = 'merge') %>%
  mutate(min = as.numeric(str_extract(data1, regex("[\\d\\,\\.]+"))), 
         units1 = str_extract(data1,  regex("[A-Za-z]+")),
         max = as.numeric(str_extract(data2,  regex("[\\d\\,\\.]+"))), 
         units2 = str_extract(data2,  regex("[A-Za-z]+"))) %>% 
  mutate(dam_height_min_m= ifelse(units1 == 'feet', round(min/3.281,3), min), #convert feet to meters 
          dam_height_max_m= ifelse(units1 == 'feet', round(max/3.281,3), max)) %>% 
  select(original_id, dam_height_min_m, dam_height_max_m)

#*DO and temp #######################################################
#*DO above thermocline, DO below thermocline, DO in thermocline, Temp Surface, Temp Bottom
do_temp<-filter(SUMM_text_new, column_name == "Dissolved oxygen above thermocline" | column_name == "Dissolved oxygen below thermocline" | column_name == 'Dissolved oxygen in thermocline' 
                | column_name == "Temperature_Surface" | column_name == "Temperature_Bottom" ) %>% 
  filter(data.number_views != 1) %>% 
  mutate(data.consensus_text = ifelse(subject_id == 83381079 & task == 'T19', NA, data.consensus_text)
  )
#do_temp_bad<- filter(do_temp, data.consensus_score < 2 ) #select out where consensus score is less than 2
#looks like all errors due to spacing except for the one reviewed by only one person as well as another too difficult to read, edit code above  

#easier to separate the temp from the DO to be able to standardize units 
temps<-filter(do_temp, column_name == "Temperature_Surface" | column_name == "Temperature_Bottom")%>%
  mutate(data.consensus_text = tolower(data.consensus_text), #make all text lowercase
         data.consensus_text = gsub('to', '-', x=data.consensus_text)
         ) %>% 
  separate(data.consensus_text,
           into = c("data1", "data2"), #separate ranges first
           sep= "-",   
           extra = 'merge') %>%
  mutate(min = as.numeric(str_extract(data1, regex("[\\d\\,\\.]+")))) %>% #extract the min; a number with commas and decimals ok
  mutate(units1 = str_extract(data1,  regex("[A-Za-z]+"))) %>% #extract min units 
  mutate(max = as.numeric(str_extract(data2,  regex("[\\d\\,\\.]+")))) %>% #extract the max
  mutate(units2 = str_extract(data2,  regex("[A-Za-z]+"))) %>% #extract max units 
  mutate(units1 = ifelse(is.na(units1), units2, units1)) %>% #incorporate units2 into units1 column 
  mutate(units= ifelse(is.na(units1) & min < 32, 'c', units1)) %>% #if the number is <32 then Celsius 
  mutate(units = ifelse(is.na(units), 'f', units)) %>% #all NAs put to Fahrenheit 
  mutate(min_c= ifelse(units == 'f', round((min-32)*(5/9),3), min)) %>% #convert Fahrenheit to Celsius 
  mutate(max_c= ifelse(units == 'f', round((max-32)*(5/9),3), max)) %>%
  select(original_id, min_c, max_c, column_name) %>% #select wanted columns
  pivot_wider( id_cols = c(original_id), #pivot table so each variable is a column 
               names_from = column_name ,
               values_from = c('min_c','max_c'),
               values_fill =  NA) %>%
  rename(temp_surface_min_c='min_c_Temperature_Surface', temp_surface_max_c='max_c_Temperature_Surface', 
         temp_bottom_min_c='min_c_Temperature_Bottom', temp_bottom_max_c='max_c_Temperature_Bottom')

# DO 
do<-filter(do_temp, column_name == "Dissolved oxygen above thermocline" | column_name == "Dissolved oxygen below thermocline" | column_name == 'Dissolved oxygen in thermocline')  %>%
      mutate(data.consensus_text=tolower(data.consensus_text), 
             data = gsub('no', '0', data.consensus_text), # replace words with numbers
             data = gsub('^\\D+', '', data), #remove anything before the number (eg 'approx' or 'about' ) 
             data = gsub('to', '-', data)
             ) %>% 
  mutate(data = ifelse(subject_id == 83381080 & task == 'T24', NA, data), 
         data = ifelse(subject_id == 83381085 & task == 'T20', 0.5, data), 
         data = ifelse(subject_id == 83381063 & task == 'T19', 7.7, data), 
         ) %>% 
  separate(data,
           into = c("data1", "data2"), #separate ranges first
           sep= "-",   
           extra = 'merge') %>%
  mutate(min = as.numeric(str_extract(data1, regex("[\\d\\,\\.]+")))) %>% #extract the min; a number with commas and decimals ok
  mutate(units1 = str_extract(data1,  regex("[A-Za-z]+"))) %>% #extract min units 
  mutate(max = as.numeric(str_extract(data2,  regex("[\\d\\,\\.]+")))) %>% #extract the max
  mutate(units2 = str_extract(data2,  regex("[A-Za-z]+"))) %>%#extract max units 
  select( original_id, min, max, column_name) %>% #select wanted columns
  pivot_wider( id_cols = c(original_id), #pivot table so each variable is a column 
               names_from = column_name ,
               values_from = c('min','max'),
               values_fill =  NA) %>%
  rename(do_above_thermo_min_ppm='min_Dissolved oxygen above thermocline', do_above_thermo_max_ppm='max_Dissolved oxygen above thermocline', 
         do_in_thermo_min_ppm='min_Dissolved oxygen in thermocline', do_in_thermo_max_ppm='max_Dissolved oxygen in thermocline', 
         do_below_thermo_min_ppm='min_Dissolved oxygen below thermocline', do_below_thermo_max_ppm='max_Dissolved oxygen below thermocline', )


#check min and max values to look for outliers 
plot(do$do_above_thermo_max_ppm)
plot(do$do_above_thermo_min_ppm)
plot(do$do_in_thermo_max_ppm)
plot(do$do_in_thermo_min_ppm)
plot(do$do_below_thermo_max_ppm)
plot(do$do_below_thermo_min_ppm)


#*thermocline  ####################################################          
thermocline<-filter(SUMM_text_new, column_name == "Thermocline_Present" | column_name == "Thermocline_location")
#thermocline_bad<- filter(thermocline, data.consensus_score < 2) #only one and appears to be correct

#standardize categorical to yes or no for thermocline present
thermocline_present<-filter(thermocline, column_name == 'Thermocline_Present') %>% 
  mutate(data.consensus_text=tolower(data.consensus_text), 
         data2=gsub("yes.*","yes", data.consensus_text), 
         data2=gsub("no.*","no", data2), 
         data2=ifelse(subject_id==83381063, 'no', data2)#change other answer to no  
         ) %>% 
  select(original_id, data2) %>%
  rename(thermocline_present=data2)


#clean numerical values
thermocline_location<-filter(thermocline, column_name == 'Thermocline_location') %>% 
  mutate(data.consensus_text = ifelse(subject_id ==83381047, NA, data.consensus_text)) %>% #remove answer that says 'none' 
  mutate(data.consensus_text=tolower(data.consensus_text), 
         data2=gsub('^\\D+', '', x=data.consensus_text), 
         data2=gsub('to', '-', x=data2), 
         data2=gsub("ft", 'feet', data2),
         data2=gsub("'", 'feet', data2) 
         ) %>% 
  separate(data2,
           into = c("data1", "data2"), #separate ranges first
           sep= "-",   
           extra = 'merge') %>%
  mutate(min = as.numeric(str_extract(data1, regex("[\\d\\,\\.]+")))) %>% #if new_min is NA then extract the min; a number with commas and decimals ok
  mutate(units1 = str_extract(data1,  regex("[A-Za-z]+"))) %>% #extract min units 
  mutate(max = as.numeric(str_extract(data2,  regex("[\\d\\,\\.]+")))) %>% #extract the max
  mutate(max = ifelse(subject_id == 83381049, 29, max)) %>% #correct typo
  mutate(units2 = str_extract(data2,  regex("[A-Za-z]+"))) %>% 
  mutate(units1 = ifelse(is.na(units1), units2, units1)) %>% 
  mutate(units = ifelse(is.na(units1), 'feet', units1)) %>% 
  mutate(thermocline_loc_min_m= ifelse(units == 'feet', round(min/3.281,3), min)) %>% #convert feet to meters 
  mutate(thermocline_loc_max_m= ifelse(units == 'feet', round(max/3.281,3), max)) %>% 
  select(original_id, thermocline_loc_min_m, thermocline_loc_max_m)

#QAQC plots to look for outliers
plot(thermocline_location$thermocline_loc_min_m)
plot(thermocline_location$thermocline_loc_max_m)

##### merge all variables into one table #### 
dates_dec<-read.csv("Lake_SUMM/SUMM_data/dec_data/clean_dates_dec.csv")

#full join all tables 
summary_december_data<-full_join(depth,area) %>% 
  full_join(secchi) %>% 
  full_join(shore_dev) %>% 
  full_join(pct_shoal) %>% 
  full_join(summ_drop_good) %>% #fishing intensity
  full_join(houses_good) %>% 
  full_join(dam_cat_good) %>% 
  full_join(dam_height_good) %>%  
  full_join(alk) %>% 
  full_join(do)%>%
  full_join(temps)%>%
  full_join(thermocline_present)%>%
  full_join(thermocline_location) %>% 
  rename(subject_id =original_id ) %>%
  left_join(dates_dec) 

#### OLDER CARDS #### 
tasks_old<-read.csv("Lake_SUMM/SUMM_data/SUMM_old_key.csv")

SUMM_text_old<-read.csv("Lake_SUMM/SUMM_data/dec_data/text_reducer_SUMM_older_dec.csv") %>%
  select(subject_id, original_id,task, 'data.aligned_text', 'data.number_views', 'data.consensus_score','data.consensus_text') %>%
  drop_na('data.number_views')%>% 
  left_join(tasks_old)  %>% 
  select(-c(subject_id)) %>% 
  rename(subject_id =original_id) %>%
  left_join(dates_dec) 

#*lake characteristics ######################################
lake_char<-filter(SUMM_text_old, column_name == "Size_Acres" | column_name == "Maximum Depth" | 
                    column_name == "Turbidity" ) %>% #one score under 2 is correct.
  mutate(data.consensus_text = tolower(data.consensus_text), #text to lower 
         data2= gsub('^\\D+', '', x=data.consensus_text), #remove anything before the number #note this also gets rid of cells that only have text (e.g. "very turbid")
         data2= gsub("'", 'feet', data2), # replace ' with feet 
         data2= gsub("ft\\.", 'feet', data2), # replace with feet
         data2= gsub("ft", 'feet', data2), # replace with feet 
         data2= gsub(' 1/2', '.5', data2), # replace 1/2 with .5 with a space
         data2= gsub('1/2', '0.5', data2), # replace 1/2 with .5 without a space
         data2= gsub('1/4', '0.25', data2)
  ) %>% 
  separate(data2, #extract data and units 
           into = c("data1", "data2"), #separate ranges first
           sep= "-",   
           extra = 'merge') %>%
  mutate(min = as.numeric(str_extract(data1, regex("[\\d\\,\\.]+"))), #extract a number with commas and decimals ok
         units1 = str_extract(data1,  regex("[A-Za-z]+")), #extract min units 
         max = as.numeric(str_extract(data2,  regex("[\\d\\,\\.]+"))), #extract the max
         units2 = str_extract(data2,  regex("[A-Za-z]+")) #extract max units 
  )  %>%
  select(subject_id, column_name, min, max) 

#change units to final format, need to split out variables
area<-filter(lake_char, column_name == "Size_Acres") %>% 
  mutate(lake_area_min_ha= round(min/2.471,3)) %>%
  select(subject_id, lake_area_min_ha)

depth<-filter(lake_char, column_name == "Maximum Depth") %>% #everything is in feet
  mutate(max_depth_min_m= round(min/3.281,3), min) %>% #convert feet to meters 
  mutate(max_depth_max_m= round(max/3.281,3)) %>% #convert feet to meters
  select(subject_id, max_depth_min_m, max_depth_max_m)

secchi<-filter(lake_char, column_name == "Turbidity") %>% 
  mutate(secchi_min_m =round(min/3.281,3), min) %>% #convert feet to meters 
  mutate(secchi_max_m =round(max/3.281,3), max) %>% #convert feet to meters 
  select(subject_id, secchi_min_m, secchi_max_m)

clean_lake_char<-full_join(area, depth) %>%
  full_join(secchi)

#*Recreational dev #####################################################
development<-filter(SUMM_text_old, column_name == "Recreational dev" ) %>% 
  mutate(data.consensus_text = tolower(data.consensus_text), #make all text lowercase
         data2 = gsub('none', NA_character_, data.consensus_text)
  ) %>% 
  pivot_wider( id_cols = c(subject_id), #pivot table so each variable is a column 
               names_from = column_name ,
               values_from = c('data2'),
               values_fill =  NA) %>%  #turn blank cells into NAs
  rename(development = 'Recreational dev') 
#all consensus

#* fishing pressure ##### 
#want to consolidate into none, permitted and not permitted #a little different from the newer cards, category is "public fishing") 
fishing_pressure<-filter(SUMM_text_old,column_name == "Public Fishing")  %>% 
  mutate(data.consensus_text = tolower(data.consensus_text), #text to lower 
  )  %>% 
  mutate(data2= case_when( 
    data.consensus_text == '?' ~ NA_character_,
    data.consensus_text == 'presumably allowed' ~ 'permitted',
    data.consensus_text == 'yes' ~ 'permitted',
    data.consensus_text == 'not permitted.' ~ 'not permitted',
    TRUE ~ as.character(data.consensus_text)
  )) %>%
  pivot_wider( id_cols = c(subject_id), #pivot table so each variable is a column 
               names_from = column_name ,
               values_from = c('data.consensus_text','data2'),
               values_fill =  NA) %>%
  rename(public_fishing_text='data.consensus_text_Public Fishing', public_fishing='data2_Public Fishing')

#*Alkalinity  #################################################
#a lot of the data has the investigators writing pH in the alkalinity category, 
#alk was only kept if it was listed as M.O. ppm (methyl orange) from either the hardness or the alkalinity category 
alk<-filter(SUMM_text_old,column_name == "Alkalinity")  %>%
  mutate(methylorange_alk_min_ppm =  NA, #creat empty columns first 
         methylorange_alk_max_ppm =  NA,
         methylorange_alk_min_ppm =  ifelse(subject_id == "82940965", 157, methylorange_alk_min_ppm), 
         methylorange_alk_min_ppm =  ifelse(subject_id == '82940999', 114, methylorange_alk_min_ppm), 
         methylorange_alk_max_ppm = ifelse(subject_id == '82940999', 137, methylorange_alk_max_ppm),
        methylorange_alk_min_ppm =  ifelse(subject_id == '82940943', 167, methylorange_alk_min_ppm), 
        methylorange_alk_max_ppm =  ifelse(subject_id == '82940943', 172, methylorange_alk_max_ppm),    
         )%>% 
  select(subject_id, methylorange_alk_min_ppm, methylorange_alk_max_ppm)

#*dam information ########################
#need to split - dam in outlet will be Y/N, effect of fish movement could be Y/N or none, some, yes, height would be numerical  
#kept original column, but converted answers into a binary "yes/no" dam present and passable for fish 
#any partial or passability for fish was a yes
#anything x was a no. 
#anything that was unclear or questionable wrote NA because effect is unknown 
#dams_category_bad<- filter(dams_category, data.consensus_score < 2) #these are all dashes except for 3 changed in R

dams_category<-filter(SUMM_text_old, column_name == "Dam in outlet" | column_name == 'Passable for fish')%>% 
  mutate(data.consensus_text = tolower(data.consensus_text), #text to lower 
         data2= gsub('-', NA, x=data.consensus_text),
         data2= gsub('\\.', '', data2),
         data2= gsub('\\?', '', data2),
  )  %>% 
  mutate(data2= case_when( 
    data2 == 'former outlet completely closed' ~ 'yes', # if ~ then 
    TRUE ~ as.character(data2)
  )) %>% 
  pivot_wider( id_cols = c(subject_id), #pivot table so each variable is a column 
               names_from = column_name ,
               values_from = c('data.consensus_text','data2'),
               values_fill =  NA) %>%
  rename(dam_outlet_text='data.consensus_text_Dam in outlet', dam_passable_for_fish_text='data.consensus_text_Passable for fish', 
         dam_outlet='data2_Dam in outlet', dam_passable_for_fish='data2_Passable for fish')

#dam height data numerical 
dams_height<-filter(SUMM_text_old, column_name == "Height of dam" ) %>%#everything is just a dash 
  mutate(dam_height_min_m= NA, 
         dam_height_max_m= NA) %>% 
  select(subject_id, dam_height_min_m, dam_height_max_m)

#*temp  #######################################################
temp<-filter(SUMM_text_old, column_name == "Temperature_Surface" | column_name == "Temperature_Deep Water" ) %>% 
  mutate(min = str_extract(data.consensus_text, regex("[\\d\\,\\.]+"))) %>% #extract the min; a number with commas and decimals ok
  mutate(units1 = str_extract(data.consensus_text,  regex("[A-Za-z]+"))) %>% #extract min units 
  mutate(min = ifelse(subject_id == 82940999  & task == 'T3' , 8.3, min), #pull out just the temp
         min = ifelse(subject_id == 82940943  & task == 'T3' , 22.5, min),
         min=as.numeric(min)
  ) %>%
  mutate(min_c= ifelse(units1 == 'F', round((min-32)*(5/9),3), min)) %>% ##convert F to C (32°F − 32) × 5/9 = 0°C %>%
  select(subject_id, min_c, column_name) %>% #select wanted columns
  pivot_wider( id_cols = c(subject_id), #pivot table so each variable is a column 
               names_from = column_name ,
               values_from = c('min_c'),
               values_fill =  NA) %>%
  rename(temp_surface_min_c='Temperature_Surface',  temp_bottom_min_c='Temperature_Deep Water')

#* dissoloved oxygen #### 
# DO need to split into surface, thermocline, and bottom if possible to match the other data 
#everything here is surface - so above thermocline 
do<-filter(SUMM_text_old, column_name == "Oxygen") %>% 
  mutate(do_above_thermo_min_ppm = NA, #create new column
         do_above_thermo_min_ppm = ifelse(subject_id == 82940943, 7.9, do_above_thermo_min_ppm),
         do_above_thermo_min_ppm = ifelse(subject_id == 82940999, 5.0, do_above_thermo_min_ppm),
         do_above_thermo_min_ppm = ifelse(subject_id == 82940965, 7.8, do_above_thermo_min_ppm)) %>% 
  select(subject_id, do_above_thermo_min_ppm) 
 

#### merge ####
summary_old_clean<-left_join(clean_lake_char, development) %>% 
  left_join(fishing_pressure) %>%
  left_join(alk) %>% 
  left_join(dams_category) %>% 
  left_join(dams_height) %>% 
  left_join(temp) %>% 
  left_join(do)%>%
  left_join(dates_dec) 


#### merge old and new cards #### 
summary_all<-gtools::smartbind(summary_old_clean, summary_december_data) %>% 
  select(-c(max_depth_max_m)) %>% 
  rename(max_depth_m = max_depth_min_m) %>%
  mutate(filename = basename(url_front)) %>%
  mutate(county = str_extract(filename, pattern = ".*?_")) %>% #pull out the county from the filename 
  mutate(county = str_sub(county, start = 1, end = -2)) %>% #gets rid of the underscore
  mutate(lakename = str_extract(filename, pattern = "_.*?([_\\d]|T\\d)")) %>% # pull out the lake from the filename 
  mutate(lakename = str_sub(lakename, start = 2, end = -2)) %>% #gets rid of the underscore
  mutate(lakename = toupper(lakename)) %>% 
  mutate(county = toupper(county)) %>% 
  drop_na(url_front)

### add new keys based on lakename and county####
humphries_modified <-read.csv("/Users/katelynking/Desktop/UofM/CHANGES/lake_matching/HUMPHRIES_IFR_LAKES.csv", na.strings = c("", "NA")) %>% 
  rename(new_key = New_Key, county = COUNTY) %>%
  mutate(county = toupper(county)) %>%
  mutate(lakename = toupper(Lake_Name)) %>%
  mutate(lakename = gsub(pattern = "\\s|[\\(\\)',.]|LAKE|POND", replacement = '', x = lakename)) %>%
  mutate(lakename = gsub(pattern = "^(WEST|EAST|NORTH|SOUTH|SOUTHEAST|SOUTHWEST|NORTHEAST|NORTHWEST|SMALL|LITTLE|BIG)?(.*)$", 
                         replacement = "\\2 \\1", x = lakename, perl = TRUE)) %>% #put these names at the end for consistency 
  mutate(lakename = gsub("\\s+", "", lakename)) %>% 
  select(new_key, county, lakename) %>% 
  drop_na(lakename) %>% 
  drop_na(county) %>% 
  filter(lakename != 'NONAME')

summary_all_clean<-left_join(summary_all, humphries_modified,  by= c('lakename', 'county')) %>%  #still leaves 14 blank
  mutate(new_key = ifelse(lakename == "BRICKYARD", "38-137", new_key),
         new_key = ifelse(lakename == "GREBET", "65-121", new_key), 
         lakename = ifelse(lakename == "GREBET", "GREBE", lakename), 
         new_key = ifelse(lakename == "MARSHMAN", "2-611", new_key), #matched based on TRS and area of the lake 
         new_key = ifelse(lakename == "FARWELL", "38-601", new_key),
         new_key = ifelse(lakename == "LIME", "38-425", new_key),
         new_key = ifelse(lakename == "MERKLE", "38-8", new_key),
         new_key = ifelse(lakename == "BREVORT", "49-97", new_key),
         new_key = ifelse(lakename == "BITLEY", "62-451", new_key),
         lakename = ifelse(lakename == "BITLEY", "BITELY", lakename), 
         new_key = ifelse(lakename == "CASCADES", "38-369", new_key),
         new_key = ifelse(lakename == "CLOVERLEAF", "2-801", new_key),
         ) %>%
  filter(new_key != '2-474' | is.na(new_key)) %>% # remove duplicate #Alger Sand should be 2-592 NOT 2-474
    filter(new_key != '2-252' | is.na(new_key)) # remove duplicate #Alger Hemlock should be 2-613 NOT 2-252

#could not find freymuth, heydlauff, Hoffman

#write.csv(summary_all_clean, "Lake_SUMM/lake_summary_dec.csv", row.names = FALSE)


  