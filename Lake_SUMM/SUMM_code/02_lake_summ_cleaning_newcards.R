### Lake Summary data cleaning new cards ###
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


#### dropdown SUMM new: fishing intensity  ##################################### 
#dropdown_reducer_SUMM_new - this file already has urls, and tasks/codes replaced from OpenRefine
new_dropdown<-read.csv("Lake_SUMM/SUMM_data/dropdown_reducer_SUMM_new.csv", na.strings = c("", "NA")) %>% #urls are on here if you want them
  select(subject_id, task, 'data.value') %>%
  drop_na('data.value') %>% 
  rename(data_value = 'data.value') 

#because dropdown answers are different than dates, have to separate a little differently 
new_dropdown<- separate(data=new_dropdown, col=data_value, c("review1", "review2", "review3", "review4" ), sep = ",") %>%
  separate(col=review1, c("answer1", "review1"), sep = ":") %>%
  separate(col=review2, c("answer2", "review2"), sep = ":") %>%
  separate(col=review3, c("answer3", "review3"), sep = ":") %>%
  separate(col=review4, c("answer4", "review4"), sep = ":")
  
new_dropdown$answer1 <- gsub("[[:punct:]]", "", new_dropdown$answer1)  #remove punctuation 
new_dropdown$review1 <- as.numeric( gsub("[[:punct:]]", "", new_dropdown$review1) ) #remove punctuation 
new_dropdown$answer2<- gsub("[[:punct:]]", "", new_dropdown$answer2)  #remove punctuation 
new_dropdown$review2 <- as.numeric(gsub("[[:punct:]]", "", new_dropdown$review2) )  #remove punctuation 
new_dropdown$answer3 <- gsub("[[:punct:]]", "", new_dropdown$answer3)  #remove punctuation 
new_dropdown$review3 <- as.numeric(gsub("[[:punct:]]", "", new_dropdown$review3))  #remove punctuation 
new_dropdown$answer4 <- gsub("[[:punct:]]", "", new_dropdown$answer4)  #remove punctuation 
new_dropdown$review4 <- as.numeric(gsub("[[:punct:]]", "", new_dropdown$review4) )#remove punctuation 

# keep 4s and 3s OR any that were 3,2,1 but don't have disagreements (NAs)
summ_drop_good<-dplyr::filter(new_dropdown, (review1 >= 3 | is.na(review2) | review2 >= 3)) %>%
  select(subject_id, task, answer1)
#have all none actually mean no pressure, whereas a field left blank is NAs bacause the information is unknown
summ_drop_good$answer1 <-ifelse((summ_drop_good$answer1 == "None" | summ_drop_good$answer1 == "none" |summ_drop_good$answer1 == "0" ),  "None", summ_drop_good$answer1)
summ_drop_good$answer1 <-ifelse((summ_drop_good$answer1 == "Field Left Blank" | summ_drop_good$answer1 == "leftblank" | summ_drop_good$answer1 == "" | summ_drop_good$answer1 == "unknown"), NA, summ_drop_good$answer1) 

summ_fishpress_format<-tidyr::pivot_wider(data= summ_drop_good, 
                                       id_cols = c(subject_id),
                                       names_from = task, 
                                       values_from = c(answer1), 
                                       values_fill = list(answer = NA) )


#manual check the ones <2 reviews or split  ##
summ_drop_bad<-filter(new_dropdown, review1 < 3 & !is.na(review2) & review2 < 3) #

#write.csv(summ_drop_bad, "/Users/katelynking/Desktop/summ_drop_review.csv", row.names = FALSE) (review K. King)
summ_drop_bad<-read.csv("Lake_SUMM/SUMM_data/manual_review_data/summ_fishintensity_review_kk.csv")

summ_fishpress_format2<-tidyr::pivot_wider(data= summ_drop_bad, 
                                        id_cols = c(subject_id),
                                        names_from = task, 
                                        values_from = c(answer), 
                                        values_fill = list(answer = NA) )

#want to join and replace NAs from one table with data from another if there, need rqdatatable package
fishpress_clean<-natural_join(summ_fishpress_format, summ_fishpress_format2, by  = 'subject_id', jointype = "FULL") %>%
  rename(fishing_intensity_summer_text = 'Intensity of Fishing - Summer', fishing_intensity_winter_text = 'Intensity of Fishing - Winter')
fishpress_clean$fishing_intensity_summer_text<-tolower(fishpress_clean$fishing_intensity_summer_text) #make all text lowercase
fishpress_clean$fishing_intensity_winter_text<-tolower(fishpress_clean$fishing_intensity_winter_text) #make all text lowercase

summary(as.factor(fishpress_clean$fishing_intensity_summer_text))

#want to consolidate into none, light, medium, and heavy - if an in between category listed, go with heavier 
fishpress_clean<-fishpress_clean %>% 
  mutate(fishing_intensity_summer= case_when( 
    fishing_intensity_summer_text == 'extremelylight' ~ 'light', # if ~ then 
    fishing_intensity_summer_text == 'fairlyheavy' ~ 'heavy',
    fishing_intensity_summer_text == 'formerlyheavy' ~ 'heavy',
    fishing_intensity_summer_text == 'heavyearlysummer' ~ 'heavy',
    fishing_intensity_summer_text == 'heavyper1973mailcensus' ~ 'heavy',
    fishing_intensity_summer_text == 'heavysince711940' ~ 'heavy',
    fishing_intensity_summer_text == 'light4boatsleftatlake' ~ 'light',
    fishing_intensity_summer_text == 'lightatpresent' ~ 'light',
    fishing_intensity_summer_text == 'lightbecauseofpoorfishing' ~ 'light',
    fishing_intensity_summer_text == 'lightfishing' ~ 'light',
    fishing_intensity_summer_text == 'lightforbassmostly' ~ 'light',
    fishing_intensity_summer_text == 'lightmediumfallspring' ~ 'medium',
    fishing_intensity_summer_text == 'lightmediumfishingpressure' ~ 'medium',
    fishing_intensity_summer_text == 'lightmedtoheavyinspringfall' ~ 'medium',
    fishing_intensity_summer_text == 'lightnone' ~ 'light',
    fishing_intensity_summer_text == 'lightnow' ~ 'light',
    fishing_intensity_summer_text == 'lightoneboat' ~ 'light',
    fishing_intensity_summer_text == 'lightornone' ~ 'light',
    fishing_intensity_summer_text == 'lightpracticallynone' ~ 'light',
    fishing_intensity_summer_text == 'lighttoheavy' ~ 'medium',
    fishing_intensity_summer_text == 'lighttomedium' ~ 'medium',
    fishing_intensity_summer_text == 'lighttomedium6boatstoday' ~ 'medium',
    fishing_intensity_summer_text == 'lighttomoderate' ~ 'medium',
    fishing_intensity_summer_text == 'lighttonone' ~ 'light',
    fishing_intensity_summer_text == 'medheavy' ~ 'heavy',
    fishing_intensity_summer_text == 'medinspringlightinsummer' ~ 'medium',
    fishing_intensity_summer_text == 'medium ' ~ 'medium',
    fishing_intensity_summer_text == 'medium3boatspresent' ~ 'medium',
    fishing_intensity_summer_text == 'mediumatpresent' ~ 'medium',
    fishing_intensity_summer_text == 'mediumheavyinspring' ~ 'heavy',
    fishing_intensity_summer_text == 'mediuminformationlacking' ~ 'medium',
    fishing_intensity_summer_text == 'mediumlight' ~ 'medium',
    fishing_intensity_summer_text == 'mediummostlyfromshore' ~ 'medium',
    fishing_intensity_summer_text == 'mediumorheavy' ~ 'heavy',
    fishing_intensity_summer_text == 'mediumpoor' ~ 'medium',
    fishing_intensity_summer_text == 'mediumsummerincreaseinfall' ~ 'medium',
    fishing_intensity_summer_text == 'mediumtoheavy' ~ 'heavy',
    fishing_intensity_summer_text == 'mediumtolight' ~ 'medium',
    fishing_intensity_summer_text == 'mediumtoratherheavy' ~ 'heavy',
    fishing_intensity_summer_text == 'moderate' ~ 'medium',
    fishing_intensity_summer_text == 'moderatelyheavy' ~ 'heavy',
    fishing_intensity_summer_text == 'modheavy' ~ 'heavy',
    fishing_intensity_summer_text == 'noneinmostyears' ~ 'none',
    fishing_intensity_summer_text == 'nonelightinspring' ~ 'light',
    fishing_intensity_summer_text == 'nonetolight' ~ 'light',
    fishing_intensity_summer_text == 'practicallynone' ~ 'none',
    fishing_intensity_summer_text == 'practicallynoneuptothissummerbutwasmuchfrequentedaftersurvey' ~ 'none',
    fishing_intensity_summer_text == 'private' ~ 'light',
    fishing_intensity_summer_text == 'probablylight' ~ 'light',
    fishing_intensity_summer_text == 'probablynone' ~ 'none',
    fishing_intensity_summer_text == 'springlight' ~ 'light',
    fishing_intensity_summer_text == 'veryheavy' ~ 'heavy',
    fishing_intensity_summer_text == 'verylight' ~ 'light',
    fishing_intensity_summer_text == 'verylightatpresent' ~ 'light',
    fishing_intensity_summer_text == 'verylittle' ~ 'light',
    fishing_intensity_summer_text == 'verylittleornone' ~ 'light',
    TRUE ~ as.character(fishing_intensity_summer_text)
  ))
fishpress_clean$fishing_intensity_summer<-as.factor(fishpress_clean$fishing_intensity_summer)

#winter #want to consolidate into none, light, medium, and heavy - if an in between category listed, go with heavier 
summary(as.factor(fishpress_clean$fishing_intensity_winter_text))

fishpress_clean<-fishpress_clean %>% 
  mutate(fishing_intensity_winter= case_when( 
    fishing_intensity_winter_text == '7' ~ 'heavy', # if ~ then 
    fishing_intensity_winter_text == 'almostnone' ~ 'light',
    fishing_intensity_winter_text == 'average' ~ 'medium',
    fishing_intensity_winter_text == 'cisconettingseason' ~ 'medium',
    fishing_intensity_winter_text == 'common' ~ 'heavy',
    fishing_intensity_winter_text == 'fairlyheavy' ~ 'heavy',
    fishing_intensity_winter_text == 'heavyforpikeandbluegill' ~ 'heavy',
    fishing_intensity_winter_text == 'icefishingforsmeltinwinter' ~ 'medium',
    fishing_intensity_winter_text == 'lightheavyonmudbay' ~ 'heavy',
    fishing_intensity_winter_text == 'lightifany' ~ 'light',
    fishing_intensity_winter_text == 'lightinformationlacking' ~ 'light',
    fishing_intensity_winter_text == 'lightmedium' ~ 'medium',
    fishing_intensity_winter_text == 'lightornone' ~ 'light',
    fishing_intensity_winter_text == 'lightpoor' ~ 'light',
    fishing_intensity_winter_text == 'lightprobablynone' ~ 'light',
    fishing_intensity_winter_text == 'lights' ~ 'light',
    fishing_intensity_winter_text == 'lightspearing' ~ 'light',
    fishing_intensity_winter_text == 'lighttomedium' ~ 'medium',
    fishing_intensity_winter_text == 'lighttonone' ~ 'light',
    fishing_intensity_winter_text == 'lighttonouse' ~ 'light',
    fishing_intensity_winter_text == 'littleatpresent' ~ 'light',
    fishing_intensity_winter_text == 'medium ' ~ 'medium',
    fishing_intensity_winter_text == 'mediumheavy' ~ 'heavy',
    fishing_intensity_winter_text == 'mediumicefishingforsmeltinwinter' ~ 'medium',
    fishing_intensity_winter_text == 'mediumlight' ~ 'medium',
    fishing_intensity_winter_text == 'mediummanylargecrappiestaken' ~ 'medium',
    fishing_intensity_winter_text == 'mediumtoheavy' ~ 'heavy',
    fishing_intensity_winter_text == 'mediumtolight' ~ 'medium',
    fishing_intensity_winter_text == 'moderate' ~ 'medium',
    fishing_intensity_winter_text == 'moderateamount' ~ 'medium',
    fishing_intensity_winter_text == 'moderatetoheavy' ~ 'heavy',
    fishing_intensity_winter_text == 'moderatetolight' ~ 'medium',
    fishing_intensity_winter_text == 'na' ~ NA_character_,
    fishing_intensity_winter_text == 'night' ~ 'light',
    fishing_intensity_winter_text == 'nodata' ~ NA_character_,
    fishing_intensity_winter_text == 'noneatpresent' ~ 'none',
    fishing_intensity_winter_text == 'noneillegal' ~ 'none',
    fishing_intensity_winter_text == 'nonexistent' ~ 'none',
    fishing_intensity_winter_text == 'notaccessible' ~ 'none',
    fishing_intensity_winter_text == 'notknown' ~ NA_character_,
    fishing_intensity_winter_text == 'notmuch' ~ 'light',
    fishing_intensity_winter_text == 'notopen' ~ 'none',
    fishing_intensity_winter_text == 'possiblynone' ~ 'none',
    fishing_intensity_winter_text == 'practicallynone' ~ 'none',
    fishing_intensity_winter_text == 'probablylight' ~ 'light',
    fishing_intensity_winter_text == 'probablynone' ~ 'none',
    fishing_intensity_winter_text == 'quiteheavy' ~ 'heavy',
    fishing_intensity_winter_text == 'reportedlight' ~ 'light',
    fishing_intensity_winter_text == 'someinthespring' ~ 'light',
    fishing_intensity_winter_text == 'springlight' ~ 'light',
    fishing_intensity_winter_text == 'springsuckersbullheads' ~ 'light',
    fishing_intensity_winter_text == 'unaccessible' ~ 'none',
    fishing_intensity_winter_text == 'unknownbutassumedtobeverylight' ~ 'light',
    fishing_intensity_winter_text == 'veryheavy' ~ 'heavy',
    fishing_intensity_winter_text == 'verylight' ~ 'light',
    fishing_intensity_winter_text == 'verylittle' ~ 'light',
    fishing_intensity_winter_text == 'washeavynowclosed' ~ 'heavy',
    TRUE ~ as.character(fishing_intensity_winter_text)
  ))
fishpress_clean$fishing_intensity_winter<-as.factor(fishpress_clean$fishing_intensity_winter)
summary(fishpress_clean$fishing_intensity_winter)

#write.csv(fishpress_clean, 'Lake_SUMM/SUMM_data/clean_data/clean_fish_pressure.csv', row.names = FALSE)

######################################################################## 
#### text_SUMM_new: lake variables ######################## 
######################################################################## 
SUMM_text_new<-read.csv("Lake_SUMM/SUMM_data/text_reducer_SUMM_new.csv") %>%
  select(subject_id, task, 'data.aligned_text', 'data.number_views', 'data.consensus_score','data.consensus_text', URL_front, URL_back) %>%
  drop_na('data.number_views')
SUMM_new_tasks<-read.csv("Lake_SUMM/SUMM_data/SUMM_new_key.csv", header=TRUE)
SUMM_text_new<-left_join(SUMM_text_new, SUMM_new_tasks) # join with the task values to get the actual names

#best way to tackle this is to split up by task (21 tasks)
#https://stackoverflow.com/questions/54311359/use-gsub-remove-all-string-before-first-numeric-character
summary(as.factor(SUMM_text_new$column_name)) #check out all of the tasks 

#*lake characteristics ######################################
lake_char<-filter(SUMM_text_new, column_name == "Area" | column_name == "Max Depth" | column_name == "Area of Vegetation" | 
                      column_name == "Secchi disk " )
#note area of vegetation was changed to 'width of shoal' in some cards
#filter out the ones <50% for manual review 
lake_char_manual<-filter(lake_char, data.consensus_score <= 2)
#write.csv(lake_char_manual, "/Users/katelynking/Desktop/lake_char_manual.csv", row.names = FALSE)

#clean up the good data 
lake_char_good<-filter(lake_char, data.consensus_score > 2)
lake_char_good$data.consensus_text<-tolower(lake_char_good$data.consensus_text) #make all text lowercase
lake_char_good$data2<-gsub("\\[[^][]*]", "", lake_char_good$data.consensus_text) #remove the numbers in brackets at the beginning of some observations that should not be there
lake_char_good$data2<-gsub('none', '0', lake_char_good$data2) # replace words with numbers
lake_char_good$data2<-gsub('one', '1', lake_char_good$data2) # replace words with numbers
lake_char_good$data2<-gsub('nil', '0', lake_char_good$data2) # replace words with numbers
lake_char_good$data2<-gsub('^\\D+', '', x=lake_char_good$data2) #remove anything before the number (eg 'approx' or 'about' ) #^ means start of string, \\D+ means one or more characters other than a digit 
lake_char_good$data2<-gsub('to', '-', x=lake_char_good$data2) # change 'to' to - symbol for ranges
lake_char_good$data2<-gsub("'", 'feet', lake_char_good$data2) # replace ' with feet 
lake_char_good$data2<-gsub("ft\\.", 'feet', lake_char_good$data2) # replace with feet 
lake_char_good$data2<-gsub("ft", 'feet', lake_char_good$data2) # replace with feet 
lake_char_good$data2<-gsub('"', 'inches', lake_char_good$data2) # replace '' with inches 
lake_char_good$data2<-gsub('in\\.', 'inches', lake_char_good$data2) # replace '' with inches 
lake_char_good$data2<-gsub(' 1/2', '.5', lake_char_good$data2) # replace 1/2 with .5 with a space
lake_char_good$data2<-gsub('1/2', '0.5', lake_char_good$data2) # replace 1/2 with .5 without a space
lake_char_good$data2<-gsub('%', 'pct', lake_char_good$data2) # replace % with pct
lake_char_good$data2<-gsub('1/3', '0.33', lake_char_good$data2) # replace fractions 
lake_char_good$data2<-gsub('1/4', '0.25', lake_char_good$data2) # replace fractions
lake_char_good$data2<-gsub('1/10', '0.1', lake_char_good$data2) # replace fractions

#function to convert ft and in to just feet 
ftin2ft <- function(data) {
  feet <- as.numeric(str_extract(data, "(\\d)+(?= feet)"))
  inch <- as.numeric(str_extract(data, "(\\d)+(?= inches)"))
  feet + inch/12
}

#extract data and units 
lake_char_good<- lake_char_good %>% 
  separate(data2,
           into = c("data1", "data2"), #separate ranges first
           sep= "-",   
           extra = 'merge') %>%
  mutate(new_min=round(ftin2ft(data1),3), #function to combine ft and in
         new_max=round(ftin2ft(data2),3)) %>% 
  mutate(min = ifelse(is.na(new_min), (str_extract(data1, regex("[\\d\\,\\.]+"))), new_min))%>% #if new_min is NA then extract the min; a number with commas and decimals ok
  mutate(units1 = str_extract(data1,  regex("[A-Za-z]+"))) %>% #extract min units 
  mutate(max = ifelse(is.na(new_max), (str_extract(data2,  regex("[\\d\\,\\.]+"))), new_max)) %>% #extract the max
  mutate(units2 = str_extract(data2,  regex("[A-Za-z]+"))) #extract max units 

#specific fields noted to still have issues 
lake_char_good$min<-ifelse(lake_char_good$subject_id == 63264425 & lake_char_good$task == 'T14', 8.5, lake_char_good$min) # this one should be 8.5 and not 8.5.5
lake_char_good$min<-gsub(',', '', lake_char_good$min) #remove commas 
lake_char_good$max<-gsub(',', '', lake_char_good$max) #remove commas 
lake_char_good$min<-as.numeric(lake_char_good$min) #change min data column to numeric 
lake_char_good$max<-as.numeric(lake_char_good$max) #change max data column to numeric 
lake_char_good$min<-ifelse(lake_char_good$subject_id == 63265438 & lake_char_good$task == 'T12', 75, lake_char_good$min) # this one should be 75%
lake_char_good$min<-ifelse(lake_char_good$subject_id == 63264918 & lake_char_good$task == 'T8', 2598, lake_char_good$min) # 3.25 miles long x 1.25 miles wide = 4.06 sq miles x 640 = 2598 acres
lake_char_good$min<-ifelse((lake_char_good$subject_id == 63264568 | lake_char_good$subject_id == 63264619 | lake_char_good$subject_id == 63265090 | lake_char_good$subject_id == 63265602) & lake_char_good$task == 'T12', (lake_char_good$min*100), lake_char_good$min) #is a proportion so need to convert to pct  
lake_char_good$min<-ifelse(lake_char_good$subject_id == 63264915 & lake_char_good$task == 'T12', NA, lake_char_good$min) #changed to NA - no information in this field 
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63264633 & lake_char_good$task == 'T12', 3, lake_char_good$max) #should be a range, add in max 
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63264752 & lake_char_good$task == 'T12', 2, lake_char_good$max) #should be a range, add in max 
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63264808 & lake_char_good$task == 'T14', 8, lake_char_good$max) #should be a range, add in max 
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63265122 & lake_char_good$task == 'T8', 3, lake_char_good$max) #should be a range, add in max 
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63265353 & lake_char_good$task == 'T12', 2, lake_char_good$max) #should be a range, add in max 
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63265535 & lake_char_good$task == 'T8', 3, lake_char_good$max) #should be a range, add in max 
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63265647 & lake_char_good$task == 'T12', 10, lake_char_good$max) #should be a range, add in max 
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63265749 & lake_char_good$task == 'T8', 60, lake_char_good$max) #should be a range, add in max 
lake_char_good$min<-ifelse(lake_char_good$subject_id == 63265070 & lake_char_good$task == 'T8', 10.3, lake_char_good$min) #should be in acres  
lake_char_good$units1<-ifelse(lake_char_good$subject_id == 63265070 & lake_char_good$task == 'T8', 'acres', lake_char_good$units1) #should be in acres 

#QA/QC if max is the same or greater than min need to review 
lake_char_good$review<-ifelse(lake_char_good$min >= lake_char_good$max, TRUE, FALSE) 
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63264497 & lake_char_good$task == 'T8', NA, lake_char_good$max) 
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63264502 & lake_char_good$task == 'T14', 10, lake_char_good$max) 
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63264539 & lake_char_good$task == 'T10', 15, lake_char_good$max)  
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63264686 & lake_char_good$task == 'T8', NA, lake_char_good$max) 
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63264752 & lake_char_good$task == 'T14', NA, lake_char_good$max) 
lake_char_good$min<-ifelse(lake_char_good$subject_id == 63264752 & lake_char_good$task == 'T14', 6.5, lake_char_good$min) 
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63264753 & lake_char_good$task == 'T8', NA, lake_char_good$max) 
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63264769 & lake_char_good$task == 'T10', NA, lake_char_good$max) 
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63264804 & lake_char_good$task == 'T14', 3.5, lake_char_good$max)
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63264859 & lake_char_good$task == 'T8', NA, lake_char_good$max)
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63264862 & lake_char_good$task == 'T14', NA, lake_char_good$max)
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63264881 & lake_char_good$task == 'T14', 19.5, lake_char_good$max)
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63265008 & lake_char_good$task == 'T14', 15.5, lake_char_good$max)
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63265156 & lake_char_good$task == 'T8', 10, lake_char_good$max)
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63265220 & lake_char_good$task == 'T12', 10, lake_char_good$max)
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63265319 & lake_char_good$task == 'T8', NA, lake_char_good$max)
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63265350 & lake_char_good$task == 'T8', NA, lake_char_good$max)
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63265370 & lake_char_good$task == 'T10', NA, lake_char_good$max)
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63265383 & lake_char_good$task == 'T14', 12.5, lake_char_good$max)
lake_char_good$min<-ifelse(lake_char_good$subject_id == 63265383 & lake_char_good$task == 'T14', 11.5, lake_char_good$min)
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63265462 & lake_char_good$task == 'T14', NA, lake_char_good$max)
lake_char_good$min<-ifelse(lake_char_good$subject_id == 63265462 & lake_char_good$task == 'T14', 3.75, lake_char_good$min)
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63265475 & lake_char_good$task == 'T14', 2.25, lake_char_good$max)
lake_char_good$min<-ifelse(lake_char_good$subject_id == 63265475 & lake_char_good$task == 'T14', 2.33, lake_char_good$min)
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63265541 & lake_char_good$task == 'T8', 10, lake_char_good$max)
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63265579 & lake_char_good$task == 'T10', NA, lake_char_good$max)
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63265745 & lake_char_good$task == 'T10', NA, lake_char_good$max)
lake_char_good$min<-ifelse(lake_char_good$subject_id == 63265745 & lake_char_good$task == 'T10', 82.5, lake_char_good$min)
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63265792 & lake_char_good$task == 'T8', NA, lake_char_good$max)
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63265830 & lake_char_good$task == 'T8', NA, lake_char_good$max)
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63265863 & lake_char_good$task == 'T14', NA, lake_char_good$max)
lake_char_good$min<-ifelse(lake_char_good$subject_id == 63265863 & lake_char_good$task == 'T14', 7.5, lake_char_good$min)

#several obs got changed to dates... 
lake_char_good$min<-ifelse(lake_char_good$subject_id == 63264448 & lake_char_good$task == 'T12', 1, lake_char_good$min)
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63264448 & lake_char_good$task == 'T12', 2, lake_char_good$max)
lake_char_good$min<-ifelse(lake_char_good$subject_id == 63264496 & lake_char_good$task == 'T12', 1, lake_char_good$min)
lake_char_good$max<-ifelse(lake_char_good$subject_id == 63264496 & lake_char_good$task == 'T12', 2, lake_char_good$max)
lake_char_good$min<-ifelse(lake_char_good$subject_id == 63265614 & lake_char_good$task == 'T12', 0.4, lake_char_good$min)
lake_char_good$min<-ifelse(lake_char_good$subject_id == 63265663 & lake_char_good$task == 'T12', 0.1, lake_char_good$min)
lake_char_good$min<-ifelse(lake_char_good$subject_id == 63265920 & lake_char_good$task == 'T12', 0.33, lake_char_good$min)


# change Secchi disk "to bottom" or "bottom" to the max depth value if available ?


#standardize units ; use summary(as.factor(lake_char_good$units1)) to see all units ; determine unit based on card image or variable
lake_char_good<-lake_char_good %>% 
  mutate(units1 = ifelse(is.na(units1), units2, units1)) %>% #incorporate units2 into units1 column 
  mutate(units= case_when( #standardize units into new column 
    units1 == 'a' ~ 'acres', # if ~ then 
    units1 == 'ac' ~ 'acres',
    units1 == 'acre' ~ 'acres',
    units1 == 'acs' ~ 'acres',
    units1 == 'am' ~ 'acres',
    units1 == 'approx' ~ NA_character_, 
    units1 == 'approximate' ~ 'acres',
    units1 == 'ares' ~ 'acres',
    units1 == 'at' ~ 'acres',
    units1 == 'bot' ~ 'feet',
    units1 == 'by' ~ 'acres',
    units1 == 'confeett' ~ 'feet',
    units1 == 'date' ~ NA_character_,
    units1 == 'est' ~ 'acres', 
    units1 == 'estimated' ~ 'acres', 
    units1 == 'except' ~ 'percent',
    units1 == 'f' ~ 'feet',
    units1 == 'found' ~ 'feet',
    units1 == 'fpct' ~ 'percent', 
    units1 == 'i' ~ 'acres',
    units1 == 'in' ~ 'acres',
    units1 == 'jan' ~ 'acres',
    units1 == 'jun' ~ 'feet',
    units1 == 'kst' ~ 'acres',
    units1 == 'm' ~ 'meters',
    units1 == 'map' ~ NA_character_,
    units1 == 'mapped' ~ NA_character_,
    units1 == 'miles' ~ "acres", 
    units1 == 'moss' ~ NA_character_, 
    units1 == 'observed' ~ 'feet',
    units1 == 'of' ~ 'percent', 
    units1 == 'or' ~ NA_character_, 
    units1 == 'pct' ~ 'percent', 
    units1 == 'per' ~ 'percent', 
    units1 == 'plus' ~ 'feet',
    units1 == 'rough' ~ 'acres',
    units1 == 'seen' ~ 'percent',
    units1 == 'st' ~ NA_character_, #need to remove this card because it has 3 lakes on it 
    units1 == 'surface' ~ 'acres',
    units1 == 'u' ~ 'acres',
    units1 == 'very' ~ NA_character_, 
    units1 == 'volume' ~ 'acres',
    units1 == 'yd' ~ 'yards',
    TRUE ~ as.character(units1)
  )) %>%
  select(subject_id, column_name, min, max, units)

#check again 
summary(as.factor(lake_char_good$units))

#merging back in manually reviewed data 
manual_lake_char<-read.csv("Lake_SUMM/SUMM_data/manual_review_data/lake_char_manual_calla.csv", na.strings = c("", "NA")) %>%
  select(subject_id, column_name, min, max, units) %>% 
  mutate(min = as.numeric(min),
         max=as.numeric(max))

lake_char_clean<-gtools::smartbind(lake_char_good, manual_lake_char)

#change units to final format, need to split out variables
area<-filter(lake_char_clean, column_name == "Area")
veg<-filter(lake_char_clean, column_name == "Area of Vegetation")
depth<-filter(lake_char_clean, column_name == "Max Depth")
secchi<-filter(lake_char_clean, column_name == "Secchi disk ")

area<-area %>% #area on card is in acres
  mutate(units = ifelse(is.na(units), 'acres', units)) %>% #all NAs put to acres 
  mutate(lake_area_min_ha= ifelse(units == 'acres', round(min/2.471,3), min)) %>% #convert acres to ha 
  mutate(lake_area_max_ha= ifelse(units == 'acres', round(max/2.471,3), max)) %>% #convert acres to ha 
  select(subject_id, lake_area_min_ha, lake_area_max_ha)

depth<-depth %>% #depth on card is in feet - might make sense to choose the highest depth as the max, instead of having a range
  mutate(units = ifelse(is.na(units), 'feet', units)) %>% #all NAs put to feet 
  mutate(max_depth_min_m= ifelse(units == 'feet', round(min/3.281,3), min)) %>% #convert feet to meters 
  mutate(max_depth_max_m= ifelse(units == 'feet', round(max/3.281,3), max)) %>% #convert feet to meters 
  mutate(max_depth_min_m= ifelse(units == 'inches', round(min/39.37,3), max_depth_min_m)) %>% #convert inches to meters 
  mutate(max_depth_max_m= ifelse(units == 'inches', round(max/39.37,3), max_depth_max_m)) %>% #convert inches to meters 
  select(subject_id, max_depth_min_m, max_depth_max_m)

secchi<-secchi %>% #depth on card is in feet - might make sense to choose the highest depth as the max, instead of having a range
  mutate(units = ifelse(is.na(units), 'feet', units)) %>% #all NAs put to feet 
  mutate(secchi_min_m= ifelse(units == 'feet', round(min/3.281,3), min)) %>% #convert feet to meters 
  mutate(secchi_max_m= ifelse(units == 'feet', round(max/3.281,3), max)) %>% #convert feet to meters 
  mutate(secchi_min_m= ifelse(units == 'inches', round(min/39.37,3), secchi_min_m)) %>% #convert inches to meters 
  mutate(secchi_max_m= ifelse(units == 'inches', round(max/39.37,3), secchi_max_m)) %>% #convert inches to meters 
  select(subject_id, secchi_min_m, secchi_max_m)

clean_lake_char<-full_join(area, depth) %>%
  full_join(secchi)
#write.csv(clean_lake_char, 'Lake_SUMM/SUMM_data/clean_data/clean_lake_char.csv', row.names = FALSE)
clean_lake_char<-read.csv('Lake_SUMM/SUMM_data/clean_data/clean_lake_char.csv')

#* percent shoal #####################################################
pct_shoal<-filter(SUMM_text_new, column_name == "Percent shoal")
#need to change " all, entire, entire lake, entire pond, all shoal, all of lake, all of pond" to 100 % and "none" to 0 
pct_shoal$data.consensus_text<-tolower(pct_shoal$data.consensus_text) #make all of the words lowercase 
pct_shoal<-pct_shoal %>%
  mutate(data2= case_when( 
    data.consensus_text == 'all' ~ '100', # if ~ then 
    data.consensus_text == 'entire' ~ '100',
    data.consensus_text == 'entire lake' ~ '100',
    data.consensus_text == 'entire pond' ~ '100',
    data.consensus_text == 'all shoal' ~ '100',
    data.consensus_text == 'all of lake' ~ '100',
    data.consensus_text == 'all of pond' ~ '100',
    data.consensus_text == 'no dropoff' ~ '100',
    data.consensus_text == 'none' ~ '0',
    TRUE ~ as.character(data.consensus_text)
  )) 


shoal_good<- filter(pct_shoal, data.consensus_score > 2)
shoal_good$data2<-gsub('^\\D+', '', x=shoal_good$data2) #remove anything before the number (eg 'approx' or 'about' )
shoal_good$data2<-gsub('to', '-', x=shoal_good$data2)
shoal_good<- shoal_good%>%  #next split up the data and the text, which is usually units 
  separate(data2,
           into = c("data", "text"), 
           sep= "%", 
           extra = 'merge') %>%
  separate(data,
           into = c("min", "max"), 
           sep= "-",  #separate ranges 
           extra = 'merge')  

#pull out obs that have anything other than digits 
shoal_good<-transform(shoal_good,
          review = ifelse(grepl("\\D+",shoal_good[,"min"]), 'Yes', 'No')) 
manual_review<-filter(shoal_good, review == "Yes") 

shoal_no_review<-filter(shoal_good, review == "No") %>%
  select(subject_id, column_name, min, max)
shoal_no_review$max<-gsub('40 - 40', '40', x=shoal_no_review$max) #replace one incorrect value

#*manual check consensus scores <=2 and labeled with above review: Elise 
shoal_bad<- filter(pct_shoal, data.consensus_score <= 2) 
#write.csv(shoal_bad, "/Users/katelynking/Desktop/pct_shoal_manual.csv")
#write.csv(manual_review, "/Users/katelynking/Desktop/manual_review.csv")
shoal_bad<-read.csv("Lake_SUMM/SUMM_data/manual_review_data/pct_shoal_manual_eg.csv") %>%
  select(subject_id, column_name, min, max)
manual_review<-read.csv("Lake_SUMM/SUMM_data/manual_review_data/pct_shoal_manual_kk.csv") %>%
  select(subject_id, column_name, min, max)

#merge all data 
pct_shoal_clean<-gtools::smartbind(shoal_no_review, shoal_bad, manual_review)
pct_shoal_clean$min<-as.numeric(pct_shoal_clean$min)
pct_shoal_clean$max<-as.numeric(pct_shoal_clean$max)

clean_shoal_pct<-pivot_wider(pct_shoal_clean, 
                          id_cols = c(subject_id),
                          names_from = column_name ,
                          values_from = c('min','max'),
                          values_fill =  NA) %>%
        rename(min_shoal_pct = 'min_Percent shoal', max_shoal_pct = 'max_Percent shoal')
#write.csv(clean_shoal_pct, 'Lake_SUMM/SUMM_data/clean_data/clean_shoal_pct.csv', row.names = FALSE)

#*boat resorts, cottages, hotels and homes, liveries #####################################################
houses<-filter(SUMM_text_new, column_name == "Cottages" | column_name =="Boat Resorts " | column_name =="Hotels and Homes" | column_name =="Liveries"  )
#change "one" to numeric 1 and "none" to numeric 0 prior to subbing out text 
houses_bad<- filter(houses, data.consensus_score < 2)  #Calla review 
#re-order columns 
houses_bad <- houses_bad[, c(1,3,4,5,6,9,7,8)]
#write.csv(houses_bad, "/Users/katelynking/Desktop/houses_manual.csv", row.names = FALSE)

#clean up the good data 
houses_good<-filter(houses, data.consensus_score >= 2)
houses_good$data.consensus_text<-tolower(houses_good$data.consensus_text) #make all text lowercase
houses_good$data2<-gsub("\\[[^][]*]", "", houses_good$data.consensus_text) #remove the numbers in brackets at the beginning of some observations that should not be there
houses_good$data2<-gsub('none', '0', houses_good$data2) # replace words with numbers
houses_good$data2<-gsub('no', '0', houses_good$data2) # replace words with numbers
houses_good$data2<-gsub('one', '1', houses_good$data2) # replace words with numbers
houses_good$data2<-gsub('two', '2', houses_good$data2) # replace words with numbers
houses_good$data2<-gsub('three', '3', houses_good$data2) # replace words with numbers
houses_good$data2<-gsub('dozen', '12', houses_good$data2) # replace words with numbers
houses_good$data2<-gsub('^\\D+', '', x=houses_good$data2) #remove anything before the number (eg 'approx' or 'about' ) #^ means start of string, \\D+ means one or more characters other than a digit 
houses_good$data2<-gsub('to', '-', x=houses_good$data2) # change 'to' to - symbol for ranges

#extract data and units 
houses_good<- houses_good %>% 
  separate(data2,
           into = c("data1", "data2"), #separate ranges first
           sep= "-",   
           extra = 'merge') %>% 
  mutate(min = str_extract(data1, regex("[\\d\\,\\.]+")), #pull out number keep decimals and commas 
          max = str_extract(data2,  regex("[\\d\\,\\.]+")))

#several obs got changed to dates:
houses_good$min<-ifelse(houses_good$subject_id == 63264421 & houses_good$task == 'T0', 6, houses_good$min) #jun
houses_good$max<-ifelse(houses_good$subject_id == 63264421 & houses_good$task == 'T0', 8, houses_good$max) #jun
houses_good$min<-ifelse(houses_good$subject_id == 63264906 & houses_good$task == 'T0', 6, houses_good$min) #jun 
houses_good$max<-ifelse(houses_good$subject_id == 63264906 & houses_good$task == 'T0', 8, houses_good$max) #jun
houses_good$min<-ifelse(houses_good$subject_id == 63264340 & houses_good$task == 'T0', 8, houses_good$min) #aug
houses_good$max<-ifelse(houses_good$subject_id == 63264340 & houses_good$task == 'T0', 10, houses_good$max) #aug
houses_good$min<-ifelse(houses_good$subject_id == 63265012 & houses_good$task == 'T0', 8, houses_good$min) #aug
houses_good$max<-ifelse(houses_good$subject_id == 63265012 & houses_good$task == 'T0', 10, houses_good$max) #aug
houses_good$min<-ifelse(houses_good$subject_id == 63265946 & houses_good$task == 'T3', 8, houses_good$min) #aug
houses_good$max<-ifelse(houses_good$subject_id == 63265946 & houses_good$task == 'T3', 10, houses_good$max) #aug
houses_good$min<-ifelse(houses_good$subject_id == 63265025 & houses_good$task == 'T0', 10, houses_good$min) #oct
houses_good$max<-ifelse(houses_good$subject_id == 63265025 & houses_good$task == 'T0', 15, houses_good$max) #oct

#check out max values 
houses_good$max<-ifelse(houses_good$subject_id == 63265551 & houses_good$task == 'T1', NA, houses_good$max)
houses_good$max<-ifelse(houses_good$subject_id == 63266022 & houses_good$task == 'T3', NA, houses_good$max)
houses_good$max<-ifelse(houses_good$subject_id == 63265848 & houses_good$task == 'T0', NA, houses_good$max)
houses_good$min<-ifelse(houses_good$subject_id == 63265848 & houses_good$task == 'T0', 26, houses_good$min)
houses_good$max<-ifelse(houses_good$subject_id == 63264601 & houses_good$task == 'T0', NA, houses_good$max)
houses_good$max<-ifelse(houses_good$subject_id == 63264601 & houses_good$task == 'T0', NA, houses_good$max)

#merging back in manually reviewed data 
manual_houses<-read.csv("Lake_SUMM/SUMM_data/manual_review_data/houses_manual_calla.csv", na.strings = c("", "NA")) %>%
  select(subject_id, column_name, min, max)
manual_houses$min<-as.numeric(manual_houses$min)
manual_houses$max<-as.numeric(manual_houses$max)

houses_good$min<-as.numeric(houses_good$min) #change min data column to numeric 
houses_good$max<-as.numeric(houses_good$max) #change max data column to numeric 
houses2<-select(houses_good, subject_id, column_name, min, max)

#merge all data 
houses_clean<-gtools::smartbind(houses2, manual_houses)

clean_houses<-pivot_wider(houses_clean, 
                             id_cols = c(subject_id),
                             names_from = column_name ,
                             values_from = c('min','max'),
                             values_fill =  NA) %>%
  rename(resorts_min_n = 'min_Boat Resorts ', resorts_max_n = 'max_Boat Resorts ', 
         liveries_min_n = 'min_Liveries', liveries_max_n = 'max_Liveries',
         cottages_min_n = 'min_Cottages', cottages_max_n = 'max_Cottages',
         hotels_homes_min_n = 'min_Hotels and Homes', hotels_homes_max_n = 'max_Hotels and Homes')

#write.csv(clean_houses, 'Lake_SUMM/SUMM_data/clean_data/clean_houses.csv', row.names = FALSE)


#*methyl alkalinity  #################################################
alk<-filter(SUMM_text_new,column_name == "Methyl Orange Alk Range") 

alk_bad<- filter(alk, data.consensus_score < 2)  #Kartik review 
#write.csv(alk_bad, "/Users/katelynking/Desktop/alk_manual.csv")

#clean up the good data 
alk_good<-filter(alk, data.consensus_score >= 2)
alk_good$data.consensus_text<-tolower(alk_good$data.consensus_text) #make all text lowercase
alk_good$data.aligned_text<-tolower(alk_good$data.aligned_text) #make all text lowercase
alk_good$notes<-str_extract(alk_good$data.aligned_text, regex("purple")) #add a notes column when methyl purple was used 
alk_good$data2<-gsub("\\[[^][]*]", "", alk_good$data.consensus_text) #remove the numbers in brackets at the beginning of some observations that should not be there
alk_good$data2<-gsub('^\\D+', '', x=alk_good$data2) #remove anything before the number (eg 'approx' or 'about' ) #^ means start of string, \\D+ means one or more characters other than a digit 
alk_good$data2<-gsub('to', '-', x=alk_good$data2) # change 'to' to - symbol for ranges

#extract data and units 
alk_good<- alk_good %>% 
  separate(data2,
           into = c("data1", "data2"), #separate ranges first
           sep= "-",   
           extra = 'merge') %>% 
  mutate(min = str_extract(data1, regex("[\\d\\,\\.]+")), #pull out number keep decimals and commas 
         max = str_extract(data2,  regex("[\\d\\,\\.]+")))

#QA/QC if max is the same or greater than min need to review 
alk_good$min<-as.numeric(alk_good$min) #change min data column to numeric 
alk_good$max<-as.numeric(alk_good$max) #change max data column to numeric 
alk_good$review<-ifelse(alk_good$min >= alk_good$max, TRUE, FALSE) 
alk_good$min<-ifelse(alk_good$subject_id == 63265317, 174, alk_good$min) #values that went to NA after numeric transformation 
alk_good$max<-ifelse(alk_good$subject_id == 63265317, 185, alk_good$max) #values that went to NA after numeric transformation 
alk_good$min<-ifelse(alk_good$subject_id == 63264575, 126, alk_good$min) #values that went to NA after numeric transformation 
alk_good$max<-ifelse(alk_good$subject_id == 63264575, 143, alk_good$max) #values that went to NA after numeric transformation 
alk_good$min<-ifelse(alk_good$subject_id == 63264683, 7, alk_good$min) #values that went to NA after numeric transformation 
alk_good$max<-ifelse(alk_good$subject_id == 63264683, 8, alk_good$max) #values that went to NA after numeric transformation 
alk_good$max<-ifelse(alk_good$subject_id == 63264353, 136, alk_good$max) # max  wrong 
alk_good$max<-ifelse(alk_good$subject_id == 63264383, 2, alk_good$max) # max  wrong 
alk_good$max<-ifelse(alk_good$subject_id == 63264396, 189, alk_good$max) # max  wrong 
alk_good$max<-ifelse(alk_good$subject_id == 63264397, 189, alk_good$max) # max  wrong 
alk_good$max<-ifelse(alk_good$subject_id == 63264501, 15, alk_good$max) # max  wrong 
alk_good$max<-ifelse(alk_good$subject_id == 63264686, 7, alk_good$max) # max  wrong 
alk_good$max<-ifelse(alk_good$subject_id == 63264754, 105, alk_good$max) # max  wrong 
alk_good$max<-ifelse(alk_good$subject_id == 63264797, 152, alk_good$max) # max  wrong 
alk_good$max<-ifelse(alk_good$subject_id == 63264929, 23, alk_good$max) # max  wrong 
alk_good$max<-ifelse(alk_good$subject_id == 63264934, 11, alk_good$max) # max  wrong 
alk_good$max<-ifelse(alk_good$subject_id == 63264983, 108, alk_good$max) # max  wrong 
alk_good$max<-ifelse(alk_good$subject_id == 63265008, 27, alk_good$max) # max  wrong 
alk_good$max<-ifelse(alk_good$subject_id == 63265044, 142, alk_good$max) # max  wrong 
alk_good$max<-ifelse(alk_good$subject_id == 63265260, 120, alk_good$max) # max  wrong 
alk_good$max<-ifelse(alk_good$subject_id == 63265390, 152, alk_good$max) # max  wrong 
alk_good$max<-ifelse(alk_good$subject_id == 63265472, 11, alk_good$max) # max  wrong #note this is in grains caco3 units
alk_good$max<-ifelse(alk_good$subject_id == 63265474, 153.9, alk_good$max) # max  wrong 
alk_good$max<-ifelse(alk_good$subject_id == 63265555, 225, alk_good$max) # max wrong 
alk_good$max<-ifelse(alk_good$subject_id == 63265626, 8.5, alk_good$max) # max  wrong 
alk_good$max<-ifelse(alk_good$subject_id == 63265646, 10, alk_good$max) # max  wrong 
alk_good$max<-ifelse(alk_good$subject_id == 63265777, 131, alk_good$max) # max  wrong 
alk_good$max<-ifelse(alk_good$subject_id == 63265810, 109, alk_good$max) # max  wrong 
alk_good$max<-ifelse(alk_good$subject_id == 63265869, 5, alk_good$max) # max  wrong
alk_good$max<-ifelse(alk_good$subject_id == 63265915, 110, alk_good$max) # max  wrong 
alk_good$max<-ifelse(alk_good$subject_id == 63265980, 113, alk_good$max) # max wrong 


#several obs got changed to dates:
alk_good$min<-ifelse(alk_good$subject_id == 63264316 , 8, alk_good$min) #aug
alk_good$max<-ifelse(alk_good$subject_id == 63264316 , 9, alk_good$max) #aug
alk_good$min<-ifelse(alk_good$subject_id == 63265570 , 12, alk_good$min) #dec
alk_good$max<-ifelse(alk_good$subject_id == 63265570 , 21, alk_good$max) #dec
alk_good$min<-ifelse(alk_good$subject_id == 63265867 , 4, alk_good$min) #dec
alk_good$max<-ifelse(alk_good$subject_id == 63265867 , 5, alk_good$max) #dec

#add units column 
alk_good$units = ifelse(alk_good$subject_id == 63265472, 'gpg', 'ppm') #one observation is in grains per gallon, everything else is ppm 
alk2<-select(alk_good, subject_id, column_name, min, max, units, notes)

#merging back in manually reviewed data 
manual_alk<-read.csv("Lake_SUMM/SUMM_data/manual_review_data/alk_manual_kt.csv", na.strings = c("", "NA")) %>%
  select(subject_id, column_name, min, max, units, notes)

#merge all data 
alk_clean<-gtools::smartbind(alk2, manual_alk)

#standardize units 
alk_clean<-alk_clean%>%
  mutate(units = ifelse(is.na(units), 'ppm', units)) %>% #all NAs put to ppm 
  mutate(methylorange_alk_min_ppm= ifelse(units == 'gpg', round(min*17.1,3), min)) %>% #convert gpg to ppm 
  mutate(methylorange_alk_max_ppm= ifelse(units == 'gpg', round(max*17.1,3), max)) %>% #convert 
  select(subject_id, methylorange_alk_min_ppm, methylorange_alk_max_ppm, notes)

#write.csv(alk_clean, 'Lake_SUMM/SUMM_data/clean_data/clean_alk.csv', row.names = FALSE)


#*shore development index  #############################################
#some wrote in a % which looks like development houses, others wrote an index value for how round it is 
#We want the shoreline development index of a lake. This the ratio of the length of the lake's shoreline to the circumference of a circle with the same area as the lake.
#DL= 1 for perfectly circular lakes and >1 for lakes with complex shapes 
#did not keep text only fields (e.g. "irregular" or "nearly oval" )
# L is the length of the lake's shoreline, and A is the lake's area. The length and area should be measured in the units (e.g., m and m2, or km and km2)
shore_dev<-filter(SUMM_text_new, column_name == "Shore Development")
shore_dev_bad<- filter(shore_dev, data.consensus_score < 2)  #Calla review 
#write.csv(shore_dev_bad, "/Users/katelynking/Desktop/shore_dev_manual.csv", row.names = FALSE)

#clean up the good data 
shore_dev_good<-filter(shore_dev, data.consensus_score >= 2)
shore_dev_good$data.consensus_text<-tolower(shore_dev_good$data.consensus_text) #make all text lowercase
shore_dev_good$data2<-gsub("\\[[^][]*]", "", shore_dev_good$data.consensus_text) #remove the numbers in brackets at the beginning of some observations that should not be there
shore_dev_good$data2<-gsub('none', '0', shore_dev_good$data2) # don't want 0s, but change none to 0 so that 'one' part of none does not change into 1 in the next step 
shore_dev_good$data2<-gsub('one', '1', shore_dev_good$data2) # anything with "nearly one" or "approaching 1" will be captured as 1 
shore_dev_good$data2<-gsub('^\\D+', '', x=shore_dev_good$data2) #remove anything before the number (eg 'approx' or 'about' ) #^ means start of string, \\D+ means one or more characters other than a digit 
shore_dev_good$data2<-gsub('%', 'pct', shore_dev_good$data2) # replace % with pct
lake_char_good$data2<-gsub('1/2', '0.5', lake_char_good$data2) # replace 1/2 with .5 without a space

#split numbers from text, no ranges for this metric 
shore_dev_good<- shore_dev_good %>% 
  mutate(number = str_extract(data2, regex("[\\d\\,\\.]+")), 
          text = str_extract(data2,  regex("[a-z]+"))) 

#specific fields noted to still have issues 
shore_dev_good$number<-ifelse(shore_dev_good$subject_id == 63265070, 1.25, shore_dev_good$number) # checked the card 

summary(as.factor(shore_dev_good$text))
#only want index values
shore_dev_good$number<-gsub(',', '', shore_dev_good$number) #remove commas 
shore_dev_good$number<-as.numeric(shore_dev_good$number)

shore_dev_good<-shore_dev_good %>% 
  mutate(data= case_when( 
    text == 'boat' ~  as.numeric(NA), #one boat access is not an index
    text == 'feet' ~  as.numeric(NA), # feet of campsite  is not an index
    text == 'pct' ~ as.numeric(NA), #dont want percent - we have other metric that ask for houses along shoreline for development 
    number > 10 ~ as.numeric(NA), #anything above 10 without units indicated is probably not an index, most likely pct or something else 
    number == 0 ~ as.numeric(NA), #the index can't be 0 
    TRUE ~ as.numeric(number)
  ))

shore_dev_clean<-select(shore_dev_good, subject_id, data, text)

#read in manual values 
shore_man<-read.csv("Lake_SUMM/SUMM_data/manual_review_data/shore_dev_manual_calla.csv") %>%
  select(subject_id, data, units) %>%
  rename(text = units)
#remove pct values and will remove the 3 lakes 
shore_man<-shore_man %>% 
  mutate(data= case_when( 
    text == 'pct' ~ as.numeric(NA), 
    TRUE ~ as.numeric(data)
  ))


#merge data tables 
shore_merge<-gtools::smartbind(shore_dev_clean, shore_man)

#can use miles provided in some observations and the area of the lake to calculate the index  
shore_merge$data<-ifelse(shore_merge$subject_id == 63264388, round((shore_merge$data*1.609)/(2*(sqrt(3.14*0.9308))),2), shore_merge$data) # convert miles to km then divide by 2sqrt(pi*area). convert area from ha to km2 
shore_merge$data<-ifelse(shore_merge$subject_id == 63265112, round((shore_merge$data*1.609)/(2*(sqrt(3.14*0.40065))),2), shore_merge$data) # convert miles to km then divide by 2sqrt(pi*area). convert area from ha to km2 
shore_merge$data<-ifelse(shore_merge$subject_id == 63265162, round((shore_merge$data*1.609)/(2*(sqrt(3.14*1.79077))),2), shore_merge$data) # convert miles to km then divide by 2sqrt(pi*area). convert area from ha to km2 
shore_merge$data<-ifelse(shore_merge$subject_id == 63265889, round((shore_merge$data*1.609)/(2*(sqrt(3.14*0.58276))),2), shore_merge$data) # convert miles to km then divide by 2sqrt(pi*area). convert area from ha to km2 
#same thing but with feet provided
shore_merge$data<-ifelse(shore_merge$subject_id == 63265320, round((shore_merge$data/3281)/(2*(sqrt(3.14*0.02023))),1), shore_merge$data) #convert feet to km 

clean_shore_dev<-select(shore_merge, subject_id, data) %>% 
  rename(shore_dev_index = data)
#write.csv(clean_shore_dev, 'Lake_SUMM/SUMM_data/clean_data/clean_shore_dev.csv', row.names = FALSE)

#*dam information ########################
#need to split - dam in outlet will be Y/N, effect of fish movement could be Y/N or none, some, yes, height would be numerical  
dams_category<-filter(SUMM_text_new, column_name == "Dam in outlet" | column_name == 'Dam effect on fish movements')
dams_height<-filter(SUMM_text_new, column_name == "Height of dam" )

dams_height_bad<- filter(dams_height, data.consensus_score < 2)  #Elise
dams_category_bad<- filter(dams_category, data.consensus_score < 2) #Calla
#write.csv(dams_height_bad, "/Users/katelynking/Desktop/dams_height_manual.csv", row.names = FALSE)
#write.csv(dams_category_bad, "/Users/katelynking/Desktop/dams_category_manual.csv", row.names = FALSE)

#clean up the good data : categorical use OpenRefineby Justin Schell 
dam_cat_good<-filter(dams_category, data.consensus_score >= 2)
#write.csv(dam_cat_good, "/Users/katelynking/Desktop/dams_category_refine.csv", row.names = FALSE)
#kept original column, but converted answers into a binary "yes/no" dam present and effect on fish 
#any partial or seasonal effect on fish was a yes
#anything x was a no. 
#anything that was unclear or questionable wrote NA because effect is unknown 
dam_cat<-read.csv("Lake_SUMM/SUMM_data/manual_review_data/dams_category_refine_JS.csv", na.strings = c("", "NA")) %>%
  select(subject_id, data.consensus_text, dam_effect_fish, dam_outlet, column_name) %>%
  pivot_wider( id_cols = c(subject_id),
                    names_from = column_name ,
                    values_from = c('data.consensus_text','dam_effect_fish', 'dam_outlet'),
                    values_fill =  NA) %>% 
  select(subject_id, 'dam_outlet_Dam in outlet', 'dam_effect_fish_Dam effect on fish movements', 'data.consensus_text_Dam in outlet', 'data.consensus_text_Dam effect on fish movements' ) %>%
  rename(dam_outlet_text='data.consensus_text_Dam in outlet', dam_effect_fish_text='data.consensus_text_Dam effect on fish movements', 
         dam_outlet='dam_outlet_Dam in outlet', dam_effect_fish='dam_effect_fish_Dam effect on fish movements')

#read in manual data check 
dam_cat_manual<-read.csv("Lake_SUMM/SUMM_data/manual_review_data/dams_category_manual_calla.csv", na.strings = c("", "NA")) %>%
  select(subject_id, data, data_binary, column_name) %>%
  pivot_wider( id_cols = c(subject_id),
               names_from = column_name ,
               values_from = c('data','data_binary'),
               values_fill =  NA) %>%
  rename(dam_outlet_text='data_Dam in outlet', dam_effect_fish_text='data_Dam effect on fish movements', 
         dam_outlet='data_binary_Dam in outlet', dam_effect_fish='data_binary_Dam effect on fish movements')

#want to join and replace NAs from one table with data from another if there, need rqdatatable package
dam_cat_clean<-natural_join(dam_cat, dam_cat_manual, by  = 'subject_id', jointype = "FULL")
#re-order columns 
dam_cat_clean <- dam_cat_clean[, c(1, 4, 2, 5,3)]
#write.csv(dam_cat_clean, 'Lake_SUMM/SUMM_data/clean_data/clean_dam_category.csv', row.names = FALSE)
                       
#clean up the good data : numerical  
dam_height_good<-filter(dams_height, data.consensus_score >= 2)
dam_height_good$data.consensus_text<-tolower(dam_height_good$data.consensus_text) #make all text lowercase
dam_height_good$data2<-gsub("\\[[^][]*]", "", dam_height_good$data.consensus_text) #remove the numbers in brackets at the beginning of some observations that should not be there
dam_height_good$data2<-gsub('none', NA, dam_height_good$data2) 
dam_height_good$data2<-gsub('two', 2, dam_height_good$data2) 
dam_height_good$data2<-gsub('^\\D+', '', x=dam_height_good$data2) #remove anything before the number (eg 'approx' or 'about' ) #^ means start of string, \\D+ means one or more characters other than a digit 
dam_height_good$data2<-gsub('to', '-', x=dam_height_good$data2) # change 'to' to - symbol for ranges
dam_height_good$data2<-gsub("'", 'feet', dam_height_good$data2) # replace ' with feet 
dam_height_good$data2<-gsub("ft\\.", 'feet', dam_height_good$data2) # replace with feet 
dam_height_good$data2<-gsub("ft", 'feet', dam_height_good$data2) # replace with feet 
dam_height_good$data2<-gsub('"', 'inches', dam_height_good$data2) # replace '' with inches 
dam_height_good$data2<-gsub('in\\.', 'inches', dam_height_good$data2) # replace '' with inches 
dam_height_good$data2<-gsub('-1/2', '.5', dam_height_good$data2) # replace 1/2 with .5 
dam_height_good$data2<-gsub('- 1/2', '.5', dam_height_good$data2) # replace 1/2  spaces .5 
dam_height_good$data2<-gsub(' 1/2', '.5', dam_height_good$data2) # replace 1/2 with .5 with a space

#extract data and units 
dam_height_good<- dam_height_good %>% 
  separate(data2,
           into = c("data1", "data2"), #separate ranges first
           sep= "-",   
           extra = 'merge') %>%
  mutate(min = str_extract(data1, regex("[\\d\\,\\.]+")), 
        units1 = str_extract(data1,  regex("[A-Za-z]+")),
        max = str_extract(data2,  regex("[\\d\\,\\.]+")), 
        units2 = str_extract(data2,  regex("[A-Za-z]+"))) 

#several obs need updated 
dam_height_good$min<-ifelse(dam_height_good$subject_id == 63265761, 3.5, dam_height_good$min)
dam_height_good$max<-ifelse(dam_height_good$subject_id == 63265761, 4, dam_height_good$max)
dam_height_good$min<-ifelse(dam_height_good$subject_id == 63264716 , NA, dam_height_good$min) #2feet below lake level does not give height of dam
dam_height_good$min<-ifelse(dam_height_good$subject_id == 63265193, 1.5, dam_height_good$min)

summary(as.factor(dam_height_good$units1))

#standardize units ; use summary(as.factor(lake_char_good$units1)) to see all units ; determine unit based on card image or variable
dam_height_good<-dam_height_good %>% 
  mutate(units= case_when( #standardize units into new column 
    units1 == 'and' ~ 'feet', # if ~ then 
    units1 == 'foot' ~ 'feet',
    TRUE ~ as.character(units1)
  )) %>%
  select(subject_id, column_name, min, max, units)

#check again 
summary(as.factor(dam_height_good$units))
dam_height_good$min<-as.numeric(dam_height_good$min)
dam_height_good$max<-as.numeric(dam_height_good$max)

#merging back in manually reviewed data 
manual_dam_height<-read.csv("Lake_SUMM/SUMM_data/manual_review_data/dams_height_manual_evg.csv", na.strings = c("", "NA")) %>%
  select(subject_id, column_name, min, max, units)
manual_dam_height$min<-as.numeric(manual_dam_height$min)
manual_dam_height$max<-as.numeric(manual_dam_height$max)

dam_height_clean<-gtools::smartbind(dam_height_good, manual_dam_height)

#change units to final format, 
dam_height_clean<-dam_height_clean %>% 
  mutate(units = ifelse(is.na(units), 'feet', units)) %>% #all NAs put to feet 
  mutate(dam_height_min_m= ifelse(units == 'feet', round(min/3.281,3), min)) %>% #convert feet to meters 
  mutate(dam_height_max_m= ifelse(units == 'feet', round(max/3.281,3), max)) %>% #convert feet to meters 
  mutate(dam_height_min_m= ifelse(units == 'inches', round(min/39.37,3), dam_height_min_m)) %>% #convert inches to meters 
  mutate(dam_height_max_m= ifelse(units == 'inches', round(max/39.37,3), dam_height_max_m)) %>% #convert inches to meters 
  select(subject_id, dam_height_min_m, dam_height_max_m)

#write.csv(dam_height_clean, 'Lake_SUMM/SUMM_data/clean_data/clean_dam_height.csv', row.names = FALSE)

#*DO and temp calla #######################################################
#*DO above thermocline, DO below thermocline, DO in thermocline, Temp Surface, Temp Bottom
do_temp<-filter(SUMM_text_new, column_name == "Dissolved oxygen above thermocline" | column_name == "Dissolved oxygen below thermocline" | column_name == 'Dissolved oxygen in thermocline' 
             | column_name == "Temperature_Surface" | column_name == "Temperature_Bottom" )
do_temp_bad<- filter(do_temp, data.consensus_score < 2 & data.number_views >1) #select out where consensus score is <50% and reviewed by more than 1 
#write.csv(do_temp_bad, "/Users/katelynking/Desktop/do_temp_manual.csv", row.names = FALSE)

#clean good data 
do_temp_good<- filter(do_temp, data.consensus_score >= 2) #select out where consensus score is >=50% 

#easier to separate the temp from the DO to be able to standardize units 
temps<-filter(do_temp_good, column_name == "Temperature_Surface" | column_name == "Temperature_Bottom")

temps$data.consensus_text<-tolower(temps$data.consensus_text) #make all text lowercase
temps$data2<-gsub("\\[[^][]*]", "", temps$data.consensus_text) #remove the numbers in brackets at the beginning of some observations that should not be there
temps$data2<-gsub('none', '0', temps$data2) # replace words with numbers
temps$data2<-gsub('^\\D+', '', x=temps$data2) #remove anything before the number (eg 'approx' or 'about' ) #^ means start of string, \\D+ means one or more characters other than a digit 
temps$data2<-gsub('to', '-', x=temps$data2) # change 'to' to - symbol for ranges
temps$data2<-gsub("'", 'feet', temps$data2) # replace ' with feet 
temps$data2<-gsub("ft\\.", 'feet', temps$data2) # replace with feet 
temps$data2<-gsub("ft", 'feet', temps$data2) # replace with feet 
temps$data2<-gsub(' 1/2', '.5', temps$data2) # replace 1/2 with .5 with a space
temps$data2<-gsub('1/2', '0.5', temps$data2) # replace 1/2 with .5 without a space

#extract data and units 
temps<- temps %>% 
  separate(data2,
           into = c("data1", "data2"), #separate ranges first
           sep= "-",   
           extra = 'merge') %>%
  mutate(min = str_extract(data1, regex("[\\d\\,\\.]+"))) %>% #extract the min; a number with commas and decimals ok
  mutate(units1 = str_extract(data1,  regex("[A-Za-z]+"))) %>% #extract min units 
  mutate(max = str_extract(data2,  regex("[\\d\\,\\.]+"))) %>% #extract the max
  mutate(units2 = str_extract(data2,  regex("[A-Za-z]+"))) #extract max units 

summary(as.factor(temps$units1))
#change data to numeric
temps$min<-as.numeric(temps$min) #change min data column to numeric 
temps$max<-as.numeric(temps$max) #change max data column to numeric 

#standardize units ; determine unit based on card image or variable
temps<-temps %>% 
  mutate(units1 = ifelse(is.na(units1), units2, units1)) %>% #incorporate units2 into units1 column 
  mutate(units= case_when( #standardize units into new column 
    units1 == 'a' ~ 'fahrenheit', # if ~ then 
    units1 == 'air' ~ 'fahrenheit',
    units1 == 'am' ~ NA_character_,
    units1 == 'and' ~ 'fahrenheit',
    units1 == 'at' ~ NA_character_, #at means there is also a depth listed. mix of C and F 
    units1 == 'bot' ~ 'fahrenheit',
    units1 == 'c' ~ 'celsius',
    units1 == 'cent' ~ 'celsius',
    units1 == 'centigrade' ~ 'celsius',
    units1 == 'dbr' ~ 'fahrenheit',
    units1 == 'deg' ~ NA_character_, # mix of C and F 
    units1 == 'degrees' ~ NA_character_, # mix of C and F 
    units1 == 'f' ~ 'fahrenheit',
    units1 == 'feet' ~ NA_character_, # mix of C and F 
    units1 == 'in' ~ 'fahrenheit',
    units1 == 'inversion' ~ 'celsius',
    units1 == 'noon' ~ 'fahrenheit',
    units1 == 'see' ~ 'fahrenheit',
    units1 == 'survey' ~ NA_character_,
    units1 == 'taken' ~ NA_character_,
    units1 == 'upper' ~ 'celsius',
    units1 == 'with' ~ 'fahrenheit',
    TRUE ~ as.character(units1)
  )) %>%
  mutate(units= ifelse(is.na(units) & (units2 == 'c' | units2 == 'cent'), 'celsius', units))%>% #if units 2 is Celsius use this 
  mutate(units= ifelse(is.na(units) & units2 == 'f', 'fahrenheit', units)) %>% #if units 2 is Fahrenheit use this 
  mutate(units= ifelse(min < 32, 'celsius', units))  #if the number is <32 then Celsius 
  
#specific fields noted to still have issues - change the value 
temps$min<-ifelse(temps$subject_id == 63265302 & temps$task == 'T23', NA, temps$min) # air temp given, not water temp
temps$max<-ifelse(temps$subject_id == 63265783 & temps$task == 'T23', 45, temps$max) # 
temps$max<-ifelse(temps$subject_id == 63265478 & temps$task == 'T15', NA, temps$max) # 
temps$min<-ifelse(temps$subject_id == 63264659 & temps$task == 'T15', 28.1, temps$min) # 
temps$max<-ifelse(temps$subject_id == 63264666 & temps$task == 'T23', NA, temps$max) # 
temps$max<-ifelse(temps$subject_id == 63265567 & temps$task == 'T15', NA, temps$max) # 
temps$max<-ifelse(temps$subject_id == 63265697 & temps$task == 'T23', NA, temps$max) # 
temps$max<-ifelse(temps$subject_id == 63265971 & temps$task == 'T23', NA, temps$max) # 
temps$min<-ifelse(temps$subject_id == 63265172 & temps$task == 'T23', 53, temps$min) # 
temps$max<-ifelse(temps$subject_id == 63265172 & temps$task == 'T15', 78, temps$max) # 
temps$max<-ifelse(temps$subject_id == 63265793 & temps$task == 'T15', NA, temps$max) # 
temps$max<-ifelse(temps$subject_id == 63264337 & temps$task == 'T15', NA, temps$max) # 
temps$max<-ifelse(temps$subject_id == 63264506 & temps$task == 'T23', NA, temps$max) # 
temps$max<-ifelse(temps$subject_id == 63264770, NA, temps$max) # both tasks should be NA in the max
temps$max<-ifelse(temps$subject_id == 63264850 & temps$task == 'T15', NA, temps$max) # 
temps$max<-ifelse(temps$subject_id == 63264851 & temps$task == 'T15', NA, temps$max) # 
temps$max<-ifelse(temps$subject_id == 63264853 & temps$task == 'T15', NA, temps$max) # 
temps$max<-ifelse(temps$subject_id == 63265058 & temps$task == 'T15', NA, temps$max) # 
temps$max<-ifelse(temps$subject_id == 63265113 & temps$task == 'T15', NA, temps$max) # 
temps$max<-ifelse(temps$subject_id == 63265370, NA, temps$max) # both tasks should be NA in the max
temps$max<-ifelse(temps$subject_id == 63265423 & temps$task == 'T15', NA, temps$max) # 
temps$max<-ifelse(temps$subject_id == 63265425 & temps$task == 'T15', NA, temps$max) # 
temps$max<-ifelse(temps$subject_id == 63265430 & temps$task == 'T15', NA, temps$max) # 
temps$max<-ifelse(temps$subject_id == 63265475 & temps$task == 'T15', NA, temps$max) # 
temps$max<-ifelse(temps$subject_id == 63265483 & temps$task == 'T15', NA, temps$max) # 
temps$min<-ifelse(temps$subject_id == 63265555 & temps$task == 'T23', 71, temps$min) # 
temps$max<-ifelse(temps$subject_id == 63265555 & temps$task == 'T23', NA, temps$max) # 
temps$max<-ifelse(temps$subject_id == 63265840 & temps$task == 'T15', NA, temps$max) # 
temps$max<-ifelse(temps$subject_id == 63265843, NA, temps$max) # both tasks should be NA in the max
temps$max<-ifelse(temps$subject_id == 63265870 & temps$task == 'T15', NA, temps$max) # 
temps$min<-ifelse(temps$subject_id == 63264307 & temps$task == 'T23', 56.6, temps$min) # if feet was listed first, have to change to temp
temps$min<-ifelse(temps$subject_id == 63264308 & temps$task == 'T23', 5.2, temps$min) # 
temps$max<-ifelse(temps$subject_id == 63264308 & temps$task == 'T23', NA, temps$max) # 
temps$min<-ifelse(temps$subject_id == 63264311 & temps$task == 'T23', 56.6, temps$min) # 
temps$min<-ifelse(temps$subject_id == 63264421 & temps$task == 'T23', 11.5, temps$min) # 
temps$min<-ifelse(temps$subject_id == 63264607 & temps$task == 'T23', 50.2, temps$min) # 
temps$max<-ifelse(temps$subject_id == 63264607 & temps$task == 'T23', NA, temps$max) # 
temps$min<-ifelse(temps$subject_id == 63264654 & temps$task == 'T23', 24.2, temps$min) # 
temps$max<-ifelse(temps$subject_id == 63264654 & temps$task == 'T23', NA, temps$max) # 
temps$min<-ifelse(temps$subject_id == 63264683 & temps$task == 'T23', 6.9, temps$min) # 
temps$max<-ifelse(temps$subject_id == 63264683 & temps$task == 'T23', NA, temps$max) # 
temps$min<-ifelse(temps$subject_id == 63264686 & temps$task == 'T23', 6.2, temps$min) # 
temps$max<-ifelse(temps$subject_id == 63264686 & temps$task == 'T23', NA, temps$max) # 
temps$min<-ifelse(temps$subject_id == 63264701 & temps$task == 'T23', 45.3, temps$min) # 
temps$min<-ifelse(temps$subject_id == 63264703 & temps$task == 'T23', 45.3, temps$min) # 
temps$min<-ifelse(temps$subject_id == 63265017 & temps$task == 'T23', 7.2, temps$min) # 
temps$max<-ifelse(temps$subject_id == 63265017 & temps$task == 'T23', NA, temps$max) # 
temps$min<-ifelse(temps$subject_id == 63265070 & temps$task == 'T23', 14.2, temps$min) # 
temps$max<-ifelse(temps$subject_id == 63265070 & temps$task == 'T23', NA, temps$max) # 
temps$min<-ifelse(temps$subject_id == 63265079 & temps$task == 'T23', 18.6, temps$min) # 
temps$max<-ifelse(temps$subject_id == 63265079 & temps$task == 'T23', NA, temps$max) # 
temps$min<-ifelse(temps$subject_id == 63265192 & temps$task == 'T23', 13.6, temps$min) # 
temps$max<-ifelse(temps$subject_id == 63265192 & temps$task == 'T23', NA, temps$max) # 
temps$min<-ifelse(temps$subject_id == 63265357 & temps$task == 'T23', NA, temps$min) # 
temps$min<-ifelse(temps$subject_id == 63265360 & temps$task == 'T23', 49, temps$min) # 
temps$min<-ifelse(temps$subject_id == 63265748 & temps$task == 'T23', 57, temps$min) # 
temps$max<-ifelse(temps$subject_id == 63265748 & temps$task == 'T23', NA, temps$max) # 
temps$min<-ifelse(temps$subject_id == 63265827 & temps$task == 'T23', 22.3, temps$min) # 
temps$max<-ifelse(temps$subject_id == 63265827 & temps$task == 'T23', NA, temps$max) # 
temps$min<-ifelse(temps$subject_id == 63265915 & temps$task == 'T23', 7.7, temps$min) # 
temps$max<-ifelse(temps$subject_id == 63265915 & temps$task == 'T23', NA, temps$max) # 
temps$min<-ifelse(temps$subject_id == 63265946 & temps$task == 'T23', 60, temps$min) # 
temps$max<-ifelse(temps$subject_id == 63265946 & temps$task == 'T23', 64, temps$max) # 
temps$min<-ifelse(temps$subject_id == 63266067 & temps$task == 'T23', 62, temps$min) # 
temps$min<-ifelse(temps$subject_id == 63266078 & temps$task == 'T23', 65, temps$min) # 
temps$min<-ifelse(temps$subject_id == 63266079 & temps$task == 'T23', 13.2, temps$min) # 
temps$max<-ifelse(temps$subject_id == 63266079 & temps$task == 'T23', NA, temps$max) # 
temps$min<-ifelse(temps$subject_id == 63266082 & temps$task == 'T23', 19.5, temps$min) # 
temps$max<-ifelse(temps$subject_id == 63266082 & temps$task == 'T23', NA, temps$max) # 
temps$min<-ifelse(temps$subject_id == 63264915 & temps$task == 'T15', NA, temps$min) # 
temps$min<-ifelse(temps$subject_id == 63265464 & temps$task == 'T15', NA, temps$min) # 
temps$min<-ifelse(temps$subject_id == 63265132 & temps$task == 'T15', NA, temps$min) # date
temps$max<-ifelse(temps$subject_id == 63265132 & temps$task == 'T15', NA, temps$max) # date
temps$min<-ifelse(temps$subject_id == 63265860 & temps$task == 'T15', 71, temps$min) # 
temps$max<-ifelse(temps$subject_id == 63265860 & temps$task == 'T15', NA, temps$max) # 
temps$max<-ifelse(temps$subject_id == 63266073 & temps$task == 'T15', 24.5, temps$max) # 
temps$max<-ifelse(temps$subject_id == 63264407 & temps$task == 'T23', NA, temps$max) # 
temps$max<-ifelse(temps$subject_id == 63264461 & temps$task == 'T15', NA, temps$max) # 

#convert F to C (32°F − 32) × 5/9 = 0°C
temps<-temps %>% #
  mutate(units= ifelse(min < 32, 'celsius', units)) %>%  #if the number is <32 then Celsius 
  mutate(units = ifelse(is.na(units), 'fahrenheit', units)) %>% #all NAs put to Fahrenheit 
  mutate(min_c= ifelse(units == 'fahrenheit', round((min-32)*(5/9),3), min)) %>% #convert Fahrenheit to Celsius 
  mutate(max_c= ifelse(units == 'fahrenheit', round((max-32)*(5/9),3), max)) %>%
  select(subject_id, min_c, max_c, column_name) %>% #select wanted columns
  pivot_wider( id_cols = c(subject_id), #pivot table so each variable is a column 
               names_from = column_name ,
               values_from = c('min_c','max_c'),
               values_fill =  NA) %>%
  rename(temp_surface_min_c='min_c_Temperature_Surface', temp_surface_max_c='max_c_Temperature_Surface', 
         temp_bottom_min_c='min_c_Temperature_Bottom', temp_bottom_max_c='max_c_Temperature_Bottom')

# DO 
do<-filter(do_temp_good, column_name == "Dissolved oxygen above thermocline" | column_name == "Dissolved oxygen below thermocline" | column_name == 'Dissolved oxygen in thermocline')  

do$data.consensus_text<-tolower(do$data.consensus_text) #make all text lowercase
do$data2<-gsub("\\[[^][]*]", "", do$data.consensus_text) #remove the numbers in brackets at the beginning of some observations that should not be there
do$data2<-gsub('none', '0', do$data2) # replace words with numbers
do$data2<-gsub('^\\D+', '', x=do$data2) #remove anything before the number (eg 'approx' or 'about' ) #^ means start of string, \\D+ means one or more characters other than a digit 
do$data2<-gsub('to', '-', x=do$data2) # change 'to' to - symbol for ranges
do$data2<-gsub(';', '-', x=do$data2) #capture more ranges by changing ; to -
do$data2<-gsub("'", 'feet', do$data2) # replace ' with feet 
do$data2<-gsub("ft\\.", 'feet', do$data2) # replace with feet 
do$data2<-gsub("ft", 'feet', do$data2) # replace with feet 
do$data2<-gsub(' 1/2', '.5', do$data2) # replace 1/2 with .5 with a space
do$data2<-gsub('1/2', '0.5', do$data2) # replace 1/2 with .5 without a space

#extract data and units 
do<- do %>% 
  separate(data2,
           into = c("data1", "data2"), #separate ranges first
           sep= "-",   
           extra = 'merge') %>%
  mutate(min = str_extract(data1, regex("[\\d\\,\\.]+"))) %>% #extract the min; a number with commas and decimals ok
  mutate(units1 = str_extract(data1,  regex("[A-Za-z]+"))) %>% #extract min units 
  mutate(max = str_extract(data2,  regex("[\\d\\,\\.]+"))) %>% #extract the max
  mutate(units2 = str_extract(data2,  regex("[A-Za-z]+"))) #extract max units 


#check min and max values to look for outliers 
do$min<-ifelse(do$subject_id == 63265731 & do$task == 'T24', 0.976, do$min) #  #missing decimal 
do$min<-ifelse(do$subject_id == 63265489 & do$task == 'T19', 9.17, do$min) #  #missing decimal 
do$min<-ifelse(do$subject_id == 63265313 & do$task == 'T24', 5.6, do$min) #
do$max<-ifelse(do$subject_id == 63265313 & do$task == 'T24', 1.1, do$max) #
do$min<-ifelse(do$subject_id == 63264451 & do$task == 'T24', NA, do$min) #these are temperatures, not DO 
do$max<-ifelse(do$subject_id == 63264451 & do$task == 'T24', NA, do$max) #these are temperatures, not DO 
do$min<-ifelse(do$subject_id == 63264451 & do$task == 'T20', NA, do$min) #these are temperatures, not DO 
do$max<-ifelse(do$subject_id == 63264451 & do$task == 'T20', NA, do$max) #these are temperatures, not DO 
do$min<-ifelse(do$subject_id == 63264709 & do$task == 'T24', 6.0, do$min) #missing decimal 
do$min<-ifelse(do$subject_id == 63265075 & do$task == 'T20', 0.25, do$min) #missing decimal 
do$min<-ifelse(do$subject_id == 63264714 & do$task == 'T20', 0.5, do$min) #missing decimal 

#change data to numeric
do$min<-as.numeric(do$min) #change min data column to numeric 
do$max<-as.numeric(do$max) #change max data column to numeric 

summary(as.factor(do$units1))
#if units1 is feet, then often depth of DO written first, and DO is in the max column, so switch to DO (prob DO if <10) 
#standardize units ; everything should be in ppm = mg/L 
do$units1 = ifelse(is.na(do$units1), 'ppm', do$units1) #add this so that the next function will work 
do$min<-ifelse((do$units1 == 'feet' & do$min >= 10), do$max, do$min) #this changes a bunch of mins >10 to NA.... need to figure out what to do 

#check weird units -  fields noted to still have issues - change the value 
do$max<-ifelse(do$subject_id == 63264449 & do$task == 'T19', 7.16, do$max) # 
do$max<-ifelse(do$subject_id == 63265807 & do$task == 'T20', 0.0, do$max) # use lowest DO value
do$max<-ifelse(do$subject_id == 63264660 & do$task == 'T19', 4.5, do$max) #
do$max<-ifelse(do$subject_id == 63264690 & do$task == 'T19', 6.7, do$max) # 
do$max<-ifelse(do$subject_id == 63264690 & do$task == 'T20', 0.0, do$max) # 
do$max<-ifelse(do$subject_id == 63264902 & do$task == 'T19', 9.5, do$max) # 
do$max<-ifelse(do$subject_id == 63264986 & do$task == 'T19', 5.0, do$max) # 
do$max<-ifelse(do$subject_id == 63265127 & do$task == 'T19', 1.9, do$max) # 
do$max<-ifelse(do$subject_id == 63265127 & do$task == 'T20', 0.0, do$max) # 
do$max<-ifelse(do$subject_id == 63265130 & do$task == 'T19', 7.1, do$max) # 
do$max<-ifelse(do$subject_id == 63265163 & do$task == 'T19', 7.2, do$max) # 
do$max<-ifelse(do$subject_id == 63265166 & do$task == 'T19', 7.4, do$max) # 
do$max<-ifelse(do$subject_id == 63265720 & do$task == 'T19', 8.5, do$max) # 
do$max<-ifelse(do$subject_id == 63265706 & do$task == 'T19', 4.998, do$max) #
do$max<-ifelse(do$subject_id == 63265739 & do$task == 'T19', 6.4, do$max) # 
do$max<-ifelse(do$subject_id == 63265899 & do$task == 'T19', 4.8, do$max) # 
do$min<-ifelse(do$subject_id == 63264906 & do$task == 'T24', NA, do$min) # these are temperatures
do$max<-ifelse(do$subject_id == 63264906 & do$task == 'T24', NA, do$max) # these are temperatures
do$min<-ifelse(do$subject_id == 63264909 & do$task == 'T24', NA, do$min) # these are temperatures
do$max<-ifelse(do$subject_id == 63264909 & do$task == 'T24', NA, do$max) # these are temperatures
do$max<-ifelse(do$subject_id == 63264603 & do$task == 'T20', 1.8, do$max) #
do$max<-ifelse(do$subject_id == 63264374 & do$task == 'T24', 7.7, do$max) # 
do$max<-ifelse(do$subject_id == 63264374 & do$task == 'T24', 3, do$max) # 
do$max<-ifelse(do$subject_id == 63264719 & do$task == 'T20', NA, do$max) # 
do$min<-ifelse(do$subject_id == 63264719 & do$task == 'T20', 0, do$min) # 
do$max<-ifelse(do$subject_id == 63264729 & do$task == 'T20', 0.8, do$max) # 
do$min<-ifelse(do$subject_id == 63264757 & do$task == 'T20', 7.04, do$min) # 
do$max<-ifelse(do$subject_id == 63264757 & do$task == 'T20', 0.18, do$max) # 
do$max<-ifelse(do$subject_id == 63264758 & do$task == 'T20', 0.6, do$max) #
do$max<-ifelse(do$subject_id == 63264760 & do$task == 'T24', 2.8, do$max) # 
do$min<-ifelse(do$subject_id == 63265017 & do$task == 'T20', 3.4, do$min) # 
do$max<-ifelse(do$subject_id == 63265017 & do$task == 'T20', 0.6, do$max) # 
do$min<-ifelse(do$subject_id == 63265128 & do$task == 'T19', 7.8, do$min) # 
do$min<-ifelse(do$subject_id == 63265327 & do$task == 'T20', 0.8, do$min) # 
do$max<-ifelse(do$subject_id == 63265357 & do$task == 'T20', 0.6, do$max) # 
do$max<-ifelse(do$subject_id == 63265360 & do$task == 'T24', 2.8, do$max) # 
do$min<-ifelse(do$subject_id == 63265642 & do$task == 'T20', 8.2, do$min) # 
do$min<-ifelse(do$subject_id == 63265664 & do$task == 'T20', 7.1, do$min) # 
do$min<-ifelse(do$subject_id == 63265827 & do$task == 'T20', 8.4, do$min) # 
do$max<-ifelse(do$subject_id == 63265827 & do$task == 'T20', 5.4, do$max) #
do$min<-ifelse(do$subject_id == 63265984 & do$task == 'T20', 1.7, do$min) # 
do$max<-ifelse(do$subject_id == 63265098 & do$task == 'T19', NA, do$max) # 
do$min<-ifelse(do$subject_id == 63265098 & do$task == 'T19', 7.5, do$min) # 
do$min<-ifelse(do$subject_id == 63264576 & do$task == 'T19',NA, do$min) # 
do$min<-ifelse(do$subject_id == 63264988 & do$task == 'T20', 3, do$min) # 
do$min<-ifelse(do$subject_id == 63265797 & do$task == 'T24', 3, do$min) # 
do$max<-ifelse(do$subject_id == 63264499 & do$task == 'T19', NA, do$max) # 
do$max<-ifelse(do$subject_id == 63264653 & do$task == 'T24', 0, do$max) # 
do$max<-ifelse(do$subject_id == 63264959 & do$task == 'T20', NA, do$max) # 
do$max<-ifelse(do$subject_id == 63264961 & do$task == 'T19', NA, do$max) # 
do$max<-ifelse(do$subject_id == 63265058 & do$task == 'T19', NA, do$max) # 
do$max<-ifelse(do$subject_id == 63265076 & do$task == 'T20', NA, do$max) # 
do$max<-ifelse(do$subject_id == 63265076 & do$task == 'T24', NA, do$max) # 
do$max<-ifelse(do$subject_id == 63265197 & do$task == 'T19', 9.6, do$max) # check to see that this one has a bottom DO
do$max<-ifelse(do$subject_id == 63265240 & do$task == 'T19', NA, do$max) # 
do$max<-ifelse(do$subject_id == 63265617 & do$task == 'T24', 1.9, do$max) # 
do$max<-ifelse(do$subject_id == 63265627 & do$task == 'T19', 6.92, do$max) # 
do$max<-ifelse(do$subject_id == 63265629 & do$task == 'T19', 4.73, do$max) # 
do$max<-ifelse(do$subject_id == 63265707 & do$task == 'T19', 7.95, do$max) # 
do$max<-ifelse(do$subject_id == 63265955 & do$task == 'T19', NA, do$max) # 
do$max<-ifelse(do$subject_id == 63265970 & do$task == 'T19', 7.5, do$max) # 
do$min<-ifelse(do$subject_id == 63265697 & do$task == 'T20', 0, do$min) # 
do$max<-ifelse(do$subject_id == 63264729 & do$task == 'T19', 3.85, do$max) # 
do$max<-ifelse(do$subject_id == 63265327 & do$task == 'T19', 3.85, do$max) # 
do$max<-ifelse(do$subject_id == 63265735 & do$task == 'T19', 7.75, do$max) # 
do$max<-ifelse(do$subject_id == 63266061 & do$task == 'T19', 0.9, do$max) # 
do$max<-ifelse(do$subject_id == 63264513 & do$task == 'T19', 8.3, do$max) # 
do$min<-ifelse(do$subject_id == 63264971 & do$task == 'T20', NA, do$min) # 
do$max<-ifelse(do$subject_id == 63264418 & do$task == 'T20', NA, do$max) # 
do$min<-ifelse(do$subject_id == 63265874 & do$task == 'T24', 9, do$min) # got chagned to date
do$max<-ifelse(do$subject_id == 63265874 & do$task == 'T24', 5, do$max) # got changed to date
do$max<-ifelse(do$subject_id == 63265930 & do$task == 'T19', NA, do$max) # got changed to date
do$min<-ifelse(do$subject_id == 63264339 & do$task == 'T19', 8.4, do$min) #check the min NAs with bot in the units1 
do$min<-ifelse(do$subject_id == 63266011 & do$task == 'T19', 11.2, do$min) #check the min NAs with bot in the units1 
do$max<-ifelse(do$subject_id == 63266011 & do$task == 'T19', NA, do$max) 
do$min<-ifelse(do$subject_id == 63266049 & do$task == 'T19', 8.2, do$min) #check the min NAs with bot in the units1 

#63266011: need a new row - the "above thermocline" question contains a bottom DO values 
do<-do %>% 
  tibble::add_row(subject_id = as.integer(63266011), column_name = "Dissolved oxygen below thermocline", min=3.4)

do<-select(do, subject_id, min, max, column_name) %>% #select wanted columns
  pivot_wider( id_cols = c(subject_id), #pivot table so each variable is a column 
               names_from = column_name ,
               values_from = c('min','max'),
               values_fill =  NA) %>%
  rename(do_above_thermo_min_ppm='min_Dissolved oxygen above thermocline', do_above_thermo_max_ppm='max_Dissolved oxygen above thermocline', 
         do_in_thermo_min_ppm='min_Dissolved oxygen in thermocline', do_in_thermo_max_ppm='max_Dissolved oxygen in thermocline', 
         do_below_thermo_min_ppm='min_Dissolved oxygen below thermocline', do_below_thermo_max_ppm='max_Dissolved oxygen below thermocline', )

#merge temperature and DO tables
do_temp_both<-full_join( temps, do)
#re-order columns 
do_temp_both <- do_temp_both[, c(1,2, 4, 3,5, 6,9,8,11,7,10)]

#read and add in manual values 
do_temp_manual<-read.csv("Lake_SUMM/SUMM_data/manual_review_data/do_temp_manual_calla.csv", na.strings = c("", "NA")) %>%
  select(subject_id, column_name, min, max, units) 

#separate do and temp so that we can standardize units 
temps_manual<-filter(do_temp_manual, column_name == "Temperature_Surface" | column_name == "Temperature_Bottom")
do_manual<-filter(do_temp_manual, column_name == "Dissolved oxygen above thermocline" | column_name == "Dissolved oxygen below thermocline" | column_name == 'Dissolved oxygen in thermocline')  

#convert F to C (32°F − 32) × 5/9 = 0°C
temps_manual<-temps_manual %>% # 
  mutate(units= ifelse(min < 32, 'C', units)) %>%  #if the number is <32 then Celsius 
  mutate(units = ifelse(is.na(units), 'F', units)) %>% #all NAs put to Fahrenheit 
  mutate(min_c= ifelse(units == 'F', round((min-32)*(5/9),3), min)) %>% #convert Fahrenheit to Celsius 
  mutate(max_c= ifelse(units == 'F', round((max-32)*(5/9),3), max)) %>%
  select(subject_id, min_c, max_c, column_name) %>% #select wanted columns
  pivot_wider( id_cols = c(subject_id), #pivot table so each variable is a column 
               names_from = column_name ,
               values_from = c('min_c','max_c'),
               values_fill =  NA) %>%
  rename(temp_surface_min_c='min_c_Temperature_Surface', temp_surface_max_c='max_c_Temperature_Surface', 
         temp_bottom_min_c='min_c_Temperature_Bottom', temp_bottom_max_c='max_c_Temperature_Bottom')

do_manual<-select(do_manual, subject_id, min, max, column_name) %>% #select wanted columns
  pivot_wider( id_cols = c(subject_id), #pivot table so each variable is a column 
               names_from = column_name ,
               values_from = c('min','max'),
               values_fill =  NA) %>%
  rename(do_above_thermo_min_ppm='min_Dissolved oxygen above thermocline', do_above_thermo_max_ppm='max_Dissolved oxygen above thermocline', 
         do_in_thermo_min_ppm='min_Dissolved oxygen in thermocline', do_in_thermo_max_ppm='max_Dissolved oxygen in thermocline', 
         do_below_thermo_min_ppm='min_Dissolved oxygen below thermocline', do_below_thermo_max_ppm='max_Dissolved oxygen below thermocline', )

#merge temperature and DO tables
do_temp_both_manual<-full_join( temps_manual, do_manual)

#want to join and replace NAs from one table with data from another if there, need rqdatatable package
do_temp_clean<-natural_join(do_temp_both, do_temp_both_manual, by  = 'subject_id', jointype = "FULL")

#re-order columns 
do_temp_clean <- do_temp_clean[, c(1,10, 11, 8, 9, 2,3,6,7,4,5)]


#write.csv(do_temp_clean, 'Lake_SUMM/SUMM_data/clean_data/clean_do_temps.csv', row.names = FALSE)

#*thermocline  ####################################################          
thermocline<-filter(SUMM_text_new, column_name == "Thermocline_Present" | column_name == "Thermocline_location")
thermocline_bad<- filter(thermocline, data.consensus_score < 2)
#write.csv(thermocline_bad, "/Users/katelynking/Desktop/thermocline_manual.csv", row.names = FALSE)

#separate good categorical and numerical data 
thermocline_good<-filter(thermocline, data.consensus_score >= 2)

#standardize categorical to yes or no for thermocline present
thermocline_present<-filter(thermocline_good, column_name == 'Thermocline_Present')
thermocline_present$data.consensus_text<-tolower(thermocline_present$data.consensus_text) #make all text lowercase
thermocline_present$data2<-gsub("\\[[^][]*]", "", thermocline_present$data.consensus_text) #remove the numbers in brackets at the beginning of some observations that should not be there
thermocline_present$data2<-gsub('none', 'no', thermocline_present$data2) # no is desired outcome 
thermocline_present$data2<-gsub('none.', 'no', thermocline_present$data2) # no is desired outcome 
thermocline_present$data2<-gsub("yes.*","yes",thermocline_present$data2) #if yes is followed by any other characters, change to yes 
thermocline_present$data2<-gsub("no.*","no",thermocline_present$data2) #if yes is followed by any other characters, change to yes 

summary(as.factor(thermocline_present$data2))
#change other random answers to yes or no 
thermocline_present<-thermocline_present %>%
  mutate(data2= case_when( 
    data2 == ' yes' ~ 'yes', # if ~ then 
    data2 == '-' ~ 'no',
    data2 == '--' ~ 'no',
    data2 == '?' ~ as.character(NA),
    data2 == '1' ~ 'yes',
    data2 == '15 ft' ~ 'yes',
    data2 == "15' - 17.1*c. - __ 18' - 14.5*c c" ~ 'yes',
    data2 == "21' to 30'" ~ 'yes',
    data2 == "couldn't be stratified" ~ 'no',
    data2 == "in no" ~ 'yes',
    data2 == "in very limited area in 1940." ~ 'yes',
    data2 == "possibly the beginning of one" ~ 'yes',
    data2 == "probably never formed" ~ 'no',
    data2 == "probably no" ~ 'no',
    data2 == "rather rapid drop in temp. 9.1* in 12 inches" ~ 'yes', #this one will need to add a location at 12 in
    data2 == "slight thermocline" ~ 'yes',
    data2 == "technically, yes" ~ 'yes',
    TRUE ~ as.character(data2)
  )) %>%
  select(subject_id, data2) %>%
  rename(thermocline_present=data2)

summary(as.factor(thermocline_present$data2))

#clean numerical values
thermocline_location<-filter(thermocline_good, column_name == 'Thermocline_location')
thermocline_location$data.consensus_text<-tolower(thermocline_location$data.consensus_text) #make all text lowercase
thermocline_location$data2<-gsub("\\[[^][]*]", "", thermocline_location$data.consensus_text) #remove the numbers in brackets at the beginning of some observations that should not be there
thermocline_location$data2<-gsub('^\\D+', '', x=thermocline_location$data2) #remove anything before the number (eg 'approx' or 'about' ) #^ means start of string, \\D+ means one or more characters other than a digit 
thermocline_location$data2<-gsub('to', '-', x=thermocline_location$data2) # change 'to' to - symbol for ranges
thermocline_location$data2<-gsub('and', '-', x=thermocline_location$data2) # change 'to' to - symbol for ranges
thermocline_location$data2<-gsub("'", 'feet', thermocline_location$data2) # replace ' with feet  
thermocline_location$data2<-gsub("ft", 'feet', thermocline_location$data2) # replace with feet 
thermocline_location$data2<-gsub('"', 'inches', thermocline_location$data2) # replace '' with inches 
thermocline_location$data2<-gsub(' 1/2', '.5', thermocline_location$data2) # replace 1/2 with .5 with a space
thermocline_location$data2<-gsub('-1/2', '.5', thermocline_location$data2) # replace 1/2 with .5 with a space

#extract data and units 
thermocline_location<- thermocline_location %>% 
  separate(data2,
           into = c("data1", "data2"), #separate ranges first
           sep= "-",   
           extra = 'merge') %>%
  mutate(min = str_extract(data1, regex("[\\d\\,\\.]+"))) %>% #if new_min is NA then extract the min; a number with commas and decimals ok
  mutate(units1 = str_extract(data1,  regex("[A-Za-z]+"))) %>% #extract min units 
  mutate(max = str_extract(data2,  regex("[\\d\\,\\.]+"))) %>% #extract the max
  mutate(units2 = str_extract(data2,  regex("[A-Za-z]+"))) #extract max units 

#QA/QC if max is the same or greater than min need to review 
thermocline_location$min<-as.numeric(thermocline_location$min) #change min data column to numeric 
thermocline_location$max<-as.numeric(thermocline_location$max) #change max data column to numeric 
thermocline_location$review<-ifelse(thermocline_location$min >= thermocline_location$max, TRUE, FALSE) 
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63264393, 25, thermocline_location$max) 
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63264444, 21, thermocline_location$max) 
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63264445, 12, thermocline_location$max) 
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63264480, 12, thermocline_location$max) 
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63264502, 18, thermocline_location$max) 
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63264511, 18, thermocline_location$max) 
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63264553, 10, thermocline_location$max) 
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63264631, 14, thermocline_location$max) 
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63264633, 15, thermocline_location$max) 
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63264665, 19, thermocline_location$max) 
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63264719, 12, thermocline_location$max) 
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63264742, 12, thermocline_location$max) 
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63264812, 18, thermocline_location$max) 
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63264823, 25, thermocline_location$max) 
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63265082, 18, thermocline_location$max) 
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63265115, 18, thermocline_location$max) 
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63265124, 23, thermocline_location$max) 
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63265223, 21, thermocline_location$max) 
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63265270, 30, thermocline_location$max) 
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63265313, 20, thermocline_location$max) 
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63265376, 22.5, thermocline_location$max) 
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63265533, 33, thermocline_location$max) 
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63265690, 18, thermocline_location$max) 
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63265840, NA, thermocline_location$max) 
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63265950, 20, thermocline_location$max) 

#several obs got changed to dates... 
thermocline_location$min<-ifelse(thermocline_location$subject_id == 63266009, 12, thermocline_location$min)
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63266009, 20, thermocline_location$max) 
thermocline_location$min<-ifelse(thermocline_location$subject_id == 63265307, 10, thermocline_location$min)
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63265307, 20, thermocline_location$max) 
thermocline_location$min<-ifelse(thermocline_location$subject_id == 63265309, 10, thermocline_location$min)
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63265309, 20, thermocline_location$max) 
thermocline_location$min<-ifelse(thermocline_location$subject_id == 63265317, 10, thermocline_location$min)
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63265317, 20, thermocline_location$max) 
thermocline_location$min<-ifelse(thermocline_location$subject_id == 63265387, 10, thermocline_location$min)
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63265387, 32, thermocline_location$max) 
thermocline_location$min<-ifelse(thermocline_location$subject_id == 63265814, 10, thermocline_location$min)
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63265814, 22, thermocline_location$max) 
thermocline_location$min<-ifelse(thermocline_location$subject_id == 63265731, 9, thermocline_location$min)
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63265731, 18, thermocline_location$max) 

summary(as.factor(thermocline_location$units1)) # look at weird units to find observations to be corrected
#several other observations found to be incorrect 
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63264836, NA, thermocline_location$max) #listed as 4-6 ppm, which is not a location 
thermocline_location$min<-ifelse(thermocline_location$subject_id == 63264836, NA, thermocline_location$min) #listed as 4-6 ppm, which is not a location 
thermocline_location$max<-ifelse(thermocline_location$subject_id == 63264369, NA, thermocline_location$max) #should be to bottom 
thermocline_location$min<-ifelse(thermocline_location$subject_id == 63265876, NA, thermocline_location$min) #listed as a temperature, which is not a location  

#standardize units to meters 
thermocline_location<-thermocline_location %>%
  mutate(units= case_when( 
    units1 == 'c' ~ 'feet',  # if ~ then 
    units1 == 'meter' ~ 'meters',
    units1 == 'or' ~ 'feet',
    TRUE ~ as.character(units1)
  )) 
summary(as.factor(thermocline_location$units))

#convert feet to meters
thermocline_location<-thermocline_location %>% # on card is in feet 
  mutate(units = ifelse(is.na(units), 'feet', units)) %>% #all NAs put to feet 
  mutate(thermocline_loc_min_m= ifelse(units == 'feet', round(min/3.281,3), min)) %>% #convert feet to meters 
  mutate(thermocline_loc_max_m= ifelse(units == 'feet', round(max/3.281,3), max)) %>%
  select(subject_id, thermocline_loc_min_m, thermocline_loc_max_m, units2) #select wanted columns

#if units2 has 'bot', then the thermocline goes to the bottom
#change to bottom to the max depth value 
depth<-read.csv("Lake_SUMM/SUMM_data/clean_data/clean_lake_char.csv") %>% 
  select(subject_id, max_depth_min_m)
thermo_depth<-left_join(thermocline_location, depth)

thermo_depth<-thermo_depth %>% 
  mutate(thermocline_loc_max_m = ifelse(units2 == 'bot' & is.na(thermocline_loc_max_m), max_depth_min_m, thermocline_loc_max_m)) %>% #if units2 has 'bot', then change to the max depth value
  mutate(flag_bottom = ifelse(units2 == 'bot', "yes", NA)) %>% 
  select(subject_id, thermocline_loc_min_m, thermocline_loc_max_m, flag_bottom)
         
#read in manual values 
thermocline_manual_num<-read.csv("Lake_SUMM/SUMM_data/manual_review_data/thermocline_manual_kt.csv", na.strings = c("", "NA")) %>%
  select(subject_id, column_name, location_min, location_max, units) %>%
  filter(column_name == "Thermocline_location") %>%
  rename(min=location_min, max=location_max)

#convert feet to meters 
thermocline_manual_num$min<-as.numeric(thermocline_manual_num$min) #change max data column to numeric 
thermocline_manual_num$max<-as.numeric(thermocline_manual_num$max) #change max data column to numeric 
thermocline_manual_num<-thermocline_manual_num %>% # on card is in feet 
  mutate(units = ifelse(is.na(units), 'feet', units)) %>% #all NAs put to feet 
  mutate(thermocline_loc_min_m= ifelse(units == 'feet', round(min/3.281,3), min)) %>% #convert feet to meters 
  mutate(thermocline_loc_max_m= ifelse(units == 'feet', round(max/3.281,3), max)) %>% 
  select(subject_id, thermocline_loc_min_m, thermocline_loc_max_m)

#merge numeric values 
thermocline_loc_clean<-gtools::smartbind(thermo_depth, thermocline_manual_num)
                                         
#merge categorical values 
thermocline_manual_cat<-read.csv("Lake_SUMM/SUMM_data/manual_review_data/thermocline_manual_kt.csv", na.strings = c("", "NA")) %>%
  select(subject_id, column_name, location_min) %>%
  filter(column_name == "Thermocline_Present") %>%
  rename(thermocline_present=location_min) %>%
  select(subject_id, thermocline_present)

thermocline_pres_clean<-gtools::smartbind(thermocline_present, thermocline_manual_cat)


#merge thermocline location and thermocline present tables
thermocline_both<-full_join(thermocline_loc_clean, thermocline_pres_clean)

#write.csv(thermocline_both, 'Lake_SUMM/SUMM_data/clean_data/clean_thermocline.csv', row.names = FALSE)


#### merge all tables and join with new_keys and comments and tags #### 
dates<-read.csv("Lake_SUMM/SUMM_data/clean_data/clean_dates.csv")
lake_char<-read.csv("Lake_SUMM/SUMM_data/clean_data/clean_lake_char.csv")
shoal<-read.csv("Lake_SUMM/SUMM_data/clean_data/clean_shoal_pct.csv")
houses<-read.csv("Lake_SUMM/SUMM_data/clean_data/clean_houses.csv")
alk<-read.csv("Lake_SUMM/SUMM_data/clean_data/clean_alk.csv")
shore_dev<-read.csv("Lake_SUMM/SUMM_data/clean_data/clean_shore_dev.csv")
dam_cat<-read.csv("Lake_SUMM/SUMM_data/clean_data/clean_dam_category.csv")
dam_height<-read.csv("Lake_SUMM/SUMM_data/clean_data/clean_dam_height.csv")
do_temp<-read.csv("Lake_SUMM/SUMM_data/clean_data/clean_do_temps.csv")
thermo<-read.csv("Lake_SUMM/SUMM_data/clean_data/clean_thermocline.csv")
fish_pressure<-read.csv("Lake_SUMM/SUMM_data/clean_data/clean_fish_pressure.csv")

# read in new_key info from txt file so that the new_key does not change to a date 
new_keys<-read.delim("Lake_SUMM/SUMM_data/SUMM_lakematch_4JAN22.txt",  sep = ",",  na.strings = c("", "NA"), header = TRUE) %>%
  select(subject_id, county, lakename, New_Key) %>%
  rename(new_key = New_Key)
#read in subject ids - original ids used for basic workflow, and then separate workflow used for new vs old cards so additional subject id created 
text_subject<-read.csv('Lake_SUMM/SUMM_data/text_subject_ids_new.csv')

lake_summary<-left_join(dates, text_subject, by=c("subject_id" = "subject_id_match")) %>% #this should match the subject to dates
  left_join(lake_char, by=c("subject_id.y" = "subject_id")) %>% # this should match by the second subject id 
  left_join(shoal, by=c("subject_id.y" = "subject_id")) %>% 
  left_join(houses, by=c("subject_id.y" = "subject_id")) %>% 
  left_join(alk, by=c("subject_id.y" = "subject_id")) %>% 
  left_join(shore_dev, by=c("subject_id.y" = "subject_id"))  %>% 
  left_join(dam_cat, by=c("subject_id.y" = "subject_id")) %>% 
  left_join(dam_height, by=c("subject_id.y" = "subject_id"))  %>% 
  left_join(do_temp, by=c("subject_id.y" = "subject_id")) %>% 
  left_join(thermo, by=c("subject_id.y" = "subject_id"))  %>% 
  left_join(fish_pressure, by=c("subject_id.y" = "subject_id")) %>% 
  left_join(new_keys) %>%
  select(-c(subject_id.y)) %>% #remove secondary subject id 
  rename(flag_alk = notes)
#re-order columns 
lake_summary <- lake_summary[, c(1,56,55,54,2,3,4,5,6,7,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,
                                 40,41,42,43,44,45,46,47,48,49,50,51,52,53,8,9)]

#remove subjects 63264926 (58687958), 63265573 (59071125), 63265689 (59071255),  and 63265642 (59071199) because they have 2 or 3 lakes on one card. 
lake_summary<-lake_summary[!(lake_summary$subject_id=="58687958" | lake_summary$subject_id=="59071125" | lake_summary$subject_id=="59071255" | lake_summary$subject_id=="59071199"),]
#write final dataset 
#write.csv(lake_summary, 'Lake_SUMM/SUMM_data/clean_data/lake_summary_qaqc.csv', row.names = FALSE)



