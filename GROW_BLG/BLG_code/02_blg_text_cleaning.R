### BLUEGILL GROW CARDS ###
## written by Katelyn King 

#### load libraries #### 
library(dplyr)
library(tidyr)
library(stringr)
library(rqdatatable)

#read in general data 
#read in dates from basic workflow   
grow_dates <-read.csv("GROW_general/grow_dates.csv", header=TRUE, na.strings = c("", "NA"))

#read in urls 
urls<-read.csv("urls/basic_GROW_URLs.csv", na.strings = c("", "NA"))  %>% 
  distinct(subject_id, .keep_all = TRUE) %>% 
  rename(url_front = URL_front, url_back = URL_back)

#task values 
grow_task_values<-read.csv("GROW_BLG/BLG_data/GROW_task_values.csv", header=TRUE) %>% 
  select(task, column_name, old_name) %>%
  mutate(old_name = trimws(old_name, which = c("both"))) #trim white space 

#read in lake match data from Faye and Michael's work  
lake_match <-read.csv("GROW_general/grow_card_lake_match_13Sep2021.csv", header=TRUE, na.strings = c("", "NA")) %>%
  select(subject_id, county, lakename, 'new.key') %>%
  rename(new_key = 'new.key') %>%
  drop_na('new_key')


#### blgrow_text (length info) cleaning ####
blgrow_text<-read.csv("GROW_BLG/BLG_data/text_reducer_bluegill_texts.csv") %>%
  select(subject_id, task, 'data.aligned_text', 'data.number_views', 'data.consensus_score','data.consensus_text') %>%
  drop_na('data.number_views')%>%
  left_join(grow_task_values) # join with the task values to get the actual names


#remove string after // 
blgrow_text$data<-str_remove(blgrow_text$data.consensus_text, "//.*")
#remove one weird answer '3 six year old samples' should say 'no six year old samples' 
blgrow_text$data<-ifelse(blgrow_text$data =='3 six year old samples', 0, blgrow_text$data) 
#cards are in inches unless otherwise noted #keep only units, get rid of extra stuff 
#summary(as.factor(blgrow_text$unit)) # check out all of the things in units, don't want anything in parenthesis which are weights. looked at several cards where weird things are happening   
blgrow_text<-blgrow_text %>%
  separate(data, c("data", "units1"), sep = " ") %>% #new column for units 
           separate(data,
           into = c("data", "text"), 
           sep= "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])") %>% #separate numbers from text
            mutate(units= case_when(
              units1 == 'in' ~ 'inches', # if ~ then 
              units1 == 'in.' ~ 'inches', # if ~ then 
              units1 == 'inches' ~ 'inches',
              units1 == 'mm' ~ 'mm',
              units1 == 'cm' ~ 'cm',
            text=="mm" ~ "mm"
            )) #else be NA if its not any of these conditions 

#*manual check consensus scores <=2 #### 
blg_text_bad1<- filter(blgrow_text, data.number_views == 1) # what was viewed by only 1 person - if only one person wrote an answer then that means 3 people left blank
#what was viewed by >= 2 people but half didnt agree 
blg_text_bad2<- filter(blgrow_text, (data.number_views >2 & data.consensus_score <= 2)| (data.number_views ==2 & data.consensus_score < 2) ) 

#good data where consensus is >50% 
blg_text_good<-filter(blgrow_text, (data.number_views >2 & data.consensus_score > 2)| (data.number_views ==2 & data.consensus_score >= 2) ) %>%
            select(subject_id, column_name, data)

#manual review by Calla
#text_review<-left_join(blg_text_bad2,urls ) %>% 
 # select(subject_id, 'data.aligned_text', 'data.consensus_text', column_name, data, units, URL_front, URL_back)
#write.csv(text_review, "/Users/katelynking/Desktop/text_review.csv", row.names = FALSE)

#manual review by KK 
#write.csv(blg_text_bad1, "/Users/katelynking/Desktop/blg_text_bad1.csv", row.names = FALSE)

#merging back in manually reviewed data 
manual_length1<-read.csv("GROW_BLG/BLG_data/manual_review_data/text_review_calla.csv", na.strings = c("", "NA")) %>%
  mutate(column_name = trimws(column_name, which = c("both"))) %>% #trim white space 
  left_join(grow_task_values, by=c("column_name" = "old_name")) %>%
  select(subject_id, column_name.y, data)  %>% 
  rename(column_name = column_name.y)

manual_length2<-read.csv("GROW_BLG/BLG_data/manual_review_data/blg_text_bad1_KK.csv", na.strings = c("", "NA")) %>%
  select(subject_id, column_name, data)%>%
  mutate(column_name = trimws(column_name, which = c("both"))) %>% #trim white space 
  left_join(grow_task_values, by=c("column_name" = "old_name")) %>%
  select(subject_id, column_name.y, data)  %>% 
  rename(column_name = column_name.y)

lengths_clean<-gtools::smartbind(blg_text_good, manual_length1, manual_length2) %>%
             pivot_wider( #rotate so that each of the tasks is a column 
                               id_cols = c(subject_id),
                               names_from = column_name, 
                               values_from = c(data), 
                               values_fill = list(data = NA) )

#### blgrow_dropdown cleaning #### 
#read in files
blgrow_dropdown<-read.csv("GROW_BLG/BLG_data/dropdown_reducer_bluegill_texts_dropdowns.csv", na.strings = c("", "NA")) %>%
  select(subject_id, task, 'data.value') %>%
  drop_na('data.value') %>% 
  rename(data_value = 'data.value') 
grow_age_values<-read.csv("GROW_BLG/BLG_data/GROW_age_values.csv", header=TRUE) %>% 
  mutate(code = as.factor(code))


#remove punctuation and replace the space with _ 
blgrow_dropdown$sep <- gsub("[[:punct:]]", "", blgrow_dropdown$data_value)  #remove punctuation 
blgrow_dropdown$sep<-gsub("\\s", "_", blgrow_dropdown$sep) #replace space with _

#separate into four columns, one for each of the reviews if there are different answers
blgrow_dropdown<-separate(data=blgrow_dropdown, col=sep, c("age_hash", "review", "age_hash2", "review2", "age_hash3", "review3", "age_hash4", "review4" ), sep = "_")
blgrow_dropdown$age_hash<-as.factor(blgrow_dropdown$age_hash)
blgrow_dropdown$age_hash2<-as.factor(blgrow_dropdown$age_hash2)
blgrow_dropdown$age_hash3<-as.factor(blgrow_dropdown$age_hash3)
blgrow_dropdown$age_hash4<-as.factor(blgrow_dropdown$age_hash4)

# join 
blgrow_dropdown <-left_join(blgrow_dropdown, grow_task_values) %>% #get actual column name 
  left_join(grow_age_values, by=c('age_hash' = 'code')) %>% #get age values for the first review
  left_join(grow_age_values, by=c('age_hash2' = 'code'))  %>% #get age values for the 2nd review
  left_join(grow_age_values, by=c('age_hash3' = 'code')) %>% #get age values for the 3rd review
  left_join(grow_age_values, by=c('age_hash4' = 'code')) %>% #get age values for the 4th review
  rename(age1=age_group.x, age2=age_group.y, age3=age_group.x.x, age4=age_group.y.y)

#for some reason this hash code is not matching 9f71969064233 (note there aren't any review4s)
blgrow_dropdown$age1<-ifelse(blgrow_dropdown$age_hash =='9f71969064233', 5, blgrow_dropdown$age1) 
blgrow_dropdown$age2<-ifelse(blgrow_dropdown$age_hash2 =='9f71969064233', 5, blgrow_dropdown$age2) 
blgrow_dropdown$age3<-ifelse(blgrow_dropdown$age_hash3 =='9f71969064233', 5, blgrow_dropdown$age3) 

# keep >= 3s OR any that were 2 but don't have disagreements (NAs) 
age_good<-filter(blgrow_dropdown, review >= 3 | review2 >= 3 | (review == 2 & is.na(review2)) ) %>%
  mutate(age= case_when(
    is.na(review2) ~ age1, # if ~ then (total agreement)
    review2 > review ~ age2, # if ~ then (majority selected)
    review > review2 ~ age1 # if ~ then (majority selected)
  )) %>%
  select(subject_id, column_name, age)

#*filter out the ones for manual review by Calla #only 429 ####
age_bad<-filter(blgrow_dropdown, review < 3  & review2 < 3 | (review == 1 & is.na(review2))) %>%
  select(-c(task, data_value, age_hash, age_hash2, age_hash3, age_hash4))
#ages_for_review<-left_join(age_bad,urls )
#ages_for_review$age<- NA
#write.csv(ages_for_review, "/Users/katelynking/Desktop/ages_for_review.csv", row.names = FALSE)
age_manual<-read.csv( "GROW_BLG/BLG_data/manual_review_data/ages_manual_Calla.csv") %>% #after manual review 
            select(subject_id, column_name, age)%>% 
  mutate(column_name = trimws(column_name, which = c("both"))) %>% #trim white space 
  left_join(grow_task_values, by=c("column_name" = "old_name")) %>%
  select(subject_id, column_name.y, age)  %>% 
  rename(column_name = column_name.y)

#join manually reviewed and good datasets 
age_data<-rbind(age_good, age_manual)%>%
  pivot_wider(id_cols = c(subject_id),
               names_from = column_name, 
               values_from = c(age), 
               values_fill = list(age = NA) )

#*join age data: dropdown ages and length text  ####
all_grow<-left_join(age_data, lengths_clean) %>% 
  pivot_longer(-subject_id,
               names_to =c("row", ".value"),
               names_sep ="_row") %>% 
  rename(age_group = " age group ", fish_count = " Number of Fish ", length_range = " Length Range ", length_mean = " Mean Length ") %>% 
  filter(if_any(c("age_group", "fish_count", "length_range", "length_mean"), complete.cases)) %>%
  mutate(length_range = gsub("mm" , '', length_range), #remove units from some cells 
         length_mean = gsub("mm" , '', length_mean), 
         length_range = gsub("cm" , '', length_range), #remove units from some cells 
         length_mean = gsub("cm" , '', length_mean)
  )   %>%
  separate(length_range, c("length_min", "length_max"), sep = "-")  %>% 
  mutate(across(where(is.character), ~ na_if(.,""))) %>% # get rid of blank cell 
  mutate(fish_count = as.numeric(as.character(fish_count)), 
         length_min = as.numeric(as.character(length_min)), 
         length_max = as.numeric(as.character(length_max)),
         length_mean = as.numeric(as.character(length_mean))
  )%>% 
  select(-c(row))  %>% 
  drop_na('age_group')

#QAQC
#check NAs in age group and check if something is coerced to NA from the as.numeric code
#everything that was coerced should be NAs
#age group NAs should all not be there 


#### join all data #### 
#join with basic dates and with lake matching info (which include already basic text)
grow_units<-read.csv("/Users/katelynking/Desktop/grow_card_units.csv")  

#read in comments 
blgrow_comments<-read.csv("GROW_BLG/BLG_data/BLG-comments_2021-07-15.csv") %>%
  select(subject_id, "X_...comment_body") %>%
  rename(comments = "X_...comment_body")

comments <- aggregate(blgrow_comments[2], blgrow_comments[-2], 
                      FUN = function(X) paste(unique(X), collapse=", "))

GROW<-left_join(all_grow, grow_dates) %>%
  left_join(lake_match) %>%
  left_join(grow_units) %>% #units from manual review and tags
  left_join(urls) %>%
  left_join(comments) %>%
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
  mutate(length_mean_mm = ifelse(is.na(length_mean_mm) & fish_count == 1, length_min_mm, length_mean_mm)) %>% #if only one fish, fill in length mean with the same as the min length 
  mutate(length_min_mm = ifelse(subject_id =='58642811' & age_group == 7, 180.34, length_min_mm) ) %>% #one outlier that looks like a typo based on max and mean values 
  mutate(length_min_mm = ifelse(fish_count == 0 & !is.na(fish_count), NA, length_min_mm), #QAQC if the fish count is 0 and not NA (for some reason changes these also) then should not be length values. these were all manually checked
         length_max_mm = ifelse(fish_count == 0 & !is.na(fish_count), NA,length_max_mm),
         length_mean_mm = ifelse(fish_count == 0 & !is.na(fish_count), NA, length_mean_mm)
  )%>% 
  mutate(age_group = ifelse(subject_id == "59687825", NA, age_group) )#fix one age 0 has no age given 

#QAQC check for outliers 
plot(GROW$length_max_mm) 
plot(GROW$length_min_mm)
plot(GROW$length_mean_mm)

#add some comments for cards with multiple bluegill records
GROW$comments<-ifelse(GROW$subject_id == "58642823", ' two gear types', GROW$comments) #insert comment two gear types 58642823
GROW$comments<-ifelse(GROW$subject_id == "58642824", ' two gear types', GROW$comments)#insert comment two gear types 58642824

GROW<-GROW %>% 
  mutate(comment = ifelse(is.na(comment), comments, comment)
         ) %>% 
  select(-c(comments)) 



#re-order columns 
GROW <- GROW[, c(1, 15,14,13, 2, 3, 16,17,18, 4,5,6, 7, 8,9,12, 10,11)]

#final data set 
write.csv(GROW, "GROW_BLG/blg_grow_qaqc.csv", row.names = FALSE)

#### add DEC data #### 
text<-read.csv("GROW_BLG/BLG_data/text_reducer_bluegill_dec.csv") %>%
  select(subject_id, task, 'data.aligned_text', 'data.number_views', 'data.consensus_score','data.consensus_text') %>%
  drop_na('data.number_views') %>% 
  naniar::replace_with_na(replace = list(data.consensus_text = c('-', '--', "---", '...', '. . .'))) %>% #function that replaces all of a value with NA 
  drop_na('data.consensus_text') %>%
  left_join(grow_task_values) %>% # join with the task values to get the actual names
  separate(data.consensus_text, c("data", "trash"), sep = "\\(", remove = FALSE) %>% #remove parenthesis which are weights
  separate(data, into = c("data", "trash2"), sep = " ") 

#good data where consensus is >50% 
text_good<-filter(text, data.consensus_score >= 2 ) %>%
  select(subject_id, column_name, data) 

#what was viewed by >= 2 people but half didnt agree or only 1 person reviewed 
text_bad<- filter(text, data.consensus_score < 2) %>% 
  left_join(grow_dates) %>% #to get urls 
  drop_na(url_front) #remove card without a url 
#write.csv(text_bad,  "/Users/katelynking/Desktop/grow_blg_dec_review.csv", row.names = FALSE )
#checked all of these manually, all correct except for one instance change in code below 

#merging back in manually reviewed data 
manual_length<-text_bad%>%
  select(subject_id,column_name, data) %>% 
  mutate(data = ifelse(subject_id == 82941855 & column_name == "Second_row Length Range ", '6.8-8.0', data) 
         )

lengths_clean<-gtools::smartbind(text_good, manual_length) %>%
  mutate(column_name = trimws(column_name, which = c("both"))) %>% #trim white space 
  pivot_wider( #rotate so that each of the tasks is a column 
    id_cols = c(subject_id),
    names_from = column_name, 
    values_from = c(data), 
    values_fill = list(data = NA) ) 

#* blgrow_dropdown cleaning #### 
#read in files
grow_age_values<-read.csv("GROW_BLG/BLG_data/GROW_age_values.csv", header=TRUE) %>% 
  mutate(code = as.factor(code))

grow_task_values<-read.csv("GROW_BLG/BLG_data/GROW_task_values.csv", header=TRUE)

blgrow_dropdown<-read.csv("GROW_BLG/BLG_data/dropdown_reducer_bluegill_dec.csv", na.strings = c("", "NA")) %>%
  select(subject_id, task, 'data.value') %>%
  drop_na('data.value') %>% 
  rename(data_value = 'data.value') %>% 
  mutate(sep = gsub("[[:punct:]]", "", data_value), 
         sep=gsub("\\s", "_", sep)
  )%>% 
  separate(col=sep, c("age_hash", "review" ), sep = "_") %>% 
  mutate(age_hash=as.factor(age_hash)) %>% 
  left_join(grow_task_values) %>% #get actual column name 
  left_join(grow_age_values, by=c('age_hash' = 'code')) 


# keep >=  2 
age_good<-filter(blgrow_dropdown, review >= 2 ) %>%
  select(subject_id, column_name, age_group)

#filter out the ones for manual review ####
age_bad<-filter(blgrow_dropdown, review < 2 )  %>% 
          left_join(grow_dates) %>%  #to get urls 
            drop_na(url_front)
#checked all of these manually, all correct except for one instance change in code below 

#merging back in manually reviewed data 
age_manual<-age_bad%>%
  select(subject_id,column_name, age_group) %>% 
  mutate(age_group = ifelse(subject_id == 82939732 & column_name == "Second_row age group ", '2', age_group) 
  )

#join manually reviewed and good datasets 
age_data<-rbind(age_good, age_manual)%>%
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(age_group), 
              values_fill = list(age_group = NA) )

#join age data: dropdown ages and length text  #
all_grow<-left_join(age_data, lengths_clean) %>% 
  pivot_longer(-subject_id,
               names_to =c("row", ".value"),
               names_sep ="_row") %>% 
  rename(age_group = " age group ", fish_count = " Number of Fish", length_range = " Length Range", length_mean = " Mean Length") %>% 
  filter(if_any(c("age_group", "fish_count", "length_range", "length_mean"), complete.cases)) %>%
  separate(length_range, c("length_min", "length_max"), sep = "-") %>% 
  mutate(fish_count = as.numeric(as.character(fish_count)), 
         length_min = as.numeric(as.character(length_min)), 
         length_max = as.numeric(as.character(length_max)),
         length_mean = as.numeric(as.character(length_mean))
  )%>% 
  select(-c(row)) 


#### clean up units and join all data #### 
grow_units<-read.csv("/Users/katelynking/Desktop/grow_card_units.csv")  

GROW<-left_join(all_grow, grow_dates) %>%
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
  mutate(length_mean_mm = ifelse(is.na(length_mean_mm) & fish_count == 1, length_min_mm, length_mean_mm)) 

#QAQC check for outliers 
plot(GROW$length_max_mm) 
plot(GROW$length_min_mm)
plot(GROW$length_mean_mm) 

#reorder columns so that identifiers are at the beginning, followed by fish info, dates, and urls
GROW <- GROW[, c(1, 15,14,13, 2, 3, 16,17,18, 4,5,6, 7, 8,9,12, 10,11)]

#bind with the other data 
grow_blg<-read.csv("GROW_BLG/blg_grow_qaqc.csv")

grow_join<-rbind(GROW, grow_blg)

write.csv(grow_join, "GROW_BLG/blg_grow_all_qaqc.csv", row.names = FALSE)

