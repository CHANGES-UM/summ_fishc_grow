### FISH Collection (FISHc) data cleaning ###
## written by Katelyn King 
### abundance  data

#### load libraries #### 
library(dplyr)
library(tidyr)
library(stringr)
library(rqdatatable)
library(naniar)

urls<-read.csv("urls/new_urls.csv") #have the bad urls removed already 
#dont need tasks - they are splitting sp into groups 

#### abundance survey data- select a species and select the number of individuals #### 
#total vote count is all species on the card, so not useful because it is aggregated 
abund_survey<-read.csv("FISHc/FISHc_data/abundance_data/survey_reducer_abundance.csv", na.strings = c("", "NA")) %>%
  select(-c(workflow_id, reducer, task, data.total_vote_count, data.aggregation_version)) %>%
  rename(species = 'data.choice', front = "URL.4", back= "URL.8") 

#for manual review #if choice count = 1 only one choice was selected (just one person, no alternate choices)
#abund_review<-filter(abund_survey, data.choice_count == 1) %>% 
 # drop_na('front') %>%# drop without url link 
  #select(subject_id, front, back, species)
#write.csv(abund_review, "/Users/Desktop/fishc_abund_review.csv", row.names = FALSE) 

#good cards clean up 
abund_long<- filter(abund_survey,  data.choice_count >1) %>%
  pivot_longer( #pivot back to longer  
  cols = starts_with("data.answers_howmany."),
  names_to = "abund",
  names_prefix = "data.answers_howmany.", #remove entire prefix from the column name 
  values_to = "reviews",
  values_drop_na = TRUE
) 

#pull out ones with ties for manual review               
multi<-abund_long %>% 
          group_by(subject_id, species) %>% 
            filter(n()>1) %>% #select out where different answers were selected 
  pivot_wider(id_cols= c(subject_id, species),
              names_from = reviews,
              values_from = abund,  
              values_fn = length) %>% #counts when reviews were split evenly
        filter(`1` == '4' | `1` == '3' | `1` == '2') %>% #pull out ones with ties to review manually n=95 
        left_join(urls) %>%  #get urls back 
        select(-c( `1`,  `2`,  `3`))
#write.csv(multi, "/Users/Desktop/fishc_multi_review.csv", row.names = FALSE) 

#combine the good and manual data 
not_multi<-abund_long %>% 
    filter(reviews > 1) %>% 
  select(-c(data.choice_count, reviews))
ties<-read.csv("FISHc/FISHc_data/manual_reviews/fishc_abund_multi_review_kk.csv") %>% 
  select(-c(comment)) %>% 
  drop_na(abund)
abund_bad<-read.csv("FISHc/FISHc_data/manual_reviews/fishc_abund_review_cb.csv", na.strings = c("", "NA"))%>% 
  select(-c(comments))  %>% 
  drop_na(abund)

abundance_all<-rbind(not_multi, abund_bad, ties) 

#* check all cards with parenthesis to determine which abundance number to use ####
#volunteers tagged cards with parenthesis, Calla checked tags
brackets<-read.csv("FISHc/FISHc_data/abundance_data/brackets_cb.csv") %>% 
  select(subject_id, use, comments)

not_high_abund <-abundance_all  %>%
  filter(abund != "1350") %>% #remove answers that were ranges 13-50 or >50 these were transcribed 
  filter(abund != "51") %>%  
  left_join(brackets) %>%#join with bracket info (the other high abund has already been corrected by Calla with comments)
mutate(abund = ifelse(subject_id == "58749853" & species == "largemouthbass", 1, abund), #start changing abund numbers if the incorrect parenthesis was used 
       abund = ifelse(subject_id == "58749853" & species == "rockbass", 1, abund), 
       abund = ifelse(subject_id == "58750572"& species == "perchyellowperch", 4, abund),
       abund = ifelse(subject_id == "59105590" & species == "iowadarter", 11, abund),
       abund = ifelse(subject_id == "59964327" & species == "browntrout", NA, abund),
       abund = ifelse(subject_id == "59964327" & species == "perchyellowperch", 3, abund),
       abund = ifelse(subject_id == "59969694" & species == "largemouthbass", 11, abund),
       abund = ifelse(subject_id == "58750103" & species == "johnnydarter", 2, abund),
       abund = ifelse(subject_id == "58750103" & species == "iowadarter", 2, abund),
       abund = ifelse(subject_id == " 58750283" & species == "iowadarter", 3, abund),
       abund = ifelse(subject_id == " 58752090" & species == "largemouthbass", 48, abund),
       abund = ifelse(subject_id == " 58752090" & species == "commonsuckerwhitesucker", 74, abund),
       abund = ifelse(subject_id == " 58752094" & species == "perchyellowperch", 7, abund),
       abund = ifelse(subject_id == " 58752094" & species == "commonsuckerwhitesucker", 14, abund),
        abund = ifelse(subject_id == " 58752095" & species == "largemouthbass", 48, abund),
       abund = ifelse(subject_id == " 58752095" & species == "blackcrappie", 4, abund),
       abund = ifelse(subject_id == " 58752095" & species == "commonsuckerwhitesucker", 74, abund),
       abund = ifelse(subject_id == " 58752100" & species == "perchyellowperch", 7, abund),
       abund = ifelse(subject_id == " 58752100" & species == "commonsuckerwhitesucker", 14, abund),
       abund = ifelse(subject_id == " 59963638" & species == "largemouthbass", 10, abund),
       abund = ifelse(subject_id == " 59963638" & species == "smallmouthbass", 8, abund),
       abund = ifelse(subject_id == " 59963862" & species == "brownbullhead", 5, abund),
       abund = ifelse(subject_id == " 59965408" & species == "commonsuckerwhitesucker", 500, abund),
       abund = ifelse(subject_id == " 59967544" & species == "northernpike", 3, abund),
       abund = ifelse(subject_id == " 59967544" & species == "walleyeyellowpikeperch", 2, abund),
       abund = ifelse(subject_id == " 59967544" & species == "bluegill", 12, abund),
       abund = ifelse(subject_id == " 59967544" & species == "pumpkinseed", 2, abund),
       abund = ifelse(subject_id == " 59967544" & species == "rockbass", 7, abund),
       abund = ifelse(subject_id == " 58749864" & species == "perchyellowperch", 150, abund),
       abund = ifelse(subject_id == " 58749864" & species == "largemouthbass", 90, abund),
       abund = ifelse(subject_id == " 58749864" & species == "bluntnosedbluntnoseminnow", 10, abund),
       abund = ifelse(subject_id == " 58750648" & species == "smallmouthbass", 4, abund),
       abund = ifelse(subject_id == " 58751863" & species == "largemouthbass", 34, abund),
       abund = ifelse(subject_id == " 58752096" & species == "blackcrappie", 360, abund),
       abund = ifelse(subject_id == " 59105881" & species == "perchyellowperch", 57, abund),
       abund = ifelse(subject_id == " 59963645" & species == "perchyellowperch", 51, abund),
       abund = ifelse(subject_id == " 59964393" & species == "northernpike", 7, abund),
       abund = ifelse(subject_id == " 59964393" & species == "pumpkinseed", 6, abund),
       abund = ifelse(subject_id == " 59965407" & species == "pumpkinseed", 60, abund),
       abund = ifelse(subject_id == " 59966952" & species == "largemouthbass", 4, abund),
       abund = ifelse(subject_id == " 59968422" & species == "brooktrout",6, abund),
       abund = ifelse(subject_id == " 59969132" & species == "largemouthbass",6, abund)
) %>% 
  select(-c(use)) #remove use column but leave comments 



#*pull out large abund for re-classifying #### 
#high_abund <-abundance_all  %>%
 # filter(abund == "1350" | abund == "51") #13-50 and >51 were categorical 

#these cards were all filled back in manually with the actual number 
lmb_abund<-read.csv("FISHc/FISHc_data/abundance_data/lmb_abund.csv") %>% 
  mutate(abund = ifelse(subject_id == " 58750103" & species == "largemouthbass", 2, abund), 
         abund = ifelse(subject_id == " 59968417" & species == "largemouthbass",13, abund), 
         abund = ifelse(subject_id == " 59970424" & species == "largemouthbass",20, abund)
  )
blg_abund<-read.csv("FISHc/FISHc_data/abundance_data/bluegill_abund.csv") %>% 
  mutate( abund = ifelse(subject_id == " 59969132" & species == "bluegill",21, abund), 
          abund = ifelse(subject_id == " 58748674" & species == "bluegill",16, abund)
  )
cis_abund<-read.csv("FISHc/FISHc_data/abundance_data/cisco_abund.csv")
perch_abund<-read.csv("FISHc/FISHc_data/abundance_data/perch_abund.csv") %>% 
  mutate(abund = ifelse(subject_id == " 58752090" & species == "perchyellowperch", 100, abund), 
         abund = ifelse(subject_id == " 58752095" & species == "perchyellowperch", 100, abund),
         abund = ifelse(subject_id == " 59963862" & species == "perchyellowperch", 64, abund))
wae_abund<-read.csv("FISHc/FISHc_data/abundance_data/walleye_abund.csv")
other_abund<-read.csv("FISHc/FISHc_data/abundance_data/other_abund.csv") %>% 
  mutate(abund = ifelse(subject_id == " 59963679" & species == "rockbass", 18, abund), 
         comments = ifelse(subject_id == " 59963679" & species == "rockbass", "17 im and adult and 1 fry", comments))

#*combine everything back together 
abundance_new<-rbind(not_high_abund, lmb_abund, blg_abund, cis_abund, perch_abund, wae_abund, other_abund) %>% 
  drop_na(subject_id) %>% #somehow got a bunch of extra blank rows 
  drop_na(abund) #several that did not have abundance

#### text data - other species not listed from the choices #### 
# get rid of anything that only had 1 review- looks like these people just recorded everything 
abund_text<-read.csv("FISHc/FISHc_data/abundance_data/text_reducer_abundance.csv") %>%
  select(subject_id, 'data.number_views', 'data.consensus_text') %>% #don't need task names(split by category only)
  drop_na('data.number_views') %>% #drop rows that don't have anything 
  rename(data_number_views ='data.number_views', fish_extras=data.consensus_text) %>%
  dplyr::filter(data_number_views > 1)  %>% #remove anything with only one person 
  select(-c(data_number_views)) %>% 
  group_by(subject_id) %>%
  summarise_at(vars(-group_cols()), ~ toString(.[!is.na(.)])) %>%  #aggregate all sp from same card from dif rows into one row 
  left_join(urls)


#### ADD DEC DATA #### 
abund_text_dec<-read.csv("FISHc/FISHc_data/abundance_data/text_reducer_abund_dec.csv") %>%
  select(subject_id, 'data.number_views', 'data.consensus_text', 'URL.4', 'URL.8' ) %>% #don't need task names(split by category only)
  drop_na('data.number_views') %>% #drop rows that don't have anything 
  rename(data_number_views ='data.number_views', fish_extras=data.consensus_text) %>%
  dplyr::filter(data_number_views > 1)  %>% #remove anything with only one person 
   select(-c(data_number_views)) %>% 
  group_by(subject_id) %>%
  summarise_at(vars(-group_cols()), ~ toString(.[!is.na(.)])) %>%  #aggregate all sp from same card from dif rows into one row 
  rename(front = 'URL.4', back= 'URL.8')

abund_text_all <-rbind(abund_text, abund_text_dec)

#### question data - is there abundance data listed on this card? ####
#if no abundance data then all fish listed as 0s, because still may have been effort  
abund_q<-read.csv("FISHc/FISHc_data/abundance_data/question_reducer_abundance.csv",  na.strings = c("", "NA")) %>%  
  pivot_wider(id_cols = c(subject_id), #pivot abundance table so species are on the columns
              names_from = task, 
              values_from = c("data.there.is.no.abundance.data.listed.in.any.category.on.this.card")) %>% 
  select(subject_id, T1) %>% 
  rename(no_data= T1) %>% 
  drop_na(no_data) %>% 
  anti_join(abundance_new) %>% #cant be abundance data in the fish dat 1106 OR in the abund_text where people recorded other fish 
  anti_join(abund_text_all) %>% 
  left_join(urls) %>% 
  mutate(species = "bluegill", #, # need to add dummy variables so that I can join with the other abund data before pivoting the table 
         abund = 0, 
         comments = NA_character_) %>% 
  select(-c(no_data))

# add abundance question data before pivot 
abundance_new<-rbind(abundance_new, abund_q)

comments<-select(abundance_new, subject_id, comments)%>% 
          mutate(across(c("comments"), ~ifelse(.=="", NA, as.character(.)))) %>% #blank cells need to be NA 
          group_by(subject_id) %>%
          summarise_at(vars(-group_cols()), ~ toString(.[!is.na(.)])) %>%  #aggregate comments into one columns 
          rename(abund_comments = comments )

#pivot abundance table so species are on the columns
fish_dat<-abundance_new %>% 
  pivot_wider(id_cols = c(subject_id), 
              names_from = species, 
              values_from = c(abund), 
              values_fill = list(abund = "0") #species that have NA will be set to 0 
  ) %>% 
  select(-c(walleye)) %>% #fish_dat has an extra "walleye" column exactly same as walleyepikeperch, except subject 58753536 should have 7 
  mutate(walleyeyellowpikeperch = ifelse(subject_id == 58753536, 7, walleyeyellowpikeperch)
         ) %>%
  tibble %>% 
  select("subject_id", sort(colnames(.))) %>% #order species alphabetically 
  left_join(urls) %>% 
  left_join(comments) 

#* final join with extra fish text #### 
abund_final<-full_join(fish_dat, abund_text_all, by="subject_id") %>% 
  mutate(front = ifelse(is.na(front.x), front.y, front.x), 
         back = ifelse(is.na(back.x), back.y, back.x)
         ) %>% 
  select(-c(front.x, front.y, back.x, back.y))
         

#check where species are NA 
#fish_extra_check<-abund_final%>% 
 # filter(is.na(bandedkillifishmenonakillifish))
fish_extra_check<-read.csv("FISHc/FISHc_data/manual_reviews/fish_extra_check.csv") %>% 
  mutate(across(-subject_id, as.character))

#rows_update() modifies existing rows in fishc with new data from updated_new_key
abund_final2<-rows_update(abund_final, fish_extra_check, by = "subject_id", unmatched = "ignore")%>% 
  mutate(across(2:56, ~ replace_na(.x, '0'))) %>% #set the NAs for fish to 0s after checks
  mutate(cisco = ifelse(subject_id == 59105756, 1, cisco), 
         cisco = ifelse(subject_id ==59106134, 1, cisco), 
        cisco = ifelse(subject_id ==59966533, 49, cisco)) # a few cards were missing cisco observations 

write.csv(abund_final2, "FISHc/FISHc_data/final_data/fishc_abund.csv", row.names = FALSE)


