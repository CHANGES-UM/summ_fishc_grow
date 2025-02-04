### FISH Collection (FISHc) gear data cleaning ###
## written by Katelyn King 
### text gear information 

#### load libraries #### 
library(dplyr)
library(tidyr)
library(stringr)
library(rqdatatable)
library(naniar)

urls<-read.csv("urls/new_urls.csv") #have the bad urls removed already 

#subjects that were removed from workflow should be removed from dataset
bad_urls<-read.csv("urls/oldbfirst_subjects_to_remove.csv") 

#### FISHc text cleaning ##########################################
fishc_task_values<-read.csv("FISHc/FISHc_data/FISHc_task_values.csv", header=TRUE)

fishc_text<-read.csv("FISHc/FISHc_data/text_data/text_reducer_fishc_texts.csv") %>% #text is ALL
  select(subject_id, task, 'data.aligned_text', 'data.number_views', 'data.consensus_score','data.consensus_text') %>%
  drop_na('data.number_views')%>%
  left_join(fishc_task_values, by=c('task' = 'code')) %>% # join with the task values to get the actual names
              left_join(urls)  %>% 
              anti_join(bad_urls) %>% #remove bad subjects 
              drop_na('front') %>%  # drop cards that don't have url #remove rows where entry was a dash or ?
  naniar::replace_with_na(replace = list(data.consensus_text = c('-','_', '--', '- -', '---', '- - -', '----', '?', '??????', '......?'))) %>% #function that replaces all of a value with NA 
  drop_na('data.consensus_text') %>% 
  mutate(column_name=tolower(column_name))

#best way to tackle this is to split up by task 
summary(as.factor(fishc_text$column_name)) #check out all of the tasks 


##### gear ######################################
gear<-filter(fishc_text, column_name == "gear_used_kind" | column_name == "gear_used_mesh" | column_name == "gear_used_length" ) %>% 
          select(-c(task, data.number_views))
  
#filter out the ones for manual review ; looks like many between 1.5-2 are just due to feet vs ' specifications
fishc_gear_bad<-filter(gear, data.consensus_score < 1.5)

#read in manual reviewed 
fish_gear_manual<-read.csv("FISHc/FISHc_data/manual_reviews/fishc_gear_bad_kk.csv") %>%
  select(subject_id, column_name, data, front, back) %>%
  rename(data.consensus_text = data)
    
#clean up the data - pull out all gears and classify theme 
gear_good<-filter(gear, data.consensus_score >= 1.5) %>%
  select(subject_id, column_name, data.consensus_text, front, back) %>%
  rbind(fish_gear_manual) %>% #join good with manual reviews 
      mutate(data.consensus_text=tolower(data.consensus_text), #make all text lowercase
             data2=gsub("'", 'feet', data.consensus_text), # replace characters with units
             data2=gsub("ft\\.", 'feet', data2),
             data2=gsub("ft", 'feet', data2), 
             data2=gsub('"', 'inches', data2), 
             data2=gsub(" 1/2", '.5', data2), #replace fractions
             data2=gsub("1/2", '.5', data2), 
             data2=gsub(" 1/3", '.33', data2), 
             data2=gsub("1/4", '.25', data2), 
             data2=gsub(" 1/4", '.25', data2), 
             data2=gsub('3/4', '.75', data2), 
             data2=gsub(" 3/4", '.75', data2), 
             data2=gsub('cs', 'common', data2), #replace cs with common 
             data2=gsub('c\\.s', 'common', data2),
             data2=gsub('c\\. s\\.', 'common', data2),
             data2=gsub('c/s', 'common', data2), 
             data2=gsub('fylee', 'fyke', data2), #specific mistakes 
             data2=gsub('fike', 'fyke', data2), #specific mistakes 
             data2=gsub('h & l', 'hook and line', data2), 
             data2=gsub('h and l', 'hook and line', data2),
             data2=ifelse(subject_id == 58642530 | subject_id ==58753131, '1x4 trap net', data2), #change tn to trap net  
             data2=ifelse(subject_id == 58748949, 'live minnows and worms - hook', data2), #adding hook for ease of extraction
             data2=ifelse(subject_id == 58750866 | subject_id == 59967404 | subject_id == 61361238, 'powdered derris - rotenone', data2), #adding rotenone
             data2=ifelse(subject_id == 58753571, '240 v dc pulse 18 feet boat - shock', data2), #adding shock
             data2=ifelse((subject_id == 59105566 | subject_id ==59969766 | subject_id ==59969781) & column_name == 'gear_used_kind', '110 volt generator - shock', data2), #adding shock
             data2=ifelse(subject_id == 59105814, 'spinner-minnow and plug - hook', data2), #adding hook
             data2=ifelse(subject_id == 59106037 & column_name == 'gear_used_kind', '110 volt generator - shock', data2), #adding shock
             data2=ifelse(subject_id == 59965059, 'deep-sea outfits ( hook and line)', data2), #adding hook and line from point of examinination
             data2=ifelse((subject_id == 59969924 | subject_id ==59969925 | subject_id == 59969924) & column_name == 'gear_used_kind', 'wisconsin fyke net', data2), #adding the word fyke 
             data2=ifelse((subject_id == 59967027 | subject_id == 59967037 | subject_id == 59967039) & column_name == 'gear_used_kind', 'trammel net ( gill )', data2), 
             data2=ifelse(subject_id == 61361805, 'underwater light - observation', data2), 
             data2=ifelse(subject_id == 59966959, 'picked up dead fish with canoe - observation', data2), 
             data2=ifelse(subject_id == 59963984, 'direct sight records, ekman dredge, fish picked up dead - observation', data2), 
             data2=ifelse(subject_id == 59969768 & column_name == 'gear_used_kind', 'fiberglass boat - 110 volt generator shock', data2), 
             data2=ifelse(subject_id ==  59969849, '240 ac - boston whaler - shock', data2), 
             )  %>% 
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(data2)
              ) %>%          #could use | or but then get all characters in one column
  mutate(   gr1 = str_extract(gear_used_kind,"(?<=^| )(bag).*?(?=$| )"), #The regular expression matches the word preceded by a space or the start of the string (?<=^| ), and then as few characters as possible .*? until the next space or the end of the string (?=$| )
            gr2 = str_extract(gear_used_kind,"(?<=^| )(common).*?(?=$| )"),
            gr3 = str_extract(gear_used_kind,"(?<=^| )(gill).*?(?=$| )"),
            gr4 = str_extract(gear_used_kind,"(?<=^| )(exp).*?(?=$| )"),
            gr5 = str_extract(gear_used_kind,"(?<=^| )(bar).*?(?=$| )"),
            gr6 = str_extract(gear_used_kind,"(fyke).*?(?=$| )"),
            gr7 = str_extract(gear_used_kind,"(trap).*?(?=$| )"),
            gr8 = str_extract(gear_used_kind,"(?<=^| )(boom).*?(?=$| )"),
            gr9 = str_extract(gear_used_kind,"(?<=^| )(electro).*?(?=$| )"), 
            gr10 = str_extract(gear_used_kind,"(?<=^| )(shock).*?(?=$| )"), 
            gr11=str_extract(gear_used_kind,"(?<=^| )(hook).*?(?=$| )"),
            gr12=str_extract(gear_used_kind,"(?<=^| )(rod).*?(?=$| )"),
            gr13=str_extract(gear_used_kind,"(?<=^| )(seine).*?(?=$| )"),
            gr14=str_extract(gear_used_kind,"(?<=^| )(spear).*?(?=$| )"),
            gr15=str_extract(gear_used_kind,"(?<=^| )(angling).*?(?=$| )"),
            gr16 = str_extract(gear_used_mesh,"(?<=^| )(exp).*?(?=$| )"), #capture exp from mesh 
            gr17 = str_extract(gear_used_mesh,"(?<=^| )(bar).*?(?=$| )"), #capture bar from mesh 
            gr18=str_extract(gear_used_kind,"(?<=^| )(handline).*?(?=$| )"),
            gr19=str_extract(gear_used_kind,"(?<=^| )(trawl).*?(?=$| )"),
            gr20=str_extract(gear_used_kind,"(rotenone).*?(?=$| )"),
            gr21=str_extract(gear_used_kind,"(?<=^| )(hand).*?(?=$| )"),
            gr22=str_extract(gear_used_kind,"(?<=^| )(dip).*?(?=$| )"),
            gr23=str_extract(gear_used_kind,"(?<=^| )(rough).*?(?=$| )"),
            gr24= str_extract(gear_used_length,"(?<=^| )(bag).*?(?=$| )"), #capture bag seine from length (sometimes appears here)
            gr25 = str_extract(gear_used_mesh,"(?<=^| )(common).*?(?=$| )"), #capture common sense seine from mesh 
            gr26 = str_extract(gear_used_kind,"(?<=^| )(troll).*?(?=$| )"),
            gr27 = str_extract(gear_used_kind,"(?<=^| )(hoop).*?(?=$| )"),
            gr28 = str_extract(gear_used_kind,"(?<=^| )(worm).*?(?=$| )"),
            gr29 = str_extract(gear_used_kind,"(?<=^| )(flies).*?(?=$| )"),
            gr30 = str_extract(gear_used_kind,"(?<=^| )(seining box).*?(?=$| )"),
            gr31 = str_extract(gear_used_kind,"(?<=^| )(observation).*?(?=$| )"),
            gr32 = str_extract(gear_used_kind,"(?<=^| )(pole).*?(?=$| )"),
            gr33 = str_extract(gear_used_kind,"(?<=^| )(poison).*?(?=$| )"),
            
            gr4 = ifelse(is.na(gr4), gr16, gr4), #incorporate exp mesh if not captured from gear used kind
            gr5 = ifelse(is.na(gr5), gr17, gr5), #incorporate bar mesh if not captured from gear used kind
            gr4 = ifelse(is.na(gr4) & is.na(gr5), gr3, gr4), #if both gr4 and 5 are NA incorporate gill gr3 into gear 4 (exp) 
            gr1= ifelse(is.na(gr1), gr24, gr1), #incorporate bag from length gr24 into the gr1 bag column 
            gr2= ifelse(is.na(gr2), gr25, gr2), #incorporate common from mesh into the gr2 common column
            gr1 = ifelse(is.na(gr1) & is.na(gr2), gr13, gr1), #if both gr1 and gr2 are NA incorporate seine gr13 into gr1 bag 
            gr8 = ifelse(is.na(gr8), gr10, gr8), #incorporate shock into gear 8
            gr11 = ifelse(is.na(gr11), gr12, gr11), #incorporate rod into gear 11 hook
            gr11 = ifelse(is.na(gr11), gr15, gr11), #incorporate angling into gear 11 hook 
            gr11 = ifelse(is.na(gr11), gr18, gr11), #incorporate handline into gear 11 hook
            gr11 = ifelse(is.na(gr11), gr26, gr11), #incorporate troll into gear 11 hook
            gr11 = ifelse(is.na(gr11), gr28, gr11), #incorporate worm into gear 11 hook
            gr11 = ifelse(is.na(gr11), gr29, gr11), #incorporate flies into gear 11 hook
            gr11 = ifelse(is.na(gr11), gr32, gr11), #incorporate pole into gear 11 hook
            gr7 = ifelse(is.na(gr7), gr23, gr7), #incorporate rough into gear 7 trap net 
            gr20 = ifelse(is.na(gr20), gr33, gr20) #incorporatepoinson into rotenone
            ) %>% 
          select(-c(gr3, gr10, gr12, gr13, gr15, gr16, gr17, gr18, gr23, gr24, gr25, gr26, gr28, gr29, gr32, gr33)) %>%   #check to make sure every row has been assigned a gear  #gear_good$review<-apply(gear_good[, 5:18], MARGIN = 1, FUN = function(x) all(is.na(x)))
  pivot_longer( #pivot back to longer so that each separate gear is on a new row 
        cols = starts_with("gr"),
        names_to = "gear",
        names_prefix = "gr",
        values_to = "gear2",
        values_drop_na = TRUE
        ) %>%
  mutate(gear2=gsub("[[:punct:]]|[[:digit:]]","",gear2), #remove random punctuation 
         gear= case_when(
           gear2 == 'angling' ~ 'rod', # if ~ then #standardize gear2 names 
           gear2 == 'bag' ~ 'seine',  
           gear2 == 'bar' ~ 'gillnet', 
           gear2 == 'barge' ~ 'shock',
           gear2 == 'barinches' ~ 'gillnet', 
           gear2 == 'bars' ~ 'gillnet', 
           gear2 == 'bart' ~ 'gillnet', 
           gear2 == 'boom' ~ 'shock',
           gear2 == 'boomshocker' ~ 'shock',
           gear2=="common" ~ "seine", 
           gear2=="commonc" ~ "seine",
           gear2=="commonm" ~ "seine",
           gear2=="dip" ~ "dipnet",
           gear2=="electro" ~ "shock",
           gear2=="electrodes" ~ "shock", 
           gear2=="electrofishing" ~ "shock", 
           gear2=="electroshocked" ~ "shock",
           gear2=="electroshocker" ~ "shock", 
           gear2=="electroshocking" ~ "shock", 
           gear2=="exp" ~ "gillnet", 
           gear2=="exper" ~ "gillnet", 
           gear2=="experemental" ~ "gillnet", 
           gear2=="experi" ~ "gillnet",
           gear2=="experim" ~ "gillnet",
           gear2=="experimental" ~ "gillnet",
           gear2=="experimentalinches" ~ "gillnet", 
           gear2=="experimentals" ~ "gillnet", 
           gear2=="expgill" ~ "gillnet", 
           gear2=="expinches" ~ "gillnet",
           gear2=="flies" ~ "rod",
           gear2=="fyke" ~ "fyke net", 
           gear2=="fykenet" ~ "fyke net", 
           gear2=="fykes" ~ "fyke net", 
           gear2=="gill" ~ "gillnet", 
           gear2=="gillnet" ~ "gillnet", 
           gear2=="gillnets" ~ "gillnet", 
           gear2=="gills" ~ "gillnet", 
           gear2=="hand" ~ "handnet", 
           gear2=="hands" ~ "hands", 
           gear2=="handline" ~ "rod",
           gear2=="hook" ~ "rod", 
           gear2=="hookandlineworms" ~ "rod", 
           gear2=="hookline" ~ "rod", 
           gear2=="hoop" ~ "hoop net", 
           gear2=="observation" ~ "observation", 
           gear2=="poison" ~ "rotenone", 
           gear2=="pole" ~ "rod", 
           gear2=="rod" ~ "rod", 
           gear2=="rods" ~ "rod", 
           gear2=="rotenone" ~ "rotenone",
           gear2=="rough" ~ "trap net",
           gear2=="seine" ~ "seine",
           gear2=="seines" ~ "seine",
           gear2=="seining box" ~ "seining box",
           gear2=="shock" ~ "shock",
           gear2=="shocker" ~ "shock",
           gear2=="shocking" ~ "shock",
           gear2=="spear" ~ "spear",
           gear2=="speargun" ~ "spear",
           gear2=="spears" ~ "spear",
           gear2=="spearunderwater" ~ "spear",
           gear2=="trap" ~ "trap net",
           gear2=="trapnet" ~ "trap net",
           gear2=="trapnets" ~ "trap net",
           gear2=="traps" ~ "trap net", 
           gear2=="trawl" ~ "trawl",
           gear2=="trolled" ~ "rod",
           gear2=="trolling" ~ "rod",
           gear2=="worms" ~ "rod"
         )
  )

#need to pull out multiple gear types from single gear types so I can get effort/mesh for multiple 
multi_gear <- gear_good %>%
  group_by(subject_id) %>%
  filter(n()>1)  %>%
  left_join(urls)

#pull out single gear
single_gear<-gear_good %>% #single gear + multi_gear =  the gear_good data 
  group_by(subject_id) %>%
  filter(n()==1) %>% #
  mutate(number = ifelse((gear == "fyke net" | gear == "gillnet" | gear == "seine" | gear == "trap net"), str_extract(gear_used_kind, regex("[\\d]+")), NA), #extract the first number as number of nets
         number = ifelse((is.na(number) & (gear == "fyke net" | gear == "gillnet" | gear == "seine" | gear == "trap net")), "1" , number ), #if the number is na and it is a net, then put 1 
         gear_effort = ifelse(gear == "shock", str_extract(gear_used_mesh, regex("[\\d]+")), NA), #for shock, use time from mesh  
         gear_effort_units= ifelse(gear == "shock", str_extract(gear_used_mesh, regex("[a-z]+")), NA), 
         gear_effort = ifelse((is.na(gear_effort) & gear == "shock"), str_extract(gear_used_length, regex("[\\d]+")), gear_effort), #for shock use time from length column
         gear_effort_units= ifelse((is.na(gear_effort_units) & gear == "shock"), str_extract(gear_used_length, regex("[a-z]+")), gear_effort_units), 
         gear_effort_units = ifelse((gear == "fyke net" | gear == "gillnet" | gear == "seine" | gear == "trap net"), str_extract(gear_used_kind, regex("haul|night|day|hours|set")), gear_effort_units)
  ) %>%
  left_join(urls)

#* read in multi gear####
#double check for duplicates - did an anti_join and no overlap
#some rows get deleted because they are not actually multiple and some get added if they didn't catch it 
multi_gear<-read.csv("FISHc/FISHc_data/manual_reviews/fishc_multi_gear_cb.csv",  na.strings = c("", "NA")) %>% 
  select(-c(gear_mesh, mesh_units, gear_length, length_units)) %>%    #for now dont need this info separate 
  mutate(effort_flag= ifelse(number=="unknown", "unknown", NA),  #create an effort flag 
         number= ifelse(number=="unknown", NA, number), #change the unknown in the number column to an NA 
         number=as.numeric(as.character(number))
         ) 

multi_gear_2<-read.csv("FISHc/FISHc_data/manual_reviews/fishc_multi_gear_part2_cb.csv",  na.strings = c("", "NA")) %>% 
  select(-c(gear_mesh, mesh_units, gear_length, length_units)) %>%    #for now dont need this info separate 
  mutate(effort_flag= ifelse(number=="unknown", "unknown", NA),  #create an effort flag 
         number= ifelse(number=="unknown", NA, number), #change the unknown in the number column to an NA 
         number=as.numeric(as.character(number)), 
         gear = case_when(
                    gear == 'fyke net' ~ 'fyke',
                    gear == 'gillnet' ~ 'gill',
                    gear == 'trap net' ~ 'trap',
                    TRUE ~ gear )
          ) %>% 
  rename(gear_effort=gear_set, gear_effort_units=gear_set_units)  

#* read in single gear ####

#single gear was manually checked for number assignments and efforts, some were split if it was found that there were multiple gear 
single_gear<-read.csv("FISHc/FISHc_data/manual_reviews/single_gear.csv", na.strings = c("", "NA")) %>% 
  rename(comments = X) %>% 
  mutate(effort_flag= ifelse(number=="unknown", "unknown", NA),  #create an effort flag 
         number= ifelse(number=="unknown", NA, number), #change the unknown in the number column to an NA 
         number=as.numeric(as.character(number)), 
         gear = case_when(
           gear == 'fyke net' ~ 'fyke',
           gear == 'gillnet' ~ 'gill',
           gear == 'trap net' ~ 'trap',
           TRUE ~ gear )
  ) 

#* re-combine all and make gears into columns #### 
comments<-rbind(multi_gear, multi_gear_2, single_gear) %>% 
  mutate(flag= ifelse((gear_effort=="unknown"| gear_effort=="few" | gear_effort=="8 to 10"), "unknown", effort_flag)
  )%>%#mutate is not keeping exicting unknowns for some reason
  mutate(effort_flag = coalesce(effort_flag, flag)) %>% #join the two columns into one new one 
  select(subject_id, comments, effort_flag) %>% 
  distinct(subject_id, .keep_all= TRUE)

gear_all<-rbind(multi_gear, multi_gear_2, single_gear) %>%
        mutate(effort_flag= ifelse(gear_effort=="unknown"| gear_effort=="few" | gear_effort=="8 to 10", "unknown", effort_flag),  #effort flag 
               gear_effort= ifelse(gear_effort=="unknown" | gear_effort=="few" | gear_effort=="8 to 10", NA, gear_effort),
               gear_effort=as.numeric(as.character(gear_effort))) %>%
  mutate(gear2 = case_when(
    gear2=="exp" ~ "exp", 
    gear2=="exper" ~ "exp", 
    gear2=="experemental" ~ "exp", 
    gear2=="experi" ~ "exp",
    gear2=="experim" ~ "exp",
    gear2=="experimental" ~ "exp",
    gear2=="experimentalinches" ~ "exp", 
    gear2=="experimentals" ~ "exp", 
    gear2=="expgill" ~ "exp", 
    gear2=="expinches" ~ "exp",
    gear2=="bar" ~ "straight",
    gear2=="bars" ~ "straight",
    TRUE ~ gear2
  )) %>% 
  rename(type = gear, descript = gear2) %>% 
  group_by(subject_id) %>% #need each row to be a unique card 
  mutate(ID = paste0("gear", 1:n())) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(subject_id, gear_used_kind, gear_used_mesh, gear_used_length),
               names_from = c(ID), 
               values_from = c(type, descript, number, gear_effort, gear_effort_units), 
               values_fn = list(number=sum) )  %>% 
  left_join(comments)
  

#reorder columns  
gear_all <- gear_all[, c(1,2,3,4,5,9,13,17,21,6,10,14,18,22,7,11,15,19,23,8,12,16,20,24, 25,26   )]

#remove effort  and number columns for database - these are not super realiable and will need to be done on an individual basis by researcher 
gear_all<-select(gear_all, -c(number_gear1, gear_effort_gear1, gear_effort_units_gear1,number_gear2, gear_effort_gear2, gear_effort_units_gear2,
                              number_gear3, gear_effort_gear3, gear_effort_units_gear3, number_gear4, gear_effort_gear4, gear_effort_units_gear4, 
                              comments, effort_flag)) 
  

##### area_covered ######################################
area_covered<-filter(fishc_text, column_name == "area_covered" ) %>% 
  select(-c(task, data.number_views))

#filter out the ones <50% for manual review 
#area_covered_bad<-filter(area_covered, data.consensus_score < 2)

area_manual<-read.csv( "FISHc/FISHc_data/manual_reviews/fishc_area_review_kq.csv") %>% 
  anti_join(bad_urls) %>% #remove bad subjects 
  drop_na('front')  %>% # drop cards that don't have url 
  select(subject_id, data, column_name) 

#take good obs and join back with manual 
area_covered_good<-filter(area_covered, data.consensus_score >= 2) %>% 
  select(subject_id, data.consensus_text, column_name) %>% 
  rename(data = data.consensus_text) %>% 
  rbind(area_manual)%>% 
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(data)) 

##### depth of collection ######################################
depth<-filter(fishc_text, column_name == "depth_of_collection" ) %>% 
  select(-c(task, data.number_views))

#many of these are discrepancies in the way people wrote 14ft-15ft vs 14'-15' vs 14feet or space differences 
#depth_bad<-filter(depth, data.consensus_score < 1.5)

depth_manual<-read.csv( "FISHc/FISHc_data/manual_reviews/fishc_depth_review_cb.csv") %>% 
  select(subject_id, data, column_name) 

#take good depth and join back with manual
depth_good<-filter(depth, data.consensus_score >= 1.5) %>% 
  select(subject_id, data.consensus_text, column_name) %>% 
  rename(data = data.consensus_text) %>% 
  rbind(depth_manual) %>% 
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(data)) 

##### field_no ######################################
field_no<-filter(fishc_text, column_name == "field_no" ) %>% 
  select(-c(task, data.number_views))

#field_bad<-filter(field_no, data.consensus_score < 2)

field_manual<-read.csv( "FISHc/FISHc_data/manual_reviews/fishc_fieldno_review_kt.csv") %>% 
  select(subject_id, data, column_name)

field_good<-filter(field_no, data.consensus_score >= 2)  %>% 
  select(subject_id, data.consensus_text, column_name) %>% 
  rename(data = data.consensus_text) %>% 
  rbind(field_manual) %>% 
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(data))  



##### point of examination ######################################
site<-filter(fishc_text, column_name == "point_of_examination" ) %>% 
  select(-c(task, data.number_views))

#site_bad<-filter(site, data.consensus_score < 2)

site_manual<-read.csv( "FISHc/FISHc_data/manual_reviews/fishc_site_review_cb.csv") %>% 
  select(subject_id, data, column_name)

site_good<-filter(site, data.consensus_score >= 2)  %>% 
  select(subject_id, data.consensus_text, column_name) %>% 
  rename(data = data.consensus_text) %>% 
  rbind(site_manual) %>% 
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(data))

##### time ######################################
time<-filter(fishc_text, column_name == "time") %>% 
  select(-c(task, data.number_views))
#time_bad<-filter(time, data.consensus_score < 2)
time_manual<-read.csv( "FISHc/FISHc_data/manual_reviews/fishc_time_cb.csv",  na.strings = c("", "NA")) %>% 
  select(subject_id, data, column_name)

time_good<-filter(time, data.consensus_score >= 2)  %>% 
  select(subject_id, data.consensus_text, column_name) %>% 
  rename(data = data.consensus_text) %>% 
  rbind(time_manual) %>% 
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(data))  

##### water surface temperature ######################################
surface_temp<-filter(fishc_text, column_name == "water_surface_temperature") %>% 
  select(-c(task, data.number_views)) %>% 
  separate(data.consensus_text, #extract data and units
           into = c("data1", "data2"), #separate ranges first
           sep= "-",   
           extra = 'merge') %>%
  mutate(min = str_extract(data1, regex("[\\d\\,\\.]+"))) %>% #extract the min; a number with commas and decimals ok
  mutate(units1 = str_extract(data1,  regex("[A-Za-z]+"))) %>% #extract min units 
  mutate(max = str_extract(data2,  regex("[\\d\\,\\.]+"))) %>% #extract the max
  mutate(units2 = str_extract(data2,  regex("[A-Za-z]+"))) %>% #extract max units 
  transform( min = sub('(.{2})(.+)', '\\1.\\2', min), #removed all of the decimal places in OpenRefine, adding back in
                max = sub('(.{2})(.+)', '\\1.\\2', max)) %>% 
  mutate(min = as.numeric(min), #change min data column to numeric , warning ok 
          max=as.numeric(max) ) %>% #change max data column to numeric , warning ok 
  select(-c(data1, data2))

#reorder columns  
surface_temp <- surface_temp[, c(1,2,3,7,8,9,10, 4, 5,6 )]

#standardize units ; determine unit based on card image; QA/QC min and max values
surface_temp_good<-filter(surface_temp, data.consensus_score >= 2) %>% 
  mutate(units1 = tolower(units1), 
         units2= tolower(units2)) %>%
  mutate(units1 = ifelse(is.na(units1), units2, units1)) %>% #incorporate units2 into units1 column 
  mutate(units= case_when( #standardize units into new column 
    units1 == 'about' ~ 'F', # if ~ then 
    units1 == 'air' ~ 'F',
    units1 == 'am' ~ 'F',
    units1 == 'and' ~ 'F',
    units1 == 'approx' ~ "F",  
    units1 == 'at' ~ 'F',
    units1 == 'average' ~ 'F',
    units1 == 'between' ~ 'F',
    units1 == 'by' ~ 'F',
    units1 == 'c' ~ 'C',
    units1 == 'celcius' ~ 'C',
    units1 == 'cold' ~ NA_character_,
    units1 == 'cool' ~ NA_character_,
    units1 == 'dec' ~ NA_character_,
    units1 == 'deg' ~ NA_character_, 
    units1 == 'degree' ~ NA_character_, 
    units1 == 'degrees' ~ NA_character_, 
    units1 == 'degreess' ~ NA_character_, 
    units1 == 'east' ~ 'F',
    units1 == 'f' ~ 'F',
    units1 == 'fahrenheit' ~ "F", 
    units1 == 'feet' ~ 'F',
    units1 == 'frozen' ~ 'ice',
    units1 == 'hd' ~ 'F',
    units1 == 'high' ~ NA_character_,
    units1 == 'ice' ~ 'ice',
    units1 == 'in' ~ 'F',
    units1 == 'inch' ~ 'ice',
    units1 == 'inlet' ~ 'F',
    units1 == 'lake' ~ 'F',
    units1 == 'low' ~ 'F',
    units1 == 'missed' ~ NA_character_,
    units1 == 'none' ~ NA_character_,
    units1 == 'not' ~ NA_character_,
    units1 == 'o' ~ "F",
    units1 == 'on' ~ "F",
    units1 == 'over' ~ "F",
    units1 == 'pm' ~ "F",
    units1 == 'preceding' ~ 'F',
    units1 == 'pres' ~ 'F',
    units1 == 'present' ~ 'F',
    units1 == 'see' ~ NA_character_,
    units1 == 'set' ~ 'F',
    units1 == 'shore' ~ 'F',
    units1 == 'spring' ~ 'F',
    units1 == 'sta' ~ 'F',
    units1 == 'surface' ~ NA_character_,
    units1 == 'thermometer' ~ NA_character_,
    units1 == 'to' ~ NA_character_,
    units1 == 'unknown' ~ NA_character_,
    units1 == 'varied' ~ NA_character_,
    units1 == 'various' ~ NA_character_,
    units1 == 'warm' ~ NA_character_,
    units1 == 'water' ~ 'F',
    TRUE ~ NA_character_
  )) %>%
  mutate(units= ifelse((min < 32 & !is.na(min)), 'C', units)) %>% #if the number is <32 then Celsius and not NA because want to keep NAs
  mutate(units = ifelse(is.na(units), 'F', units)) %>% #all NAs put to Fahrenheit 
  mutate(min = ifelse(subject_id == 59105795 | subject_id == 59969360 |subject_id ==  59969361, 62, min), #specific fields noted to still have issues - change the value
         max = ifelse(subject_id == 59965777 | subject_id == 59965780 |subject_id ==  59965781 |subject_id ==  59965782, 65, max), 
         max = ifelse(subject_id == 58750469, 43, max),
         min = ifelse(subject_id ==59969816, 0.3, min),
         min = ifelse(subject_id ==59106412, 66, min),
         max = ifelse(subject_id ==59106412 | subject_id ==58751875 | subject_id ==58751874 | subject_id ==58751876, 68, max),
         min = ifelse(subject_id ==59106436 | subject_id ==59106439, 66, min),
         max = ifelse(subject_id ==59106436 | subject_id ==59106439, 76, max),
         min = ifelse(subject_id ==59106441, 59, min),
         max = ifelse(subject_id ==59106441, 63, max),
         min = ifelse(subject_id ==59969255 |subject_id == 59964211 | subject_id == 58748928 | subject_id == 58748929|subject_id == 58748930|subject_id ==58748931 |subject_id ==58748933  , NA, min), 
         max = ifelse(subject_id ==59105574 | subject_id ==59968405 | subject_id ==59968408 | subject_id ==58752453 | subject_id ==59106167 | subject_id ==59106149 | subject_id ==58752016, NA, max),
         max = ifelse(subject_id ==59965878, 79, max), 
         max = ifelse(subject_id ==59967637, 71, max), 
         min = ifelse(subject_id ==59106160 | subject_id ==58751644, 74, min),
         min = ifelse(subject_id ==59106167 | subject_id ==59106168 | subject_id ==59106161 | subject_id ==58752625, 72, min),
         min = ifelse(subject_id ==  59106158 | subject_id== 59106162, 73, min), 
         min = ifelse(subject_id == 59963677, 65, min), 
         max = ifelse(subject_id == 59963677, 75, max), 
         min = ifelse(subject_id == 58752959, 60, min),
         min = ifelse(subject_id == 58752016, 70, min),
         max = ifelse(subject_id ==59965474, 19.3, max), 
         max = ifelse(subject_id ==58749113 | subject_id ==58749127, 26, max), 
         max = ifelse(subject_id ==58751994, 64, max),
         max = ifelse(subject_id ==59968287 ,68, max),
        max = ifelse(subject_id ==  59969551 | subject_id == 58751816,70,  max),
        max = ifelse(subject_id == 59969649 , 75, max),
        max = ifelse(subject_id == 58750973, 65, max), 
        max = ifelse(subject_id == 58750964, 80, max), 
        max = ifelse(subject_id == 59105449, 84, max), 
        min = ifelse(subject_id == 59106419, 59, min), 
        max = ifelse(subject_id == 59106419, 60, max), 
        min = ifelse(subject_id ==59106407 | subject_id ==59106417, 71, min), 
        max = ifelse(subject_id == 59106407| subject_id ==59106417, 70, max), 
        min = ifelse(subject_id ==59106410 | subject_id ==59106413, 63, min), 
        min = ifelse(subject_id ==59106424 | subject_id == 59106426, 62, min),
        min = ifelse(subject_id ==59106434, 64, min), 
        min = ifelse(subject_id ==59106435 | subject_id ==59106438, 71, min),
        min = ifelse(subject_id ==59106437, 68, min),
        min = ifelse(subject_id ==59106440, 76, min),
        min = ifelse(subject_id ==59967294, 67, min), #look at max values
        max = ifelse(subject_id ==59966989 | subject_id ==59965711 | subject_id ==59969770 | subject_id ==58750355 | subject_id ==58751363 | subject_id == 58752368 | subject_id == 59968409 | subject_id == 59967873 | subject_id == 59968227, NA, max), 
        max = ifelse(subject_id ==58752960, 64, max), 
        min = ifelse(subject_id ==58752960, 61, min), 
        max = ifelse(subject_id ==59106433, 63, max), 
        min = ifelse(subject_id ==59106433, 62, min), 
         )  %>% 
  select(subject_id, min, max, units, column_name, front, back) %>% 
  mutate(comments = ifelse(units=="ice", "ice", NA)) %>% #keep ice as a comment 
  mutate(units = ifelse(is.na(min), NA, units))

#filter out <2 for manual review 
#surface_temp_bad<-filter(surface_temp, data.consensus_score < 2)
#read in manual temps  
temp_manual<-read.csv( "FISHc/FISHc_data/manual_reviews/fishc_temp_review_kt.csv",  na.strings = c("", "NA")) %>% 
  select(subject_id, min, max, units1, column_name, front, back) %>% 
  rename(units = units1) %>% 
  mutate(comments= NA) %>% 
  mutate(min = ifelse(subject_id == 59964562, NA, min), #specific fields change to just a value 
         min = ifelse(subject_id == 59968801, 82, min),
         min = ifelse(subject_id == 59968465 | subject_id == 59967544, 75, min),
         min = ifelse(subject_id == 59969563, 74, min), 
         min = ifelse(subject_id == 59966521, 42, min), 
         min = ifelse(subject_id == 59969331, 38, min), 
         min = as.numeric(min)) 

#bind together data and convert F to C (32°F − 32) × 5/9 = 0°C
temps<- rbind(surface_temp_good, temp_manual) %>% #
  mutate(units= ifelse(min < 32, 'C', units)) %>%  #if the number is <32 then Celsius 
  mutate(units = ifelse(is.na(units), 'F', units)) %>% #all NAs put to Fahrenheit 
  mutate(min_c= ifelse(units == 'F', round((min-32)*(5/9),3), min)) %>% #convert Fahrenheit to Celsius 
  mutate(max_c= ifelse(units == 'F', round((max-32)*(5/9),3), max)) %>%
  select(subject_id, min_c, max_c, column_name) %>% #select wanted columns
  pivot_wider( id_cols = c(subject_id), #pivot table so each variable is a column 
               names_from = column_name ,
               values_from = c('min_c','max_c'),
               values_fill =  NA) %>%
  rename(temp_surface_min_c='min_c_water_surface_temperature', temp_surface_max_c='max_c_water_surface_temperature')

##### remarks ######################################
remarks<-filter(fishc_text, column_name == "remarks") %>% 
  select(-c(task, data.number_views))
#filter out manual review remarks, remove data that should be NA 
#remarks_bad<-filter(remarks, data.consensus_score < 2) %>%
 # mutate('data.consensus_text' = ifelse(data.consensus_text=='none' | data.consensus_text=='None' | data.consensus_text=='nothing' | data.consensus_text=='Nothing Written' | data.consensus_text== "N.A", NA, data.consensus_text )
#  ) %>% 
 # drop_na(data.consensus_text)

remarks_manual<-read.csv( "FISHc/FISHc_data/manual_reviews/fishc_remarks_kt.csv",  na.strings = c("", "NA")) %>% 
  select(subject_id, data, column_name)

remarks_good<-filter(remarks, data.consensus_score >= 2)  %>% 
  select(subject_id, data.consensus_text, column_name) %>% 
  rename(data = data.consensus_text) %>% 
  rbind(remarks_manual) %>% 
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(data))  

########## merge all the data back together ############################
all_text_clean<- full_join(site_good, gear_all) %>%  #point of examination #13371 and gear info #13400
                full_join(area_covered_good) %>% # area covered #6131
                full_join(depth_good) %>%  #depth of collection #13053
                full_join(time_good) %>% #time of sampling #12973
                full_join(temps) %>% #surface water temp #11661
                full_join(field_good) %>% #field_no #10574
                full_join(remarks_good)  #remarks on card #6713

#write.csv(all_text_clean, "FISHc/FISHc_data/final_data/fishc_text.csv", row.names = FALSE)


 #### ADD DEC DATA #### 
fishc_text_dec<-read.csv("FISHc/FISHc_data/text_data/text_reducer_fishc_dec.csv", na.strings = c("", "NA")) %>% #text is ALL
  select(subject_id, task, 'data.aligned_text', 'data.number_views', 'data.consensus_score','data.consensus_text', 'URL.4', "URL.8") %>%
  rename( front= 'URL.4', back= "URL.8") %>% 
  drop_na('data.number_views')%>%
  left_join(fishc_task_values, by=c('task' = 'code')) %>% # join with the task values to get the actual names
  naniar::replace_with_na(replace = list(data.consensus_text = c('-','_', '--', '- -', '---', '- - -', '----', '?', '??????', '......?'))) %>% #function that replaces all of a value with NA 
  drop_na('data.consensus_text') %>% 
  mutate(column_name=tolower(column_name))
#first do an anti_join so that we don't keep anything that is a duplicate from the first dataset
fishc_text_dec<-anti_join(fishc_text_dec, fishc_text, by=c('subject_id')) #returns rows without a match in y 

#* gear ######################################
#filter out the ones for manual review, these were checked and fixed below
#fishc_gear_bad<-filter(gear, data.consensus_score < 2)
gear<-filter(fishc_text_dec, column_name == "gear_used_kind" | column_name == "gear_used_mesh" | column_name == "gear_used_length" ) %>% 
  select(-c(task, data.number_views)) %>% 
  mutate(data.consensus_text=tolower(data.consensus_text), #make all text lowercase
         data.consensus_text= ifelse(subject_id == 82937390 & column_name == 'gear_used_length', NA, data.consensus_text),
         data.consensus_text= ifelse(subject_id == 82937363 & column_name == 'gear_used_mesh', NA, data.consensus_text),
         data2=gsub("'", 'feet', data.consensus_text), # replace characters with units
         data2=gsub("ft\\.", 'feet', data2),
         data2=gsub("ft", 'feet', data2), 
         data2=gsub('"', 'inches', data2), 
         data2=gsub(" 1/2", '.5', data2), #replace fractions
         data2=gsub("1/2", '.5', data2), 
         data2=gsub(" 1/3", '.33', data2), 
         data2=gsub("1/4", '.25', data2), 
         data2=gsub(" 1/4", '.25', data2), 
         data2=gsub('3/4', '.75', data2), 
         data2=gsub(" 3/4", '.75', data2), 
         data2=gsub('c. s.', 'common', data2), #replace cs with common 
  )%>% 
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(data2)
  ) %>%          #could use | or but then get all characters in one column
  mutate(   gr1 = str_extract(gear_used_kind,"(?<=^| )(bag).*?(?=$| )"), #The regular expression matches the word preceded by a space or the start of the string (?<=^| ), and then as few characters as possible .*? until the next space or the end of the string (?=$| )
            gr2 = str_extract(gear_used_kind,"(?<=^| )(common).*?(?=$| )"),
            gr3 = str_extract(gear_used_kind,"(?<=^| )(gill).*?(?=$| )"),
            gr4 = str_extract(gear_used_kind,"(?<=^| )(exp).*?(?=$| )"),
            gr5 = str_extract(gear_used_kind,"(?<=^| )(bar).*?(?=$| )"),
            gr6 = str_extract(gear_used_kind,"(fyke).*?(?=$| )"),
            gr7 = str_extract(gear_used_kind,"(trap).*?(?=$| )"),
            gr8 = str_extract(gear_used_kind,"(?<=^| )(boom).*?(?=$| )"),
            gr9 = str_extract(gear_used_kind,"(?<=^| )(electro).*?(?=$| )"), 
            gr10 = str_extract(gear_used_kind,"(?<=^| )(shock).*?(?=$| )"), 
            gr11=str_extract(gear_used_kind,"(?<=^| )(hook).*?(?=$| )"),
            gr12=str_extract(gear_used_kind,"(?<=^| )(rod).*?(?=$| )"),
            gr13=str_extract(gear_used_kind,"(?<=^| )(seine).*?(?=$| )"),
            gr14=str_extract(gear_used_kind,"(?<=^| )(spear).*?(?=$| )"),
            gr15=str_extract(gear_used_kind,"(?<=^| )(angling).*?(?=$| )"),
            gr16 = str_extract(gear_used_mesh,"(?<=^| )(exp).*?(?=$| )"), #capture exp from mesh 
            gr17 = str_extract(gear_used_mesh,"(?<=^| )(bar).*?(?=$| )"), #capture bar from mesh 
            gr18=str_extract(gear_used_kind,"(?<=^| )(pulse).*?(?=$| )"),
            gr19=str_extract(gear_used_kind,"(?<=^| )(trawl).*?(?=$| )"),
            gr20=str_extract(gear_used_kind,"(rotenone).*?(?=$| )"),
            gr21=str_extract(gear_used_kind,"(?<=^| )(hand).*?(?=$| )"),
            gr22=str_extract(gear_used_kind,"(?<=^| )(dip).*?(?=$| )"),
            gr23=str_extract(gear_used_kind,"(?<=^| )(rough).*?(?=$| )"),
            gr24= str_extract(gear_used_length,"(?<=^| )(bag).*?(?=$| )"), #capture bag seine from length (sometimes appears here)
            gr25 = str_extract(gear_used_mesh,"(?<=^| )(common).*?(?=$| )"), #capture common sense seine from mesh 
            
            gr4 = ifelse(is.na(gr4), gr16, gr4), #incorporate exp mesh if not captured from gear used kind
            gr5 = ifelse(is.na(gr5), gr17, gr5), #incorporate bar mesh if not captured from gear used kind
            gr4 = ifelse(is.na(gr4) & is.na(gr5), gr3, gr4), #if both gr4 and 5 are NA incorporate gill gr3 into gear 4 (exp) 
            gr1= ifelse(is.na(gr1), gr24, gr1), #incorporate bag from length gr24 into the gr1 bag column 
            gr2= ifelse(is.na(gr2), gr25, gr2), #incorporate common from mesh into the gr2 common column
            gr1 = ifelse(is.na(gr1) & is.na(gr2), gr13, gr1), #if both gr1 and gr2 are NA incorporate seine gr13 into gr1 bag 
            gr8 = ifelse(is.na(gr8), gr10, gr8), #incorporate shock into gear 8
            gr11 = ifelse(is.na(gr11), gr12, gr11), #incorporate rod into gear 11 hook
            gr11 = ifelse(is.na(gr11), gr15, gr11), #incorporate angling into gear 11 hook 
            gr8 = ifelse(is.na(gr8), gr18, gr8), #incorporate pulse into gear 8 
  ) %>% 
  select(-c(gr3, gr10, gr12, gr13, gr15, gr16, gr17, gr18, gr23, gr24, gr25)) %>%   #check to make sure every row has been assigned a gear  #gear_good$review<-apply(gear_good[, 5:18], MARGIN = 1, FUN = function(x) all(is.na(x)))
  pivot_longer( #pivot back to longer so that each separate gear is on a new row 
    cols = starts_with("gr"),
    names_to = "gear",
    names_prefix = "gr",
    values_to = "gear2",
    values_drop_na = TRUE
  ) %>%
  mutate(gear2=gsub("[[:punct:]]|[[:digit:]]","",gear2), #remove random punctuation 
         gear= case_when( # if ~ then #standardize gear2 names 
           gear2 == 'bag' ~ 'seine',  
           gear2 == 'bar' ~ 'gillnet', 
           gear2 == 'boom' ~ 'shock',
           gear2 == 'boom-shocker' ~ 'shock',
           gear2 == 'boomshocker' ~ 'shock',
           gear2=="common" ~ "seine", 
           gear2=="electrofishing" ~ "shock", 
           gear2=="electroshocker" ~ "shock", 
           gear2=="electroshocking" ~ "shock", 
           gear2=="exp" ~ "gillnet", 
           gear2=="exper" ~ "gillnet", 
           gear2=="experimental" ~ "gillnet", 
           gear2=="fyke" ~ "fyke net", 
           gear2=="gill" ~ "gillnet", 
           gear2=="pulse" ~ "shock", 
           gear2=="seine" ~ "seine",
           gear2=="shocker" ~ "shock",
           gear2=="trap" ~ "trap net"
         )
  ) %>% #standardize gear 2 
  mutate(gear2 = case_when(
    gear2=="exp" ~ "exp", 
    gear2=="exper" ~ "exp", 
    gear2=="experimental" ~ "exp", 
    gear2=="bar" ~ "straight",
    gear2=="pulse" ~ "shocker",
  TRUE ~ gear2
)) %>% 
  rename(type = gear, descript = gear2) %>% 
  group_by(subject_id) %>% #need each row to be a unique card 
  mutate(ID = paste0("gear", 1:n())) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(subject_id, gear_used_kind, gear_used_mesh, gear_used_length),
              names_from = c(ID), 
              values_from = c(type, descript)
              ) 

#* area covered ####
area_covered<-filter(fishc_text_dec, column_name == "area_covered" ) %>% 
  select(-c(task, data.number_views))

#filter out the ones <50% for manual review 
area_covered_bad<-filter(area_covered, data.consensus_score < 2) #remove these, they are recorded in "gear"

#take good obs and join back with manual 
area_covered_good<-filter(area_covered, data.consensus_score >= 2) %>% 
  select(subject_id, data.consensus_text, column_name) %>% 
  rename(data = data.consensus_text) %>% 
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(data)) 

#* depth of collection ######################################
depth<-filter(fishc_text_dec, column_name == "depth_of_collection" ) %>% 
  select(-c(task, data.number_views)) %>% 
  mutate(data.consensus_text = ifelse(subject_id == "82937384", '6in-12feet', data.consensus_text))%>% 
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(data.consensus_text)) 

#these are all correct except one, which is corrected above 
#depth_bad<-filter(depth, data.consensus_score < 2)

#*field_no ######################################
field_no<-filter(fishc_text_dec, column_name == "field_no" ) %>% 
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(data.consensus_text))  
#all data are >2 consensus 


#* point of examination ######################################
site<-filter(fishc_text_dec, column_name == "point_of_examination" ) %>% 
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(data.consensus_text))  

#site_bad<-filter(site, data.consensus_score < 2) # these are all correct 


#* time ######################################
time<-filter(fishc_text_dec, column_name == "time") %>% 
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(data.consensus_text))  

#time_bad<-filter(time, data.consensus_score < 2)# these are all correct 

#*water surface temperature ######################################
#surface_temp_bad<-filter(surface_temp, data.consensus_score < 2) #these are all good 
surface_temp<-filter(fishc_text_dec, column_name == "water_surface_temperature") %>% 
  select(-c(task, data.number_views)) %>% 
  separate(data.consensus_text, #extract data and units
           into = c("data1", "data2"), #separate ranges first
           sep= "-",   
           extra = 'merge') %>%
  mutate(min = str_extract(data1, regex("[\\d\\,\\.]+"))) %>% #extract the min; a number with commas and decimals ok
  mutate(units1 = str_extract(data1,  regex("[A-Za-z]+"))) %>% #extract min units 
  mutate(max = str_extract(data2,  regex("[\\d\\,\\.]+"))) %>% #extract the max
  mutate(units2 = str_extract(data2,  regex("[A-Za-z]+"))) %>% #extract max units 
  mutate(min = as.numeric(min), #change min data column to numeric , warning ok 
         max=as.numeric(max) ) %>% #change max data column to numeric , warning ok 
  select(-c(data1, data2)) %>% 
  mutate(units1 = ifelse(is.na(units1), units2, units1)) %>% #incorporate units2 into units1 column 
  mutate(units1= case_when( 
    units1 == 'East' ~ 'F', # if ~ then 
    units1 == 'none' ~ NA_character_,
    units1 == 'ice' ~ NA_character_,
    TRUE ~ units1 )
  ) %>% 
  mutate(units1 = ifelse(is.na(units1), 'F', units1)) %>% #anything that is NA is F 
  select(subject_id, min, max, units1, column_name, front, back) %>%
  mutate(min_c= ifelse(units1 == 'F', round((min-32)*(5/9),3), min)) %>% #convert Fahrenheit to Celsius 
  mutate(max_c= ifelse(units1 == 'F', round((max-32)*(5/9),3), max)) %>%
  select(subject_id, min_c, max_c, column_name) %>% #select wanted columns
  pivot_wider( id_cols = c(subject_id), #pivot table so each variable is a column 
               names_from = column_name ,
               values_from = c('min_c','max_c'),
               values_fill =  NA) %>%
  rename(temp_surface_min_c='min_c_water_surface_temperature', temp_surface_max_c='max_c_water_surface_temperature')

#* remarks ######################################
remarks<-filter(fishc_text_dec, column_name == "remarks") %>% 
      mutate(data.consensus_text = ifelse(subject_id == 82937360, NA, data.consensus_text)) %>% 
    pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(data.consensus_text))  

#remarks_bad<-filter(remarks, data.consensus_score < 2)#all good except one, which is corrected above  


########## merge all the data back together ############################
all_dec_clean<- full_join(site, gear) %>%  
  full_join(area_covered_good) %>%
  full_join(depth) %>%  
  full_join(time) %>% 
  full_join(surface_temp) %>% 
  full_join(field_no) %>% 
  full_join(remarks)  %>% 
  mutate(type_gear4 = NA, 
         descript_gear4 = NA) # add columns to join with the other fishc data 

fichc_text<-rbind(all_text_clean, all_dec_clean)

write.csv(fichc_text, "FISHc/FISHc_data/final_data/fishc_text.csv", row.names = FALSE)


