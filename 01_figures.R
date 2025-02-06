#building SDMs and looking at correlation between predicted probability of occurrence/habitat suitability 
#and occurrence/abundance/growth.
#starting with LMB then moving to other species.
# loads library
library(dplyr)
library(maptools)
library(lubridate)
library(tidyr)
library(ggplot2)
library(mapdata)
library(sf)

#### data wrangling ####
#lake summary card
#59070593 card should be 2-361 new_key
all_fishc<-read.csv("FISHc/FISHc_data/final_data/fishc_qaqc_Jan2025.csv")

points<-read.csv("museum_matching/data/Humphries_modified.csv")

fish_mapping<-all_fishc %>% 
                left_join(points) %>% 
  distinct(new_key, .keep_all = TRUE) %>% 
  dplyr::select(new_key, LONG_DD, LAT_DD)

grow_mapping<-all_grow %>% 
  left_join(points) %>% 
  distinct(new_key, .keep_all = TRUE) %>% 
  dplyr::select(new_key, LONG_DD, LAT_DD)

summ_mapping<-all_summ %>% 
  left_join(points) %>% 
  distinct(new_key, .keep_all = TRUE) %>% 
  dplyr::select(new_key, LONG_DD, LAT_DD)

#what don't we have new_keys for 
n_distinct(all_summ$subject_id) 
n_distinct(all_grow$subject_id) 
n_distinct(all_fishc$subject_id) 
summ_nolake<-filter(all_summ, is.na(new_key)) %>% #12 cards left unmatched 
  select(new_key, lakename, county)
fishc_nolake<-filter(all_fishc, is.na(new_key)) %>% #28 cards left unmatched 
  select(new_key, lakename, county)
grow_nolake<-all_grow %>% distinct(subject_id, .keep_all = TRUE) %>%
  filter(is.na(new_key)) %>% #36 cards left unmatched 
  select(new_key, lakename, county)
(12 + 28+ 36) /(13444 + 4134 + 1876) *100 # <1% of the cards 0.4%

all<-rbind(summ_nolake, fishc_nolake, grow_nolake)
n_distinct(all$lakename, all$county) #51 lakes 

#* map of all lakes matched  #### 
#read in MI map
data("us_states", package = "spData")
us_states = st_transform(us_states, crs = "+proj=longlat +datum=WGS84 +no_defs") 
MI<-dplyr::filter(us_states, NAME == 'Michigan')
plot(MI)

# plot
(fish_map<-ggplot() +
    geom_sf(data=MI, aes(), fill = "white") + 
    geom_point(data=fish_mapping, aes(x = LONG_DD, y = LAT_DD ), size=0.75) +
    theme_void() +
    theme( panel.background = element_rect(colour = "black"))
)

# plot
(grow_map<-ggplot() +
    geom_sf(data=MI, aes(), fill = "white") + 
    geom_point(data=grow_mapping, aes(x = LONG_DD, y = LAT_DD ), size=0.75) +
    theme_void() +
    theme( panel.background = element_rect(colour = "black"))
)

# plot
(summ_map<-ggplot() +
    geom_sf(data=MI, aes(), fill = "white") + 
    geom_point(data=summ_mapping, aes(x = LONG_DD, y = LAT_DD), size=0.75) +
    theme_void() +
    theme( panel.background = element_rect(colour = "black"), 
           (panel.background = element_blank()))
)

fig1<-cowplot::plot_grid(summ_map, fish_map,  grow_map, nrow=1, labels = c('a', 'b', 'c'), 
                         vjust=5)


ggsave("/Users/katelynking/Desktop/fig1.png",fig1, bg='#ffffff', 
       width = 8,
       height = 4,
       units = c("in"))


#* freq of years #### 
(fish_year<-all_fishc%>% 
  ggplot(aes(x = begin_date_year)) + 
  geom_histogram(bins = 70, color = "black", fill = "lightblue") + 
  xlab("sample year") + ylab("number of cards") + 
   theme_bw() +
  theme( panel.background = element_rect(colour = "black"), 
         (panel.background = element_blank()))
)

summ_year<-all_summ%>% 
  ggplot(aes(x = begin_date_year)) + 
  geom_histogram(bins = 70, color = "black", fill = "lightblue") + 
  xlab("sample year")  + ylab("number of cards") + 
  theme_bw() +
  theme( panel.background = element_rect(colour = "black"), 
         (panel.background = element_blank()))


grow_year<-all_grow%>% 
  group_by(subject_id) %>%
  ggplot(aes(x = begin_date_year)) + 
  geom_histogram(bins = 70, color = "black", fill = "lightblue") + 
  xlab("sample year")  + ylab("number of cards") +
  theme_bw() +
  theme( panel.background = element_rect(colour = "black"), 
         (panel.background = element_blank()))



fig2<-cowplot::plot_grid(summ_year, fish_year, grow_year, nrow=1, labels = c('a', 'b', 'c'))

ggsave("/Users/katelynking/Desktop/fig2.png",fig2,
       width = 8,
       height = 4,
       units = c("in"))

range(all_summ$begin_date_year , na.rm = TRUE)
range(all_grow$begin_date_year, na.rm = TRUE)
range(all_fishc$begin_date_year, na.rm = TRUE)

#* what fish species on records #### 
fish_sp<-dplyr::select(all_fishc,subject_id, begin_date_year, bandedkillifishmenonakillifish:yellowbullhead)
fish_sp[ , 3:57 ][ fish_sp[ , 3:57 ] > 0 ] <- 1 #set data to 1s and 0s 
fish_sp[, 3:57 ] <- sapply(fish_sp[, 3:57 ],as.numeric)

fishes<-fish_sp %>% 
pivot_longer(cols = bandedkillifishmenonakillifish:yellowbullhead,
             names_to = "species",
             values_to = "pres") %>% 
  drop_na(pres)

summary_sp<-fishes %>%
  group_by(species) %>%
  summarize(Freq=sum(pres))

summary_sp_year<-fishes %>%
  group_by(species, begin_date_year) %>%
  summarize(Freq=sum(pres)) %>% 
  mutate(decade = case_when( # if ~ then
    begin_date_year >=1920 & begin_date_year<1930 ~ 1920,
    begin_date_year >=1930 & begin_date_year<1940 ~ 1930,
    begin_date_year >=1940 & begin_date_year<1950 ~ 1940,
    begin_date_year >=1950 & begin_date_year<1960 ~ 1950,
    begin_date_year >=1960 & begin_date_year<1970 ~ 1960,
    begin_date_year >=1970 & begin_date_year<1980 ~ 1970,
    begin_date_year >=1980 & begin_date_year<1990 ~ 1980,
    begin_date_year >=1990 & begin_date_year<2000 ~ 1990)
  )

#* 5 most frequent
(most_freq<-summary_sp%>% 
  filter(Freq > 3450) %>% 
  arrange(Freq) %>% 
  mutate(sp_new = c(
         'northern_pike', 'pumpkinseed', 'white_sucker', 'bluegill', 'yellow_perch')) %>%
  ggplot(aes(x = reorder(sp_new, -Freq), y= Freq)) + 
  geom_col(color = "black", fill = "lightblue") + 
  xlab("species") + ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + 
    theme_bw() +
    theme( panel.background = element_rect(colour = "black"), 
           (panel.background = element_blank()))
  
)

#maybe try stacked
sp_year_plot<-summary_sp_year%>% 
  filter(species == 'bluegill' | species == 'northernpike' | species ==  'perchyellowperch' | species == 'pumpkinseed'  | species == 'commonsuckerwhitesucker') %>% 
  mutate(species_name = case_when( # if ~ then
    species == 'bluegill' ~ 'bluegill',
    species == 'northernpike' ~ 'northern_pike',
    species ==  'perchyellowperch' ~ "yellow_perch",
    species == 'pumpkinseed' ~ "pumpkinseed",
    species == 'commonsuckerwhitesucker' ~ 'white_sucker' )
    )%>%
  ggplot(aes(fill =decade, x = species_name, y= Freq)) + 
  geom_bar(position = "stack", stat= "identity") + 
  xlab("species") + ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + 
  theme_bw() +
  theme( panel.background = element_rect(colour = "black"), 
         (panel.background = element_blank()))


#least_freq<-summary_sp%>% 
#  filter(Freq < 44) %>% 
 # arrange(Freq) %>% 
  #mutate(sp_new = c(
   # 'white_bass','sauger', 'shortnose_gar',  "blackstrip_topminnow", 'spotted_gar')) %>%
  #ggplot(aes(x = reorder(sp_new, -Freq), y= Freq)) + 
  #geom_col(color = "black", fill = "lightblue") + 
  #xlab("species") + ylab("Frequency") +
  #theme(axis.text.x = element_text(angle = 45, vjust = 0.5))


fig3<-cowplot::plot_grid(most_freq, least_freq, nrow=1, labels = c('a', 'b'))

ggsave("/Users/katelynking/Desktop/sp_year_plot.png",sp_year_plot,
       width = 8,
       height = 4,
       units = c("in"))

#* what lake has most fishc records ####
fish_records<-fishc %>%
  group_by(new_key) %>%
  summarize(Freq=n())

green_lake<-filter(fishc, new_key == "81-46" )

green_lake[, 32:86 ] <- sapply(green_lake[, 32:86 ],as.numeric)

fishes<-green_lake %>% 
  group_by(begin_date_year) %>% 
  pivot_longer(cols = c(32:86),
               names_to = "species",
               values_to = "pres") %>% 
  drop_na(pres) %>% 
  filter(pres > 0)

#*cisco abundance #### 
cisco_dat<-select(fishc,subject_id, new_key, lakename, county, cisco, begin_date_year) %>% 
  mutate(cisco = ifelse(cisco == 'checkmarksymbol', '0', cisco), 
         cisco = as.numeric(cisco), 
         cisco = coalesce(cisco, 0), #this puts the NAs to 0s
         pres = ifelse(cisco > 0, "yes", "no")) %>%
  left_join(metadatabase) %>% #try to get lat/lon 
  group_by(new_key) %>% 
  slice_max(cisco) %>% # keep ones with data 
  distinct(new_key, .keep_all = TRUE) %>% # remove duplicate cards (should now only be 0s that we save one of)
  mutate(dataset = "fishc") %>% 
  select(-c(Lake_Name))

hist(cisco_dat$cisco)

cisco_SUMM_dat<-select(SUMM_fish,subject_id, new_key, lakename, county, cisco_herring_whitefish, begin_date_year) %>% 
  mutate(cisco = coalesce(cisco_herring_whitefish, 0), #this puts the NAs to 0s
         pres = ifelse(cisco > 0, "yes", "no")) %>%
  left_join(metadatabase) %>% #try to get lat/lon 
  group_by(new_key) %>% 
  slice_max(cisco) %>% # keep rows with data (1)
  distinct(new_key, .keep_all = TRUE) %>%  # remove multi lake samples (e.g. multiple years in the same lake) (should now only be 0s that we save one of)
  select(-c(cisco_herring_whitefish, Lake_Name)) %>% #remove this column so that there is only the cisco column 
  mutate(dataset = "summ")

#pull out presence from both datasets and see if there is any new information 
cisco_pres<-cisco_dat %>% 
  filter(pres == "yes")
cisco_SUMM_pres<-cisco_SUMM_dat %>% 
  filter(pres == "yes")

setdiff( cisco_SUMM_pres$new_key, cisco_pres$new_key) #CHECK OUT UNique new_keys

cisco_all<-rbind(cisco_pres, cisco_SUMM_pres) %>% 
            distinct(new_key, .keep_all = TRUE) %>% 
  mutate(new_key = gsub("-", "_", new_key)) 

write.csv(cisco_all, "/Users/katelynking/Desktop/cisco_pres.csv")

#presence -
cisco_pres_map<-MI_basemap + geom_point(data=cisco_dat, aes(x = LONG_DD, y = LAT_DD, colour = c(pres)), size=1 ) +  
  theme_bw() + 
  scale_color_manual(values=c("gray", "darkblue")) +
  labs(colour="cisco") + 
  theme(legend.position = c(0.25, 0.25))

ggsave("/Users/katelynking/Desktop/cisco_pres.png",cisco_pres_map )

#abundance 
cis_abund<-filter(cisco_dat, cisco >0 & cisco <100)
cisco_abund_map<-MI_basemap + geom_point(data=cis_abund, aes(x = LONG_DD, y = LAT_DD, colour = c(cisco)), size=2) +  
  theme_bw() + 
  scale_colour_gradient(low = "gray",high = "darkblue") +
  labs(colour="cisco")  + 
  theme(legend.position = c(0.25, 0.25))

plot(cis_abund$cisco)

ggsave("/Users/katelynking/Desktop/cisco_abund_map.png",cisco_abund_map )

#* gear ####
#summary of gears used 
barplot(prop.table(table(fishc$type_gear1))) +
  scale_x_discrete(guide = guide_axis(angle = 90)) 

#hard to see the ones that were only used once 
ggplot(fishc, aes(x=type_gear1)) +
  geom_bar()+
  scale_x_discrete(guide = guide_axis(angle = 90)) 

#combine back gear columns 
fishc<-fishc%>% 
  unite( col='all_gear', c('type_gear1', 'type_gear2', 'type_gear3', 'type_gear4'), na.rm = TRUE, sep=';', remove = FALSE)

#number of gears used per lake 
fish_sp<-select(fishc,subject_id, bandedkillifishmenonakillifish:yellowbullhead)

#### GROW FIGURES #### 
#for loop 
species_list = unique(all_grow$species)
species_plots = list()

#moving loop through species! 
for(i in species_list) {  
  print( ggplot(all_grow %>% 
                  filter(species == i), aes(x=as.factor(age_group), y=length_mean_mm)) + 
           geom_boxplot() + 
           ggtitle(i) +
           ylab("average length (mm)") + 
           xlab("age group")
  )
  Sys.sleep(2) #this moves through plots 
}

#saving plots to a list 
for(i in species_list) {  
  species_plots[[i]] = ggplot(all_grow %>% 
                                filter(species == i), aes(x=as.factor(age_group), y=length_mean_mm)) + 
    geom_boxplot() + 
    ggtitle(i) +
    ylab("average length (mm)") + 
    xlab("age group")
  print(species_plots[[i]])
  ggsave(species_plots[[i]], 
         file=paste0("plot_", i,".png"), dpi=300)
}

#select species with the most data, >200 obs
top_grow<-filter(all_grow, species == 'black_crappie' | species == 'bluegill' | species == 'brook_trout' |
                   species == 'brown_trout' | species == 'cisco' | species == 'common_white_sucker' | 
                   species == 'lake_trout' | species == 'largemouth_bass' | species == 'northern_pike'| 
                   species == 'pumpkinseed_sunfish' | species == 'rainbow_trout' | species == 'rock_bass' | 
                   species == 'smallmouth_bass' | species == 'tiger_muskie' | species == 'walleye' |species == 'yellow_perch') %>% 
  drop_na(age_group) %>% 
  filter(age_group <16) %>% #remove some outliers for plotting 
  mutate(species_name = case_when(species == 'black_crappie' ~ "black crappie n=4361",
                                  species == 'bluegill' ~ "bluegill n=10357", 
                                  species == 'brook_trout' ~ "brook trout n=293",
                                  species == 'brown_trout' ~ "brown trout n=395",
                                  species == 'cisco' ~ "cisco n=526", 
                                  species == 'common_white_sucker' ~ "white sucker n=464",
                                    species == 'lake_trout' ~ "lake trout n=236", 
                                    species == 'largemouth_bass' ~"largemouth bass n=7458",
                                    species == 'northern_pike'~"northern pike n=6277",
                                    species == 'pumpkinseed_sunfish'~"pumpkinseed n=5078",
                                    species == 'rainbow_trout' ~"rainbow trout n=447",
                                    species == 'rock_bass' ~"rock bass n=3131",
                                    species == 'smallmouth_bass' ~"smallmouth bass n=2836",
                                    species == 'tiger_muskie'~"tiger muskie n=218",
                                    species == 'walleye' ~"walleye  n=3940",
                                    species == 'yellow_perch'~"yellow perch n=9460")
         )

#facet wrap 
fig4<-ggplot(top_grow, aes(x=as.factor(age_group), y=length_mean_mm)) + 
  geom_boxplot() + 
  ylab("average length (mm)") + 
  xlab("age group") + 
  facet_wrap(~species_name,scales = "free" )
  

ggsave("/Users/katelynking/Desktop/fig4.png",fig4,
       width = 10,
       height = 8,
       units = c("in"))

#### SUMM CARD ####
map_summ <-left_join(SUMM,metadatabase) #get lat/lon 

#* DO figures #### 

DO  <-filter(SUMM, do_above_thermo_min_ppm < 30) %>% #remove outliers
  left_join(metadatabase) #try to get lat/lon 
plot(DO$max_depth_min_m, DO$do_above_thermo_min_ppm)

plot(SUMM$max_depth_min_m,SUMM$do_below_thermo_min_ppm)



#*shoreline ####
map_summ$shoreline_all<- map_summ$resorts_min_n + map_summ$liveries_min_n + map_summ$cottages_min_n + map_summ$hotels_homes_min_n

small<-filter(map_summ, shoreline_all <600)

shoreline<-MI_basemap + geom_point(data=filter(map_summ, shoreline_all <100), 
                          aes(x = LONG_DD, y = LAT_DD, colour = c(shoreline_all)), alpha = 0.8) +  
  theme_bw() + 
  labs(colour="# of buildings") + 
  theme(legend.position = c(0.25, 0.25))

ggsave("/Users/katelynking/Desktop/shoreline_map.png",shoreline )

#* TEMP ####
#add SEASON  
temp<-filter(map_summ, !is.na(temp_surface_min_c)) %>% 
  filter(temp_surface_min_c < 75) %>% #remove outliers 
  mutate(temp_season = case_when(begin_date_month == "June" ~ "summer",
                                 begin_date_month == "July" ~ "summer", 
                                 begin_date_month == "August" ~"summer", 
                                 begin_date_month == "September" ~ "fall",
                                 begin_date_month == "October" ~ "fall",
                                 begin_date_month == "November" ~ "fall",
                                 begin_date_month == "December" ~ "winter",
                                 begin_date_month == "January" ~ "winter",
                                 begin_date_month == "February" ~ "winter",
                                 begin_date_month == "March" ~ "spring",
                                 begin_date_month == "April" ~ "spring",
                                 begin_date_month == "May" ~ "spring")
         )
 
# Grouped barplot using ggplot2
ggplot(temp,                                      
       aes(x = temp_season,
           y = temp_surface_min_c,
           fill = temp_season)) +
  geom_bar(stat = "identity",
           position = "dodge")

#frequency plot 
temp_plot<-ggplot(data=subset(temp, !is.na(temp_season)), aes(x = temp_surface_min_c)) + 
      geom_density(aes(color = temp_season)) +
        scale_color_manual(values = c("darkgreen", "#EFC000FF", "#0073C2FF", "#FC4E07")) + 
  xlab("surface temperature C") + 
  theme(legend.position = c(0.25, 0.75))

ggsave("/Users/katelynking/Desktop/shoreline_map.png",temp_plot )

#map
summer_temp_plot<-MI_basemap + geom_point(data=subset(temp, temp_season == "summer"), aes(x = LONG_DD, y = LAT_DD, colour = c(temp_surface_min_c)), size=2) +  
    theme_bw() + 
  labs(colour="Surface temp") + 
  scale_colour_gradient(low = "blue",high = "orange") + 
  theme(legend.position = c(0.25, 0.25))

ggsave("/Users/katelynking/Desktop/summer_temp_plot.png",summer_temp_plot )

#map
MI_basemap + geom_point(data=subset(temp, temp_season == "fall"), aes(x = LONG_DD, y = LAT_DD, colour = c(temp_surface_min_c)), size=2) +  
  theme_bw() + 
  labs(colour="Surface temp") + 
  scale_colour_gradient(low = "blue",high = "orange") + 
  theme(legend.position = c(0.25, 0.25))

#* secchi ####
secchi<-filter(map_summ, !is.na(secchi_min_m))
p + geom_point(data=secchi, aes(x = LONG_DD, y = LAT_DD, colour = c(secchi_min_m)), size=2) +  
  theme_bw() + 
  scale_colour_gradient(low = "green",high = "blue") +
  labs(colour="Secchi depth (m)")

hist(secchi$secchi_min_m)
plot(map_summ$shoreline_all, map_summ$secchi_min_m)

#* create wordclouds ####
library(wordcloud)
library(wordcloud2)
library(tm)
#Create a vector containing only the text
text <- SUMM$fishing_intensity_summer_text
text <- SUMM$fishing_intensity_summer
text <- SUMM$fishing_intensity_winter_text
text <- SUMM$lakename
text <- fishc$type_gear1
# Create a corpus  
docs <- Corpus(VectorSource(text))
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 3, max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
wordcloud2(df) #can change shape

#lake name 
wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words=20, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

#*
