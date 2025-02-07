
# loads library
library(dplyr)
library(maptools)
library(lubridate)
library(tidyr)
library(ggplot2)
library(mapdata)
library(sf)

#### read in data ####

points<-read.csv("whole_database/mdnr_latlons.csv") %>% 
  distinct(new_key, .keep_all = TRUE) 

all_fishc<-read.csv("FISHc/FISHc_data/final_data/fishc_qaqc_Feb2025.csv")
all_summ<-read.csv( "Lake_SUMM/summ_data_Feb2025.csv")
all_grow<-read.csv("GROW_general/grow_qaqc_Feb2025.csv")

fish_mapping<-all_fishc %>% 
                left_join(points) %>% 
  distinct(new_key, .keep_all = TRUE) %>% 
  dplyr::select(new_key, long_dd, lat_dd)

grow_mapping<-all_grow %>% 
  left_join(points) %>% 
  distinct(new_key, .keep_all = TRUE) %>% 
  dplyr::select(new_key, long_dd, lat_dd)

summ_mapping<-all_summ %>% 
  left_join(points) %>% 
  distinct(new_key, .keep_all = TRUE) %>% 
  dplyr::select(new_key, long_dd, lat_dd)

#* calculate what don't we have new_keys for ####
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

#### map of all lakes matched  Figure 2#### 
#read in MI map
data("us_states", package = "spData")
us_states = st_transform(us_states, crs = "+proj=longlat +datum=WGS84 +no_defs") 
MI<-dplyr::filter(us_states, NAME == 'Michigan')
plot(MI)

# plot
(fish_map<-ggplot() +
    geom_sf(data=MI, aes(), fill = "white") + 
    geom_point(data=fish_mapping, aes(x = long_dd, y = lat_dd ), size=0.75) +
    theme_void() +
    theme( panel.background = element_rect(colour = "black"))
)

# plot
(grow_map<-ggplot() +
    geom_sf(data=MI, aes(), fill = "white") + 
    geom_point(data=grow_mapping, aes(x = long_dd, y = lat_dd ), size=0.75) +
    theme_void() +
    theme( panel.background = element_rect(colour = "black"))
)

# plot
(summ_map<-ggplot() +
    geom_sf(data=MI, aes(), fill = "white") + 
    geom_point(data=summ_mapping, aes(x = long_dd, y = lat_dd), size=0.75) +
    theme_void() +
    theme( panel.background = element_rect(colour = "black"), 
           (panel.background = element_blank()))
)

lake_map<-cowplot::plot_grid(summ_map, fish_map,  grow_map, nrow=1, labels = c('a', 'b', 'c'), 
                         vjust=5)


ggsave("Figures/lake_distribution.png",lake_map, bg='#ffffff', 
       width = 8,
       height = 4,
       units = c("in"))


#### freq of years figure 3 #### 
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



year_plot<-cowplot::plot_grid(summ_year, fish_year, grow_year, nrow=1, labels = c('a', 'b', 'c'))

ggsave("Figures/year_plot.png",year_plot,
       width = 8,
       height = 4,
       units = c("in"))

range(all_summ$begin_date_year , na.rm = TRUE)
range(all_grow$begin_date_year, na.rm = TRUE)
range(all_fishc$begin_date_year, na.rm = TRUE)

####common fish species on records Figure 5 #### 
fish_sp<-dplyr::select(all_fishc,subject_id, begin_date_year, banded_killifish:yellow_bullhead)
fish_sp[ , 3:57 ][ fish_sp[ , 3:57 ] > 0 ] <- 1 #set data to 1s and 0s 
fish_sp[, 3:57 ] <- sapply(fish_sp[, 3:57 ],as.numeric)

fishes<-fish_sp %>% 
pivot_longer(cols =  banded_killifish:yellow_bullhead,
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
  filter(species == 'bluegill' | species == 'northern_pike' | species ==  'yellow_perch' | species == 'pumpkinseed'  | species == 'white_sucker') %>% 
  ggplot(aes(fill =decade, x = species, y= Freq)) + 
  geom_bar(position = "stack", stat= "identity") + 
  xlab("species") + ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + 
  theme_bw() +
  theme( panel.background = element_rect(colour = "black"), 
         (panel.background = element_blank()))


ggsave("Figures/sp_year_plot.png",sp_year_plot,
       width = 8,
       height = 4,
       units = c("in"))


#### GROW FIGURES Figure 6 #### 

#select species with the most data, >200 obs
top_grow<-filter(all_grow, species == 'black_crappie' | species == 'bluegill' | species == 'brook_trout' |
                   species == 'brown_trout' | species == 'lake_herring' | species == 'white_sucker' | 
                   species == 'lake_trout' | species == 'largemouth_bass' | species == 'northern_pike'| 
                   species == 'pumpkinseed' | species == 'rainbow_trout' | species == 'rock_bass' | 
                   species == 'smallmouth_bass' | species == 'tiger_muskie' | species == 'walleye' |species == 'yellow_perch') %>% 
  drop_na(age_group) %>% 
  filter(age_group <16) %>% #remove some outliers for plotting 
  mutate(species_name = case_when(species == 'black_crappie' ~ "black crappie n=4361",
                                  species == 'bluegill' ~ "bluegill n=10357", 
                                  species == 'brook_trout' ~ "brook trout n=293",
                                  species == 'brown_trout' ~ "brown trout n=395",
                                  species == 'lake_herring' ~ "lake herring n=526", 
                                  species == 'white_sucker' ~ "white sucker n=464",
                                    species == 'lake_trout' ~ "lake trout n=236", 
                                    species == 'largemouth_bass' ~"largemouth bass n=7458",
                                    species == 'northern_pike'~"northern pike n=6277",
                                    species == 'pumpkinseed'~"pumpkinseed n=5078",
                                    species == 'rainbow_trout' ~"rainbow trout n=447",
                                    species == 'rock_bass' ~"rock bass n=3131",
                                    species == 'smallmouth_bass' ~"smallmouth bass n=2836",
                                    species == 'tiger_muskie'~"tiger muskie n=218",
                                    species == 'walleye' ~"walleye  n=3940",
                                    species == 'yellow_perch'~"yellow perch n=9460")
         )

#facet wrap 
grow_figure<-ggplot(top_grow, aes(x=as.factor(age_group), y=length_mean_mm)) + 
  geom_boxplot() + 
  ylab("average length (mm)") + 
  xlab("age group") + 
  facet_wrap(~species_name,scales = "free" )
  

ggsave("Figures/grow_figure.png",grow_figure,
       width = 10,
       height = 8,
       units = c("in"))

#### SUMM CARD Figures 4####
summ_variable_maps<-all_summ %>% 
  left_join(points) %>% 
  distinct(new_key, .keep_all = TRUE) %>% 
  dplyr::select(new_key, long_dd, lat_dd, begin_date_month, resorts_min_n, liveries_min_n, cottages_min_n, hotels_homes_min_n, fishing_intensity_summer, temp_surface_min_c) %>% 
  mutate(shoreline_all = resorts_min_n + liveries_min_n + cottages_min_n + hotels_homes_min_n
)

#*fishing intensity  ####
summ_variable_maps$fishing_intensity_summer <- factor(summ_variable_maps$fishing_intensity_summer, levels = c("heavy", "medium", "light", "none"))

fishing<-ggplot() +
  geom_sf(data=MI, aes(), fill = "white") +
  geom_point(data=filter(summ_variable_maps, !is.na(fishing_intensity_summer)), 
             aes(x = long_dd, y = lat_dd, colour = c(fishing_intensity_summer)), alpha = 0.8) +  
  theme_bw() + 
  labs(colour="fishing intensity") + 
  scale_colour_brewer(palette = "BrBG", direction = - 1) + 
  theme_void() +
  theme( panel.background = element_rect(colour = "black")) + 
  theme(legend.position = c(0.25, 0.25))


#* TEMP ####
#add SEASON  
temp<-filter(summ_variable_maps, !is.na(temp_surface_min_c)) %>% 
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
 


#map
(temp_plot<-ggplot() +
  geom_sf(data=MI, aes(), fill = "white") + 
  geom_point(data=subset(temp, temp_season == "summer"), aes(x = long_dd, y = lat_dd, colour = c(temp_surface_min_c)), size=2) +  
    theme_bw() + 
  labs(colour="Surface temp") + 
    scale_colour_gradient2(low = "blue",high = "red",
                         mid ="yellow",midpoint = 20)  +  
    theme_void() +
    theme( panel.background = element_rect(colour = "black"))+
  theme(legend.position = c(0.25, 0.25))
)


fishing_temp_map<-cowplot::plot_grid(fishing, temp_plot, nrow=1, labels = c('a', 'b'), 
                                     hjust = - 5, vjust =2)


ggsave("Figures/fishing_temp_map.png",fishing_temp_map, bg='#ffffff', 
       width = 8,
       height = 4,
       units = c("in"))
