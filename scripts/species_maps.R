#### --------------------------------------------------------------------------------##
#### -------------------------------- Species maps --------------------------------####
#### --------------------------------------------------------------------------------##

library(tidyverse)
library(rworldmap)
library(ggalt)
library(gridExtra)

cand <- read.csv("data/allspp_medians.csv")

countries <- read.csv("data/RL_data_29_01_2020/countries.csv")

rl_cat <- read.csv("data/RL_data_29_01_2020/simple_summary.csv")


#### --------------------------------- Sort data ----------------------------------####


## Select best estimate for all candidates only

cand <- cand %>% 
  filter(est == "Best") %>% 
  select(Scientific.name, class, median1993, median2010)


## Sort country data, retain native and reintroduced ranges only, merge with candidates

countries %>% 
  select(scientificName, name, presence, origin) %>% 
  rename(Scientific.name = scientificName) %>% 
  right_join(cand, by = "Scientific.name") %>% 
  rename(region = name) %>% 
  filter(origin %in% c("Native", "Reintroduced")) %>% 
  mutate_if(is.factor, as.character) ->
  cand


## Mergw with current RL categories

rl_cat <- rl_cat %>% 
  select(scientificName, redlistCategory) %>% 
  rename(Scientific.name = scientificName)

cand <- left_join(cand, rl_cat, by = "Scientific.name")


## Remove range where extant spp are extinct, but retain extinct ranges for EW spp

ext <- filter(cand, presence %in% c("Extant", "Possibly Extant") & redlistCategory != "Extinct in the Wild")
exw <- filter(cand, presence == "Extinct Post-1500" & redlistCategory == "Extinct in the Wild")

cand <- bind_rows(ext, exw)


## Make sure country names are correctly mappable

cand$region[cand$region == "Korea, Democratic People's Republic of"] <- c("North Korea")
cand$region[cand$region == "Korea, Republic of"] <- c("South Korea")
cand$region[cand$region %in% c("Russian Federation (Eastern Asian Russia)", "Russian Federation")] <- c("Russia")
cand$region[cand$region == "Syrian Arab Republic"] <- c("Syria")
cand$region[cand$region == "Taiwan, Province of China"] <- c("Taiwan")
cand$region[cand$region %in% c("United States", "United States Minor Outlying Islands")] <- c("USA")
cand$region[cand$region == "Viet Nam"] <- c("Vietnam")
cand$region[cand$region == "Hong Kong"] <- c("China")
cand$region[cand$region == "RÃ©union"] <- c("Reunion")
cand$region[cand$region == "Brunei Darussalam"] <- c("Brunei")
cand$region[cand$region == "Cabo Verde"] <- c("Cape Verde")
cand$region[cand$region == "French Southern Territories"] <- c("French Southern and Antarctic Lands")
cand$region[cand$region == "Lao People's Democratic Republic"] <- c("Laos")


#### ------------------------------- Count numbers --------------------------------####


## For candidate species

aggcand <- cand %>% 
  group_by(region, class) %>% 
  count() %>% 
  spread(class, n, fill = NA)


## For 1993 species

agg1993 <- cand %>% 
  filter(median1993 > 50) %>% 
  group_by(region, class) %>% 
  count() %>% 
  spread(class, n, fill = NA)


## For 2010 species

agg2010 <- cand %>% 
  filter(median2010 > 50) %>% 
  group_by(region, class) %>% 
  count() %>% 
  spread(class, n, fill = NA)


## Make list of 3 dataframes

alldfs <- list(aggcand, agg1993, agg2010)



#### --------------------------------- Make plots ----------------------------------####


## Empty plot lists:

plota <- list()
plotb <- list()


## Make figures:

for (i in 1:3) {
  map.all <- map_data(map = "world")
  map.all <- full_join(map.all, alldfs[[i]], by = "region") 
  #map.all$MAMMALIA[map.all$region == "Guam"] <- 10 ## add in so colour scales match (but can't be seen in map)
  
  ## Get island nation/ overseas territories coordinates
  islands <- map.all %>% 
    filter(!is.na(AVES) | !is.na(MAMMALIA)) %>% 
    filter(region %in% c("Cooke Islands", "French Polynesia", "Guam", "Mauritius", "Northern Mariana Islands", 
                         "Puerto Rico", "Reunion", "Seychelles")) %>% 
    select(long, lat, region, AVES) %>% 
    group_by(region) %>% 
    mutate(mlong = mean(long)) %>% 
    mutate(mlat = mean(lat)) %>% 
    select(-long, -lat) %>% 
    unique() %>% 
    ungroup() 
  
  ## Make bird plot
  ggplot() + 
    geom_map(data = map.all, map = map.all, 
             aes(map_id = region, x = long, y = lat, fill = AVES), colour = "black", size = 0.1) + 
    coord_proj("+proj=cea +lat_ts=37.5") +
    labs(tag = "a) Birds       ", x = "", y = "") + 
    # add some circles around island nations:
    geom_point(data = islands, aes(x = mlong, y = mlat, colour = factor(AVES)), size = 2, pch = 21, stroke = 2) +
    guides(colour = "none") +
    theme_void() +
    theme(text = element_text(size = 10),
          legend.key.size = unit(0.4, "cm")) ->
    plota[[i]]
  
  ## Make mammal plot
  ggplot() + 
    geom_map(data = map.all, map = map.all,
             aes(map_id = region, x = long, y = lat, fill = MAMMALIA), colour = "black", size = 0.1) + 
    coord_proj("+proj=cea +lat_ts=37.5") +
    labs(tag = "b) Mammals", x = "", y = "") +
    guides(colour = "none") +
    theme_void() +
    theme(text = element_text(size = 10)) ->
    plotb[[i]]
}


#### --------------------------------- Save plots ----------------------------------####


## Make candidate plot with adjusted colour scales and save

plotacand <- plota[[1]] +
  scale_fill_gradient2(low = "#edf8b1", mid = "#41b6c4", high = "#0c2c84", midpoint = 5.5, guide = "legend",
                       breaks = c(1, 3, 5, 7, 9)) + 
  scale_colour_manual(values = c("#EDF8B1", "#C6E9B5", "#A0DAB9")) +
  labs(fill = "Number of species") +
  theme(legend.position = "bottom")
  
plotbcand <- plotb[[1]] +
  scale_fill_gradient2(low = "#edf8b1", mid = "#41b6c4", high = "#0c2c84", midpoint = 5.5, guide = "legend") + 
  theme(legend.position = "none")

mapcand <- grid.arrange(plotacand, plotbcand, heights = c(1.07, 1))
ggsave("output/candidatemap.png", mapcand, width = 7.08661, height = 7, unit = "in", dpi = 1000)



## Make 1993 plot with adjusted colour scales and save

plota1993 <- plota[[2]] +
  scale_fill_gradient2(low = "#edf8b1", mid = "#41b6c4", high = "#0c2c84", midpoint = 3.5, guide = "legend",
                       breaks = seq(1, 6, 1)) +
  labs(fill = "Number of species") +
  guides(fill = guide_legend(nrow = 1)) +
  scale_colour_manual(values = c("#edf8b1", "#97d7ba")) +
  theme(legend.position = "bottom")

plotb1993 <- plotb[[2]] +
  scale_fill_gradient2(low = "#edf8b1", mid = "#41b6c4", high = "#0c2c84", midpoint = 3.5, guide = "legend") +
  theme(legend.position = "none")

map1993 <- grid.arrange(plota1993, plotb1993, heights = c(1.07, 1))
ggsave("output/1993map.pdf", map1993, width = 7.08661, height = 7, unit = "in", dpi = 1000)



## Make 2010 plot with adjusted colour scales and save

plota2010 <- plota[[3]] +
  scale_fill_gradient2(low = "#edf8b1", mid = "#41b6c4", high = "#0c2c84", midpoint = 3, guide = "legend",
                       breaks = seq(1, 5, 1)) +
  scale_colour_manual(values = c("#edf8b1", "#7accbd")) +
  labs(fill = "Number of species") +
  theme(legend.position = "bottom")

plotb2010 <- plotb[[3]] +
  scale_fill_gradient2(low = "#edf8b1", mid = "#41b6c4", high = "#0c2c84", midpoint = 3, guide = "legend") +
  theme(legend.position = "none")

map2010 <- grid.arrange(plota2010, plotb2010, heights = c(1.07, 1))
ggsave("output/2010map.png", map2010, width = 7.08661, height = 7, unit = "in", dpi = 1000)


