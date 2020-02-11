#### --------------------------------------------------------------------------------##
#### -------------------------------- Species maps --------------------------------####
#### --------------------------------------------------------------------------------##

library(tidyverse)
library(rworldmap)
library(ggalt)
library(gridExtra)

cand <- read.csv("data/candidate_countries.csv")


#### --------------------------------- Sort data ----------------------------------####

## Count number in each class by country for candidates, for 1993, and for 2010, & make list


## For candidate species

aggcand <- cand %>% 
  group_by(region, className) %>% 
  count() %>% 
  spread(className, n, fill = NA)


## For 1993 species

agg1993 <- cand %>% 
  filter(!is.na(t1993)) %>% 
  group_by(region, className) %>% 
  count() %>% 
  spread(className, n, fill = NA)


## For 2010 species

agg2010 <- cand %>% 
  filter(!is.na(t2010)) %>% 
  group_by(region, className) %>% 
  count() %>% 
  spread(className, n, fill = NA)


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
             aes(map_id = region, x = long, y = lat, fill = AVES), colour = "black", size = 0.01) + 
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
             aes(map_id = region, x = long, y = lat, fill = MAMMALIA), colour = "black", size = 0.01) + 
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

tiff("output/candidatemap.tiff", width = 7.08661, height = 7, res = 300, units = "in")
plot <- grid.arrange(plotacand, plotbcand, heights = c(1.07, 1))
dev.off()


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

tiff("output/1993map.tiff", width = 7.08661, height = 7, res = 600, units = "in")
plot <- grid.arrange(plota1993, plotb1993, heights = c(1.07, 1))
dev.off()


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

tiff("output/2010map.tiff", width = 7.08661, height = 7, res = 300, units = "in")
plot <- grid.arrange(plota2010, plotb2010, heights = c(1.07, 1))
dev.off()


