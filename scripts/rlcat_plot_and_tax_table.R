#### ------------------------------------------------------------------------------##
#### ----------------------- RL plot and taxonomic table ------------------------####
#### ------------------------------------------------------------------------------##

library(tidyverse)
library(lemon)


allcand <- read.csv("data/allspp_medians.csv")
rlcat <- read.csv("data/RL_data_29_01_2020/simple_summary.csv")


## -------------------------------- Sort data -----------------------------------####

rlcat <- rlcat %>% 
  select(scientificName, redlistCategory, populationTrend) %>% 
  rename(Scientific.name = scientificName)

allcand <- allcand %>% 
  filter(est == "Best") %>% 
  select(Scientific.name, class, median1993, median2010) %>% 
  left_join(rlcat, by = "Scientific.name")



## Reorder Red List categories and population trends

allcand$redlistCategory  = factor(allcand$redlistCategory, levels(allcand$redlistCategory)[c(3, 1, 2, 5, 4)])
levels(allcand$populationTrend)[levels(allcand$populationTrend) %in% c("Unknown", "")] <- "Unknown/NA"
allcand$populationTrend  = factor(allcand$populationTrend, levels(allcand$populationTrend)[c(2, 4, 3, 1)])

## Split names over 2 lines

levels(allcand$redlistCategory) <- gsub(" ", "\n", levels(allcand$redlistCategory))
levels(allcand$redlistCategory)[levels(allcand$redlistCategory) == "Extinct\nin\nthe\nWild"] <- "Extinct in\nthe Wild"


## ---------------------------- Make list of 3 filtered dfs ---------------------####

spp1993 <- filter(allcand, median1993 > 50)
spp2010 <- filter(allcand, median2010 > 50)
alldfs <- list(allcand, spp1993, spp2010)


# ---------------------------- RL category plot ----------------------------------####

plot <- list()

for (i in 1:3) {
  ggplot(data = alldfs[[i]], aes(x = redlistCategory, fill = populationTrend)) +
  geom_bar(colour = "black", size = 0.01) +
  scale_fill_manual(values = c("#d7191c", "#ffffbf", "#2c7bb6", "grey50"), name = "Population trend") +
  facet_rep_wrap(~ class, ncol = 1,
             labeller = labeller(className = c(AVES = "a) Birds", MAMMALIA = "b) Mammals")),
             repeat.tick.labels = TRUE) +
  labs(x = "IUCN Red List category", y = "Number of species") +
  theme_classic() +
  theme(text = element_text(size = 8.5),
        legend.position = c(0.85, 0.33),
        strip.text = element_text(hjust = 0, size = 9),
        strip.background = element_rect(colour = NA),
        legend.title = element_text(size = 8)) ->
    plot[[i]]
}



## Adjust axes and save candidate plot

plot[[1]] + coord_cartesian(ylim = c(1.1, 25))
ggsave("output/candidatesrlcat.pdf", width = 8, height = 14, unit = "cm", dpi = 600)


## Adjust axes and save 1993 plot

plot[[2]] +
  scale_y_continuous(breaks = seq(0, 15, 3)) +  
  coord_cartesian(ylim = c(0.7, 15))
ggsave("output/1993rlcat.pdf", width = 8, height = 14, unit = "cm", dpi = 600)


## Adjust axes and save 1993 plot

plot[[3]] +
  scale_y_continuous(breaks = seq(0, 15, 3)) +
  coord_cartesian(ylim = c(0.7, 15))
ggsave("output/2010rlcat.pdf", width = 8, height = 14, unit = "cm", dpi = 600)



