#### ------------------------------------------------------------------------------##
#### ----------------------- RL plot and taxonomic table ------------------------####
#### ------------------------------------------------------------------------------##

library(tidyverse)
library(lemon)


allcand <- read.csv("data/all_info_candidates.csv")


## --------------------------- Adjust factor levels -----------------------------####

## Reorder Red List categories and population trends

allcand$redlistCategory  = factor(allcand$redlistCategory, levels(allcand$redlistCategory)[c(3, 1, 2, 5, 4)])
allcand$populationTrend  = factor(allcand$populationTrend, levels(allcand$populationTrend)[c(1, 3, 2, 4)])
levels(allcand$populationTrend)[levels(allcand$populationTrend) == "Unknown"] <- "Unknown/NA"


## Split names over 2 lines

levels(allcand$redlistCategory) <- gsub(" ", "\n", levels(allcand$redlistCategory))
levels(allcand$redlistCategory)[levels(allcand$redlistCategory) == "Extinct\nin\nthe\nWild"] <- "Extinct in\nthe Wild"


## ---------------------------- Make list of 3 filtered dfs ---------------------####

spp1993 <- filter(allcand, !is.na(t1993))
spp2010 <- filter(allcand, !is.na(t2010))
alldfs <- list(allcand, spp1993, spp2010)


# ---------------------------- RL category plot ----------------------------------####

plot <- list()

for (i in 1:3) {
  ggplot(data = alldfs[[i]], aes(x = redlistCategory, fill = populationTrend)) +
  geom_bar(colour = "black", size = 0.01) +
  scale_fill_manual(values = c("#d7191c", "#ffffbf", "#2c7bb6", "grey50"), name = "Population trend") +
  facet_rep_wrap(~ className, ncol = 1,
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



## ---------------------------- Taxonomic table ---------------------------------####

df <- list()
headings <- c("Candidates", "1993 species", "2010 species")


## Count number of species in each family, for all 3 species groups

for (i in 1:3) {
  df[[i]] <- alldfs[[i]] %>% 
    select(Scientific.name, className, familyName) %>% 
    group_by(className, familyName) %>% 
    count(name = paste(headings[i]))
}


## Merge all together

taxonomy <- df[[1]] %>% 
  left_join(df[[2]], by = c("className", "familyName")) %>% 
  left_join(df[[3]], by = c("className", "familyName")) %>% 
  replace_na(list(`1993 species` = 0, `2010 species` = 0))

write_csv(taxonomy, "output/taxonomytable.csv")

