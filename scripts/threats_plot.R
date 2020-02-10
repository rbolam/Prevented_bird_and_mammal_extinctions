#### -------------------------------------------------------------------------------##
#### -------------------------------- Threats plots --------------------------------##
#### -------------------------------------------------------------------------------##

library(tidyverse)
library(gridExtra)


threats <- read.csv("data/threats.csv", na.strings = c("NA", ""))

threats <- threats %>% 
  filter(timing != "Future") %>% 
  select(scientificName, name, code, ias) %>% 
  rename(Scientific.name = scientificName) 

## Get candidate list
allspp <- read.csv("data/all_info_candidates.csv")
allspp <- select(allspp, className, Scientific.name, t1993, t2010)


threats <- left_join(allspp, threats, by = "Scientific.name")


## Add in level 1 threats and names
threats$code2 <- threats$code
threats <- separate(threats, code2, into = c("threat_level1"), extra = "drop")


threat_level1 <- rep(1:12)
tnames <- c("Residential & commercial development", "Agriculture & aquaculture", "Energy production & mining", 
            "Transportation & service corridors", "Biological resource use", "Human intrusions & disturbance",
            "Natural system modifications", "Invasive & other problematic\nspecies, genes & diseases",
            "Pollution", "Geological events", "Climate change & severe weather", "Other")
tnames <- data.frame(cbind(threat_level1, tnames))

threats <- left_join(threats, tnames, by = "threat_level1")  
threats$threat_level1 <- as.integer(threats$threat_level1)

threats$tnames <- as.character(threats$tnames)
threats$name <- as.character(threats$name)

threats$tnames[threats$code %in% c("5.1.1", "5.1.2", "5.1.3")] <- "Hunting & collecting terrestrial animals"
threats$tnames[threats$code %in% c("5.3.1.", "5.3.3", "5.3.4", "5.3.5")] <- "Logging & wood harvesting"
threats$tnames[threats$code %in% c("5.4.3", "5.4.4")] <- "Fishing & harvesting aquatic resources"
threats$tnames[threats$code %in% c("7.1.1", "7.1.2", "7.1.3")] <- "Fire & fire suppression"
threats$tnames[threats$code %in% c("7.2.11", "7.2.3")] <- "Dams & water management/use"
threats$tnames[threats$code %in% c("7.3")] <- "Other ecosystem modifications"
threats <- filter(threats, !is.na(code))


## Make 3 separate dfs for cand, 1993 and 2010


thr1993 <- filter(threats, !is.na(t1993))
thr2010 <- filter(threats, !is.na(t2010))

alldfs <- list(threats, thr1993, thr2010)
  
plota <- list()
plotb <- list()


## Make plot of ongoing/ ongoing & past level 1 threats -----------------------------####

for (i in 1:3) {
  alldfs[[i]] %>% 
  filter(className == "AVES") %>% 
  select(Scientific.name, tnames) %>% 
  unique() %>% 
  count(tnames) %>% 
  ggplot(aes(x = fct_reorder(tnames, n), y = n)) +
  geom_col(fill = "#1b9e77") +
  labs(x = "", y = "Number of species", tag = "a) Birds") +
  theme_classic() +
  theme(text = element_text(size = 8),
        plot.tag.position = c(0.05, 1)) ->
  plota[[i]]
  
  alldfs[[i]] %>% 
  filter(className == "MAMMALIA") %>% 
  select(Scientific.name, tnames) %>% 
  unique() %>% 
  count(tnames) %>% 
  ggplot(aes(x = fct_reorder(tnames, n), y = n)) +
  geom_col(fill = "#d95f02") +
  labs(x = "", y = "Number of species", tag = "b) Mammals")  +
  theme_classic() +
  theme(text = element_text(size = 8),
        plot.tag.position = c(0.1, 1.05))  ->
  plotb[[i]]
}

(plotacand <- plota[[1]] + coord_flip(ylim = c(1.4, 35)))
(plotbcand <- plotb[[1]] + coord_flip(ylim = c(1.4, 35)))
ccand <- grid.arrange(plotacand, plotbcand, ncol = 1)
ggsave("output/candidatethreats.pdf", ccand, width = 8, height = 14, unit = "cm", dpi = 600)


(plota1993 <- plota[[2]] + coord_flip(ylim = c(1.1, 25)))
(plotb1993 <- plotb[[2]] + coord_flip(ylim = c(1.1, 25)))
c1993 <- grid.arrange(plota1993, plotb1993, ncol = 1)
ggsave("output/1993threats.pdf", c1993, width = 8, height = 12, unit = "cm", dpi = 600)

(plota2010 <- plota[[3]] + 
    coord_flip(ylim = c(0.7, 16)))
(plotb2010 <- plotb[[3]] + 
    coord_flip(ylim = c(0.7, 16)))
c2010 <- grid.arrange(plota2010, plotb2010, ncol = 1)
ggsave("output/2010threats.pdf", c2010, width = 8, height = 12, unit = "cm", dpi = 600)

