rm(list = ls(all.names = TRUE))
library(tidyverse)
library(gridExtra)


threats <- read.csv("rl_data_29_01_2020/threats.csv", na.strings = c("NA", ""))
threats <- threats %>% 
  filter(timing != "Future") %>% 
  select(scientificName, name, code, ias) %>% 
  rename(Scientific.name = scientificName) 

## Get candidate list
allspp <- read.csv("all_info_candidates.csv")
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
ggsave("candthreats.jpg", ccand, width = 8, height = 14, unit = "cm", dpi = 300)


(plota1993 <- plota[[2]] + coord_flip(ylim = c(1.1, 25)))
(plotb1993 <- plotb[[2]] + coord_flip(ylim = c(1.1, 25)))
c1993 <- grid.arrange(plota1993, plotb1993, ncol = 1)
ggsave("1993threats.jpg", c1993, width = 8, height = 12, unit = "cm", dpi = 1000)

(plota2010 <- plota[[3]] + 
    coord_flip(ylim = c(0.7, 16)))
(plotb2010 <- plotb[[3]] + 
    coord_flip(ylim = c(0.7, 16)))
c2010 <- grid.arrange(plota2010, plotb2010, ncol = 1)
ggsave("2010threats.jpg", c2010, width = 8, height = 12, unit = "cm", dpi = 300)




## --------------- Make plot of named invasive species ----------------####

threats %>% 
  filter(!is.na(t1993)) %>% 
  select(Scientific.name, ias) %>% 
  filter(!is.na(ias)) %>% 
  unique() %>%
  count(ias) %>% 
  rename(total_no = n) ->
  no_ias

threats %>% 
  filter(!is.na(t1993)) %>% 
  select(Scientific.name, className, ias) %>% 
  filter(!is.na(ias)) %>% 
  unique() %>% 
  count(ias, className) %>% 
  left_join(no_ias, by = "ias") %>% 
  ggplot(aes(fct_reorder(ias, total_no), y = n)) +
  geom_col(aes(fill = className)) +
  coord_flip(ylim = c(0.9, 18)) +
  scale_y_continuous(breaks = seq(0, 18, 3)) + 
  scale_fill_brewer(palette = "Dark2", name = "", labels = c("Birds", "Mammals")) +
  labs(x = "", y = "Number of\nspecies affected") +
  guides(fill = guide_legend(override.aes = list(size = 0.5))) +
  theme_classic() +
  theme(text = element_text(size = 10),
        legend.position = c(0.9, 0.9),
        legend.text = element_text(size = 8))
ggsave("namedinvasives.jpg", width = 14, height = 17, unit = "cm", dpi = 300)



## Make plot of ongoing level 2 threats ----------------------------####

threats$code <- fct_relevel(threats$code, c("10.1", "11.1", "11.2", "11.3", "11.4", "11.5", "12.1"), after = Inf)

#labelnames <- select(threats, code, name) %>% 
#  unique() %>% 
#  arrange(code, name) %>% 
#  mutate(levelt = str_count(code, "[.]"))


threats %>% 
  filter(timing != "Future") %>% 
  select(Scientific.name, tnames, name, code, threat_level1, className) %>% 
  unique() %>% 
  ggplot(aes(x = code, fill = fct_reorder(tnames, threat_level1))) +
  geom_bar() +
  scale_fill_brewer(palette = "Set3", name = "Level 1 threats") +
  facet_wrap(~className, scales = "free_y", ncol = 1) +
  #scale_x_discrete(labels = labelnames$name) +
  labs(x = "", y = "Number of species") +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5),
        text = element_text(size = 13))
ggsave("ongoingpastthreats_level2.jpg", width = 9, height = 5, dpi = 300)




## Sankey diagram -------------------------------------------------------------------------------####
library(ggalluvial)
library(viridis)

#actions$Action <- as.character(actions$Action)
threatmerge <- threats %>% 
  select(Scientific.name, tnames) %>% 
  unique()

## Count level 1 actions
lev1_thr <- actions %>% 
  left_join(threatmerge, by = "Scientific.name") %>% 
  left_join(classif, by = "Action") %>% 
  select(-Notes, -Common.name, -Action, -action_name) %>% 
  group_by(category, tnames) %>% 
  count() %>% 
  ungroup() %>% 
  filter(!is.na(tnames))

## Count level 2 actions
lev2_thr <- actions %>% 
  left_join(threatmerge, by = "Scientific.name") %>% 
  select(-Notes, -Common.name) %>% 
  group_by(Action, tnames) %>% 
  count() %>% 
  left_join(classif, by = "Action") %>% 
  ungroup() %>% 
  filter(!is.na(tnames))


## Level 1 flow
ggplot(lev1_thr, aes(axis1 = tnames, axis2 = category, y = n)) +
  geom_alluvium(aes(fill = category), width = 1/12, alpha = 1) +
  geom_stratum(width = 1/12) +
  geom_label(stat = "stratum", infer.label = TRUE) +
  scale_fill_viridis_d(option = "E", direction = 1, name = "Action level 1") +
  theme(legend.position = "bottom")
ggsave("flow.jpg", width = 14, height = 8)

## Making a more sensible plot

actions_bar <- actions %>% 
  left_join(threatmerge, by = "Scientific.name") %>% 
  left_join(classif, by = "Action") %>% 
  filter(!is.na(tnames))

actions_bar$tnames  = factor(actions_bar$tnames, levels(actions_bar$tnames)[c(11, 1, 4, 12, 2, 6, 8, 7, 10, 5, 3, 9)])
actions_bar$category = factor(actions_bar$category, levels(actions_bar$category)[c(3, 2, 6, 1, 4, 5)])


## Action level 1 & threats
ggplot(data = actions_bar, aes(x = category)) +
  geom_bar(aes(fill = tnames), position = position_dodge2(preserve = "single")) +
  facet_wrap(~category, scales = "free_x", nrow = 2) +
  scale_fill_viridis_d(option = "E", direction = -1, name = "Threats") +
  labs(x = "", y = "Number of species") +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom")
ggsave("thr_lev1act.jpg", width = 9, height = 6)

