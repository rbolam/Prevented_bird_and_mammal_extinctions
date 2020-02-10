#### -------------------------------------------------------------------------------##
#### ------------------------- Conservation Actions plot -------------------------####
#### -------------------------------------------------------------------------------##

library(tidyverse)
library(viridis)
library(gridExtra)

## Load all spp and associated actions
actions <- read.csv("data/Conservation_actions.csv")

## Load IUCN action classification scheme info
classif <- read.csv("data/action_classification_scheme.csv")

## Load all candidate species
allcand <- read.csv("data/all_info_candidates.csv")
allcand <- select(allcand, Scientific.name, className)


## --------------------------- Add taxonomic information -------------------------####

act1993 <- actions %>% 
  left_join(classif, by = "Action") %>% 
  left_join(allcand, by = "Scientific.name") %>% 
  select(-Notes, -Action) %>% 
  rename(act_lev2 = action_name) %>% 
  rename(act_lev1 = category)


## --------------------------------- Sort factor levels --------------------------####

levels(act1993$act_lev1) <- c(levels(act1993$act_lev1), "Livelihood, economic &\nother incentives")
act1993$act_lev1[act1993$act_lev1 == "Livelihood, economic & other incentives"] <- "Livelihood, economic &\nother incentives"
act1993 <- droplevels(act1993)
act1993$act_lev1  = factor(act1993$act_lev1, levels(act1993$act_lev1)[c(3, 2, 5, 1, 4, 6)])


## Split all actions across different lines
levels(act1993$act_lev2) <- gsub(" ", "\n", levels(act1993$act_lev2))


## Split any over more than 2 lines back into 2
act1993$act_lev2 <- as.character(act1993$act_lev2)
act1993$act_lev2[act1993$act_lev2 == "Invasive/problematic\nspecies\ncontrol"] <- "Invasive/problematic\nspecies control"
act1993$act_lev2[act1993$act_lev2 == "Awareness\n&\ncommunications"] <- "Awareness &\ncommunications"
act1993$act_lev2[act1993$act_lev2 == "Habitat\n&\nnatural\nprocess\nrestoration"] <- "Habitat & natural\nprocess restoration"
act1993$act_lev2[act1993$act_lev2 == "Compliance\nand\nenforcemen"] <- "Compliance &\nenforcement"
act1993$act_lev2[act1993$act_lev2 == "Policies\nand\nregulations"] <- "Policies &\nregulations"
act1993$act_lev2[act1993$act_lev2 == "Linked\nenterprises\n&\nlivelihood\nalternatives"] <- 
  "Linked enterprises &\nlivelihood alternatives"
act1993$act_lev2[act1993$act_lev2 == "Resource\n&\nhabitat\nprotection"] <- "Resource &\nhabitat protection"



#### ------------------------- Make plots for 1993 and 2010 ----------------------####


act2010 <- filter(act1993, incl_2010 == "yes")

alldfs <- list(act1993, act2010)

plota <- list()
plotb <- list()


for (i in 1:2) {
  alldfs[[i]] %>% 
    filter(className == "AVES") %>% 
    select(Scientific.name, act_lev2) %>% 
    unique() %>% 
    count(act_lev2) -> a
  
  alldfs[[i]] %>% 
    filter(className == "AVES") %>% 
    select(act_lev2, act_lev1) %>% 
    unique() %>% 
    left_join(a, by = "act_lev2") %>% 
    ggplot(aes(x = fct_reorder(act_lev2, n), y = n)) +
    geom_col(aes(fill = act_lev1)) +
    scale_fill_viridis_d(option = "E") +
    labs(x = "", y = "Number of species", tag = "a) Birds") +
    theme_classic() +
    theme(text = element_text(size = 8),
          plot.tag.position = c(0.05, 1.02),
          legend.position = "none",
          plot.margin = unit(c(15, 5.5, 5.5, 5.5),"pt")) ->
    plota[[i]]
  
  alldfs[[i]] %>% 
    filter(className == "MAMMALIA") %>% 
    select(Scientific.name, act_lev2) %>% 
    unique() %>% 
    count(act_lev2) -> b
  
  alldfs[[i]] %>% 
    filter(className == "MAMMALIA") %>% 
    select(act_lev2, act_lev1) %>% 
    unique() %>% 
    left_join(b, by = "act_lev2") %>% 
    ggplot(aes(x = fct_reorder(act_lev2, n), y = n)) +
    geom_col(aes(fill = act_lev1)) +
    scale_fill_viridis_d(option = "E", name = "") +
    labs(x = "", y = "Number of species", tag = "b) Mammals") +
    theme_classic() +
    theme(text = element_text(size = 8),
          plot.tag.position = c(0.05, 1.02),
          plot.margin = unit(c(15, 5.5, 5.5, 5.5),"pt")) ->
    plotb[[i]]
}


## Plot for 1993

(plota1993 <- plota[[1]] + 
    coord_flip(ylim = c(1, 21)))
  
(plotb1993 <- plotb[[1]] + 
    coord_flip(ylim = c(1, 21)) +
    theme(legend.position = c(0.77, 0.27)))

c1993 <- grid.arrange(plota1993, plotb1993, nrow = 1)

ggsave("output/1993actions.pdf", c1993, width = 18, height = 10, unit = "cm", dpi = 600)



## Plot for 2010

(plota2010 <- plota[[2]] + 
    coord_flip(ylim = c(0.6, 13)) +
    scale_y_continuous(breaks = seq(0, 12, 3)))

(plotb2010 <- plotb[[2]] + 
    coord_flip(ylim = c(0.6, 13)) +
    theme(legend.position = c(0.76, 0.37)) +
    scale_y_continuous(breaks = seq(0, 12, 3)))

c2010 <- grid.arrange(plota2010, plotb2010, nrow = 1)

ggsave("output/2010actions.pdf", c2010, width = 18, height = 8, unit = "cm", dpi = 600)


