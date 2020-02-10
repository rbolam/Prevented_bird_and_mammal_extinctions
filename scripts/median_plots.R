#### -------------------------------------------------------------------------------##
#### ----------------------------- Plots of medians ------------------------------####
#### -------------------------------------------------------------------------------##

library(tidyverse)
library(viridis)
library(cowplot)
library(gridExtra)


allsppmed <- read.csv("data/allspp_medians.csv")



## Reorder factor levels

### Agreement
allsppmed$agreement1993 = factor(allsppmed$agreement1993, levels(allsppmed$agreement1993)[c(1, 3, 2)])
allsppmed$agreement2010 = factor(allsppmed$agreement2010, levels(allsppmed$agreement2010)[c(1, 3, 2)])

### Type of estimate
allsppmed$est = factor(allsppmed$est, levels(allsppmed$est)[c(2, 1, 3)])



## Set up plot background

fill <- c("Very unlikely", "Quite unlikely", "Quite possible\nbut unlikely", "More likely than not", "Quite likely", 
          "Very likely", "Virtually certain")
ymax <- c(10, 25, 50, 75, 90, 99, 100)
ymin <- c(0, 10, 25, 50, 75, 90, 99)
fill <- c(1:7)
rect <- data.frame(cbind(ymax, ymin, fill))


#### ----------------------------- Birds -----------------------------------------####


## Make bird plot for 1993 - 2020

(a <- filter(allsppmed, class == "AVES") %>% 
   ggplot() +
   geom_point(aes(x = fct_reorder(Common.name, order1993), y = median1993), alpha = 0)  +
   geom_rect(data = rect, aes(xmin = 0, xmax = 45.6, ymin = ymin, ymax = ymax, fill = fill), alpha = 0.8) +
   scale_fill_viridis() +
   geom_point(aes(x = fct_reorder(Common.name, order1993), y = median1993, size = agreement1993), colour = "grey10")+
   geom_line(aes(x = Common.name, y = median1993, colour = "grey10"), colour = "grey10") +
   coord_flip(ylim = c(4.5, 98)) +
   scale_size_manual(values = c(5, 3, 1)) +
   labs(x = "", y = "Probability that extinction\nhas been prevented", title = "1993 – 2020", tag = "a)") +
   theme_classic() +
   theme(axis.ticks.length = unit(.05, "cm"),
         text = element_text(size = 17),
         panel.spacing = unit(2, "lines"),
         legend.position = "none",
         plot.title = element_text(hjust = 0.5),
         plot.margin = unit(c(5.5, 5.5, 5.5, 30),"pt"),
         plot.tag.position = c(0.06, 0.99)))


## Add lines and text on left

(b <- ggdraw() +
    draw_plot(a) +
    draw_line(x = c(0.08, 0.08), y = c(0.849, 0.96), color = "#DCE318FF", size = 1) +
    draw_line(x = c(0.08, 0.1), y = c(0.96, 0.96), color = "#DCE318FF", size = 1) +
    draw_line(x = c(0.08, 0.1), y = c(0.849, 0.849), color = "#DCE318FF", size = 1) +
    draw_text("Extinct in\nthe Wild", vjust = 0, x = 0.07, y = 0.903, angle = 90, colour = "#DCE318FF") +
    
    draw_line(x = c(0.08, 0.08), y = c(0.554, 0.845), color = "#94D840FF", size = 1) +
    draw_line(x = c(0.08, 0.1), y = c(0.845, 0.845), color = "#94D840FF", size = 1) +
    draw_line(x = c(0.08, 0.1), y = c(0.554, 0.554), color = "#94D840FF", size = 1) +
    draw_text("Median \u2265 90", vjust = 0, x = 0.07, y = 0.698, angle = 90, colour = "#94D840FF") +
    
    draw_line(x = c(0.08, 0.08), y = c(0.344, 0.55), color = "#56C667FF", size = 1) +
    draw_line(x = c(0.08, 0.1), y = c(0.55, 0.55), color = "#56C667FF", size = 1) +
    draw_line(x = c(0.08, 0.1), y = c(0.344, 0.344), color = "#56C667FF", size = 1) +
    draw_text("50 < Median < 90", vjust = 0, x = 0.07, y = 0.445, angle = 90, colour = "#56C667FF") +
    
    draw_line(x = c(0.08, 0.08), y = c(0.08, 0.34), color = "#39558CFF", size = 1) +
    draw_line(x = c(0.08, 0.1), y = c(0.34, 0.34), color = "#39558CFF", size = 1) +
    draw_line(x = c(0.08, 0.1), y = c(0.08, 0.08), color = "#39558CFF", size = 1) +
    draw_text("Median \u2264 50", vjust = 0, x = 0.07, y = 0.21, angle = 90, colour = "#39558CFF"))


## Make bird plot for 2010 - 2020

(c <- filter(allsppmed, class == "AVES" & !is.na(median2010)) %>% 
    ggplot() +
    geom_point(aes(x = fct_reorder(Common.name, order2010), y = median2010), alpha = 0) +
    geom_rect(data = rect, aes(xmin = 0, xmax = 29.6, ymin = ymin, ymax = ymax, fill = fill), alpha = 0.8) +
    scale_fill_viridis(labels = c("Very unlikely", 
                                  "Quite unlikely", 
                                  "Quite possible but unlikely", 
                                  "More likely than not", 
                                  "Quite likely", 
                                  "Very likely", 
                                  "Virtually certain"),
                       guide = "legend", name = "Probability") +
    geom_point(aes(x = fct_reorder(Common.name, order2010), y = median2010, size = agreement2010), colour = "grey10") +
    geom_line(aes(x = Common.name, y = median2010, colour = "grey10"), colour = "grey10") +
    coord_flip(ylim = c(4.5, 98)) +
    scale_size_manual(values = c(5, 3, 1), name = "Level of\nagreement", drop = FALSE) +
    labs(x = "", y = "Probability that extinction\nhas been prevented", title = "2010 – 2020", tag = "b)") +
    theme_classic() +
    theme(axis.ticks.length = unit(.05, "cm"),
          text = element_text(size = 17),
          plot.margin = unit(c(5.5, 5.5, 185, 5.5),"pt"),
          legend.position = c(-0.2, -0.13),
          legend.justification = c("left", "top"),
          legend.box = "horizontal", 
          legend.direction = "vertical",
          plot.title = element_text(hjust = 0.5),
          plot.tag.position = c(0.06, 0.99)) + 
    guides(colour = guide_legend(override.aes = list(size = 5), order = 2),
           fill = guide_legend(order = 1),
           size = guide_legend(order = 3)))


## Add lines and text on left

(d <- ggdraw() +
    draw_plot(c) +
    draw_line(x = c(0.03, 0.03), y = c(0.834, 0.956), color = "#DCE318FF", size = 1) +
    draw_line(x = c(0.03, 0.05), y = c(0.956, 0.956), color = "#DCE318FF", size = 1) +
    draw_line(x = c(0.03, 0.05), y = c(0.834, 0.834), color = "#DCE318FF", size = 1) +
    
    draw_line(x = c(0.03, 0.03), y = c(0.769, 0.83), color = "#94D840FF", size = 1) +
    draw_line(x = c(0.03, 0.05), y = c(0.83, 0.83), color = "#94D840FF", size = 1) +
    draw_line(x = c(0.03, 0.05), y = c(0.769, 0.769), color = "#94D840FF", size = 1) +
    
    draw_line(x = c(0.03, 0.03), y = c(0.574, 0.765), color = "#56C667FF", size = 1) +
    draw_line(x = c(0.03, 0.05), y = c(0.765, 0.765), color = "#56C667FF", size = 1) +
    draw_line(x = c(0.03, 0.05), y = c(0.574, 0.574), color = "#56C667FF", size = 1) +
    
    draw_line(x = c(0.03, 0.03), y = c(0.33, 0.57), color = "#39558CFF", size = 1) +
    draw_line(x = c(0.03, 0.05), y = c(0.57, 0.57), color = "#39558CFF", size = 1) +
    draw_line(x = c(0.03, 0.05), y = c(0.33, 0.33), color = "#39558CFF", size = 1))


## Arrange and save

e <- grid.arrange(b, d, nrow = 1)
ggsave("output/median_scores_birds.pdf", e, dpi = 300, width = 15, height = 10, device = cairo_pdf)
ggsave("output/median_scores_birds.png", e, dpi = 300, width = 381, height = 254, units = "mm")



#### ----------------------------- Mammals ----------------------------------------####

## Make mammal plot for 1993 - 2020

(f <- filter(allsppmed, class == "MAMMALIA") %>% 
   ggplot() +
   geom_point(aes(x = fct_reorder(Common.name, order1993), y = median1993), alpha = 0)  +
   geom_rect(data = rect, aes(xmin = 0, xmax = 24.6, ymin = ymin, ymax = ymax, fill = fill), alpha = 0.8) +
   scale_fill_viridis() +
   geom_point(aes(x = fct_reorder(Common.name, order1993), y = median1993, size = agreement1993), colour = "grey10")+
   geom_line(aes(x = Common.name, y = median1993, colour = "grey10"), colour = "grey10") +
   coord_flip(ylim = c(4.5, 98)) +
   scale_size_manual(values = c(5, 3, 1)) +
   labs(x = "", y = "Probability that extinction\nhas been prevented", title = "1993 – 2020", tag = "a)") +
   theme_classic() +
   theme(axis.ticks.length = unit(.05, "cm"),
         text = element_text(size = 15),
         panel.spacing = unit(2, "lines"),
         legend.position = "none",
         plot.title = element_text(hjust = 0.5),
         plot.margin = unit(c(5.5, 5.5, 5.5, 30),"pt"),
         plot.tag.position = c(0.06, 0.99)))


## Add lines and text on left

(g <- b <- ggdraw() +
    draw_plot(f) +
    draw_line(x = c(0.08, 0.08), y = c(0.849, 0.955), color = "#DCE318FF", size = 1) +
    draw_line(x = c(0.08, 0.1), y = c(0.955, 0.955), color = "#DCE318FF", size = 1) +
    draw_line(x = c(0.08, 0.1), y = c(0.849, 0.849), color = "#DCE318FF", size = 1) +
    draw_text("Extinct in\nthe Wild", vjust = 0, x = 0.07, y = 0.902, angle = 90, colour = "#DCE318FF") +
    
    draw_line(x = c(0.08, 0.08), y = c(0.714, 0.845), color = "#94D840FF", size = 1) +
    draw_line(x = c(0.08, 0.1), y = c(0.845, 0.845), color = "#94D840FF", size = 1) +
    draw_line(x = c(0.08, 0.1), y = c(0.714, 0.714), color = "#94D840FF", size = 1) +
    draw_text("Median \u2265 90", vjust = 0, x = 0.07, y = 0.78, angle = 90, colour = "#94D840FF") +
    
    draw_line(x = c(0.08, 0.08), y = c(0.384, 0.71), color = "#56C667FF", size = 1) +
    draw_line(x = c(0.08, 0.1), y = c(0.71, 0.71), color = "#56C667FF", size = 1) +
    draw_line(x = c(0.08, 0.1), y = c(0.384, 0.384), color = "#56C667FF", size = 1) +
    draw_text("50 < Median < 90", vjust = 0, x = 0.07, y = 0.547, angle = 90, colour = "#56C667FF") +
    
    draw_line(x = c(0.08, 0.08), y = c(0.09, 0.38), color = "#39558CFF", size = 1) +
    draw_line(x = c(0.08, 0.1), y = c(0.38, 0.38), color = "#39558CFF", size = 1) +
    draw_line(x = c(0.08, 0.1), y = c(0.09, 0.09), color = "#39558CFF", size = 1) +
    draw_text("Median \u2264 50", vjust = 0, x = 0.07, y = 0.235, angle = 90, colour = "#39558CFF"))


## Make mammal plot for 2010 - 2020

(h <- filter(allsppmed, class == "MAMMALIA" & !is.na(median2010)) %>% 
    ggplot() +
    geom_point(aes(x = fct_reorder(Common.name, order2010), y = median2010), alpha = 0) +
    geom_rect(data = rect, aes(xmin = 0, xmax = 19.6, ymin = ymin, ymax = ymax, fill = fill), alpha = 0.8) +
    scale_fill_viridis(labels = c("Very unlikely", 
                                  "Quite unlikely", 
                                  "Quite possible but unlikely", 
                                  "More likely than not", 
                                  "Quite likely", 
                                  "Very likely", 
                                  "Virtually certain"),
                       guide = "legend", name = "Probability") +
    geom_point(aes(x = fct_reorder(Common.name, order2010), y = median2010, size = agreement2010), colour = "grey10") +
    geom_line(aes(x = Common.name, y = median2010, colour = "grey10"), colour = "grey10") +
    coord_flip(ylim = c(4.5, 98)) +
    scale_size_manual(values = c(5, 3, 1), name = "Level of\nagreement", drop = FALSE) +
    labs(x = "", y = "Probability that extinction\nhas been prevented", title = "2010 – 2020", tag = "b)") +
    theme_classic() +
    theme(axis.ticks.length = unit(.05, "cm"),
          text = element_text(size = 15),
          plot.margin = unit(c(5.5, 5.5, 160, 5.5),"pt"),
          legend.position = c(-0.2, -0.15),
          legend.justification = c("left", "top"),
          legend.box = "horizontal", 
          legend.direction = "vertical",
          plot.title = element_text(hjust = 0.5),
          plot.tag.position = c(0.06, 0.99)) + 
    guides(colour = guide_legend(override.aes = list(size = 5), order = 2),
           fill = guide_legend(order = 1),
           size = guide_legend(order = 3)))


## Add lines and text on left

(i <- ggdraw() +
    draw_plot(h) +
    draw_line(x = c(0.03, 0.03), y = c(0.894, 0.956), color = "#DCE318FF", size = 1) +
    draw_line(x = c(0.03, 0.05), y = c(0.956, 0.956), color = "#DCE318FF", size = 1) +
    draw_line(x = c(0.03, 0.05), y = c(0.894, 0.894), color = "#DCE318FF", size = 1) +
    
    draw_line(x = c(0.03, 0.03), y = c(0.739, 0.89), color = "#56C667FF", size = 1) +
    draw_line(x = c(0.03, 0.05), y = c(0.89, 0.89), color = "#56C667FF", size = 1) +
    draw_line(x = c(0.03, 0.05), y = c(0.739, 0.739), color = "#56C667FF", size = 1) +
    
    draw_line(x = c(0.03, 0.03), y = c(0.35, 0.735), color = "#39558CFF", size = 1) +
    draw_line(x = c(0.03, 0.05), y = c(0.735, 0.735), color = "#39558CFF", size = 1) +
    draw_line(x = c(0.03, 0.05), y = c(0.35, 0.35), color = "#39558CFF", size = 1))


## Arrange and save

j <- grid.arrange(g, i, nrow = 1)
ggsave("output/median_scores_mammals.pdf", j, dpi = 300, width = 13.5, height = 8, device = cairo_pdf)
ggsave("output/median_scores_mammals.png", j, dpi = 300, width = 381, height = 254, units = "mm")


