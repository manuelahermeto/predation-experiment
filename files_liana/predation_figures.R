---
author: "Liana Chesini Rossi"
date: "19/10/2021"
html_document: default
pdf_document: default
word_document: default
---

  
setwd("C:/Users/cliente/Documents/Doutorado/Chapter1_Predation_rates/analises")


rm(list=ls())


################################################################################
# Forest class predation figures ###############################################
################################################################################



library(ggplot2)
library(dplyr)
library(reshape2)  # melt function
library(ggpubr)    # Combine Multiple GGPlots into a Figure



# read the table
proportion_predation <- data.frame(read.table(file.choose(), header = T, sep = "\t"))
# Data come from the csv called 'predation_figures.csv'
str(proportion_predation)


################################################################################
# FIGURE MODEL 1 - Predation in pre-EN forest 

# plot with predation incidence (%) in unburned forests

# Remove the burned forests
forest_class = proportion_predation %>%
  filter(fire_2015 == "unburned")
str(forest_class)

# Organize the data to include in the model
forest_class_pred <- melt(forest_class, id.vars = "forest_class",
                         measure.vars=c("total"))
str(forest_class_pred)

# Check values wich include just unburned forest
forest_class_pred$value

predation_forest_class_bar = ggplot(data= forest_class_pred, aes(x = forest_class, y = value, fill = forest_class))+
  scale_x_discrete(limits = c("UF", "LF", "LBF", "SF")) +
  geom_boxplot (position = "dodge",
                fill= "#37ABC8") + 
  coord_cartesian (ylim = c(32,78)) +
  labs(x = "Forest class",
       y = "Predation incidence (%)") +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  theme_classic() +
  theme (axis.title = element_text(size = 10), 
         axis.text = element_text(size = 10),
         legend.text = element_text(size = 10),
         legend.title = element_text(size = 10),
         legend.position = "none",
         legend.background = element_rect(fill="transparent")) +
  theme (panel.border = element_blank(), strip.background = element_rect (color = "transparent")) +
  guides (color = guide_legend (override.aes = list (size = 3)))
predation_forest_class_bar

# save the figure pdf
ggsave("predation_forest_class_bar.pdf", width = 4, height = 4)



################################################################################
# FIGURE MODEL 2 - Predation in EN-fire-affected forests 

# plot with predation incidence (%) in burned forests
# Filtra as secundparias
forest_burned = proportion_predation %>%
  filter(forest_class != "SF")
str(forest_burned)

# organiza os dados para serem plotados
forest_burned_pred <- melt(forest_burned, id.vars = "forest_class", "fire_2015",
                          measure.vars=c("unburned", "burned"))
str(forest_burned_pred)

predation_forest_burned_bar_semleg = ggplot(data= forest_burned_pred, aes(x = forest_class, y = value, fill = fire_2015))+
  scale_x_discrete(limits = c("UF", "LF", "LBF", "SF")) +
  geom_boxplot (position = "dodge") +
  scale_fill_manual (name= "Fire El Niño 2015-16", values = c("#37ABC8","#A02C2C")) + 
  coord_cartesian (ylim = c(0,100)) +
  labs(x = "Forest class",
       y = "Predation incidence (%)") +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  theme_classic() +
  theme (axis.text = element_text(size = 10), 
         legend.text = element_text(size = 10),
         legend.title = element_text(size = 10), 
         legend.position = "none",
         legend.background = element_rect(fill="transparent")) +
  theme (panel.border = element_blank(), strip.background = element_rect (color = "transparent")) +
  guides (color = guide_legend (override.aes = list (size = 3)))
predation_forest_burned_bar_semleg

# save the figure
ggsave("predation_forest_burned_barAB.pdf", width = 4, height = 4)


# Combine GGPlots 2a and 2b into a Figure
figure2_semleg <- ggarrange(predation_forest_class_bar, predation_forest_burned_bar_semleg,
                         labels = c("(a)", "(b)"), ncol = 2, nrow = 1)
figure2_semleg

# save the figure
ggsave("figure2_semleg.pdf", width = 8, height = 4)



################################################################################
# FIGURE MODELS 3 - 4 - Arthropod and non-arthropd predator in pre-EN forest
# disturbances


proportion_predation2 <- data.frame(read.table(file.choose(), header = T, sep = "\t"))
# Data come from the csv called 'predation_figures1.csv'
str(proportion_predation2)

# Filtra as secundparias e as florestas que nao queimaram do gráfico
forest_class_fig2 = proportion_predation2 %>%
  filter(fire_2015 =="unburned")
str(forest_class_fig2)


################################################################################
## FIGURE MODELS 3 - Arthropod predator in pre-EN fores

# organiza os dados para serem plotados
forest_class_pred2 <- melt(forest_class_fig2, id.vars = "forest_class",
                          measure.vars=c("arthropod"))
str(forest_class_pred2)

# without legend
predation_plot_bar3a = ggplot(data= forest_class_pred2, aes(x = forest_class, y = value, fill = forest_class)) +
  scale_x_discrete(limits = c("UF", "LF", "LBF", "SF")) +
  geom_boxplot (position = "dodge",
                fill= "#37ABC8") + 
  coord_cartesian (ylim = c(75,100)) +
  labs(x = "Forest class",
       y = "Predation incidence (%)",
       title = "Arthropod") +
  theme_classic() +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  theme (axis.title = element_text(size = 10), 
         axis.text = element_text(size = 10), 
         legend.text = element_text(size = 10),
         legend.title = element_text(size = 10),
         legend.position = "none",
         plot.title = element_text(hjust = 0.5)) +
  theme (panel.border = element_blank(), strip.background = element_rect (color = "transparent")) +
  guides (color = guide_legend (override.aes = list (size = 3)))
predation_plot_bar3a

# save the figure
ggsave("predation_plot_bar3a.pdf", width = 4, height = 4)


################################################################################
##  FIGURE MODELS 4 - Non-arthropd predator in pre-EN fores

# organiza os dados para serem plotados
forest_class_pred3 <- melt(forest_class_fig2, id.vars = "forest_class",
                           measure.vars=c("multigroup"))
str(forest_class_pred3)

predation_plot_bar3b = ggplot(data= forest_class_pred3, aes(x = forest_class, y = value, fill = forest_class))+
  scale_x_discrete(limits = c("UF", "LF", "LBF", "SF")) +
  geom_boxplot (position = "dodge",
                fill= "#37ABC8") + 
  coord_cartesian (ylim = c(0,25)) +
  labs(x = "Forest class",
       y = "Predation incidence (%)",
       title = "No-arthropod") +
  theme_classic() +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  theme (axis.title = element_text(size = 10), 
         axis.text = element_text(size = 10), 
         legend.text = element_text(size = 10),
         legend.title = element_text(size = 10),
         legend.position = "none",
         plot.title = element_text(hjust = 0.5)) +
  theme (panel.border = element_blank(), strip.background = element_rect (color = "transparent")) +
  guides (color = guide_legend (override.aes = list (size = 3)))
predation_plot_bar3b

# save the figure
ggsave("predation_plot_bar3b.pdf", width = 4, height = 4)


# Combine GGPlots 3a and 3b into a Figure
figure3 <- ggarrange(predation_plot_bar3a, predation_plot_bar3b,
                    labels = c("(a)", "(b)"), ncol = 2, nrow = 1)
figure3

# save the figure
ggsave("figure3.pdf", width = 8, height = 4)



################################################################################
# FIGURES MODELS 5 - 6 - Arthropod and non-artropod predators in EN-fire-affected 
# forests 


# Remove secondary forest
forest_class_art1 = proportion_predation %>%
  filter(forest_class != "SF")
str(forest_class_art1)


################################################################################
## FIGURES MODEL 5 - Arthropod predators in EN-fire-affected 

# organiza os dados para serem plotados
#forest_class_art <- melt(forest_class_art1, id.vars = "forest_class", 
# "arthropod","fire_2015", measure.vars=c("unburned", "burned"))
#str(forest_class_art)

# organiza os dados para serem plotados from unburned to burned
forest_class_art1$fire_2015 <-factor(forest_class_art1$fire_2015, levels = c("unburned", "burned"))

predation_plot_bar4a = ggplot(data= forest_class_art1, aes(x = forest_class, y = arthropod, fill = fire_2015))+
  scale_x_discrete(limits = c("UF", "LF", "LBF")) +
  geom_boxplot (position = "dodge") +
  scale_fill_manual (name= "Fire El Niño 2015-16", values = c("#37ABC8","#A02C2C")) + 
  coord_cartesian (ylim = c(70,100)) +
  labs(x = "Forest class",
       y = "Predation incidence (%)",
       title = "Arthropod") +
  theme_classic() +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  theme (axis.title = element_text(size = 10), 
         axis.text = element_text(size = 10), 
         legend.text = element_text(size = 10),
         legend.title = element_text(size = 10),
         legend.position = "none",
         plot.title = element_text(hjust = 0.5)) +
  theme (panel.border = element_blank(), strip.background = element_rect (color = "transparent")) +
  guides (color = guide_legend (override.aes = list (size = 3)))
predation_plot_bar4a

# save the figure
ggsave("predation_plot_bar4a.pdf", width = 4, height = 4)


################################################################################
# FIGURES MODEL 6 - Non-artropod predators in EN-fire-affected 

forest_class_mult = proportion_predation2 %>%
  filter(forest_class != "SF")
str(forest_class_mult) 

# organiza os dados para serem plotados
forest_class_multigroup <- melt(proportion_predation2, id.vars = "forest_class", "multigroup","fire_2015",
                          measure.vars=c("burned", "unburned"))

forest_class_mult$fire_2015 <-factor(forest_class_mult$fire_2015, levels = c("unburned", "burned"))

predation_plot_bar4b = ggplot(data= forest_class_mult, aes(x = forest_class, y = multigroup, fill = fire_2015))+
  scale_x_discrete(limits = c("UF", "LF", "LBF")) +
  geom_boxplot (position = "dodge") +
  scale_fill_manual (name= "Fire El Niño 2015-16", values = c("#37ABC8","#A02C2C")) + 
  coord_cartesian (ylim = c(0,40)) +
  labs(x = "Forest class",
       y = "Predation incidence (%)",
       title = "No-arthropod") +
  theme_classic() +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  theme (axis.title = element_text(size = 10), 
         axis.text = element_text(size = 10), 
         legend.text = element_text(size = 8),
         legend.title = element_text(size = 8),
         legend.position = "none",
         plot.title = element_text(hjust = 0.5)) +
  theme (panel.border = element_blank(), strip.background = element_rect (color = "transparent")) +
  guides (color = guide_legend (override.aes = list (size = 3)))
predation_plot_bar4b

# save the figure
ggsave("predation_plot_bar4b.pdf", width = 4, height = 4)


# Combine GGPlots 4a and 4b into a Figure
figure4 <- ggarrange(predation_plot_bar4a, predation_plot_bar4b,
                    labels = c("(a)", "(b)"), ncol = 2, nrow = 1)
figure4

# save the figure
ggsave("figure4.pdf", width = 8, height = 4)


# Combine GGPlots 3 a-b and 4-b into a Figure
figure_3abcd <- ggarrange(predation_plot_bar3a, predation_plot_bar4a, predation_plot_bar3b, predation_plot_bar4b,
                        labels = c("(a)", "(b)", "(c)", "(d)"), ncol = 2, nrow = 2)
figure_3abcd

# save the figure
ggsave("figure_3abcd.pdf", width = 8, height = 8)



##### THE END




################################################################################
### Y AXIS EXPANDED (0 - 100) AND DOTS REMOVED #################################
################################################################################


################################################################################
### # FIGURE MODEL 1 - Predation in pre-EN forest 

# plot with predation incidence (%) in unburned forests 
# Y AXIS EXPANDED (0 - 100) and dots removed 

# plot with predation incidence (%) in unburned forests

# Remove the burned forests
forest_class = proportion_predation %>%
  filter(fire_2015 == "unburned")
str(forest_class)

# Remove secondary forest
forest_class_art1 = proportion_predation %>%
  filter(forest_class != "SF")
str(forest_class_art1)

# Organize the data to include in the model
forest_class_pred <- melt(forest_class, id.vars = "forest_class",
                          measure.vars=c("total"))
str(forest_class_pred)

# Check values wich include just unburned forest
forest_class_pred$value

predation_forest_class_bar_leg = ggplot(data= forest_class_pred, aes(x = forest_class, y = value, fill = forest_class))+
  scale_x_discrete(limits = c("UF", "LF", "LBF", "SF")) +
  geom_boxplot (position = "dodge",
                fill= "#37ABC8") + 
  coord_cartesian (ylim = c(0,100)) +
  labs(x = "Forest class",
       y = "Predation incidence (%)") +
  theme_classic() +
  theme (axis.title = element_text(size = 10), 
         axis.text = element_text(size = 10),
         legend.text = element_text(size = 10),
         legend.title = element_text(size = 10),
         legend.position = "none",
         legend.background = element_rect(fill="transparent")) +
  theme (panel.border = element_blank(), strip.background = element_rect (color = "transparent")) +
  guides (color = guide_legend (override.aes = list (size = 3)))
predation_forest_class_bar_leg

# save the figure pdf
ggsave("predation_forest_class_bar_leg.pdf", width = 4, height = 4)



################################################################################
# FIGURE MODEL 2 - Predation in EN-fire-affected forests 

# plot with predation incidence (%) in burned forests
# Y AXIS EXPANDED (0 - 100) AND DOTS REMOVED 

# Filtra as secundparias
forest_burned = proportion_predation %>%
  filter(forest_class != "SF")
str(forest_burned)

# organiza os dados para serem plotados
forest_burned_pred <- melt(forest_burned, id.vars = "forest_class", "fire_2015",
                           measure.vars=c("unburned", "burned"))
str(forest_burned_pred)

# inclui as SF pra juntar as duas figuras 2ab
predation_forest_burned_bar_leg = ggplot(data= forest_burned_pred, aes(x = forest_class, y = value, fill = fire_2015))+
  scale_x_discrete(limits = c("UF", "LF", "LBF", "SF")) +
  geom_boxplot (position = "dodge") +
  scale_fill_manual (name= "Fire El Niño 2015-16", values = c("#37ABC8","#A02C2C")) + 
  coord_cartesian (ylim = c(0,100)) +
  labs(x = "Forest class",
       y = "Predation incidence (%)") +
  theme_classic() +
  theme (axis.text = element_text(size = 10), 
         legend.text = element_text(size = 10),
         legend.title = element_text(size = 10), 
         legend.position = "none",
         legend.background = element_rect(fill="transparent")) +
  theme (panel.border = element_blank(), strip.background = element_rect (color = "transparent")) +
  guides (color = guide_legend (override.aes = list (size = 3)))
predation_forest_burned_bar_leg

# save the figure
ggsave("predation_forest_burned_bar_leg.pdf", width = 4, height = 4)


# Combine GGPlots 4a and 4b into a Figure
figure2_leg <- ggarrange(predation_forest_class_bar_leg, predation_forest_burned_bar_leg,
                     labels = c("(a)", "(b)"), ncol = 2, nrow = 1)
figure2_leg

# save the figure
ggsave("figure2_leg.pdf", width = 8, height = 4)



################################################################################
# FIGURE MODELS 3 - 4 - Arthropod and non-arthropd predator in pre-EN forest
# disturbances

# Y AXIS EXPANDED (0 - 100) AND DOTS REMOVED 

proportion_predation2 <- data.frame(read.table(file.choose(), header = T, sep = "\t"))
# Data come from the csv called 'predation_figures1.csv'
str(proportion_predation2)

# barplot com dados de artrópodes e lagartas predadas por mais de um predador 
# (aves, mamíferos e répteis) em florestas não queimadas 

# Filtra as secundparias e as florestas que nao queimaram do gráfico
forest_class_fig2 = proportion_predation2 %>%
  filter(fire_2015 =="unburned")
str(forest_class_fig2)


################################################################################
# FIGURE MODELS 3 Arthropod predation in pre-EN forest disturbances

# organiza os dados para serem plotados
forest_class_pred_art <- melt(forest_class_fig2, id.vars = "forest_class",
                           measure.vars=c("arthropod"))
str(forest_class_pred_art)

# without legend
predation_plot_bar3a_art = ggplot(data= forest_class_pred_art, aes(x = forest_class, y = value, fill = forest_class)) +
  scale_x_discrete(limits = c("UF", "LF", "LBF", "SF")) +
  geom_boxplot (position = "dodge",
                fill= "#37ABC8") + 
  coord_cartesian (ylim = c(0,100)) +
  labs(x = "Forest class",
       y = "Predation incidence %",
       title = "Arthropod") +
  theme_classic() +
  theme (axis.title = element_text(size = 10), 
         axis.text = element_text(size = 10), 
         legend.text = element_text(size = 10),
         legend.title = element_text(size = 10),
         legend.position = "none",
         plot.title = element_text(hjust = 0.5)) +
  theme (panel.border = element_blank(), strip.background = element_rect (color = "transparent")) +
  guides (color = guide_legend (override.aes = list (size = 3)))
predation_plot_bar3a_art

# save the figure
ggsave("predation_plot_bar3a_art.pdf", width = 4, height = 4)


################################################################################
# FIGURE MODEL 4 - Non-arthropd predator in pre-EN forest disturbances

# organiza os dados para serem plotados
forest_class_pred3_mult <- melt(forest_class_fig2, id.vars = "forest_class",
                           measure.vars=c("multigroup"))
str(forest_class_pred3_mult)

predation_plot_bar3b_mult = ggplot(data= forest_class_pred3_mult, aes(x = forest_class, y = value, fill = forest_class))+
  scale_x_discrete(limits = c("UF", "LF", "LBF", "SF")) +
  geom_boxplot (position = "dodge",
                fill= "#37ABC8") + 
  coord_cartesian (ylim = c(0,100)) +
  labs(x = "Forest class",
       y = "Predation incidence %",
       title = "No-arthropod") +
  theme_classic() +
  theme (axis.title = element_text(size = 10), 
         axis.text = element_text(size = 10), 
         legend.text = element_text(size = 10),
         legend.title = element_text(size = 10),
         legend.position = "none",
         plot.title = element_text(hjust = 0.5)) +
  theme (panel.border = element_blank(), strip.background = element_rect (color = "transparent")) +
  guides (color = guide_legend (override.aes = list (size = 3)))
predation_plot_bar3b_mult

# save the figure
ggsave("predation_plot_bar3b_mult.pdf", width = 4, height = 4)


# Combine GGPlots 3a and 3b into a Figure
figure3_leg <- ggarrange(predation_plot_bar3a_art, predation_plot_bar3b_mult,
                     labels = c("(a)", "(b)", ncol = 2, nrow = 1))
figure3_leg

# save the figure
ggsave("figure3_leg.pdf", width = 8, height = 4)


################################################################################
# FIGURES MODELS 5 - 6 - Arthropod and non-artropod predators in EN-fire-affected 
# forests 

# organiza os dados para serem plotados
#forest_class_art <- melt(forest_class_art1, id.vars = "forest_class", "arthropod","fire_2015",
#  measure.vars=c("unburned", "burned"))
#str(forest_class_art)

# Remove secondary forest
forest_class_art1 = proportion_predation %>%
  filter(forest_class != "SF")
str(forest_class_art1)


################################################################################
# FIGURE MODEL 5 Arthropod predators in EN-fire-affected forests 


# organiza os dados para serem plotados from unburned to burned
forest_class_art1$fire_2015 <-factor(forest_class_art1$fire_2015, levels = c("unburned", "burned"))

predation_plot_bar4ac_art = ggplot(data= forest_class_art1, aes(x = forest_class, y = arthropod, fill = fire_2015))+
  scale_x_discrete(limits = c("UF", "LF", "LBF", "SF")) +
  geom_boxplot (position = "dodge") +
  scale_fill_manual (name= "Fire El Niño 2015-16", values = c("#37ABC8","#A02C2C")) + 
  coord_cartesian (ylim = c(0,100)) +
  labs(x = "Forest class",
       y = "Predation incidence %",
       title = "Arthropod") +
  theme_classic() +
  theme (axis.title = element_text(size = 10), 
         axis.text = element_text(size = 10), 
         legend.text = element_text(size = 10),
         legend.title = element_text(size = 10),
         legend.position = "none",
         plot.title = element_text(hjust = 0.5)) +
  theme (panel.border = element_blank(), strip.background = element_rect (color = "transparent")) +
  guides (color = guide_legend (override.aes = list (size = 3)))
predation_plot_bar4ac_art

# save the figure
ggsave("predation_plot_bar4ac_art.pdf", width = 4, height = 4)


################################################################################
# FIGURE MODEL 6 - Non-artropod predators in EN-fire-affected forests 


forest_class_mult = proportion_predation2 %>%
  filter(forest_class != "SF")
str(forest_class_mult) 

# organiza os dados para serem plotados
forest_class_multigroup <- melt(proportion_predation2, id.vars = "forest_class", "multigroup","fire_2015",
                                measure.vars=c("burned", "unburned"))

forest_class_mult$fire_2015 <-factor(forest_class_mult$fire_2015, levels = c("unburned", "burned"))

predation_plot_bar4bd_mult = ggplot(data= forest_class_mult, aes(x = forest_class, y = multigroup, fill = fire_2015))+
  scale_x_discrete(limits = c("UF", "LF", "LBF", "SF")) +
  geom_boxplot (position = "dodge") +
  scale_fill_manual (name= "Fire El Niño 2015-16", values = c("#37ABC8","#A02C2C")) + 
  coord_cartesian (ylim = c(0,100)) +
  labs(x = "Forest class",
       y = "Predation incidence %",
       title = "No-arthropod") +
  theme_classic() +
  theme (axis.title = element_text(size = 10), 
         axis.text = element_text(size = 10), 
         legend.text = element_text(size = 8),
         legend.title = element_text(size = 8),
         legend.position = "none",
         plot.title = element_text(hjust = 0.5)) +
  theme (panel.border = element_blank(), strip.background = element_rect (color = "transparent")) +
  guides (color = guide_legend (override.aes = list (size = 3)))
predation_plot_bar4bd_mult

# save the figure
ggsave("predation_plot_bar4bd_mult.pdf", width = 4, height = 4)


#OFICIAL PAPER FIGURE 3
#### Combine GGPlots 3a and b into a Figure (juntei as secundárias ao fogo)
figure3 <- ggarrange(predation_plot_bar4ac_art, predation_plot_bar4bd_mult,
                         labels = c("(a)", "(b)"), ncol = 2, nrow = 1)
figure3

### save the figure
ggsave("figure3.pdf", width = 8, height = 4)


#THE END


# Combine GGPlots 4a and 4b into a Figure
#  <- ggarrange(predation_plot_bar4a_art, predation_plot_bar4b_mult,
#                     labels = c("(a)", "(b)"), ncol = 2, nrow = 1)
#figure4_leg

# save the figure
#ggsave("figure4_leg.pdf", width = 8, height = 4)

# Combine GGPlots 3 a-b and 4-b into a Figure
# figure_leg <- ggarrange(predation_plot_bar3a_art, predation_plot_bar4a_art, predation_plot_bar3b_mult, 
#                        predation_plot_bar4b_mult,
#                         labels = c("(a)", "(b)", "(c)", "(d)"), ncol = 2, nrow = 2)
# figure_leg

# save the figure
# ggsave("figure_leg.pdf", width = 8, height = 8)

# Combine GGPlots 3 a-b and 4-b into a Figure (changed the order to show the differences)
# figure_3abcd_100y_withoudots <- ggarrange(predation_plot_bar3a_art, predation_plot_bar3b_mult, predation_plot_bar4a_art,  
#                        predation_plot_bar4b_mult,
#                        labels = c("(a)", "(b)", "(c)", "(d)"), ncol = 2, nrow = 2)
# figure_3abcd_100y_withoudots

# save the figure
# ggsave("figure_3abcd_100y_withoudots.pdf", width = 8, height = 8)
