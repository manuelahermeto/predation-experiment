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
### Predation incidence models #################################################
################################################################################



library("lme4")    # glmer and drop1
library("dplyr")   # put forest in order 
library("emmeans") # emmeans
library("DHARMa")  # test residual



# read the table
predation_data1 <- data.frame(read.table(file.choose(), header = T, sep = "\t"))
# Data come from the csv called 'predation.chapter1.txt'
str(predation_data1)



################################################################################
## 1. Predation in pre-EN forest disturbance class

## What is the response variable?
# total predation 
## What is the predictor variable?
# pre-EN forest disturbance class (UF, LF, LBF, S)

# Model: predation ~ forest_class + ( 1 | trans_cod)


# filter to use just unburned forest and exclude lost and doubt caterpillars
forest_predation <- predation_data1 %>% 
  filter(fire_2015 == 0, lost == 0, excluded == 0)
# # check the data
names(forest_predation)
str(forest_predation)

# Put undisturbed forest (UF) as a reference to compare among other forests
forest_predation$forest_class = relevel(factor(forest_predation$forest_class), 
                                        ref = "UF")

# Modeling glmer - maximum likelihood: Laplace Approximation and 
# link function-logit 
forest_modell <- glmer(predation ~ forest_class + ( 1 | trans_cod), 
                      data = forest_predation, 
                      family = binomial)
summary(forest_modell)

#likelihood ratio test
drop1(forest_modell, test = "Chisq")  
# There is no differences among pre-EN forest disturbance class 

# Test residual overdispersion  
residuals <- simulateResiduals(fittedModel = forest_modell, n = 1000)
plot(residuals) # normal distribution



################################################################################
## 2. Predation in EN-fire-affected forests


## What is the response variable?
# total predation 
## What is the predictor variable?
# pre-EN forest disturbance class (UF, LF, LBF, SF) and 
# EN-fire-affected plots (0 whitouth fire, 1 with fire)

# Model: predation ~ forest class + fire + forest class * fire, random factor=plot


# maintain SF and filter caterpillar didn't lost and in doubt
forest_predation2 <- predation_data1 %>% 
  filter(lost == 0, excluded == 0)

# check the data without secondary forests, lost and excluded caterpillars
names(forest_predation2)
str(forest_predation2$forest_class)
na.omit(forest_predation2$forest_class)
na.omit(forest_predation2$lost)

# Put undisturbed forest (UF) as a reference to compare among other forests
forest_predation2$forest_class = relevel(factor(forest_predation2$forest_class), 
                                         ref = "UF")

# Modeling glmer - maximum likelihood: Laplace Approximation and link function-logit 
forest_modell2 <- glmer(predation ~ fire_2015 + forest_class*fire_2015 + 
                         ( 1 | trans_cod), 
                       data = forest_predation2, 
                       family = binomial)
summary(forest_modell2)   

# likelihood ratio test
drop1(forest_modell2, test = "Chisq") 
# There is differences among pre-EN forest disturbance class and EN-fire-affected 
# forests

#paraiwise post-hoc test with emmeans function
paiwise.modell2 <- emmeans(forest_modell2, list(pairwise ~ forest_class*fire_2015))
paiwise.modell2

# Test residual overdispersion  
residuals <- simulateResiduals(fittedModel = forest_modell2, n = 1000)
plot(residuals) # normal distribution



################################################################################
## 3. Arthropod predator in pre-EN forest disturbance


## What is the response variable?
# arthropd predation 
## What is the predictor variable?
# pre-EN forest disturbance (UF, LF, LBF, SF).

# forest class effects on arthropod predation (1) and others groups (0)

# Model: caterpillars predated by arthropods ~ forest_class + ( 1 | trans_cod)


# filter to use just unburned forest and the caterpillar didn't lost and in doubt
forest_predation_art <- predation_data1 %>% 
  filter(fire_2015 == 0, lost == 0, excluded == 0, predation != 0)
str(forest_predation_art)

# Put undisturbed forest (UF) as a reference to compare among other forests
forest_predation_art$forest_class = relevel(factor(forest_predation_art$forest_class), 
                                            ref = "UF")

# Modeling glmer
forest_model_arthropod <- glmer(arthropod ~ forest_class + 
                                 ( 1 | trans_cod), 
                               data = forest_predation_art, 
                               family = binomial)
summary(forest_model_arthropod)

#likelihood ratio test
drop1(forest_model_arthropod, test = "Chisq") 
# There is no differences on arthropods predation among pre-EN forest disturbance 

# Test residual overdispersion  
residuals <- simulateResiduals(fittedModel = forest_model_arthropod, n = 1000)
plot(residuals) # normal distribution



################################################################################
## 4. Non-arthropods (vertebrates) predation in pre-EN forest disturbance

## What is the response variable?
# non-arthropods predation 
## What is the predictor variable?
# pre-EN forest disturbance (UF, LF, LBF, SF).

# Model: caterpillars predated by birds, mammals and reptiles ~ forest_class + 
# ( 1 | trans_cod)


# unpredated caterpillas were removided
forest_predation_all <- predation_data1 %>% 
  filter(fire_2015 == 0, predation != 0, lost == 0, excluded == 0)
str(forest_predation_all)

# Put undisturbed forest (UF) as a reference to compare among other forests
forest_predation_all$forest_class = relevel(factor(forest_predation_all$forest_class), 
                                            ref = "UF")

# Modeling glmer
multitaxa.forest.all <- glmer(arthropod_multiple ~ forest_class + 
                                 ( 1 | trans_cod), 
                               data = forest_predation_forest, 
                               family = binomial)
summary(multitaxa.forest.all)

#likelihood ratio test
drop1(multitaxa.forest.all, test = "Chisq") # no differences
# There is no differences on non-arthropods predation among pre-EN forest disturbance 

# Test residual overdispersion  
residuals <- simulateResiduals(fittedModel = multitaxa.forest.all, n = 1000)
plot(residuals) # normal distribution



################################################################################
## 5. Arthropod predation  in  EN-fire-affected forests

## What is the response variable?
# arthropods predation 
## What is the predictor variable?
# pre-EN forest disturbance (UF, LF, LBF, SF and
# EN-fire-affected plots (0 - whitouth fire, 1 - with fire).

# Model: caterpillars predated by arthropod ~ forest_class + ( 1 | trans_cod)


# filter to use just unburned forest and the caterpillar didn't lost and in 
# doubt. Remove secondary forest
forest_predation_forest.fire <- predation_data1 %>% 
  filter(lost == 0, excluded == 0, predation != 0)
str(forest_predation_forest.fire)

# Put undisturbed forest (UF) as a reference to compare among other forests
forest_predation_forest.fire$forest_class = relevel(factor(forest_predation_forest.fire$forest_class), 
                                                    ref = "UF")

## complete model
# caterpillars predated by arthropod ~ forest_class + fire_2015 + forest_class*fire_2015
forest_model_arthropod.fire = glmer(arthropod ~ forest_class + fire_2015 + 
                                      forest_class*fire_2015 +  ( 1 | trans_cod), 
                                    data = forest_predation_forest.fire, 
                                    family = binomial)
summary(forest_model_arthropod.fire)

#likelihood ratio test
drop1(forest_model_arthropod.fire, test = "Chisq") # Non significant 
# Must simplify the model.


## Removed pre-EN forest disturbance
# caterpillars predated by arthropod ~ fire_2015 + forest_class*fire_2015
forest_model_arthropod.fire2 <- glmer(arthropod ~ fire_2015 + 
                                       forest_class*fire_2015 +  ( 1 | trans_cod), 
                                     data = forest_predation_forest.fire, 
                                     family = binomial)
summary(forest_model_arthropod.fire2)

# likelihood ratio test
drop1(forest_model_arthropod.fire2, test = "Chisq") # Non significant


## Removed pre-EN forest disturbance and EN-fire-affected forest
# caterpillars predated by arthropod ~ forest_class*fire_2015
forest_model_arthropod.fire3 <- glmer(arthropod ~ forest_class*fire_2015 +  
                                       ( 1 | trans_cod), 
                                     data = forest_predation_forest.fire, 
                                     family = binomial)
summary(forest_model_arthropod.fire3)

#likelihood ratio test
drop1(forest_model_arthropod.fire3, test = "Chisq") # Non significant


## Removed pre-EN forest disturbance and forest*fire interaction
# caterpillars predated by arthropod ~ forest_class*fire_2015
forest_model_arthropod.fire4 <- glmer(arthropod ~ fire_2015 + 
                                       ( 1 | trans_cod), 
                                     data = forest_predation_forest.fire, 
                                     family = binomial)
summary(forest_model_arthropod.fire4)

#likelihood ratio test
drop1(forest_model_arthropod.fire4, test = "Chisq") # Non significant
# There is no differences on arthropods predation among EN-fire-affected forests


# forest_model_arthropod.fire4 - lower value
# Test residual overdispersion  
residuals <- simulateResiduals(fittedModel = forest_model_arthropod.fire4, n = 1000)
plot(residuals)



################################################################################ 
# 6. Non-arthropods predation in EN-fire-affected forests

## What is the response variable?
# non-arthropods predation 
## What is the predictor variable?
# pre-EN forest disturbance (UF, LF, LBF, SF and
# EN-fire-affected plots (0 - whitouth fire, 1 - with fire).

# Model: caterpillars predated by more than one taxon - birds, mammals and
# reptiles ~ forest_class + fire_2015 + forest_class*fire_2015 + ( 1 | trans_cod)


# unpredated caterpillas were removided
forest_predation.art_multitaxa.all <- predation_data1 %>% 
  filter(predation != 0, lost == 0, excluded == 0)
str(forest_predation.art_multitaxa.all)

# Put undisturbed forest (UF) as a reference to compare among other forests
forest_predation.art_multitaxa.all$forest_class = relevel(factor(forest_predation.art_multitaxa.all$forest_class), 
                                                    ref = "UF")

## complete model
# caterpillars predated by non-arthropod ~ forest_class + fire_2015 + forest_class*fire_2015
forest_modell.all <- glmer(arthropod_multiple ~ forest_class + fire_2015 +  
                            forest_class*fire_2015 + ( 1 | trans_cod),
                          data= forest_predation.art_multitaxa.all,
                          family = binomial)
summary(forest_modell.all)  # fire effects

#likelihood ratio test
drop1(forest_modell.all, test = "Chisq") # Non significant
# Must simplify the model.


## Removed pre-EN forest disturbance
# caterpillars predated by non-arthropod ~ fire_2015 + forest_class*fire_2015
forest_modell.all1 <- glmer(arthropod_multiple ~  fire_2015 +  
                            forest_class*fire_2015 + ( 1 | trans_cod),
                          data= forest_predation.art_multitaxa.all,
                          family = binomial)
summary(forest_modell.all1) # fire effects

# likelihood ratio test
drop1(forest_modell.all1, test = "Chisq") # non significance


# Removed pre-EN forest and EN-fire-affected forest
# caterpillars predated by non-arthropod ~ forest_class*fire_2015
forest_modell.all2 <- glmer(arthropod_multiple ~  forest_class*fire_2015 + 
                             ( 1 | trans_cod),
                           data= forest_predation.art_multitaxa.all,
                           family = binomial)
summary(forest_modell.all2) # fire effects

# likelihood ratio test
drop1(forest_modell.all2, test = "Chisq") # non significance


## Removed pre-EN forest disturbance and forest*fire interaction
# caterpillars predated by non-arthropod ~ fire_2015
forest_modell.all3 = glmer(arthropod_multiple ~  fire_2015 +  
                              ( 1 | trans_cod),
                           data= forest_predation.art_multitaxa.all,
                           family = binomial)
summary(forest_modell.all3)  # fire effects

#likelihood ratio test
drop1(forest_modell.all3, test = "Chisq") # significant results=
# There is differences on non-arthropods predation among EN-fire-affected forests

#paraiwise post-hoc test with emmeans function
paiwise.modell.all <- emmeans(forest_modell.all, list(pairwise ~ forest_class*fire_2015))
paiwise.modell.all

# Test residual overdispersion  
residuals <- simulateResiduals(fittedModel = forest_modell.all3, n = 1000)
plot(residuals)



#THE END