# Diretório----
setwd("C:/Users/ERIKA/Downloads/Manuela/predation-experiment")

# Pacotes usados
library("lme4")    # glmer and drop1
library("dplyr")   # 
library("emmeans") # emmeans
library("DHARMa")  # test residual
library("ggplot2") #plot visuals
library("ggtext") #change text in ggplot
library("readr") #read csv

# Ler os dados (que está em .csv)
predation_data1 <- read.csv("paraty_experimento_predacao_2023.csv",header = TRUE)

# Data come from the csv called 'paraty_experimento_predacao_2023.csv'
str(predation_data1)

# Q1. Predation incidence in different treatments----

###Exclusão de lagartas "lost = 1"----
predation_data1 <- predation_data1 %>% 
  filter(lost != 1)

## Model m1: predation ~ treatment + ( 1 | individual_code)----
m1 <- glmer(predation ~ treatment + (1 | individual_code),
            data = predation_data1,
            family = binomial)

summary(m1)

##Testando as diferenças entre as réplicas----

pairwise.m1 <- emmeans(m1, list(pairwise ~ treatment))
pairwise.m1

##Teste de Tukey----

tukey_testm1 <- emmeans(m1, pairwise ~ treatment, adjust = "tukey")
tukey_testm1

### Porcentagem de predação por tratamento----
porcentagem_predacao <- predation_data1 %>%
  group_by(treatment) %>%
  summarise(
    total = n(),                               # Total de indivíduos por tratamento
    predados = sum(predation == 1),             # Quantos foram predados (predation == 1)
    porcentagem = (predados / total) * 100      # Calcula a porcentagem
  )

# Q2. Predation by different guilds in different treatments----

##Juntar guildas em "guilds_caterpillar" e criar coluna "presence"
guilds_caterpillar <- predation_data1 %>%
  pivot_longer(
    cols = c(mammal, reptile, bird, arthropod),
    names_to = "guild",
    values_to = "presence"
  )

#Excluir répteis
predation_data1 <- guilds_caterpillar %>%
  filter(guild != "reptile")

#Excluir lagarta id = 11----
predation_data1 <- predation_data1[predation_data1$n. != 11, ]

## Model m2: guild ~ treatment + ( 1 | individual_code)----

m2 <- glmer(presence ~ guild * treatment + (1|individual_code), 
            data = predation_data1, 
            family = binomial)
summary(m2)

##Proporção média de cada guilda---- #salvar gráfico como "fig1_m2"----

#Adicionar imagens no gráfico fig1_m2----

#Colocar o endereço das imagens e agrupar em "guild_images1"
guild_images1 <- c(
  "bird" = "<img src='images/pteroglossus.png' width='40'/>",
  "mammal" = "<img src='images/mammals.png' width='40'/>",
  "arthropod" = "<img src='images/formicophorms.png' width='40'/>"
)

# Adiciona coluna com HTML da imagem pro facet labels
predation_data1 <- predation_data1 %>%
  mutate(guild_label = guild_images1[guild])

# Cria o gráfico com as imagens como título (facet label)
pdf("fig1_m2.pdf", width = 10, height = 6, pointsize = 12)
png("fig1_m2.png", width = 10, height = 6, units = 'in', res=300)

ggplot(predation_data1, aes(x = treatment, y = presence, color = guild)) +
  stat_summary(fun = mean, geom = "point", size = 3, position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~ guild_label) +
  labs(
    y = "Predation frequency (mean ± SD)",
    x = "",
    color = "Guild"
  ) +
  scale_x_discrete(labels = c(
    "agroforestry" = "A",
    "forest" = "F",
    "restauration" = "R"
  )) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_markdown(size = 16),
    axis.text.x = element_text(size = 16)
  )

fig1_m2

dev.off()

##Análise post-hoc para entender quais combinações de fatores são significativamente diferentes entre si----
em2 <- emmeans(m2, ~ guild * treatment, type = "response")
summary(em2)

pairs(em2, adjust = "tukey")

##Salvar o pairs em csv (tukey_testm2)----
## Execute a comparação de pares
tukey_testm2 <- pairs(em2, adjust = "tukey")

## Converter para data frame
tukeym2_df <- as.data.frame(tukey_testm2)

## Salve como CSV
write.csv(tukeym2_df, file = "tukeytest_m2.csv", row.names = FALSE)

#Ler csv
tukey_testm2 <- read_csv("tukeytest_m2.csv")

##Visualizar os resultados e salvar o gráfico como "fig2_m2"----
pdf("fig2_m2.pdf", width = 10, height = 6, pointsize = 12)
png("fig2_m2.png", width = 10, height = 6, units = 'in', res=300)

plot(em2, comparisons = FALSE)   #FALSE = sem setas / TRUE = com setas

fig2_m2
dev.off()

#############################################################################
#Repetir as análises para dados de frugivoria---------------

# Ler os dados (que está em .csv)
frugivory_data1 <- read.csv("paraty_experimento_frugivoria_2023.csv",header = TRUE)

# Data come from the csv called 'paraty_experimento_frugivoria_2023.csv'
str(frugivory_data1)

# Q3. Frugivory incidence in different treatments----

###Exclusão de frutos "lost = 1"----

frugivory_data1 <- frugivory_data1 %>% 
  filter(lost != 1)

# Model f1: frugivory ~ treatment + ( 1 | individual_code)----

f1 <- glmer(frugivory ~ treatment + (1 | individual_code),
            data = frugivory_data1,
            family = binomial)

summary(f1)

##Testando as diferenças entre as réplicas----

pairwise.f1 <- emmeans(f1, list(pairwise ~ treatment))
pairwise.f1

##Teste de Tukey----

tukey_testf1 <- emmeans(f1, pairwise ~ treatment, adjust = "tukey")
tukey_testf1

plot(f1)

#N de indivíduos por tratamento
frugivory_data1 %>%                                           #10 SAF
  group_by(treatment) %>%                                     #11 Floresta
  summarise(numero_individuos = n_distinct(individual_code))  #5 Restauração

### Calcular a porcentagem de frugivoria por tratamento----
porcentagem_frugivoria <- frugivory_data1 %>%
  group_by(treatment) %>%
  summarise(
    total = n(),                               # Total de indivíduos por tratamento
    consumidos = sum(frugivory == 1),             # Quantos foram consumidos (frugivory == 1)
    porcentagem = (consumidos / total) * 100      # Calcula a porcentagem
  )

# Q4.Frugivory by different guilds in different treatments----

## Agrupa as guildas e cria uma coluna "presence"----
guilds_fruits <- frugivory_data1 %>%
  pivot_longer(
    cols = c(mammal, reptile, bird, arthropod),
    names_to = "guild",
    values_to = "presence"
  )

## Exclui os répteis----
frugivory_data1 <- guilds_fruits %>%
  filter(guild != "reptile")

#Model f2: guild ~ treatment + ( 1 | individual_code)----

f2 <- glmer(presence ~ guild * treatment + (1|individual_code), 
            data = frugivory_data1, 
            family = binomial)

summary(f2)

##Proporção média de cada guilda---- #salvar gráfico como "fig1_f2"----

#Colocar o endereço das imagens
guild_images <- c(
  "bird" = "<img src='images/pteroglossus.png' width='40'/>",
  "mammal" = "<img src='images/mammals.png' width='40'/>",
  "arthropod" = "<img src='images/formicophorms.png' width='40'/>"
)

# Adiciona coluna com HTML da imagem pro facet labels
frugivory_data1 <- frugivory_data1 %>%
  mutate(guild_label = guild_images[guild])

# Criar o gráfico com as imagens como título (facet label)
pdf("fig1_f2.pdf", width = 10, height = 6, pointsize = 12)
png("fig1_f2.png", width = 10, height = 6, units = 'in', res=300)

ggplot(frugivory_data1, aes(x = treatment, y = presence, color = guild)) +
  stat_summary(fun = mean, geom = "point", size = 3, position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~ guild_label) +
  labs(
    y = "Frugivory frequency (mean ± SD)",
    x = "",
    color = "Guild"
  ) +
  scale_x_discrete(labels = c(
    "agroforestry" = "A",
    "forest" = "F",
    "restauration" = "R"
  )) +
  theme_minimal(base_size = 16) +
  theme(
    strip.text = element_markdown(size = 19),
    axis.text.x = element_text(size = 19)
  )

fig1_f2

dev.off()

##Análise post-hoc para entender quais combinações de fatores são significativamente diferentes entre si----
ef2 <- emmeans(f2, ~ guild * treatment, type = "response")
summary(ef2)

pairs(ef2, adjust = "tukey")

##Salvar o pairs em csv (tukey_testf2)----
## Execute a comparação de pares
tukey_testf2 <- pairs(ef2, adjust = "tukey")

## Converta para data frame
tukeyf2_df <- as.data.frame(tukey_testf2)

## Salve como CSV
write.csv(tukeyf2_df, file = "tukeytest_f2.csv", row.names = FALSE)

#Ler csv
tukey_testf2 <- read_csv("tukeytest_f2.csv")

##Visualizar os resultados e salvar o gráfico como "fig2_f2"----
#fig2_f2=plot(ef2, comparisons = FALSE) #FALSE = sem setas / TRUE = com setas
pdf("fig2_f2.pdf", width = 10, height = 6, pointsize = 12)
png("fig2_f2.png", width = 10, height = 6, units = 'in', res=300)

ggplot(ef2testdf, aes(y = treatment, x = prob, color = treatment, group = treatment)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbarh(aes(xmin = asymp.LCL, xmax = asymp.UCL),
                 position = position_dodge(width = 0.3),
                 height = 0.2,
                 size = 1.2,
                 alpha = 0.5) +
  scale_x_continuous(limits = c(0, 0.8)) +
  labs(x = NULL, y = NULL) +
  facet_grid(guild ~ ., scales = "fixed", switch = "x") +
  theme_gray() +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

fig2_f2

dev.off()
