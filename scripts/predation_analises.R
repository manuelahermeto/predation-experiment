
#dizer o diretório
setwd("C:/Users/ERIKA/Downloads/Manuela/predation-experiment")

#pacotes usados
library("lme4")    # glmer and drop1
library("dplyr")   # 
library("emmeans") # emmeans
library("DHARMa")  # test residual
library("ggplot2") #plot visuals
library("ggtext") #change text in ggplot
library("readr") #read csv

#load RData----
load("analyses_predation_paraty.RData")

# read the data (que está em .csv)
predation_data1 <- read.csv("paraty_experimento_predacao_2023.csv",header = TRUE)
# Data come from the csv called 'paraty_experimento_predacao_2023.csv'
str(predation_data1)

# 1. Predation incidence in different treatments----

## What is the response variable?
# total predation 
## What is the predictor variable?
# type of treatment (F1, F2, SAF1, SAF2, R)

# Model: predation ~ treatment + ( 1 | individual_code)----

#Agrupando colunas "treatment" e "treatment.number"
#Não utilizado
#predation_data1 <- predation_data1 %>%
#      *mutate(id_treatment = paste(treatment, treatment.number, sep = "_"))
#write.csv(predation_data1, "C:/Users/ERIKA/Downloads/Manuela/predation-experiment/predation_data1.csv", row.names = FALSE)

#Análises considerando 5 tratamentos (predation_data1)
#Não utilizado
#m1 <- glmer(predation ~ id_treatment + (1 | individual_code),
#           data = predation_data1,
#           family = binomial)
#summary(m1)

#Testando as diferenças entre as réplicas

#paiwise.m1 <- emmeans(m1, list(pairwise ~ id_treatment))
#paiwise.m1

##Análises considerando 3 tratamentos (m1)----
###Exclusão de lagartas "lost = 1"----
predation_data1 <- predation_data1 %>% 
  filter(lost != 1)

##Análises considerando 3 tratamentos (m1)----
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

plot(m1)

##Um gráfico para cada tratamento----
predation_data1 %>%
  ggplot(aes(x = predation)) +
  geom_histogram(bins = 2, fill = "steelblue", color = "black") +
  facet_wrap(~ treatment) +
  theme_minimal() +
  labs(x = "Predação", 
       y = "Contagem",
       title = "Histograma de Predação Separado por Tratamento")

#N de indivíduos por tratamento
predation_data1 %>%
  group_by(treatment) %>%
  summarise(numero_individuos = n_distinct(individual_code))

### Calcular a porcentagem de predação por tratamento----
porcentagem_predacao <- predation_data1 %>%
  group_by(treatment) %>%
  summarise(
    total = n(),                               # Total de indivíduos por tratamento
    predados = sum(predation == 1),             # Quantos foram predados (predation == 1)
    porcentagem = (predados / total) * 100      # Calcula a porcentagem
  )

#2.Predation by different guilds in different treatments----
#Model 2: guild ~ treatment + ( 1 | individual_code)----

##Juntar grupos em "guild"
guilds_caterpillar <- predation_data1 %>%
 pivot_longer(
    cols = c(mammal, reptile, bird, arthropod),
    names_to = "guild",
    values_to = "presence"
  )

##Testando outlier
#install.packages("car")
#library(car)
#outlierTest(m1) # testa observações com resíduos studentizados extremos
#resultado: a observação 937 teve o maior resíduo studentizado (~2.96), com valor-p sem correção de 0.003

##Investigar a observação 937
#dados_long[937, ]
#Outlier tem uma explicação biológica: única predação por mamífero na restauração

##Frequência média só de bird----
guilds_caterpillar %>%
  filter(guild == "bird") %>%
  ggplot(aes(x = treatment, y = presence)) +
  stat_summary(fun = mean, geom = "point", size = 3, color = "darkred") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, color = "darkred") +
  labs(
    title = "Frequência média de predação por aves por tratamento",
    y = "Proporção média",
    x = "Tratamento"
  ) +
  theme_minimal()

##contagem absoluta de predação por bird
guilds_caterpillar %>%
  filter(guild == "bird") %>%
  group_by(treatment) %>%
  summarise(
    total_obs = n(),
    total_predacao = sum(presence),
    proporcao = total_predacao / total_obs
  )

#Não houve predação por répteis - retirar da análise, gerar novo modelo (m2)----

#Excluir répteis
predation_data1 <- guilds_caterpillar %>%
  filter(guild != "reptile")

#Excluir lagarta id = 11----
predation_data1 <- predation_data1[predation_data1$n. != 11, ]

#Gerar novo modelo (m2)----

m2 <- glmer(presence ~ guild * treatment + (1|individual_code), 
            data = predation_data1, 
            family = binomial)
summary(m2)
plot(m2)

##Criar as quatro colunas de sampling effort na tabela predation_data1----

#Coluna "sampling_effort_euterpe" de nº de açaí para 3 tratamentos
#predation_data1 <- predation_data1 %>%
#  mutate(sampling_effort_euterpe = case_when(
#    treatment == "forest" ~ 11,
#    treatment == "agroforestry" ~ 10,
#    treatment == "restoration" ~ 5,
#    TRUE ~ NA_real_
#  ))

#Coluna "sampling_effort_caterpillar" de nº de lagartas para 3 tratamentos
#predation_data1 <- predation_data1 %>%
#  mutate(sampling_effort_caterpillar = case_when(
#    treatment == "forest" ~ 110,
#    treatment == "agroforestry" ~ 100,
#    treatment == "restoration" ~ 50,
#    TRUE ~ NA_real_
#  ))

#Coluna "total_se_euterpe" de nº de açaí para 5 tratamentos
#predation_data1 <- predation_data1 %>%
#  mutate(total_se_euterpe = case_when(
#    treatment_number == "forest1" ~ 5,
#    treatment_number == "forest2" ~ 6,
#    treatment_number %in% c("agroforestry1", "agroforestry2", "restoration1") ~ 5,
#    TRUE ~ NA_real_
#  ))

#Coluna "total_se_caterpillar" de nº de lagartas para 5 tratamentos
#predation_data1 <- predation_data1 %>%
#  mutate(total_se_caterpillar = case_when(
#    treatment_number == "forest1" ~ 50,
#    treatment_number == "forest2" ~ 60,
#    treatment_number %in% c("agroforestry1", "agroforestry2", "restoration1") ~ 50,
#    TRUE ~ NA_real_
#  ))

#Adicionar "sampling_effort_euterpe" no modelo (NÃO UTILIZADO)
#m2 <- glmer(presence ~ guild * treatment + sampling_effort_euterpe + (1 | individual_code),
#            data = predation_data1,
#            family = binomial)

##Números absolutos de cada guilda -----
ggplot(predation_data1, aes(x = treatment, y = presence, color = guild)) +
  geom_point(position = position_jitter(width = 0.2, height = 0.05), alpha = 0.6, size = 1) +
  facet_wrap(~ guild) +
  theme_minimal()

##Proporção média de cada guilda---- #salvar gráfico como "fig1_m2"----

#adicionando imagens no gráfico fig1_m2----

#Colocar o endereço das imagens
guild_images1 <- c(
  "bird" = "<img src='images/pteroglossus.png' width='40'/>",
  "mammal" = "<img src='images/mammals.png' width='40'/>",
  "arthropod" = "<img src='images/formicophorms.png' width='40'/>"
)

# Adiciona coluna com HTML da imagem pro facet labels
predation_data1 <- predation_data1 %>%
  mutate(guild_label = guild_images1[guild])

# Criar o gráfico com as imagens como título (facet label)
fig1_m2 <- ggplot(predation_data1, aes(x = treatment, y = presence, color = guild)) +
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

##Análise post-hoc para entender quais combinações de fatores são significativamente diferentes entre si----
em2 <- emmeans(m2, ~ guild * treatment, type = "response")
summary(em2)

pairs(em2, adjust = "tukey")

#linhas 3, 16, 22, 23 = p < 0.05
#3 = arthropod agroforestry / arthropod forest
#16 = mammal agroforestry / arthropod forest
#22 = arthropod forest / bird forest
#23 = arthropod forest / mammal forest

##Salvar o pairs em csv (tukey_testm2)----
## Execute a comparação de pares
tukey_testm2 <- pairs(em2, adjust = "tukey")

## Converta para data frame
tukeym2_df <- as.data.frame(tukey_testm2)

## Salve como CSV
write.csv(tukeym2_df, file = "tukeytest_m2.csv", row.names = FALSE)

#Ler csv
tukey_testm2 <- read_csv("tukeytest_m2.csv")

##Visualizar os resultados e salvar o gráfico como "fig2_m2"----
fig2_m2=plot(em2, comparisons = FALSE) #FALSE = sem setas / TRUE = com setas
fig2_m2

#aumenta a fonte
fig2_m2 + 
  theme(
   axis.text = element_text(size = 13),
  )

#Salvar RData----
save.image("analyses_predation_paraty.RData")

#3.Traits: Number of fruiting Euterpe edulis in each buffer----

#Filtrar só Euterpe_edulis no frutos_buffer e selecionar colunas relevantes
euterpe_df <- frutos_buffer %>%
  filter(species == "Euterpe_edulis") %>%
  select(individual_code, n_individuals)

#Fazer o left_join para adicionar a coluna no predation_data1
predation_data1 <- predation_data1 %>%
  left_join(euterpe_df, by = "individual_code") %>%
  rename(fruiting_euterpe = n_individuals)

#Modelo 3 com fruiting_euterpe como covariável----
m3 <- glmer(presence ~ guild * treatment + fruiting_euterpe + (1|individual_code), 
            data = predation_data1, 
            family = binomial)
summary(m3)
plot(m3)

plot(boxplot(fruiting_euterpe ~ treatment, data = predation_data1))


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
#Model 2: guild ~ treatment + ( 1 | individual_code)----

#Excluir répteis----
#Agrupa as guildas e cria uma coluna "presence"
guilds_fruits <- frugivory_data1 %>%
 pivot_longer(
    cols = c(mammal, reptile, bird, arthropod),
    names_to = "guild",
    values_to = "presence"
  )

#Exclui os répteis
frugivory_data1 <- guilds_fruits %>%
  filter(guild != "reptile")

#Gerar novo modelo (f2)----

f2 <- glmer(presence ~ guild * treatment + (1|individual_code), 
            data = frugivory_data1, 
            family = binomial)
summary(f2)
plot(f2)

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
fig1_f2 <- ggplot(frugivory_data1, aes(x = treatment, y = presence, color = guild)) +
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
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_markdown(size = 16),
        axis.text.x = element_text(size = 16)
  )

fig1_f2

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
fig2_f2=plot(ef2, comparisons = FALSE) #FALSE = sem setas / TRUE = com setas

#aumenta a fonte
fig2_f2 + 
  theme(
    axis.text = element_text(size = 13),
  )
fig2_f2
