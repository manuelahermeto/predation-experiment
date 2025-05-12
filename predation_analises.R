#test run

#dizer o diretório
setwd("C:/Users/ERIKA/Downloads/Manuela/predation-experiment")

#pacotes usados
library("lme4")    # glmer and drop1
library("dplyr")   # put forest in order 
library("emmeans") # emmeans
library("DHARMa")  # test residual

#load RData----
load("analyses_predation_paraty.RData")

# read the data (que está em .csv)
predation_data1 <- read.csv("paraty_experimento_predacao_2023 csv.csv",header = TRUE)
# Data come from the csv called 'paraty_experimento_predacao_2023 csv.csv'
str(predation_data1)

# 1. Predation incidence in different treatments----

## What is the response variable?
# total predation 
## What is the predictor variable?
# type of treatment (F1, F2, SAF1, SAF2, R)

# Model: predation ~ treatment + ( 1 | individual_code)----

#Agrupando colunas "treatment" e "treatment.number"
#Não utilizado
*predation_data2 <- predation_data1 %>%
      *mutate(id_treatment = paste(treatment, treatment.number, sep = "_"))
*write.csv(predation_data2, "C:/Users/ERIKA/Downloads/Manuela/predation-experiment/predation_data2.csv", row.names = FALSE)

#Análises considerando 5 tratamentos (predation_data2)
#Não utilizado
*m1 <- glmer(predation ~ id_treatment + (1 | individual_code),
            data = predation_data2,
            family = binomial)
*summary(m1)

#Testando as diferenças entre as réplicas

*paiwise.m1 <- emmeans(m1, list(pairwise ~ id_treatment))
*paiwise.m1

##Análises considerando 3 tratamentos (m1)----
###Exclusão de lagartas "lost = 1"----

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

library(dplyr)
library(ggplot2)

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

## Gráfico de barras com porcentagem por tratamento----
ggplot(porcentagem_predacao, aes(x = treatment, y = porcentagem, fill = treatment)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0(round(porcentagem, 1), "%")), 
            vjust = -0.5, size = 4) +
  theme_minimal() +
  labs(x = "Tratamento", y = "Predação (%)", title = "Porcentagem de Predação por Tratamento") +
  theme(legend.position = "none")


#2.Predation by different guilds in different treatments----
#Model 2: guild ~ treatment + ( 1 | individual_code)----

##Juntar grupos em "guild"----
#Não utilizado pois inclui reptiles
*all_guilds <- predation_data1 %>%
  pivot_longer(
    cols = c(mammal, reptile, bird, arthropod),
    names_to = "guild",
    values_to = "presence"
  )

##Análise com modelo binomial
#NÃO UTILIZADO
*m <- glm(presence ~ guild * treatment,
              data = predation_data2,
              family = binomial)

summary(m)

##Frequência de cada guilda por tratamento
*predation_data2 %>%
  count(treatment, guild) %>%
  ggplot(aes(x = treatment, y = n, fill = guild)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = "Incidência de predação por guilda", x = "Tratamento") +
  theme_minimal()

##Testando outlier
*install.packages("car")
*library(car)
*outlierTest(m1) # testa observações com resíduos studentizados extremos
#resultado: a observação 937 teve o maior resíduo studentizado (~2.96), com valor-p sem correção de 0.003

##Investigar a observação 937
*dados_long[937, ]
#Outlier tem uma explicação biológica: única predação por mamífero na restauração


##gráfico geom_jitter por guilda
*ggplot(dados_long, aes(x = treatment, y = presence, color = guild)) +
  geom_jitter(width = 0.2, height = 0.05, alpha = 0.6, size = 2) +
  labs(
    title = "Presença de grupos de predadores por ambiente",
    y = "Presença (1 = sim)",
    x = "Ambiente"
  ) +
  facet_wrap(~ guild) +
  theme_minimal()

##geom_point() e position_jitter
*ggplot(dados_long, aes(x = treatment, y = presence, color = guild)) +
  geom_point(position = position_jitter(width = 0.2, height = 0.05), alpha = 0.6, size = 2) +
  facet_wrap(~ guild) +
  theme_minimal()

##proporções esperadas por tratamento: stat_summary()
*ggplot(dados_long, aes(x = treatment, y = presence, color = guild)) +
  stat_summary(fun = mean, geom = "point", size = 3, position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~ guild) +
  labs(
    title = "Proporção média de predação por guilda e tratamento",
    y = "Frequência média de presença",
    x = "Tratamento"
  ) +
  theme_minimal()

##Frequência média só de bird----
all_guilds %>%
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
all_guilds %>%
  filter(guild == "bird") %>%
  group_by(treatment) %>%
  summarise(
    total_obs = n(),
    total_predacao = sum(presence),
    proporcao = total_predacao / total_obs
  )

##Contagem absoluta de predação por reptile----
all_guilds %>%
  filter(guild == "reptile") %>%
  group_by(treatment) %>%
  summarise(
    total_obs = n(),
    total_predacao = sum(presence),
    proporcao = total_predacao / total_obs
  )

#Não houve predação por répteis - retirar da análise, gerar novo modelo (m2)----

#Excluir répteis
predation_data2 <- all_guilds %>%
  filter(guild != "reptile")

#Gerar novo modelo (m2)----
m2 <- glm(presence ~ guild * treatment,
          data = predation_data2,
          family = binomial)

summary(m2)
plot(m2)

##Números absolutos de cada guilda - geom_point() e position_jitter----
ggplot(predation_data2, aes(x = treatment, y = presence, color = guild)) +
  geom_point(position = position_jitter(width = 0.2, height = 0.05), alpha = 0.6, size = 1) +
  facet_wrap(~ guild) +
  theme_minimal()

##Proporção média de cada guilda---- #salvar gráfico como "fig1_m2"----
fig1_m2=ggplot(predation_data2, aes(x = treatment, y = presence, color = guild)) +
  stat_summary(fun = mean, geom = "point", size = 3, position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~ guild) +
  labs(
    title = "",
    y = "Predation frequency (mean ± SD)",
    x = ""
  ) +
  theme_minimal()

fig1_m2

#teste - adicionando imagens no gráfico fig1_m2----
library(ggplot2)
library(ggtext)
library(dplyr)

#Colocar o endereço das imagens
guild_images <- c(
  "bird" = "<img src='images/bird.png' width='40'/>",
  "mammal" = "<img src='images/mammal.png' width='40'/>",
  "arthropod" = "<img src='images/arthropod.png' width='40'/>"
)

# Adiciona coluna com HTML da imagem pro facet labels
predation_data2 <- predation_data2 %>%
  mutate(guild_label = guild_images[guild])

# Criar o gráfico com as imagens como título (facet label)
fig1_m2 <- ggplot(predation_data2, aes(x = treatment, y = presence, color = guild)) +
  stat_summary(fun = mean, geom = "point", size = 3, position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~ guild_label) +  # Use the new column with images as facet labels
  labs(
    y = "Predation frequency (mean ± SD)",
    x = "",
    color = "Guild"  # Keeps guild name in the legend
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_markdown(size = 16)  # This enables HTML rendering in the facet labels
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

##Salvar o pairs em csv----
## Execute a comparação de pares
tukey_testm2 <- pairs(em2, adjust = "tukey")

## Converta para data frame
tukeym2_df <- as.data.frame(tukey_testm2)

## Salve como CSV
write.csv(tukeym2_df, file = "tukeytest_m2.csv", row.names = FALSE)

#Ler csv
library(readr)
tukey_testm2 <- read_csv("tukeytest_m2.csv")

##Visualizar os resultados e salvar o gráfico como "fig2_m2"----
fig2_m2=plot(em2, comparisons = FALSE) #FALSE = sem setas / TRUE = com setas
fig2_m2

library(ggplot2)
#aumenta a fonte
fig2_m2 + 
  theme(
   axis.text = element_text(size = 13),
  )

#Salvar RData----
save.image("analyses_predation_paraty.RData")

*#3.Predation by multiple guilds in different treatments----
#Model 3: multiple ~ treatment + ( 1 | individual_code)----
#Não utilizado

## GLM com família quasipoisson (melhor se há overdispersion)
*m3 <- glm(multiple ~ treatment, data = predation_data4, family = quasipoisson)

*summary(m3)
*anova(m3, test = "F")  # ANOVA para avaliar efeito de treatment

# Obter emmeans
*em3 <- emmeans(m3, ~ treatment)

# Comparações de pares com ajuste Tukey
*pairs_m3 <- pairs(em3, adjust = "tukey")
*summary(pairs_m3)

*plot(m3)

#m3 não será usado por apenas 6 lagartas tiveram multiple = 2----
