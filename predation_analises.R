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
predation_data2 <- predation_data1 %>%
      mutate(id_treatment = paste(treatment, treatment.number, sep = "_"))
write.csv(predation_data2, "C:/Users/ERIKA/Downloads/Manuela/predation-experiment/predation_data2.csv", row.names = FALSE)

#Análises considerando 5 tratamentos (predation_data2)
#Não utilizado
*m1 <- glmer(predation ~ id_treatment + (1 | individual_code),
            data = predation_data2,
            family = binomial)
*summary(m1)

#Testando as diferenças entre as réplicas

*paiwise.m1 <- emmeans(m1, list(pairwise ~ id_treatment))
*paiwise.m1

##Análises considerando 3 tratamentos (predation_data1)----

m2 <- glmer(predation ~ treatment + (1 | individual_code),
            data = predation_data1,
            family = binomial)
summary(m2)

#Testando as diferenças entre as réplicas

pairwise.m2 <- emmeans(m2, list(pairwise ~ treatment))
pairwise.m2

#Teste de Tukey

tukey_test <- emmeans(m2, pairwise ~ treatment, adjust = "tukey")
tukey_test

plot(m2)

library(ggplot2)

ggplot(predation_data1, aes(x = treatment, y = predation)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Tratamento", y = "Predação", title = "Boxplot de Predação por Tratamento")

hist(predation_data1$predation)

library(dplyr)
library(ggplot2)

predation_data1 %>%
  ggplot(aes(x = predation, fill = treatment)) +
  geom_histogram(position = "dodge", bins = 2) +
  theme_minimal() +
  labs(x = "Predação (0 = não, 1 = sim)", 
       y = "Contagem", 
       title = "Histograma de Predação por Tratamento")

#Um gráfico para cada tratamento
predation_data1 %>%
  ggplot(aes(x = predation)) +
  geom_histogram(bins = 2, fill = "steelblue", color = "black") +
  facet_wrap(~ treatment) +
  theme_minimal() +
  labs(x = "Predação (0 = não, 1 = sim)", 
       y = "Contagem",
       title = "Histograma de Predação Separado por Tratamento")

#N de indivíduos por tratamento
predation_data1 %>%
  group_by(treatment) %>%
  summarise(numero_individuos = n_distinct(individual_code))

###Excluindo lagartas "lost = 1"----
predation_data3 <- predation_data1 %>%
  filter(lost != 1)

####atualização: predation_data3 não foi salvo, então predation_data4 é o df sem lagartas não predadas

#Rodar o modelo considerando predation_data3----
m4 <- glmer(predation ~ treatment + (1 | individual_code),
            data = predation_data4,
            family = binomial)
summary(m4)

pairwise.m4 <- emmeans(m4, list(pairwise ~ treatment))
pairwise.m4

### Calcular a porcentagem de predação por tratamento----
porcentagem_predacao <- predation_data4 %>%
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

## Calcular proporção e erro padrão----
porcentagem_predacao <- predation_data3 %>%
  group_by(treatment) %>%
  summarise(
    total = n(),
    predados = sum(predation == 1),
    proporcao = predados / total,
    erro_padrao = sqrt((proporcao * (1 - proporcao)) / total),
    porcentagem = proporcao * 100,
    erro_padrao_perc = erro_padrao * 100
  )

## Calculando a porcentagem de predação de cada buffer----
predacao_individual <- predation_data3 %>%
  group_by(treatment, individual_code) %>%
  summarise(
    total_obs = n(),                          # Número de observações para o indivíduo
    predados = sum(predation == 1),            # Número de vezes predado
    porcentagem_predacao = (predados / total_obs) * 100
  ) %>%
  ungroup()

m4 =lm(porcentagem_predacao~treatment,data = predacao_individual)
summary.lm(m4)

#Testar as regressões 

m4 = glm(porcentagem_predacao~treatment,
                data = predacao_individual,
                family = binomial(link="logit"))

predacao_individual <- predacao_individual %>%
  mutate(proporcao_predacao = porcentagem_predacao / 100)

summary(m4)

#2.Predation by different guilds in different treatments----
#Model 2: guild ~ treatment + ( 1 | individual_code)----

##Excluindo lagartas lost = 1----
predation_data4 <- predation_data2 %>%
  filter(lost != 1)

##Transformar os dados para formato longo----
dados_long <- predation_data4 %>%
  pivot_longer(
    cols = c(mammal, reptile, bird, arthropod),
    names_to = "guild",
    values_to = "presence"
  )

##Análise com modelo binomial----
m1 <- glm(presence ~ guild * treatment,
              data = dados_long,
              family = binomial)

summary(m1)

##Frequência de cada guilda por tratamento----
dados_long %>%
  count(treatment, guild) %>%
  ggplot(aes(x = treatment, y = n, fill = guilda)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = "Incidência de predação por guilda", x = "Tratamento") +
  theme_minimal()


##Plotando m1----
plot(m1)

##Testando outlier----
install.packages("car")
library(car)
outlierTest(m1) # testa observações com resíduos studentizados extremos
#resultado: a observação 937 teve o maior resíduo studentizado (~2.96), com valor-p sem correção de 0.003

##Investigar a observação 937
dados_long[937, ]
#Outlier tem uma explicação biológica: única predação por mamífero na restauração


##gráfico geom_jitter por guilda----
ggplot(dados_long, aes(x = treatment, y = presence, color = guild)) +
  geom_jitter(width = 0.2, height = 0.05, alpha = 0.6, size = 2) +
  labs(
    title = "Presença de grupos de predadores por ambiente",
    y = "Presença (1 = sim)",
    x = "Ambiente"
  ) +
  facet_wrap(~ guild) +
  theme_minimal()

##geom_point() e position_jitter----
ggplot(dados_long, aes(x = treatment, y = presence, color = guild)) +
  geom_point(position = position_jitter(width = 0.2, height = 0.05), alpha = 0.6, size = 2) +
  facet_wrap(~ guild) +
  theme_minimal()

##proporções esperadas por tratamento: stat_summary()----
ggplot(dados_long, aes(x = treatment, y = presence, color = guild)) +
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
dados_long %>%
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
dados_long %>%
  filter(guilda == "bird") %>%
  group_by(treatment) %>%
  summarise(
    total_obs = n(),
    total_predacao = sum(presence),
    proporcao = total_predacao / total_obs
  )

##contagem absoluta de predação por reptile----
dados_long %>%
  filter(guild == "reptile") %>%
  group_by(treatment) %>%
  summarise(
    total_obs = n(),
    total_predacao = sum(presence),
    proporcao = total_predacao / total_obs
  )

#Não houve predação por répteis - retirar da análise, gerar novo modelo (m2)----

#Excluir répteis
dados_long2 <- dados_long %>%
  filter(guild != "reptile")

#Gerar novo modelo
m2 <- glm(presence ~ guilda * treatment,
          data = dados_long2,
          family = binomial)

summary(m2)
plot(m2)

##Números absolutos de cada guilda - geom_point() e position_jitter----
ggplot(dados_long2, aes(x = treatment, y = presence, color = guilda)) +
  geom_point(position = position_jitter(width = 0.2, height = 0.05), alpha = 0.6, size = 1) +
  facet_wrap(~ guilda) +
  theme_minimal()

##Proporção média de cada guilda---- #salvar gráfico como "fig1"----
fig1=ggplot(dados_long2, aes(x = treatment, y = presence, color = guild)) +
  stat_summary(fun = mean, geom = "point", size = 3, position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~ guild) +
  labs(
    title = "",
    y = "Predation frequency (mean ± SD)",
    x = ""
  ) +
  theme_minimal()

#teste - adicionando imagens no gráfico fig1----
library(ggplot2)
library(ggtext)
library(dplyr)

# Colocar o endereço das imagens
guild_images <- c(
  "bird" = "<img src='images/bird.png' width='40'/>",
  "mammal" = "<img src='images/mammal.png' width='40'/>",
  "arthropod" = "<img src='images/arthropod.png' width='40'/>"
)

# Add new column with the image HTML for facet labels
dados_long2 <- dados_long2 %>%
  mutate(guild_label = guild_images[guilda])

# Criar o gráfico com as imagens como título (facet label)
fig1 <- ggplot(dados_long2, aes(x = treatment, y = presence, color = guilda)) +
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

fig1

#aumentar fonte do texto
+ theme_minimal(base_size = 14) +
  theme(
    strip.text = element_markdown(size = 16),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16)
  )

##Análise post-hoc para entender quais combinações de fatores são significativamente diferentes entre si----
em2 <- emmeans(m2, ~ guilda * treatment, type = "response")
summary(em2)

pairs(em2, adjust = "tukey")

#linhas 3, 16, 22, 23 = p < 0.05
#3 = arthropod agroforestry / arthropod forest
#16 = mammal agroforestry / arthropod forest
#22 = arthropod forest / bird forest
#23 = arthropod forest / mammal forest

##Visualizar os resultados e salvar o gráfico como "fig_m2"----
fig_m2=plot(em2, comparisons = FALSE) #FALSE = sem setas / TRUE = com setas
fig_m2

library(ggplot2)
#aumenta a fonte
fig_m2 + 
  theme(
   axis.text = element_text(size = 13),
  )

##Salvar o pairs em csv----
## Execute a comparação de pares
pairs <- pairs(em2, adjust = "tukey")

## Converta para data frame
pairs_df <- as.data.frame(pairs)

## Salve como CSV
write.csv(pairs_df, file = "tukeytest.csv", row.names = FALSE)

#Salvar RData----
save.image("analyses_predation_paraty.RData")

#3.Predation by multiple guilds in different treatments----
#Model 3: multiple ~ treatment + ( 1 | individual_code)----

## GLM com família quasipoisson (melhor se há overdispersion)
m3 <- glm(multiple ~ treatment, data = predation_data4, family = quasipoisson)

summary(m3)
anova(m3, test = "F")  # ANOVA para avaliar efeito de treatment

# Obter emmeans
em3 <- emmeans(m3, ~ treatment)

# Comparações de pares com ajuste Tukey
pairs_m3 <- pairs(em3, adjust = "tukey")
summary(pairs_m3)

plot(m3)

#m3 não será usado por apenas 6 lagartas tiveram multiple = 2
