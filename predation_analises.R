#test run

#dizer o diretório
setwd("C:/Users/ERIKA/Downloads/Manuela/predation-experiment")

#pacotes usados
library("lme4")    # glmer and drop1
library("dplyr")   # put forest in order 
library("emmeans") # emmeans
library("DHARMa")  # test residual

# read the data (que está em .csv)
predation_data1 <- read.csv("paraty_experimento_predacao_2023 csv.csv",header = TRUE)
# Data come from the csv called 'paraty_experimento_predacao_2023 csv.csv'
str(predation_data1)

################################################################################
## 1. Predation incidence in different treatments----

## What is the response variable?
# total predation 
## What is the predictor variable?
# type of treatment (F1, F2, SAF1, SAF2, R)

# Model: predation ~ treatment + ( 1 | individual_code)

#Agrupando colunas "treatment" e "treatment.number"
predation_data2 <- predation_data1 %>%
      mutate(id_treatment = paste(treatment, treatment.number, sep = "_"))
write.csv(predation_data2, "C:/Users/ERIKA/Downloads/Manuela/predation-experiment/predation_data2.csv", row.names = FALSE)

#Análises considerando 5 tratamentos (predation_data2)

m1 <- glmer(predation ~ id_treatment + (1 | individual_code),
            data = predation_data2,
            family = binomial)
summary(m1)

#Testando as diferenças entre as réplicas

paiwise.m1 <- emmeans(m1, list(pairwise ~ id_treatment))
paiwise.m1

#Análises considerando 3 tratamentos (predation_data1)

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

#N de indivíduos por tratamento----
predation_data1 %>%
  group_by(treatment) %>%
  summarise(numero_individuos = n_distinct(individual_code))

###Excluindo lagartas "lost = 1"----
predation_data3 <- predation_data1 %>%
  filter(lost != 1)

#Rodar o modelo considerando predation_data3
m3 <- glmer(predation ~ treatment + (1 | individual_code),
            data = predation_data3,
            family = binomial)
summary(m3)

pairwise.m3 <- emmeans(m3, list(pairwise ~ treatment))
pairwise.m3

### Calcular a porcentagem de predação por tratamento----
porcentagem_predacao <- predation_data3 %>%
  group_by(treatment) %>%
  summarise(
    total = n(),                               # Total de indivíduos por tratamento
    predados = sum(predation == 1),             # Quantos foram predados (predation == 1)
    porcentagem = (predados / total) * 100      # Calcula a porcentagem
  )

### Gráfico de barras com porcentagem por tratamento----
ggplot(porcentagem_predacao, aes(x = treatment, y = porcentagem, fill = treatment)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0(round(porcentagem, 1), "%")), 
            vjust = -0.5, size = 4) +
  theme_minimal() +
  labs(x = "Tratamento", y = "Predação (%)", title = "Porcentagem de Predação por Tratamento") +
  theme(legend.position = "none")

# Calcular proporção e erro padrão
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

#Calculando a porcentagem de predação de cada buffer----
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



