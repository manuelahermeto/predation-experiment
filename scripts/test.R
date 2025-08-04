
f2test <- lm(presence ~ guild * treatment, data = frugivory_data1)

test <- emmeans(f2, ~ guild | treatment, 
                at = list(guild = c("arthropod","bird","mammal"), 
                          treatment = c("agroforestry","forest","restauration")))
plot(test)

test <- emmeans(f2, ~ guild | treatment,
                at = list(guild = c("arthropod", "bird", "mammal"), 
                          treatment = c("agroforestry", "forest", "restauration")),
                vcov. = vcov(f2, full = FALSE))
plot(test)


ef2test <- emmeans(f2, ~ guild | treatment, type = "response")
summary(ef2test)
plot(ef2test)

library(ggplot2)

# Converter emmeans para data frame
ef2testdf <- as.data.frame(ef2test)

# Ocultar barras com UCL == 1
ef2testdf$asymp.UCL[ef2testdf$asymp.UCL >= 0.999] <- NA
ef2testdf$asymp.LCL[is.na(ef2testdf$asymp.UCL)] <- NA

# Plot personalizado
ggplot(ef2testdf, aes(x = prob, y = guild, color = treatment, group = treatment)) +
  geom_point(position = position_dodge(0.3), size = 3) +
  geom_errorbar(aes(xmin = asymp.LCL, xmax = asymp.UCL),
                position = position_dodge(0.3),
                width = 0.2,
                size = 1.5,
                alpha = 0.5) +  #transparência da linha
  scale_x_continuous(limits = c(0, 0.8)) +
  labs(x = NULL, y = NULL) +
  theme_gray()

ggplot(ef2testdf, aes(x = prob, y = guild, color = guild, group = guild)) +
  geom_point(position = position_dodge(0.3), size = 3) +
  geom_errorbar(aes(xmin = asymp.LCL, xmax = asymp.UCL),
                position = position_dodge(0.3),
                width = 0.2,
                size = 1.2,
                alpha = 0.5) +
  scale_x_continuous(limits = c(0, 0.8)) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~ treatment, nrow = 1, scales = "fixed") +  # ⬅️ facetas na horizontal com mesmo eixo x
  theme_gray()

###Agrupar por tratamento
ggplot(ef2testdf, aes(y = guild, x = prob, color = guild, group = guild)) +
  geom_point(position = position_dodge(0.3), size = 3) +
  geom_errorbar(aes(xmin = asymp.LCL, xmax = asymp.UCL),
                position = position_dodge(0.3),
                width = 0.2,
                size = 1.2,
                alpha = 0.5) +
  scale_x_continuous(limits = c(0, 0.8)) +
  labs(x = NULL, y = NULL) +
  facet_grid(treatment ~ ., scales = "fixed", switch = "x") +  # facetas verticais, eixo x compartilhado
  theme_gray() +
  theme(strip.placement = "outside",     # nomes dos tratamentos à esquerda
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 1),
        legend.position = "none")  # remove a legenda

#### Agrupar por guilda
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













