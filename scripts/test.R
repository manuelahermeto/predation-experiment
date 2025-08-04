
em2test <- emmeans(f2, ~ guild | treatment, type = "response")
summary(em2testdf)
plot(ef2test)


# Converter emmeans para data frame
ef2testdf <- as.data.frame(ef2test)

# Ocultar barras com UCL == 1
ef2df$asymp.UCL[ef2df$asymp.UCL >= 0.999] <- NA
ef2df$asymp.LCL[is.na(ef2df$asymp.UCL)] <- NA

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
#Colocar tratamentos na ordem

ggplot(ef2test, aes(y = treatment, x = prob, color = treatment, group = treatment)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbarh(aes(xmin = asymp.LCL, xmax = asymp.UCL),
                 position = position_dodge(width = 0.3),
                 height = 0.2,
                 size = 1.2,
                 alpha = 0.5) +
  scale_x_continuous(limits = c(0, 0.8)) +
  labs(x = NULL, y = NULL) +
  facet_grid(guild ~ ., scales = "fixed", switch = "x") +
  theme_gray(base_size = 18) +  # ⬅️ Aumenta a fonte geral
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 1),
        legend.position = "none")


str(em2df$treatment)
class(ef2test)


summary(ef2df)

#Colocar tratamentos em ordem
em2df$treatment_order <- factor(em2df$treatment_order,
                                    levels = c("agroforestry", "restauration", "forest"))
#Gráfico fig2_m2
ggplot(em2df, aes(y = treatment_order, x = prob, color = treatment_order, group = treatment_order)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = asymp.LCL, xmax = asymp.UCL),
                 height = 0.2,
                 size = 1.2,
                 alpha = 0.5) +
  scale_x_continuous(limits = c(0, 0.8)) +
  scale_y_discrete(limits = levels(em2df$treatment_order)) +  
  labs(x = NULL, y = NULL) +
  facet_grid(guild ~ ., scales = "fixed", switch = "x") +
  theme_gray(base_size = 18) +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 1),
        legend.position = "none")
