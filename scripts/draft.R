


ggplot(predation_data1, aes(x = treatment, y = presence, color = guild)) +
  stat_summary(fun = mean, geom = "point", size = 3, position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~ guild_label) +
  labs(
    y = "Predation frequency (mean ± CI)",
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


ggplot(predation_data1, aes(x = treatment, y = presence, color = guild)) +
  geom_jitter(width = 0.2, height = 0.05, alpha = 0.3, size = 2) +
  stat_summary(fun = mean, geom = "point", size = 3, position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~ guild_label) +
  labs(
    y = "Predation frequency (mean ± CI)",
    x = "",
    color = "Guild"
  ) +
  theme_minimal(base_size = 14)
