source("figures/style.R"); source("figures/vot_data.R")
d <- vot_data %>% group_by(speaker, condition) %>% summarise(VOT = mean(VOT), .groups = "drop")
m <- d %>% group_by(condition) %>% summarise(VOT = mean(VOT), .groups = "drop")
p <- ggplot(d, aes(condition, VOT, group = speaker)) +
  geom_line(colour = "grey55", linewidth = 0.6) + geom_point(colour = "grey35", size = 1.8) +
  geom_line(data = m, aes(condition, VOT, group = 1), inherit.aes = FALSE, colour = "black", linewidth = 1.6) +
  geom_point(data = m, aes(condition, VOT), inherit.aes = FALSE, size = 3.6, shape = 21, fill = "white", stroke = 1.4) +
  scale_x_discrete(labels = c(voiced = "有声条件", voiceless = "無声条件"), expand = expansion(add = 0.25)) +
  labs(x = NULL, y = "話者ごとの平均VOT（ms）")
save_fig(p, "ch05_speaker_lines", w = 5.2, h = 3.4)
