source("figures/style.R"); source("figures/vot_data.R")
d <- vot_data %>% group_by(speaker, gender, condition) %>% summarise(v = mean(VOT), .groups = "drop") %>%
  pivot_wider(names_from = condition, values_from = v) %>% mutate(diff = voiceless - voiced)
g <- d %>% group_by(gender) %>% summarise(m = mean(diff), .groups = "drop")
tt <- t.test(diff ~ gender, d); print(tt$statistic); print(g)
d$gender <- factor(d$gender, levels = c("M", "F"), labels = c("男性（S01–S05）", "女性（S06–S10）"))
g$gender <- factor(g$gender, levels = c("M", "F"), labels = levels(d$gender))
set.seed(3)
p <- ggplot(d, aes(gender, diff)) +
  geom_point(aes(shape = gender, fill = gender), position = position_jitter(width = 0.07, height = 0),
             size = 3, colour = "grey15") +
  geom_point(data = g, aes(y = m), shape = 95, size = 14, colour = "black") +
  scale_shape_manual(values = c(21, 24), guide = "none") +
  scale_fill_manual(values = c("grey25", "grey75"), guide = "none") +
  scale_x_discrete(expand = expansion(add = 0.6)) +
  labs(x = NULL, y = "話者ごとの作り分け\n（無声 − 有声の平均VOT, ms）")
save_fig(p, "ch03_speaker_level", w = 4.6, h = 3.4)
