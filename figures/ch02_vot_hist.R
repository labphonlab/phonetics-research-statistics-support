source("figures/style.R"); source("figures/vot_data.R")
suppressPackageStartupMessages(library(e1071))
st <- vot_data %>% group_by(condition) %>%
  summarise(mean = mean(VOT), median = median(VOT), sd = sd(VOT), iqr = IQR(VOT),
            skew = skewness(VOT, type = 2), .groups = "drop")
print(as.data.frame(st))
lab <- c(voiced = "有声条件", voiceless = "無声条件")
st$lab <- sprintf("平均 %.1f ms（実線）\n中央値 %.1f ms（破線）\nSD %.1f / IQR %.1f", st$mean, st$median, st$sd, st$iqr)
p <- ggplot(vot_data, aes(VOT, fill = condition)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 4, boundary = 0, colour = "white", linewidth = 0.3) +
  geom_density(aes(colour = condition), fill = NA, linewidth = 0.7, adjust = 1.2) +
  geom_vline(data = st, aes(xintercept = mean), linetype = "solid", linewidth = 0.6) +
  geom_vline(data = st, aes(xintercept = median), linetype = "dashed", linewidth = 0.6) +
  geom_text(data = st, aes(x = Inf, y = Inf, label = lab), inherit.aes = FALSE,
            hjust = 1.05, vjust = 1.3, size = 3.1, family = "hira", lineheight = 1.05) +
  facet_wrap(~condition, ncol = 1, labeller = as_labeller(lab)) +
  scale_fill_manual(values = c(voiced = "grey70", voiceless = "grey40"), guide = "none") +
  scale_colour_manual(values = c(voiced = "grey15", voiceless = "grey15"), guide = "none") +
  scale_x_continuous(breaks = seq(0, 120, 20)) +
  labs(x = "VOT（ms）", y = "密度")
save_fig(p, "ch02_vot_hist", w = 5.6, h = 4.2)
