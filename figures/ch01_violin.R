# 図1-1：条件別のVOTの分布（バイオリンプロット＋箱ひげ図＋個々の観測）。1.6節。
source("figures/style.R"); source("figures/vot_data.R")
d <- vot_data
p <- ggplot(d, aes(condition, VOT)) +
  geom_violin(aes(fill = condition), trim = FALSE, colour = "grey30", linewidth = 0.4, alpha = 0.9) +
  geom_boxplot(width = 0.14, outlier.shape = NA, colour = "black", fill = "white", linewidth = 0.5) +
  geom_jitter(width = 0.05, height = 0, size = 0.9, colour = "grey20", alpha = 0.55) +
  scale_fill_manual(values = c("grey80", "grey55"), guide = "none") +
  scale_x_discrete(labels = c(voiced = "有声条件", voiceless = "無声条件")) +
  labs(x = NULL, y = "VOT（ms）")
save_fig(p, "ch01_violin", w = 5.2, h = 3.7)
