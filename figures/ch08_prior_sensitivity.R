# 図8-2：事前分布の幅 x 話者数の感度分析（8.3節の表の数値をそのまま可視化。再当てはめはしない）。
source("figures/style.R")
d <- data.frame(
  n_spk = rep(c(3, 5, 10), each = 3),
  prior = rep(c("normal(0, 2)", "normal(0, 10)", "normal(0, 30)"), times = 3),
  mean  = c(0.47, 11.48, 52.39, 0.61, 17.83, 54.02, 1.01, 31.80, 50.54),
  lo    = c(-3.4, -7.8, 21.3, -3.3, -3.3, 35.5, -3.0, 9.8, 38.4),
  hi    = c(4.3, 31.5, 75.0, 4.5, 38.7, 68.9, 5.0, 47.4, 61.4))
d$prior <- factor(d$prior, levels = c("normal(0, 2)", "normal(0, 10)", "normal(0, 30)"))
d$spk <- factor(d$n_spk)
p <- ggplot(d, aes(spk, mean, group = prior, linetype = prior, shape = prior, fill = prior)) +
  geom_hline(yintercept = 52.5, colour = "grey30", linewidth = 0.5) +
  geom_linerange(aes(ymin = lo, ymax = hi), position = position_dodge(0.5), colour = "grey35", linewidth = 0.7) +
  geom_point(position = position_dodge(0.5), size = 3, colour = "black") +
  scale_shape_manual(values = c(21, 22, 24), name = "効果の事前分布") +
  scale_fill_manual(values = c("white", "grey60", "black"), name = "効果の事前分布") +
  scale_linetype_manual(values = c("solid", "solid", "solid"), name = "効果の事前分布", guide = "none") +
  scale_y_continuous(breaks = seq(-10, 80, 10)) +
  labs(x = "話者数（名）", y = "事後平均と95%信用区間（ms）")
save_fig(p, "ch08_prior_sensitivity", w = 5.6, h = 3.6)
