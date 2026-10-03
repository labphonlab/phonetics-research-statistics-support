# 第16章 図16-1：測る指標の数と誤検出率（16.4節・notebook ch16 と同一のシミュレーション、seed 2026、2000回）
source("figures/style.R")
set.seed(2026)
n_sim <- 2000
any_significant <- replicate(n_sim, {
  n <- 40
  g <- rep(c("a", "b"), each = n / 2)
  p_values <- c(t.test(rnorm(n) ~ g)$p.value, t.test(rnorm(n) ~ g)$p.value, t.test(rnorm(n) ~ g)$p.value)
  any(p_values < 0.05)
})
sim3 <- mean(any_significant)
cat("3指標のどれかが有意:", round(sim3, 3), " 理論値:", round(1 - 0.95^3, 3), "\n")
k <- 1:10
th <- data.frame(k, rate = 100 * (1 - 0.95^k))
pt <- data.frame(k = 3, rate = 100 * sim3)
p <- ggplot(th, aes(k, rate)) +
  geom_hline(yintercept = 5, linetype = "dotted", colour = "grey40") +
  geom_line(linewidth = 0.7, colour = "grey30") +
  geom_point(shape = 21, fill = "white", colour = "grey30", size = 2.2) +
  geom_segment(aes(x = 1.8, xend = 2.9, y = 27.5, yend = 15.2), data = NULL, inherit.aes = FALSE, linewidth = 0.3, colour = "grey30") +
  geom_point(data = pt, shape = 21, fill = "black", colour = "black", size = 3.8) +
  annotate("text", x = 0.7, y = 30, hjust = 0, size = 3.2, family = "hira",
           label = sprintf("3指標：シミュレーション %.1f%%\n（理論値 %.1f%%）", 100 * round(sim3, 3), 100 * (1 - 0.95^3))) +
  annotate("text", x = 10, y = 7.5, hjust = 1, size = 3.2, family = "hira", label = "1指標のとき：5%") +
  scale_x_continuous(breaks = 1:10) + scale_y_continuous(limits = c(0, 45), breaks = seq(0, 40, 10)) +
  labs(x = "測って見比べる指標の数（真の効果はすべて0）", y = "「どれか1つでも有意」になる確率（%）")
save_fig(p, "ch16_researcher_dof", w = 5.6, h = 3.4)
