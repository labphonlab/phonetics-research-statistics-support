# 図12-1：比較の数kと「どれか1つが偶然有意」になる確率。
# 独立な検定の理論値 1-(1-.05)^k、Bonferroni（各検定を .05/k で判定）、
# および第12章12.2節の3群シミュレーション（seed 12、5000回）の実測値。
suppressPackageStartupMessages(library(tidyverse))
source("figures/style.R")
k <- 1:10
cur <- bind_rows(tibble(k, p = 1 - 0.95^k, m = "調整なし（独立な検定の理論値）"),
                 tibble(k, p = 1 - (1 - 0.05 / k)^k, m = "Bonferroni / Holm 調整後"))
cat("1-0.95^3 =", round(1 - 0.95^3, 4), " 1-0.95^10 =", round(1 - 0.95^10, 4), "\n")
stopifnot(round(1 - 0.95^3, 4) == 0.1426, round(1 - 0.95^10, 2) == 0.40)
set.seed(12)
n_sim <- 5000; n_per <- 10
p_raw <- p_holm <- p_bonf <- numeric(n_sim)
for (i in seq_len(n_sim)) {
  d <- data.frame(g = rep(c("A", "B", "C"), each = n_per), y = rnorm(3 * n_per))
  p <- pairwise.t.test(d$y, d$g, p.adjust.method = "none", pool.sd = FALSE)$p.value
  p <- p[!is.na(p)]
  p_raw[i] <- min(p); p_holm[i] <- min(p.adjust(p, "holm")); p_bonf[i] <- min(p.adjust(p, "bonferroni"))
}
sim <- c(raw = mean(p_raw < .05), holm = mean(p_holm < .05), bonf = mean(p_bonf < .05)); print(sim)
stopifnot(round(sim, 4) == c(0.1212, 0.04, 0.04))
simd <- tibble(k = 3, p = c(sim[["raw"]], sim[["bonf"]]), m = c("調整なし（3群シミュレーション）", "Bonferroni / Holm（3群シミュレーション）"))
p <- ggplot() +
  geom_hline(yintercept = 0.05, colour = "grey50", linetype = "dotted", linewidth = 0.5) +
  geom_line(data = cur, aes(k, p, linetype = m), linewidth = 0.8) +
  geom_point(data = cur %>% filter(m == "調整なし（独立な検定の理論値）"), aes(k, p), size = 1.8) +
  geom_point(data = simd, aes(k, p), shape = c(22, 21), size = 3.4, fill = c("white", "grey40"), stroke = 1) +
  annotate("text", x = 2.9, y = 0.24, hjust = 1, size = 3, family = "hira", label = "k = 3：14.3%\n（理論値）") +
  annotate("text", x = 3.2, y = 0.118, hjust = 0, size = 3, family = "hira", label = "実測 12.1%") +
  annotate("text", x = 10, y = 1 - 0.95^10 + 0.045, hjust = 1, size = 3, family = "hira", label = "k = 10：40.1%") +
  annotate("text", x = 3.2, y = 0.022, hjust = 0, size = 3, family = "hira", label = "実測 4.0%") +
  annotate("text", x = 10, y = 0.05 + 0.03, hjust = 1, size = 3, family = "hira", label = "5%") +
  scale_linetype_manual(values = c("solid", "longdash"), name = NULL) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.5), expand = expansion(mult = c(0, 0.02))) +
  labs(x = "比較の数 k", y = "どれかが偶然有意になる確率") +
  guides(linetype = guide_legend(nrow = 2))
save_fig(p, "ch12_familywise_curve", w = 5.6, h = 3.8)
