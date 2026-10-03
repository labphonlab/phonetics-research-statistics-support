# 第15章 図15-3：情報を持つ欠測（15.4節・notebook ch15 と同一のデータ生成、seed 16）
suppressPackageStartupMessages(library(tidyverse))
source("figures/style.R")
set.seed(16)
n_speaker <- 20; n_item <- 40
f0_data <- expand_grid(speaker = sprintf("S%02d", 1:n_speaker), item = 1:n_item) %>%
  mutate(cond = rep(c("modal", "creaky"), length.out = n()),
         f0_true = 180 + if_else(cond == "creaky", -25, 0) +
           rnorm(n_speaker, 0, 15)[as.integer(factor(speaker))] + rnorm(n(), 0, 12),
         p_fail = plogis(-1.0 - 0.10 * (f0_true - 180)),
         failed = rbinom(n(), 1, p_fail),
         f0_obs = if_else(failed == 1, NA_real_, f0_true))
s <- f0_data %>% group_by(cond) %>%
  summarise(fail = mean(failed), true = mean(f0_true), obs = mean(f0_obs, na.rm = TRUE))
print(s)
cat(sprintf("diff true %.2f obs %.2f\n", s$true[s$cond=="modal"]-s$true[s$cond=="creaky"], s$obs[s$cond=="modal"]-s$obs[s$cond=="creaky"]))
lv <- c(creaky = "きしみ声条件", modal = "通常条件")
d <- f0_data %>% mutate(c2 = factor(lv[cond], levels = lv), status = if_else(failed == 1, "抽出に失敗", "測定できた"))
m <- s %>% mutate(c2 = factor(lv[cond], levels = lv)) %>% pivot_longer(c(true, obs), names_to = "kind", values_to = "m") %>%
  mutate(kind = factor(kind, levels = c("true", "obs"), labels = c("真の平均（全トークン）", "観測された平均（測定できた分のみ）")))
lab <- s %>% mutate(c2 = factor(lv[cond], levels = lv), lab = sprintf("失敗率 %.1f%%", 100 * fail))
set.seed(1)
p <- ggplot(d, aes(f0_true, c2)) +
  geom_jitter(aes(shape = status, colour = status), height = 0.2, size = 1.5, alpha = 0.8) +
  geom_segment(data = m, aes(x = m, xend = m, y = as.integer(c2) - 0.38, yend = as.integer(c2) + 0.38, linetype = kind), linewidth = 0.9, colour = "black") +
  geom_text(data = lab, aes(x = 112, y = as.integer(c2) + 0.42, label = lab), inherit.aes = FALSE, hjust = 0, family = "hira", size = 3.2) +
  scale_shape_manual(values = c("測定できた" = 16, "抽出に失敗" = 4), name = NULL) +
  scale_colour_manual(values = c("測定できた" = "grey55", "抽出に失敗" = "black"), name = NULL) +
  scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
  guides(shape = guide_legend(order = 1, nrow = 1), colour = guide_legend(order = 1, nrow = 1), linetype = guide_legend(order = 2, nrow = 2)) +
  scale_y_discrete(expand = expansion(add = c(0.55, 0.7))) +
  labs(x = "真のF0（Hz）", y = NULL) + theme(legend.box = "vertical", legend.spacing.y = unit(0, "pt"), legend.margin = margin(0,0,0,0), legend.key.height = unit(10, "pt"))
save_fig(p, "ch15_informative_missing", w = 5.6, h = 3.6)
