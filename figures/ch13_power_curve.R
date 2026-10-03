# 図13-1：2群比較のt検定の検定力曲線（SD = 15 ms、有意水準.05）。本文13.1節の power.t.test() と同じ計算。
# 図13-2：真の効果5msのとき、標準誤差ごとの検定力と、有意になった推定値の誇張率（Type M）。13.4節の表と同じ設定。
suppressPackageStartupMessages(library(tidyverse))
source("figures/style.R")
n_seq <- 5:100
curve <- expand_grid(n = n_seq, delta = c(5, 10)) %>%
  mutate(power = map2_dbl(n, delta, ~ power.t.test(n = .x, delta = .y, sd = 15, sig.level = 0.05)$power),
         lab = factor(paste0("差 ", delta, " ms"), levels = c("差 10 ms", "差 5 ms")))
pts <- expand_grid(n = c(10, 20, 40, 80), delta = c(5, 10)) %>%
  mutate(power = map2_dbl(n, delta, ~ power.t.test(n = .x, delta = .y, sd = 15, sig.level = 0.05)$power),
         lab = factor(paste0("差 ", delta, " ms"), levels = c("差 10 ms", "差 5 ms")))
print(pts)   # 本文表の theory 列（.105 .176 .313 .554 / .292 .538 .838 .987）と一致するはず
th <- c(.105, .176, .313, .554, .292, .538, .838, .987)
stopifnot(all(abs(pts$power[order(pts$delta, pts$n)] - th) < 6e-4))
key <- pts %>% filter((delta == 10 & n == 20) | (delta == 5 & n == 80))
p1 <- ggplot(curve, aes(n, power, linetype = lab)) +
  geom_hline(yintercept = 0.8, colour = "grey50", linewidth = 0.4, linetype = "longdash") +
  annotate("text", x = 100, y = 0.82, label = "検定力 .80", hjust = 1, vjust = 0, size = 3, family = "hira", colour = "grey30") +
  geom_line(linewidth = 0.9, colour = "black") +
  geom_point(data = pts, aes(shape = lab), size = 2.4, fill = "white", colour = "black") +
  geom_point(data = key, shape = 21, size = 4.2, fill = "grey55", colour = "black", stroke = 1) +
  geom_text(data = key, aes(label = sprintf(".%02d", round(power * 100))), nudge_y = -0.07, size = 3.2, family = "hira") +
  scale_shape_manual(values = c(21, 24)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .2), labels = function(x) sub("^0", "", sprintf("%.1f", x))) +
  scale_x_continuous(breaks = c(10, 20, 40, 60, 80, 100)) +
  guides(shape = guide_legend(title = NULL), linetype = guide_legend(title = NULL)) +
  labs(x = "各群の語数 n", y = "検定力")
save_fig(p1, "ch13_power_curve", w = 5.4, h = 3.5)

# Type M / Type S
set.seed(1)
tm <- map_dfr(c(1.5, 3, 5), function(se) {
  est <- rnorm(1e6, 5, se); sig <- abs(est / se) > qnorm(.975)
  tibble(se = se, power = mean(sig), typeS = mean(sig & est < 0) / mean(sig),
         exagg = mean(abs(est[sig])) / 5)
})
print(tm)
# 本文表: 0.915/0.000/1.05, 0.385/0.000/1.60, 0.170/0.009/2.49
se_seq <- seq(0.8, 6, by = 0.1)
exg <- map_dfr(se_seq, function(se) {
  z <- 5 / se; cr <- qnorm(.975)
  pw <- pnorm(z - cr) + pnorm(-z - cr)
  # 有意なときの |推定値| の期待値/真値（正規の切断モーメントによる解析解）
  num <- se * (dnorm(cr - z) + dnorm(cr + z)) + 5 * (pnorm(z - cr) - pnorm(-z - cr))
  tibble(se, power = pw, exagg = num / pw / 5)
})
stopifnot(abs(approx(exg$se, exg$exagg, 3)$y - tm$exagg[2]) < 0.02)
pm <- tm %>% mutate(lab = sprintf("SE %.1f ms\n%.2f倍", se, exagg))
p2 <- ggplot(exg, aes(power, exagg)) +
  geom_hline(yintercept = 1, colour = "grey50", linewidth = 0.4, linetype = "longdash") +
  geom_line(linewidth = 0.9) +
  geom_point(data = pm, size = 3.6, shape = 21, fill = "grey55", stroke = 1) +
  geom_text(data = pm, aes(label = lab), nudge_y = 0.22, nudge_x = c(-0.03, 0.04, 0.09), size = 3, lineheight = 0.95, family = "hira") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, .2), labels = function(x) sub("^0", "", sprintf("%.1f", x))) +
  scale_y_continuous(limits = c(0.8, 3.6), breaks = c(1, 2, 3)) +
  labs(x = "検定力", y = "有意な推定値の誇張率（倍）")
save_fig(p2, "ch13_typeM", w = 5.2, h = 3.3)
