# 図11-1：正答率が同じ（.84）3人の聞き手をROC空間に置く。等分散ガウスの信号検出モデル。
# 数値は第11章「概念をつかむ」節と同じ（ヒット数・誤警報数は100試行中）。
suppressPackageStartupMessages(library(tidyverse))
source("figures/style.R")
dc <- data.frame(listener = c("A", "B", "C"), hit_n = c(84, 95, 73), fa_n = c(16, 27, 5)) %>%
  mutate(hit = hit_n / 100, fa = fa_n / 100, accuracy = (hit_n + (100 - fa_n)) / 200,
         dprime = qnorm(hit) - qnorm(fa), crit = -(qnorm(hit) + qnorm(fa)) / 2)
print(dc)
stopifnot(all(dc$accuracy == .84), all(round(dc$dprime, 2) == c(1.99, 2.26, 2.26)), all(round(dc$crit, 2) == c(0, -.52, .52)))
f <- seq(0.001, 0.999, length.out = 600)
roc <- function(d) tibble(fa = f, hit = pnorm(qnorm(f) + d), d = d)
curves <- bind_rows(roc(1.99), roc(2.26)) %>%
  mutate(lab = factor(sprintf("d' = %.2f", d), levels = c("d' = 2.26", "d' = 1.99")))
# 正答率 = .84 の等高線：(hit + 1 - fa)/2 = .84 → hit = fa + .68
iso <- tibble(fa = c(0, .32), hit = c(.68, 1))
dc <- dc %>% mutate(cs = ifelse(abs(crit) < 1e-9, "0.00", sprintf("%+.2f", crit)),
                    lab = sprintf("聞き手%s\nd'=%.2f, c=%s", listener, dprime, cs),
                    dx = c(0.03, 0.03, 0.035), dy = c(-0.045, -0.055, -0.035))
p <- ggplot() +
  geom_abline(slope = 1, intercept = 0, colour = "grey60", linewidth = 0.4) +
  geom_line(data = iso %>% mutate(fa = pmin(fa, 0.32)), aes(fa, hit), colour = "grey50", linewidth = 0.7, linetype = "dotted") +
  geom_line(data = curves %>% filter(fa <= 0.5), aes(fa, hit, linetype = lab), linewidth = 0.8) +
  geom_point(data = dc, aes(fa, hit, shape = listener), size = 3.6, fill = c("white", "grey55", "black"), stroke = 1) +
  geom_text(data = dc, aes(fa + dx, hit + dy, label = lab), size = 3, lineheight = 0.95, family = "hira", hjust = 0) +
  scale_shape_manual(values = c(A = 21, B = 24, C = 22), guide = "none") +
  scale_linetype_manual(values = c("solid", "longdash"), name = NULL) +
  scale_x_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, .1), labels = function(x) sub("^0", "", sprintf("%.1f", x)), expand = expansion(mult = 0.01)) +
  scale_y_continuous(limits = c(0.5, 1), breaks = seq(0.5, 1, .1), labels = function(x) sub("^0", "", sprintf("%.1f", x)), expand = expansion(mult = 0.01)) +
  coord_fixed(ratio = 1) +
  labs(x = "誤警報率", y = "ヒット率")
save_fig(p, "ch11_roc_listeners", w = 5.2, h = 4.6)
