# 図：edfの異なる平滑曲線を同じ12点に当てはめる（本文7.2「概念をつかむ：edfは曲線の複雑さ」(A)と同一コード・seed）
source("figures/style.R")
suppressPackageStartupMessages({library(tidyverse); library(mgcv)})
f_true <- function(x) 120 + 40 * sin(pi * x)
x <- seq(0, 1, length.out = 12); grid <- data.frame(x = seq(0, 1, length.out = 200))
set.seed(7)
d <- data.frame(x = x, y = f_true(x) + rnorm(12, sd = 6))
y_new <- f_true(x) + rnorm(12, sd = 6)
ms <- list("直線" = gam(y ~ x, data = d),
           "k = 4 固定" = gam(y ~ s(x, k = 4, fx = TRUE), data = d),
           "既定 s(x)" = gam(y ~ s(x), data = d, method = "REML"),
           "k = 10 固定" = gam(y ~ s(x, k = 10, fx = TRUE), data = d))
rows <- imap_dfr(ms, function(m, nm) {
  edf <- sum(m$edf) - 1; rss <- sum(resid(m)^2); rn <- sqrt(mean((y_new - fitted(m))^2))
  cat(nm, round(edf, 2), round(rss, 2), round(rn, 2), "\n")
  lab <- sprintf("%s\nedf = %.2f、RSS = %.1f\n新値RMSE = %.2f", nm, edf, rss, rn)
  data.frame(x = grid$x, fit = as.numeric(predict(m, grid)), panel = lab)
})
rows$panel <- factor(rows$panel, levels = unique(rows$panel))
tr <- data.frame(x = grid$x, y = f_true(grid$x))
p <- ggplot(rows) +
  geom_line(data = tr, aes(x, y), colour = "grey60", linewidth = 0.6, linetype = "dashed") +
  geom_line(aes(x, fit), linewidth = 1.0, colour = "black") +
  geom_point(data = d, aes(x, y), shape = 21, size = 1.9, fill = "white", stroke = 0.7) +
  facet_wrap(~panel, ncol = 2) +
  labs(x = "正規化時間", y = "F0（Hz）")
save_fig(p, "ch07_edf_panels", w = 5.6, h = 3.8)
