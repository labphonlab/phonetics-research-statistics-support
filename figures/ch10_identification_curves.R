# 図10-1：同定曲線（F0別）と、GLMMから求めたカテゴリ境界。データ生成は ch10 ノートブックと同一。
source("figures/style.R")
suppressPackageStartupMessages({library(tidyverse); library(lme4)})
set.seed(2024)
n_listener <- 24; vot_steps <- seq(10, 70, by = 10); f0_levels <- c("low", "high"); n_rep <- 8
design <- expand_grid(listener = sprintf("L%02d", 1:n_listener), vot = vot_steps, f0 = f0_levels, rep = 1:n_rep)
b0 <- -3.2; b_vot <- 0.09; b_f0 <- 0.8
listener_intercept <- rnorm(n_listener, 0, 0.7); listener_slope <- rnorm(n_listener, 0, 0.03)
perc <- design %>% mutate(li = as.integer(factor(listener)),
  f0_num = if_else(f0 == "high", 0.5, -0.5),
  eta = b0 + (b_vot + listener_slope[li]) * vot + b_f0 * f0_num + listener_intercept[li],
  response = rbinom(n(), size = 1, prob = plogis(eta))) %>% select(-li, -eta)
m <- glmer(response ~ vot + f0_num + (1 + vot | listener), family = binomial, data = perc)
fe <- fixef(m)
bd <- c(mid = -fe[[1]] / fe[["vot"]], low = -(fe[[1]] - 0.5 * fe[["f0_num"]]) / fe[["vot"]],
        high = -(fe[[1]] + 0.5 * fe[["f0_num"]]) / fe[["vot"]])
print(round(fe, 4)); print(round(bd, 2)); cat("weight", round(fe[["f0_num"]] / fe[["vot"]], 2), "\n")
pts <- perc %>% group_by(vot, f0) %>% summarise(p = mean(response), .groups = "drop")
print(pts %>% pivot_wider(names_from = f0, values_from = p))
xs <- seq(5, 75, length.out = 200)
cur <- expand_grid(vot = xs, f0 = f0_levels) %>%
  mutate(p = plogis(fe[[1]] + fe[["vot"]] * vot + fe[["f0_num"]] * if_else(f0 == "high", 0.5, -0.5)))
lab <- c(low = "F0 低", high = "F0 高")
cur$g <- lab[cur$f0]; pts$g <- lab[as.character(pts$f0)]
bds <- tibble(g = lab[c("low", "high")], b = bd[c("low", "high")])
p <- ggplot() +
  geom_hline(yintercept = 0.5, colour = "grey55", linewidth = 0.4, linetype = "dashed") +
  geom_segment(data = bds, aes(x = b, xend = b, y = 0, yend = 0.5, linetype = g), colour = "grey40", linewidth = 0.5) +
  geom_line(data = cur, aes(vot, p, linetype = g), linewidth = 1.1) +
  geom_point(data = pts, aes(vot, p, shape = g, fill = g), size = 2.8) +
  annotate("text", x = bd[["low"]] + 1.2, y = 0.03, label = sprintf("%.2f ms", bd[["low"]]), hjust = 0, family = "hira", size = 3.1) +
  annotate("text", x = bd[["high"]] - 1.2, y = 0.03, label = sprintf("%.2f ms", bd[["high"]]), hjust = 1, family = "hira", size = 3.1) +
  scale_linetype_manual(values = c("F0 低" = "solid", "F0 高" = "longdash"), name = NULL) +
  scale_shape_manual(values = c("F0 低" = 21, "F0 高" = 24), name = NULL) +
  scale_fill_manual(values = c("F0 低" = "white", "F0 高" = "grey25"), name = NULL) +
  scale_x_continuous(breaks = vot_steps) +
  scale_y_continuous(breaks = seq(0, 1, .25), labels = scales::percent_format(accuracy = 1)) +
  labs(x = "刺激のVOT（ms）", y = "「無声」と答えた割合")
save_fig(p, "ch10_identification_curves", w = 5.6, h = 3.6)

# 聞き手別の曲線（GLMMのBLUPを含む予測）
li <- coef(m)$listener
lc <- expand_grid(listener = rownames(li), vot = xs) %>%
  mutate(p = plogis(li[listener, "(Intercept)"] + li[listener, "vot"] * vot))
cat("listener vot coef range", round(range(li$vot), 4), "\n")
cat("listener boundary range (BLUP, f0 mid)", round(range(-li[,1]/li$vot), 1), "\n")
pm <- tibble(vot = xs, p = plogis(fe[[1]] + fe[["vot"]] * vot))
p2 <- ggplot() +
  geom_hline(yintercept = 0.5, colour = "grey55", linewidth = 0.4, linetype = "dashed") +
  geom_line(data = lc, aes(vot, p, group = listener), colour = "grey60", linewidth = 0.4) +
  geom_line(data = pm, aes(vot, p), colour = "black", linewidth = 1.5) +
  scale_x_continuous(breaks = vot_steps) +
  scale_y_continuous(breaks = seq(0, 1, .25), labels = scales::percent_format(accuracy = 1)) +
  labs(x = "刺激のVOT（ms）", y = "「無声」と答える確率（F0が中間のとき）")
save_fig(p2, "ch10_listener_curves", w = 5.6, h = 3.4)
