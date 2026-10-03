# 図：多項ロジスティック回帰の予測確率（本文6.6、set.seed(6)、ノートブックと同一）
source("figures/style.R")
suppressPackageStartupMessages({library(tidyverse); library(nnet)})
set.seed(6)
accent_types <- c("平板型", "頭高型", "中高型", "尾高型")
d <- expand_grid(speaker = paste0("S", 1:12), word = 1:20) %>%
  mutate(
    true_type = sample(accent_types, n(), replace = TRUE, prob = c(0.35, 0.25, 0.20, 0.20)),
    fall_pos = case_when(
      true_type == "平板型" ~ rnorm(n(), 0.80, 0.22),
      true_type == "頭高型" ~ rnorm(n(), 0.18, 0.20),
      true_type == "中高型" ~ rnorm(n(), 0.45, 0.20),
      true_type == "尾高型" ~ rnorm(n(), 0.65, 0.20)),
    fall_pos = pmin(pmax(fall_pos, 0), 1),
    true_type = factor(true_type, levels = accent_types))
m <- multinom(true_type ~ fall_pos, data = d, trace = FALSE)
print(round(coef(m), 3))
print(round(predict(m, data.frame(fall_pos = c(0.1, 0.5, 0.9)), type = "probs"), 3))
cat("acc", round(mean(predict(m, d) == d$true_type), 3), "chance", round(max(table(d$true_type)) / nrow(d), 3), "\n")
g <- data.frame(fall_pos = seq(0, 1, length.out = 201))
pr <- as.data.frame(predict(m, g, type = "probs")) %>% bind_cols(g) %>%
  pivot_longer(-fall_pos, names_to = "type", values_to = "p") %>% mutate(type = factor(type, levels = accent_types))
marks <- data.frame(fall_pos = c(0.1, 0.5, 0.9))
lt <- c("solid", "dashed", "dotdash", "dotted"); gr <- c("grey10", "grey35", "grey10", "grey45")
lab <- data.frame(type = factor(accent_types, levels = accent_types), fall_pos = c(0.86, 0.14, 0.22, 0.78), dy = c(0.07, 0.07, 0.07, 0.07))
lab <- lab %>% rowwise() %>% mutate(p = pr$p[pr$type == type][which.min(abs(pr$fall_pos[pr$type == type] - fall_pos))] + dy) %>% ungroup()
p <- ggplot(pr, aes(fall_pos, p, linetype = type, colour = type)) +
  geom_vline(data = marks, aes(xintercept = fall_pos), colour = "grey75", linewidth = 0.4, linetype = "solid") +
  geom_line(linewidth = 1.0) +
  geom_text(data = lab, aes(label = type), family = "hira", size = 3.2, show.legend = FALSE, hjust = 0.5) +
  scale_linetype_manual(values = lt, name = NULL) + scale_colour_manual(values = gr, name = NULL) +
  scale_x_continuous(breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)) +
  labs(x = "F0下降の位置 fall_pos（0＝語頭付近、1＝語末付近）", y = "予測確率") +
  theme(legend.position = "bottom")
save_fig(p, "ch06_multinom_probs", w = 5.6, h = 3.4)
