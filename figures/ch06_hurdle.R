# 図：ハードルモデルの考え方（本文6.7、set.seed(66)、ノートブックと同一）
source("figures/style.R")
suppressPackageStartupMessages({library(tidyverse); library(lme4); library(lmerTest); library(patchwork)})
set.seed(66)
d <- expand_grid(speaker = paste0("S", 1:12), word = 1:20) %>%
  mutate(
    condition = rep(c("normal", "fast"), length.out = n()),
    sp_re = rep(rnorm(12, 0, 0.5), each = 20)[as.integer(factor(speaker))],
    p_prevoice = plogis(0.4 + ifelse(condition == "fast", -1.0, 0) + sp_re),
    uses_prevoice = rbinom(n(), 1, p_prevoice),
    prevoice_duration = ifelse(uses_prevoice == 1, rgamma(n(), shape = 9, rate = 0.15), NA),
    VOT = ifelse(uses_prevoice == 1, -prevoice_duration, rnorm(n(), 5, 8)))
print(round(coef(summary(lmer(VOT ~ condition + (1 | speaker), data = d))), 3))
print(round(coef(summary(glmer(uses_prevoice ~ condition + (1 | speaker), data = d, family = binomial))), 4))
print(round(coef(summary(lmer(prevoice_duration ~ condition + (1 | speaker), data = filter(d, uses_prevoice == 1)))), 3))
cl <- c(fast = "速い発話", normal = "普通の速さ"); d$cond <- factor(cl[d$condition], levels = cl[c("normal", "fast")])
sm <- d %>% group_by(cond) %>% summarise(use = mean(uses_prevoice), dur = mean(prevoice_duration, na.rm = TRUE),
                                         vot = mean(VOT), n = n(), k = sum(uses_prevoice))
print(sm)
d$grp <- factor(ifelse(d$uses_prevoice == 1, "前voicingあり（VOT＜0）", "前voicingなし（VOT＞0）"),
                levels = c("前voicingあり（VOT＜0）", "前voicingなし（VOT＞0）"))
pA <- ggplot(d, aes(VOT, fill = grp)) +
  geom_histogram(binwidth = 8, boundary = 0, colour = "white", linewidth = 0.2) +
  geom_vline(data = sm, aes(xintercept = vot), linetype = "dashed", linewidth = 0.6) +
  geom_text(data = sm, aes(x = vot + 3, y = Inf, label = sprintf("1本の平均 %.1f", vot)), vjust = 1.6, hjust = 0,
            size = 2.7, family = "hira", inherit.aes = FALSE, colour = "black") +
  facet_wrap(~cond, ncol = 1) +
  scale_fill_manual(values = c("grey25", "grey75"), name = NULL) +
  labs(x = "VOT（ms）", y = "度数") + theme(legend.position = "bottom", legend.direction = "vertical", legend.text = element_text(size = 8))
pB <- ggplot(sm, aes(cond, use)) +
  geom_col(width = 0.5, fill = "grey35") +
  geom_text(aes(label = sprintf("%.0f%%", 100 * use)), vjust = -0.5, size = 3, family = "hira") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(x = NULL, y = "使用率") + labs(subtitle = "第1段階：使うかどうか（p = 0.041）") +
  theme(plot.subtitle = element_text(size = 8.5))
dd <- d %>% filter(uses_prevoice == 1)
pC <- ggplot(dd, aes(cond, prevoice_duration)) +
  geom_jitter(width = 0.12, height = 0, shape = 21, fill = "white", colour = "grey35", size = 1, stroke = 0.4) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "black") +
  coord_cartesian(ylim = c(0, 140)) +
  labs(x = NULL, y = "持続時間（ms）", subtitle = "第2段階：使った場合の長さ（p = 0.72）") +
  theme(plot.subtitle = element_text(size = 8.5))
p <- pA | (pB / pC)
p <- p + plot_layout(widths = c(1.15, 1))
save_fig(p, "ch06_hurdle", w = 5.8, h = 3.8)
