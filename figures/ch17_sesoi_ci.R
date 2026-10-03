# 第17章 図17-1（効果量の信頼区間とSESOI）・図17-2（サブサンプリング）。本文17.2〜17.5節と同一のコード・seed。
source("figures/style.R")
suppressPackageStartupMessages({library(tidyverse); library(lme4); library(lmerTest)})
set.seed(90)
d <- expand_grid(speaker = paste0("S", 1:200), token = 1:100) %>%
  mutate(
    sp_re = rep(rnorm(200, 0, 10), each = 100),
    condition = rep(c("a","b"), length.out = n()),
    VOT = 50 + ifelse(condition == "b", 0.3, 0) + sp_re + rnorm(n(), 0, 15))
m <- lmer(VOT ~ condition + (1 | speaker), data = d)
print(summary(m)$coefficients)
est <- fixef(m)["conditionb"]; se <- sqrt(vcov(m)["conditionb","conditionb"])
ci90 <- est + c(-1.645, 1.645) * se
ci95 <- est + c(-1.96, 1.96) * se
cat("est", round(est, 3), "se", round(se, 3), "ci90", round(ci90, 3), "ci95", round(ci95, 3), "\n")

# ---- 図17-1 ----
p1 <- ggplot() +
  annotate("rect", xmin = -2, xmax = 2, ymin = -Inf, ymax = Inf, fill = "grey88") +
  geom_vline(xintercept = 0, linetype = "dotted", colour = "grey20") +
  geom_vline(xintercept = c(-2, 2), linetype = "dashed", colour = "grey35", linewidth = 0.5) +
  geom_segment(aes(x = ci95[1], xend = ci95[2], y = 1.45, yend = 1.45), linewidth = 0.5, colour = "grey40") +
  geom_segment(aes(x = ci90[1], xend = ci90[2], y = 1, yend = 1), linewidth = 1.6, colour = "black") +
  geom_point(aes(x = est, y = 1), shape = 21, size = 3.8, fill = "white", stroke = 1.3) +
  geom_point(aes(x = est, y = 1.45), shape = 21, size = 2.4, fill = "white", stroke = 0.8) +
  annotate("text", x = 2.1, y = 1.45, hjust = 0, size = 3.1, family = "hira", label = "95%信頼区間\n（0を含まない＝p<.05）") +
  annotate("text", x = 2.1, y = 1.0, hjust = 0, size = 3.1, family = "hira", label = "90%信頼区間\n（同等性検定に使う）") +
  annotate("text", x = -1, y = 0.7, size = 3.1, family = "hira", label = "SESOI\n±2 ms") +
  annotate("text", x = 0.02, y = 1.9, hjust = 0, size = 3.1, family = "hira", label = "効果ゼロ") +
  scale_x_continuous(limits = c(-3, 5.4), breaks = -2:2) +
  scale_y_continuous(limits = c(0.4, 2.0)) +
  labs(x = "条件bの効果（ms）", y = NULL) +
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
save_fig(p1, "ch17_sesoi_ci", w = 5.6, h = 2.9)

# ---- 図17-2 ----
speakers <- unique(d$speaker)
set.seed(91)
holdout_errors <- replicate(20, {
  test_sp <- sample(speakers, 20)
  train <- filter(d, !speaker %in% test_sp); test <- filter(d, speaker %in% test_sp)
  m_tr <- lmer(VOT ~ condition + (1 | speaker), data = train)
  pred <- predict(m_tr, newdata = test, allow.new.levels = TRUE)
  sqrt(mean((test$VOT - pred)^2))
})  # 本文と乱数列を揃えるため実行（図には使わない）
set.seed(92)
subsample_est <- function(prop) {
  sub_sp <- sample(speakers, round(length(speakers) * prop))
  fixef(lmer(VOT ~ condition + (1 | speaker), data = filter(d, speaker %in% sub_sp)))["conditionb"]
}
sub <- map_dfr(c(0.25, 0.5, 0.75, 1.0), function(p) {
  e <- replicate(10, subsample_est(p))
  cat(sprintf("話者%.0f%%使用: 平均=%.3f, SD=%.3f\n", p*100, mean(e), sd(e)))
  tibble(prop = p, est = e)
})
sub <- sub %>% mutate(lab = factor(sprintf("%d%%\n（%d名）", round(prop*100), round(200*prop)),
  levels = sprintf("%d%%\n（%d名）", c(25, 50, 75, 100), c(50, 100, 150, 200))))
p2 <- ggplot(sub, aes(lab, est)) +
  geom_hline(yintercept = est, linetype = "dashed", colour = "grey35") +
  geom_point(shape = 21, fill = "grey60", colour = "grey20", size = 2,
             position = position_jitter(width = 0.1, height = 0, seed = 1)) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3.4, fill = "white", stroke = 1) +
  annotate("text", x = 4.45, y = est + 0.2, hjust = 1, size = 3.1, family = "hira", label = sprintf("破線：全話者での\n推定値 %.3f", est)) +
  labs(x = "使用した話者の割合（10回ずつ抽出）", y = "条件bの効果の推定値（ms）")
save_fig(p2, "ch17_subsample", w = 5.4, h = 3.4)
