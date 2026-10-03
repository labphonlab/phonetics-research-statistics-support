# 図11-3：反応時間の対数変換前後の残差。データ・モデルは第11章11.4節のコード（seed 21）と同一。
suppressPackageStartupMessages({library(tidyverse); library(lme4); library(lmerTest); library(patchwork)})
source("figures/style.R")
set.seed(21)
n_subj <- 30; n_item <- 40
rt <- expand_grid(subj = sprintf("S%02d", 1:n_subj), item = sprintf("I%02d", 1:n_item)) %>%
  mutate(cond = rep(c("easy", "hard"), length.out = n()),
         mu = 6.2 + if_else(cond == "hard", 0.15, 0) + rnorm(n_subj, 0, 0.18)[as.integer(factor(subj))] +
           rnorm(n_item, 0, 0.08)[as.integer(factor(item))],
         RT = exp(mu + rnorm(n(), 0, 0.25)))
m_raw <- lmer(RT ~ cond + (1 | subj) + (1 | item), data = rt)
m_log <- lmer(log(RT) ~ cond + (1 | subj) + (1 | item), data = rt)
w_raw <- shapiro.test(resid(m_raw))$statistic; w_log <- shapiro.test(resid(m_log))$statistic
cat("W raw:", round(w_raw, 4), " W log:", round(w_log, 4), "\n")
stopifnot(round(w_raw, 4) == 0.9735, round(w_log, 4) == 0.9988)
qq <- function(r, lab, w) {
  z <- (r - mean(r)) / sd(r); d <- tibble(th = qnorm(ppoints(length(z))), z = sort(z))
  ggplot(d, aes(th, z)) + geom_abline(slope = 1, intercept = 0, colour = "grey40", linetype = "dashed", linewidth = 0.5) +
    geom_point(size = 0.5, colour = "grey20", alpha = 0.6) +
    annotate("text", x = -2.9, y = 5.2, hjust = 0, vjust = 1, size = 3.2, family = "hira", label = sprintf("Shapiro-Wilk W = %.4f", w)) +
    coord_cartesian(xlim = c(-3, 3), ylim = c(-4, 5.5)) +
    labs(title = NULL, x = "正規分布の理論分位点", y = "標準化した残差の分位点") + ggtitle(lab) +
    theme(plot.title = element_text(family = "hira", size = 10, face = "plain"))
}
p <- (qq(resid(m_raw), "(a) 生の反応時間（ms）", w_raw) | qq(resid(m_log), "(b) 対数変換後", w_log))
save_fig(p, "ch11_rt_log", w = 5.8, h = 3.0)
