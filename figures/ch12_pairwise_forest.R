# 図12-1：調音位置の対比較（labial/alveolar/velar）の信頼区間。調整なし・Tukey・Bonferroni。
# データ・モデルは第12章のコードと同一（seed 31）。
suppressPackageStartupMessages({library(tidyverse); library(lme4); library(lmerTest); library(emmeans)})
source("figures/style.R")
set.seed(31)
n_speaker <- 16; n_word <- 30
places <- c("labial", "alveolar", "velar")
vot_place <- expand_grid(speaker = sprintf("S%02d", 1:n_speaker), word = sprintf("W%02d", 1:n_word)) %>%
  mutate(place = factor(places[((as.integer(sub("W", "", word)) - 1) %% 3) + 1], levels = places),
         si = as.integer(factor(speaker)), wi = as.integer(factor(word)),
         VOT = c(labial = 55, alveolar = 58.2, velar = 75)[as.character(place)] +
           rnorm(n_speaker, 0, 8)[si] + rnorm(n_word, 0, 4)[wi] + rnorm(n(), 0, 7)) %>%
  select(-si, -wi)
model_place <- lmer(VOT ~ place + (1 | speaker) + (1 | word), data = vot_place)
em <- emmeans(model_place, ~ place)
adj <- c("なし" = "none", "Tukey" = "tukey", "Bonferroni" = "bonferroni")
res <- imap_dfr(adj, function(a, nm) {
  pr <- pairs(em, adjust = a)
  ci <- as.data.frame(confint(pr)); pv <- as.data.frame(pr)
  tibble(contrast = ci$contrast, est = ci$estimate, lo = ci$lower.CL, hi = ci$upper.CL, p = pv$p.value, method = nm)
})
print(res %>% select(method, contrast, est, lo, hi, p), n = 20)
stopifnot(abs(res$p[res$method == "なし"][1] - 0.0241) < 5e-4,
          abs(res$p[res$method == "Tukey"][1] - 0.0605) < 5e-4,
          abs(res$p[res$method == "Bonferroni"][1] - 0.0723) < 5e-4)
lab <- c("labial - alveolar" = "labial − alveolar", "labial - velar" = "labial − velar", "alveolar - velar" = "alveolar − velar")
res <- res %>% mutate(contrast = factor(lab[as.character(contrast)], levels = rev(lab)),
                      method = factor(method, levels = names(adj)),
                      plab = ifelse(p < 0.0001, "p<.0001", sprintf("p=%.4f", p)))
dodge <- position_dodge(width = 0.6)
sub <- res %>% filter(contrast == "labial − alveolar")
p <- ggplot(res, aes(est, contrast, shape = method, linetype = method, fill = method)) +
  geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.5) +
  geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = 0.18, position = dodge, linewidth = 0.6) +
  geom_point(position = dodge, size = 2.8, stroke = 0.9) +
  geom_text(data = sub, aes(x = 1.2, label = plab), position = dodge, hjust = 0, size = 3, family = "hira",
            show.legend = FALSE) +
  scale_shape_manual(values = c(21, 24, 22), name = "調整") +
  scale_fill_manual(values = c("white", "grey45", "black"), name = "調整") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = "調整") +
  scale_x_continuous(limits = c(-22.5, 7), breaks = seq(-20, 0, 5)) +
  labs(x = "VOTの差（ms）と95%信頼区間", y = NULL)
save_fig(p, "ch12_pairwise_forest", w = 5.6, h = 3.4)
