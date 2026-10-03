# 図：話者ごとのランダム効果（本文5.3、lmer(VOT ~ condition + (1 + condition | speaker) + (1 | word))）
source("figures/style.R"); source("figures/vot_data.R")
suppressPackageStartupMessages({library(lme4); library(lmerTest)})
m <- lmer(VOT ~ condition + (1 + condition | speaker) + (1 | word), data = vot_data)
vc <- as.data.frame(VarCorr(m)); print(vc[, c("grp","var1","sdcor")])
cat("fixef:", round(fixef(m), 3), "\n")
re <- ranef(m, condVar = TRUE)$speaker
pv <- attr(re, "postVar")
d <- do.call(rbind, lapply(1:2, function(j) data.frame(
  speaker = rownames(re), est = re[, j], se = sqrt(pv[j, j, ]),
  term = c("話者の切片（有声条件の基準値のずれ）", "話者の傾き（条件の効果の大きさのずれ）")[j])))
d$term <- factor(d$term, levels = unique(d$term))
sdl <- data.frame(term = levels(d$term), sd = c(4.034, 16.682))
cat("SD of ranef (slope):", sd(re[, 2]), " sd(intercept):", sd(re[, 1]), "\n")
d <- d %>% group_by(term) %>% mutate(speaker = factor(speaker, levels = speaker[order(est)])) %>% ungroup()
# 並べ替えはfacetごとに行うため、順位ラベルを使う
d <- d %>% group_by(term) %>% arrange(est, .by_group = TRUE) %>% mutate(rank = row_number()) %>% ungroup()
p <- ggplot(d, aes(est, rank)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey30") +
  geom_linerange(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se), linewidth = 0.5, colour = "grey40") +
  geom_point(shape = 21, size = 2.6, fill = "white", stroke = 0.9) +
  geom_text(aes(label = speaker, x = -Inf), hjust = -0.15, size = 2.5, family = "hira", colour = "grey30") +
  geom_text(data = sdl, aes(x = Inf, y = 0.6, label = sprintf("SD = %.2f", sd)), hjust = 1.1, size = 3, family = "hira", inherit.aes = FALSE) +
  facet_wrap(~term, scales = "free_x") +
  scale_y_continuous(breaks = NULL, expand = expansion(mult = c(0.06, 0.04))) +
  scale_x_continuous(expand = expansion(mult = c(0.13, 0.05))) +
  labs(x = "固定効果からのずれ（ms）。点＝予測値、線＝95%区間", y = "話者（ずれの小さい順）") +
  theme(panel.grid.major.y = element_blank())
save_fig(p, "ch05_caterpillar", w = 5.8, h = 3.6)
