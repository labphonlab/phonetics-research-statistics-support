source("figures/style.R"); source("figures/vot_data.R")
m <- lm(VOT ~ condition, data = vot_data)
d <- vot_data %>% mutate(fit = fitted(m), res = resid(m))
sds <- d %>% group_by(condition) %>% summarise(fit = mean(fit), sd = sd(res), .groups = "drop")
print(sds)
set.seed(1)
p <- ggplot(d, aes(fit, res)) +
  geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.4) +
  geom_point(aes(shape = condition, fill = condition), position = position_jitter(width = 2.5, height = 0),
             size = 1.9, colour = "grey15", alpha = 0.85) +
  geom_text(data = sds, inherit.aes = FALSE, aes(x = fit, y = 38, label = sprintf("残差SD %.1f ms", sd)), family = "hira", size = 3.6) +
  scale_x_continuous(limits = c(0, 100), breaks = round(sds$fit, 1), labels = function(x) sprintf("%.1f", x)) +
  scale_shape_manual(values = c(voiced = 21, voiceless = 24), labels = c("有声条件", "無声条件"), name = NULL) +
  scale_fill_manual(values = c(voiced = "grey70", voiceless = "grey25"), labels = c("有声条件", "無声条件"), name = NULL) +
  coord_cartesian(ylim = c(-42, 40)) +
  labs(x = "予測値（ms）", y = "残差（ms）")
save_fig(p, "ch03_resid_fitted", w = 5.4, h = 3.5)
