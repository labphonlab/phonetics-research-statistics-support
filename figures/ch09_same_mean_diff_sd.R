# 図9-1：平均が同じでも分布は違う（9.2節の人工データ。set.seed(2026)、各条件60観測）。
source("figures/style.R")
set.seed(2026)
d <- data.frame(cond = factor(rep(c("A", "B"), each = 60)),
                y = c(rnorm(60, 50, 5), rnorm(60, 50, 15)))
s <- aggregate(y ~ cond, d, function(v) c(m = mean(v), s = sd(v)))
s <- data.frame(cond = s$cond, m = s$y[, "m"], s = s$y[, "s"]); print(s)
print(t.test(y ~ cond, data = d, var.equal = TRUE)$p.value)
s$lab <- sprintf("平均 %.1f、SD %.1f", s$m, s$s)
lv <- c(A = "条件A", B = "条件B"); d$c2 <- lv[as.character(d$cond)]; s$c2 <- lv[as.character(s$cond)]
set.seed(1)
p <- ggplot(d, aes(y, c2)) +
  geom_jitter(aes(shape = c2), height = 0.12, size = 1.6, colour = "grey40", alpha = 0.8) +
  geom_errorbar(data = s, aes(xmin = m - s, xmax = m + s, y = c2), inherit.aes = FALSE,
                width = 0.25, linewidth = 0.9, colour = "black") +
  geom_point(data = s, aes(m, c2), inherit.aes = FALSE, shape = 21, fill = "white", size = 3.8, stroke = 1.3) +
  geom_text(data = s, aes(x = m, y = c2, label = lab), inherit.aes = FALSE, nudge_y = 0.33,
            family = "hira", size = 3.2) +
  scale_shape_manual(values = c(16, 17), guide = "none") +
  scale_y_discrete(expand = expansion(add = c(0.5, 0.65))) +
  labs(x = "測定値（任意単位）", y = NULL)
save_fig(p, "ch09_same_mean_diff_sd", w = 5.6, h = 3.0)
