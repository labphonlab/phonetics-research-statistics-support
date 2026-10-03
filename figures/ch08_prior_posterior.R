# 図8-1：条件の効果（b_conditionvoiceless）の事前分布と事後分布の重ね描き。
# 本文8.3節の priors_new（intercept normal(50,30), b normal(0,30), sd exponential(0.1)）で当てはめる。
source("figures/style.R"); source("figures/vot_data.R")
suppressPackageStartupMessages(library(brms))
priors_new <- c(prior(normal(50, 30), class = "Intercept"),
                prior(normal(0, 30), class = "b"),
                prior(exponential(0.1), class = "sd"))
fit <- brm(VOT ~ condition + (1 + condition | speaker) + (1 | word),
           data = vot_data, family = gaussian(), prior = priors_new,
           chains = 4, iter = 2000, warmup = 1000, seed = 42,
           control = list(adapt_delta = 0.95), refresh = 0, silent = 2)
beta <- as_draws_df(fit)$b_conditionvoiceless
ci <- quantile(beta, c(.025, .975))
cat(sprintf("post mean %.2f  sd %.2f  95%%CI [%.2f, %.2f]\n", mean(beta), sd(beta), ci[1], ci[2]))
cat("max Rhat", max(brms::rhat(fit), na.rm = TRUE), " divergent", sum(subset(nuts_params(fit), Parameter == "divergent__")$Value), "\n")
x <- seq(-100, 100, length.out = 801)
dens <- density(beta, from = -100, to = 100, n = 801, bw = "SJ")
d <- rbind(data.frame(x = x, y = dnorm(x, 0, 30), dist = "事前分布 normal(0, 30)"),
           data.frame(x = dens$x, y = dens$y, dist = "事後分布"))
d$dist <- factor(d$dist, levels = c("事前分布 normal(0, 30)", "事後分布"))
band <- subset(d, dist == "事後分布" & x >= ci[1] & x <= ci[2])
p <- ggplot(d, aes(x, y)) +
  geom_area(data = band, fill = "grey70", alpha = 0.8) +
  geom_line(aes(linetype = dist, linewidth = dist, colour = dist)) +
  geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.3) +
  geom_vline(xintercept = mean(beta), colour = "grey15", linewidth = 0.4, linetype = "dotted") +
  scale_linetype_manual(values = c("dashed", "solid"), name = NULL) +
  scale_linewidth_manual(values = c(0.8, 1.2), name = NULL) +
  scale_colour_manual(values = c("grey45", "black"), name = NULL) +
  annotate("text", x = mean(beta), y = max(dens$y) * 1.08,
           label = sprintf("事後平均 %.2f\n95%%信用区間 [%.2f, %.2f]", mean(beta), ci[1], ci[2]),
           family = "hira", size = 3.2, vjust = 0) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  coord_cartesian(xlim = c(-60, 100)) +
  labs(x = "無声条件の効果（ms）", y = "確率密度")
save_fig(p, "ch08_prior_posterior", w = 5.6, h = 3.4)
