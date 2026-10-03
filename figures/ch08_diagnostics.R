# 図8-3（トレースプロット）と図8-4（事後予測チェック）。8.4節のモデル（priors_new、seed=42、adapt_delta=0.95）。
# 図8-3の右は、反復を極端に短くした「悪い例」。同じモデルを iter=250, warmup=125 で当てはめる。
source("figures/style.R"); source("figures/vot_data.R")
suppressPackageStartupMessages(library(brms))
priors_new <- c(prior(normal(50, 30), class = "Intercept"),
                prior(normal(0, 30), class = "b"),
                prior(exponential(0.1), class = "sd"))
frm <- bf(VOT ~ condition + (1 + condition | speaker) + (1 | word))
fit <- brm(frm, data = vot_data, family = gaussian(), prior = priors_new,
           chains = 4, iter = 2000, warmup = 1000, seed = 42,
           control = list(adapt_delta = 0.95), refresh = 0, silent = 2)
short <- update(fit, iter = 250, warmup = 125, seed = 42, refresh = 0, silent = 2, recompile = FALSE)
cat("good: max Rhat", max(brms::rhat(fit), na.rm = TRUE), " min Bulk ESS", min(posterior::summarise_draws(fit)$ess_bulk, na.rm = TRUE), "\n")
cat("short: max Rhat", max(brms::rhat(short), na.rm = TRUE), " min Bulk ESS", min(posterior::summarise_draws(short)$ess_bulk, na.rm = TRUE), "\n")

par <- "b_conditionvoiceless"
tr <- function(m, label) {
  d <- as_draws_df(m); data.frame(it = d$.iteration, chain = factor(d$.chain), y = d[[par]], panel = label)
}
dd <- rbind(tr(fit, "本文のモデル\n（反復2000、ウォームアップ1000）"),
            tr(short, "反復を極端に短くした例\n（反復250、ウォームアップ125）"))
dd$panel <- factor(dd$panel, levels = unique(dd$panel))
p3 <- ggplot(dd, aes(it, y, linetype = chain, colour = chain)) +
  geom_line(linewidth = 0.35) +
  scale_colour_manual(values = c("grey10", "grey35", "grey55", "grey75"), name = "鎖") +
  scale_linetype_manual(values = c("solid", "longdash", "dotdash", "dotted"), name = "鎖") +
  facet_wrap(~ panel, scales = "free_x") +
  labs(x = "ウォームアップ後の反復", y = "無声条件の効果（ms）") +
  theme(strip.text = element_text(size = 8.5))
save_fig(p3, "ch08_trace", w = 6.2, h = 3.3)

yrep <- posterior_predict(fit, ndraws = 100, seed = 1)
dens_one <- function(v) { d <- density(v, from = -10, to = 130, n = 256); data.frame(x = d$x, y = d$y) }
rep_d <- do.call(rbind, lapply(seq_len(nrow(yrep)), function(i) cbind(dens_one(yrep[i, ]), id = i)))
obs_d <- dens_one(vot_data$VOT)
p4 <- ggplot() +
  geom_line(data = rep_d, aes(x, y, group = id), colour = "grey70", linewidth = 0.25) +
  geom_line(data = obs_d, aes(x, y), colour = "black", linewidth = 1.1) +
  annotate("text", x = 62, y = 0.0165, label = "実データ（太線）", family = "hira", size = 3.2, hjust = 0) +
  annotate("text", x = 62, y = 0.0145, label = "事後予測（灰色の細線、100本）", family = "hira", size = 3.2, hjust = 0, colour = "grey40") +
  labs(x = "VOT（ms）", y = "確率密度")
save_fig(p4, "ch08_ppcheck", w = 5.4, h = 3.3)
