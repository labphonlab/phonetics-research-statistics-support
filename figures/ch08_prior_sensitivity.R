# 図8-2：事前分布の幅 x 話者数の感度分析。8.3節の表と同じ9回の当てはめを実際に行う
# （ノートブックのセルと同じ設定：adapt_delta=0.99, max_treedepth=12, seed=42）。
# 9回で10〜20分かかるので、結果を figures/ch08_prior_sensitivity.csv に保存し、あれば再利用する。
# 当てはめをやり直すときは、csvを消すか、環境変数 FORCE_REFIT=1 を付けて実行する。
source("figures/style.R"); source("figures/vot_data.R")
csv <- "figures/ch08_prior_sensitivity.csv"
if (!file.exists(csv) || nzchar(Sys.getenv("FORCE_REFIT"))) {
  suppressPackageStartupMessages(library(brms))
  res <- list()
  for (n_sp in c(3, 5, 10)) {
    sp <- levels(factor(vot_data$speaker))[1:n_sp]
    d <- droplevels(subset(vot_data, speaker %in% sp))
    for (s in c(2, 10, 30)) {
      f <- brm(VOT ~ condition + (1 + condition | speaker) + (1 | word), data = d,
        prior = c(set_prior("normal(50, 30)", class = "Intercept"),
                  set_prior(paste0("normal(0, ", s, ")"), class = "b"),
                  set_prior("exponential(0.1)", class = "sd")),
        chains = 4, iter = 2000, warmup = 1000, seed = 42, refresh = 0, silent = 2,
        control = list(adapt_delta = 0.99, max_treedepth = 12))
      fe <- fixef(f)["conditionvoiceless", ]
      res[[length(res) + 1]] <- data.frame(n_spk = n_sp, prior_sd = s,
        mean = round(fe["Estimate"], 2), lo = round(fe["Q2.5"], 1), hi = round(fe["Q97.5"], 1))
    }
  }
  write.csv(do.call(rbind, res), csv, row.names = FALSE)
}
d <- read.csv(csv)
print(d, row.names = FALSE)
d$prior <- factor(paste0("normal(0, ", d$prior_sd, ")"),
                  levels = c("normal(0, 2)", "normal(0, 10)", "normal(0, 30)"))
d$spk <- factor(d$n_spk)
p <- ggplot(d, aes(spk, mean, group = prior, shape = prior, fill = prior)) +
  geom_hline(yintercept = 52.5, colour = "grey30", linewidth = 0.5) +
  geom_linerange(aes(ymin = lo, ymax = hi), position = position_dodge(0.5), colour = "grey35", linewidth = 0.7) +
  geom_point(position = position_dodge(0.5), size = 3, colour = "black") +
  scale_shape_manual(values = c(21, 22, 24), name = "効果の事前分布") +
  scale_fill_manual(values = c("white", "grey60", "black"), name = "効果の事前分布") +
  scale_y_continuous(breaks = seq(-10, 80, 10)) +
  labs(x = "話者数（名）", y = "事後平均と95%信用区間（ms）")
save_fig(p, "ch08_prior_sensitivity", w = 5.6, h = 3.6)
