# 図5-2：S02の観測数を減らしたときの部分プーリング（本文 5.3「概念をつかむ：部分プーリング」の表と同一）
source("figures/style.R"); source("figures/vot_data.R")
suppressPackageStartupMessages({library(lme4); library(lmerTest)})
fit_thin <- function(keep) {
  thin <- vot_data %>% group_by(speaker, condition) %>%
    filter(speaker != "S02" | is.na(keep) | row_number() <= keep) %>% ungroup()
  m <- lmer(VOT ~ condition + (1 + condition | speaker) + (1 | word), data = thin)
  raw <- thin %>% group_by(speaker, condition) %>% summarise(m = mean(VOT), n = n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = condition, values_from = c(m, n)) %>%
    mutate(raw = m_voiceless - m_voiced, n = n_voiced + n_voiceless) %>% select(speaker, n, raw)
  est <- coef(m)$speaker %>% tibble::rownames_to_column("speaker") %>% transmute(speaker, model = conditionvoiceless)
  s <- left_join(raw, est, by = "speaker") %>% filter(speaker == "S02")
  data.frame(n = s$n, raw = s$raw, model = s$model, grand = unname(fixef(m)["conditionvoiceless"]))
}
res <- do.call(rbind, lapply(list(NA, 4, 1), fit_thin))
print(round(res, 2))
res$lab <- factor(sprintf("S02の観測 %d件", res$n), levels = sprintf("S02の観測 %d件", res$n))
res$pct <- 100 * abs(res$model - res$raw) / abs(res$raw - res$grand)
p <- ggplot(res) +
  geom_vline(aes(xintercept = grand), linetype = "dashed", colour = "grey30") +
  geom_segment(aes(x = raw, xend = model, y = 1, yend = 1), linewidth = 0.9, colour = "black",
               arrow = arrow(length = unit(0.12, "in"), type = "closed")) +
  geom_point(aes(raw, 1), shape = 21, size = 3.6, fill = "white", stroke = 1.3) +
  geom_text(aes(raw, 1.28, label = "個別推定"), size = 3, family = "hira") +
  geom_text(aes(grand, 0.55, label = sprintf("全体平均 %.1f", grand)), size = 3, family = "hira", hjust = 0.5) +
  geom_text(aes(model, 0.72, label = sprintf("モデル推定 %.1f", model)), size = 3, family = "hira") +
  geom_text(aes(70, 1.62, label = sprintf("全体平均側へ %.0f%% 移動", pct)), size = 3.1, family = "hira") +
  facet_wrap(~lab, ncol = 1) + coord_cartesian(ylim = c(0.4, 1.8), xlim = c(45, 90)) +
  labs(x = "S02の条件効果（無声−有声、ms）", y = NULL) +
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
save_fig(p, "ch05_partial_pooling", w = 5.6, h = 4.2)
