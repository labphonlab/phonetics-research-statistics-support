# 図7-1：F0軌跡の擬似データ（第7章、set.seed(1)）とGAMM。コードは本文・ノートブックと同一設定
source("figures/style.R")
suppressPackageStartupMessages({library(tidyverse); library(mgcv); library(itsadug)})
set.seed(1)
n_speaker <- 10; n_trial <- 15; time_points <- seq(0, 1, length.out = 20)
traj_data <- expand_grid(speaker = paste0("S", sprintf("%02d", 1:n_speaker)), trial = 1:n_trial, time = time_points) %>%
  mutate(condition = if_else(trial <= n_trial / 2, "rising", "falling"),
         speaker_offset = rnorm(n_speaker, 0, 5)[as.numeric(factor(speaker))],
         base = if_else(condition == "rising", 120 + 40 * time, 160 - 40 * time))
true_rho <- 0.7
ar_errors <- traj_data %>% group_by(speaker, trial) %>%
  group_modify(~ { e <- numeric(nrow(.x)); e[1] <- rnorm(1, 0, 3)
    for (t in 2:length(e)) e[t] <- true_rho * e[t - 1] + rnorm(1, 0, 3 * sqrt(1 - true_rho^2))
    tibble(ar_e = e) }) %>% ungroup() %>% pull(ar_e)
traj_data$F0 <- traj_data$base + traj_data$speaker_offset + ar_errors
traj_data <- traj_data %>% mutate(condition = factor(condition), speaker = factor(speaker))

model_gamm <- bam(F0 ~ condition + s(time, by = condition, k = 10) + s(time, speaker, bs = "fs", m = 1), data = traj_data)
print(round(summary(model_gamm)$p.table, 5)); print(round(summary(model_gamm)$s.table, 2))
# AR(1)（本文7.3と同じ手順）。start_event は素のdata.frameで
traj_data <- start_event(as.data.frame(traj_data), column = "time", event = c("speaker", "trial"), label.event = "Event")
traj_data$start.event <- traj_data$start.event
model_gamm <- bam(F0 ~ condition + s(time, by = condition, k = 10) + s(time, speaker, bs = "fs", m = 1), data = traj_data)
not_first <- !traj_data$start.event
r0 <- resid(model_gamm); rho_est <- cor(r0[not_first], r0[which(not_first) - 1]); cat("rho", rho_est, "\n")
model_ar <- bam(F0 ~ condition + s(time, by = condition, k = 10) + s(time, speaker, bs = "fs", m = 1),
                data = traj_data, rho = rho_est, AR.start = traj_data$start.event)
cat("sig2", model_gamm$sig2, model_ar$sig2, "\n")

# 話者の効果を除いた母集団レベルの曲線と95%帯
nd <- expand.grid(time = time_points, condition = levels(traj_data$condition), speaker = levels(traj_data$speaker)[1])
band <- function(m, lab) {
  pr <- predict(m, nd, exclude = "s(time,speaker)", se.fit = TRUE, newdata.guaranteed = TRUE)
  nd %>% mutate(fit = pr$fit, lo = fit - 1.96 * pr$se.fit, hi = fit + 1.96 * pr$se.fit, model = lab)
}
b0 <- band(model_gamm, "AR(1)なし"); b1 <- band(model_ar, "AR(1)あり")
print(b0 %>% group_by(condition) %>% summarise(w = mean(hi - lo)))
print(b1 %>% group_by(condition) %>% summarise(w = mean(hi - lo)))
cl <- c(falling = "下降調（falling）", rising = "上昇調（rising）")
obs <- traj_data %>% group_by(condition, time) %>% summarise(F0 = mean(F0), .groups = "drop")
tj <- traj_data %>% group_by(speaker, condition, time) %>% summarise(F0 = mean(F0), .groups = "drop")

p1 <- ggplot() +
  geom_line(data = tj, aes(time, F0, group = interaction(speaker, condition)), colour = "grey70", linewidth = 0.3) +
  geom_ribbon(data = b0, aes(time, ymin = lo, ymax = hi, fill = condition), alpha = 0.35) +
  geom_line(data = b0, aes(time, fit, linetype = condition), linewidth = 1.2, colour = "black") +
  geom_point(data = obs, aes(time, F0, shape = condition), size = 1.8, fill = "white", stroke = 0.6) +
  scale_fill_manual(values = GREYS, labels = cl, name = NULL) +
  scale_linetype_manual(values = c("solid", "longdash"), labels = cl, name = NULL) +
  scale_shape_manual(values = c(21, 24), labels = cl, name = NULL) +
  labs(x = "正規化時間（0=開始、1=終了）", y = "F0（Hz）")
save_fig(p1, "ch07_gamm_fit", w = 5.6, h = 3.6)
