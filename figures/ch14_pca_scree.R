# 図14-2：PCAのスクリープロットとPC1–PC2平面の母音重心。本文14.3節（pb52、話者内zスコア）と同一。
suppressPackageStartupMessages({library(tidyverse); library(phonTools); library(patchwork)})
source("figures/style.R")
data(pb52)
pb <- pb52 %>% group_by(speaker) %>%
  mutate(f1z = as.numeric(scale(f1)), f2z = as.numeric(scale(f2)), f3z = as.numeric(scale(f3))) %>% ungroup()
pc <- prcomp(pb[, c("f1z", "f2z", "f3z")], scale. = FALSE)
print(summary(pc)); print(round(pc$rotation, 3))
pv <- pc$sdev^2 / sum(pc$sdev^2)
stopifnot(round(cumsum(pv), 4)[1:2] == c(0.5356, 0.8355), round(cumsum(pv)[2], 3) == 0.836)
sc <- tibble(vowel = pb$vowel, PC1 = pc$x[, 1], PC2 = pc$x[, 2]) %>% group_by(vowel) %>% summarise(PC1 = mean(PC1), PC2 = mean(PC2), .groups = "drop")
vm <- pb %>% group_by(vowel) %>% summarise(f1z = mean(f1z), f2z = mean(f2z), .groups = "drop")
mds <- cmdscale(dist(vm[, c("f1z", "f2z")]), k = 2)
stopifnot(round(cor(mds[, 1], vm$f2z), 3) == 0.834, round(cor(mds[, 2], vm$f1z), 3) == 0.652)
sd_ <- tibble(pc = paste0("PC", 1:3), v = pv, cum = cumsum(pv))
pa <- ggplot(sd_, aes(pc)) +
  geom_col(aes(y = v), fill = "grey75", colour = "grey15", linewidth = 0.3, width = 0.7) +
  geom_line(aes(y = cum, group = 1), linetype = "dashed", linewidth = 0.6) +
  geom_point(aes(y = cum), size = 2.4, shape = 21, fill = "white", stroke = 0.9) +
  geom_text(data = sd_[2:3, ], aes(y = cum, label = sprintf("%.1f%%", cum * 100)), vjust = -0.9, size = 3, family = "hira") +
  geom_text(aes(y = v / 2, label = sprintf("%.1f%%", v * 100)), vjust = 0.5, size = 2.7, family = "hira") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1.1), breaks = seq(0, 1, .25), expand = expansion(mult = c(0, 0))) +
  labs(x = NULL, y = "分散の割合") + ggtitle("(a) 寄与率（棒）と累積（破線）") +
  theme(plot.title = element_text(family = "hira", size = 9, face = "plain"))
pb_ <- ggplot(sc, aes(PC1, PC2, label = vowel)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) + geom_vline(xintercept = 0, colour = "grey70", linewidth = 0.3) +
  geom_point(size = 1.8, shape = 21, fill = "grey40") +
  geom_text(vjust = -0.8, size = 3.2, family = "hira") +
  scale_x_continuous(expand = expansion(mult = 0.12)) + scale_y_continuous(expand = expansion(mult = 0.12)) +
  labs(x = sprintf("PC1（%.1f%%）", pv[1] * 100), y = sprintf("PC2（%.1f%%）", pv[2] * 100)) +
  ggtitle("(b) 10母音の重心（PC1–PC2平面）") + theme(plot.title = element_text(family = "hira", size = 9, face = "plain"))
save_fig(pa + pb_ + plot_layout(widths = c(1, 1.15)), "ch14_pca_scree", w = 5.8, h = 3.1)
