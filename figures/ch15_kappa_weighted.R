# 第15章 図15-1：通常のkappaと重み付きkappa（15.2節・notebook ch15 と同一のデータ生成、seed 15）
suppressPackageStartupMessages(library(irr))
source("figures/style.R")
set.seed(15)
n_item <- 60
truth <- rnorm(n_item, 0, 1)
make_rating <- function(bias, noise) {
  as.integer(cut(truth + bias + rnorm(n_item, 0, noise), breaks = c(-Inf, -1, 0, 1, Inf), labels = 1:4))
}
ratings <- data.frame(r1 = make_rating(0, 0.4), r2 = make_rating(0, 0.4), r3 = make_rating(0.6, 0.4))
k_plain <- kappa2(ratings[, c("r1","r3")])$value
k_w <- kappa2(ratings[, c("r1","r3")], weight = "squared")$value
cat(sprintf("kappa r1-r2 %.3f, r1-r3 %.3f, weighted r1-r3 %.3f, agree r1-r2 %.3f\n",
            kappa2(ratings[, c("r1","r2")])$value, k_plain, k_w, mean(ratings$r1 == ratings$r2)))
tb <- as.data.frame(table(r1 = ratings$r1, r3 = ratings$r3))
tb$r1 <- as.integer(as.character(tb$r1)); tb$r3 <- as.integer(as.character(tb$r3))
tb$dist <- abs(tb$r1 - tb$r3)
tb$distf <- factor(tb$dist, levels = 0:3, labels = c("一致", "1段差", "2段差", "3段差"))
cat("一致:", sum(tb$Freq[tb$dist == 0]), " 1段差:", sum(tb$Freq[tb$dist == 1]), " 2段以上:", sum(tb$Freq[tb$dist >= 2]), "\n")
p <- ggplot(tb, aes(r3, r1)) +
  geom_tile(aes(fill = distf), colour = "white", linewidth = 0.8) +
  geom_text(aes(label = ifelse(Freq > 0, Freq, ""), colour = distf == "一致"), family = "hira", size = 4, show.legend = FALSE) +
  scale_colour_manual(values = c("TRUE" = "white", "FALSE" = "black")) +
  scale_fill_manual(values = c("一致" = "grey20", "1段差" = "grey65", "2段差" = "grey85", "3段差" = "grey97"), name = "評定のずれ") +
  scale_x_continuous(breaks = 1:4) + scale_y_reverse(breaks = 1:4) +
  labs(x = "評定者3（甘め）の評定", y = "評定者1の評定") +
  annotate("text", x = 4.75, y = 2.5, hjust = 0, vjust = 0.5, size = 3.2, family = "hira",
           label = sprintf("通常のkappa\n%.3f\n\n重み付きkappa\n（二乗の重み）\n%.3f", k_plain, k_w)) +
  coord_cartesian(xlim = c(0.5, 6.4), clip = "off") +
  theme(legend.position = "bottom", panel.grid = element_blank(), plot.margin = margin(5, 5, 5, 5))
save_fig(p, "ch15_kappa_weighted", w = 5.4, h = 3.8)
