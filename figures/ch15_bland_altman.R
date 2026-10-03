# 第15章 図15-1：Bland-Altmanプロット（本文15.3節・notebook ch15 と同一のデータ生成・seed 15）
source("figures/style.R")
set.seed(15)
manual <- rnorm(80, 60, 15)
auto <- manual + 4 + rnorm(80, 0, 6)
diff <- auto - manual; avg <- (auto + manual) / 2
bias <- mean(diff); sd_diff <- sd(diff)
lo <- bias - 1.96 * sd_diff; hi <- bias + 1.96 * sd_diff
cat(sprintf("bias %.2f sd %.2f LoA [%.2f, %.2f] r %.3f outside %d/80\n", bias, sd_diff, lo, hi, cor(manual, auto), sum(diff < lo | diff > hi)))
df <- data.frame(avg, diff)
xr <- range(avg)
p <- ggplot(df, aes(avg, diff)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4, linetype = "dotted") +
  geom_hline(yintercept = bias, colour = "black", linewidth = 0.9) +
  geom_hline(yintercept = c(lo, hi), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_point(shape = 21, fill = "grey60", colour = "grey20", size = 1.9, alpha = 0.85) +
  annotate("text", x = xr[2] + 2, y = bias, hjust = 0, size = 3.1, family = "hira", label = sprintf("平均バイアス\n%.2f ms", bias)) +
  annotate("text", x = xr[2] + 2, y = hi, hjust = 0, size = 3.1, family = "hira", label = sprintf("上側の限界\n%.2f ms", hi)) +
  annotate("text", x = xr[2] + 2, y = lo, hjust = 0, size = 3.1, family = "hira", label = sprintf("下側の限界\n%.2f ms", lo)) +
  scale_x_continuous(limits = c(30, 130), breaks = seq(40, 100, 20)) +
  labs(x = "2つの測定の平均（ms）", y = "差：自動 − 手動（ms）")
save_fig(p, "ch15_bland_altman", w = 5.4, h = 3.5)
