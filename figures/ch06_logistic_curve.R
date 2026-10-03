# 図6-1：ロジスティック曲線（線形予測子→確率）。本文 6.2〜6.3 の係数 0.9518・-0.8136 を使う
source("figures/style.R")
b0 <- 0.9518; b1 <- -0.8136
cur <- data.frame(x = seq(-4, 4, length.out = 400)); cur$p <- plogis(cur$x)
pts <- data.frame(cond = factor(c("有声条件", "無声条件"), levels = c("有声条件", "無声条件")), x = c(b0, b0 + b1))
pts$p <- plogis(pts$x); print(pts)
p <- ggplot(cur, aes(x, p)) +
  geom_hline(yintercept = c(0, 1), colour = "grey60", linewidth = 0.3) +
  geom_line(linewidth = 1.1, colour = "grey15") +
  geom_segment(data = pts, aes(x = x, xend = x, y = 0, yend = p), linetype = c("solid", "dashed"), colour = "grey30") +
  geom_segment(data = pts, aes(x = -4, xend = x, y = p, yend = p), linetype = c("solid", "dashed"), colour = "grey30") +
  geom_point(data = pts, aes(shape = cond, fill = cond), size = 4.2, stroke = 1.2) +
  scale_shape_manual(values = c(24, 21), name = NULL) +
  scale_fill_manual(values = c("white", "grey15"), name = NULL) +
  annotate("text", x = b0 + b1 - 0.08, y = 0.15, label = sprintf("無声\nログオッズ %.4f\n確率 %.4f", b0 + b1, plogis(b0 + b1)),
           hjust = 1, size = 3, family = "hira", lineheight = 0.95) +
  annotate("text", x = b0 + 0.08, y = 0.15, label = sprintf("有声\nログオッズ %.4f\n確率 %.4f", b0, plogis(b0)),
           hjust = 0, size = 3, family = "hira", lineheight = 0.95) +
  scale_x_continuous(breaks = -4:4, expand = expansion(add = c(0, 0.1))) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) +
  labs(x = "線形予測子（ログオッズ）", y = "確率")
save_fig(p, "ch06_logistic_curve", w = 5.4, h = 3.5)

# 図6-2：条件付き確率と周辺確率（本文 6.3「条件付き確率と周辺確率」）
set.seed(1)
mk <- function(s, lab) {
  z <- rnorm(1e6, b0, s)
  list(lab = lab, s = s, cond = plogis(b0), marg = mean(plogis(z)))
}
r <- list(mk(0.6096, "話者SD = 0.6096（章の推定値）"), mk(3, "話者SD = 3（大きなばらつき）"))
for (q in r) cat(q$lab, round(q$cond, 4), round(q$marg, 4), "\n")
cur2 <- do.call(rbind, lapply(r, function(q) data.frame(lab = q$lab, x = seq(-6, 8, length.out = 400))))
cur2$p <- plogis(cur2$x); cur2$lab <- factor(cur2$lab, levels = sapply(r, `[[`, "lab"))
sp <- do.call(rbind, lapply(r, function(q) data.frame(lab = q$lab, x = b0 + c(-1, 1) * q$s,
        marg = q$marg, cond = q$cond)))
sp$p <- plogis(sp$x); sp$lab <- factor(sp$lab, levels = levels(cur2$lab))
mm <- do.call(rbind, lapply(r, function(q) data.frame(lab = q$lab, cond = q$cond, marg = q$marg)))
mm$lab <- factor(mm$lab, levels = levels(cur2$lab))
p2 <- ggplot(cur2, aes(x, p)) +
  geom_line(linewidth = 1, colour = "grey15") +
  geom_vline(xintercept = b0, colour = "grey60", linewidth = 0.3) +
  geom_hline(data = mm, aes(yintercept = cond), linetype = "solid", colour = "grey55", linewidth = 0.6) +
  geom_hline(data = mm, aes(yintercept = marg), linetype = "dashed", colour = "black", linewidth = 0.7) +
  geom_point(data = sp, aes(y = p), shape = 21, fill = "white", size = 3, stroke = 1.1) +
  geom_point(data = mm, aes(x = b0, y = cond), shape = 22, fill = "grey55", size = 3.2) +
  geom_text(data = mm, aes(x = -5.9, y = cond + 0.045, label = sprintf("条件付き %.4f", cond)), hjust = 0, size = 2.9, family = "hira", colour = "grey35") +
  geom_text(data = mm, aes(x = -5.9, y = marg - 0.05, label = sprintf("周辺 %.4f", marg)), hjust = 0, size = 2.9, family = "hira") +
  facet_wrap(~lab) + scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(x = "線形予測子（ログオッズ）", y = "確率")
save_fig(p2, "ch06_marginal_vs_conditional", w = 5.8, h = 3.3)
