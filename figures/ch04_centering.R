# 図4-2：F0の中心化前後で、切片が何を表すか（交互作用モデル、条件はsum coding）
source("figures/style.R"); source("figures/vot_data.R")
d <- vot_data %>% mutate(cs = C(condition, sum), F0c = F0 - mean(F0))
m_raw <- lm(VOT ~ cs * F0, d); m_cen <- lm(VOT ~ cs * F0c, d)
cr <- summary(m_raw)$coef; cc <- summary(m_cen)$coef
print(round(cr, 4)); print(round(cc, 4))
stopifnot(abs(cr[1,1]-53.8)<.05, abs(cr[1,2]-8.36)<.01, abs(cc[1,1]-49.1)<.05, abs(cc[1,2]-0.93)<.01,
          abs(cr[4,1]-0.00405)<5e-6, abs(cr[4,4]-0.9497)<5e-5, abs(summary(m_raw)$r.squared-0.8043)<5e-5)
mu <- mean(d$F0)
lines_df <- function(b, xs, shift) {   # b: 係数 (切片, cs, F0, cs:F0)
  rbind(data.frame(x = xs, y = (b[1]+b[2]) + (b[3]+b[4])*xs, g = "有声（+1）"),
        data.frame(x = xs, y = (b[1]-b[2]) + (b[3]-b[4])*xs, g = "無声（−1）"),
        data.frame(x = xs, y = b[1] + b[3]*xs, g = "2条件の平均（切片の線）"))
}
pan <- c("A　生のF0（切片はF0 = 0 Hzでの値）", "B　中心化したF0（切片は平均F0での値）")
xr <- seq(0, 170, length.out = 50); xc <- seq(0 - mu, 170 - mu, length.out = 50)
L <- rbind(transform(lines_df(coef(m_raw), xr), panel = pan[1], obs = FALSE),
           transform(lines_df(coef(m_cen), xc), panel = pan[2], obs = FALSE))
L$obs <- ifelse(L$panel == pan[1], L$x >= min(d$F0) & L$x <= max(d$F0), L$x >= min(d$F0c) & L$x <= max(d$F0c))
L$seg <- ifelse(L$panel == pan[1], ifelse(L$x < min(d$F0), "lo", ifelse(L$x > max(d$F0), "hi", "obs")),
                ifelse(L$x < min(d$F0c), "lo", ifelse(L$x > max(d$F0c), "hi", "obs")))
L$panel <- factor(L$panel, levels = pan)
pts <- rbind(data.frame(x = d$F0, y = d$VOT, panel = pan[1]), data.frame(x = d$F0c, y = d$VOT, panel = pan[2]))
pts$panel <- factor(pts$panel, levels = pan)
ic <- data.frame(panel = factor(pan, levels = pan), x = 0, y = c(cr[1,1], cc[1,1]), se = c(cr[1,2], cc[1,2]),
  lab = c(sprintf("切片 %.1f（SE %.2f）\nデータ範囲の外への外挿", cr[1,1], cr[1,2]),
          sprintf("切片 %.1f（SE %.2f）\n観測が密集する点の値", cc[1,1], cc[1,2])))
ic$lx <- c(6, -136); ic$ly <- c(cr[1,1] + 10, cc[1,1] + 16)
rng <- data.frame(panel = factor(pan, levels = pan), xmin = c(min(d$F0), min(d$F0c)), xmax = c(max(d$F0), max(d$F0c)))
p <- ggplot() +
  geom_point(data = pts, aes(x, y), colour = "grey70", size = 0.8) +
  geom_line(data = L, aes(x, y, linetype = g, linewidth = g, alpha = obs, group = interaction(g, seg, panel)), colour = "black") +
  geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.4) +
  geom_errorbar(data = ic, aes(x = x, ymin = y - 1.96*se, ymax = y + 1.96*se), width = 6, linewidth = 0.6) +
  geom_point(data = ic, aes(x, y), shape = 21, size = 3.2, fill = "white", stroke = 1.2) +
  geom_text(data = ic, aes(lx, ly, label = lab), hjust = 0, vjust = 0.5, size = 2.5, family = "hira", lineheight = 0.95) +
  facet_wrap(~panel, nrow = 1, scales = "free_x") +
  scale_linetype_manual(values = c("有声（+1）" = "dashed", "無声（−1）" = "dotted", "2条件の平均（切片の線）" = "solid"), name = NULL) +
  scale_linewidth_manual(values = c("有声（+1）" = 0.6, "無声（−1）" = 0.6, "2条件の平均（切片の線）" = 1.0), name = NULL) +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.45), guide = "none") +
  guides(linetype = guide_legend(nrow = 1), linewidth = guide_legend(nrow = 1)) +
  labs(x = "F0（A：Hz　B：平均129.8 Hzからのずれ）", y = "VOT（ms）") +
  theme(legend.key.width = unit(1.2, "cm"), legend.text = element_text(size = 8))
save_fig(p, "ch04_centering", w = 5.8, h = 3.6)
