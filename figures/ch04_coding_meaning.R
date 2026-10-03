# 図4-1：3水準の頻度帯で、treatment / sum / Helmert の係数が「どの平均とどの平均の差か」を示す
source("figures/style.R"); source("figures/vot_data.R")
word_number <- as.integer(sub("word_", "", as.character(vot_data$word)))
pair_id <- ceiling(word_number / 2)
band <- c("low", "mid", "high")[((pair_id - 1) %% 3) + 1]
vf <- vot_data %>% mutate(freq = factor(band, levels = c("low", "mid", "high")),
  VOT = VOT + case_when(freq == "low" ~ 0, freq == "mid" ~ -6, freq == "high" ~ -12))
m <- tapply(vf$VOT, vf$freq, mean); gm <- mean(m)
cf <- list(tr = coef(lm(VOT ~ freq, vf)), su = coef(lm(VOT ~ C(freq, sum), vf)),
           he = coef(lm(VOT ~ C(freq, contr.helmert), vf)))
print(round(m, 3)); print(round(gm, 3)); print(lapply(cf, round, 3))
stopifnot(abs(cf$tr[1]-47.073)<5e-4, abs(cf$tr[2]+3.139)<5e-4, abs(cf$tr[3]+8.046)<5e-4,
          abs(cf$su[1]-43.344)<5e-4, abs(cf$su[2]-3.729)<5e-4, abs(cf$su[3]-0.589)<5e-4,
          abs(cf$he[2]+1.570)<5e-4, abs(cf$he[3]+2.159)<5e-4)
lv <- c("low", "mid", "high"); xs <- c(low = 1, mid = 2, high = 3)
fmt <- function(x) sprintf("%.3f", x) |> sub("-", "−", x = _)
mp <- data.frame(x = 1:3, y = as.numeric(m))
panels <- c("treatment coding", "sum coding", "Helmert coding")
mp <- do.call(rbind, lapply(panels, function(p) transform(mp, panel = p)))
mp$panel <- factor(mp$panel, levels = panels)
# 基準線（点線）：treatmentは低の平均、sumとHelmertは3水準平均の平均
ref <- data.frame(panel = factor(panels, levels = panels), y = c(m[["low"]], gm, gm))
P <- function(i) factor(panels[i], levels = panels)
mid_low <- (m[["low"]] + m[["mid"]]) / 2
# 係数を表す矢印（基準 → 対象）と、そのラベルの位置（hj=0で右、1で左に伸びる）
ar <- data.frame(
  panel = c(P(1), P(1), P(2), P(2), P(3), P(3)),
  x = c(2, 3, 1, 2, 2, 3), xend = c(2, 3, 1, 2, 2, 3),
  y = c(m[["low"]], m[["low"]], gm, gm, m[["low"]], mid_low),
  yend = c(m[["mid"]], m[["high"]], m[["low"]], m[["mid"]], m[["mid"]], m[["high"]]),
  lab = c(paste0("係数1\n", fmt(cf$tr[2])), paste0("係数2\n", fmt(cf$tr[3])),
          paste0("係数1\n", fmt(cf$su[2])), paste0("係数2\n", fmt(cf$su[3])),
          paste0("差 ", fmt(m[["mid"]]-m[["low"]]), "\n÷2 = 係数1\n", fmt(cf$he[2])),
          paste0("差 ", fmt(m[["high"]]-mid_low), "\n÷3 = 係数2\n", fmt(cf$he[3]))),
  xl = c(1.9, 2.9, 1.1, 2.15, 1.9, 2.9),
  yl = c(45.2, 42.5, 45.2, 44.7, 45.8, 41.8),
  hj = c(1, 1, 0, 0, 1, 1))
txt <- data.frame(panel = c(P(1), P(2), P(3)), x = c(1.25, 0.5, 0.5), y = c(m[["low"]], gm, gm),
  vj = c(-0.5, 1.3, 1.3),
  lab = c(paste0("切片 ", fmt(cf$tr[1]), "（低の平均）"), paste0("切片 ", fmt(cf$su[1]), "\n（平均の平均）"),
          paste0("切片 ", fmt(cf$he[1]), "\n（平均の平均）")))
p <- ggplot() +
  geom_hline(data = ref, aes(yintercept = y), linetype = "dotted", linewidth = 0.6) +
  geom_segment(data = ar, aes(x = x, xend = xend, y = y, yend = yend - sign(yend - y) * 0.45), linewidth = 0.7,
               arrow = arrow(length = unit(0.12, "cm"), type = "closed")) +
  geom_point(data = mp, aes(x, y), shape = 21, size = 3.0, fill = "white", stroke = 1.1) +
  geom_point(data = data.frame(panel = P(3), x = 3, y = mid_low), aes(x, y), shape = 4, size = 2.6, stroke = 1) +
  geom_text(data = ar, aes(xl, yl, label = lab, hjust = hj), size = 2.4, family = "hira", lineheight = 0.95) +
  geom_text(data = txt, aes(x, y, label = lab, vjust = vj), size = 2.4, family = "hira", hjust = 0, lineheight = 0.95) +
  facet_wrap(~panel, nrow = 1) +
  scale_x_continuous(breaks = 1:3, labels = c("低", "中", "高"), limits = c(0.45, 3.5)) +
  scale_y_continuous(limits = c(37, 49)) +
  labs(x = "頻度帯", y = "VOTの水準平均（ms）")
save_fig(p, "ch04_coding_meaning", w = 5.8, h = 3.6)
