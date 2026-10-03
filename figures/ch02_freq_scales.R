# 図2-3：周波数尺度の変換曲線（表2-2の定義）。低周波ほど細かく、高周波ほど粗くなる
source("figures/style.R")
suppressPackageStartupMessages({library(phonTools); library(dplyr)})
data(pb52)
bark <- function(f) 26.81 / (1 + 1960/f) - 0.53
erb  <- function(f) 21.4 * log10(1 + 0.00437*f)
mel  <- function(f) 2595 * log10(1 + f/700)
st   <- function(f, ref = 100) 12 * log2(f / ref)
# 本文2.6節の検算（Peterson & Barney 1952 の全話者平均F1）
mm <- pb52 %>% group_by(vowel) %>% summarise(F1 = mean(f1), .groups = "drop")
g <- function(v) mm$F1[mm$vowel == v]
cat(sprintf("i-I: Hz差 %.1f Bark差 %.2f Bark/Hz %.5f\n", g("I")-g("i"), bark(g("I"))-bark(g("i")), (bark(g("I"))-bark(g("i")))/(g("I")-g("i"))))
cat(sprintf("V-A: Hz差 %.1f Bark差 %.2f Bark/Hz %.5f\n", g("A")-g("V"), bark(g("A"))-bark(g("V")), (bark(g("A"))-bark(g("V")))/(g("A")-g("V"))))
stopifnot(round(g("I")-g("i"),1)==136.9, round(bark(g("I"))-bark(g("i")),2)==1.33,
          round(g("A")-g("V"),1)==111.6, round(bark(g("A"))-bark(g("V")),2)==0.78)
cat(sprintf("ST: %.6f %.6f\n", st(110)-st(100), st(220)-st(200)))
# パネルA：4つの尺度を、100〜5000 Hzの範囲で0〜1に規格化して形だけ比べる
fb <- seq(100, 5000, length.out = 400)
nz <- function(y) (y - y[1]) / (y[length(y)] - y[1])
pB <- rbind(data.frame(f = fb, y = nz(bark(fb)), s = "Bark"), data.frame(f = fb, y = nz(erb(fb)), s = "ERB"),
            data.frame(f = fb, y = nz(mel(fb)), s = "Mel"), data.frame(f = fb, y = nz(st(fb)), s = "セミトーン"),
            data.frame(f = fb, y = (fb - 100) / 4900, s = "ヘルツ（線形）"))
pB$s <- factor(pB$s, levels = c("ヘルツ（線形）", "Bark", "ERB", "Mel", "セミトーン"))
A <- ggplot(pB, aes(f, y, linetype = s, linewidth = s, colour = s)) + geom_line() +
  scale_linetype_manual(values = c("ヘルツ（線形）" = "dotted", Bark = "solid", ERB = "longdash", Mel = "dashed", "セミトーン" = "dotdash"), name = NULL) +
  scale_linewidth_manual(values = c("ヘルツ（線形）" = 0.6, Bark = 0.8, ERB = 0.8, Mel = 0.8, "セミトーン" = 0.8), name = NULL) +
  scale_colour_manual(values = c("ヘルツ（線形）" = "grey50", Bark = "black", ERB = "grey15", Mel = "grey35", "セミトーン" = "grey25"), name = NULL) +
  guides(linetype = guide_legend(ncol = 1), colour = guide_legend(ncol = 1), linewidth = guide_legend(ncol = 1)) +
  theme(legend.position = "inside", legend.position.inside = c(0.72, 0.28), legend.key.width = unit(0.9, "cm"), legend.text = element_text(size = 7.5), legend.key.height = unit(0.4, "cm")) +
  labs(x = "周波数（Hz）", y = "変換後の値（0〜1に規格化）") +
  scale_x_continuous(breaks = seq(0, 5000, 1000))
# パネルB：本文のF1の2対で、1 Hzあたりの変化量（Bark/Hz）を比べる
pr <- data.frame(f1 = c(g("i"), g("V")), f2 = c(g("I"), g("A")))
pr$rate <- (bark(pr$f2) - bark(pr$f1)) / (pr$f2 - pr$f1) * 1000
pr$lab <- sprintf(c("/i/→/ɪ/", "/ʌ/→/ɑ/") %>% paste0("\nF1 %.0f→%.0f\nHz差 %.1f\nBark差 %.2f"), pr$f1, pr$f2, pr$f2 - pr$f1, bark(pr$f2) - bark(pr$f1))
pr$pair <- factor(pr$lab, levels = pr$lab)
B <- ggplot(pr, aes(pair, rate)) + geom_col(width = 0.55, fill = c("grey30", "grey70"), colour = "grey15") +
  geom_text(aes(label = sprintf("%.2f", rate)), vjust = -0.5, size = 3, family = "hira") +
  scale_y_continuous(limits = c(0, 11), expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "1000 Hzあたりの変化量（Bark）") +
  theme(axis.text.x = element_text(size = 8))
library(patchwork)
p <- A + B + plot_layout(widths = c(1, 1)) + plot_annotation(tag_levels = "A")
save_fig(p, "ch02_freq_scales", w = 5.8, h = 3.5)
