# 図の共通スタイル：白黒印刷でも読めるよう、色ではなく濃淡・線種・形で区別する。
suppressPackageStartupMessages({library(ggplot2); library(showtext); library(sysfonts)})
# 日本語フォント：macOSのヒラギノがあればそれを、なければGoogle FontsのNoto Sans JP（要ネット接続。
# Colabでも使える）を使う。どちらも使えないときは既定のフォントになり、日本語が欠ける。
.hira <- "/System/Library/Fonts/ヒラギノ角ゴシック W3.ttc"
if (file.exists(.hira)) {
  font_add("hira", .hira)
} else {
  ok <- tryCatch({ font_add_google("Noto Sans JP", "hira"); TRUE }, error = function(e) FALSE)
  if (!ok) warning("日本語フォントを用意できませんでした。図の日本語が表示されない可能性があります。")
}
dir.create("figures", showWarnings = FALSE)
showtext_auto(); showtext_opts(dpi = 300)
theme_book <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = "hira") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "grey88", linewidth = 0.3),
          axis.line = element_line(colour = "grey30", linewidth = 0.4),
          legend.position = "bottom", plot.title = element_blank(),
          strip.text = element_text(face = "plain"))
}
theme_set(theme_book())
GREYS <- c("grey15", "grey55")           # 2群の塗り・線の標準
save_fig <- function(p, name, w = 6.0, h = 3.6) {
  ragg::agg_png(file.path("figures", paste0(name, ".png")), width = w, height = h, units = "in", res = 300)
  print(p); dev.off()
}
