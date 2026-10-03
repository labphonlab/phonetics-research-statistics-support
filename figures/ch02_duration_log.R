# 図2-4：持続時間の対数変換前後（本文2.7節のコードと同じ擬似データ、seed 21）
source("figures/style.R")
suppressPackageStartupMessages(library(e1071))
set.seed(21)
dur <- rlnorm(300, meanlog = log(180), sdlog = 0.35)
r <- c(skew = skewness(dur), p = shapiro.test(dur)$p.value, lskew = skewness(log(dur)), lp = shapiro.test(log(dur))$p.value)
print(r); cat(mean(dur), median(dur), exp(mean(log(dur))), "\n")
stopifnot(round(r[1],3)==0.755, signif(r[2],3)==1.14e-06, round(r[3],3)==-0.181, round(r[4],3)==0.491,
          round(mean(dur),1)==190.6, round(median(dur),1)==183.4, round(exp(mean(log(dur))),1)==180.0)
pan <- c("A　生の持続時間（ms）", "B　対数変換後（log ms）")
d <- rbind(data.frame(x = dur, panel = pan[1]), data.frame(x = log(dur), panel = pan[2]))
d$panel <- factor(d$panel, levels = pan)
ann <- data.frame(panel = factor(pan, levels = pan), x = c(Inf, -Inf),
  lab = c(sprintf("歪度 %.3f\nShapiro-Wilk p = %.2e", r[1], r[2]),
          sprintf("歪度 %.3f\nShapiro-Wilk p = %.3f", r[3], r[4])))
ann$x <- c(max(dur), min(log(dur)))
dn <- rbind(data.frame(x = seq(min(dur), max(dur), length.out = 300), panel = pan[1])
              |> transform(y = dnorm(x, mean(dur), sd(dur))),
            data.frame(x = seq(min(log(dur)), max(log(dur)), length.out = 300), panel = pan[2])
              |> transform(y = dnorm(x, mean(log(dur)), sd(log(dur)))))
dn$panel <- factor(dn$panel, levels = pan)
p <- ggplot(d, aes(x)) +
  geom_histogram(aes(y = after_stat(density)), bins = 24, fill = "grey65", colour = "white", linewidth = 0.3) +
  geom_line(data = dn, aes(x, y), linetype = "dashed", linewidth = 0.7) +
  geom_text(data = ann, aes(x = c(Inf, Inf), y = Inf, label = lab), hjust = 1.05, vjust = 1.3, size = 3, family = "hira", lineheight = 1.05) +
  facet_wrap(~panel, nrow = 1, scales = "free") +
  labs(x = NULL, y = "密度")
save_fig(p, "ch02_duration_log", w = 5.8, h = 3.2)
