source("figures/style.R")
suppressPackageStartupMessages({library(phonTools); library(dplyr)})
data(pb52)
V <- c("{"="æ","3'"="ɚ",A="ɑ",E="ɛ",i="i",I="ɪ",O="ɔ",u="u",U="ʊ",V="ʌ")
tt <- c(m = "男性", w = "女性", c = "子ども")  # 図には男性と子どもだけを描く（本文の比較に合わせる）
pb <- pb52 %>% transmute(speaker, talker = factor(tt[as.character(type)], levels = tt),
                         vowel = V[as.character(vowel)], f1, f2)
z <- pb %>% group_by(speaker) %>% mutate(f1z = as.numeric(scale(f1)), f2z = as.numeric(scale(f2))) %>% ungroup()
mk <- function(d, a, b, panel) d %>% group_by(talker, vowel) %>%
  summarise(F1 = mean(.data[[a]]), F2 = mean(.data[[b]]), .groups = "drop") %>% mutate(panel = panel)
# 検算：本文2.5節の数値
print(mk(z, "f1", "f2", "raw") %>% filter(vowel == "i"))
print(mk(z, "f1z", "f2z", "z") %>% filter(vowel == "i"))
# 2パネルを同じ向きで描くため、列名をF1/F2に統一
m <- bind_rows(mk(z, "f1", "f2", "生のHz"), mk(z, "f1z", "f2z", "Lobanov正規化後（z）")) %>%
  mutate(panel = factor(panel, levels = c("生のHz", "Lobanov正規化後（z）")),
         vowel = factor(vowel, levels = unname(V)))
ord <- c("i","ɪ","ɛ","æ","ɑ","ɔ","ʊ","u")
poly <- m %>% filter(vowel %in% ord) %>% mutate(vowel = factor(vowel, levels = ord)) %>%
  arrange(panel, talker, vowel)
m <- m %>% filter(talker != "女性"); poly <- poly %>% filter(talker != "女性")
# ラベルは母音の位置を、パネルの重心から外向きに押し出して置く（記号と重ならないように）
lab <- m %>% group_by(panel, vowel) %>% summarise(F1 = mean(F1), F2 = mean(F2), .groups = "drop") %>%
  group_by(panel) %>% mutate(cx = mean(F2), cy = mean(F1), rx = diff(range(F2)), ry = diff(range(F1)),
    dx = (F2 - cx) / rx, dy = (F1 - cy) / ry,
    dx = ifelse(vowel == "ɚ", 0, dx), dy = ifelse(vowel == "ɚ", 2, dy), nn = sqrt(dx^2 + dy^2),
    lx = F2 + dx / nn * 0.05 * rx, ly = F1 + dy / nn * ifelse(vowel == "ɚ", 0.13, 0.06) * ry) %>% ungroup()
p <- ggplot(m, aes(F2, F1)) +
  geom_polygon(data = poly, aes(group = talker, linetype = talker), fill = NA, colour = "grey30", linewidth = 0.6) +
  geom_point(aes(shape = talker, fill = talker), size = 2.2, colour = "grey15", stroke = 0.6) +
  geom_text(data = lab, aes(x = lx, y = ly, label = vowel), family = "hira", size = 4.6, fontface = "bold") +
  facet_wrap(~panel, scales = "free") +
  scale_x_reverse() + scale_y_reverse() +
  scale_shape_manual(values = c(男性 = 21, 子ども = 22), name = NULL) +
  scale_fill_manual(values = c(男性 = "grey15", 子ども = "white"), name = NULL) +
  scale_linetype_manual(values = c(男性 = "solid", 子ども = "dashed"), name = NULL) +
  labs(x = "F2（左ほど前舌）", y = "F1（上ほど狭い）") +
  theme(strip.text = element_text(size = 11), legend.key.width = grid::unit(1.6, "lines"))
save_fig(p, "ch02_vowel_norm", w = 5.8, h = 3.6)
