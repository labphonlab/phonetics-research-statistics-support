# 図11-2：床効果が「存在しない交互作用」を作る。潜在尺度の効果は両群とも+1.0（交互作用ゼロ）。
# データ生成は第11章11.2節のコード（seed 99）と同一。
suppressPackageStartupMessages(library(tidyverse))
source("figures/style.R")
set.seed(99)
n_rater <- 20; n_talker <- 40
talkers <- tibble(talker = sprintf("T%02d", 1:n_talker), l1 = rep(c("native", "learner"), each = n_talker / 2))
rd <- expand_grid(rater = sprintf("R%02d", 1:n_rater), talker = talkers$talker, task = c("read", "spont")) %>%
  left_join(talkers, by = "talker") %>%
  mutate(eta = if_else(l1 == "learner", 4.0, 0) + if_else(task == "spont", 1.0, 0) + rlogis(n()),
         rating = as.integer(cut(eta, breaks = c(-Inf, 1.5, 2.0, 2.5, 3.0, 3.8, 5.0, Inf), labels = 1:7)))
print(table(rd$rating))
m <- rd %>% group_by(l1, task) %>% summarise(mean_rating = round(mean(rating), 3), .groups = "drop") %>%
  pivot_wider(names_from = task, values_from = mean_rating) %>% mutate(diff = spont - read)
print(m)
stopifnot(as.integer(table(rd$rating)) == c(619, 113, 80, 101, 155, 215, 317),
          round(m$diff, 3)[m$l1 == "learner"] == 1.13, round(m$diff, 3)[m$l1 == "native"] == 0.567)
rd <- rd %>% mutate(grp = factor(ifelse(l1 == "native", "母語話者", "学習者"), levels = c("母語話者", "学習者")),
                    tk = factor(ifelse(task == "read", "読み上げ", "自発"), levels = c("読み上げ", "自発")))
dist <- rd %>% count(grp, tk, rating) %>% group_by(grp, tk) %>% mutate(p = n / sum(n)) %>% ungroup()
ann <- m %>% mutate(grp = factor(ifelse(l1 == "native", "母語話者", "学習者"), levels = c("母語話者", "学習者")),
                    lab = sprintf("平均の差 %.2f\n（潜在尺度では両群とも +1.0）", diff))
p <- ggplot(dist, aes(rating, p, fill = tk)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75, colour = "grey15", linewidth = 0.3) +
  geom_text(data = ann, aes(x = 7.4, y = 0.88, label = lab), inherit.aes = FALSE, hjust = 1, vjust = 1, size = 3, family = "hira", lineheight = 0.95) +
  facet_wrap(~grp) +
  scale_fill_manual(values = c("grey95", "grey35"), name = NULL) +
  scale_x_continuous(breaks = 1:7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.9), expand = expansion(mult = c(0, 0.02))) +
  labs(x = "評定値（1〜7）", y = "割合")
save_fig(p, "ch11_floor_effect", w = 5.8, h = 3.4)
