# 第14章 図14-1：話者単位100回分割での分類精度の分布。
# 手順・seedは本文14.2節（notebooks の ch14 セル6・8）と同一。乱数列を揃えるため本文のコードをそのまま実行する。
source("figures/style.R")
suppressPackageStartupMessages({library(MASS); library(phonTools); library(tidyverse)})
data(pb52)

pb <- pb52 %>%
  mutate(talker_type = recode(as.character(type), m = "男性", w = "女性", c = "子ども")) %>%
  group_by(speaker) %>%
  # 第2章と同じLobanov正規化（話者ごとのzスコア）。
  # ⚠️ 正規化のパラメータ（各話者の平均・SD）は「その話者の全トークン」
  #   （訓練・テストに分ける前の全体）から計算される。Lobanov・Nearey1は
  #   F1・F2の値だけを使い、母音カテゴリのラベルは使わない教師なしの変換
  #   なのでラベル漏洩ではないが、**テスト話者についても音響測定値そのものは
  #   正規化の計算に使われている**——「その話者の母音一式は既に観測できている」
  #   トランスダクティブな設定になる。1トークンだけの完全に未知な話者を
  #   分類する課題とは異なる、より緩やかな評価設定であることに注意する
  #   （詳細は本文14.2節）。ただし後述のWatt & Fabricius法は、基準となる
  #   母音（/i/・/ɑ/相当）を特定するのに母音ラベルそのものを使うため、
  #   この「ラベルは未知」という説明は当てはまらない点にも注意する。
  mutate(f1z = as.numeric(scale(f1)), f2z = as.numeric(scale(f2))) %>%
  ungroup()

# ★★訓練・テストは「トークン単位」ではなく「話者単位」で分ける。
#   トークン単位でランダムに分けると（例: sample(nrow(pb), ...)）、
#   同じ話者の他のトークンが訓練データに混ざり込む。その場合、モデルは
#   「未知の話者の母音を当てる」課題ではなく「既知の話者の、まだ見ていない
#   トークンを当てる」課題を解くことになり、精度は本来より高く出る。
set.seed(5)
speakers <- unique(pb$speaker)
train_speakers <- sample(speakers, floor(0.7 * length(speakers)))
train <- pb %>% filter(speaker %in% train_speakers)
test  <- pb %>% filter(!speaker %in% train_speakers)

accuracy <- function(model, data) mean(predict(model, data)$class == data$vowel)

test_single <- test
lda_raw  <- lda(vowel ~ f1 + f2, data = train)     # 生のHz
lda_norm <- lda(vowel ~ f1z + f2z, data = train)   # 話者内で正規化

cat("  生のHz F1,F2       :", round(accuracy(lda_raw, test), 3), "\n")
cat("  話者内正規化 F1,F2 :", round(accuracy(lda_norm, test), 3), "\n")

# どの母音どうしが混同されやすいか
pred <- predict(lda_norm, test)$class
confusion <- as.data.frame(table(true = test$vowel, pred = pred)) %>%
  filter(true != pred) %>% arrange(desc(Freq)) %>% head(5)
cat("\n最も混同された組み合わせ（この1回の分割）:\n")
print(confusion)

# --- 実行結果 ---
# 10母音の判別・未知話者への汎化（当てずっぽうなら10%、この1回の分割のみ）
#   生のHz F1,F2       : 0.713
#   話者内正規化 F1,F2 : 0.935
#
# 最も混同された組み合わせ（この1回の分割）:
#   true pred Freq
#      u    U    5
#      I    E    4
#      A    V    4
#      {    E    3
#      O    u    3
#
# 【出力の読み方】
# ・話者内正規化は、生のHzより判別精度を上げるはずである。使っている
#   情報はどちらもF1とF2の2つだけで、変えたのは
#   「話者ごとの声道長の違いを取り除いたかどうか」だけである。
# ・混同表が音声学的に筋の通った形をしているか確認する。
#   隣り合う母音どうしの混同（例: u と U）は自然だが、
#   もし i と A のように空間の端どうしが混同されていたら、
#   それは母音の性質ではなくコードの誤りを疑う場面である。
# ・⚠️ **ただし、この93.5%という数値を代表精度として報告してはならない。**
#   set.seed(5)という1つの分割がたまたま返した値にすぎない。次のセルで、
#   分割を100回繰り返して平均・SD・分布を確認する。

# ⏳ 数十秒（LDAは軽いモデルなので、800回近くのfitでもすぐ終わる）。

# Nearey1（formant-intrinsic、対数平均を引く）
pb <- pb %>% group_by(speaker) %>%
  mutate(f1_nearey = log(f1) - mean(log(f1)), f2_nearey = log(f2) - mean(log(f2)),
         f3z = as.numeric(scale(f3))) %>%
  ungroup()

# Watt & Fabricius（3つの角の重心を基準点にする）
# ⚠️ d$vowel == "i" / "A" で母音ラベルを直接使って角を特定している。
#   Lobanov・Nearey1と違い、この手法はテスト話者についても母音ラベルの
#   一部を使う点で、より緩い評価設定になっている（本文14.2節参照）。
speakers <- unique(pb$speaker)
wf_list <- lapply(speakers, function(sp) {
  d <- pb %>% filter(speaker == sp)
  Fi1 <- mean(d$f1[d$vowel == "i"]); Fi2 <- mean(d$f2[d$vowel == "i"])
  Fa1 <- mean(d$f1[d$vowel == "A"]); Fa2 <- mean(d$f2[d$vowel == "A"])
  Fu1 <- Fi1; Fu2 <- Fi1                       # 計算上の/u/: F1(u')=F2(u')=F1(i)
  S1 <- (Fi1 + Fa1 + Fu1) / 3; S2 <- (Fi2 + Fa2 + Fu2) / 3
  d$f1_wf <- d$f1 / S1; d$f2_wf <- d$f2 / S2
  d
})
pb <- bind_rows(wf_list)

acc      <- function(m, d) mean(predict(m, d)$class == d$vowel)
acc_type <- function(m, d) mean(predict(m, d)$class == d$talker_type)

set.seed(100)
n_rep <- 100
res <- vector("list", n_rep)
for (i in 1:n_rep) {
  train_speakers <- sample(speakers, floor(0.7 * length(speakers)))
  train <- pb %>% filter(speaker %in% train_speakers)
  test  <- pb %>% filter(!speaker %in% train_speakers)

  m_raw       <- lda(vowel ~ f1 + f2, data = train)
  m_lob       <- lda(vowel ~ f1z + f2z, data = train)
  m_lob_f3raw <- lda(vowel ~ f1z + f2z + f3, data = train)
  m_lob_f3z   <- lda(vowel ~ f1z + f2z + f3z, data = train)
  m_nearey    <- lda(vowel ~ f1_nearey + f2_nearey, data = train)
  m_wf        <- lda(vowel ~ f1_wf + f2_wf, data = train)
  m_type_raw  <- lda(talker_type ~ f1 + f2, data = train)
  m_type_norm <- lda(talker_type ~ f1z + f2z, data = train)

  pb2 <- pb %>% filter(vowel %in% c("i", "I")) %>% mutate(vowel = droplevels(vowel))
  sp2 <- unique(pb2$speaker); tr2 <- sample(sp2, floor(0.7 * length(sp2)))
  train2 <- pb2 %>% filter(speaker %in% tr2); test2 <- pb2 %>% filter(!speaker %in% tr2)
  m_ii <- lda(vowel ~ f1z + f2z, data = train2)

  res[[i]] <- tibble(
    raw = acc(m_raw, test), lobanov = acc(m_lob, test),
    lobanov_f3raw = acc(m_lob_f3raw, test), lobanov_f3norm = acc(m_lob_f3z, test),
    nearey = acc(m_nearey, test), wf = acc(m_wf, test),
    type_raw = acc_type(m_type_raw, test), type_norm = acc_type(m_type_norm, test),
    ii = acc(m_ii, test2)
  )
}
allres <- bind_rows(res)
single <- accuracy(lda_norm, test_single)
print(round(sapply(allres[,c("raw","lobanov","nearey","wf")], function(x) c(mean=mean(x), sd=sd(x))*100),2))
cat("single:", round(single*100,1), "\n")
lv <- c("raw", "wf", "nearey", "lobanov")
lab <- c(raw = "生のHz", wf = "Watt & Fabricius", nearey = "Nearey1", lobanov = "Lobanov")
long <- allres %>% select(raw, wf, nearey, lobanov) %>% pivot_longer(everything(), names_to = "method", values_to = "acc") %>%
  mutate(acc = acc * 100, method = factor(method, levels = lv, labels = lab[lv]))
mn <- long %>% group_by(method) %>% summarise(m = mean(acc), .groups = "drop")
shades <- setNames(c("grey85", "grey65", "grey45", "grey25"), levels(long$method))
p <- ggplot(long, aes(acc, method)) +
  geom_boxplot(aes(fill = method), width = 0.45, outlier.shape = NA, colour = "grey20", linewidth = 0.4, alpha = 0.9) +
  geom_jitter(height = 0.12, size = 0.6, colour = "grey30", alpha = 0.45) +
  geom_point(data = mn, aes(m, method), shape = 23, size = 3, fill = "white", stroke = 1) +
  geom_vline(xintercept = single * 100, linetype = "dashed", colour = "black", linewidth = 0.6) +
  annotate("text", x = single * 100 - 0.6, y = 4.5, label = "単一分割（seed 5）93.5%", hjust = 1, size = 3.2, family = "hira") +
  scale_fill_manual(values = shades, guide = "none") +
  scale_x_continuous(limits = c(55, 100), breaks = seq(60, 100, 10)) +
  labs(x = "未知話者の母音分類精度（%）　◇＝100回の平均", y = NULL)
save_fig(p, "ch14_split_accuracy", w = 5.6, h = 3.4)
