# 本書共通のVOT擬似データ（第1章と同じ手順・seed）。ノートブックのコードと同一。
suppressPackageStartupMessages(library(tidyverse))
set.seed(42)  # 乱数のseedを固定する。同じ数字なら誰が実行しても同じ結果になる
n_speaker <- 10                    # 話者数
n_word    <- 20                    # 単語数（各話者が全単語を1回ずつ発話する）
n_obs     <- n_speaker * n_word    # = 200行

# 何行目がどの話者・どの単語かを表す添字。あとで話者ごと・単語ごとの
# 値を引くのに使う（speaker_intercept[speaker_idx] のように書ける）。
speaker_idx <- rep(1:n_speaker, each  = n_word)
word_idx    <- rep(1:n_word,    times = n_speaker)

# 1行ごとにvoiced/voicelessを交互に並べる。話者1人あたり20行なので、
# どの話者を見ても必ずvoiced10回・voiceless10回が含まれる
# ——これは第5章で「話者ごとにconditionの効果を推定する」ために必須の設計。
condition_vec <- rep(c("voiced", "voiceless"), times = n_obs / 2)

# ---- 話者差・単語差を「先に」作っておく ----
# 実際の音声データでは、同じ条件でも話者によってVOTの平均が違い（声道長・
# 発話速度・方言）、単語によっても違う（後続母音・語長・頻度）。
# 擬似データでもこの構造を明示的に作っておかないと、第5章以降で使う
# 混合効果モデルが「推定すべきばらつきが最初から存在しない」データを
# 相手にすることになり、分散推定値がすべて0（singular fit）になってしまう。
speaker_intercept <- rnorm(n_speaker, mean = 0, sd = 6)
# 話者ごとのVOTの底上げ量。どの条件にも一律にかかる（＝ランダム切片）。
speaker_slope <- rnorm(n_speaker, mean = 0, sd = 10)
# 話者ごとの「有声と無声をどれだけ大きく作り分けるか」の差（＝ランダム傾き）。
# 話者間のばらつきは無声（VOTが長い側）で特に大きいことが知られているので、
# voiceless条件にだけ加える。
word_intercept <- rnorm(n_word, mean = 0, sd = 3)
# 単語ごとの癖（＝項目のランダム切片）。

vot_data <- tibble(
  speaker   = paste0("S", sprintf("%02d", speaker_idx)),
  word      = paste0("word_", word_idx),
  gender    = rep(rep(c("M", "F"), each = n_speaker / 2), each = n_word),
  condition = condition_vec,
  # VOTは「固定効果 + 話者差 + 単語差 + 残差」を足し合わせて作る。
  # これは第5章で当てはめる混合効果モデルの式そのものであり、
  # モデルが推定した分散をこの生成時の値と見比べられるようにしてある。
  VOT = 20 +                                              # 有声条件の基準値
    if_else(condition == "voiceless", 55, 0) +            # 無声条件の上乗せ（→約75ms）
    speaker_intercept[speaker_idx] +                      # 話者のランダム切片
    if_else(condition == "voiceless",                     # 話者のランダム傾き
            speaker_slope[speaker_idx], 0) +
    word_intercept[word_idx] +                            # 単語のランダム切片
    # 残差。無声条件のほうがばらつきが大きい（実際のVOTデータもそうなっている）。
    # 第3章のモデル診断で「等分散性が崩れている」例として使う。
    rnorm(n_obs, mean = 0, sd = if_else(condition == "voiced", 4, 8)),
  F0 = rnorm(n_obs, mean = 130, sd = 15)  # F0はここでは条件と無関係に生成
)

vot_data <- vot_data %>%
  mutate(
    # 文字列のままだとRが「順序のない値」として扱うため、
    # 話者・単語・性別・条件をすべてfactor（因子）型に変換しておく。
    # 特にconditionはlevels=でvoiced/voicelessの順序を明示し、
    # 以降のモデルの切片が「voiced群の平均」になるよう固定する。
    speaker   = factor(speaker),
    word      = factor(word),
    gender    = factor(gender),
    condition = factor(condition, levels = c("voiced", "voiceless"))
  )

