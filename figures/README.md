# 図の再生成スクリプト

書籍の図39点（画像）を、Rで再生成するスクリプトです。本文・ノートブックと同じ擬似データ（`vot_data.R`、`set.seed(42)`）や、
各章のコードと同じ設定で描いています。図は白黒印刷でも読めるよう、色ではなく濃淡・線種・形で区別してあります。

## 使い方

リポジトリのルートで、スクリプトを1本ずつ実行します（出力は `figures/` に300dpiのPNGで保存されます）。

```r
source("figures/ch05_speaker_lines.R")   # 図5-1
```

ターミナルからは `Rscript figures/ch05_speaker_lines.R` です。

- 日本語フォントは、macOSのヒラギノがあればそれを、なければGoogle FontsのNoto Sans JPを使います（ネット接続が必要）。
  Colabでも動きます。
- 必要なパッケージ: `tidyverse`、`ggplot2`、`showtext`、`sysfonts`、`ragg`、`lme4`、`lmerTest`、`emmeans`、`mgcv`、
  `itsadug`、`MASS`、`phonTools`、`e1071`、`brms`。第8章の図は `brms` でモデルを当てるため、時間がかかります（図8-1・8-3・8-4は各数分、図8-2は9回の当てはめで10〜20分。図8-2は結果をcsvに保存してあるので、通常は再当てはめしません。やり直すときは `FORCE_REFIT=1` を付けて実行します）。
- 乱数は固定してあるので、同じ環境なら同じ図になります。パッケージのバージョンが違うと、細部が変わることがあります。

## 図とスクリプトの対応

| 図 | 画像 | 再生成スクリプト |
|---|---|---|
| 図2-1 | `ch02_vot_hist.png` | `ch02_vot_hist.R` |
| 図2-2 | `ch02_vowel_norm.png` | `ch02_vowel_norm.R` |
| 図2-3 | `ch02_freq_scales.png` | `ch02_freq_scales.R` |
| 図2-4 | `ch02_duration_log.png` | `ch02_duration_log.R` |
| 図3-1 | `ch03_resid_fitted.png` | `ch03_resid_fitted.R` |
| 図3-2 | `ch03_speaker_level.png` | `ch03_speaker_level.R` |
| 図4-1 | `ch04_coding_meaning.png` | `ch04_coding_meaning.R` |
| 図4-2 | `ch04_centering.png` | `ch04_centering.R` |
| 図5-1 | `ch05_speaker_lines.png` | `ch05_speaker_lines.R` |
| 図5-2 | `ch05_caterpillar.png` | `ch05_caterpillar.R` |
| 図5-3 | `ch05_partial_pooling.png` | `ch05_partial_pooling.R` |
| 図6-1 | `ch06_logistic_curve.png` | `ch06_logistic_curve.R` |
| 図6-2 | `ch06_marginal_vs_conditional.png` | `ch06_logistic_curve.R` |
| 図6-3 | `ch06_multinom_probs.png` | `ch06_multinom_probs.R` |
| 図6-4 | `ch06_hurdle.png` | `ch06_hurdle.R` |
| 図7-1 | `ch07_gamm_fit.png` | `ch07_gamm_fit.R` |
| 図7-2 | `ch07_edf_panels.png` | `ch07_edf_panels.R` |
| 図8-1 | `ch08_prior_posterior.png` | `ch08_prior_posterior.R` |
| 図8-2 | `ch08_prior_sensitivity.png` | `ch08_prior_sensitivity.R` |
| 図8-3 | `ch08_trace.png` | `ch08_diagnostics.R` |
| 図8-4 | `ch08_ppcheck.png` | `ch08_diagnostics.R` |
| 図9-1 | `ch09_same_mean_diff_sd.png` | `ch09_same_mean_diff_sd.R` |
| 図10-1 | `ch10_identification_curves.png` | `ch10_identification_curves.R` |
| 図10-2 | `ch10_listener_curves.png` | `ch10_identification_curves.R` |
| 図11-1 | `ch11_roc_listeners.png` | `ch11_roc_listeners.R` |
| 図11-2 | `ch11_floor_effect.png` | `ch11_floor_effect.R` |
| 図11-3 | `ch11_rt_log.png` | `ch11_rt_log.R` |
| 図12-1 | `ch12_familywise_curve.png` | `ch12_familywise_curve.R` |
| 図12-2 | `ch12_pairwise_forest.png` | `ch12_pairwise_forest.R` |
| 図13-1 | `ch13_power_curve.png` | `ch13_power_curve.R` |
| 図13-2 | `ch13_typeM.png` | `ch13_power_curve.R` |
| 図14-1 | `ch14_split_accuracy.png` | `ch14_split_accuracy.R` |
| 図14-2 | `ch14_pca_scree.png` | `ch14_pca_scree.R` |
| 図15-1 | `ch15_kappa_weighted.png` | `ch15_kappa_weighted.R` |
| 図15-2 | `ch15_bland_altman.png` | `ch15_bland_altman.R` |
| 図15-3 | `ch15_informative_missing.png` | `ch15_informative_missing.R` |
| 図16-1 | `ch16_researcher_dof.png` | `ch16_researcher_dof.R` |
| 図17-1 | `ch17_sesoi_ci.png` | `ch17_sesoi_ci.R` |
| 図17-2 | `ch17_subsample.png` | `ch17_sesoi_ci.R` |

共通ファイルは、`style.R`（テーマ・日本語フォント・保存関数）と `vot_data.R`（本書共通のVOT擬似データ）です。
