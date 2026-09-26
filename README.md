# 音声研究のための統計解析 — Notebook

書籍『音声研究のための統計解析—RとColaboratoryで学ぶ実践ハンドブック』
（音声学ライブラリ 第3巻）に対応する R Notebook です。

## 使い方

**バッジを押すだけです。** Colab が開き、そのまま実行できます。
Googleドライブへのコピーも、ファイルの配置も要りません。

書き換えを残したいときだけ、**ファイル → ドライブにコピーを保存** を一度行ってください。

| 章 | 開く | コードセル |
|---|---|---|
| 全章統合版 | [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/labphonlab/phonetics-research-statistics-support/blob/main/notebooks/00_all_in_one.ipynb) | 111 |
| Rの基礎 | [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/labphonlab/phonetics-research-statistics-support/blob/main/notebooks/ch00_programming_primer.ipynb) | 4 |
| データ整形と可視化 | [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/labphonlab/phonetics-research-statistics-support/blob/main/notebooks/ch01_data_wrangling_viz.ipynb) | 13 |
| 記述統計 | [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/labphonlab/phonetics-research-statistics-support/blob/main/notebooks/ch02_descriptive_stats.ipynb) | 13 |
| 回帰と分散分析 | [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/labphonlab/phonetics-research-statistics-support/blob/main/notebooks/ch03_regression_anova.ipynb) | 5 |
| 対比コーディング | [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/labphonlab/phonetics-research-statistics-support/blob/main/notebooks/ch04_contrast_coding.ipynb) | 5 |
| 線形混合モデル | [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/labphonlab/phonetics-research-statistics-support/blob/main/notebooks/ch05_linear_mixed_models.ipynb) | 7 |
| 一般化線形混合モデル | [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/labphonlab/phonetics-research-statistics-support/blob/main/notebooks/ch06_glmm.ipynb) | 7 |
| 一般化加法混合モデル | [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/labphonlab/phonetics-research-statistics-support/blob/main/notebooks/ch07_gamm.ipynb) | 5 |
| ベイズ統計（brms） | [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/labphonlab/phonetics-research-statistics-support/blob/main/notebooks/ch08_bayesian_brms.ipynb) | 8 |
| 多変量解析と関数データ解析 | [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/labphonlab/phonetics-research-statistics-support/blob/main/notebooks/ch09_multivariate_fda.ipynb) | 6 |
| 知覚実験（同定） | [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/labphonlab/phonetics-research-statistics-support/blob/main/notebooks/ch10_perception_identification.ipynb) | 6 |
| 信号検出理論・順序回帰・反応時間 | [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/labphonlab/phonetics-research-statistics-support/blob/main/notebooks/ch11_perception_sdt_ordinal_rt.ipynb) | 6 |
| 多重比較 | [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/labphonlab/phonetics-research-statistics-support/blob/main/notebooks/ch12_multiple_comparisons.ipynb) | 6 |
| 検定力と標本サイズ | [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/labphonlab/phonetics-research-statistics-support/blob/main/notebooks/ch13_power_sample_size.ipynb) | 6 |
| 分類とクラスタリング | [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/labphonlab/phonetics-research-statistics-support/blob/main/notebooks/ch14_classification_clustering.ipynb) | 4 |
| 信頼性と欠測 | [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/labphonlab/phonetics-research-statistics-support/blob/main/notebooks/ch15_reliability_missingness.ipynb) | 4 |
| 報告と再現可能性 | [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/labphonlab/phonetics-research-statistics-support/blob/main/notebooks/ch16_reporting_reproducibility.ipynb) | 5 |
| targetsによるパイプライン | [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/labphonlab/phonetics-research-statistics-support/blob/main/notebooks/10_pipeline_targets.ipynb) | 2 |

`00_all_in_one` は全章をまとめたものです。1本で通して進めたい場合はこちらを使ってください。

## データについて

Notebook はデータを外部から読み込みません。**その場で生成します。**
乱数のseedを固定してあるので、誰がいつ実行しても同じ結果になります。
ネットワークが不安定でも、配布元のURLが変わっても、結果は変わりません。

`data/vot_f0_synthetic.csv` は `10_pipeline_targets` が使う合成データです。

## 実行環境

Colab の R ランタイムで動きます。パッケージのインストールは各Notebookの
冒頭セルが行います。`brms` はStanのコンパイルを伴うため、初回は数分かかります。

図の日本語は、Google Fonts の Noto Sans JP を取得して表示します（要ネット接続）。

## 書籍との対応

Notebook は書籍の各章に対応します。**書籍側が正典**で、
このリポジトリはその写しです。`tools/sync_notebooks.py` が
書籍リポジトリから機械的に取り込み、Colabバッジを付けています。
手でコピーしていないので、記述の食い違いが起きません。

## ライセンス

MITライセンスで無償公開しています。改変・再配布・研究や教育での利用は自由です。
出典表示は必須ではありませんが、歓迎します。詳しくは `LICENSE` を参照してください。

## 正誤・要望

不具合や誤りは Issues でお知らせください。
