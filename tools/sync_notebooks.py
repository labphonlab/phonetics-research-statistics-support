#!/usr/bin/env python3
"""書籍リポジトリから Notebook を取り込み、Colabバッジを付ける。

手でコピーしない。書籍側が正典で、ここはその写しである。
既定では書籍リポジトリの HEAD（コミット済みの状態）から取る。
作業中の変更を巻き込まないためで、別セッションが編集中でも安全に走る。

使い方:
    python3 tools/sync_notebooks.py                 # HEAD から取り込む
    python3 tools/sync_notebooks.py --ref main      # 任意のrefから
    python3 tools/sync_notebooks.py --worktree      # 作業中の状態から（要注意）
"""
import argparse, json, pathlib, subprocess, sys

ROOT = pathlib.Path(__file__).parent.parent
SRC = pathlib.Path.home() / "Projects/business/books/phonetics-research-statistics"
OWNER_REPO = "labphonlab/phonetics-research-statistics-support"

TITLES = {
    "00_all_in_one": "全章統合版",
    "ch00_programming_primer": "Rの基礎",
    "ch01_data_wrangling_viz": "データ整形と可視化",
    "ch02_descriptive_stats": "記述統計",
    "ch03_regression_anova": "回帰と分散分析",
    "ch04_contrast_coding": "対比コーディング",
    "ch05_linear_mixed_models": "線形混合モデル",
    "ch06_glmm": "一般化線形混合モデル",
    "ch07_gamm": "一般化加法混合モデル",
    "ch08_bayesian_brms": "ベイズ統計（brms）",
    "ch09_multivariate_fda": "多変量解析と関数データ解析",
    "ch10_perception_identification": "知覚実験（同定）",
    "ch11_perception_sdt_ordinal_rt": "信号検出理論・順序回帰・反応時間",
    "ch12_multiple_comparisons": "多重比較",
    "ch13_power_sample_size": "検定力と標本サイズ",
    "ch14_classification_clustering": "分類とクラスタリング",
    "ch15_reliability_missingness": "信頼性と欠測",
    "ch16_reporting_reproducibility": "報告と再現可能性",
    "10_pipeline_targets": "targetsによるパイプライン",
}
ORDER = ["00_all_in_one"] + [k for k in TITLES if k.startswith("ch")] + ["10_pipeline_targets"]


def badge_cell(stem):
    url = f"https://colab.research.google.com/github/{OWNER_REPO}/blob/main/notebooks/{stem}.ipynb"
    return {
        "cell_type": "markdown",
        "metadata": {},
        "source": [
            f'<a href="{url}" target="_parent">'
            '<img src="https://colab.research.google.com/assets/colab-badge.svg" '
            'alt="Open In Colab"/></a>\n',
            "\n",
            "このバッジから開くと、設定なしでそのまま実行できます。\n",
            "書き換えを残したいときは **ファイル → ドライブにコピーを保存** を一度行ってください。\n",
        ],
    }


def read_from_git(ref, path):
    r = subprocess.run(["git", "show", f"{ref}:{path}"], cwd=SRC,
                       capture_output=True, text=True)
    if r.returncode != 0:
        return None
    return r.stdout


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--ref", default="HEAD")
    ap.add_argument("--worktree", action="store_true")
    a = ap.parse_args()

    if not SRC.exists():
        sys.exit(f"書籍リポジトリが見つからない: {SRC}")

    if a.worktree:
        dirty = subprocess.run(["git", "status", "--porcelain", "notebooks/"],
                               cwd=SRC, capture_output=True, text=True).stdout.strip()
        if dirty:
            print(f"警告: 作業中の変更が {len(dirty.splitlines())} 件ある状態から取り込む")

    (ROOT / "notebooks").mkdir(exist_ok=True)
    done, missing = [], []
    for stem in ORDER:
        rel = f"notebooks/{stem}.ipynb"
        if a.worktree:
            p = SRC / rel
            text = p.read_text() if p.exists() else None
        else:
            text = read_from_git(a.ref, rel)
        if text is None:
            missing.append(stem)
            continue
        nb = json.loads(text)
        # 既存のバッジセルがあれば置き換える
        cells = [c for c in nb["cells"]
                 if "colab-badge.svg" not in "".join(c.get("source", []))]
        nb["cells"] = [badge_cell(stem)] + cells
        nb.setdefault("metadata", {}).setdefault("colab", {})["provenance"] = []
        (ROOT / rel).write_text(json.dumps(nb, ensure_ascii=False, indent=1) + "\n")
        done.append(stem)

    # 付属データ
    for name in ["vot_f0_synthetic.csv"]:
        text = (read_from_git(a.ref, f"data/{name}") if not a.worktree
                else (SRC / "data" / name).read_text())
        if text:
            (ROOT / "data" / name).write_text(text)

    print(f"取り込み {len(done)} 本 (ref={a.ref if not a.worktree else 'worktree'})")
    if missing:
        print(f"見つからなかった: {missing}")
    return 1 if missing else 0


if __name__ == "__main__":
    sys.exit(main())
