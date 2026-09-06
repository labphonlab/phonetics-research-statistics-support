#!/usr/bin/env python3
"""Notebook のRコードを実際に実行して、エラーが出ないかを確かめる。

構文が通ることと、走ることは別である。parse だけでは
「存在しない列を参照する」「引数名が違う」といった誤りは見つからない。

Colab では各Notebookがパッケージを導入するが、ここでは導入済みの
ローカル環境で走らせる。install.packages は requireNamespace で
守られているので、導入済みなら飛ばされる。
"""
import argparse, json, pathlib, re, subprocess, sys, tempfile, time

ROOT = pathlib.Path(__file__).parent.parent

# ネットワークに出る処理は環境で結果が変わるため、実行時は無効化する。
# フォント取得は図の見た目の問題で、コードの正しさとは別である。
SKIP_PAT = re.compile(r'(sysfonts::font_add_google|showtext|font_add_google)')


def cells_to_script(nb_path, workdir):
    nb = json.loads(nb_path.read_text())
    parts = ["setwd('%s')" % workdir,
             "options(warn=1)",
             "pdf(NULL)  # 図はファイルに出さない"]
    for c in nb["cells"]:
        if c["cell_type"] != "code":
            continue
        src = "".join(c.get("source", []))
        if not src.strip():
            continue
        if SKIP_PAT.search(src):
            # フォント取得行だけを落とす。他の行は残す。
            src = "\n".join(l for l in src.split("\n") if not SKIP_PAT.search(l))
        parts.append(src)
    return "\n\n".join(parts)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--timeout", type=int, default=420)
    ap.add_argument("--only", default=None)
    a = ap.parse_args()

    data = ROOT / "data"
    ok, fail, timeout = [], [], []
    for p in sorted(ROOT.glob("notebooks/*.ipynb")):
        if a.only and a.only not in p.name:
            continue
        with tempfile.TemporaryDirectory() as wd:
            # 付属データを作業フォルダに置く（相対パスで読むNotebookがある）
            for f in data.glob("*"):
                (pathlib.Path(wd) / f.name).write_text(f.read_text())
            script = pathlib.Path(wd) / "run.R"
            script.write_text(cells_to_script(p, wd))
            t0 = time.time()
            try:
                r = subprocess.run(["Rscript", "--vanilla", str(script)],
                                   capture_output=True, text=True, timeout=a.timeout)
            except subprocess.TimeoutExpired:
                timeout.append((p.name, a.timeout))
                print(f"  ⏱ {p.name}: {a.timeout}秒で打ち切り")
                continue
            el = time.time() - t0
            err = r.stderr
            if r.returncode != 0:
                # 最初の Error 行を拾うと誤診する。try() で囲んだ意図的な
                # エラー例示が先に現れ、本当に止まった箇所を隠すため。
                # 実行を止めたのは最後のエラーなので、末尾から探す。
                errs = [l for l in err.split("\n")
                        if l.startswith("Error") or l.startswith("エラー")]
                line = errs[-1] if errs else err.strip().split("\n")[-1][:150]
                fail.append((p.name, line[:150]))
                print(f"  ✗ {p.name} ({el:.0f}s): {line[:130]}")
            else:
                ok.append((p.name, el))
                print(f"  ✓ {p.name} ({el:.0f}s)")

    print(f"\n成功 {len(ok)} / 失敗 {len(fail)} / 時間切れ {len(timeout)}")

    # 無条件の install.packages は、実行のたびに再導入が走って待ち時間になる。
    # requireNamespace で守られているかを見る。
    print("\nパッケージ導入の書き方")
    for p in sorted(ROOT.glob("notebooks/*.ipynb")):
        nb = json.loads(p.read_text())
        src = "\n".join("".join(c.get("source", []))
                        for c in nb["cells"] if c["cell_type"] == "code")
        lines = [l for l in src.split("\n")
                 if "install.packages(" in l and not l.strip().startswith("#")]
        bare = [l for l in lines if "requireNamespace" not in l]
        if bare:
            print(f"  ⚠ {p.name}: 無条件の導入が {len(bare)} 箇所"
                  f"（守られているもの {len(lines) - len(bare)} 箇所）")
    return 1 if fail else 0


if __name__ == "__main__":
    sys.exit(main())
