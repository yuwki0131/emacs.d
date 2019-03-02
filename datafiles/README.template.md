# 自分用 ~/.emacs.d

* 修正中 & 未確認 (´･_･`)

* 修正して使うこと。

* READMEですがメモです。

* Linux Mint用

## スクリーンショット

<img src="img/img_use1.png" width="200" hspace="10"><img src="img/img_use2.png" width="200" hspace="10"><img src="img/img_use3.png" width="200" hspace="10">

## ~/.emacs.d/configディレクトリ以下構成

### elispファイル構成

|   | *.elファイル         | use-package | 設定                                   |
|:-:|:---------------------|:-----------:|:---------------------------------------|
| A | appearance-conf      | -           | emacsデフォルトの外見設定              |
| B | builtin-conf         | -           | emacsデフォルトの設定項目              |
| C | common-lang-conf     | o           | 言語共通設定 or 複数言語に共通する設定 |
| D | display-package-conf | o           | 表示系パッケージの設定項目             |
| E | edit-package-conf    | o           | 編集系パッケージの設定項目             |
| F | font-conf            | -           | フォント設定                          |
| G | grep-package-conf    | o           | 補完/grep系パッケージの設定項目        |
| J | jump-package-conf    | o           | 移動系パッケージの設定項目             |
| K | key-binding          | -           | Globalなキーバインドはここに一括       |
| L | language-conf        | o           | 各言語設定                             |
| M | modeline-conf        | -           | モードライン設定                       |
| O | outsider-eslip       | -           | 外部から持ち込んだコード               |
| P | package-conf         | o           | 各パッケージの設定項目                 |
| S | search-package-conf  | o           | 検索系パッケージの設定項目             |
| T | tips-eslip           | -           | 雑多な追加機能のコード                 |
| U | util-elisp           | -           | configファイル用のユーティリティ       |
| W | wm-conf              | -           | emacsをWindowManagerにする時の設定       |

* 順不同
* 残り(G, H, I, N, Q, R, V, X, Y, Z)

## 設定

```
$ git clone https://github.com/yuwki0131/emacs.d
$ mv emacs.d ~/.emacs.d
```

### elpaからpackage-install

初期化するとエラーレポートが出るので、以下のコマンドを実行。
不足しているelpaの最新版パッケージがインストールされる。

```
M-x install-complements
```

インストールしたら再起動。

## byte-compile *.el files

`.emacs.d/*.elc`と`.emacs.d/config/*.elc`のファイルを更新。

```
M-x refresh-byte-compile
```

`.emacs.d/*.elc`と`.emacs.d/config/*.elc`のファイルを削除。

```
M-x delete-byte-compile
```
