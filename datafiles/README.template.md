# 自分用 ~/.emacs.d

* 修正中 & 未確認 (´･_･`)

* 修正して使うこと。

* READMEですがメモです。

* Linux Mint用

## スクリーンショット

<img src="img/img_use1.png" width="200" hspace="10"><img src="img/img_use2.png" width="200" hspace="10"><img src="img/img_use3.png" width="200" hspace="10">

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

`.emacs.d/*.elc`と`.emacs.d/alphabets/*.elc`のファイルを更新。

```
M-x refresh-byte-compile
```

`.emacs.d/*.elc`と`.emacs.d/alphabets/*.elc`のファイルを削除。

```
M-x delete-byte-compile
```

## ~/.emacs.d/alphabetsディレクトリ以下構成

### elispファイル構成

|   | *.elファイル         | use-package | 設定                                 |
|:-:|:---------------------|:-----------:|:-------------------------------------|
| A | appearance-conf      | -           | emacsデフォルトの外見設定            |
| B | builtin-conf         | -           | emacsデフォルトの設定項目            |
| C | complements-packages | o           | 補完系パッケージの設定項目           |
| D | display-packages     | o           | 表示系パッケージの設定項目           |
| E | edit-packages        | o           | 編集系パッケージの設定項目           |
| F | font-conf            | -           | フォント設定                         |
| G | grep-packages        | o           | 補完/grep系パッケージの設定項目      |
| H | highlight-packages   | o           | ハイライト系パッケージの設定項目     |
| J | jump-packages        | o           | 移動系パッケージの設定項目           |
| K | key-binding          | -           | Globalなキーバインドはここに一括     |
| L | language-conf        | o           | 各言語設定                           |
| O | outsider-eslip       | -           | 外部から持ち込んだコード             |
| R | rest-packages        | o           | 分類できなかったパッケージの設定項目 |
| S | search-packages      | o           | 検索系パッケージの設定項目           |
| T | tips-eslip           | -           | 雑多な追加機能のコード               |
| U | util-elisp           | -           | alphabetsファイル用のユーティリティ  |
| W | wm-conf              | -           | emacsをWindowManagerにする時の設定   |

* 順不同
* 残り(I, M, N, Q, R, V, X, Y, Z)
