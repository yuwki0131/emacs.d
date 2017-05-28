# 標準キーバインドの一部抜粋

* 普段あまり使わないもの、忘れがちなもの、知らなかったもののリスト
* kmacro系は難しそうなので使うの&調べるのやめておく。
* 順不同

| キーバインド | コマンド名 | 説明 or コメント |
|:----------------:|:-----------:|:--|
| M-z | zap-to-char | 特定の文字まで削除 |
| C-_ | undo | undo(別のキーにバインドしているため、あまりつかってない) |
| C-/ | undo | undo(別のキーにバインドしているため、あまりつかってない) |
| C-\ | toggle-input-method | emacsでのinput-method切り替え。<br/>OS側のinput-methodを使っているため、これも普段は使わない。|
| C-x C-b | list-buffers | バッファの一覧表示 |
| C-x C-left | previous-buffer | 好みの問題 |
| C-x C-right | next-buffer | 好みの問題 |
| C-x C-+ | text-scale-adjust | テキストサイズを拡大 |
| C-x C-- |	text-scale-adjust | テキストサイズを縮小 |
| C-x C-0 |	text-scale-adjust | テキストサイズを縮小(ただし、挙動がよく分かってない) |
| C-x C-= | text-scale-adjust | テキストサイズを拡大(ただし、挙動がよく分かってない) |
| C-x C-o | delete-blank-lines | 周囲の空行削除して、1行の空行にする(普通に消すのよりは早いのかも) |
| C-x C-p |	mark-page | ページ(バッファ)全体を選択? |
| C-x C-q | read-only-mode | 現在のバッファのread-onlyモード切り替え |
| C-x C-r | find-file-read-only | read-onlyモードでバッファを開く |
| C-x C-t | transpose-lines | 行入れ替え |
| C-x C-u | upcase-region | 選択範囲の英字を大文字にする |
| C-x h | mark-whole-buffer | ファイル内容を全選択する。<br/>私は今までM-\< C-SPC M-\>で同じことをやってたけど。。。|
| C-x i	| insert-file | 別ファイルの内容をカーソル位置にインサートする。<br/>ちょっと便利にはなるがそれほどでもない気がする。 |
| C-x l	| count-lines-page | ファイルの行数と、カーソルの現在位置を教えてくれる。<br/>しかし、行番号表示してれば多分不要。|
| C-x u	| undo | undo (しかしEmacsのundo、いくつあるんだ...) |
| M-SPC | just-one-space | 周囲のスペースを削除し1つのスペースに。<br/>(ただし、私の環境だとOSのショートカットと競合している)|
| M-$ | ispell-word | 単語単位のスペルチェッカ(名前つける時のチェックに結構有用かも) |
| M-k | kill-sentence | 1文を削除。kill-wordみたいな感じで使う。 |
| M-g M-g | goto-line | 該当行に移動。(M-gでgoto-lineを設定している人も) |
| M-g M-n | next-error | 次のエラーに移動(するっぽい。未確認) |
| M-g M-p | previous-error | 前のエラーに移動(するっぽい。未確認) |
| M-! | shell-command | ミニバッファからシェルコマンド実行 |
| M-DEL | backward-kill-word | 好みの問題。しかし、EmacsでわざわざDeleteキー使う? |
