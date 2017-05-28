# 標準キーバインドの一部抜粋

* 普段あまり使わないもの、忘れがちなもの、知らなかったもののリスト
* kmacro系は難しそうなので使うの&調べるのやめておく。
* 順不同

| キーバインド | コマンド名 | 説明 or コメント |
|:----------------:|:--------------------:|:--|
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
| C-x i	| insert-file | 別ファイルの内容をカーソル位置にインサートする。ちょっと便利にはなるがそれほどでもない気がする。 |
| M-$ | ispell-word | 単語単位のスペルチェッカ(名前つける時のチェックに結構有用かも) |
| M-k | kill-sentence | 1文を削除。kill-wordみたいな感じで使う。 |
| M-! | shell-command | ミニバッファからシェルコマンド実行 |
| M-DEL | backward-kill-word | 好みの問題。しかし、EmacsでわざわざDeleteキー使う? |
