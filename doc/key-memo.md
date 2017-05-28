# 有用な標準キーバインドのリスト

* 有用だが普段あまり使わない、または、忘れがちな標準キーバインドのリスト
* kmacro系は難しそうなので使うのやめておく。
* 順不同

| キーバインド | コマンド名 | 説明 |
|:----------|:------------------|:---------------------------------------------------|
| M-z | zap-to-char| 特定の文字まで削除 |
| C-_ | undo | undo(別のキーにバインドしているため、あまりつかってない) |
| C-/ | undo | undo(別のキーにバインドしているため、あまりつかってない) |
| C-\ | toggle-input-method | emacsでのinput-method切り替え。OS側のモノを使っているため、これも普段は使わない|
| C-x C-b | list-buffers | バッファの一覧表示 |
| C-x <C-left> | previous-buffer | これ意味ある? |
| C-x <C-right> | next-buffer | これ意味ある? |
| C-x C-+ | text-scale-adjust | テキストサイズを拡大 |
| C-x C-- |	text-scale-adjust | テキストサイズを縮小 |
| C-x C-0 |	text-scale-adjust | テキストサイズを縮小(ただし、挙動がよく分かってない) |
| C-x C-= | text-scale-adjust | テキストサイズを拡大(ただし、挙動がよく分かってない) |
| M-$ | ispell-word | スペルチェッカ(結構有用かも) |
| M-k | kill-sentence | 1文を削除。kill-wordみたいな感じで使う。 |
