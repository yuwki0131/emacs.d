# 自分用 ~/.emacs.d

発展途上 & 未確認 (´・_・`)


## キーバインド

デフォルト以外のキーバインド設定
|カテゴリ|キーバインド|function|機能名|
| --------------- |:---------------| -------------------- |:-------|

|prefix key 解除|C-e|unbind|prefix key 解除|
|prefix key 解除|C-a|unbind|prefix key 解除|
|prefix key 解除|C-z|unbind|prefix key 解除|
|prefix key 解除|M-m|unbind|prefix key 解除|
|prefix key 解除|M-j|unbind|prefix key 解除|
|編集|C-h|delete-backward-char|delete action|
|編集|M-h|backward-kill-word|delete action|
|移動|C-m|forward-paragraph|move by paragraph|
|移動|M-m|backward-paragraph|move by paragraph|
|移動|C-M-f|next-buffer-with-skip*|next / previous buffer with skip *|
|移動|C-M-p|previous-buffer-with-skip*|next / previous buffer with skip *|
|機能|C-q|undo|undo & redo|
|機能|M-q|redo|undo & redo|
|移動|M-p|scroll-up-in-place|カーソル位置固定のままスクロール|
|移動|M-n|scroll-down-in-place|カーソル位置固定のままスクロール|
|移動|M-g|goto-line|goto-line１回で|
|移動|M-a|ace-jump-mode|ace jump mode|
|移動|C-.|goto-next-TODO|TODOコメント管理|
|編集|C-:|insert-underscore|アンダースコア挿入|
|移動|C-?|highlight-symbol-next|シンボル移動　(highlight-symbol-mode)|
|移動|C-!|highlight-symbol-prev|シンボル移動　(highlight-symbol-mode)|
|編集|C-RET|kill-until-corresp-paren|括弧操作|
|編集|C-l|insert-parenthesis|括弧操作|
|編集|C-S-l|insert-angle-brackets|括弧操作|
|編集|M-l|insert-brackets|括弧操作|
|編集|M-S-l|insert-squares|括弧操作|
|編集|M-RET|yas-insert-snippet|snippet : スニペット挿入|
|機能|C-z C-k|describe-bindings|キーバインドの表示|
|機能|C-z p|toggle-truncate-lines|手前の空白を削除 (delete until black key)|
|機能|C-z C-r|replace-string|replace string my shortcut|
|機能|C-z f|set-file-name-coding-system|change encoding|
|機能|C-z C-b|grep-this|grep this % grep find this|
|機能|C-z C-f|grep-find-this|grep this % grep find this|
|機能|C-z C-s|swoop|swoop|
|機能|C-z s|make-buffer-small|make buffer small|
|機能|C-z C-o|nurumacs-map-toggle|sublime風のoutline|
|機能|C-z m|magit-status|magit (Emacs Git)|
|機能|C-z r|rgrep|rgrep grep|
|機能|C-z r|google-this|google this|
|機能|C-z C-t|bm-toggle|bm-toggle|
|機能|C-z t|bm-show|bm-toggle|
|機能|C-z M-t|bm-show-all|bm-toggle|
|編集|C-a C-a|comment-dwim|コメントアウト/コメントイン|
|編集|C-a C-u|upcase-word|upcase/downcase-word|
|編集|C-a C-p|downcase-word|upcase/downcase-word|
|編集|C-a C-k|kill-this-buffer|現在のバッファを消す|
|編集|C-a C-d|insert-date-normal|現在時刻挿入|
|編集|C-a M-d|insert-date-markdown|現在時刻挿入|
|編集|C-a C-m|insert--s|insert commment line|
|編集|C-a C-a |comment-dwim|multi-comment-out-in keys|
|編集|C-a C-f|merge2lines|merge 2 lines|
|移動|C-e C-l|goto-last-change|goto-chg|
|移動|C-e C-j|point-undo|point undo|
|移動|C-e C-k|point-redo|point undo|
|移動|C-e C-s|search-forward-regexp|正規表現検索|
|移動|C-e C-r|search-backward-regexp|正規表現検索|
|移動|C-e C-d|vr/query-replace|Visual Regexp|
|移動|C-e C-a|move-beginning-of-line|デフォルトの行先頭後尾移動 Ctrl-e / Ctrl-a の再設定|
|移動|C-e C-e|move-end-of-line|デフォルトの行先頭後尾移動 Ctrl-e / Ctrl-a の再設定|
|移動|C-e C-c|shell|quick shell|
|移動|C-e C-v|move-to-scratch|quick shell|
|移動|C-e C-w|move-to-repl|quick shell|
|移動|C-e C-l|recenter-top-bottom|top center bottom間移動|
|移動|C-e C-b|previous-buffer-with-skip*|バッファ移動 (アスタリスク付バッファはスキップ)|
|移動|C-e C-f|next-buffer-with-skip*|バッファ移動 (アスタリスク付バッファはスキップ)|
|移動|C-e C-n|highlight-symbol-next|シンボル移動　(highlight-symbol-mode)|
|移動|C-e C-p|highlight-symbol-prev|シンボル移動　(highlight-symbol-mode)|
