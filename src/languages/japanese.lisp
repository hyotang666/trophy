(in-package :trophy)

(handler-bind ((error
                 (lambda (condition)
                   (let ((restart (find-restart 'continue condition)))
                     (when restart
                       (invoke-restart restart))))))
  (translate:define-language :ja))

(translate:add-translations :ja
  "symbol-usage-level1" "シンボル使用頻度ランキングTOP１０をコンプリートしました。"
  "symbol-usage-level2" "シンボル使用頻度ランキング10 ~~ 20をコンプリートしました。"
  "symbol-usage-level3" "シンボル使用頻度ランキング20 ~~ 30をコンプリートしました。"
  "symbol-usage-level4" "シンボル使用頻度ランキング30 ~~ 40をコンプリートしました。"
  "symbol-usage-level5" "シンボル使用頻度ランキング40 ~~ 50をコンプリートしました。"
  "symbol-usage-level6" "シンボル使用頻度ランキング50 ~~ 60をコンプリートしました。"
  "symbol-usage-level7" "シンボル使用頻度ランキング60 ~~ 70をコンプリートしました。"
  "symbol-usage-level8" "シンボル使用頻度ランキング70 ~~ 80をコンプリートしました。"
  "symbol-usage-level9" "シンボル使用頻度ランキング80 ~~ 90をコンプリートしました。"
  "symbol-usage-level10" "シンボル使用頻度ランキング90 ~~ 100をコンプリートしました。"
  "symbol-usage-level11" "シンボル使用頻度ランキング100 ~~ 110をコンプリートしました。"
  "symbol-usage-level12" "シンボル使用頻度ランキング110 ~~ 120をコンプリートしました。"
  "symbol-usage-level13" "シンボル使用頻度ランキング120 ~~ 130をコンプリートしました。"
  "symbol-usage-level14" "シンボル使用頻度ランキング130 ~~ 140をコンプリートしました。"
  "symbol-usage-level15" "シンボル使用頻度ランキング140 ~~ 150をコンプリートしました。"
  "symbol-usage-level16" "シンボル使用頻度ランキング150 ~~ 160をコンプリートしました。"
  "symbol-usage-level17" "シンボル使用頻度ランキング160 ~~ 170をコンプリートしました。"
  "symbol-usage-level18" "シンボル使用頻度ランキング170 ~~ 180をコンプリートしました。"
  "symbol-usage-level19" "シンボル使用頻度ランキング180 ~~ 190をコンプリートしました。"
  "symbol-usage-level20" "シンボル使用頻度ランキング190 ~~ 200をコンプリートしました。"
  "symbol-usage-level21" "シンボル使用頻度ランキング200 ~~ 210をコンプリートしました。"
  "symbol-usage-level22" "シンボル使用頻度ランキング210 ~~ 220をコンプリートしました。"
  "symbol-usage-level23" "シンボル使用頻度ランキング220 ~~ 230をコンプリートしました。"
  "symbol-usage-level24" "シンボル使用頻度ランキング230 ~~ 240をコンプリートしました。"
  "symbol-usage-level25" "シンボル使用頻度ランキング240 ~~ 250をコンプリートしました。"
  "symbol-usage-level26" "シンボル使用頻度ランキング250 ~~ 260をコンプリートしました。"
  "symbol-usage-level27" "シンボル使用頻度ランキング260 ~~ 270をコンプリートしました。"
  "symbol-usage-level28" "シンボル使用頻度ランキング270 ~~ 280をコンプリートしました。"
  "symbol-usage-level29" "シンボル使用頻度ランキング280 ~~ 290をコンプリートしました。"
  "symbol-usage-level30" "シンボル使用頻度ランキング290 ~~ 300をコンプリートしました。"
  "symbol-usage-level31" "シンボル使用頻度ランキング300 ~~ 310をコンプリートしました。"
  "symbol-usage-level32" "シンボル使用頻度ランキング310 ~~ 320をコンプリートしました。"
  "symbol-usage-level33" "シンボル使用頻度ランキング320 ~~ 330をコンプリートしました。"
  "symbol-usage-level34" "シンボル使用頻度ランキング330 ~~ 340をコンプリートしました。"
  "symbol-usage-level35" "シンボル使用頻度ランキング340 ~~ 350をコンプリートしました。"
  "symbol-usage-level36" "シンボル使用頻度ランキング350 ~~ 360をコンプリートしました。"
  "symbol-usage-level37" "シンボル使用頻度ランキング360 ~~ 370をコンプリートしました。"
  "symbol-usage-level38" "シンボル使用頻度ランキング370 ~~ 380をコンプリートしました。"
  "symbol-usage-level39" "シンボル使用頻度ランキング380 ~~ 390をコンプリートしました。"
  "symbol-usage-level40" "シンボル使用頻度ランキング390 ~~ 400をコンプリートしました。"
  "symbol-usage-level41" "シンボル使用頻度ランキング400 ~~ 410をコンプリートしました。"
  "symbol-usage-level42" "シンボル使用頻度ランキング410 ~~ 420をコンプリートしました。"
  "symbol-usage-level43" "シンボル使用頻度ランキング420 ~~ 430をコンプリートしました。"
  "symbol-usage-level44" "シンボル使用頻度ランキング430 ~~ 440をコンプリートしました。"
  "symbol-usage-level45" "シンボル使用頻度ランキング440 ~~ 450をコンプリートしました。"
  "symbol-usage-level46" "シンボル使用頻度ランキング450 ~~ 460をコンプリートしました。"
  "symbol-usage-level47" "シンボル使用頻度ランキング460 ~~ 470をコンプリートしました。"
  "symbol-usage-level48" "シンボル使用頻度ランキング470 ~~ 480をコンプリートしました。"
  "dictionary-is-released" "辞書 ~S が~Sにより開放されました。"
  "dictionary-is-completed" "辞書 ~S をコンプリートしました。"
  "first-sexp" "初めてのS式を取得しました。"
  "first-error" "初めてのエラーを取得しました。"
  "first-macro" "初めてのマクロを取得しました。"
  "first-special-operator" "初めての特殊形式を取得しました。"
  "first-time-defun" "初めての関数定義を取得しました。"
  "setf-philia" "称号：SETF愛好者を取得しました。"
  "first-time-declare" "初めての宣言を取得しました。"
  "declare-philia" "称号：宣言愛好者を取得しました。"
  "loop-philia" "称号：LOOP愛好者を取得しました。"
  "first-time-defmethod" "初めてのメソッド定義を取得しました。"
  "lambda-master" "称号：無名関数マスターを取得しました。"
  "format-black-belt" "称号：FORMAT黒帯を取得しました。"
  "first-time-&key" "初めてのキーワードパラメタを取得しました。"
  "first-time-values" "初めての多値返却を取得しました。"
  "first-time-in-package" "初めてのお出かけを取得しました。"
  "nomad" "称号：流浪の民を取得しました。"
  "first-time-&optional" "初めてのオプショナル引数を取得しました。"
  "first-time-&rest" "初めての可変長引数を取得しました。"
  "first-time-defgeneric" "初めての総称関数定義を取得しました。"
  "first-time-defmacro" "初めてのマクロ定義を取得しました。"
  "first-time-defconstant" "初めての定数定義を取得しました。"
  "first-time-defclass" "初めてのクラス定義を取得しました。"
  "first-time-defvar" "初めてのVAR定義を取得しました。"
  "setq-principleists" "称号：SETQ原理主義者を取得しました。"
  "first-time-defparameter" "初めてのPARAMETER定義を取得しました。"
  "first-time-defpackage" "初めてのパッケージ定義を取得しました。"
  "first-time-define-condition" "初めてのコンディション定義を取得しました。"
  "first-time-defstruct" "初めての構造体定義を取得しました。"
  "first-time-eval-when" "称号：マクロ中級者を取得しました。"
  "first-time-deftype" "初めての型定義を取得しました。"
  "first-time-define-compiler-macro" "初めてのコンパイラマクロ定義を取得しました。"
  ":q" "Trophy REPLを抜けてCommon Lisp REPLに戻る。"
  "?" "特殊コマンドをプリント。"
  ":a" "実績を確認。"
  ":d" "辞書情報を確認。"
  ":l" "言語の切り替え。"
  "achievements-are-done" "の実績が開放されました。"
  "check-dictionary?" "~%辞書を確認するにはその名前を入力してください。~%終了するには :q を入力してください。~%>> "
  "check-others?" "他の辞書も確認しますか？"
  "congratulations-aa" "　　　　　　　　。 ◇◎｡ｏ.:O☆οo.
　　　　　　　。:゜ ◎::O☆∧∧☆｡∂:o゜
　　　　　　 ／。○。 ∂( ﾟДﾟ)O◇。☆
　　　　　／　　◎|￣￣∪￣∪￣￣￣|:◎:
　　　 ／　 　 ☆｡|　おめでとう! |☆
　 ▼ 　 　 。○..ｉｏ.｡◇.☆＿＿＿＿| 。.:
∠▲―――――☆ :∂ｉo☆ ゜◎∂:."
  "gatcha-aa" "　　　　 ∧＿∧
　／＼（　・∀・）／ヽ
（ ●　と　　　つ　● ）
　＼/⊂、　 　ノ　＼ノ
　　　　　し’"
  "explain-:d" "Trophy REPL内で:dと入力すれば開放された辞書を確認できます。"
  "welcome-aa" "　　　　　　　　　　　　ようこそLispの世界へ。
　 　　　　  ∧＿∧　　このﾃｷｰﾗは私のｵｺﾞﾘだ
　　　　 　 (`･ω･´)　　ｼｭｯ
　　　　　　（つ 　　と彡 ./
　　　　　　　　　　／ 　./
　　　　　　　 　／　 　./
　　　　　　　／//　  　/
　　　　　 ／　旦　　 /
　　 　 ／　 　　　　/"
  "explain-?" "ヘルプを参照するにはTrophy REPL内で'?'と入力してください。"
  "explain-:a" "実績を確認するにはTrophy REPL内で':a'と入力してください。"
  "explain-debugger" "この後デバッガREPLに入ります。
ABORTというリスタートを選択することでREPLに戻れます。"
  "tips-is-added" "Tipsが追加されました。"
  "explain-:t" "開放されたTipsを確認するにはTrophy REPL内で':t'と入力してください。"
  "no-tips" "Tipsはありません。"
  "see-tips?" "~%Tipsを見るには名前を入力してください。~%終了するには':q'と入力してください。~%>> "
  "check-others?" "他のも見ますか？")