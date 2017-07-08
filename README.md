# sicp

http://sicp.iijlab.net/ の問題の答えを載せています。

* DrRacketを利用しています。
* https://wizardbook.wordpress.com/ を参考にしつつ、実際に実行できるように全コードを載せています。
* 問題 2.87 以降について公開しています。過去分は気が向いたら公開します。

## DrRacketについて
Download Racket
https://download.racket-lang.org/

* ダウンロードして展開し、アプリケーションにコピーするだけ。
    * DrRacket以外にも関係ないものがいくつかインストールされるのが気になるが。。
* 起動後は[言語]-[Other]-[R5RS]にするだけ。 -> 言語はデフォルトの The Racket Language のままでいい。
    * R5RS だと true, false の定義などが存在しない。後述の planet neil/sicp を利用する。
    * デフォルトのThe Racket Languageが #lang を利用するという意味になる模様。
* ソースの先頭に以下を追加する。
    * #lang planet neil/sicp
    * http://www.neilvandyke.org/racket/sicp/
* ステップ実行も出来て、実行時エラーもどこで失敗したかも分かり、とても使いやすい！
    * まずデバッグ実行する。この状態でステップ実行が可能になる。
    * ブレークポイントはデバッグ実行中にソースコードをマウスオーバーすると赤い点がでるので、そこを右クリックすればいい。
