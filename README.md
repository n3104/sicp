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

### neil/sicpについて
[http://planet.racket-lang.org/package-source/neil/sicp.plt/1/18/main.rkt](http://planet.racket-lang.org/package-source/neil/sicp.plt/1/18/main.rkt) がソースファイル。ここを読むといろいろ分かる。

* 先頭に #lang r5rs と書かれているように、neil/sicp は r5rs ベース。
	* 同じ名前で<code>define</code>は出来ない。再定義する場合は<code>set!</code>を利用する。関数も再定義可能。
* [sicp/e_3_06.rkt at master · n3104/sicp](https://github.com/n3104/sicp/blob/master/e_3_06.rkt) に記載しているように<code>#%require</code>を用いることでracket/base から任意の関数を追加可能。
