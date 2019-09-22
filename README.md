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
* 起動後は[言語]の設定はデフォルトのThe Racket Languageのままでいい。
    * デフォルトのThe Racket Languageが<code>#lang</code>を利用してソースファイル内で実行する言語を指定するという意味になる模様。
* ソースの先頭に以下を追加する。
    * <code>#lang planet neil/sicp</code>
    * http://www.neilvandyke.org/racket/sicp/
* ステップ実行も出来て、実行時エラーもどこで失敗したかも分かり、とても使いやすい！
    * まずデバッグ実行する。この状態でステップ実行が可能になる。
    * ブレークポイントはデバッグ実行中にソースコードをマウスオーバーすると赤い点がでるので、そこを右クリックすればいい。

### neil/sicpについて
[http://planet.racket-lang.org/package-source/neil/sicp.plt/1/18/main.rkt](http://planet.racket-lang.org/package-source/neil/sicp.plt/1/18/main.rkt) がソースファイル。ここを読むといろいろ分かる。

* 先頭に #lang r5rs と書かれているように、neil/sicp は r5rs ベース。
	* 同じ名前で<code>define</code>は出来ない。再定義する場合は<code>set!</code>を利用する。関数も再定義できる。
* [sicp/e_3_06.rkt at master · n3104/sicp](https://github.com/n3104/sicp/blob/master/e_3_06.rkt) に記載しているように<code>#%require</code>を用いることで racket/base などRacketの任意の関数を追加できる。
* [sicp/e_4_55.rkt at master · n3104/sicp · GitHub](https://github.com/n3104/sicp/blob/master/e_4_55.rkt) に記載しているよう<code>include</code>を用いることでコードを外部ファイル化できる。
	* 読み込み対象である [sicp/c_4_4.rkt at master · n3104/sicp](https://github.com/n3104/sicp/blob/master/c_4_4.rkt) の先頭の<code>#lang</code>はコメントアウトする必要があった。そのままにしておくとエラーになった。
	* [12.10 File Inclusion](https://docs.racket-lang.org/reference/include.html) が公式ドキュメント。
