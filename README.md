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
    * デフォルトのThe Racket Languageが `#lang` を利用してソースファイル内で実行する言語を指定するという意味になる模様。
* ソースの先頭に以下を追加する。
    * `#lang planet neil/sicp`
    * http://www.neilvandyke.org/racket/sicp/
* ステップ実行も出来て、実行時エラーもどこで失敗したかも分かり、とても使いやすい！
    * まずデバッグ実行する。この状態でステップ実行が可能になる。
    * ブレークポイントはデバッグ実行中にソースコードをマウスオーバーすると赤い点がでるので、そこを右クリックすればいい。

### neil/sicpについて
[http://planet.racket-lang.org/package-source/neil/sicp.plt/1/18/main.rkt](http://planet.racket-lang.org/package-source/neil/sicp.plt/1/18/main.rkt) がソースファイル。ここを読むといろいろ分かる。

* 先頭に #lang r5rs と書かれているように、neil/sicp は r5rs ベース。
	* 同じ名前で `define` は出来ない。再定義する場合は `set!` を利用する。関数も再定義できる。
* [sicp/e_3_06.rkt at master · n3104/sicp](https://github.com/n3104/sicp/blob/master/e_3_06.rkt) に記載しているように `#%require` を用いることで racket/base などRacketの任意の関数を追加できる。
* [sicp/e_4_55.rkt at master · n3104/sicp · GitHub](https://github.com/n3104/sicp/blob/master/e_4_55.rkt) に記載しているよう `include` を用いることでコードを外部ファイル化できる。
	* 読み込み対象である [sicp/c_4_4.rkt at master · n3104/sicp](https://github.com/n3104/sicp/blob/master/c_4_4.rkt) の先頭の `#lang` はコメントアウトする必要があった。そのままにしておくとエラーになった。
	* [12.10 File Inclusion](https://docs.racket-lang.org/reference/include.html) が公式ドキュメント。
* [sicp/e_4_55.rkt at master · n3104/sicp · GitHub](https://github.com/n3104/sicp/blob/master/e_4_55.rkt) に記載しているようにシンボルをケースセンシティブにしたい場合はシンボルの先頭に `#cs` を付与する必要がある。
	* [1.3 The Reader](https://docs.racket-lang.org/reference/reader.html#%28part._parse-symbol%29) が公式ドキュメント。
	* [13.4 Reading](https://docs.racket-lang.org/reference/Reading.html#%28def._%28%28quote._~23~25kernel%29._read-case-sensitive%29%29) に記載のある `read-case-sensitive` 関数でケースセンシティブに評価するかどうかを本来は指定可能だが、[3 R5RS Module Language](https://docs.racket-lang.org/r5rs/r5rs-mod.html)に記載の通り `(read-case-sensitive #f)` で初期化した上で変更できないようになっているので neil/sicp では個々のシンボルに `#cs` を付与する必要がある。
