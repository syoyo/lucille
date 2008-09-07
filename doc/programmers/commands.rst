.. highlightlang:: c
.. _xref_commands: 


lucille レンダラコアライブラリ
------------------------------

lucille は RIB インターフェイスを実装したレンダラとして、ユーザプログラムにレンダラコア部のライブラリをリンクして利用することができる. ユーザプログラムは以下のライブラリをリンクする.

* libribase
* libridisplay
* libriimageio
* librirender
* libritransport
* libriri

lsh(lucille shell)
------------------

lsh は、上記のレンダラライブラリと、RIB ファイルのパーザを結合したコマンドラインプログラムである. RIB ファイルを入力として受け取り、レンダリング処理を行うことができる.

Usage
~~~~~

::

  $ lsh [options] <input.rib>

オプションは以下の通りである.

::

  --debug             デバッグを有効にする(開発者向け)
  --output            出力ファイル名を指定する(Display の指定を上書き)     
  --info              利用可能なディスプレイドライバを表示
  --version           バージョン表示　
  --verbose           各種情報を表示するようにする
  
  


rockenfield
-----------

lucille 向けフレームバッファドライバである. ``$(top)/tools/rockenfield`` にそのコードが置かれている. lsh と TCP/IP 通信でレンダリング結果を受け取る. TCP/IP でやりとりを行うため、たとえばレンダリング処理をサーバで行い、レンダリングの確認はローカルのマシンで表示させるなどのネットワーク越しの構成で利用することができる.

rockenfield は FLTK を利用している. コンパイルするにはあらかじめ FLTK 1.1.9 がシステムにインストールされている必要がある(`http://www.fltk.org/ <http//www.fltk.org/>`_).

Usage
~~~~~

::

  $ rockenfield
