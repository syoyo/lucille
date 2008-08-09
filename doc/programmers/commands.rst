.. highlightlang:: c
.. _xref_commands: 


lucille レンダラコアライブラリ
------------------------------

lucille は RIB インターフェイスを実装したレンダラとして、ユーザプログラムにレンダラコア部のライブラリをリンクして利用することができる. ユーザプログラムは以下のライブラリをリンクする.

* libribase
* libridisplay
* librirender
* libritransport
* libriri

lsh(lucille shell)
------------------

lsh は、上記のレンダラライブラリと、RIB ファイルのパーザを結合したコマンドラインプログラムである. RIB ファイルを入力として受け取り、レンダリング処理を行うことができる. 詳細は :ref:`xref_cmd_lsh` を参照のこと.


rockenfield
-----------

lucille 向けフレームバッファドライバである. ``$(top)/tools/rockenfield`` にそのコードが置かれている. lsh と TCP/IP 通信でレンダリング結果を受け取る. TCP/IP でやりとりを行うため、たとえばレンダリング処理をサーバで行い、レンダリングの確認はローカルのマシンで表示させるなどのネットワーク越しの構成で利用することができる.

rockenfield は FLTK を利用している. コンパイルするにはあらかじめ FLTK 1.1.9 がシステムにインストールされている必要がある(`fltk <http;//www.fltk.org/>`_).
