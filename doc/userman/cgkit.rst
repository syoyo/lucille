================
cgkit を利用する
================

ここでは、cgkit を利用して、Python で RenderMan シーンをスクリプティングし、結果を RIB ファイルに吐き出して lucille でレンダリングするという流れを説明します.

cgkit は Python で各種グラフィックスプログラミングを行うためのツールキットですが、その一部として RenderMan Interface バインディングが用意されています.

インストール
-------------

まず、cgkit を http://cgkit.sourceforge.net/ から入手してください. cgkit のコンパイルには C++ コンパイラや boost, boost-python などが必要になります.
