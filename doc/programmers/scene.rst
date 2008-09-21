.. highlightlang:: c
.. _xref_scene: 

シーンデータ
============

lucille では, 外部向けプログラム用に RIB(RenderMan) 形式のシーン読み込みをサポートしている.

lucille 内部では独自のシーン構造を定義しており、そのための内部シーンデータ構築 API を用意している.
つまり、シーンデータを定義する各 RIB 関数(たとえば RiLightSource() など)はこの内部シーンデータを構成する API 呼び出しを呼び出すように実装されてる.::

  RtLightHandle
  RiLightSourceV(RtToken name, RtInt n, RtToken tokens[], RtPointer params[])
  {
      return ri_api_light_source(name, n, tokens, params);
  }


ジオメトリ
----------

lucille では、シーンジオメトリはすべてポリゴン(三角形のみ. 四角形などの三角形以外のポリゴンはサポートされない)へ変換されて保持される. 曲面ジオメトリなどはポリゴンへと変換される. ただし曲面のサポートは十分ではないため、モデリングツールでシーンデータを出力する段階ですでにジオメトリをすべて三角形に変換しておくことを勧める.

ポリゴンには、頂点ごとにカラー、テクスチャ座標、法線(tangent)、接ベクトル(tangent)、従法線(binormal)を持つことができる.

ポリゴンのジオメトリデータ構造は ``src/render/geom.h`` に ``ri_geom_t`` として定義されている. それぞれのコンポーネント(座標、法線、カラーなど)を配列として持つフラットな構成となっている. また、ジオメトリに割り当てられたシェーダやマテリアルへのポインタも保持している. ::

  typedef struct _ri_geom_t {
      ri_vector_t   *positions;               /* vertex position(P)       */
      unsigned int   npositions;
      ri_vector_t   *normals;                 /* vertex normal(N)         */
      unsigned int   nnormals;
      ri_vector_t   *tangents;                /* tangent vector           */
      unsigned int   ntangents;
      ri_vector_t   *binormals;               /* binormal vector          */
      unsigned int   nbinormals;
      ri_vector_t   *colors;                  /* vertex color(Cs)         */
      unsigned int   ncolors;
      ri_vector_t   *opacities;               /* vertex opacity(Os)       */
      unsigned int   nopacities;
      RtFloat       *texcoords;               /* texture coordinates
                                               * (st, 2 floats)           */
      unsigned int   ntexcoords;
      unsigned int  *indices;                 /* vertex index             */
      unsigned int   nindices;
  
      char          *shadername;              /* surface shader name      */
      ri_shader_t   *shader;                  /* surface shader           */
      ri_material_t *material;
  
      int            two_side;                /* two-sided or not         */
      ri_float_t     kd;                      /* diffuse coefficient      */
      ri_float_t     ks;                      /* specular coefficient     */
  
  } ri_geom_t;


ポリゴンの各頂点データはインデックス形式でアクセスされる. たとえば 0 番目のポリゴンの頂点が欲しい場合は以下のようにして頂点データを取得することができる. ::

  ri_geom_t    *geom;
  unsigned int  idx[3];
  ri_vector_t   p[3];

  /* ポリゴンは三角形なので、3 頂点ぶんのインデックスが必要 */
  idx[0] = geom->indices[3 * 0 + 0];
  idx[1] = geom->indices[3 * 0 + 1];
  idx[2] = geom->indices[3 * 0 + 2];

  vcpy(p[0], geom->positions[idx[0]]);
  vcpy(p[1], geom->positions[idx[1]]);
  vcpy(p[2], geom->positions[idx[2]]);

  
indices にはインデックス値が格納されている. ポリゴンはすべて 3 角形として定義するので、n 番目の頂点に対応するインデックスを取り出す場合は 3n+0, 3n+1, 3n+2 とアクセスして取り出すことができる.
    

ライト
------

lucille では以下のライトをサポートしている

* エリアライト
* ドームライト(skylight, HDRI lighting)

ポイントライトはサポートされない.

エリアライト
------------

任意の三角形形状をエリアライトとして定義することができる. エリアライトを定義するジオメトリ情報は ``ri_geom_t`` と共通である. エリアライトの光源色は一定である.ポリゴンごとに設定することはできない.

エリアライトは以下のようにプログラミングする.::

  ri_geom_t  *geom  = ri_geom_new();
  
  ...    /* setup geometry */

  ri_light_t *light = ri_light_new();
  
  ri_light_attach_geom(light, geom);


ドームライト
------------

ドームライトは無限遠にある全球状のライトである. スカイライトや HDRI ライティング(テクスチャを光源とみなす手法)を行うのに適している.

