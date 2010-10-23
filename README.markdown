# README
## 概要
- Erlangの項(のバイナリ表現)とCommon Lispのオブジェクトの相互変換を行う
- Common LispとErlangのポート経由(open_port,port_command,etc)での通信時の利用を想定

## 注意事項
- 文字列を扱う場合、使用するCommon Lispの処理系がユニコード(UTF-32)に対応していない場合、文字化けが生じる可能性がある
  - 一度バイナリに変換してやりとりを行うなら問題ない

- DestributionHeader,ATOM_CACHE_REF及び圧縮形式には未対応
  - 上記のいずれもポート経由で通信を行う場合は不要なので、今回は実装から外している

## インストール方法
    * (require :asdf)
    * (require :asdf-install)
    * (asdf-install:install "erlterm-0.0.1.tar.gz")

## API
#### Package# erlterm
     > メインパッケージ

#### Function# (decode-term binary-input-stream &key (packet nil)) => term
     > Erlang項のバイナリエンコーディングをCommon Lispのオブジェクトにデコードする
     > binary-intput-stream: 入力元のバイナリストリーム
     > packet: バイナリデータ本体の前に、受信するバイトの長さが付与されている場合は、そのサイズデータのバイト長を指定する。
     >         サイズデータが付与されていない場合はnilを指定する。
     >         (member nil 1 2 4)
     >         ※ 現バージョンでは、このサイズデータは単に読み捨てられている
     > term: デコードされたCommon Lispオブジェクト

#### Function# (encode-term term binary-output-stream &key (packet nil) (minor-version 0)) => t
     > Common LispのオブジェクトをErlang項のバイナリフォーマットにエンコードする
     > term: エンコード対象となるオブジェクト
     > binary-output-stream: バイナリデータ出力先のストリーム
     > packet: バイナリデータ本体の前に、出力バイト数を付与する場合は、そのサイズデータに用いるバイト長を指定する。
     >         サイズデータを付与しない場合はnilを指定する。
     >         (member nil 1 2 4)
     > minor-version: バイナリフォーマットのマイナーバージョン。
     >                0 or 1。
     >                1の場合は浮動小数点数のエンコーディングに、新しいフォーマットが使用される。

## 参照
- Erlang項のバイナリフォーマット
  - <http://www.erlang.org/doc/apps/erts/erl_ext_dist.html>

## 使用例

## TODO
- 全体的な最適化
- コード整理
- 包括的なテスト
- 未実装機能の実装
- 適切なエラー処理
