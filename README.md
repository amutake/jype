Jype - JSON Data Type Format
============================

Jype is a format to represent data structure of JSON.

This repository contains some packages.

- jype
  - jype parser and validator library
- jype-html
  - HTML generator from jype files

Features
--------

- algebraic data types
- polymorphic data types
- simple validator

Installing
----------

```
% for d in jype jype-html; do cd $d; cabal install; cd ../; done
```

Examples
--------

(jype format)

```
user = {
  id: int
  name: string
  description: nullable[string]
  language: language
}

language = "en" | "ja" | "fr"
```

This means:

(json format)

```json
{
  "id": 0,
  "name": "foobar",
  "description": null,
  "language": "ja"
}
```

Future Work
-----------

- aeson の `Value` 型の値が、jype の値になっているかどうかチェックするもの
- REST API のドキュメンテーションツール
