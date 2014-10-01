# jype-doc

## user

user type

**Record Type**

| key | type | description |
| --- | ---- | ----------- |
| id | int |  |
| name | string |  |

## list_range

range of list

**Record Type**

| key | type | description |
| --- | ---- | ----------- |
| limit | nullable[int] | limit of the number of list |
| offset | nullable[int] | offset list |

## complex

complex data type

**Record Type**

| key | type | description |
| --- | ---- | ----------- |
| hoge | either[either[a, b], c] | hogehoge |
| fuga | user | fugafuga |

## bool

Boolean type

**Union Type**

| type/value | description |
| ---------- | ----------- |
| true |  |
| false |  |

## nullable

nullable[a] represents either just type 'a' or value 'null'.

**Union Type**

| type/value | description |
| ---------- | ----------- |
| a |  |
| null |  |

## either

either[a,b] represents either a type 'a' or a type 'b'.

**Union Type**

| type/value | description |
| ---------- | ----------- |
| a | left value |
| b | right value |

## unit

Unit type

**Record Type**

| key | type | description |
| --- | ---- | ----------- |


## pair

pair type

**Record Type**

| key | type | description |
| --- | ---- | ----------- |
| car | a | first value |
| cdr | b | second value |

## int

Integer type

**Primitive Type**

## float

Float type

**Primitive Type**

## string

String type

**Primitive Type**

## array

Array type of type 'a'

**Primitive Type**

## option

option[a] represents either just type 'a' or there is no value.

**Primitive Type**