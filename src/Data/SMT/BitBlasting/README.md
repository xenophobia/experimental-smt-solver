# Bit-blasting
参考資料: http://www.decision-procedures.org/slides/bit-vectors.pdf

 * 整数をビットベクタで表現してSATに落としこみ、あとはSATソルバに丸投げする。
 * 無限長のベクタに落とすわけではないので、「あるビット幅に制限された演算において」充足可能か不可能かを判定する。
 * あるビット幅で充足可能解が見つからなかったらビット幅を増やして再試行する。デフォルトの設定では2から10まで。

```
$ bitblasting
X1 * X2 * X3 = 30 & X1 + X2 + X3 = 0 & X1 < X2 & X2 < X3
Constraint: X1*X2*X3=30&X1+X2+X3=0&X1<X2&X2<X3
Satisfiable by:
X1 = -5
X2 = -1
X3 = 6
```

対応している制約
```
Constraint := Constraint & Constraint
            | Term = Term
			| Term < Term
			| Term > Term

      Term := n (Integer: nは自然数)
            | Xn (Variable: nは自然数)
			| -Term
            | Term + Term
			| Term * Term
```
