# Bit-blasting
参考資料: http://www.decision-procedures.org/slides/bit-vectors.pdf

 * 整数をビットベクタで表現してSATに落としこみ、あとはSATソルバに丸投げする。
 * 無限長のベクタに落とすわけではないので、「あるビット幅に制限された演算において」充足可能か不可能かを判定する。(デフォルトだと10まで)
 * あるビット幅で充足可能解が見つからなかったらビット幅を増やしてまた試す、という方式を使うことで、解がもしあれば必ず見付かるsemi-decidableなアルゴリズムを構成可能。

```
$ bitblasting
X1+X2+X3=10 & X1+X3<5
Constraint: X1+X2+X3=10&X1+X3<5
Satisfiable by:
X1 = 2
X2 = 8
X3 = 0
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
```
