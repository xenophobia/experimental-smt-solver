# Experimental SMT solver
気になったSMTソルバを実装していく用スペース

# 現状あるやつ
READMEも各々のフォルダに散らすべきでしょう。まぁおいおいやっていく感じで。

## Bit-blasting
参考資料: http://www.decision-procedures.org/slides/bit-vectors.pdf

 * 整数をビットベクタで表現してSATに落としこみ、あとはSATソルバに丸投げする。
 * 無限長のベクタに落とすわけではないので、「与えられたビット幅に制限された演算において」充足可能か不可能かを判定する。
 * あるビット幅で充足可能解が見つからなかったらビット幅を増やしてまた試す、という方式を使うことで、解がもしあれば必ず見付かるsemi-decidableなアルゴリズムを構成可能。

```
$ bitblasting
X1 + X2 = 200 & X1 + 300 = X2
Constraint: ((Var 1 :+: Var 2) :=: Const 200) :&: ((Var 1 :+: Const 300) :=: Var 2)
Satisfiable by: fromList [(1,-50),(2,250)]
```

対応している制約
```
Constraint := Constraint & Constraint
            | Term = Term

      Term := n (Integer: nは自然数)
            | Xn (Variable: nは自然数)
            | Term + Term
```
