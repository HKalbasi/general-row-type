# general-row-type

this repository is a try to solve [this issue](https://github.com/purescript/purescript/issues/3673).

## example

```
> r1 = =a 5 {}
r1: { a :: Int  }
> r2 = =b true r1
r2: { b :: Bool , a :: Int  }
> r3 = =b 2 r2
r3: { b :: Int , a :: Int  }
> =b
Forall ['t0,'t1] ( 't0 -> ( { b :: ? | 't1 } -> { b :: 't0 | 't1 } ) )
> .b r3 
Int
> .b r1
can not match
    { b :: 't1 | 't2 }
with
    { a :: Int  }
> f = \ r =c ( isEqual ( .a r ) ( .b r ) ) r
f: Forall ['t0] ( { a :: Int , b :: Int , c :: ? | 't0 } -> { c :: Bool , a :: Int , b :: Int | 't0 } )
> f r3
{ c :: Bool , a :: Int , b :: Int  }
> f r2
can not match
    Int
with
    Bool
> f r1
can not match
    { b :: Int , c :: ? | 't1 }
with
    {  }
> compose = \ f ( \ g ( \ x f ( g x ) ) )
compose: Forall ['t0,'t1,'t2] ( ( 't0 -> 't1 ) -> ( ( 't2 -> 't0 ) -> ( 't2 -> 't1 ) ) )
> f = \ x plus x 2
f: ( Int -> Int )
> g = \ x isEqual x 5
g: ( Int -> Bool )
> compose f g
can not match
    Int
with
    Bool
> compose g f
( Int -> Bool )
> compose =a =b
Forall ['t0,'t1,'t2] ( 't0 -> ( { a :: ? | 't1 } -> { a :: ( { b :: ? | 't2 } -> { b :: 't0 | 't2 } ) | 't1 } ) )
> compose ( =a 2 ) ( =b true )
Forall ['t0] ( { b :: ? , a :: ? | 't0 } -> { a :: Int , b :: Bool | 't0 } )
> addAB = compose ( =a 2 ) ( =b true )
addAB: Forall ['t0] ( { b :: ? , a :: ? | 't0 } -> { a :: Int , b :: Bool | 't0 } )
> addAB {}
{ a :: Int , b :: Bool  }
> addAB ( =z ( \ x x ) {} )
Forall ['t0] { a :: Int , b :: Bool , z :: ( 't0 -> 't0 )  }
> writeABtoC = \ r =c ( ( .a r ) ( .b r ) ) r
writeABtoC: Forall ['t0,'t1,'t2] ( { a :: ( 't0 -> 't1 ) , b :: 't0 , c :: ? | 't2 } -> { c :: 't1 , a :: ( 't0 -> 't1 ) , b :: 't0 | 't2 } )
```

## grammar

```
<top-level> -> @exp | @str = @exp 
@exp -> @exp @exp -- apply function -- | \ @str @exp -- lambda -- | ( @exp ) -- space is neccesery -- | @int
@str -> (a-z)* | (A-Z)*
@int -> (0-9)*
```

## defined values

```
> {}
{  }
> true
Bool
> false
Bool
> plus
( Int -> ( Int -> Int ) )
> and
( Bool -> ( Bool -> Bool ) )
> isEqual
( Int -> ( Int -> Bool ) )
```

## Notes
* code can be buggy 
* thanks to [write-you-a-haskell](https://github.com/sdiehl/write-you-a-haskell), the base of code is equal to chapter 7


