
example1= do
    -- takeWhile
    -- takeWhile:: (a->Bool)->[a]->[a]
    takeWhile (/='1') "2312"
-- "23"
example2= do
    -- take
    -- take:: Int -> [a]->[a]
    take 1 [1,2,3] 
--1


exfloor::Float->Integer -- 对负数失效

exfloor=read.takeWhile(/='.').show
exmaple3= exfloor 1.2
-- 1.0

exuntil::(a->Bool)->(a->a)->a->a

exuntil p f x= if p x then x else exuntil p f (f x)
example4 = exuntil (\x->( ((mod x 2)==1) &&(x>30)) ) (\x-> x + 1 + div x 2) 1
-- 61





