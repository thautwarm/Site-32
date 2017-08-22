


[我的codewars页面](https://www.codewars.com/users/thautwarm/completed_solutions)

------------------------


Python
-------

1. Codewars-ref:  [The position of a digital string in a infinite digital string](https://www.codewars.com/kata/the-position-of-a-digital-string-in-a-infinite-digital-string/train/python)

    - Solution: [Here](https://github.com/thautwarm/My-Blog/blob/master/codewars/The-position-of-a-digital-string-in-a-infinite-digital-string.py)

    - 有趣之处在于定义一种数据结构。

    ```python
    class level_dict:
        level = dict({0:0})
        sumal = dict({0:0})
        maxmal = 0
        def __new__(self):
            return self
        def index(v):
            v-=1
            level = level_dict.level
            sumal = level_dict.sumal
            if v < 0 : return 0
            length = len(str(v))
            if length-1 not in level:
                for i in range(level_dict.maxmal+1, length):
                    level[i] = i*9*(10**(i-1))
                    sumal[i] = sumal[i-1]+level[i]
                level_dict.maxmal = length-1
            begin = int("1"+"0"*(length-1))
            return sumal[length-1]+(v - begin+1)*length

    ```
    - 对于这样一个无限可数数列，
    ```Tex
    {1,2,3,4,5,6,7,8,9, 
    1,0, 1,1, 1,2, 1,3 , 1,4, ...
    1,0,0, 1,0,1, 1,0,2, 1,0,3, ...}
    ```

    经过观察发现，可以通过以下表格表示：

<center>

| 位数 | 个数 | 间隔 |
| ------| ------ | ------ |
| 1 | 9 | 1 |
| 2 | 90 | 2 |
| 3 | 900 | 3 |
| ... | ... | ...|

</center> 

-  其他 

    注意到这样一个数列其实是正整数的字符串拆分，比如`97`，将被拆分为`9,7`两个元素放到数列中。  

    此题里需要一个函数f(x) = `x的拆分项的第一个在整个数列中的位置`， 而这个函数即是`level_dict.index`

2. Codewars-ref: [Ascii85](https://www.codewars.com/kata/ascii85-encoding-and-decoding)

    - Solution: [Here](https://github.com/thautwarm/My-Blog/blob/master/codewars/Ascii85.py)

    - 领悟parser的美好？感受一下下面的代码？

    ```python

    autocomplete = lambda n :lambda char: lambda string : string if len(string) == n else  char*(n-len(string))+string
    autocomplete_to85   = autocomplete(5)('!')
    autocomplete_from85 = autocomplete(4)('\0') 

    parser = lambda f: lambda string, parsed, ok=None :  f(string, parsed, ok)
    parser_continue = lambda  string, parsed, ok=None : (string, parsed, True)

    chunk        = lambda n : lambda f: parser(lambda string, parsed, ok:  (string[n:], append(parsed,f(string[:n])), True) if not ok else parser_continue(string, parsed))

    chunk_encode = chunk(4)(_to85  )
    chunk_decode = chunk(5)(_from85)

    parser_combine = lambda *f : lambda string, parsed, ok:(string,parsed, False)  if not len(f) or ok else parser_combine(*f[1:])(*f[0](string,parsed, False)) 

    zcompress    = parser(lambda string, parsed, ok: parser_continue(string, parsed)   if ok  else zcompress_ok(string ,parsed  ))
    zcompress_ok = parser(lambda string, parsed, ok=None:(string[4:], append(parsed, "z"   ),   True)  if string.startswith("\0"*4) else (string, parsed, False))


    zdecompress    = parser(lambda string, parsed, ok: parser_continue(string, parsed) if ok  else zdecompress_ok(string, parsed))  
    zdecompress_ok = parser(lambda string, parsed, ok=None:(string[1:], append(parsed, "\0"*4), True)  if string.startswith('z')    else (string, parsed, False))  


    encode = parser_combine(zcompress  , chunk_encode)
    decode = parser_combine(zdecompress, chunk_decode)

    parsing = lambda f : lambda string, parsed, ok =None : ''.join(parsed) if not string else  parsing(f)(*f(string, parsed, False))

    _fromAscii85 = parsing(decode)
    _toAscii85   = parsing(encode)

    ```




Java
-------
    
1. Codewars-ref:    [https://www.codewars.com/kata/binary-genetic-algorithms](https://www.codewars.com/kata/binary-genetic-algorithms)  

    - Solution: [Here](https://github.com/thautwarm/My-Blog/blob/master/codewars/binary-genetic-algorithms.java)
 
P.S :   
    
    JAVA 代码绝大部分用于编写函数式支持上，从186行开始才是解题过程。
    关于这些数据结构可见于下
    
  [个人习惯的FP支持](https://github.com/thautwarm/Stardust/tree/master/libexercise)