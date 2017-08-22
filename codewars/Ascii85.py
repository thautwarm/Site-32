#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Aug 22 20:06:23 2017

@author: misakawa
"""

import re
re_clean = re.compile('[\n\t ]')
box     = lambda x: "<~{}~>".format(x)
unbox   = lambda x: x[2:-2]
arr2str = lambda arr : ''.join(map(chr, arr)) 
andThen  = lambda *f : lambda x: x if not len(f) else andThen(*f[1:])(f[0](x))

def complete(n):
    def _1(char):
        class _2:
            def __init__(self):
                self.char          = char
                self.complete_to    = n
            def __call__(self, string):
                if len(string) == self.complete_to : return (string, 0)
                has_completed  = self.complete_to-len(string) 
                return (string+self.char*(has_completed), has_completed)
        return _2()
    return _1



dual = lambda f: lambda g: lambda *a: g(*(f(i) for i in a))
identity= lambda x:x


toInt   = lambda f: lambda bit_max: lambda string : sum([f(ord(it))*bit_max**i for i,it in enumerate(string[::-1])])  
toAscii = lambda f: lambda bit_max: lambda integer, ret = []: ret if integer ==0 else toAscii(f)(bit_max)( integer//(bit_max), [f(integer%(bit_max))]+ret)
_toAscii85_arr = andThen(toInt(identity)(256), toAscii(lambda x:x+33)(85))
_fromAscii_arr =_fromAscii85_arr = andThen(toInt(lambda x:x-33)(85), toAscii(identity)(256))

complete_Ascii256 =  complete(4)('\0')
complete_Ascii85  =  complete(5)('z')

def append(seq, item):
    seq.append(item)
    return seq

autocomplete = lambda n :lambda char: lambda string : string if len(string) == n else  char*(n-len(string))+string
autocomplete_to85 = autocomplete(5)('!')
autocomplete_from85 = autocomplete(4)('\0')

def _to85(string : "normal_len : 4 | left_len : <4"):
    if len(string) == 4:
        return  andThen(_toAscii85_arr, arr2str, autocomplete_to85)(string)
    else:
        if all(map(lambda x:x == '\0', string)):
            return '!!'+'!'*(len(string)-1)
        string, has_completed = complete_Ascii256(string)
        return andThen(_toAscii85_arr, lambda x:x[:complete_Ascii85.complete_to-has_completed], arr2str)(string)
    
def _from85(string: 'normal_len: 5 | z: 1 ' ):
    length = len(string)
    if length == 5:
        return andThen(_fromAscii85_arr, arr2str, autocomplete_from85)(string)
    else:
        if all(map(lambda x:x == '!', string)):
            return '\0'*(len(string)-1) 
        string, has_completed = complete_Ascii85(string)
        return andThen(_fromAscii85_arr,lambda x:x[:complete_Ascii256.complete_to-has_completed],  arr2str)(string)
    
    




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



fromAscii85  = lambda string : print([ord(i) for i in string]) or  andThen( unbox, lambda str:_fromAscii85(str, []))(re_clean.sub('',string))
toAscii85    = lambda string : print([ord(i) for i in string]) or  andThen(lambda str:_toAscii85(str, []), box)(string)















