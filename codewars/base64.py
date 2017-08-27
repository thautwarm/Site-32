#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Aug 26 23:09:30 2017

@author: misakawa
"""

charsets = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
andThen  = lambda *f : lambda x: x if not len(f) else andThen(*f[1:])(f[0](x))
arr2str = ''.join
def complete(n):
    def _1(char):
        class _2:
            def __init__(self):
                self.char          = char
                self.complete_to    = n
            def __call__(self, string):
                if len(string) == self.complete_to : return string
                has_completed  = self.complete_to-len(string) 
                return string+self.char*(has_completed)
        return _2()
    return _1

parser = lambda f: lambda string, parsed, ok=None :  f(string, parsed, ok)
parser_continue = lambda  string, parsed, ok=None : (string, parsed, True)

toInt   = lambda f: lambda bit_max: lambda string : sum([f(ord(it))*bit_max**i for i,it in enumerate(string[::-1])])  
toAscii = lambda f: lambda bit_max: lambda integer, ret = []: ret if integer ==0 else toAscii(f)(bit_max)( integer//(bit_max), [f(integer%(bit_max))]+ret)

ascii256_to_int = toInt(lambda x:x)(256)
_toAscii64  = toAscii(lambda x:charsets[x])(64)
ascii256_complete = complete(3)('\0')

ascii64_to_int  = toInt(lambda x:x)(64)
_toAscii256  = toAscii(lambda x:chr(x))(256)





def append(seq, item):
    seq.append(item)
    return seq
chunk  = lambda n : lambda f: parser(lambda string, parsed, ok:  (string[n:], append(parsed,f(string[:n])), True) if not ok else parser_continue(string, parsed))
parser_combine = lambda *f : lambda string, parsed, ok:(string,parsed, False)  if not len(f) or ok else parser_combine(*f[1:])(*f[0](string,parsed, False)) 

def _from_ascii256(string):
    if len(string) == 3:
        return andThen(ascii256_to_int, _toAscii64, arr2str)(string)
    n_addi = 3- len(string)
    ret = andThen(ascii256_complete, ascii256_to_int, _toAscii64, arr2str)(string) 
    ret = ret[:-n_addi]
    return ret

def _from_base64(string):
    rep = lambda x:x.replace('=','A')
    n   = 4 - len(string)
    if n is not 0:
        string += "A"*n
    str_to_ascii64  =  andThen(rep, lambda str : map(charsets.index, str), lambda arr: map(chr, arr), arr2str)
    str_to_ascii256 =  andThen(ascii64_to_int, _toAscii256)
    ret = andThen(str_to_ascii64, str_to_ascii256, arr2str)(string) 
    return ret if n is 0 else ret[:-n]
    

chunk_encode = chunk(3)(_from_ascii256)
chunk_decode = chunk(4)(_from_base64)

parsing = lambda f : lambda string, parsed, ok =None : ''.join(parsed) if not string else  parsing(f)(*f(string, parsed, False))

to_base_64   = lambda string: parsing(chunk_encode)(string, [])
from_base_64 = lambda string: parsing(chunk_decode)(string, [])
