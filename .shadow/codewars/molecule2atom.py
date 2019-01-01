#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Aug 27 12:59:57 2017

@author: misakawa
"""
import re
from collections import defaultdict
tokenize  = lambda s: re.compile(s).findall
isnum    = re.compile('\d+').match
bracket = {'(':')','[':']','{':'}'}
end    = [')',']','}']

def checkMultiple(parsed, token, ch=None):
    if len(token) and isnum(token[-1]):
        N = int(token.pop())
        if not ch:
            for it in tuple(parsed.keys()):
                parsed[it] *= N
        else:
            parsed[ch] += N-1

def parse(token):
    parsed = defaultdict(int)
    while token:
        item = token.pop()
        if item.isalpha():
            parsed[item]+=1
            checkMultiple(parsed, token, ch=item)
        elif item in end:
            checkMultiple(parsed, token, ch = None)
            return parsed
        elif item in bracket:
            parsed_r = parse(token)            
            for it in parsed_r:
                parsed[it]+=parsed_r[it]
    return parsed

def parse_molecule(string):
    token = tokenize('[A-Z][a-z]*|\d+|[()\[\]{}]')(string)
    token.reverse()
    return parse(token)