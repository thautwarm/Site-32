#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Aug 27 00:46:40 2017

@author: misakawa
"""

"""

chr stack_to:
    chr == matrix -> x_idxs, y_idxs
        if any i < - x,y from (x_idxs,y_idxs)[left: _x, _y => push options. tag] in condition:
                        1. max.eachdist (x,y) now < 1 
                        2. (x,y) not in stack_route 
                        
        
            options-push []
            chr stack_to
            stack_route-push now
            if stack_to empty -> ok
        else
            chr stack-recover
            
            try
                with options-pop goto tag
            except
                    END FALSE
                
    
"""
import numpy as np

def can_connect(*seq):
    x,y = seq[0]
    if len(set(seq)) != len(seq): 
        return False 
    for i,j in seq[1:]:
        if max( abs(x-i), abs(y-j)) >1:
            return False
        x,y = i,j
    return True

def f(version, n,  seq = [], deep=0):
    if deep is n:
        idxs = version[deep]
        for idx in idxs:
            seq[deep] = idx
            yield can_connect(*seq)
    else:
        idxs = version[deep]
        for idx in idxs:
            seq[deep] = idx
            yield from f(version, n, seq, deep+1)
def find_word(board, word):
    self = np.array(board)       
    n = len(word)-1
    seq  = [None for i in range(n+1)]
    
    seqs = [[]]
    version = [tuple(zip(*np.where(w==self))) for w in word]
    for p in version[0]:
        seqs[-1].append(p)
    for i,j in zip(range(0,n),range(1,n+1)):
        seqs_r = []
        for p1 in version[i]:
            if p1 not in seqs[-1]:continue
            for p2 in version[j]:
                if can_connect(p1,p2):
                    seqs_r.append(p2)
        seqs.append(seqs_r)
    version = seqs
    for item in f(version, n, seq, 0):
        if item:
            return True
    return False

#    n = len(word)
#    local = dict(np = np, can_connect = can_connect, to=word, self=self)
#    defhead = lambda :"def g():"
#    for_ = lambda i: "{indent}for i_{i} in zip(*np.where(to[{i}]==self)):".format(i = i, indent = " "*(i+1))
#    end_ = lambda n: "{indent}if can_connect({args}): return True".format(indent = " "*(n+1), args = ",".join(map(lambda i: "i_"+str(i),  range(n))))
#    codes= '\n'.join([defhead()]+[for_(i) for i in range(n)]+[end_(n)])
#    exec(codes,local)
#    if local['g']():
#        return True
prob = find_word([ ["I","L","A","W"],
  ["B","N","G","E"],
  ["I","U","A","O"],
  ["A","S","R","L"] ], "ILNBIA") ->> print