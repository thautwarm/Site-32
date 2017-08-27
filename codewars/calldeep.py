#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Aug 26 21:13:46 2017

@author: misakawa
"""


def add(a, b):
  return a + b
  

def add_ten(a):
  return add(a, 10)



def misc_fun():
  return add(add_ten(3), add_ten(9))



class tp(list):
    def __init__(self, ret, *args, **kwargs):
        super(tp, self).__init__(*args, **kwargs)
        self.ret = ret
    def __iter__(self):
        try:
            self.ok 
        except:
            self.ok = True
            self.ret[0]+=1
            return super(tp, self).__iter__()
        return super(tp, self).__iter__()
    
            

import sys   
sys.setrecursionlimit(1000000)
def count(func, ret):
    def _f(*args, **kwargs):
        ret[0]+=1
        return func(*args, **kwargs)
    return _f
import inspect
def count_calls(func, *args, **kwargs):
  """Count calls in function func"""
  ret = [0]
  glob = globals()
  for key in glob.keys():
      if inspect.isfunction(glob[key]) and key not in  (count.__name__, count_calls.__name__):
        glob[key]  = count(glob[key], ret)
  rv = func(*args, **kwargs)
  return ret[0], rv





