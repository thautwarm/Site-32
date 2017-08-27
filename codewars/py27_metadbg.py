#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Aug 27 21:17:33 2017

@author: misakawa
"""

class Debugger(object):
    attribute_acceses = []
    method_calls = []
    
import time
def wrapperf(self, func):
    def _f(*args, **kwargs):
        start = time.time()
        d = {'class':self.__class__,
             'method':func.__name__,
             'args': [],  
             'kwargs':kwargs,
             'time':0
                    }
        d['args'] = tuple([self]+list(args)) # fuck py27
        Debugger.method_calls.append(d)
        ret = func(*args, **kwargs)
        d['time'] = time.time()-start
        return ret
    return _f


def wrappera( k, v, self, action):
        d = {'class':self.__class__,
                 'action':action,
                 'attribute':k,
                 'value':v
                        } 
        Debugger.attribute_acceses.append(d)
        return v

def setter_hook(func):
    def fkpy27(self, name, value):
        wrappera(name, value, self, 'set')
        return func(self,name,value)
    return fkpy27

def getter_hook(func):
    def fkpy27(self, name):
        v =  func(self, name)
        if name in ('__class__','__name__'):
            return v
        if callable(v):
            wrappera(name, v, self, 'get')
            return wrapperf(self, v)
        return wrappera(name, v, self, 'get')
    return fkpy27
    
        

def init_hook(func, name):
    def fkpy27(*args, **kwargs):
        start = time.time()
        d = {'class':name,
                 'method':name,
                 'args':args,
                 'kwargs':kwargs,
                 'time':0
                        } 
        Debugger.method_calls.append(d)
        ret = func(*args, **kwargs)
        d['time'] = time.time()-start
        return ret
    return fkpy27


class Meta(type):
    def __new__(cls, name, bases, attrs):
 
        attrs['__init__'] = init_hook(attrs['__init__'], name)
        attrs['__getattribute__'] = getter_hook(object.__getattribute__)
        attrs['__setattr__'] = setter_hook(object.__setattr__)
        return type.__new__(cls, name, bases, attrs)
 
class Foo(metaclass = Meta):
    __metaclass__ = Meta
    def __init__(self, x):
        self.x = x

    def bar(self, v):
        return (self.x, v)
    
a = Foo(1)
a.bar(2)