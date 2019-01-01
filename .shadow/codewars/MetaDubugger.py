#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Aug 27 17:22:28 2017

@author: misakawa
"""

import time,sys
class Debugger(object):
    attribute_acceses = []
    method_calls = []
    
def wrapperf(self, func):
    def _f(*args, **kwargs):
        start = time.time()
        d = {'class':self.__class__,
             'method':func.__name__,
             'args':(self, *args),  
             'kwargs':kwargs,
             'time':0
                    }


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
                 'args':[],
                 'kwargs':kwargs,
                 'time':0
                        } 
        Debugger.method_calls.append(d)
        ret = func(*args, **kwargs)
        d['time'] = time.time()-start
        if sys.version_info.major == 3:
            d['args'] = (ret, *args)
            pass
        else:
            d['args'] = tuple([ret]+list(args))
        return ret
    return fkpy27
    
   

class Meta:
    def __getattribute__(self, name):
        v =  super(Meta, self).__getattribute__(name)
        if name == '__class__':
            return v
        if callable(v):
            wrappera(name, v, self, 'get')
            return wrapperf(self, v)
        return wrappera(name, v, self, 'get')
    @setter_hook
    def __setattr__(self,name, value):
        return super(Meta, self).__setattr__(name, value)
    
bc = __builtin__.__build_class__
def __build_class__(f):
    def _f(*args, **kwargs):
        ret = bc(*args, **kwargs)
        if args[1] in ['Meta','Debugger', 'EndMarkerReached' ]:
            pass
        else:
            locals()[args[1]] = init_hook(ret, args[1]) 
        return locals()[args[1]]
    return _f
__builtin__.__build_class__ = __build_class__(bc)   
        

        


