#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Aug 27 16:37:44 2017

@author: misakawa
"""



def parsertime(string):
    a ,b = map(int, string.split(':'))
    return a*60+b

def timeperiod(strtup):
    a, b = map(parsertime, strtup)
    return set(range(a,b))

def lefttime(schedule):
    time =  set(range(540, 1140))
    for strtup in sum(schedule, []):
        time.difference_update(timeperiod(strtup))
    return time

def to_succ(timeperoid):
    ret = dict()
    if not timeperoid : return ret
    begin = None
    for i in timeperoid:
        try:
            if i-last!=1:
                begin = i
                ret[begin]=1
            else:
                ret[begin]+=1
            last = i
        except:
            last  = i
            begin = i
            ret[last] = 1
    return ret
def get_start_time(schedules, duration):
    options = to_succ(lefttime(schedules)).items()
    options = list(options)
    options.sort()
    for begin, period in options:
        if period>=duration:
            hour = str(begin//60)
            minute = begin%60
            formit = lambda i: (lambda s: '0'*(2-len(s))+s)(str(i))
            hour   = formit(hour) 
            minute = formit(minute)
            return  "{hour}:{minute}".format(hour = hour, minute = minute)
    return None
a =[
    [ ['10:00', '11:00'], ['12:00', '13:00'], ['14:00', '15:00'], ['16:00', '17:00'], ['18:00', '19:00']]
    , [['10:00', '13:00'], ['14:00', '17:00'], ['18:00', '19:00']]
    ]
get_start_time(a, 60) ->> print


