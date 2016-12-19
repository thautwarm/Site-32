#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Dec 18 17:30:05 2016
 使用数据集source.csv和词向量模型MODEL2.md/DOC.md进行市场价格分析。
 source.csv和词向量模型链接见readme文档。
@author: thaut
__doc__
"""
from __future__ import division
import numpy as np
import pandas as pd
import jieba
import pickle
import math
import random
from gensim.models import Word2Vec
try:
    model
except:
    FFF=input('word2vec?[y/n]:')
    if FFF=='y':
        KKK=input('Use the very big word2vec model?! [y/n]')
        if KKK=='y':
            model=Word2Vec.load('Doc.md')
        else:
            model=Word2Vec.load('MODEL2.md')
    MAXWORDS=7
def vectorize(target_words,sen,max_words=10):
    target_words=list(jieba.cut(target_words,HMM=False))
    words=np.array(list(jieba.cut(sen,HMM=False)))
    simi=[[model.similarity(t_w,word) if word in model and t_w in model else 0 for word in words] \
            for t_w in words]

    Simi=np.array([sum([model.similarity(t_w,word) if t_w in model  and word in model else\
                           (t_w in model or word in model)             \
                        for t_w in target_words]) \
                    for word in words])
    simi*=Simi
    simi=np.sum(simi,axis=0)
    arg=np.argsort(simi)[::-1]
    words=words[arg[:max_words]]
    vec=np.sum([model[word] if word in model else np.zeros((100,)) for word in words ],axis=0)
    return vec

try:
    D
except:
    D=pd.read_csv('sources.csv')
NAMES=D['name']
CATEGORIES=D['category']
#FEATURES=np.array([vectorize(CATEGORIES[index],sentence,max_words=MAXWORDS) for index,sentence in enumerate(NAMES)])
FILE=open('FEATURES.vec','rb');FEATURES=pickle.load(FILE);FILE.close()
def Cosine(f1,f2):
    return np.linalg.norm(f1-f2)
def Most_Similar(f1,F,n,retcosine=False):
    maxies=[Cosine(f1,i) for i in F]
    args=np.argsort(maxies)[:n]
    if retcosine:
        return args,[maxies[i] for i in args]
    return args,[]
def most(ind,n,retcosine=False): # use the global var NAMES,FEATURES!
    global FEATURES
    global NAMES
    args,cosines=Most_Similar(FEATURES[ind],FEATURES,n=n,retcosine=retcosine)
    ret=NAMES[args]
    if retcosine:
        su=pd.DataFrame(cosines)
        su.columns=['cosine']
        su.index=ret.index
        ret=pd.concat((ret,su),axis=1)
    return ret
def error(ind,i):
    global D
    f=lambda x:0.05+math.exp(-(x//100))
    compare=abs(D.price[ind]-D.price[i])/D.price[ind]
    return compare<f(D.price[ind])
def most_at_url(ind,url,n=200): # use the global var NAMES,FEATURES!
    global FEATURES
    global NAMES
    global CATEGORIES
    global D
    args,cosines=Most_Similar(FEATURES[ind],FEATURES,n=n)
    for i in args:
        if CATEGORIES[i]==CATEGORIES[ind] and D.url[i]==url and error(ind,i):
            return i
    return -1
URLS=[
    'www.jd.com',
 'www.kaola.com',
 'www.secoo.com',
 'www.tmall.hk',
 'www.xiu.com',
 'www.yintai.com',
 'www.ymatou.com'
          ]
SET_CATEGROY=['母婴用品','国际美妆','食品保健','电器数码','轻奢箱包','家居日用']
FRAME=pd.DataFrame(columns=URLS)
def justwatch():
    index=int(input('Please input the ids of the whole products(<'+str(N)+'):'))
    num=int(input('Please input the number of similar product you \
    wanna show in the list :'))
    name_list=most(index,n=num,retcosine=True)
    print(name_list)
    return True
def combine_getcsv():
    global INDEX
    global FRAME
    
    for aurl in URLS:
        for acate in SET_CATEGROY:
    
            samples=D.loc[ (D.url==aurl)&\
                (D.category==acate)]
            if len(samples)<=20:
                     print('less than 20 samples!')
                     print (aurl,acate)
            amount=min(20,len(list(samples.index)))
            index=np.array(random.sample(list(samples.index),amount))
            compare_number=100
            List=most(index,n=compare_number)
            Num=List.shape[0]
            values=np.zeros((Num,len(URLS)))
            for i_index,ind in enumerate(List.index):    
                for j_index,k in enumerate(URLS):
                    arg=most_at_url(ind,k)
                    if arg!=-1:
                        values[i_index][j_index]=D.price[arg]
                    else:
                        values[i_index][j_index]=np.nan
                Frame=pd.DataFrame(values)
                Frame.columns=URLS
                Frame.index=list(List)
                FRAME=pd.concat((FRAME,Frame))
    FRAME.to_csv('RESULT.csv')
    return True
def steps_getcsv():
    global INDEX
    global FRAME
    print(list(zip(range(len(URLS)) ,URLS ) ) )
    choose=input("""Please input which url do you want to start with:""")
    url_ind=int(choose)
    print(list(zip(range(len(SET_CATEGROY)),SET_CATEGROY)))
    choose=input("""Please choose the Category of the product:""")
    cate_ind=int(choose)
    samples=D.loc[ (D.url==URLS[url_ind])&\
                (D.category==SET_CATEGROY[cate_ind])]
    if len(samples)<=20:
        print('too few samples!')
        return True
    print(samples)
    print("""Select the poducts(by ids) to compare with those from\
    other urls.\n
    """)
    print()
    try:
        index=eval(input())
        index=np.array(list(index))
    except:
        index=np.array(random.sample(list(samples.index),20))
    choose=input('How many similar words would you search for(default=200).')
    try:
        compare_number=int(choose)
    except:
        compare_number=200
    List=most(index,n=compare_number)
    Num=List.shape[0]
    values=np.zeros((Num,len(URLS)))
    for i_index,ind in enumerate(List.index):    
        for j_index,k in enumerate(URLS):
            arg=most_at_url(ind,k)
            if arg!=-1:
                values[i_index][j_index]=D.price[arg]
            else:
                values[i_index][j_index]=np.nan
    Frame=pd.DataFrame(values)
    Frame.columns=URLS
    Frame.index=list(List)
    Frame.to_csv('result.csv')
    return True
N=FEATURES.shape[0]
def Main():
    func=lambda :False
    flag=True
    switch={'1':justwatch,
            '2':steps_getcsv,
            '3':combine_getcsv,
            '4':func
            }
    while flag:
        mode=input("""Please input the mode:
  1: watch the performance of this model\n
  2: do a series of operations to get an expected csv file\n
  3: deal all!\n
  4. quit
  input:""")
        flag=switch[mode]()
        
        
        
        
Main()




    
    
    
    
    
    
    
    
    
    
    
    
    
    
