#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Dec 13 23:03:35 2016
 这个爬虫是临时赶的，帮某金鹰基金的朋友做的，实在有点粗糙。
 而且还是单线程的233.
 不过好歹能用。
@author: thaut
"""
BRE=2
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import time
import re
import random
import pandas as pd
rnd=random.random
global D
try:
    D
except:
    D=pd.DataFrame(columns=['name','url','category','price'])
try:
    this
except:
    this=webdriver.Firefox()
    #this.implicitly_wait(10)
HASTAR=[]
HASIND=[]
tar=['母婴用品','国际美妆','食品保健','电器数码','轻奢箱包','家居日用']

o=  ['ymt',
      'jmhwg',
      'hlm',
      'yt',
      'sk',
      'zx',
      'klhg',
      'xhs',
      'jd']
      
urllist=['www.ymatou.com',
      'www.tmall.hk',
      'www.mei.com/index.html',
      'www.yintai.com',
      'www.secoo.com',
      'www.xiu.com',
      'www.kaola.com',
      'www.xiaohongshu.com',
      'www.jd.com']
urls=dict(zip(o,urllist))
"""
urls={'ymt':'www.ymatou.com',
      'jmhwg':'www.tmall.hk',
      'hlm':'www.mei.com/index.html',
      'yt':'www.yintai.com',
      'sk':'www.secoo.com',
      'zx':'www.xiu.com',
      'klhg':'www.kaola.com',
      'xhs':'www.xiaohongshu.com',
      'jd':'www.jd.com'}
"""
      
nextpage=['//ul[@class="pagination"]//a[@data-pageindex="REP"]',
          '//div[@class="ui-page"]//a[@class="ui-page-next"]',
            '',
            '//div[@class="p-page"]//a[@class="b-p-down"]',
            '//div[@class="product_page"]//a[@class="next"]',
            '//div[@class="Pagenum"]//a[@class="next-page"]',
            '//div[@class="splitPages"]//a[@class="nextPage"]',
            '',
            '//div[@class="page clearfix"]//a[@class="pn-next"]'
          ]

      
"""
ids=[False]*len(urls)
ids[6]=True;ids[8]=True
"""

ids=[False,
     False,
     False,
     False,
     False,
     False,
     True,
     False,
     True
     ]
     
     
"""about input element in HTMLs """

dname={'ymt':'k',
      'jmhwg':'q',
      'hlm':'ret',
      'yt':'keyword',
      'sk':'keyword',
      'zx':'kw',
      'klhg':'topSearchInput',#id
      'xhs':'ret',
      'jd':'key'}
      
XPATH_Name=['//div[@class="search-list"]//li[@class="product-item "]//p[@class="name"]',
            '//div[@class="product  "]//p[@class="productTitle"]',
          '//div[@class="listproduct"]//div[@class="product_content"]//a[@target="_blank"]',
            '//div[@class="p-listInfo"]//div[@class="p-listTxt"]',
            '//div[@class="show_tips"]//dd[@class="dl_name"]',
            '//li[@class="item"]//li[@class="tit"]',
            '//div[@class="goodswrap promotion"]//a[@class="title"]',
            '',
            '//li[@class="gl-item"]//div[@class="p-name p-name-type-2"]//em',
            ]
            
XPATH_Price=['//div[@class="search-list"]//li[@class="product-item "]//p[@class="price"]',
             '//div[@class="product  "]//p[@class="productPrice"]',
                '',
                '//div[@class="p-listInfo"]//div[@class="p-listPrice p-price"]',
                '//div[@class="show_tips"]//dd[@class="dl_price clearfix"]',
                '//li[@class="item"]//li[@class="price"]',
                '//div[@class="goodswrap promotion"]//p[@class="price"]',
                '',
                
                '//li[@class="gl-item"]//div[@class="p-price"]'
        ]

#the diary logs :to note where the errors took place.
logs=[]           





def searchin(url,bre=BRE): 
    this.get('http://'+url)
    time.sleep(bre)
    
def NextPage(ind,page_index,limit=20): #judge whether to turn to the next page
    if page_index>limit:return False
    try:
        name=this.find_elements_by_xpath(nextpage[ind].replace('REP',str(page_index)))[0]
        name.click()
        time.sleep(1)
        return True
    except:
        return False
def datamine(order,url,tar_i): #Get the datas
    #print(order)
    #print(XPATH_Price[order],XPATH_Name[order])
    try:
        price=this.find_elements_by_xpath(XPATH_Price[order])
        name=this.find_elements_by_xpath(XPATH_Name[order])
        price=[re.findall('[\d|\.]+',i.text)[0] for i in price]
        name=[i.text for i in name]
    except:
        time.sleep(1)
        price=this.find_elements_by_xpath(XPATH_Price[order])
        name=this.find_elements_by_xpath(XPATH_Name[order])
        price=[re.findall('[\d|\.]+',i.text)[0] for i in price]
        name=[i.text for i in name]
    #print(name)
    try:
        if len(name)==0:
            log='prob: None ',urllist[order],tar_i
            
            logs.append(log)
            print (log)
            return []
        if len(name)!=len(price):
            log='prob: Neq ',urllist[order]
            logs.append(log)
            print (log)
        #print(price)
        sd=pd.DataFrame(columns=['url','category','price'])
        sd['price']=price
        sd['name']=name
        sd['category']=tar_i
        sd['url']=url
    except:
        return []
    return sd 
def TRY(dnamei,m='name'):
    if m=='name':
        names=this.find_elements_by_name(dnamei)
    elif m=='id':
        names=this.find_elements_by_id(dnamei)
    for name in names:
                    try:
                        name.send_keys(Keys.BACKSPACE)
                        #name.send_keys(tar_i+Keys.ENTER)
                        break
                    except:
                        continue
    try:
        name
        return name
    except:
        return False
def Oper(ind,i,tar_i,m='name'):
                global D
                print(tar_i)
                searchin(urls[i]);
                for r in range(5):
                    name=TRY(dname[i],m)
                    if name:
                        break
                    else:
                            time.sleep(2)
                try:
                    name
                    if name==False:
                        undefined
                except:
                    searchin(urls[i]);
                    for r in range(5):
                        name=TRY(dname[i],m)
                        if name:
                            break
                        else:
                            time.sleep(2)
                name.clear()
                name.send_keys('233')
                time.sleep(0.7)
                name.clear()
                name.send_keys(tar_i+Keys.ENTER)
                time.sleep(10)
                if len(this.window_handles)>1:
                    this.switch_to_window(this.window_handles[1])
                    time.sleep(5)
                
                """Next Page"""
                
                page_index=1
                while True:
                    print (page_index)
                    D_=datamine(ind,urls[i],tar_i)
                    if len(D_)!=0:
                        print('concat')
                        D=pd.concat((D,D_))
                    page_index+=1
                    
                    flag=NextPage(ind,page_index)
                    time.sleep(3.5)
                    for handle in this.window_handles[2:]:
                        this.switch_to_window(handle)
                        this.close()
                    if len(this.window_handles)>1:
                        this.switch_to_window(this.window_handles[1])
                        time.sleep(1.5)
                        
                    if not flag:
                        break
                """END : NEXT PAGE"""    
                
                for handle in this.window_handles[1:]:
                    this.switch_to_window(handle)
                    this.close()
                this.switch_to_window(this.window_handles[0])
                
                time.sleep(0.3)
def getdatas(o):
    global HASTAR
    global HASIND
    for ind,i in enumerate(o):
        if ind in HASIND:continue
        else:
            HASIND.append(ind)
        print(urls[i])
        if dname[i]=='ret' :continue
        if ids[ind]==False:
            m='name'
        else:
            m='id'
        for index,tar_i in enumerate(tar):
            if tar_i in HASTAR:continue
            else:
                HASTAR.append(tar_i)
            try:
                Oper(ind,i,tar_i,m=m)
            except:
                return False,ind,index
        HASTAR=[]
        return True,False,False
def main():
    flag=True
    while flag:
        flag,ind,index=getdatas(o)
def check():
    inds=range(6)
    global D
    for ind in inds:
        url=urls[o[ind]]
        for tar_i in tar:
            print(url,tar_i)
            print(len(D.loc[(D.url==url)&(D.category==tar_i)]))
            if len(D.loc[(D.url==url)&(D.category==tar_i)])==0:
                if ids[ind]==False:
                    m='name'
                else:
                    m='id'
                Oper(ind,o[ind],tar_i,m=m)

                

    
    
    
            
                
                
    
        
    
