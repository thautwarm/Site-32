#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Dec  5 18:22:58 2016
	
 这个东西是我做的第一个GUI，它也帮我拿到了第一桶金...
 这个程序的作用是验证一个词向量的效果：
	通过在DBpedia/Ontology里进行在线查询，返回匹配的单词。
	将存在于Word2Vec模型中的单词转化为词向量。
	聚类效果，检验outliers,并计算ACC= [ N(all)-N(outliers) ] / N(all)。
  注：该Word2VecModel文件属于他人财产，此处不放上。要用的话自己训练一个咯。
@author: thaut
"""
from sklearn.cross_validation import train_test_split as tts
from Tkinter import Scrollbar,Tk,Label,Entry,Menu,Button,END,HORIZONTAL,Text
import numpy as np
def donothing():pass
from SPARQLWrapper import SPARQLWrapper, JSON
from sklearn import svm
from sklearn.covariance import EllipticEnvelope
#from sklearn.ensemble import IsolationForest
from sklearn.cluster import KMeans
from sklearn.cluster import MeanShift
from sklearn.cluster import Birch
from gensim.models import Word2Vec
try: 
    model
except:
    model = Word2Vec.load('Word2VecModel.w2c')
params={'alpha':0.1}




def handle(inword,LIMIT):
    target=''
    if inword=='' :
        target="President"
    else:
        target=inword
    try:
        sparql = SPARQLWrapper("http://dbpedia.org/sparql")
        myurl=target
        query="""
            PREFIX dbo: <http://dbpedia.org/ontology/>
            select ?entity 
            {
             ?entity ?property dbo:"""+myurl+""".
             FILTER regex(?entity, "http://dbpedia.org/resource", "i") 
             } 
             LIMIT """+str(LIMIT)
        sparql.setQuery(query)
        sparql.setReturnFormat(JSON)
        results = sparql.query().convert()
        alist=[]
        for result in results["results"]["bindings"]:          
                    tmpob=result["entity"]["value"][28:]
            
                    if tmpob not in alist:
                        if '_' in tmpob:
                            tmpob='DBPEDIA_ID/'+tmpob
                        alist.append(tmpob)
    except:
            alist=['']
    return alist

    
names=['KMeans','OneClassSVM','MeanShift','EllipticEnvelope','IsolationForest','Birch']
def get_model(data,index):
    index+=1
    if index==1 :
        classifier=KMeans(n_clusters=2)
        classifier.fit(data) 
    elif index==2:
        classifier=svm.OneClassSVM(nu=params['alpha']+0.005,kernel="rbf", gamma=0.1)
        classifier.fit(data)
    elif index==3:
        classifier=MeanShift(bin_seeding=True,n_jobs=-1)
        classifier.fit(data)
    elif index==4:
        classifier=EllipticEnvelope(contamination=params['alpha'])
        classifier.fit(data)
    elif index==5:
        classifier=IsolationForest( contamination=params['alpha'], random_state=None)
        classifier.fit(data)
    elif index==6:
        classifier=Birch(n_clusters=2)
        classifier.fit(data)
    return classifier
class win:
    
    def __init__(self):
        root = Tk() 
        root.title('FINDME3')
        root.geometry() 
        self.root=root
        self.ini=0
        self.ACC=np.zeros((len(names),))
        self.trained=[False for i in names]
        self.outliers=[[] for i in names]
        choose=[self._choose(i) for i in range(9)]
        label1= Label(root, text="input ontology:")
        label2= Label(root, text="LIMIT:")
        entry1     =Entry(root, bd =5) 
        entry2     =Entry(root, bd =3)
        menubar_method=Menu(root,bd=0)
        menusub=Menu(menubar_method,tearoff=0)
        for i in range(len(names)):
            menusub.add_command(label=names[i],command=choose[i])
        menubar_method.add_cascade(label='Method',menu=menusub)
        button1_1  = Button(root,command=self.cin, text='run')
        button1_2  = Button(root,command=self.root.destroy, text='exit')
        button1_3  = Button(root,command=self.available, text='Available')
        button1_4  = Button(root,command=self.train, text='Train&Test')
        button1_5  = Button(root,command=self.listall, text='List all ACC')
        button1_6  = Button(root,command=self.outlier, text='List outliers\n of this method')
        text1      =Text(root,yscrollcommand=True,xscrollcommand=True, wrap='none')
        scr1_1     = Scrollbar(root)
        scr1_2     = Scrollbar(root,orient=HORIZONTAL)
        scr1_1['command'] = text1.yview
        scr1_2['command'] = text1.xview
        
        text1.configure(yscrollcommand = scr1_1.set,xscrollcommand = scr1_2.set)
        self.configs={'label1':label1\
                      ,'label2':label2\
                      ,'entry1':entry1\
                      ,'entry2':entry2\
                      ,'button1_1':button1_1\
                      ,'button1_2':button1_2\
                      ,'button1_3':button1_3\
                      ,'button1_4':button1_4\
                      ,'button1_5':button1_5\
                      ,'button1_6':button1_6\
                      ,'text1':text1\
                      ,'scr1_1':scr1_1\
                      ,'scr1_2':scr1_2\
                      ,'menu':menubar_method}
        self.display1()
        self.root.config(menu=menubar_method)
    def display1(self):
        self.configs['scr1_1'].grid(column=11, columnspan=10,row=4,rowspan=5,ipady = 130 )
        self.configs['scr1_2'].grid(column=5,row=12,rowspan=1,ipadx = 150 )    
        self.configs['label1'].grid( column=1, columnspan=2,row=2)
        self.configs['label2'].grid( column=1, columnspan=7,row=2)
        self.configs['entry1'].grid(column=3, columnspan=2,row=2) 
        self.configs['entry2'].grid(column=5, columnspan=1,row=2) 
        self.configs['button1_1'].grid(column=7, columnspan=3,row=2,ipadx=44)
        self.configs['text1'].grid(column=1, columnspan=5,row=4,rowspan=5)
        self.configs['button1_2'].grid(column=7, columnspan=3,row=13,ipadx=44)
        self.configs['button1_3'].grid(column=7, columnspan=3,row=4,ipadx=20)
        self.configs['button1_4'].grid(column=7, columnspan=3,row=2,rowspan=5,ipadx=18)
        self.configs['button1_5'].grid(column=7, columnspan=3,row=4,rowspan=4,ipadx=10)
        self.configs['button1_6'].grid(column=7, columnspan=3,row=5,rowspan=3,ipadx=0)
    def cin(self):
        self.configs['text1'].delete('1.0',END)
        contents=self.configs['entry1'].get()
        LIMIT=self.configs['entry2'].get()
        try:
            LIMIT=int(LIMIT)
        except:
            self.configs['text1'].insert("1.0",'unexpected input:LIMIT')
            return
        self.LIST=handle(contents,LIMIT)
        out='\n'.join(self.LIST)
        out=out.replace('DBPEDIA_ID/','')
        self.configs['text1'].insert("1.0",'here are entities:\n'+out)
    def available(self):
        self.configs['text1'].delete('1.0',END)
        
        try:
            self.LIST
        except:
            self.configs['text1'].insert("1.0",'The entities have not been initialized!')
            return    
        self.LIST=np.array([i for i in self.LIST if i in model])
        out='\n'.join(self.LIST)
        out=out.replace('DBPEDIA_ID/','')
        self.configs['text1'].insert("1.0",'Available ones are listed below:\n'+out)
    def _choose(self,i):
        def f():
            return self.choose_func(i)
        return f
    def choose_func(self,ini):
        self.ini=ini
    def show_ini(self):
        print(self.ini)
        self.configs['text1'].delete('1.0',END)
        self.configs['text1'].insert("1.0",'the ini is:'+str(self.ini))
    def train(self):
        self.configs['text1'].delete('1.0',END)
        try:
            self.LIST
        except:
            self.configs['text1'].insert("1.0",'The entities have not been initialized!')
            return 
        x_train_o,x_test_o=tts(self.LIST,test_size=0.2)
        x_train=np.array([model[i] for i in x_train_o])
        x_test=np.array([model[i] for i in x_test_o])
        train_model=get_model(x_train,self.ini)
        y_pred=train_model.predict(x_test)
        labels=sorted(set(y_pred))
        most=[sum(y_pred==i) for i in labels]
        if len(most)>1:
            arg_outlier=np.argmin(most)
            outliers=x_test_o[y_pred==labels[arg_outlier]]
            self.outliers[self.ini]=outliers
        most=max(most)
        ACC=most*1.0/len(y_pred)
        self.ACC[self.ini]=ACC
        self.trained[self.ini]=True
        self.configs['text1'].insert("1.0",'Type of classifier: '+names[self.ini]+'\n The ACC is:\n'+str(ACC))
    def listall(self):
        self.configs['text1'].delete('1.0',END)
        strs=''
        for i,acc in enumerate(self.ACC):
            if self.trained[i]:
                strs+='\tmethod :'+names[i]+'\n\t ACC: '+str(acc)+'\n'
            else:
                strs+='\tmethod :'+names[i]+' has not been used!!\n'
        self.configs['text1'].insert("1.0",strs)
    def outlier(self):
        ini=self.ini
        self.configs['text1'].delete('1.0',END)
        if self.trained[ini]:
            content='\n'.join(self.outliers[ini]).replace('DBPEDIA_ID/','')
            if len(content)==0:
                content='\nNone'
            self.configs['text1'].insert('1.0','here are the outliers made by method'\
            +str(names[ini]+':\n')+content)
        else:
            self.configs['text1'].insert('1.0','this method not trained\n')
        
        
window=win()
window.root.mainloop() 
