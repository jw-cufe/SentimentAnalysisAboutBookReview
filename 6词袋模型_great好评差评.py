#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from __future__ import print_function
from __future__ import division
import os
import re
import string
import numpy as np
import itertools
import nltk
from collections import Counter
from sklearn.ensemble import RandomForestClassifier
np.random.seed(2)

great_high=[]
for f in os.listdir('/Users/Lisiyan/Documents/项目/hillarytrump演示文稿/great_high/'):
    for line in open("/Users/Lisiyan/Documents/项目/hillarytrump演示文稿/great_high/"+f):
        line=line.strip()
        great_high.append(line)
great_high=[s.split() for s in great_high]
great_low=[]
for f in os.listdir('/Users/Lisiyan/Documents/项目/hillarytrump演示文稿/great_low/'):
    for line in open("/Users/Lisiyan/Documents/项目/hillarytrump演示文稿/great_low/"+f):
        line=line.strip()
        great_low.append(line)
great_low=[s.split() for s in great_low]
resample=len(great_high)//len(great_low)
text=np.array(great_high+great_low*resample)
y=np.array([0 for _ in great_high]+[1 for _ in great_low*resample])

#模型内
print("模型内预测：")
#统计词频word_counts并根据词频生成vocabulary_inv和features
word_counts=Counter(itertools.chain(*text))
vocabulary_inv=[x[0] for x in word_counts.most_common()]
features=vocabulary_inv[:len(vocabulary_inv)//2]

#使用features生成text_features
text_features=[]
for s in text:
    tmp=nltk.FreqDist(s)
    text_features.append([tmp[w] for w in features])
text_features=np.array(text_features)

#随机森林模型
forest=RandomForestClassifier(n_estimators=500)
forest.fit(text_features,y)
result=forest.predict(text_features)
print("\n100 most important features:")
great_high_counts=Counter(itertools.chain(*great_high))
great_low_counts=Counter(itertools.chain(*great_low))
feature_importances=forest.feature_importances_
tmp={"feature":[],"high":[],"low":[]}
for i in range(100):
    i+=1
    index=np.argmax(feature_importances)
    tmp["feature"]=features[index]
    tmp["high"]=great_high_counts[tmp["feature"]]
    tmp["low"]=great_low_counts[tmp['feature']]
    feature_importances[index]=0
    print(tmp)
print("\nAccurancy: %s"%(np.sum([y[i]==result[i] for i in range(len(y))])/len(y)))
print("\nConfusion Matrix:")
def conf_mat(truth,predict):
    conf=[[0,0],[0,0]]
    for i in range(len(truth)):
        conf[truth[i]-1][predict[i]-1]+=1
    return conf
conf=conf_mat(y,result)
for i in range(len(conf)):
    print(conf[i])

#模型外
print("\n模型外预测：")
shuffle_indices=np.random.permutation(np.arange(len(y)))
text1=text[shuffle_indices]
y1=y[shuffle_indices]
n=len(y)
m=n//10
for epoch in range(10):
    print("\nEpoch: %s"%str(epoch+1))
    test_index=range(m*epoch,m*(epoch+1))
    train_index=[]
    for i in range(n):
        if not i in test_index:
            train_index.append(i)
    train=text1[train_index]
    train_labels=y1[train_index]
    test=text1[test_index]
    test_labels=y1[test_index]

    #统计词频word_counts并根据词频生成vocabulary_inv和features
    word_counts=Counter(itertools.chain(*train))
    vocabulary_inv=[x[0] for x in word_counts.most_common()]
    features=vocabulary_inv[:len(vocabulary_inv)//2]

    #使用features生成train_features和test_features
    train_features=[]
    for s in train:
        tmp=nltk.FreqDist(s)
        train_features.append([tmp[w] for w in features])
    train_features=np.array(train_features)
    test_features=[]
    for s in test:
        tmp=nltk.FreqDist(s)
        test_features.append([tmp[w] for w in features])
    test_features=np.array(test_features)

    #随机森林模型
    forest=RandomForestClassifier(n_estimators=500)
    forest.fit(train_features,train_labels)
    result=forest.predict(test_features)
    print("\n100 most important features:")
    feature_importances=forest.feature_importances_
    tmp=""
    for i in range(100):
        i+=1
        index=np.argmax(feature_importances)
        tmp+=features[index]+' '
        feature_importances[index]=0
    print(tmp)
    print("\nAccurancy: %s"%(np.sum([test_labels[i]==result[i] for i in range(len(test_labels))])/len(test_labels)))
    print("\nConfusion Matrix:")
    def conf_mat(truth,predict):
        conf=[[0,0],[0,0]]
        for i in range(len(truth)):
            conf[truth[i]-1][predict[i]-1]+=1
        return conf
    conf=conf_mat(test_labels,result)
    for i in range(len(conf)):
        print(conf[i])
