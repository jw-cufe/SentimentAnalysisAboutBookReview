#! /Users/Lisiyan/anaconda/bin/python
import os
os.chdir("/Users/Lisiyan/Documents/mystudio/vaderSentiment-master/vaderSentiment")
from vaderSentiment import sentiment as vaderSentiment

def fun1(name):
    tmp=list(open(name).readlines())
    tmp=[s.strip() for s in tmp]
    tmp=[s.split(".") for s in tmp]
    result=str([vaderSentiment(sentence) for sentence in tmp])
    file=open(name+"result","w")
    file.write(result)
    file.close()

time=list(open("/Users/Lisiyan/Documents/项目/hillarytrump/time").readlines())
time=[s.strip() for s in time]

g_time=os.listdir("/Users/Lisiyan/Documents/项目/hillarytrump/great_time/")
b_time=os.listdir("/Users/Lisiyan/Documents/项目/hillarytrump/blueprint_time/")

g_time2=[]
b_time2=[]

for i in range(len(g_time)):
    y=g_time[i]
    for j in range(len(time)):
        x=time[j]
        if x==y:
            g_time2.append(y)

for i in range(len(b_time)):
    y=b_time[i]
    for j in range(len(time)):
        x=time[j]
        if x==y:
            b_time2.append(y)

file1=["/Users/Lisiyan/Documents/项目/hillarytrump/great_time/"+i for i in g_time2]
file2=["/Users/Lisiyan/Documents/项目/hillarytrump/blueprint_time/"+i for i in b_time2]

[fun1(i) for i in file1]
[fun1(i) for i in file2]
