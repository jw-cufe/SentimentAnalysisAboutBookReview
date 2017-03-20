#! /Users/Lisiyan/anaconda/bin/python

from __future__ import print_function
from __future__ import division
import os
os.chdir("./vaderSentiment-master/vaderSentiment")
from vaderSentiment import sentiment as vaderSentiment

great=open('/Users/Lisiyan/Documents/项目/hillarytrump演示文稿/great_content.txt').readlines()
blue=open('/Users/Lisiyan/Documents/项目/hillarytrump演示文稿/blue_content.txt').readlines()
great=[s.strip() for s in great]
great=[s.split('.') for s in great]
blue=[s.strip() for s in blue]
blue=[s.split('.') for s in blue]
f=open('/Users/Lisiyan/Documents/项目/hillarytrump演示文稿/great_vader.txt','w')
for i,s in enumerate(great):
    tmp=vaderSentiment(s)
    f.write(str(tmp)+'\n')
    if i%10==0:
        print(i,end=' ')
f.close()
f=open('/Users/Lisiyan/Documents/项目/hillarytrump演示文稿/blue_vader.txt','w')
for i,s in enumerate(blue):
    tmp=vaderSentiment(s)
    f.write(str(tmp)+'\n')
    if i%10==0:
        print(i,end=' ')
f.close()
