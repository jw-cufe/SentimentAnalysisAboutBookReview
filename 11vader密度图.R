library(ggplot2)
library(showtext)
library(scales)
library(grid)
library(grDevices)
library(plyr)
showtext.auto(enable=T)

setwd("~/Documents/项目/hillarytrump演示文稿")

great_vader=readLines("great_vader.txt")
blue_vader=readLines('blue_vader.txt')
great_rate=readLines('great_rate.txt')
blue_rate=readLines('blue_rate.txt')

great_vader=gsub("\\{","",great_vader)
great_vader=gsub("\\}","",great_vader)
great_vader=gsub(",","",great_vader)
great_vader=gsub("'neg': ","",great_vader)
great_vader=gsub("'neu': ","",great_vader)
great_vader=gsub("'pos': ","",great_vader)
great_vader=gsub("'compound': ","",great_vader)
great_vader=strsplit(great_vader,' ')

blue_vader=gsub("\\{","",blue_vader)
blue_vader=gsub("\\}","",blue_vader)
blue_vader=gsub(",","",blue_vader)
blue_vader=gsub("'neg': ","",blue_vader)
blue_vader=gsub("'neu': ","",blue_vader)
blue_vader=gsub("'pos': ","",blue_vader)
blue_vader=gsub("'compound': ","",blue_vader)
blue_vader=strsplit(blue_vader,' ')

great=data.frame(rate=as.numeric(great_rate),neg=NA,neu=NA,pos=NA)
for(i in 1:length(great_rate)){
  s=great_vader[[i]]
  great$neg[i]=as.numeric(s[1])
  great$neu[i]=as.numeric(s[2])
  great$pos[i]=as.numeric(s[3])
}

blue=data.frame(rate=as.numeric(blue_rate),neg=NA,neu=NA,pos=NA)
for(i in 1:length(blue_rate)){
  s=blue_vader[[i]]
  blue$neg[i]=as.numeric(s[1])
  blue$neu[i]=as.numeric(s[2])
  blue$pos[i]=as.numeric(s[3])
}

pd1=data.frame(neg=c(great$neg,blue$neg),neu=c(great$neu,blue$neu),pos=c(great$pos,blue$pos),book=c(rep('GreatAgain',nrow(great)),rep('Blueprint',nrow(blue))))
pd1=data.frame(book=pd1$book,mood=rep(c('neg','neu','pos'),each=nrow(pd1)),y=c(pd1$neg,pd1$neu,pd1$pos))
pd1$mood=factor(pd1$mood,levels=c('pos','neu','neg'))
ph1=ggplot(data=pd1,aes(x=y,color=book,fill=book,
                          y=..density..))+
  geom_density(size=1,alpha=0.2,adjust=1.5)+
  scale_fill_manual(values=c('darkblue','darkred'))+
  scale_color_manual(values=c('darkblue','darkred'))+
  labs(x='VADER Sentiment',title='VADER Sentiment Analysis')+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title=element_text(size=15),
        legend.position=c(0.9,0.9),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        strip.text=element_text(size=12,face='bold'),
        plot.title=element_text(size=20,face='bold',
                                hjust=0.5))+
  facet_wrap(~mood,ncol=1,scales='free_y')
#print(ph1)

pdf('vader_density.pdf',width=9,height=5)
print(ph1)
dev.off()

great$rate2=ifelse(great$rate>=4,"High",'Low')
pd2=data.frame(rate=great$rate2,mood=rep(c('neg','neu','pos'),each=nrow(great)),y=c(great$neg,great$neu,great$pos))
pd2$mood=factor(pd2$mood,levels=c('pos','neu','neg'))
ph2=ggplot(data=pd2,aes(x=y,color=rate,fill=rate,
                        y=..density..))+
  geom_density(size=1,alpha=0.2,adjust=1.5)+
  scale_fill_manual(values=c('darkred','darkgreen'))+
  scale_color_manual(values=c('darkred','darkgreen'))+
  labs(x='VADER Sentiment',title='Great Again')+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title=element_text(size=15),
        legend.position=c(0.88,0.9),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        strip.text=element_text(size=12,face='bold'),
        plot.title=element_text(size=20,face='bold',
                                hjust=0.5))+
  facet_wrap(~mood,ncol=1,scales='free_y')
#print(ph2)

blue$rate2=ifelse(blue$rate>=4,"High",'Low')
pd3=data.frame(rate=blue$rate2,mood=rep(c('neg','neu','pos'),each=nrow(blue)),y=c(blue$neg,blue$neu,blue$pos))
pd3$mood=factor(pd3$mood,levels=c('pos','neu','neg'))
ph3=ggplot(data=pd3,aes(x=y,color=rate,fill=rate,
                        y=..density..))+
  geom_density(size=1,alpha=0.2,adjust=1.5)+
  scale_fill_manual(values=c('darkblue','darkgreen'))+
  scale_color_manual(values=c('darkblue','darkgreen'))+
  labs(x='VADER Sentiment',title='Blueprint')+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title=element_text(size=15),
        legend.position=c(0.88,0.9),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        strip.text=element_text(size=12,face='bold'),
        plot.title=element_text(size=20,face='bold',
                                hjust=0.5))+
  facet_wrap(~mood,ncol=1,scales='free_y')
#print(ph3)

pdf('vader比较_density.pdf',width=9,height=5)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
vplayout=function(x,y){
  viewport(layout.pos.row=x,layout.pos.col=y)
}
print(ph2,vp=vplayout(1,1))
print(ph3,vp=vplayout(1,2))
dev.off()
