#评分绘图
library(ggplot2)
require(grDevices)

setwd("~/Documents/项目/hillarytrump演示文稿")
great_rate=readLines("great_rate.txt")
blue_rate=readLines("blue_rate.txt")

great=data.frame(table(great_rate))
blue=data.frame(table(blue_rate))
rate=data.frame(x=1:5,y1=great$Freq,y2=-blue$Freq)
rate$y1_r=rate$y1/sum(rate$y1)
rate$y2_r=-rate$y2/sum(rate$y2)
rate$y1_l=paste(round(rate$y1_r*100,2),"%",sep='')
rate$y2_l=paste(round(-rate$y2_r*100,2),"%",sep='')

pd=data.frame(x=rate$x,y=c(rate$y1_r,rate$y2_r),label=c(rate$y1_l,rate$y2_l),fill=rep(c('GreatAgain','Blueprint'),each=5))
pd$x=factor(pd$x,levels=5:1)
ph1=ggplot(data=pd,
           aes(x=x,y=y,fill=fill))+
  geom_bar(stat='identity')+
  geom_text(aes(label=label,
                y=rep(c(1,-1),each=5)),
            size=4)+
  scale_fill_manual(values=c('darkblue','darkred'))+
  scale_y_continuous(limits=c(-1,1),
                     labels=c(1,0.5,0,0.5,1))+
  labs(x="Stars",y="Probability",title="Blueprint VS Great Again")+
  coord_flip()+
  theme(axis.text.x=element_text(size=18),
        axis.text.y=element_text(size=18),
        axis.title=element_text(size=20),
        plot.title=element_text(size=20,face='bold',
                                hjust=0.53),
        legend.position="None")
pdf("评分比较_bar.pdf",height=5,width=9)
print(ph1)
dev.off()
