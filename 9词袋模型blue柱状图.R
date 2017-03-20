#统计词频
setwd("~/Documents/项目/hillarytrump演示文稿")

bagofwords=readLines("bagofwords_blue_result.txt")
bagofwords=bagofwords[4:103]
bagofwords=gsub("\\{","",bagofwords)
bagofwords=gsub("\\}","",bagofwords)
bagofwords=gsub(",","",bagofwords)
bagofwords=gsub("'high': ","",bagofwords)
bagofwords=gsub("'low': ","",bagofwords)
bagofwords=gsub("'feature': ","",bagofwords)
bw=data.frame(feature=NA,high=1:100,low=1:100)
for(i in 1:100){
  tmp=strsplit(bagofwords[i]," ")[[1]]
  bw$feature[i]=gsub("\\'","",tmp[2])
  bw$high[i]=as.numeric(tmp[1])
  bw$low[i]=as.numeric(tmp[3])
}
blue_rate=as.numeric(readLines("blue_rate.txt"))
nbh=sum(blue_rate>=4)
nbl=sum(blue_rate<=3)
bw$high2=bw$high/nbh
bw$low2=bw$low/nbl

#绘图
library(ggplot2)
library(showtext)
library(scales)
library(grid)
library(grDevices)
showtext.auto(enable=T)

pd=data.frame(x=bw$feature,y=c(bw$high2,-bw$low2),fill=rep(c("high","low"),each=100))
pd$x=factor(pd$x,levels=bw$feature)
pd$fill=factor(pd$fill,levels=c("high","low"))
pd$y[pd$y>0.4]=0.4
pd$y[pd$y< -0.4]=-0.4
ph=ggplot(data=pd[c(1:30,101:130),],
          aes(x=x,y=y,fill=fill))+
  scale_fill_manual(values=c("darkblue","darkgreen"))+
  scale_y_continuous(limits=c(-0.4,0.4),
                     breaks=c(-0.4,-0.2,0,0.2,0.4),
                     labels=c('0.4+','0.2','0',
                              '0.2','0.4+'))+
  labs(x="",y="Frequncy / Numbers Of Articals",title="Thumb Up VS Thumb Down")+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(size=12,
                                 angle=45,hjust=1),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=15),
        plot.title=element_text(size=20,face='bold',
                                hjust=0.5),
        legend.position="None")
print(ph)

pdf("bagofwords_blue_bar.pdf",width=9,height=5)
print(ph)
dev.off()
