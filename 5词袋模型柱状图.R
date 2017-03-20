#统计词频
setwd("~/Documents/项目/hillarytrump演示文稿")

bagofwords=readLines("bagofwords_result.txt")
bagofwords=bagofwords[4:103]
bagofwords=gsub("\\{","",bagofwords)
bagofwords=gsub("\\}","",bagofwords)
bagofwords=gsub(",","",bagofwords)
bagofwords=gsub("'blue': ","",bagofwords)
bagofwords=gsub("'great': ","",bagofwords)
bagofwords=gsub("'feature': ","",bagofwords)
bw=data.frame(feature=NA,great=1:100,blue=1:100)
for(i in 1:100){
  tmp=strsplit(bagofwords[i]," ")[[1]]
  bw$feature[i]=gsub("\\'","",tmp[3])
  bw$great[i]=as.numeric(tmp[2])
  bw$blue[i]=as.numeric(tmp[1])
}
great_rate=as.numeric(readLines("great_rate.txt"))
blue_rate=as.numeric(readLines("blue_rate.txt"))
n_great=length(great_rate)
n_blue=length(blue_rate)
bw$great2=bw$great/n_great
bw$blue2=bw$blue/n_blue

#绘图
library(ggplot2)
library(showtext)
library(scales)
library(grid)
library(grDevices)
showtext.auto(enable=T)

pd=data.frame(x=bw$feature,y=c(bw$great2,-bw$blue2),fill=rep(c("great","blue"),each=100))
pd$x=factor(pd$x,levels=bw$feature[100:1])
pd$fill=factor(pd$fill,levels=c("great","blue"))
pd$y[pd$y>0.4]=0.4
pd$y[pd$y< -0.4]=-0.4
ph=ggplot(data=pd[c(1:30,101:130),],
          aes(x=x,y=y,fill=fill))+
  scale_fill_manual(values=c("darkred","darkblue"))+
  scale_y_continuous(limits=c(-0.4,0.4),
                     breaks=c(-0.4,-0.2,0,0.2,0.4),
                     labels=c('0.4+','0.2','0',
                              '0.2','0.4+'))+
  labs(x="",y="Frequncy / Numbers Of Articals",title="Blueprint VS Great Again")+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=15),
        plot.title=element_text(size=20,face='bold',
                                hjust=0.53),
        legend.position="None")+
  coord_flip()
print(ph)

pdf("bagofwords_bar.pdf",width=9,height=5)
print(ph)
dev.off()
