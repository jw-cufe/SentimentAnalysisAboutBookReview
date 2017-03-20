#文本清洗、构建语料库和绘制词云
library(tm)
library(SnowballC)
library(wordcloud)
library(fpc)
library(ggplot2)

setwd("~/Documents/项目/hillarytrump演示文稿")
great=readLines('great_content.txt')
great_high=readLines("great_content_high.txt")
great_low=readLines('great_content_low.txt')
blue=readLines('blue_content.txt')
blue_high=readLines('blue_content_high.txt')
blue_low=readLines('blue_content_low.txt')

mfun=function(name){
  options(warn=-1)
  text=get(name)
  gc=Corpus(VectorSource(text))
  gc=tm_map(gc,content_transformer(tolower))
  gc=tm_map(gc,removePunctuation)
  gc=tm_map(gc,removeNumbers)
  removeURL=function(x) gsub("http[[:alnum:]]*","",x)
  gc=tm_map(gc,content_transformer(removeURL))
  gc=tm_map(gc,removeWords,stopwords("english"))
  gc=tm_map(gc,stemDocument)
  gc=tm_map(gc,removeWords,c("book","read","amazon","review"))
  
  dir.create(paste('~/Documents/项目/hillarytrump演示文稿/',name,sep=""))
  writeCorpus(gc,path=paste('~/Documents/项目/hillarytrump演示文稿/',name,sep=""))
  
  gtdm=TermDocumentMatrix(gc,control=list(wordLengths=c(1,Inf)))
  gtdm=as.matrix(gtdm)
  gwf=sort(rowSums(gtdm),decreasing=T)
  set.seed(300)
  if(strsplit(name,"_")[[1]][1]=="blue"){
    color="darkblue"
    cut=10
    low_color="darkgray"
    high_color="darkblue"
  }else{
    color="darkred"
    cut=20
    low_color='darkgray'
    high_color='darkred'
  }
  
  pdf(paste('~/Documents/项目/hillarytrump演示文稿/',name,"_词云.pdf",sep=""),width=5,height=5)
  wordcloud(words=names(gwf),freq=gwf,min.freq=gwf[1]%/%cut,random.order=F,colors=color)
  dev.off()
  
  gdf=data.frame(x=names(gwf[1:15]),y=gwf[1:15])
  gdf$x=factor(gdf$x,levels=gdf$x)
  ph=ggplot(data=gdf)+
    geom_bar(aes(x=x,y=y,fill=y),stat="identity")+
    scale_fill_gradient(low=low_color,high=high_color)+
    labs(x="",y="Frequncy")+
    coord_flip()+
    theme(legend.position="None",
          axis.title.x=element_text(size=18),
          axis.text=element_text(size=15))
  pdf(paste('~/Documents/项目/hillarytrump演示文稿/',name,"_bar.pdf",sep=""),width=5,height=5)
  print(ph)
  dev.off()
}

mfun('great')
mfun('great_high')
mfun('great_low')
mfun('blue')
mfun('blue_high')
mfun('blue_low')
