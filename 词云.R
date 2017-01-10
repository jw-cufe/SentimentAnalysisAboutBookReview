#画个词云

library(tm)
library(SnowballC)
library(wordcloud)
library(fpc)

setwd("~/Documents/项目/hillarytrump")
gtime=dir("~/Documents/项目/hillarytrump/great_time")
btime=dir("~/Documents/项目/hillarytrump/blueprint_time")

pfun2=function(time,cut){
  t=as.POSIXct(time)
  time1=as.character(t-24*60*60*cut)
  time2=time
  time3=as.character(t+24*60*60*cut)
  options(warn=-1)
  
  p_great=function(t1,t2){
    t=as.character(seq(as.POSIXct(t1),as.POSIXct(t2),by=24*60*60))
    gt=gtime[gtime%in%t]
    gt=paste("~/Documents/项目/hillarytrump/great_time/",gt,sep="")
    gv=readLines(gt[1])
    for(i in 2:length(gt)){
      tmp=readLines(gt[i])
      gv=c(gv,tmp)
    }
    gc <- Corpus(VectorSource(gv))
    gc <- tm_map(gc, content_transformer(tolower))
    gc <- tm_map(gc, removePunctuation)
    gc <- tm_map(gc, removeNumbers)
    removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
    gc <- tm_map(gc, content_transformer(removeURL))
    gc <- tm_map(gc, removeWords ,stopwords("english"))
    gc <- tm_map(gc,stemDocument)
    gc <- tm_map(gc, removeWords, c("book","read","amazon","review"))
    gtdm <- TermDocumentMatrix(gc, control=list(wordLengths=c(1,Inf)))
    gtdm <- as.matrix(gtdm)
    gwf <- sort(rowSums(gtdm),decreasing=T)
    set.seed(300)
    wordcloud(words=names(gwf),freq=gwf,min.freq=gwf[1]%/%10,random.order=F,colors="red")
  }
  
  p_blueprint=function(t1,t2){
    t=as.character(seq(as.POSIXct(t1),as.POSIXct(t2),by=24*60*60))
    bt=btime[btime%in%t]
    bt=paste("~/Documents/项目/hillarytrump/blueprint_time/",bt,sep="")
    bv=readLines(bt[1])
    for(i in 2:length(bt)){
      tmp=readLines(bt[i])
      bv=c(bv,tmp)
    }
    bc <- Corpus(VectorSource(bv))
    bc <- tm_map(bc, content_transformer(tolower))
    bc <- tm_map(bc, removePunctuation)
    bc <- tm_map(bc, removeNumbers)
    removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
    bc <- tm_map(bc, content_transformer(removeURL))
    bc <- tm_map(bc, removeWords ,stopwords("english"))
    bc <- tm_map(bc,stemDocument)
    bc <- tm_map(bc, removeWords ,c("book","read","review","amazon"))
    btdm <- TermDocumentMatrix(bc, control=list(wordLengths=c(1,Inf)))
    btdm <- as.matrix(btdm)
    bwf <- sort(rowSums(btdm),decreasing=T)
    set.seed(300)
    wordcloud(words=names(bwf),freq=bwf,min.freq=bwf[1]%/%6,random.order=F,colors="blue")
  }
  name1=gsub("-","",paste(time2,"前",sep=""))
  name2=gsub("-","",paste(time2,"后",sep="")) 
  png(paste("great",name1,".png",sep=""),width=630,height=450)
  p_great(time1,time2)
  dev.off()
  png(paste("great",name2,".png",sep=""),width=630,height=450)
  p_great(time2,time3)
  dev.off()
  png(paste("blue",name1,".png",sep=""),width=630,height=450)
  p_blueprint(time1,time2)
  dev.off()
  png(paste("blue",name2,".png",sep=""),width=630,height=450)
  p_blueprint(time2,time3)
  dev.off()
}

pfun2("2016-09-26",10)
pfun2("2016-10-09",10)
pfun2("2016-10-19",10)
pfun2("2016-10-28",10)
pfun2("2016-11-08",10)

#20170109画词云
pfun3=function(time1,time2){
  t1=as.POSIXct(time1)
  t2=as.POSIXct(time2)
  options(warn=-1)
  
  p_great=function(t1,t2){
    t=as.character(seq(as.POSIXct(t1),as.POSIXct(t2),by=24*60*60))
    gt=gtime[gtime%in%t]
    gt=paste("~/Documents/项目/hillarytrump/great_time/",gt,sep="")
    gv=readLines(gt[1])
    for(i in 2:length(gt)){
      tmp=readLines(gt[i])
      gv=c(gv,tmp)
    }
    gc <- Corpus(VectorSource(gv))
    gc <- tm_map(gc, content_transformer(tolower))
    gc <- tm_map(gc, removePunctuation)
    gc <- tm_map(gc, removeNumbers)
    removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
    gc <- tm_map(gc, content_transformer(removeURL))
    gc <- tm_map(gc, removeWords ,stopwords("english"))
    gc <- tm_map(gc,stemDocument)
    gc <- tm_map(gc, removeWords, c("book","read","amazon","review"))
    gtdm <- TermDocumentMatrix(gc, control=list(wordLengths=c(1,Inf)))
    gtdm <- as.matrix(gtdm)
    gwf <- sort(rowSums(gtdm),decreasing=T)
    set.seed(300)
    wordcloud(words=names(gwf),freq=gwf,min.freq=gwf[1]%/%20,random.order=F,colors="red")
  }
  
  p_blueprint=function(t1,t2){
    t=as.character(seq(as.POSIXct(t1),as.POSIXct(t2),by=24*60*60))
    bt=btime[btime%in%t]
    bt=paste("~/Documents/项目/hillarytrump/blueprint_time/",bt,sep="")
    bv=readLines(bt[1])
    for(i in 2:length(bt)){
      tmp=readLines(bt[i])
      bv=c(bv,tmp)
    }
    bc <- Corpus(VectorSource(bv))
    bc <- tm_map(bc, content_transformer(tolower))
    bc <- tm_map(bc, removePunctuation)
    bc <- tm_map(bc, removeNumbers)
    removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
    bc <- tm_map(bc, content_transformer(removeURL))
    bc <- tm_map(bc, removeWords ,stopwords("english"))
    bc <- tm_map(bc,stemDocument)
    bc <- tm_map(bc, removeWords ,c("book","read","review","amazon"))
    btdm <- TermDocumentMatrix(bc, control=list(wordLengths=c(1,Inf)))
    btdm <- as.matrix(btdm)
    bwf <- sort(rowSums(btdm),decreasing=T)
    set.seed(300)
    wordcloud(words=names(bwf),freq=bwf,min.freq=bwf[1]%/%7,random.order=F,colors="blue")
  }
  
  pdf("great词云.pdf",width=5,height=5)
  p_great(t1,t2)
  dev.off()
  pdf("blue词云.pdf",width=5,height=5)
  p_blueprint(t1,t2)
  dev.off()
}

pfun3("2016-09-16","2016-11-18")

library(ggplot2)
time1="2016-09-16";time2="2016-11-18"
  t1=as.POSIXct(time1)
  t2=as.POSIXct(time2)
  options(warn=-1)
  
    t=as.character(seq(as.POSIXct(t1),as.POSIXct(t2),by=24*60*60))
    gt=gtime[gtime%in%t]
    gt=paste("~/Documents/项目/hillarytrump/great_time/",gt,sep="")
    gv=readLines(gt[1])
    for(i in 2:length(gt)){
      tmp=readLines(gt[i])
      gv=c(gv,tmp)
    }
    gc <- Corpus(VectorSource(gv))
    gc <- tm_map(gc, content_transformer(tolower))
    gc <- tm_map(gc, removePunctuation)
    gc <- tm_map(gc, removeNumbers)
    removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
    gc <- tm_map(gc, content_transformer(removeURL))
    gc <- tm_map(gc, removeWords ,stopwords("english"))
    gc <- tm_map(gc,stemDocument)
    gc <- tm_map(gc, removeWords, c("book","read","amazon","review"))
    gtdm <- TermDocumentMatrix(gc, control=list(wordLengths=c(1,Inf)))
    gtdm <- as.matrix(gtdm)
    gwf <- data.frame(sort(rowSums(gtdm),decreasing=T))
    gdf=data.frame(x=names(gwf[1:15]),y=gwf[1:15])
    gdf$x=factor(gdf$x,levels = gdf$x)
    ph1=ggplot(data=gdf)+
      geom_bar(aes(x=x,y=y,fill=y),stat="identity")+
      scale_fill_gradient(low="darkgray",high="darkred")+
      labs(x="",y="")+
      coord_flip()+
      theme(legend.position = "None",
            axis.text = element_text(size=15))
  
    t=as.character(seq(as.POSIXct(t1),as.POSIXct(t2),by=24*60*60))
    bt=btime[btime%in%t]
    bt=paste("~/Documents/项目/hillarytrump/blueprint_time/",bt,sep="")
    bv=readLines(bt[1])
    for(i in 2:length(bt)){
      tmp=readLines(bt[i])
      bv=c(bv,tmp)
    }
    bc <- Corpus(VectorSource(bv))
    bc <- tm_map(bc, content_transformer(tolower))
    bc <- tm_map(bc, removePunctuation)
    bc <- tm_map(bc, removeNumbers)
    removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
    bc <- tm_map(bc, content_transformer(removeURL))
    bc <- tm_map(bc, removeWords ,stopwords("english"))
    bc <- tm_map(bc,stemDocument)
    bc <- tm_map(bc, removeWords ,c("book","read","review","amazon"))
    btdm <- TermDocumentMatrix(bc, control=list(wordLengths=c(1,Inf)))
    btdm <- as.matrix(btdm)
    bwf <- sort(rowSums(btdm),decreasing=T)
    bdf=data.frame(x=names(bwf[1:15]),y=bwf[1:15])
    bdf$x=factor(bdf$x,levels = bdf$x)
    ph2=ggplot(data=bdf)+
      geom_bar(aes(x=x,y=y,fill=y),stat="identity")+
      scale_fill_gradient(low="darkgray",high="darkblue")+
      labs(x="",y="")+
      coord_flip()+
      theme(legend.position = "None",
            axis.text = element_text(size=15))
    #print(ph2)
  
  pdf("great_bar.pdf",width=5,height=5)
  print(ph1)
  dev.off()
  pdf("blue_bar.pdf",width=5,height=5)
  print(ph2)
  dev.off()

  