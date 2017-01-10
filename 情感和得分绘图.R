#将得分统计和情感统计合并绘图

library(ggplot2)
library(showtext)
library(scales)
library(grid)
library(grDevices)
library(plyr)
showtext.auto(enable=T)

setwd("~/Documents/项目/hillarytrump")
b_rate=read.csv("blueprint_rate.csv",header=T)
g_rate=read.csv("great_rate.csv",header=T)
b_mood=read.csv("blueprint_mood.csv",header=T)
g_mood=read.csv("great_mood.csv",header=T)
g=data.frame(g_mood,rate=g_rate$rate)
b=data.frame(b_mood,rate=b_rate$rate)
g$time=as.POSIXct(g$time)
b$time=as.POSIXct(b$time)

pfun1=function(time,cut=10){
  t=as.POSIXct(time)
  time1=as.character(t-24*60*60*cut)
  time2=time
  time3=as.character(t+24*60*60*cut)
  options(warn=-1)
  p_great=function(t1,t2){
    tmp_g=g[g$time<=t2 & g$time>=t1,]
    n=nrow(tmp_g)
    tmp_gx=tmp_g
    tmp_gx$rate2=ifelse(tmp_gx$rate>3,"high","low")
    tmp_gx$compound2=ifelse(tmp_gx$compound>0,"pos","neg")
    tmp_gx$compound2[tmp_gx$compound==0]="neu"
    tmp_gx[(n+1):(n+2),]=NA
    tmp_gx$rate2[(n+1):(n+2)]=c("high","low")
    tmp_gx$compound2[(n+1):(n+2)]=c("pos","neg")
    tmp_gx$compound2=factor(tmp_gx$compound2,levels=c("neg","neu","pos"))
    tmp_gx$rate2=factor(tmp_gx$rate2,levels=c("low","high"))
    tmp_g2=data.frame(x=factor(c(1,1,2,2,2)),y=c(sum(tmp_g$rate>3)/2,(sum(tmp_g$rate>3)+sum(tmp_g$rate<=3)/2),sum(tmp_g$compound>0)/2,(sum(tmp_g$compound>0)+sum(tmp_g$compound==0)/2),(sum(tmp_g$compound>0)+sum(tmp_g$compound==0)+sum(tmp_g$compound<0)/2)))
    tmp_g2$label=c(paste("High","\n",paste(round(sum(tmp_g$rate>3)/n*100,1),"%",sep="")),paste("Low","\n",paste(round(sum(tmp_g$rate<=3)/n*100,1),"%",sep="")),paste("Pos","\n",paste(round(sum(tmp_g$compound>0)/n*100,1),"%",sep="")),paste("Neu","\n",paste(round(sum(tmp_g$compound==0)/n*100,1),"%",sep="")),paste("Neg","\n",paste(round(sum(tmp_g$compound<0)/n*100,1),"%",sep="")))
    tmp_g4=data.frame(table(tmp_g$time))
    
    ph1.1=ggplot()+
      geom_bar(data=tmp_gx,aes(x=factor(1),fill=rate2),width = 0.6)+
      geom_text(data=tmp_g2[tmp_g2$x==1,],aes(x=factor(1),y=y,label=label),size=3,color="black")+
      scale_fill_manual(values=c("orange","red"))+
      theme_bw()+
      theme(panel.grid=element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            legend.position="None")+
      coord_polar(theta="y",direction=1)
    #print(ph1.1)
    
    ph1.2=ggplot()+
      geom_bar(data=tmp_gx,aes(x=factor(1),fill=compound2),width = 0.6)+
      geom_text(data=tmp_g2[tmp_g2$x==2,],aes(x=factor(1),y=y,label=label),size=3,color="black")+
      scale_fill_brewer(palette="Reds")+
      theme_bw()+
      theme(panel.grid=element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            legend.position="None")+
      coord_polar(theta="y",direction=1)
    #print(ph1.2)
    
    tmp_g3=data.frame(x=c("pos","neg"),y=c(mean(tmp_g$pos),mean(tmp_g$neg)))
    
    ph1.3=ggplot()+
      geom_bar(data=tmp_g3,aes(x=x,y=y),
               fill="tomato",
               stat="identity")+
      geom_text(data=tmp_g3,
                aes(x=x,y=y,label=round(y,2)),
                size=4,vjust=1)+
      scale_y_continuous(limits = c(0,0.2))+
      theme_bw()+
      theme(legend.position = "None",
            panel.grid = element_blank(),
            panel.border = element_blank(),
            axis.text.x=element_text(size=15),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text.y = element_blank())
    #print(ph1.3)
    return(list(ph1.1=ph1.1,ph1.2=ph1.2,ph1.3=ph1.3))
  }
  
  p_blueprint=function(t1,t2){
    tmp_b=b[b$time<=t2 & b$time>=t1,]
    n=nrow(tmp_b)
    tmp_bx=tmp_b
    tmp_bx$rate2=ifelse(tmp_bx$rate>3,"high","low")
    tmp_bx$compound2=ifelse(tmp_bx$compound>0,"pos","neg")
    tmp_bx$compound2[tmp_bx$compound==0]="neu"
    tmp_bx[(n+1):(n+2),]=NA
    tmp_bx$rate2[(n+1):(n+2)]=c("high","low")
    tmp_bx$compound2[(n+1):(n+2)]=c("pos","neg")
    tmp_bx$compound2=factor(tmp_bx$compound2,levels=c("neg","neu","pos"))
    tmp_bx$rate2=factor(tmp_bx$rate2,levels=c("low","high"))
    tmp_b2=data.frame(x=factor(c(1,1,2,2,2)),y=c(sum(tmp_b$rate>3)/2,(sum(tmp_b$rate>3)+sum(tmp_b$rate<=3)/2),sum(tmp_b$compound>0)/2,(sum(tmp_b$compound>0)+sum(tmp_b$compound==0)/2),(sum(tmp_b$compound>0)+sum(tmp_b$compound==0)+sum(tmp_b$compound<0)/2)))
    tmp_b2$label=c(paste("High","\n",paste(round(sum(tmp_b$rate>3)/n*100,1),"%",sep="")),paste("Low","\n",paste(round(sum(tmp_b$rate<=3)/n*100,1),"%",sep="")),paste("Pos","\n",paste(round(sum(tmp_b$compound>0)/n*100,1),"%",sep="")),paste("Neu","\n",paste(round(sum(tmp_b$compound==0)/n*100,1),"%",sep="")),paste("Neg","\n",paste(round(sum(tmp_b$compound<0)/n*100,1),"%",sep="")))
    tmp_b4=data.frame(table(tmp_b$time))
    
    ph2.1=ggplot()+
      geom_bar(data=tmp_bx,aes(x=factor(1),fill=rate2),width = 0.6)+
      geom_text(data=tmp_b2[tmp_b2$x==1,],aes(x=factor(1),y=y,label=label),size=3,color="black")+
      scale_fill_manual(values=c("skyblue","dodgerblue"))+
      theme_bw()+
      theme(panel.grid=element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            legend.position="None")+
      coord_polar(theta="y",direction=1)
    #print(ph2.1)
    
    ph2.2=ggplot()+
      geom_bar(data=tmp_bx,aes(x=factor(1),fill=compound2),width = 0.6)+
      geom_text(data=tmp_b2[tmp_b2$x==2,],aes(x=factor(1),y=y,label=label),size=3,color="black")+
      scale_fill_brewer(palette="Blues")+
      theme_bw()+
      theme(panel.grid=element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            legend.position="None")+
      coord_polar(theta="y",direction=1)
    #print(ph2.2)
    
    tmp_b3=data.frame(x=c("pos","neg"),y=c(mean(tmp_b$pos),mean(tmp_b$neg)))
    
    ph2.3=ggplot()+
      geom_bar(data=tmp_b3,aes(x=x,y=y),
               fill="dodgerblue",
               stat="identity",alpha=0.7)+
      geom_text(data=tmp_b3,
                aes(x=x,y=y,label=round(y,2)),
                size=4,vjust=1)+
      scale_y_continuous(limits = c(0,0.2))+
      theme_bw()+
      theme(legend.position = "None",
            panel.grid = element_blank(),
            panel.border = element_blank(),
            axis.text.x=element_text(size=15),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text.y = element_blank())
    #print(ph2.3)
    return(list(ph2.1=ph2.1,ph2.2=ph2.2,ph2.3=ph2.3))
  }
  
  tmp_g=g[g$time<=time3 & g$time>=time1,]
  tmp_b=b[b$time<=time3 & b$time>=time1,]
  tmp_g4=data.frame(table(tmp_g$time))
  tmp_b4=data.frame(table(tmp_b$time))
  tmp4=data.frame(Var1=seq(as.POSIXct(time1),as.POSIXct(time3),by=24*60*60))
  tmp4=join(tmp4,tmp_g4,by="Var1")
  tmp4=join(tmp4,tmp_b4,by="Var1")
  names(tmp4)=c("time","gn","bn")
  tmp4$gn[is.na(tmp4$gn)]=0
  tmp4$bn[is.na(tmp4$bn)]=0
  tmp4$gn=tmp4$gn+1
  tmp4$bn=tmp4$bn+1
  tmp4$gn1=as.numeric(tmp4$gn)
  tmp4$bn1=as.numeric(tmp4$bn)
  tmp4$gn1[tmp4$gn1>10]=10
  tmp4$bn1[tmp4$bn1>10]=10
  tmp4$time1=1:nrow(tmp4)
  ph=ggplot()+
    geom_point(data=tmp4,aes(x=time1,y=gn1),shape=16,
               color="red",size=4,alpha=0.8)+
    geom_point(data=tmp4,aes(x=time1,y=bn1),shape=17,
               color="blue",size=4,alpha=0.8)+
    geom_line(data=tmp4,aes(x=time1,y=gn1),linetype=5,
              color="red",size=1,alpha=0.8)+
    geom_line(data=tmp4,aes(x=time1,y=bn1),linetype=6,
              color="blue",size=1,alpha=0.8)+
    annotate("rect",xmin=1,xmax=cut,ymin=0,ymax=10,fill="green",alpha=0.2)+
    annotate("rect",xmin=cut,xmax=nrow(tmp4),ymin=0,ymax=10,fill="yellow",alpha=0.2)+
    scale_x_continuous(limits=c(1,nrow(tmp4)))+
    scale_y_continuous(limits=c(0,10),
                       breaks=c(0,5,10),
                       labels = c("0","5",">9"))+
    scale_shape(solid=F)+
    theme_bw()+
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size=15))
  #print(ph)
  
  great1=p_great(time1,time2)
  great2=p_great(time2,time3)
  blueprint1=p_blueprint(time1,time2)
  blueprint2=p_blueprint(time2,time3)
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(100,100)))
  vplayout <- function(x,y){
    viewport(layout.pos.row = x, layout.pos.col = y)
  }
  print(great1$ph1.1,vp=vplayout(1:50,1:18))
  print(great1$ph1.2,vp=vplayout(1:50,16:35))
  print(great2$ph1.1,vp=vplayout(1:50,49:68))
  print(great2$ph1.2,vp=vplayout(1:50,66:84))
  print(great1$ph1.3,vp=vplayout(1:39,33:51))
  print(great2$ph1.3,vp=vplayout(1:39,82:100))
  print(blueprint1$ph2.1,vp=vplayout(50:100,1:18))
  print(blueprint1$ph2.2,vp=vplayout(50:100,16:35))
  print(blueprint2$ph2.1,vp=vplayout(50:100,49:68))
  print(blueprint2$ph2.2,vp=vplayout(50:100,66:84))
  print(blueprint1$ph2.3,vp=vplayout(50:90,33:51))
  print(blueprint2$ph2.3,vp=vplayout(50:90,82:100))
  print(ph, vp = vplayout(40:60,1:100))
}

pdf("fig2.pdf",width=9,height=5)
pfun1("2016-09-26",10)
dev.off()
pdf("fig3.pdf",width=9,height=5)
pfun1("2016-10-09",10)
dev.off()
pdf("fig4.pdf",width=9,height=5)
pfun1("2016-10-19",10)
dev.off()
pdf("fig5.pdf",width=9,height=5)
pfun1("2016-10-28",10)
dev.off()
pdf("fig6.pdf",width=9,height=5)
pfun1("2016-11-08",10)
dev.off()
