#预处理
#第一步：将原始文本中的日期、得分和评论的内容找出来，整理成data.frame并保存。
#第二步：单独保存日期和得分
#第三步：按照时间分组，然后按天保存评论文本

##读入数据
setwd("~/Documents/项目/hillarytrump")
great=readLines("great")
blueprint=readLines("blueprint")

##将原始文本中的日期、得分和评论的内容找出来，整理成data.frame并保存

index1=grep("out of 5 stars",great)
index2=grep("Report abuse",great)
tmp1=data.frame(cbind(index1,index2))
tmp1$cut=tmp1$index2-tmp1$index1
rate=great[index1]
time=great[index1+1]
content=apply(cbind(index1+3,index2-4),1,function(x)return(great[x[1]:x[2]]))
content=as.character(sapply(1:length(content),function(x){
  tmp=content[[x]]
  doc=""
  for(i in 1:length(tmp)){
    doc=paste(doc,tmp[i])
  }
  return(doc)
}))
rate=as.numeric(sapply(rate,function(x){
  return(as.numeric(strsplit(x," out of 5 ")[[1]][1]))
  }))
time=as.character(sapply(time,function(x){
  tmp=strsplit(x," ")[[1]]
  n=length(tmp)
  return(paste(tmp[n],tmp[n-2],tmp[n-1]))
  }))
time=as.character(sapply(time,function(x){
  t=gsub(" ","-",x)
  t=gsub(",","",t)
  t=gsub("January","1",t)
  t=gsub("February","2",t)
  t=gsub("March","3",t)
  t=gsub("April","4",t)
  t=gsub("May","5",t)
  t=gsub("June","6",t)
  t=gsub("July","7",t)
  t=gsub("August","8",t)
  t=gsub("September","9",t)
  t=gsub("October","10",t)
  t=gsub("November","11",t)
  t=gsub("December","12",t)
  t=strptime(t,"%Y-%m-%d")
  t=as.character(t)
  return(t)
  }))
great1=data.frame(time=time,rate=rate,content=content)
great1$content=as.character(great1$content)
great1$time=as.POSIXct(great1$time)
write.csv(great1,file="great_all.csv",row.names=F)

index1=grep("out of 5 stars",blueprint)
index2=grep("Report abuse",blueprint)
tmp1=data.frame(cbind(index1,index2))
tmp1$cut=tmp1$index2-tmp1$index1
rate=blueprint[index1]
time=blueprint[index1+1]
content=apply(cbind(index1+3,index2-4),1,function(x)return(blueprint[x[1]:x[2]]))
content=as.character(sapply(1:length(content),function(x){
  tmp=content[[x]]
  doc=""
  for(i in 1:length(tmp)){
    doc=paste(doc,tmp[i])
  }
  return(doc)
}))
rate=as.numeric(sapply(rate,function(x){
  return(as.numeric(strsplit(x," out of 5 ")[[1]][1]))
}))
time=as.character(sapply(time,function(x){
  tmp=strsplit(x," ")[[1]]
  n=length(tmp)
  return(paste(tmp[n],tmp[n-2],tmp[n-1]))
}))
time=as.character(sapply(time,function(x){
  t=gsub(" ","-",x)
  t=gsub(",","",t)
  t=gsub("January","1",t)
  t=gsub("February","2",t)
  t=gsub("March","3",t)
  t=gsub("April","4",t)
  t=gsub("May","5",t)
  t=gsub("June","6",t)
  t=gsub("July","7",t)
  t=gsub("August","8",t)
  t=gsub("September","9",t)
  t=gsub("October","10",t)
  t=gsub("November","11",t)
  t=gsub("December","12",t)
  t=strptime(t,"%Y-%m-%d")
  t=as.character(t)
  return(t)
}))
blueprint1=data.frame(time=time,rate=rate,content=content)
blueprint1$content=as.character(blueprint1$content)
blueprint1$time=as.POSIXct(blueprint1$time)
write.csv(blueprint1,file="buleprint_all.csv",row.names=F)

##单独保存日期和得分

time=seq(as.POSIXct("2016-09-16"),as.POSIXct("2016-11-18"),by=24*60*60)
time2=as.character(time)
write(time2,file="time")
great_rate=great1[,c(1,2)][great1$time%in%time,]
blueprint_rate=blueprint1[,c(1,2)][blueprint1$time%in%time,]
write.csv(great_rate,file="great_rate.csv",row.names=F)
write.csv(blueprint_rate,file="blueprint_rate.csv",row.names=F)

##按天保存评论文本

setwd("~/Documents/项目/hillarytrump/great_time/")
time=unique(great1$time)
name=as.character(time)
for(i in 1:length(name)){
  con=as.character(great1$content[great1$time%in%time[i]])
  write(con,file=name[i])
}

setwd("~/Documents/项目/hillarytrump/blueprint_time/")
time=unique(blueprint1$time)
name=as.character(time)
for(i in 1:length(name)){
  con=as.character(blueprint1$content[blueprint1$time%in%time[i]])
  write(con,file=name[i])
}
