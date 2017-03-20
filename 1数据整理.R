#预处理
setwd("~/Documents/项目/hillarytrump演示文稿")
great=readLines("great.txt")
blueprint=readLines("blueprint.txt")

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
time=as.POSIXct(time)
summary(time)
content=content[time<'2016-11-18']
rate=rate[time<'2016-11-18']
write(content,file="great_content.txt")
write(as.character(rate),file="great_rate.txt")
write(content[rate>=4],file="great_content_high.txt")
write(content[rate<=3],file="great_content_low.txt")

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
time=as.POSIXct(time)
summary(time)
content=content[time<'2016-11-18']
rate=rate[time<'2016-11-18']
write(content,file="blue_content.txt")
write(as.character(rate),file="blue_rate.txt")
write(content[rate>=4],file="blue_content_high.txt")
write(content[rate<=3],file="blue_content_low.txt")
