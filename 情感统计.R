#将两本书的情感统计成data.frame然后保存
fun=function(filename){
  tmp=readLines(filename)
  tmp=gsub("'","",tmp)
  tmp=gsub("\\[","",tmp)
  tmp=gsub("\\]","",tmp)
  tmp=gsub("\\{","",tmp)
  tmp=gsub("\\}","",tmp)
  tmp=gsub(":","",tmp)
  tmp=gsub(",","",tmp)
  tmp=gsub("neg","",tmp)
  tmp=gsub("neu","",tmp)
  tmp=gsub("pos","",tmp)
  tmp=gsub("compound","",tmp)
  tmp=as.numeric(unlist(strsplit(tmp," ")))
  tmp=tmp[!is.na(tmp)]
  m=length(tmp)/4
  neg=tmp[4*(1:m)-3]
  neu=tmp[4*(1:m)-2]
  pos=tmp[4*(1:m)-1]
  compound=tmp[4*(1:m)]
  dat=data.frame(time=filename,neg,neu,pos,compound)
  return(dat)
}

setwd("~/Documents/项目/hillarytrump/great_time")
fn=dir()
fn=fn[grep("result",fn)]
dat=fun(fn[1])
for(i in 2:length(fn)){
  dat=rbind(dat,fun(fn[i]))
}
dat$time=as.character(gsub("result","",dat$time))
write.csv(dat,"~/Documents/项目/hillarytrump/great_mood.csv",row.names=F)

setwd("~/Documents/项目/hillarytrump/blueprint_time")
fn=dir()
fn=fn[grep("result",fn)]
dat=fun(fn[1])
for(i in 2:length(fn)){
  dat=rbind(dat,fun(fn[i]))
}
dat$time=as.character(gsub("result","",dat$time))
write.csv(dat,"~/Documents/项目/hillarytrump/blueprint_mood.csv",row.names=F)
