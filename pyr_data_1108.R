
dirpath=list.dirs('/mnt/e/ipy/procdata/2013/')
filepath=list.files(dirpath)
path13=paste(dirpath,filepath,sep='')

getdata<-function(path){
  df_rlts<-data.frame() 
  for (i in 1:length(path)){
    df_rep<-read_feather(path[i])
    df_rlts<-rbind(df_rlts,df_rep)
  }
  return(df_rlts)
} 

data13<-getdata(path13)
save(data13,file='./data/data13.rdata')
rm(data13)
gc()
load('./data/data13.rdata')

length(data13)
rm(data)
data13 %>% select(x229:x258) %>% filter(x229=='')
data13[,5:34][data13[,5:34]=='NaN']<-0
data13[,5:34][data13[,5:34]=='nan']<-0
data13[,5:34]<-lapply(data13[,5:34],as.numeric)
fee13<-data13[,5:34]
save(fee13,file='./data/fee13.rdata')
str(fee13)
fee_sum_13<-data.frame(lapply(fee13,sum))
fee_sum_13['x229']

sum(fee_sum_13[c(-1,-2,-12,-14,-15,-19)])
