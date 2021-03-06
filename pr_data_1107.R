<<<<<<< HEAD

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

splitdata<-function(data,year){
  for (var in names(data)){
    data_rep<-data[,var]
    save(data_rep,file=paste('./data/',year,'_',
                             var,'.rdata',sep=''))
  }
}
splitdata(data13,'2013')

rm(data_rep)
rm(var)
rm(vars)
rm(year)
names(data13)
data13[,var]
getdata(path13)

data13<-getdata(path13)
save(data13,file='./data/data13.rdata')
rm(data13)
gc()
load('./data/data13.rdata')
str(data13)
length(data13)

data13 %>% select(x229:x258) %>% filter(x229=='')
data13[,13:42][data13[,13:42]=='NaN']<-0
data13[,13:42]<-lapply(data13[,13:42],as.numeric)

  
  









sample<-read_csv('e:/ipy/procdata/sample.csv')

sample<-tbl_df(sample)
vars = paste('x',c(1:ncol(sample)),sep='')

names(sample)<-vars
sample %>% select(x226:x261) %>% filter(!is.na(x261))
            View
sample %>% select(x228:x260) %>% View
            
varlist<-read_csv('./rdata/var_list.csv')
rm(var.list)

csvfiles<-list.files("e:/ipy/procdata/2013")
csvfiles<-paste('e:/ipy/procdata/2013/',csvfiles,sep='')

bind_csv<-function(files){
  csv_rlts<-data.frame() 
  for (i in 1:length(files)){
    csv_rep<-read_csv(files[i],col_names=TRUE,col_types = NULL,
                      locale(encoding = 'utf-8'))
    csv_rlts<-rbind(csv_rlts,csv_rep)
  }
return(csv_rlts)
} 

hos2013<-bind_csv(csvfiles)


  
  







>>>>>>> 1f05d082487808f4ca5ae6883f4ee6bce330a354
