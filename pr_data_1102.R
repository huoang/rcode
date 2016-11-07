
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

getdata(path13)


  
  







