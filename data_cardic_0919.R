   cardic.0919<-read.csv("./cardic_0919.csv")
   cardic.0919 
   cardic.names<-cardic.0919$机构名称
   cardic.codes<-cardic.0919$机构ID
   cardic.names<-as.character(cardic.names)
   names(cardic.names)<-as.character(cardic.codes)
   
   var.list$chnnames<-str_conv(var.list$chnnames,"utf-8")
   var.list
   write.csv(var.list,file="./pro1/var_list_gbk.csv")
   
   load("./pro1/var_list.rdata")
   
   load("./pro1/hos_names_15.rdata")
   hos.code.15<-names(hos_names_15)
   hos.names.15<-str_conv(hos_names_15,"utf-8") 
   names(hos.names.15)<-hos.code.15
   ##year14
   load("./pro1/hos_names_14.rdata")
   hos.code.14<-names(hos_names_14)
   hos.names.14<-str_conv(hos_names_14,"utf-8") 
   names(hos.names.14)<-hos.code.14
   
   
   ##y15
   cardic.names.exist.15<-cardic.names[cardic.names %in% hos.names.15]
   length(cardic.names.exist.15)
   
   ##y14
   cardic.names.exist.14<-cardic.names[cardic.names %in% hos.names.14]
   length(cardic.names.exist.14)
   
   
   get.dbf<-function(lsthosnames,year){
     df.rlts<-data.table()
     for (i in 1:length(lsthosnames)){
       load(paste("./cledata/",lsthosnames[i],"_",year,".rdata",sep=""))
       #if (year!="y14" & year!="y15") {print("Input year doesn`t exist!")} else{
       #   if(year=="y14")  tbl<-tbl_df(tbl) 
       #   if(year=="y15")  tbl<-tbl_df(hos)
       #  } 
       df.rep<-hos %>% filter(x33=="03.04" | x33=="04.06")
       df.rlts<-rbind(df.rlts,df.rep)	
     }
     return(df.rlts)
   }
   
    cardic.rlt.14<-get.dbf(cardic.names.exist.14,"y14")
    write.csv(cardic.rlt.14,file="cardic_rlts_14.csv")
    
    
    cardic.rlt.15<-get.dbf(cardic.names.exist.15,"y15")
    write.csv(cardic.rlt.15,file="cardic_rlts_15.csv")
    
    
    
    