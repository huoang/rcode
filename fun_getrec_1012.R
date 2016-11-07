#step1*==================================================*
get_hos_codes<-function(strpath){
    get_hoscode<-function(x) (substr(x,50,75))
    con_01<-file(strpath,open="r")
    seek(con=con_01,where=0)
    d1<-readLines(con_01,encoding="utf-8",10000000)	
    d2<-lapply(d1,get_hoscode)
    ma1<-regexpr("41\\d{10}",d2) 
    hoscodes<-regmatches(d2,ma1)
    rlts<-levels(factor(hoscodes))
    return(rlts)
  }
  ##year2013 hoscode
  hos_code_13<-get_hos_codes("f:/data/2013/2013index.csv") 
  save(hos_code_13,file="./rdata/hos_code_13_gbk.rdata")
  #hosnames<-read.csv("/mnt/e/R/rdoc/Hosnames.csv")
  #names(hosnames)<-c("code","name")
  hos.names.dic<-as.character(hosnames$name)
  names(hos.names.dic)<-as.character(hosnames$code)   
  #save(hos.names.dic,file="./rdata/hos_names_dic_utf8.rdata")
  
  hos.name.13<-hos.names.dic[hos_code_13]
  hos.name.13[which(is.na(hos.name.13))]
  hos_code_13[which(is.na(hos.name.13))]
  hos.name.13<-hos.name.13[!is.na(hos.name.13)]
  save(hos.name.13,file="./rdata/hos_name_13_gbk.rdata")
  ## "410000002288" "410000003222" 这两个编码在数据库索引中不存在
  ##end of get code of year2013
  
  
  
  
  
  
  hos_code_1501<-get_hos_codes("e:/data/1501/301.csv")
  gc()
  hos_code_1502<-get_hos_codes("e:/data/1502/302.csv")
  gc()
  hos_code_1503<-get_hos_codes("e:/data/1503/303.csv")   
  gc()
  hos_code_1504<-get_hos_codes("e:/data/1504/304.csv")
  gc()
  
  hos_code_15<-c(hos_code_1501,hos_code_1502,
                 hos_code_1503,hos_code_1504)
  hos_code_15<-unique(hos_code_15)             ##get hos codes of y15   
  save(hos_code_15,file="hos_code_15.rdata")
##get hos codes of year 2014*=========================================*
  get_hos_codes_y14<-function(strpath){
    get_hoscode<-function(x) (substr(x,1,30)) ##length is diffrent with y2015!!
    con_01<-file(strpath,open="r")
    seek(con=con_01,where=0)
    d1<-readLines(con_01,encoding="UTF-8",5000000)	
    d2<-lapply(d1,get_hoscode)
    ma1<-regexpr("41\\d{10}",d2) 
    hoscodes<-regmatches(d2,ma1)
    rlts<-levels(factor(hoscodes))
    return(rlts)
  }
  
     hos_code_1401<-get_hos_codes_y14("/mnt/e/data/B30501/301.csv")
     gc()
     hos_code_1402<-get_hos_codes_y14("/mnt/e/data/B31502/302.csv")
     gc()
     hos_code_1403<-get_hos_codes_y14("/mnt/e/data/B31503/303.csv")
     gc()
     hos_code_1404<-get_hos_codes_y14("/mnt/e/data/B31504/304.csv")
     gc()
     hos_code_14<-c(hos_code_1401,hos_code_1402,
                 hos_code_1403,hos_code_1404)
     hos_code_14<-unique(hos_code_14)
     hos_names<-read.table("./hosnames.txt")
     names(hos_names)<-c("code","name")
     hos_names_dic<-as.character(hos_names$name)
     names(hos_names_dic)<-as.character(hos_names$code)   
     hos_names_14<-hos_names_dic[hos_code_14]
     save(hos_names_14,file="./pro1/hos_names_14.rdata")
     
##step2================================================================##    
    ##function
	get_str_by_year<-function(lsthosname){
	  #strpath<-c("/mnt/e/data/B30501/301.csv","/mnt/e/data/B31502/302.csv",
    #            "/mnt/e/data/B31503/303.csv","/mnt/e/data/B31504/304.csv")
    strpath<-("/mnt/e/data/2013index.csv")
	  get_hoscode<-function(x) (substr(x,1,30)) ##while y14,it should be (1,30);while y15,it should be(45,75)
		for (i in 1:4){
       con_01<-file(strpath[i],open="r")
	     seek(con=con_01,where=0)
	     d1<-readLines(con_01,encoding="UTF-8",5000000)    
	     d2<-lapply(d1,get_hoscode)
	     ma1<-regexpr("41\\d{10}",d2) 
       d3<-d1[which(ma1!=-1)]    	
       rm(d1)
			 rm(d2)
			 rm(ma1)
			 gc()
			 d5<-lapply(d3,get_hoscode)
			 ma2<-regexpr("41\\d{10}",d5) 		
		   hoscodes<-regmatches(d5,ma2)
			 rm(d5)
			 gc()
		for (j in 1:length(lsthosname)){ 
		   tbl_s<-d3[which(hoscodes==names(lsthosname)[j])]
	     save(tbl_s,file=paste("./sec14/",lsthosname[j],"_s",i,".rdata",sep="")) ###warning~~
	     rm(tbl_s)
		   gc()                
                    }
		rm(d3)
		gc()
		               }
	   }
		
		combine.strlst<-function(lsthosname){
      for (i in 1:length(lsthosname)){
		    load(paste("./sec14/",lsthosname[i],"_s1.rdata",sep=""))
		    tbl_s1<-tbl_s
	      load(paste("./sec14/",lsthosname[i],"_s2.rdata",sep=""))
		    tbl_s2<-tbl_s
	      load(paste("./sec14/",lsthosname[i],"_s3.rdata",sep=""))
		    tbl_s3<-tbl_s
	      load(paste("./sec14/",lsthosname[i],"_s4.rdata",sep=""))
		    tbl_s4<-tbl_s
	      tbl<-c(tbl_s1,tbl_s2,tbl_s3,tbl_s4)
	      save(tbl,file=paste("./sec14/",lsthosname[i],".rdata",sep=""))
	      rm(list=c("tbl_s1","tbl_s2","tbl_s3","tbl_s4"))
		    gc()
		    file.remove(paste("./sec14/",lsthosname[i],"_s1.rdata",sep=""))
	      file.remove(paste("./sec14/",lsthosname[i],"_s2.rdata",sep=""))
	      file.remove(paste("./sec14/",lsthosname[i],"_s3.rdata",sep=""))
	      file.remove(paste("./sec14/",lsthosname[i],"_s4.rdata",sep=""))	
		              }
			          }
	
	load("./proc/hos_names_14.rdata")
	get_str_by_year(hos_names_14)
	combine.strlst(hos_names_14)
	
	##get 2013 单独的part
	get_str_2013<-function(lsthosname){
	    strpath<-("/mnt/e/data/2013index.csv")
	    get_hoscode<-function(x) (substr(x,45,75)) 
	    con_01<-file(strpath,open="r")
	    seek(con=con_01,where=0)
	    d1<-readLines(con_01,encoding="UTF-8",10000000)    
	    d2<-lapply(d1,get_hoscode)
	    ma1<-regexpr("41\\d{10}",d2) 
	    d3<-d1[which(ma1!=-1)]    	
	    rm(d1)
	    rm(d2)
	    rm(ma1)
	    gc()
	    d5<-lapply(d3,get_hoscode)
	    ma2<-regexpr("41\\d{10}",d5) 		
	    hoscodes<-regmatches(d5,ma2)
	    rm(d5)
	    gc()
	  
	    for (i in 1:length(lsthosname)){ 
	      tbl_s<-d3[which(hoscodes==names(lsthosname)[i])]
	      save(tbl_s,file=paste("./sec13/",lsthosname[i],".rdata",sep="")) ###warning~~
	      rm(tbl_s)
	      gc()                
	      cat("...",i,length(lsthosname),"...",sep = "/")
	       }
	    rm(d3)
	    gc()
	  }
	get_str_2013(hos.name.13)
	
	
	
##step3 function part*============================================*#
###210 ->229   260>-239  14->15 x3->x5
###14>-15	  
###part get_tbl of year 14
	get.tbl.14<-function(strhosnames){    
	  df.str.rlts<-data.frame()
	  for (i in 1:length(strhosnames)){ 
	    load(paste("./sec14/",strhosnames[i],".rdata",sep=""))
	    tbl<-unlist(tbl)  
	    tbl.str.num.1<-str_count(tbl,",")
	    tbl<-tbl[which(tbl.str.num.1<=256)]
	    names(tbl)<-c(1:length(tbl))
	    tbl.str.num.2<-str_count(tbl,",") 
	    ## record backup
	    temptbl<-table(tbl.str.num.2)
	    value<-names(temptbl)
	    numbers<-as.character(temptbl)
	    df.str.rep<-data.frame(value,numbers)
	    df.str.rep$hosname<-rep(strhosnames,nrow(df.str.rep))
	    df.str.rlts<-rbind(df.str.rep,df.str.rlts)
	    ##
	    tbl.del<-tbl[which(tbl.str.num>=239 & tbl.str.num<=256)]
	    tbl.del<-gsub('".+?"',"",tbl.del)
	    tbl[which(names(tbl) %in% names(tbl.del))]<-tbl.del
	    tbl<-tstrsplit(tbl,split=",",fixed=TRUE)
	    hos<-data.frame(tbl)
	    rm(tbl)
	    gc()
	    colnames(hos)<-paste("x",c(1:ncol(hos)),sep="")
	    hos<-tbl_df(hos)
	    hos<-filter(hos,x3==names(strhosnames)[i])   ##y2014 x3!!!!
	    if(nrow(hos)==0) hos[1,]<-NA
	    if(ncol(hos)>=239) {hos<-hos %>% select(x1:x239)}
	    else {add_col<-paste("x",c((ncol(hos)):239),sep="")
	    hos[,add_col]<-NA
	    hos<-hos %>% select(x1:x239)}
	    save(hos,file=paste("./tbl14/",strhosnames[i],".rdata",sep=""))
	    cat("...",i,length(strhosnames),"...",sep = "/")
	    rm(hos)
	    gc()    
	  } 
	  save(df.str.rlts,file="./proc/fee_cols_rlts.rdata")
	}   
	    
	  load("./proc/hos_names_14.rdata")
	  get.tbl.14(hos_names_14)   
	
	  
###1.256 -> 276 2.239->260 3.14->15 4.x3->x5
###part get_tbl of year 15
	  get.tbl.15<-function(strhosnames){    
	    df.str.rlts<-data.frame()
	    
	    for (i in 1:length(strhosnames)){ 
	      
	      load(paste("./sec15/",strhosnames[i],".rdata",sep=""))
	      tbl<-unlist(tbl)  
	      tbl.str.num.1<-str_count(tbl,",")
	      tbl<-tbl[which(tbl.str.num.1<=276)]
	      names(tbl)<-c(1:length(tbl))
	      tbl.str.num.2<-str_count(tbl,",") 
	      ## record backup
	      temptbl<-table(tbl.str.num.2)
	      value<-names(temptbl)
	      numbers<-as.character(temptbl)
	      df.str.rep<-data.frame(value,numbers)
	      df.str.rep$hosname<-rep(strhosnames,nrow(df.str.rep))
	      df.str.rlts<-rbind(df.str.rp,df.str.rlts)
	      ##
	      tbl.del<-tbl[which(tbl.str.num.2>=260 & tbl.str.num.2<=276)]
	      tbl.del<-gsub('".+?"',"",tbl.del)
	      tbl[which(names(tbl) %in% names(tbl.del))]<-tbl.del
	      tbl<-tstrsplit(tbl,split=",",fixed=TRUE)
	      hos<-data.frame(tbl)
	      rm(tbl)
	      gc()
	      colnames(hos)<-paste("x",c(1:ncol(hos)),sep="")
	      hos<-tbl_df(hos)
	      hos<-filter(hos,x5==names(strhosnames)[i])   ##y2015 x5!!!!
	      if(nrow(hos)==0) hos[1,]<-NA
	      if(ncol(hos)>=260) {hos<-hos %>% select(x1:x260)}
	      else {add_col<-paste("x",c((ncol(hos)):260),sep="")
	      hos[,add_col]<-NA
	      hos<-hos %>% select(x1:x260)}
	      save(hos,file=paste("./tbl15/",strhosnames[i],".rdata",sep=""))
	      cat("...",i,length(strhosnames),"...",sep = "/")
	      rm(hos)
	      gc()    
	    } 
	    save(df.str.rlts,file="./proc/fee_cols_rlts.rdata")
	  }   
	  
	 
	  get.tbl.13(hos.name.13)	 
	  
	  ###part get_tbl of year 13
	  
	  
	  
	  get.tbl.13<-function(strhosnames){    
	    df.str.rlts<-data.frame()
	    for (i in 1:length(strhosnames)){ 
	      load(paste("./sec13/",strhosnames[i],".rdata",sep=""))
	      tbl<-unlist(tbl_s)  
	      rm(tbl_s);gc()
	      tbl.str.num.1<-str_count(tbl,",")
	      
	      tbl<-tbl[which(tbl.str.num.1<=276)]
	      names(tbl)<-c(1:length(tbl))
	      tbl.str.num.2<-str_count(tbl,",") 
	      ## record backup
	      temptbl<-table(tbl.str.num.2)
	      value<-names(temptbl)
	      numbers<-as.character(temptbl)
	      df.str.rep<-data.frame(value,numbers)
	      df.str.rep$hosname<-rep(strhosnames[i],nrow(df.str.rep))
	      df.str.rlts<-rbind(df.str.rep,df.str.rlts)
	      ##
	      tbl.del<-tbl[which(tbl.str.num.2>=260 & tbl.str.num.2<=276)]
	      tbl.del<-gsub('".+?"',"",tbl.del)
	      tbl[which(names(tbl) %in% names(tbl.del))]<-tbl.del
	      tbl<-tstrsplit(tbl,split=",",fixed=TRUE)
	      hos<-data.frame(tbl)
	      rm(tbl)
	      gc()
	      colnames(hos)<-paste("x",c(1:ncol(hos)),sep="")
	      hos<-tbl_df(hos)
	      hos<-filter(hos,x5==names(strhosnames)[i])   ##y2013 x5!!!!
	      if(nrow(hos)==0) hos[1,]<-NA
	      if(ncol(hos)>=260) {hos<-hos %>% select(x1:x260)}
	      else {add_col<-paste("x",c((ncol(hos)):260),sep="")
	      hos[,add_col]<-NA
	      hos<-hos %>% select(x1:x260)}
	      save(hos,file=paste("./tbl13/",strhosnames[i],".rdata",sep=""))
	      cat("...",i,length(strhosnames),"...",sep = "/")
	      rm(hos)
	      gc()    
	    } 
	    save(df.str.rlts,file="./proc/fee_cols_rlts.rdata")
	  }   
	  
	
	  get.tbl.13(hos.name.13)	 
	  #debug(get.tbl.13)
	  
	  load(paste("./sec13/","郑州市第七人民医院",".rdata",sep=""))
##get year14 and place them into "./cledata"
	 get.unique.tbl.14<-function(strhosnames){
	   outcols<-paste("x",c(210:238),sep="")  
	   incols<-paste("x",c(211:239),sep="")
	   for (i in 1:length(strhosnames)){ 	  
	     load(paste("./tbl14/",strhosnames[i],".rdata",sep=""))
	     hos<-data.table(hos)
	     hos[x210=="",c(outcols):=.SD,.SDcols=incols]
	     hos<-hos %>% select(x2,x3,x4,x12,x14:x239)         ##here y14 is different with y15
	     names(hos)<-paste("x",c(1:ncol(hos)),sep="")
	     hos<-tbl_df(hos)
	     hos<-unique(hos)
	     save(hos,file=paste("./cledata/",strhosnames[i],"_y14.rdata",sep=""))
	     cat("...",i,length(strhosnames),"...",sep = "/")
	     }  
	     }
	   get.unique.tbl.14(hos_names_14) 
##get year15 and place them into "./cledata"
	   ##1.14>-15 2.210->229;211->230;3.238->259;239->260.
	   get.unique.tbl.15<-function(strhosnames){
	      outcols<-paste("x",c(229:259),sep="")  
	      incols<-paste("x",c(230:260),sep="")
	      for (i in 1:length(strhosnames)){ 	  
	        load(paste("./tbl15/",strhosnames[i],".rdata",sep=""))
	        hos<-data.table(hos)
	        hos[x229=="",c(outcols):=.SD,.SDcols=incols]
	        hos<-hos %>% select(x1,x5,x2,x31,x33:x258)        ##here y15 (x1,x5,x2,x31,x33:x258) 
	        names(hos)<-paste("x",c(1:ncol(hos)),sep="")
	        hos<-tbl_df(hos)
	        hos<-unique(hos)
	        save(hos,file=paste("./cledata/",strhosnames[i],"_y15.rdata",sep=""))
	       cat("...",i,length(strhosnames),"...",sep = "/")
	     }  
	   }
	   get.unique.tbl.15(hos_names_15)
	   
	   ##get year13 and place them into "./cledata"
	   ##1.14>-13 2.210->229;211->230;3.238->259;239->260.
	   get.unique.tbl.13<-function(strhosnames){
	     outcols<-paste("x",c(229:259),sep="")  
	     incols<-paste("x",c(230:260),sep="")
	     for (i in 1:length(strhosnames)){ 	  
	       load(paste("./tbl13/",strhosnames[i],".rdata",sep=""))
	       hos<-data.table(hos)
	       hos[x229=="",c(outcols):=.SD,.SDcols=incols]
	       hos<-hos %>% select(x1,x5,x2,x31,x33:x258)        ##here y13 (x1,x5,x2,x31,x33:x258) 
	       names(hos)<-paste("x",c(1:ncol(hos)),sep="")
	       hos<-tbl_df(hos)
	       hos<-unique(hos)
	       save(hos,file=paste("./cledata/",strhosnames[i],"_y13.rdata",sep=""))
	       cat("...",i,length(strhosnames),"...",sep = "/")
	     }  
	   }
	   get.unique.tbl.13(hos.name.13)
	   
	   
###i/o part	   
	   
	 load("./rdata/hos_code_13.rdata")
	 load("./rdata/hos_names_dic_utf8.rdata")
	 load("./rdata/hos_name_13_utf8.rdata")
	 