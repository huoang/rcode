<<<<<<< HEAD
   
    setwd('/mnt/e/R')
    setwd('E:/pyr')
    getwd()
	rm(list=ls()[!(ls() %in% c())])
	
	save(list=ls(),file="")
	help(memory.size)
	memory.limit(size=1600)
	
	
	rm(list=ls()) ##
  ls()
	gc()
#========================常用包==================================#
	library(dplyr)
	library(magrittr)
	library(readr)
	library(stringr)
	library(data.table)
	library(feather)
	
	
	library(reshape2)
	library(ggplot2)
  library(plyr)
  library(foreign)
    
	library(nortest)   
	library(qcc)
	library(randomForest)
	library(rpart)
  library(rattle)
  library(rpart.plot)
  library(RColorBrewer)
	library(party)
	library(ipred)
	library(xlsx)
#=======================常用包安装===============================#	
  install.packages("dplyr")
	install.packages("plyr")
	install.packages("reshape2")
	install.packages('ggplot2')
  install.packages("data.table")
  install.packages("readr")
  install.packages("stringr")
  install.packages("foreign")
  install.packages("magrittr") 
  install.packages('feather') 
  
  
  devtools::install_github("wesm/feather/R") 
  install.packages('nortest')	 
	install.packages('qcc')
	#load("cpdata1201.Rdata")
	
	install.packages('rpart')
	install.packages('rpart.plot')
	install.packages("rattle")
	install.packages("RColorBrewer")
	install.packages("randomForest")
	install.packages("party")
	
	install.packages('ipred')
	install.packages('xlsx')
	
	tbl_cpdata<-tbl_df(cpdata)
	
    tbl_cpdata   
	##删除不必要列
	tbl_cpdata<-select(tbl_cpdata,-age60,-(id:opertms7))
	

	
	cpdata<-within(cpdata,
                   disctype[is.na(disctype)] <-"其他"  ##补充完整disctype 26个缺失值
                   )
	sum(is.na(cpdata$disctype))   ##补充完成			   
	
	str(cpdata$disctype)
	
	data_all<-tbl_df(r0913)
	data_all
    ab_opdays<-select(data_all,recordcode,address,b_opdays,a_opdays)	
	ab_opdays
	tbl_cpdata<-merge(tbl_cpdata,ab_opdays,by="recordcode")
	tbl_cpdata<-tbl_df(tbl_cpdata)
	tbl_cpdata
	
	save(list=ls(),file='cpdata1201.Rdata')
	
	x
	
=======
   
    setwd('/mnt/d/R')
    setwd('E:/pyr')
    getwd()
	rm(list=ls()[!(ls() %in% c())])
	
	save(list=ls(),file="")
	help(memory.size)
	memory.limit(size=1600)
	
	
	rm(list=ls()) ##
  ls()
	gc()
#========================常用包==================================#
	library(dplyr)
	library(magrittr)
	library(readr)
	library(stringr)
	library(data.table)
	library(feather)
	
	
	library(reshape2)
	library(ggplot2)
  library(plyr)
  library(foreign)
    
	library(nortest)   
	library(qcc)
	library(randomForest)
	library(rpart)
  library(rattle)
  library(rpart.plot)
  library(RColorBrewer)
	library(party)
	library(ipred)
	library(xlsx)
#=======================常用包安装===============================#	
  install.packages("dplyr")
	install.packages("plyr")
	install.packages("reshape2")
	install.packages('ggplot2')
  install.packages("data.table")
  install.packages("readr")
  install.packages("stringr")
  install.packages("foreign")
  install.packages("magrittr") 
  install.packages('feather') 
   
  install.packages('nortest')	 
	install.packages('qcc')
	#load("cpdata1201.Rdata")
	
	install.packages('rpart')
	install.packages('rpart.plot')
	install.packages("rattle")
	install.packages("RColorBrewer")
	install.packages("randomForest")
	install.packages("party")
	
	install.packages('ipred')
	install.packages('xlsx')
	
	tbl_cpdata<-tbl_df(cpdata)
	
    tbl_cpdata   
	##删除不必要列
	tbl_cpdata<-select(tbl_cpdata,-age60,-(id:opertms7))
	

	
	cpdata<-within(cpdata,
                   disctype[is.na(disctype)] <-"其他"  ##补充完整disctype 26个缺失值
                   )
	sum(is.na(cpdata$disctype))   ##补充完成			   
	
	str(cpdata$disctype)
	
	data_all<-tbl_df(r0913)
	data_all
    ab_opdays<-select(data_all,recordcode,address,b_opdays,a_opdays)	
	ab_opdays
	tbl_cpdata<-merge(tbl_cpdata,ab_opdays,by="recordcode")
	tbl_cpdata<-tbl_df(tbl_cpdata)
	tbl_cpdata
	
	save(list=ls(),file='cpdata1201.Rdata')
	
	x
	
>>>>>>> 1f05d082487808f4ca5ae6883f4ee6bce330a354
	