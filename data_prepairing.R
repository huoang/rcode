   
    setwd('/mnt/e/pyr')
    setwd('e:/pyr')
    getwd()
    rm(list=ls()) ##
    ls() 
    gc()  
    library(dplyr)
    library(stringr)
    library(readr)
    library(data.table)
    library(ggplot2)
    library(psych)
    
    q()
#========================常用包==================================#
  	library(dplyr)
	  library(stringr)
    library(feather)
    library(rhdf5)
    library(readr)
	  library(data.table)
  	library(plyr)
	  library(reshape2)
    
    
    
    
    library(devtools)
	  library(grid)
	  library(Rcpp)
  	library(nortest)   
	
	  library(qcc)
	  library(randomForest)
	  library(rpart)
    library(rattle)
    library(rpart.plot)
    library(RColorBrewer)
	  library(party)
  	library(car)
	  library(ipred)
#=======================常用包安装===============================#	
  install.packages('dplyr')
	install.packages('plyr')
	install.packages("data.table")
	install.packages("stringr")
	install.packages("readr")
	install.packages('ggplot2')
	install.packages('feather')
	install.packages("car")
	install.packages('nortest')	 
	install.packages("devtools")
	install.packages('psych')
	
	devtools::install_github("wesm/feather/R")
	source('http://bioconductor.org/biocLite.R')
	biocLite('rhdf5')
	
	
	install.packages('qcc')
	install.packages('rpart')
	install.packages('rpart.plot')
	install.packages("rattle")
	install.packages("RColorBrewer")
	install.packages("randomForest")
	install.packages("party")
	
	
	install.packages("ipred")
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
	
	
	
	