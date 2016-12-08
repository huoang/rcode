
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
    library(feather)
    library(actuar)
    library(FAdist)
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
	install.packages('knitr')


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
	install.packages('actuar')

	R.home('bin')
	file.path(R.home('share'),'texmf','tex','latex')
