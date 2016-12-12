
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
<<<<<<< HEAD
    library(actuar)
    library(FAdist)
    library(feather)
    library(bookdown)
    library(knitr)


=======
    library(psych)
    library(feather)
    library(actuar)
    library(FAdist)
>>>>>>> 40e44543caa9a7d6019689563009f37880d51690
    q()
#========================常用包==================================#
  	library(dplyr)
	  library(stringr)
    library(rhdf5)
    library(readr)
	  library(data.table)
  	library(plyr)
	  library(reshape2)
<<<<<<< HEAD
    library(psych)
=======

>>>>>>> 40e44543caa9a7d6019689563009f37880d51690



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
<<<<<<< HEAD
	install.packages('bookdown')
	install.packages('FAdist')
  install.packages('knitr')
=======
	install.packages('knitr')
>>>>>>> 40e44543caa9a7d6019689563009f37880d51690


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

<<<<<<< HEAD


=======
	R.home('bin')
	file.path(R.home('share'),'texmf','tex','latex')
>>>>>>> 40e44543caa9a7d6019689563009f37880d51690
