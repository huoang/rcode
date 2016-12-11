
library(dplyr) 
library(readr) 
library(xtable) 
library(feather)
fee_15 = read_feather('e:/pyr/data/procdata/fee_pertime.pyr')
rlt1 = group_by(fee_15,hoslvl,hosname)    
rlt1 = summarise(rlt1,n_hos=n_distinct(hosname),n=n())    
rlt1 = group_by(rlt1,hoslvl)  
rlt1 = summarise(rlt1,n_hos=sum(n_hos),n=sum(n))
rlt1 = data.frame(rlt1)
rlt2 <- fee_15 %>% select(hoslvl,ttlfee) %>%
       group_by(hoslvl) %>% 
       summarise(min=min(ttlfee),
                 mean=mean(ttlfee),
                 sd = sd(ttlfee),
                 med=median(ttlfee),
                 max=max(ttlfee))
rlt2 <- data.frame(rlt2)
rlt3 <- cbind(rlt1,rlt2)
rlt3[,4]<-NULL
rlt3[,1]<-c("一级",'二级','三级')
names(rlt3)<-c('医院级别','医院个数','样本数',
               '最小值','均值','标准差','中位数','最大值')

rlt3[,4:8] <- round(rlt3[,4:8],1)
save(rlt3,file='E:/pyr/doc/fig1.rdata')
rlt3[,3][1]/sum(rlt3[,3])

kruskal.test(fee_15$ttlfee~fee_15$hoslvl)

?dpois
?ppois
?dpreto
?dlnorm
xtable(rlt1,label = c("表1：医院基本情况表"),
include.rownames = FALSE,align='llll')

print('表1：样本分布情况表')



