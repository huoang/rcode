library(MASS)
library(dplyr) 
library(readr) 
library(xtable) 
library(feather)
library(actuar)
library(FAdist)
library(ggplot2)
library(reshape2)

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

rlt_sum <- fee_15 %>% select(hoslvl,ttlfee) %>%
  #group_by(hoslvl) %>% 
  summarise(min=min(ttlfee),
            mean=mean(ttlfee),
            sd = sd(ttlfee),
            med=median(ttlfee),
            max=max(ttlfee))
rlt3[4,1]<-'合计'
rlt3[4,2] <-sum(rlt3[1:3,2])
rlt3[4,3] <-sum(rlt3[1:3,3])
rlt3[4,4:8] <- rlt_sum
rlt3[,4:8] <- round(rlt3[,4:8],1)
save(rlt3,file='E:/pyr/doc/fig1.rdata')
rlt3[,3][1]/rlt3[4,3]
xtable(rlt3)



fee_15 = read_feather('e:/pyr/data/procdata/fee_pertime.pyr')

x <- fee_15$ttlfee

x <- x/10000

breaks <- c(seq(0,0.9,by=0.3),
            seq(1.8,7.2,by=1.8),
            seq(20,100,by=40)
)
freq_x<-cut(x,breaks= breaks)

freq_x<-table(freq_x)

p_x_br <- freq_x/length(x)

##dlnorm
rlt_lnorm <- fitdistr(x = x,
                      densfun = dlnorm,
                      start = list(meanlog = 0,sdlog = 1),# need to provide named list of starting values
                      lower = list(meanlog= 0,sdlog = 1))

mlog <- rlt_lnorm$estimate[1]
sdlog <- rlt_lnorm$estimate[2]
set.seed(123)
x1 <- sample(x,100000)

d<- dlnorm(x1,mlog,sdlog)
df_lnorm <- data.frame(x1,d)

ggplot(df_lnorm,aes(x = x1,y=..density..)) +
  geom_histogram(fill = 'cornsilk',color = 'purple',
                 binwidth = 0.2) +
  #geom_density()   +
  geom_line(aes(x=x1,y=d),colour="red")+
  #   stat_function(fun = dburr,
  #              args =list(shape1 = 1,
  #                         shape2 = 1.73,rate =2.2))+
  scale_x_continuous(breaks = seq(0,12,by=3)) +
  coord_cartesian(xlim = c(0,12)) +
  labs(x = '住院医疗总费用(万元)',y = '概率密度') +
  annotate('text',x=7,y=1.3,
           label = '对数正态分布模型拟合图') +
  annotate('text',x=4,y=1.3,label = '图1：') +
  annotate('text',x=5,y=1.1,label = 'meanlog:') +
  annotate('text',x=7,y=1.1,label = '0.000') +
  annotate('text',x=7,y=1.0,label = '(0.00108)') +

  annotate('text',x=5,y=0.8,label = 'sdlog:') +
  annotate('text',x=7,y=0.8,label = '1.303') +
  annotate('text',x=7,y=0.7,label = '(0.00076)') 

p_lnorm <- plnorm(x,mlog,sdlog)

p_lnorm_br <- plnorm(breaks,mlog,sdlog)

p_lnorm_br[length(p_lnorm_br)] <- 1

p_lnorm_br <- diff(p_lnorm_br)

freq_lnorm<-length(x)*p_lnorm_br

sum((freq_lnorm-freq_x)^2/freq_lnorm)

chisq.test(freq_x,p=(freq_lnorm/length(x)))

max(abs(p_x_br-p_lnorm_br))

1.36/sqrt(length(x))

####################################pareto=======================================
rlt_pareto <- fitdistr(x = x,
                       densfun = dpareto,
                       start = list(shape = 1.6,scale = 1),# need to provide named list of starting values
                       lower = list(shape = 1,scale = 1))

sha_pare <- rlt_pareto$estimate[1]
sca_pare <- rlt_pareto$estimate[2]
set.seed(123)
x1 <- sample(x,100000)

d<- dpareto(x1,sha_pare,sca_pare)
df_pareto <- data.frame(x1,d)

ggplot(df_pareto,aes(x = x1,y=..density..)) +
  geom_histogram(fill = 'cornsilk',color = 'purple',
                 binwidth = 0.2) +
  #geom_density()   +
  geom_line(aes(x=x1,y=d),colour="red")+
  #   stat_function(fun = dburr,
  #              args =list(shape1 = 1,
  #                         shape2 = 1.73,rate =2.2))+
  scale_x_continuous(breaks = seq(0,12,by=3)) +
  coord_cartesian(xlim = c(0,12)) +
  labs(x = '住院医疗总费用(万元)',y = NULL) +    #,y = '概率密度') +
  annotate('text',x=7,y=1.5,
           label = 'Pareto分布模型拟合图') +
  annotate('text',x=4,y=1.5,label = '图2：') +
  annotate('text',x=5,y=1.2,label = 'shape(a):') +
  annotate('text',x=7,y=1.2,label = '3.451') +
  annotate('text',x=7,y=1.1,label = '(0.00572)') +
  
  annotate('text',x=5,y=0.9,label = 'scale(s):') +
  annotate('text',x=7,y=0.9,label = '2.032') +
  annotate('text',x=7,y=0.8,label = '(0.00418)') 
  

p_pareto <- ppareto(x,sha_pare,sca_pare)

p_pareto_br <- ppareto(breaks,sha_pare,sca_pare)

p_pareto_br[length(p_pareto_br)] <- 1

p_pareto_br <- diff(p_pareto_br)

freq_pareto<-length(x)*p_pareto_br

sum((freq_pareto-freq_x)^2/freq_pareto)

chisq.test(freq_x,p=(freq_pareto/length(x)))

max(abs(p_x_br-p_pareto_br))

1.36/sqrt(length(x))


####################################weibull=======================================
rlt_weibull <- fitdistr(x = x,
                        densfun = dweibull,
                        start = list(shape = 1.6,scale = 1),# need to provide named list of starting values
                        lower = list(shape = 1,scale = 1))

sha_weibull <- rlt_weibull$estimate[1]
sca_weibull <- rlt_weibull$estimate[2]
set.seed(123)
x1 <- sample(x,100000)

d<- dweibull(x1,sha_weibull,sca_weibull)
df_weibull <- data.frame(x1,d)

ggplot(df_weibull,aes(x = x1,y=..density..)) +
  geom_histogram(fill = 'cornsilk',color = 'purple',
                 binwidth = 0.2) +
  #geom_density()   +
  geom_line(aes(x=x1,y=d),colour="red")+
  #   stat_function(fun = dburr,
  #              args =list(shape1 = 1,
  #                         shape2 = 1.73,rate =2.2))+
  scale_x_continuous(breaks = seq(0,12,by=3)) +
  coord_cartesian(xlim = c(0,12)) +
  labs(x = '住院医疗总费用(万元)',y = NULL)+  #y = '概率密度') +
  annotate('text',x=7,y=1.3,
           label = 'Weibull分布模型拟合图') +
  annotate('text',x=4,y=1.3,label = '图3：') +
  annotate('text',x=5,y=1.1,label = 'shape(a):') +
  annotate('text',x=7,y=1.1,label = '1.000') +
  annotate('text',x=7,y=1.0,label = '(0.00029)') +
  
  annotate('text',x=5,y=0.8,label = 'scale(b):') +
  annotate('text',x=7,y=0.8,label = '1.000') +
  annotate('text',x=7,y=0.7,label = '(0.00054)') 


p_weibull <- pweibull(x,sha_weibull,sca_weibull)

p_weibull_br <- pweibull(breaks,sha_weibull,sca_weibull)

p_weibull_br[length(p_weibull_br)] <- 1

p_weibull_br <- diff(p_weibull_br)

freq_weibull<-length(x)*p_weibull_br

sum((freq_weibull-freq_x)^2/freq_weibull)

chisq.test(freq_x,p=(freq_weibull/length(x)))

max(abs(p_x_br-p_weibull_br))

1.36/sqrt(length(x))

####################################burr=======================================
rlt_burr <- fitdistr(x = x,
                     densfun = dburr,
                     start = list(shape1 = 1,shape2 = 1.7,rate=2),# need to provide named list of starting values
                     lower = list(shape1 = 1,shape2 = 1,rate=1))

sha1_burr <- rlt_burr$estimate[1]
sha2_burr <- rlt_burr$estimate[2]
rate_burr <- rlt_burr$estimate[3]
set.seed(123)
x1 <- sample(x,100000)
d<- dburr(x1,sha1_burr,sha2_burr,rate_burr)
df_burr <- data.frame(x1,d)

ggplot(df_burr,aes(x = x1,y=..density..)) +
  geom_histogram(fill = 'cornsilk',color = 'purple',
                 binwidth = 0.2) +
  #geom_density()   +
  geom_line(aes(x=x1,y=d),colour="red")+
  #   stat_function(fun = dburr,
  #              args =list(shape1 = 1,
  #                         shape2 = 1.73,rate =2.2))+
  scale_x_continuous(breaks = seq(0,12,by=3)) +
  coord_cartesian(xlim = c(0,12)) +
  labs(x = '住院医疗总费用(万元)',y = '概率密度') +
  annotate('text',x=7,y=1.3,
           label = 'Burr(Type XII)模型拟合图') +
  annotate('text',x=4,y=1.3,label = '图4：') +
  annotate('text',x=5,y=1.1,label = 'shape1(a)') +
  annotate('text',x=7,y=1.1,label = '1.000') +
  annotate('text',x=7,y=1.0,label = '(0.00218)') +
  
  annotate('text',x=5,y=0.8,label = 'shape2(b)') +
  annotate('text',x=7,y=0.8,label = '1.719') +
  annotate('text',x=7,y=0.7,label = '(0.00157)') +
  
  annotate('text',x=5,y=0.5,label = 'rate(s)') +
  annotate('text',x=7,y=0.5,label = '2.235') +
  annotate('text',x=7,y=0.4,label = '(0.00442)')


p_burr <- pburr(x,sha1_burr,sha2_burr,rate_burr)

p_burr_br <- pburr(breaks,sha1_burr,sha2_burr,rate_burr)

p_burr_br[length(p_burr_br)] <- 1

p_burr_br <- diff(p_burr_br)

freq_burr<-length(x)*p_burr_br

sum((freq_burr-freq_x)^2/freq_burr)

chisq.test(freq_x,p=(freq_burr/length(x)))

max(abs(p_x_br-p_burr_br))

1.36/sqrt(length(x))

####################################llogis=======================================
rlt_llogis <- fitdistr(x = x,
                       densfun = dllogis,
                       start = list(shape = 1.7,rate=2),# need to provide named list of starting values
                       lower = list(shape = 1,rate=1))

sha_llogis <- rlt_llogis$estimate[1]
rate_llogis <- rlt_llogis$estimate[2]
set.seed(123)
x1 <- sample(x,100000)
d <- dllogis(x1,sha_llogis,rate_llogis)
df_llogis <- data.frame(x1,d)

ggplot(df_llogis,aes(x = x1,y=..density..)) +
  geom_histogram(fill = 'cornsilk',color = 'purple',
                 binwidth = 0.2) +
  #geom_density()   +
  geom_line(aes(x=x1,y=d),colour="red")+
  #   stat_function(fun = dllogis,
  #              args =list(shape1 = 1,
  #                         shape2 = 1.73,rate =2.2))+
  scale_x_continuous(breaks = seq(0,12,by=3)) +
  coord_cartesian(xlim = c(0,12)) +
  labs(x = '住院医疗总费用(万元)',y = NULL)+  #y = '概率密度') +
  annotate('text',x=7,y=1.3,
           label = 'Llogistic分布模型拟合图') +
  annotate('text',x=4,y=1.3,label = '图5：') +
  annotate('text',x=5,y=1.1,label = 'shape(a)') +
  annotate('text',x=7,y=1.1,label = '1.719') +
  annotate('text',x=7,y=1.0,label = '(0.00064)') +
  
  annotate('text',x=5,y=0.8,label = 'rate(b)') +
  annotate('text',x=7,y=0.8,label = '2.235') +
  annotate('text',x=7,y=0.7,label = '(0.00100)')


p_llogis <- pllogis(x,sha_llogis,rate_llogis)

p_llogis_br <- pllogis(breaks,sha_llogis,rate_llogis)

p_llogis_br[length(p_llogis_br)] <- 1

p_llogis_br <- diff(p_llogis_br)

freq_llogis<-length(x)*p_llogis_br

sum((freq_llogis-freq_x)^2/freq_llogis)

chisq.test(freq_x,p=(freq_llogis/length(x)))

max(abs(p_x_br-p_llogis_br)) 

1.36/sqrt(length(x))

#################################model result========================================  
rlt_lnorm

rlt_pareto

rlt_weibull

rlt_burr

rlt_llogis

rlt_model <- data.frame(type= 0 ,para = 0, value = 0,sd = 0)

rlt_model[1,1] <- '对数正态'
rlt_model[3,1] <- 'Pareto'
rlt_model[5,1] <- 'Weibull'
rlt_model[7,1] <- 'Burr'
rlt_model[10,1] <- 'Loglogistic'

rlt_model[1:2,2] <- attr(rlt_lnorm$sd,'names')
rlt_model[3:4,2] <- attr(rlt_pareto$sd,'names')
rlt_model[5:6,2] <- attr(rlt_weibull$sd,'names')
rlt_model[7:9,2] <- attr(rlt_burr$sd,'names')
rlt_model[10:11,2] <- attr(rlt_llogis$sd,'names')



rlt_model[1:2,3] <- rlt_lnorm$estimate
rlt_model[3:4,3] <- rlt_pareto$estimate
rlt_model[5:6,3] <- rlt_weibull$estimate
rlt_model[7:9,3] <- rlt_burr$estimate
rlt_model[10:11,3] <- rlt_llogis$estimate


rlt_model[1:2,4] <- rlt_lnorm$sd
rlt_model[3:4,4] <- rlt_pareto$sd
rlt_model[5:6,4] <- rlt_weibull$sd
rlt_model[7:9,4] <- rlt_burr$sd
rlt_model[10:11,4] <- rlt_llogis$sd

rlt_model[3,2] <- paste(rlt_model[3,2],'(a)',sep='')
rlt_model[4,2] <- paste(rlt_model[4,2],'(s)',sep='')
rlt_model[5,2] <- paste(rlt_model[5,2],'(a)',sep='')
rlt_model[6,2] <- paste(rlt_model[6,2],'(b)',sep='')
rlt_model[7,2] <- paste(rlt_model[7,2],'(a)',sep='')
rlt_model[8,2] <- paste(rlt_model[8,2],'(b)',sep='')
rlt_model[9,2] <- paste(rlt_model[9,2],'(s)',sep='')
rlt_model[10,2] <- paste(rlt_model[10,2],'(a)',sep='')
rlt_model[11,2] <- paste(rlt_model[11,2],'(s)',sep='')


rlt_model[is.na(rlt_model)] <- ''
rlt_model$value <- round(rlt_model$value,3)
rlt_model$sd <- round(rlt_model$sd,5)

save(rlt_model,file='e:/pyr/doc/fig2.rdata')

#################################freq result========================================  

df_freq <- data.frame(floor(freq_lnorm),floor(freq_pareto),
           floor(freq_weibull),floor(freq_burr),floor(freq_llogis))
           
df_freq_p <- data.frame(p_x_br,p_lnorm_br,p_pareto_br,
                        p_weibull_br,p_burr_br,
                        p_llogis_br)

names(df_freq_p) <- c('费用(万元)','实际值','Lnorm','Pareto',
                      'Weibull','Burr','Llogistic')

df_freq_p[,2:7] <- round(df_freq_p[,2:7],3)



save(df_freq_p,file='e:/pyr/doc/fig3.rdata')
xtable(df_freq_p)


#################################freq plot========================================  

x1 = sort(x1)

p_x1 = (1:length(x1))/length(x1)
p_llogis_x1 <- pllogis(x1,sha_llogis,rate_llogis)
p_burr_x1  <- pburr(x1,sha1_burr,sha2_burr,rate_burr)
p_weibull_x1 <- pweibull(x1,sha_weibull,sca_weibull)
p_pareto_x1 <- ppareto(x1,sha_pare,sca_pare)
p_lnorm_x1 <- plnorm(x1,mlog,sdlog)

df_cdf <- data.frame(x1,p_x1,p_llogis_x1,p_burr_x1,
           p_weibull_x1,p_pareto_x1,
           p_lnorm_x1)

df_cdf <- melt(df_cdf,id='x1')


ggplot(df_cdf,aes(x = x1,y = value,colour = 分布模型)) +
      geom_line() +
      #scale_colour_brewer(palette = 'set1') +
      scale_x_continuous(breaks = seq(0,12,by=3)) +
      coord_cartesian(xlim = c(0,12)) +
      labs(x = '住院医疗总费用(万元)',y = '累计概率' ) +
      theme(legend.position = c(0.8,0.5)) +
      theme(legend.background = element_blank()) +
      theme(legend.key = element_blank())


levels(df_cdf$variable) <- c('实际概率','Llogisitc',
                             'Burr','Weibull','Pareto'
                             ,'Lnorm')

names(df_cdf)[2] <- c('分布模型')




#################################loop part========================================  

test_llogis <- function(m,x,loopnum){
  loopnum <- loopnum
  rlt_shape <- 0
  rlt_rate <- 0
  rlt_ks <- 0
  rlt_chisq <- 0
  for (loopcnt in 1:loopnum){
    x1<-sample(x,2000)
    rlt_llogis <- fitdistr(x = x1,
                         densfun = m,
                         start = list(shape = 1.7,rate=2),# need to provide named list of starting values
                         lower = list(shape = 1,rate=1))
 
    est_shape <- rlt_llogis$estimate[1]
    est_rate <- rlt_llogis$estimate[2]
    
    rlt_shape[loopcnt] <-est_shape
    rlt_rate[loopcnt] <- est_rate
    rlt_ks_rep <- ks.test(x1,'pllogis',est_shape,est_rate)
    rlt_ks[loopcnt] <- rlt_ks_rep$p.value
  
    p_llogis <- pllogis(x1,sha_llogis,rate_llogis)
    p_llogis_br <- pllogis(breaks,sha_llogis,rate_llogis)
    p_llogis_br[length(p_llogis_br)] <- 1
    p_llogis_br <- diff(p_llogis_br)
    freq_llogis<-length(x1)*p_llogis_br
    freq_x1<-cut(x1,breaks= breaks)
    freq_x1<-table(freq_x1)
    rlt_chisq_rep <- chisq.test(freq_x1,p=(freq_llogis/length(x1)))
    rlt_chisq[loopcnt] <- rlt_chisq_rep$p.value
    }
  df_rlt <- data.frame(rlt_shape,rlt_rate,rlt_ks,rlt_chisq) 
  return(df_rlt)
    }

rlt <- test_llogis(dllogis,x,1000)

sum(rlt$rlt_ks <= 0.05) 




############################test lnorm==========================
test_lnorm <- function(m,x,loopnum){
  loopnum <- loopnum
  para1 <- 0
  para2 <- 0
  rlt_ks <- 0
  rlt_chisq <- 0
  for (loopcnt in 1:loopnum){
    x1<-sample(x,2000)
    rlt_lnorm <- fitdistr(x = x1,
                           densfun = m,
                           start = list(meanlog = 0,sdlog=1),# need to provide named list of starting values
                           lower = list(meanlog = 0,sdlog=1))
    
    para1_rep <-rlt$estimate[1]
    para2_rep <- rlt$estimate[2]
    para1[loopcnt] <- para1_rep
    para2[loopcnt] <- para2_rep
    rlt_ks_rep <- ks.test(x1,'plnorm',para1_rep,para2_rep)
    rlt_ks[loopcnt] <- rlt_ks_rep$p.value
    
    #p_lnorm <- plnorm(x1,para1,para2)
    #p_lnorm_br <- plnorm(breaks,para1,para2)
    #p_lnorm_br[length(p_lnorm_br)] <- 1
    #p_lnorm_br <- diff(p_lnorm_br)
    #freq_lnorm<-length(x1)*p_lnorm_br
    #freq_x1<-cut(x1,breaks= breaks)
    #freq_x1<-table(freq_x1)
    #rlt_chisq_rep <- chisq.test(freq_x1,p=(freq_lnorm/length(x1)))
    #rlt_chisq[loopcnt] <- rlt_chisq_rep$p.value
  }
  df_rlt <- data.frame(para1,para2,rlt_ks) 
  return(df_rlt)
}


rlt <- test_lnorm(dlnorm,x,100)

sum(rlt$rlt_ks < 0.05)  


############################test  weibull================================
test_weibull <- function(m,x,loopnum){
  loopnum <- loopnum
  para1 <- 0
  para2 <- 0
  rlt_ks <- 0
  #rlt_chisq <- 0
  for (loopcnt in 1:loopnum){
    x1<-sample(x,2000)
    rlt_lnorm <- fitdistr(x = x1,
                          densfun = m,
                          start = list(shape = 1,scale=1),# need to provide named list of starting values
                          lower = list(shape = 0,scale=0))
    
    para1_rep <-rlt$estimate[1]
    para2_rep <- rlt$estimate[2]
    para1[loopcnt] <- para1_rep
    para2[loopcnt] <- para2_rep
    rlt_ks_rep <- ks.test(x1,'pweibull',para1_rep,para2_rep)
    rlt_ks[loopcnt] <- rlt_ks_rep$p.value
    
    #p_lnorm <- plnorm(x1,para1,para2)
    #p_lnorm_br <- plnorm(breaks,para1,para2)
    #p_lnorm_br[length(p_lnorm_br)] <- 1
    #p_lnorm_br <- diff(p_lnorm_br)
    #freq_lnorm<-length(x1)*p_lnorm_br
    #freq_x1<-cut(x1,breaks= breaks)
    #freq_x1<-table(freq_x1)
    #rlt_chisq_rep <- chisq.test(freq_x1,p=(freq_lnorm/length(x1)))
    #rlt_chisq[loopcnt] <- rlt_chisq_rep$p.value
  }
  df_rlt <- data.frame(para1,para2,rlt_ks) 
  return(df_rlt)
}

rlt <- test_weibull(dweibull,x,1000)

sum(rlt$rlt_ks < 0.05) 

############################test  pareto================================
test_pareto <- function(m,x,loopnum){
  loopnum <- loopnum
  para1 <- 0
  para2 <- 0
  rlt_ks <- 0
  #rlt_chisq <- 0
  for (loopcnt in 1:loopnum){
    x1<-sample(x,2000)
    rlt <- fitdistr(x = x1,
                          densfun = m,
                          start = list(shape = 2,scale=2),# need to provide named list of starting values
                          lower = list(shape = 1,scale=1))
    
    para1_rep <-rlt$estimate[1]
    para2_rep <- rlt$estimate[2]
    para1[loopcnt] <- para1_rep
    para2[loopcnt] <- para2_rep
    rlt_ks_rep <- ks.test(x1,'ppareto',para1_rep,para2_rep)
    rlt_ks[loopcnt] <- rlt_ks_rep$p.value
    
    #p_lnorm <- plnorm(x1,para1,para2)
    #p_lnorm_br <- plnorm(breaks,para1,para2)
    #p_lnorm_br[length(p_lnorm_br)] <- 1
    #p_lnorm_br <- diff(p_lnorm_br)
    #freq_lnorm<-length(x1)*p_lnorm_br
    #freq_x1<-cut(x1,breaks= breaks)
    #freq_x1<-table(freq_x1)
    #rlt_chisq_rep <- chisq.test(freq_x1,p=(freq_lnorm/length(x1)))
    #rlt_chisq[loopcnt] <- rlt_chisq_rep$p.value
  }
  df_rlt <- data.frame(para1,para2,rlt_ks) 
  return(df_rlt)
}

rlt <- test_pareto(dpareto,x,1000)
sum(rlt$rlt_ks < 0.05) 

############################test burr================================

test_burr <- function(m,x,loopnum){
  loopnum <- loopnum
  para1 <- 0
  para2 <- 0
  para3 <- 0
  rlt_ks <- 0
  #rlt_chisq <- 0
  for (loopcnt in 1:loopnum){
    x1<-sample(x,2000)
    rlt <- fitdistr(x = x1,
                    densfun = m,
                    start = list(shape1 = 1,shape2=1.7,rate = 2),# need to provide named list of starting values
                    lower = list(shape1 = 1,shape2=1,rate = 1))
    
    para1_rep <- rlt$estimate[1]
    para2_rep <- rlt$estimate[2]
    para3_rep <- rlt$estimate[3]
    para1[loopcnt] <- para1_rep
    para2[loopcnt] <- para2_rep
    para3[loopcnt] <- para3_rep
    rlt_ks_rep <- ks.test(x1,'pburr',para1_rep,
                          para2_rep,para3_rep)
    rlt_ks[loopcnt] <- rlt_ks_rep$p.value
  }
  df_rlt <- data.frame(para1,para2,para3,rlt_ks) 
  return(df_rlt)
}

rlt <- test_burr(dburr,x,1000)
sum(rlt$rlt_ks < 0.5) 

x2 <- sample(x,2000) 
ks.test(x2,pburr,1,1.719,2.235)

