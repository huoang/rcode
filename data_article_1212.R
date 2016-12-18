library(dplyr) 
library(readr) 
library(xtable) 
library(feather)
library(actuar)
library(FAdist)
library(MASS)
library(ggplot2)

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

kruskal.test(fee_15$ttlfee~fee_15$hoslvl)




fee_15 = read_feather('e:/pyr/data/procdata/fee_pertime.pyr')

x <- fee_15$ttlfee

x <- x/10000

breaks <- c(seq(0,3,by=0.3),
            seq(5,10,by=5),
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
    scale_x_continuous(breaks = seq(0,15,by=3)) +
    coord_cartesian(xlim = c(0,15))
  
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
      scale_x_continuous(breaks = seq(0,15,by=3)) +
      coord_cartesian(xlim = c(0,15))
    
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
      scale_x_continuous(breaks = seq(0,15,by=3)) +
      coord_cartesian(xlim = c(0,15))
    
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
      scale_x_continuous(breaks = seq(0,15,by=3)) +
      coord_cartesian(xlim = c(0,15))
    
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
    d<- dllogis(x1,sha_llogis,rate_llogis)
    df_llogis <- data.frame(x1,d)
    
    ggplot(df_llogis,aes(x = x1,y=..density..)) +
      geom_histogram(fill = 'cornsilk',color = 'purple',
                     binwidth = 0.2) +
      #geom_density()   +
      geom_line(aes(x=x1,y=d),colour="red")+
      #   stat_function(fun = dllogis,
      #              args =list(shape1 = 1,
      #                         shape2 = 1.73,rate =2.2))+
      scale_x_continuous(breaks = seq(0,15,by=3)) +
      coord_cartesian(xlim = c(0,15))
    
    p_llogis <- pllogis(x,sha_llogis,rate_llogis)
    
    p_llogis_br <- pllogis(breaks,sha_llogis,rate_llogis)
    
    p_llogis_br[length(p_llogis_br)] <- 1
    
    p_llogis_br <- diff(p_llogis_br)
    
    freq_llogis<-length(x)*p_llogis_br
    
    sum((freq_llogis-freq_x)^2/freq_llogis)
    
    chisq.test(freq_x,p=(freq_llogis/length(x)))
    
    max(abs(p_x_br-p_llogis_br)) 
    
    1.36/sqrt(length(x))
  #################################loop part========================================  
    
    loopnum <- 1000
    rlt_shape <- 0
    rlt_ks <-0
    for (loopcnt in 1:loopnum){
      x1<-sample(x,2000)
      rlt_llogis <- fitdistr(x = x1,
                  densfun = dllogis,
                  start = list(shape = 1.7,rate=2),# need to provide named list of starting values
                  lower = list(shape = 1,rate=1))
      est_shape <- rlt_llogis$estimate[1]
      est_rate <- rlt_llogis$estimate[2]
      rlt_shape[loopcnt]<-est_shape
      rlt_ks_rep <- ks.test(x1,'pllogis',est_shape,est_rate)
      rlt_ks[loopcnt] <- rlt_ks_rep$p.value
      
    }
    
    
    #df_rlt_keep <- data.frame(para=0,p=0)
    df_rlt <- data.frame(rlt_shape,rlt_ks)
    df_rlt[which(df_rlt$rlt_ks==max(df_rlt$rlt_ks)),]
    
    
    #i=1
    
    df_rlt_keep[i,] <- df_rlt[which(df_rlt$rlt_ks==max(df_rlt$rlt_ks)),] 
    i=i+1
    
    #j=1
    #rlt_mean <- data.frame(rlt_shape=0,rlt_ls=0)
    rlt_mean[j,] <- data.frame(lapply(df_rlt,mean))
    j=j+1
    rlt_mean
    
    
#######################################   ======================================    
    
    p_llogis <- pllogis(x,rlt_mean$rlt_shape,rate_llogis)
    
    p_llogis_br <- pllogis(breaks,rlt_mean$rlt_shape,rate_llogis)
    
    p_llogis_br[length(p_llogis_br)] <- 1
    
    p_llogis_br <- diff(p_llogis_br)
    
    freq_llogis<-length(x)*p_llogis_br
    
    sum((freq_llogis-freq_x)^2/freq_llogis)
    
    chisq.test(freq_x,p=(freq_llogis/length(x)))
    
    max(abs(p_x_br-p_llogis_br)) 
    
    1.36/sqrt(length(x))
    
    
