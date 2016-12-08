dat <- rgamma(100, 3, 2)
f <- function(x, dat) {
  a <- x[1]
  b <- x[2]
  return(-sum(a*log(b)-log(gamma(a))+(a-1)*log(dat)-b*dat))
}

m <- optim(c(2, 1), f, dat=dat)

summary(m)

library(MASS)

fitdistr(fee_15$ttlfee, densfun = "Burr", lower = 0)

fitdistr

ks.test(fee_15$ttlfee,'pburr',1,780)


LL<-function(params,data)

LL<-function(params,data)
    + {#参数"params"是一个向量，依次包含了五个参数：p,mu1,sigma1,
      + #mu2,sigma2.
        + #参数"data"，是观测数据。
        + t1<-dnorm(data,params[2],params[3])
        + t2<-dnorm(data,params[4],params[5])
        + #这里的dnorm()函数是用来生成正态密度函数的。
          + f<-params[1]*t1+(1-params[1])*t2
          + #混合密度函数
            + ll<-sum(log(f))
            + #log-likelihood函数
              + return(-ll)
            + #nlminb()函数是最小化一个函数的值，但我们是要最大化log-
              + #likeilhood函数，所以需要在“ll”前加个“-”号。
              + }



burr <- function(theta,x){
  alpha <- theta[1]
  beta <- theta[2]
  lamda <- theta[3]
   n <- length(x)
   logL <- n*log(alpha)+n*log(beta)+n*alpha*log(lamda)+
      (beta-1)*sum(log(x))-(alpha+1)*sum(log(lamda+x^beta))
  return (-logL)
}

burr <- function(theta,x){
  c <- theta[1]
  k <- theta[2]
  t <- theta[3]
  #n <- length(x)
  cdf <- 1-(1+(x/t)^c)^k+1
  return (cdf)
}


result <- optim(c(3,2,2), burr,x=x)


burr_maxlik <- function(theta){
  alpha <- theta[1]
  beta <- theta[2]
  lamda <- theta[3]
  n <- length(x)
  logL <- n*log(alpha)+n*log(beta)+n*alpha*log(lamda)+
    (beta-1)*sum(log(x))-(alpha+1)*sum(log(lamda+x^beta))
  return (logL)
}
#x <- x*1000
N <- length(x)
rlt <- maxLik(burr_maxlik,start=c(3,3,2))

logb(5,5)
10^0.69897
log(5)
a <- 2
n <- 10
b <- 3
L <- a^n * b^n
log(L)
n*log(a) + n*log(b)





actuar_do_random

??actual

warnings()

   a <- rlt$estimate[1]
   b <-rlt$estimate[2]
   s <-rlt$estimate[3]


   rburr(100,a,b,s)

   hist(rburr(100,a,b,s))

   hist(x)
save(rlt,file = '/mnt/e/pyr/data/procdata/burrrlt.rdata')


x <- rburr(100,5,3,4)

p <- pburr(x,3,2,2)

q <- qburr(x,3,2,2)

hist(x)

mburr(2,3,2,2)

mburr(1,3,2,2)^2

levburr(10, 2, 3, 1, order = 2)

levburr(10, 1.5, 0.5, 1, order = 2)




??rburr
plot(x,p)

hist(sburr)

plot(sburr)




set.seed(123)




a <- result$par[1]
b <- result$par[2]
s <- result$par[3]



plot_ <- plot(x,y)



warnings()


as.numeric(as.character(result$par))

-(1/(2*sigma2))*sum((x-mu)**2)

hist(dburr(sburr, 4, 2, 2, log = FALSE))



exp(dburr(1, 2, 3, log = TRUE))

sburr <- rburr(100,4,2,2)

plot(sburr)

hist(sburr)

10^log10(5)


x <- rburr(100,3,2,1) * 1000

x <-c(2042.044,2218.153,3670.893,5149.684,
     5533.429,7111.183,11041.569,15459.771,
     783.477,1701.520,40810.770,67905.857)

hist(x)

c <- 2.1;k<-0.4;theta<-2460


cdf <- 1-(1+(x/theta)^c)^(k+1)

hist(cdf)

p <- pburr(cdf,3,2)

q <- qburr(x,3,2,1)

d <- dburr(x,5,3,1)

plot(x,cdf)

plot(x,p)

data.frame(x,d)



dburr <- function(x, c = 1, k = 1,t=2000) 1 - (1 + (x / t)^ c)^(k+1)

# Simulate data from log logistic for a test case
library(FAdist)
obs <- rllog(100)

library(actuar)

x <- rburr(100,6,3,1)

min(x);max(x)

x1 <- sample(x,1000)

x2 <- rburr(1000,1,1.72,2.09)

d <- dburr(x1,1,1.72,2.09)

d <- dburr(x2,1,1.72,2.09)

plot(x2,d)

x <- fee_15$ttlfee

x <- x[x<200000]

x <- x/10000



library(MASS)


fitdistr(x = sample(x,100000),
         densfun = dlnorm,
         start = list(meanlog = 0,sdlog = 1),# need to provide named list of starting values
         lower = list(meanlog= 0,sdlog = 1))

x1 <- sample(x,100000)

d <- dnorm(x1,8.43,1.04)

d <- dllogis(x1,1.72,2.29)

plot(x1,d)

hist(log(x))

??actuar

plot(x,dnorm(x))

?dlnorm

set.seed(123)
x1 = sample(x,10000)

##dnorm
lx1 = log(x1)
hist(lx1)

fitdistr(x = lx1,
         densfun = dnorm,
         start = list(mean = 0,sd = 1),# need to provide named list of starting values
         lower = list(mean= 0,sd = 1))

d <- dnorm(lx1,0,1.295)

df_dnorm <- data.frame(lx1,d)

head(df_dnorm)

ggplot(df_dnorm,aes(x = lx1,y=..density..)) +

  geom_histogram(fill = 'cornsilk',color = 'purple',
                 binwidth = 0.2) +
  #geom_density()   +
  geom_line(aes(x=lx1,y=d),colour="red")+
  #   stat_function(fun = dburr,
  #              args =list(shape1 = 1,
  #                         shape2 = 1.73,rate =2.2))+
  scale_x_continuous(breaks =c(-1,0,1)) +
  coord_cartesian(xlim = c(-5,5))

##dlnorm
fitdistr(x = x1,
         densfun = dlnorm,
         start = list(meanlog = 0,sdlog = 1),# need to provide named list of starting values
         lower = list(meanlog= 0,sdlog = 1))

d<- dlnorm(x1,0,1.295)

df_dlnorm <- data.frame(x1,d)

head(df_dlnorm)

ggplot(df_dlnorm,aes(x = x1,y=..density..)) +

  geom_histogram(fill = 'cornsilk',color = 'purple',
                 binwidth = 0.2) +
  #geom_density()   +
  geom_line(aes(x=x1,y=d),colour="red")+
  #   stat_function(fun = dburr,
  #              args =list(shape1 = 1,
  #                         shape2 = 1.73,rate =2.2))+
  scale_x_continuous(breaks =c(-1,0,1)) +
  coord_cartesian(xlim = c(0,5))


fee_15

##dburr
set.seed(123)
x1 = sample(x,100000)

rlt_dburr <- fitdistr(x = x1,
         densfun = dburr,
         start = list(shape1 = 1,shape2 = 1.74,rate = 2.2),# need to provide named list of starting values
         lower = list(shape1 = 1,shape2 = 1,rate = 1))

shape1<-rlt_dburr$estimate[1]
shape2<-rlt_dburr$estimate[2]
rate<-rlt_dburr$estimate[3]

d <- dburr(x1,shape1,shape2,rate)

df_burr <- data.frame(x1,d)



 ggplot(df_burr,aes(x = x1,y=..density..)) +

     geom_histogram(fill = 'cornsilk',color = 'purple',
                 binwidth = 0.2) +
     #geom_density()   +
     geom_line(aes(x=x1,y=d),colour="red")+
  #   stat_function(fun = dburr,
  #              args =list(shape1 = 1,
  #                         shape2 = 1.73,rate =2.2))+
     scale_x_continuous(breaks = seq(0,8,by=2)) +
     coord_cartesian(xlim = c(0,10))


##dpareto
fitdistr(x = x1,
         densfun = dpareto,
         start = list(shape =1.6 ,scale = 1),# need to provide named list of starting values
         lower = list(shape = 1,scale = 1))

d <- dpareto(x1,3.46,2.04)

df_dpareto  <- data.frame(x1,d)


ggplot(df_dpareto,aes(x = x1,y=..density..)) +

  geom_histogram(fill = 'cornsilk',color = 'purple',
                 binwidth = 0.2) +
  #geom_density()   +
  geom_line(aes(x=x1,y=d),colour="red")+
  #   stat_function(fun = dburr,
  #              args =list(shape1 = 1,
  #                         shape2 = 1.73,rate =2.2))+
  scale_x_continuous(breaks =  seq(0,8,by=2)) +
  coord_cartesian(xlim = c(0,10))


set.seed(123)
x1 = sample(x,100000)

##dgllogis
rlt_dllogis<-fitdistr(x = x1,
         densfun = dllogis,
         start = list(shape =1.6 ,rate = 1),# need to provide named list of starting values
         lower = list(shape = 1,rate = 1))

d <- dllogis(x1,1.73,2.22)

df_dllogis  <- data.frame(x,d)


ggplot(df_dllogis,aes(x = x,y=..density..)) +

  geom_histogram(fill = 'cornsilk',color = 'purple',
                 binwidth = 0.2) +
  #geom_density()   +
  geom_line(aes(x=x,y=d),colour="red")+
  #   stat_function(fun = dburr,
  #              args =list(shape1 = 1,
  #                         shape2 = 1.73,rate =2.2))+
  scale_x_continuous(breaks =  seq(0,8,by=2)) +
  coord_cartesian(xlim = c(0,10))

##dgllogis
fitdistr(x = x1,
         densfun = dweibull,
         start = list(shape =2 ,scale = 1),# need to provide named list of starting values
         lower = list(shape = 1,scale = 1))

d <- dweibull(x1,1,1)

df_dweibull  <- data.frame(x1,d)


ggplot(df_dweibull,aes(x = x1,y=..density..)) +

  geom_histogram(fill = 'cornsilk',color = 'purple',
                 binwidth = 0.2) +
  #geom_density()   +
  geom_line(aes(x=x1,y=d),colour="red")+
  #   stat_function(fun = dburr,
  #              args =list(shape1 = 1,
  #                         shape2 = 1.73,rate =2.2))+
  scale_x_continuous(breaks =  seq(0,8,by=2)) +
  coord_cartesian(xlim = c(0,10))

##dgamma
fitdistr(x = x1,
         densfun = dgamma,
         start = list(shape =2 ,rate = 1),# need to provide named list of starting values
         lower = list(shape = 1,rate = 1))

d <- dgamma(x1,1,1.65)

df_dgamma  <- data.frame(x1,d)


ggplot(df_dgamma,aes(x = x1,y=..density..)) +

  geom_histogram(fill = 'cornsilk',color = 'purple',
                 binwidth = 0.2) +
  #geom_density()   +
  geom_line(aes(x=x1,y=d),colour="red")+
  #   stat_function(fun = dburr,
  #              args =list(shape1 = 1,
  #                         shape2 = 1.73,rate =2.2))+
  scale_x_continuous(breaks =  seq(0,8,by=2)) +
  coord_cartesian(xlim = c(0,10))

?dgamma
