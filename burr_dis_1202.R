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
      (beta-1)*sum(log10(x))-(alpha+1)*sum(log10(lamda+x^beta))
  return (-logL)
}

sum(log10(1+x^2))

set.seed(123)
result <- optim(c(5,3,
                  1000000000),
                burr,x = sample(fee_15$ttlfee,100000))



a <- result$par[1]
b <- result$par[2]
s <- result$par[3]

y <- (a*b*(x/s)^b)/(x*(1+(x/s)^b)^(a+1))

plot_ <- plot(x,y)



warnings()


as.numeric(as.character(result$par))

-(1/(2*sigma2))*sum((x-mu)**2)

dburr(x, shape1, shape2, rate = 1, scale = 1/rate,
      log = FALSE)

exp(dburr(1, 2, 3, log = TRUE))
