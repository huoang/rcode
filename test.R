x1 <- x


max(x1)
min(x1)
breaks <- seq(0,100,by = 20)

x <- sample(x1,100000)
freq_x<-cut(x,breaks= breaks)
freq_x<-table(freq_x)

rlt_burr <- fitdistr(x = x,
             densfun = dburr,
             start = list(shape1 = 1,shape2 = 1.7,rate=2),# need to provide named list of starting values
             lower = list(shape1 = 1,shape2 = 1,rate=1))

sha1_burr <- rlt_burr$estimate[1]
sha2_burr <- rlt_burr$estimate[2]
rate_burr <- rlt_burr$estimate[3]

p_burr <- pburr(x,sha1_burr,sha2_burr,rate_burr)

p_burr_br <- pburr(breaks,sha1_burr,sha2_burr,rate_burr)

p_burr_br[length(p_burr_br)] <- 1

p_burr_br <- diff(p_burr_br)

freq_burr<-length(x)*p_burr_br

sum((freq_burr-freq_x)^2/freq_burr)

chisq.test(freq_x,p=(freq_burr/length(x)))

ks.test(x,'pburr',sha1_burr,sha2_burr,rate_burr)
