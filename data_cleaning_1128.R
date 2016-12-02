df_fee = read_feather(files15[13])

df_veri = read_feather(files15[43])

df_fee = cbind(df_fee,df_veri)

df_fee = tbl_df(df_fee[,c(1,2,4)])

df_fee_seld = df_fee %>% filter(x262 == "False")

df_fee_seld$hosname = hos_dic[df_fee_seld$x5]

df_fee_seld =  df_fee_seld %>% select(x5,hosname,x229)

names(df_fee_seld) = c('id','name','ttlfee')

df_fee_seld = df_fee_seld %>% filter(!is.na(name) & !is.na(ttlfee))

df_fee_seld$ttlfee = as.numeric(df_fee_seld$ttlfee) 

df_fee_15 <- df_fee_seld %>% filter(ttlfee > 0)

df_fee_15 <- tbl_df(df_fee_15)

df_fee_15 <- df_fee_15 %>% filter(ttlfee < 1000000)

df_fee_15 <- tbl_df(df_fee_15)

str(df_fee_15)

codes <- levels(factor(df_fee_15$id))

names <- levels(factor(df_fee_15$name))

df_fee_15 <- df_fee_15 %>% filter(!is.na(name))

################################## S 2 ====================================

df_fee_15 <- read_feather('./data/procdata/py_ttl_fee.pyr')

df_fee_15 <- 
             df_fee_15 %>% filter(!(name == 'nan' & gen == 'nan' & bird == 'nan'))

df_fee_15 <- df_fee_15 %>% filter(ttlfee >100)


df_fee_15$bird <- as.integer(df_fee_15$bird)

df_fee_15$rec <-as.integer(df_fee_15$rec)

df_fee_15$gen <-as.integer(df_fee_15$gen)

df_fee_15 <- df_fee_15 %>% arrange(-ttlfee)

df_fee_15$name <- gsub('(-)','',df_fee_15$name)


h_n_g_b <- paste(df_fee_15$hoscode,df_fee_15$name,
                 df_fee_15$gen,df_fee_15$bird,sep = '-')


fee_15 <- cbind(h_n_g_b,df_fee_15$hosname,df_fee_15$rec,df_fee_15$ttlfee)

fee_15 <- tbl_df(fee_15)

names(fee_15) <- c('hngb','hosname','rec','ttlfee')

fee_15$ttlfee <- as.numeric(fee_15$ttlfee)

fee_15_rlt <- fee_15 %>% 
  select(hngb,ttlfee) %>%
  group_by(hngb) %>% 
  summarise(n = n(),mean = mean(ttlfee)
            ,ttl = sum(ttlfee)) 


df_hngb <- fee_15_rlt$hngb
    
df_hngb<-unlist(df_hngb)

df_hngb<-tstrsplit(df_hngb,split = '-',fixed =TRUE)  

df_hngb  <- data.frame(df_hngb)

colnames(df_hngb) <- c('hoscode','name','gen','bird')

df_hngb <- tbl_df(df_hngb)

fee_15_perhead <- cbind(df_hngb,fee_15_rlt$n,fee_15_rlt$mean,fee_15_rlt$ttl)

fee_15_perhead <- tbl_df(fee_15_perhead)

names(fee_15_perhead) <- c("hoscode","name","gen","bird","n","mean","ttl" )

fee_perhead_exna %>% summarise(max(n))

fee_perhead_exna <- fee_15_perhead %>% filter(!(n>10 & name == 'nan'))

fee_perhead_exna <- fee_perhead_exna %>% filter(ttl > 100)  

fee_perhead_exna %>% group_by(hoscode) %>% 
  summarise(n=n(),fee_by_id = mean(ttl)) %>% 
  filter (n > 1000) %>%
  View

write_feather(fee_perhead_exna,
              '/mnt/e/pyr/data/procdata/fee_perhead_exna.pyr')

write_feather(df_fee_15,
              '/mnt/e/pyr/data/procdata/df_fee_15.pyr')


################################ S3 ====================================


add_vars <- function(df){
    ## add hoslvl,hosloc
    hos_info <- read_csv('/mnt/e/pyr/data/procdata/hos_info.csv',
                     col_names = TRUE,
                     col_types = NULL,
                     locale(encoding = 'gbk'))
    areacode <- read_table('/mnt/e/pyr/data/procdata/area.txt',
                           col_names = FALSE,
                           col_types = NULL,
                           locale(encoding = 'gbk'))
    gdp_urban <- read_csv('/mnt/e/pyr/data/procdata/gdp_urban.csv',
                          col_names = TRUE,
                          col_types = NULL,
                          locale(encoding = 'gbk'))
    
    hos_info_colname <- colnames(hos_info)
    hos_info_var <- data.frame(colnames(hos_info),hos_info_colname,
                           stringsAsFactors = FALSE)
    colnames(hos_info) <- paste('x',c(1:ncol(hos_info)),sep = '')
    hcode <- hos_info$x2
    hname <- hos_info$x3
    names(hname) <- hcode
    hoscodes <- levels(factor(df$hoscode))
    hos_info <- hos_info[hos_info$x2 %in% hoscodes,]
    hos_info <- hos_info %>% select(x2,x3,x10,x12)
    hoscode <- hos_info$x2 
    hoslvl <- hos_info$x12
    hosloc <- hos_info$x10
    hoslvl[hoslvl == 9] <- 1
    names(hoslvl) <- hoscode
    names(hosloc) <- hoscode
    df$hoscode <- as.character(df$hoscode)
    df$hoslvl <- hoslvl[df$hoscode]
    df$hosloc <- hosloc[df$hoscode]
    df$hosloc <- substr(df$hosloc,1,4)
    urbancode <- areacode[areacode$X1 %in% 
                         paste(c(4101:4117,4190),
                         '00',sep = ''),]
    urbancode$X2 <- as.character(urbancode$X2)
    urbancode$X2[18] <- '济源市'
    names(urbancode) <- c('uid','uname')
    uname <- urbancode$uname
    uid <- urbancode$uid
    uname <- gsub('(\\s+)','',uname)
    names(uid) <- uname
    gdp_urban <- gdp_urban[order(gdp_urban$pcg,
                          decreasing = TRUE),]
    gdp_urban$ecolvl <- 1 
    gdp_urban$ecolvl[7:12] <- 2
    gdp_urban$ecolvl[13:18] <- 3
    gdp_urban$urban <- gsub('(\\s+)','',gdp_urban$urban)
    gdp_urban$uid <- uid[gdp_urban$urban]
    gdp_urban$uid <- substr(gdp_urban$uid,1,4)
    ecolvl <- gdp_urban$ecolvl
    names(ecolvl) <- gdp_urban$uid
    df$ecolvl <- ecolvl[df$hosloc]
    df$hosname <- hname[df$hoscode]
    return(df)
    }

fee_perhead <- read_feather(
  '/mnt/e/pyr/data/procdata/fee_perhead_exna.pyr')

fee_pertime <- read_feather(
  '/mnt/e/pyr/data/procdata/df_fee_15.pyr')


fee_perhead <- add_vars(fee_perhead)     

names(fee_perhead)[7] <- 'ttlfee'

fee_pertime <- add_vars(fee_pertime)





selcode <-fee_perhead %>% group_by(hoscode,hosname) %>% 
           summarise(n=n(),fee_by_id = mean(ttlfee)) %>%
           filter(n > 1000) %>%
           select(hoscode)
fee_perhead <- fee_perhead[fee_perhead$hoscode %in% selcode$hoscode,]

fee_pertime <- fee_pertime[fee_pertime$hoscode %in% selcode$hoscode,]

write_feather(fee_perhead,'/mnt/e/pyr/data/procdata/fee_perhead.pyr')

write_feather(fee_pertime,'/mnt/e/pyr/data/procdata/fee_pertime.pyr')

fee_pertime %>% group_by(hoscode,hosname) %>% 
  summarise(n=n(),fee_by_id = mean(ttlfee)) %>% 
  View

###################################S 4==============================

df_fee_15 <- df_fee_15 %>% 
  select(hoscode,hosname,hoslvl,hosloc,ecolvl,ttlfee)

fee_perhead %>% group_by(hosloc) %>%
  summarise(n=n(),med=median(ttlfee),mean=mean(ttlfee))

fee_pertime %>% group_by(hoslvl) %>%
  summarise(n=n(),med=median(ttlfee),mean=mean(ttlfee))

fee_pertime %>% group_by(ecolvl) %>%
  summarise(n=n(),med=median(ttl),mean=mean(ttl))

fee_perhead %>% filter(is.na(ecolvl))



df_fee_15 %>% group_by(ecolvl) %>%
  summarise(n=n(),med=median(ttlfee),mean=mean(ttlfee))

myplot <- function(df,binsize,breaks){
  binsize <- diff(range(df$ttlfee)) / binsize
  breaks = seq(0,max(df$ttlfee),by = breaks)
  ggplot(df,aes(x = ttlfee)) +
    geom_histogram(fill = 'cornsilk',color = 'purple',
                 binwidth = binsize) +
    scale_x_continuous(breaks = breaks) +
    coord_cartesian(xlim = c(0,100000))
    }
myplot(fee_perhead,3000,50000)

summary(fee_pertime)

psych :: describe(fee_pertime$ttlfee)





###############################S 5=============================

var <- fee_15$ttlfee
h <- 2000
p_gau<-function(var,h){
  n<-length(var)
  rlt_p<-lapply(var,function(x){
    numerator<-sum(exp(-(((x-var)^2)/(2*h*h))))
    denominator<-n*h*sqrt(2*pi)
    p<-numerator/denominator
    return(p)})
  rlt_p<-as.numeric(rlt_p)
  plot(var,rlt_p)
  return(rlt_p)				 
}

p_gau(fee_15$ttlfee,2000)

??Burr

exp(dburr(1, 2, 3, log = TRUE))

x <- rnorm(10,5,6)

plot(pburr(x,1,10,5))

str(pburr(x,1,10,5))

pburr(qburr(p, 2, 3, 2), 2, 3, 2)



f<-function(y,a,b,h,t){
  k<-(b-a)*y+a+b-2*t
  z<-exp(-k*k/(8*h*h))
  z
}


I<-function(a,b,h,t){
  x1<-0.8611363;x2<-0.339981
  A<-0.3478548;B<-0.6521452
  q<-A*(f(x1,a,b,h,t)+f((-1)*x1,a,b,h,t))+
    B*(f(x2,a,b,h,t)+f((-1)*x2,a,b,h,t))
  q
}

p<-function(a,b,h,vecvar){
  y<-I(a,b,h,vecvar)
  m<-2*sqrt(2*pi)*h*length(vecvar)
  p<-(b-a)*y/m
  sum(p)
}

p_fee <- p(min(fee_15$ttlfee),max(fee_15$ttlfee),300000,fee_15$ttlfee)



len <- 24
x <- runif(len, 0.1, 1)
y <- x^3 + rnorm(len, 0, 0.06)
ds <- data.frame(x = x, y = y)
str(ds)

plot(y ~ x, main = "Known cubic, with noise")
s <- seq(0, 1, length = 100)
lines(s, s^3, lty = 2, col = "green")

m <- nls(y ~ I(x^power), data = ds,
         start = list(power = 1), trace = T)

summary(m)

log(y)
model <- lm(I(log(y)) ~ I(log(x)))








##############################I/O part============================


write_feather(df_fee_15,'./data/procdata/py_ttl_fee.pyr')

df_fee_15 <- read_feather('./data/procdata/py_ttl_fee.pyr')



load('./data/procdata/hos_dic.rdata')

fee_perhead <- read_feather('/mnt/e/pyr/data/procdata/fee_perhead_full.pyr') ##

fee_15 <- read_feather('e:/pyr/data/procdata/df_fee_15.pyr')  ##


