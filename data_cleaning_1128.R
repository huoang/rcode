################################ S  ====================================

df_fee_15 <- read_feather('./data/procdata/py_ttl_fee.pyr')

n_g_b <- paste(df_fee_15$name,
                 df_fee_15$gen,df_fee_15$bird,sep = '-')
head(n_g_b)

fee_na <- df_fee_15 %>% filter(name == 'nan' | is.na(gen) | is.na(bird))

fee_15_perhead %>% summarise(max = max(ttl),min =min(ttl))

fee_15_perhead %>% filter(ttl < 100) %>% View

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


  
fee_15_rlt <- read_feather('/mnt/e/pyr/data/procdata/fee_15.pyr')

df_hngb <- fee_15_rlt$hngb
    
df_hngb<-unlist(df_hngb)


df_hngb<-tstrsplit(df_hngb,split = '-',fixed =TRUE)  

df_hngb  <- data.frame(df_hngb)

colnames(df_hngb) <- c('hoscode','name','gen','bird')

df_hngb <- tbl_df(df_hngb)

fee_15_perhead <- cbind(df_hngb,fee_15_rlt$n,fee_15_rlt$mean,fee_15_rlt$ttl)

fee_15_perhead <- tbl_df(fee_15_perhead)

names(fee_15_perhead) <- c("hoscode","name","gen","bird","n","mean","ttl" )

fee_perhead_exna <- fee_15_perhead %>% filter(!(name == 'nan' & gen == 'NA' & bird == 'NA')) 

fee_perhead_exna %>% summarise(max(n))

fee_perhead_exna <- fee_perhead_exna %>% filter(!(n>10 & name == 'nan'))

fee_perhead_exna <- fee_perhead_exna %>% filter(ttl > 100)  

fee_perhead_exna %>% group_by(hoscode) %>% 
  summarise(n=n(),fee_by_id = mean(ttl)) %>% 
  filter (n > 1000) %>%
  View

dups_exc_nan <- df_fee_15_exc_nan[duplicated(nmgenbir_exc_nan),]

dups_exc_nan %>% group_by(hosname) %>% summarise(n=n())

df_fee_15 %>% filter(hoscode == '410000000708') 

fee_exc_dup %>% filter(hoscode == '410000000708') 

(353952 - 245681) / 353952

(177290 - 126288) / 177290

df_fee_15 <- df_fee_15 %>% 
  select(hoscode,hosname,hoslvl,hosloc,ecolvl,ttlfee)


?duplicated

minouts <- df_fee_15 %>% filter(ttlfee < 50)

qs <- c(0, 0.25, 0.75)

df_fee_15 %>% group_by(hoslvl) %>%
  summarise(n=n(),quan = list(quantile(ttlfee,qs)),
            med=median(ttlfee),mean=mean(ttlfee)) %>%
  str




df_fee_15 %>% group_by(hosloc) %>%
  summarise(n=n(),med=median(ttlfee),mean=mean(ttlfee))


df_fee_15 %>% group_by(ecolvl) %>%
  summarise(n=n(),med=median(ttlfee),mean=mean(ttlfee))

binsize <- diff(range(df_fee_15$ttlfee)) / 2000

breaks = seq(0,max(df_fee_15$ttlfee),by = 50000)


ggplot(df_fee_15[df_fee_15$ecolvl==1,],aes(x = ttlfee)) +
  geom_histogram(fill = 'cornsilk',color = 'purple',
                 binwidth = binsize) +
  scale_x_continuous(breaks = breaks) +
  coord_cartesian(xlim = c(0,100000))

##############################I/O part============================

write_feather(df_fee_15,'./data/procdata/py_ttl_fee.pyr')

write_feather(fee_15_rlt,'/mnt/e/pyr/data/procdata/fee_15.pyr')

write_feather(fee_15_rlt,'/mnt/e/pyr/data/procdata/fee_15_exc_na.pyr')

df_fee_15 <- read_feather('./data/procdata/py_ttl_fee.pyr')

