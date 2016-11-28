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

sum_ttl <- df_fee_15 %>% group_by(id,name) %>% 
  summarise(n=n(),fee_by_id = mean(ttlfee)) %>%
  filter(n > 1000) %>%
  arrange(fee_by_id) %>%
  ungroup %>%
  View

#######################  S =======================================
hos_info <- read_csv('/mnt/e/pyr/data/procdata/hos_info.csv',
                     col_names = TRUE,
                     col_types = NULL,
                     locale(encoding = 'gbk'))


hos_info_colname <- colnames(hos_info)

hos_info_var <- data.frame(colnames(hos_info),hos_info_colname,
           stringsAsFactors = FALSE)


colnames(hos_info) <- paste('x',c(1:ncol(hos_info)),sep = '')

hoscodes <- levels(factor(df_fee_15$hoscode))

hos_info <- hos_info[hos_info$x2 %in% hoscodes,]

hos_info <- hos_info %>% select(x2,x3,x10,x12)

hoscode <- hos_info$x2 

hoslvl <- hos_info$x12

hosloc <- hos_info$x10

names(hoslvl) <- hoscode

names(hosloc) <- hoscode

df_fee_15$hoslvl <- hoslvl[df_fee_15$hoscode]

df_fee_15[df_fee_15$hoslvl == 9,]$hoslvl <- 1

df_fee_15$hosloc <- hosloc[df_fee_15$hoscode]

df_fee_15$hosloc <- substr(df_fee_15$hosloc,1,4)

areacode <- read.table('e:/pyr/data/procdata/area.txt')

urbancode <- areacode[areacode$V1 %in% paste(c(4101:4117,4190),
                                             '00',sep = ''),]
urbancode$V2 <- as.character(urbancode$V2)

urbancode$V2[18] <- '济源市'

names(urbancode) <- c('uid','uname')

uname <- urbancode$uname

uid <- urbancode$uid

uname <- gsub('(\\s+)','',uname)

names(uid) <- uname

gdp_urban <- read_csv('e:/pyr/data/procdata/gdp_urban.csv',
                      col_names = TRUE,
                      col_types = NULL,
                      locale(encoding = 'gbk'))
gdp_urban <- gdp_urban[order(gdp_urban$pcg,decreasing = TRUE),]

gdp_urban$ecolvl <- 1 

gdp_urban$ecolvl[7:12] <- 2

gdp_urban$ecolvl[13:18] <- 3

gdp_urban$urban <- gsub('(\\s+)','',gdp_urban$urban)

gdp_urban$uid <- uid[gdp_urban$urban]

gdp_urban$uid <- substr(gdp_urban$uid,1,4)

ecolvl <- gdp_urban$ecolvl

names(ecolvl) <- gdp_urban$uid

df_fee_15$ecolvl <- ecolvl[df_fee_15$hosloc]

################################ S  ====================================

df_fee_15$bird <- as.integer(df_fee_15$bird)

df_fee_15$rec <-as.integer(df_fee_15$rec)

df_fee_15$gen <-as.integer(df_fee_15$gen)

df_fee_15 <- df_fee_15 %>% arrange(-ttlfee)

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

write_feather(fee_15_rlt,'/mnt/e/pyr/data/procdata/fee_15.pyr')

df_fee_15[duplicated(h_n_g_b),]

df_fee_15 %>% filter(name == '')


fee_exc_nan  <- df_fee_15 %>% filter(!(name == 'nan' |
                        gen == 'nan' | bird == 'nan'))

fee_inc_nan  <- df_fee_15 %>% filter((name == 'nan' |
                                      gen == 'nan' | bird == 'nan'))


nmgenbir_exc_nan <- paste(df_fee_15_exc_nan$name,
                          df_fee_15_exc_nan$gen,
                          df_fee_15_exc_nan$bird,sep = '-')

fee_exc_nan_dup <- df_fee_15_exc_nan[!duplicated(nmgenbir_exc_nan),]


fee_exc_dup   <- rbind(fee_exc_nan_dup,fee_inc_nan)

fee_exc_dup <- tbl_df(fee_exc_dup)

fee_exc_dup %>% group_by(hoscode,hosname) %>% 
  summarise(n=n(),fee_by_id = mean(ttlfee)) %>% 
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

df_fee_15 <- read_feather('./data/procdata/py_ttl_fee.pyr')

kdxq <-  read_feather(
  'e:/pyr/data/y2015/2015perhos/410000206898_2015.pyr')


kdxq[,229:261] <- lapply(kdxq[,229:261],as.character)

kdxq[,229:261]<- lapply(kdxq[,229:261],as.numeric)

kdxq[is.na(kdxq)] <-0

kdxq <- kdxq %>% select(x35,x40,x69:x73,x121:x140,x229:x260) %>%
                 filter(x229 < 50) 

dups <- kdxq %>% select(x31,x32,x33,x34,x59,x64,x69:x72,x229)

dups[duplicated(dups$x31),] %>% View

dups[duplicated(dups$x32) & duplicated(dups$x33) &
       duplicated(dups$x34),] %>% 
       filter(x32 == '') %>%
       View

dups %>% filter(x32 == '')





