getfiles <- function(strpath) {
  rfiles = dir(strpath,full.names = TRUE)
  path.len = lapply(rfiles,nchar)
  path.len = as.numeric(path.len)
  path.df = data.frame(rfiles,path.len)
  path.df = path.df %>% arrange(rfiles) %>% arrange(path.len) 
  path.df$rfiles = as.character(path.df$rfiles)
  return (path.df$rfiles)
}

files15 <- getfiles('/mnt/e/pyr/data/y2015/2015x/')

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

sum_ttl <- df_fee_15 %>% group_by(id,name) %>% 
  summarise(n=n(),ttlfee_by_id = mean(ttlfee)) %>%
  filter(n > 2000) %>%
  arrange(ttlfee_by_id) %>%
  ungroup %>%
  View

df_fee_15 %>% filter(id == '410000000624')



quan999 <- quantile(df_fee_15$ttlfee,.9999)

quan001 <- quantile(df_fee_15$ttlfee,.0001)

df_fee_15 <- 
  df_fee_15 %>% filter(ttlfee < quan999 & ttlfee > quan001)

df_fee_15 %>% filter(ttlfee >= 200000)

hist_fee_15 <- 
        ggplot(df_fee_15,aes(x = ttlfee,
                         y = ..density..)) +
        geom_histogram(fill = 'cornsilk',color = 'grey60',
              binwidth = binsize) +
        scale_x_continuous(breaks = breaks,
                           limits = c(0,200000))
        geom_density()
                

binsize <- diff(range(df_fee_15$ttlfee)) / 2000
        
breaks = seq(0,max(df_fee_15$ttlfee),by = 50000)

  ggplot(df_fee_15,aes(x = ttlfee)) +
    geom_histogram(fill = 'cornsilk',color = 'purple',
                   binwidth = binsize) +
    scale_x_continuous(breaks = breaks) +
    coord_cartesian(xlim = c(0,150000))

df_fee_15_log <-        
    df_fee_15 %>% filter(ttlfee > 50) %>%
              group_by(id,name)   %>%
              summarise(n=n())
          
    smp_log <- sample(log(df_fee_15_log$ttlfee),1000)
    
    ggplot(df_fee_15_log,aes(x = log(ttlfee))) +
    geom_histogram(fill = 'cornsilk',color = 'purple') 
    
    pearson.test(smp_log)
    shapiro.test(smp_log)
    min(log(df_fee_15$ttlfee))  
    min(df_fee_15$ttlfee)
    exp(9)

str(hist_fee_15)


summary(df_fee_seld$ttlfee)



################################fin_rep================================
fina_rep = read_csv('e:/pyr/data/procdata/fee_inp.csv',col_names = FALSE,
                    col_types = NULL,
                    locale(encoding = 'gbk'))



fina_rep_inpfee15 = fina_rep %>% select(X1,X12,X13,X30,X31) 

fina_rep_inpfee15 = fina_rep_inpfee15[4:nrow(fina_rep_inpfee15),]

fina_rep_inpfee15[,2:5] <- lapply(fina_rep_inpfee15[,2:5] ,as.character)

fina_rep_inpfee15[,2:5] <- lapply(fina_rep_inpfee15[,2:5] ,as.numeric)

str(fina_rep_inpfee15)






#############################write part============================

write_feather(df_fee_seld,'./data/procdata/ttlfee_2015.pyr')

save(df_fee_seld,file='./data/procdata/ttlfee_2015.rdata')

save(hos_dic,file = 'e:/pyr/data/procdata/hos_dic.rdata')

write_feather(fina_rep_inpfee15,'./data/procdata/fina_rep_fee15.pyr')


hdf_fee <- H5Fcreate("./data/hdf5/R_fee_15.h5")

hdf_fee <- H5Fopen("./data/hdf5/R_fee_15.h5")

hos_dic <- data.frame(names(hos_dic),
                    hos_dic,stringsAsFactors = FALSE)

h5writeDataset.data.frame(hos_dic,hdf_fee, 'hos_dic', 
                          level=7, DataFrameAsCompound = TRUE)


df_fee_seld <- lapply(df_fee_seld,as.character)

df_fee_seld <- data.frame(df_fee_seld,stringsAsFactors = FALSE)

h5writeDataset.data.frame(df_fee_seld,hdf_fee, 'fee_15', 
                          level=7, DataFrameAsCompound = TRUE)

h5writeDataset.data.frame(fina_rep_inpfee15,hdf_fee, 'fin_fee_15', 
                          level=7)

H5Fclose(hdf_fee)

h5ls(h5_perhos)

py_hdf_fee <- H5Fopen("./data/hdf5/py_fee_15.h5")

H5Fis_hdf5("./data/hdf5/py_fee_15.h5")

H5Dread(py_hdf_fee,'fin_rep15')

H5Fcreate("./data/hdf5/py_fee_15.h5")

H5Fclose(py_hdf_fee)

H5Fis_hdf5("./data/hdf5/py_fee_15.h5")

H5Fopen("./data/hdf5/py_fee_15.h5")

H5Fclose("./data/hdf5/py_fee_15.h5")

H5close()

fin_rep15 <- h5read("./data/hdf5/py_fee_15.h5",'fin_rep15')$table

fin_rep15$X1<-str_conv(fin_rep15$X1,'utf-8')

fin_rep15 <- tbl_df(fin_rep15)

##########################perhos======================
h5_perhos <- H5Fcreate("./data/hdf5/R_perhos.h5")

h5_perhos <- H5Fopen("./data/hdf5/R_perhos.h5")

nyzxyy <- read_feather(
  'e:/pyr/data/y2015/2015perhos/410000002820_2015.pyr')

xmdyyy <-  read_feather(
  'e:/pyr/data/y2015/2015perhos/410000000884_2015.pyr')

out_ny <- nyzxyy %>% filter(x229 > 1000000)

out_xm <- xmdyyy %>% filter(x229 > 1000000)

out_xm %>% select(x229 : x260)

xmdyyy[,229:260] <- lapply(xmdyyy[,229:260],as.numeric)

h5writeDataset.data.frame(nyzxyy,h5_perhos,'410000002820', 
                          level=7, DataFrameAsCompound = TRUE)

h5writeDataset.data.frame(xmdyyy,h5_perhos,'410000000884', 
                          level=7)


H5Fclose(py_perhos_15)

py_perhos_15 = H5Fopen("./data/hdf5/py_perhos_15")

h5ls(py_perhos_15)

nyzxyy <- h5read(py_perhos_15,'fin_rep15')$table

nyzxyy <- tbl_df(nyzxyy)

nyzxyy

#############################load part==============================

write_feather(hos_dic,'./data/procdata/hos_dic.pyr')

load('./data/procdata/hos_dic.rdata')

load('./data/procdata/ttlfee_2015.rdata')

read_feather('./data/procdata/ttlfee_2015.pyr')


