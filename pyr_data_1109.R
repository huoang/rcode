'e/pyr/data/y2015/2015x/'

getfiles <- function(strpath) {
  rfiles = dir(strpath,full.names = TRUE)
  path.len = lapply(rfiles,nchar)
  path.len = as.numeric(path.len)
  path.df = data.frame(rfiles,path.len)
  path.df = path.df %>% arrange(rfiles) %>% arrange(path.len) 
  path.df$rfiles = as.character(path.df$rfiles)
  return (path.df$rfiles)
}

files15 <- getfiles('e:/pyr/data/y2015/2015x/')

df_fee = read_feather(files15[13])

df_veri = read_feather(files15[43])

df_fee = cbind(df_fee,df_veri)

df_fee = tbl_df(df_fee[,c(1,2,4)])

df_fee_seld = df_fee %>% filter(x262 == "False")

load('e:/pyr/data/procdata/hos_dic.rdata')

df_fee_seld$hosname = hos_dic[df_fee_seld$x5]

df_fee_seld =  df_fee_seld %>% select(x5,hosname,x229)

names(df_fee_seld) = c('id','name','ttlfee')

df_fee_seld$ttlfee = as.numeric(df_fee_seld$ttlfee) 

df_fee_seld = df_fee_seld %>% filter(!is.na(name) & !is.na(ttlfee))

df_fee_seld %>% filter(is.na(ttlfee))

df_fee_seld %>% filter(is.na(name))

df_fee_seld = lapply(df_fee_seld,as.character) 

df_fee_seld = tbl_df(df_fee_seld)

df_fee_seld = data.frame(df_fee_seld)

write_feather(df_fee_seld,'./data/procdata/ttlfee_2015.pyr')

str(df_fee_seld)

read_feather('./data/procdata/ttlfee_2015.pyr')

save(df_fee_seld,file='./data/procdata/ttlfee_2015.rdata')

sum_ttl = df_fee_seld %>% group_by(name) %>% 
            summarise(n=n(),ttlfee_by_id = mean(ttlfee)) %>%
            arrange(ttlfee_by_id)

fina_rep = read_csv('e:/pyr/data/procdata/fee_inp.csv',col_names = FALSE,
                    col_types = NULL,
                    locale(encoding = 'gbk'))

fina_rep %>% select(X1,X12,X13,X30,X31) %>% filter(X1 == '河南省人民医院')

fina_rep %>% select(X1,X12,X13,X30,X31) %>% filter(X1 == '郑州大学第一附属医院')

fina_rep %>% select(X1,X12,X13,X30,X31) %>% filter(X1 == '河南省胸科医院')


fina_rep_inpfee15 = fina_rep %>% select(X1,X12,X13,X30,X31) 

fina_rep_inpfee15 = fina_rep_inpfee15[4:nrow(fina_rep_inpfee15),]

fina_rep_inpfee15[,2:5] <- lapply(fina_rep_inpfee15[,2:5] ,as.character)

fina_rep_inpfee15[,2:5] <- lapply(fina_rep_inpfee15[,2:5] ,as.numeric)

str(fina_rep_inpfee15)



df_fee_seld %>% filter(is.na(ttlfee))

df_fee_seld %>% filter(is.na(name))

hosdic %>% filter(X2 == '410000000186')








levels(as.factor(df_fee_seld$x5))




rdatafiles = dir('/mnt/e/R/rdoc/',full.names = TRUE)

which(substr(rdatafiles,17,length(rdatafiles)) %in% 'hosnames.csv' )

hosdic = read_csv(rdatafiles[1],col_names =FALSE,col_types =NULL,locale(encoding = 'gbk'))

hosdic = hosdic[2:nrow(hosdic),2:3] 

hos_dic = hosdic$X3
names(hos_dic) = hosdic$X2

write_feather(hosdic,'./data/procdata/hos_dic.pyr')




################################io part==================================
h5createFile("e:/pry/data/fee.h5")







