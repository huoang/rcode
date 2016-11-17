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



df_fee_seld$hosname = hos_dic[df_fee_seld$x5]

df_fee_seld =  df_fee_seld %>% select(x5,hosname,x229)

names(df_fee_seld) = c('id','name','ttlfee')

df_fee_seld$ttlfee = as.numeric(df_fee_seld$ttlfee) 

df_fee_seld = df_fee_seld %>% filter(!is.na(name) & !is.na(ttlfee))

df_fee_seld %>% filter(is.na(ttlfee))

df_fee_seld %>% filter(is.na(name))

sum_ttl = df_fee_seld %>% group_by(name) %>% 
  summarise(n=n(),ttlfee_by_id = mean(ttlfee)) %>%
  arrange(ttlfee_by_id)

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


hdf_fee <- H5Fcreate("./data/hdf5/df_fee_15.h5")
h5write(hos_dic,fhdf5,"hos_dic")
h5ls(fhdf5)
rm(hos_dic)
rm(hosdic)
class(hos_dic)
hos_dic<-data.frame(names(hos_dic),hos_dic)
hos_dic<-tbl_df(hos_dic)
h5writeDataset.data.frame(hos_dic, fhdf5, 'hosdic', 
            level=5, DataFrameAsCompound = TRUE)
hosdic <- h5read(fhdf5,"hosdic",DataFrameAsCompound = TRUE)
class(hosdic)


H5Fclose(fid)
?h5createFile

#############################load part==============================

load('e:/pyr/data/procdata/hos_dic.rdata')

read_feather('./data/procdata/ttlfee_2015.pyr')


