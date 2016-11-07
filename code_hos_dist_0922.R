   prov.smp<-c("河南省人民医院","郑州大学第一附属医院","郑州大学第二附属医院","河南科技大学第一附属医院", "新乡医学院第一附属医院",
            "河南大学第一附属医院","河南科技大学第二附属医院","新乡医学院第三附属医院","郑州大学第五附属医院","河南省胸科医院") 
   urban.smp<-c("濮阳市人民医院","平顶山市第一人民医院","新乡市第一人民医院","漯河市第一人民医院","周口市中心医院",
             "驻马店市中心医院","南阳市中心医院", "商丘市中心医院","信阳市中心医院","焦作市人民医院") 
  #select_county_sample[[1]]
   county.smp<-c("嵩县人民医院", "汝阳县人民医院", "宜阳县人民医院","宝丰县人民医院","叶县人民医院","鲁山县人民医院",
              "获嘉县人民医院","原阳县人民医院","延津县人民医院","南乐县人民医院","舞阳县人民医院","临颍县人民医院",
              "西华县第一人民医院","太康县人民医院","鹿邑县人民医院","西平县人民医院","正阳县人民医院","确山县人民医院",
              "西峡县人民医院","镇平县人民医院","唐河县人民医院","睢县人民医院","永城市人民医院","柘城县人民医院",
              "潢川县人民医院","罗山县人民医院","商城县人民医院", "武陟县人民医院","沁阳市人民医院","修武县人民医院")
   smp.old<-c(prov.smp,urban.smp,county.smp)
   load("./hos_names_15.rdata")
   
   grepnames<-function(names.old,names.new){
      names.rlt<-NULL
      for (i in 1:length(names.old)){
          names.rep<-names.new[grep(names.old[i],names.new)]
          names.rlt<-c(names.rep,names.rlt)
        }
      return(names.rlt)
      }
   smp.names<-grepnames(smp.old,hos_names_15)
   ##year14
   smp.names<-smp.names[-47]
   smp.names<-smp.names[-48]
   save(smp.names,file="./pro1/smp_names.rdata")
   load("./pro1/smp_names.rdata")
   
   
   ##==================================================================================##
   ##function get_vars from "./cledata" 
   get.vars<-function(lsthosnames,year,strvars){
     df.rlts<-data.table()
     for (i in 1:length(lsthosnames)){
        load(paste("./cledata/",lsthosnames[i],"_",year,".rdata",sep=""))
        df.rep<-eval(parse(text=paste("select(hos,",strvars,")",sep="")))
        df.rlts<-rbind(df.rlts,df.rep)	
         }
      return(df.rlts)
         }
  ##function get_fee_list      
  get.fee<-function(hoslst,year){
     feelst<-list()
     for (i in 1:length(hoslst)){
        load(paste("./cledata/",hoslst[i],"_",year,".rdata",sep=""))
        feelst[[i]]<-hos$x201
        feelst[[i]]<-as.character(feelst[[i]]) 
        feelst[[i]]<-as.numeric(feelst[[i]]) 
        feelst[[i]]<-feelst[[i]][feelst[[i]]!="0"] ##del "0"
        feelst[[i]]<-feelst[[i]][!is.na(feelst[[i]])] ##del "NA"
        quar.01<-quantile(feelst[[i]],0.01)
        quar.99<-quantile(feelst[[i]],0.99)
        feelst[[i]]<-feelst[[i]][feelst[[i]]>quar.01 & feelst[[i]]<quar.99] ##winsor .01 & .99
        }
     return(feelst)
        } 
   ##year 2015
   
   smp.hos.fee.15<-get.fee(smp.names,"y15")
   names(smp.hos.fee.15)<-smp.names
   str(smp.hos.fee.15)
   rlt.wil.prov.15<-wilcox.com(smp.hos.fee.15[41:50],alt="less")
   rlt.wil.urban.15<-wilcox.com(smp.hos.fee.15[31:40],alt="less")
   rlt.wil.county.15<-wilcox.com(smp.hos.fee.15[1:30],alt="less")
   smp.names
   prov.wil.score.15<-fun.wilcox.score(rlt.wil.prov.15)
   urban.wil.score.15<-fun.wilcox.score(rlt.wil.urban.15)
   county.wil.score.15<-fun.wilcox.score(rlt.wil.county.15)
   score.15<-rbind(prov.wil.score.15,urban.wil.score.15,county.wil.score.15)
   
   ##year 2014
   
   smp.hos.fee.14<-get.fee(smp.names,"y14")
   str(smp.hos.fee.14)
   names(smp.hos.fee.14)<-smp.names
   str(smp.hos.fee.14)
   rlt.wil.prov.14<-wilcox.com(smp.hos.fee.14[41:50],alt="less")
   rlt.wil.urban.14<-wilcox.com(smp.hos.fee.14[31:40],alt="less")
   rlt.wil.county.14<-wilcox.com(smp.hos.fee.14[1:30],alt="less")
   smp.names
   prov.wil.score.14<-fun.wilcox.score(rlt.wil.prov.14)
   urban.wil.score.14<-fun.wilcox.score(rlt.wil.urban.14)
   county.wil.score.14<-fun.wilcox.score(rlt.wil.county.14)
   score.14<-rbind(prov.wil.score.14,urban.wil.score.14,county.wil.score.14)
   
   
   
   
   wil.score<-merge(score.14,score.15,by="hos",sort=FALSE)
   names(wil.score)<-c("hos","socre14","score15")
   save(wil.score,file="./pro1/wil_score.rdata")
   load("./pro1/var_list.rdata")
   var.list
   ## pharm y14
   smp.pharm.vars<-get.vars(smp.names,"y14","x2,x3,x201,x218,x219,x220")
   save(smp.pharm.vars,file="./pro1/smp_pharm_vars.rdata")
   load("./pro1/smp_pharm_vars.rdata")
   outcols=c("x201","x218","x219","x220")
   smp.pharm.vars[,c(outcols):=lapply(.SD,as.character),
                  .SDcols=c("x201","x218","x219","x220")]
   smp.pharm.vars[,c(outcols):=lapply(.SD,as.numeric),
                  .SDcols=c("x201","x218","x219","x220")]
   smp.pharm.vars<-smp.pharm.vars[x201!="0"]
   smp.pharm.vars<-smp.pharm.vars[!is.na(x201)]
   smp.pharm.vars[is.na(x218),x218:=0]
   smp.pharm.vars[is.na(x220),x220:=0]  ##if is.na(x220),then x220<-0
   
   smp.pharm.vars[,.(sum.fee=sum(x201),w.phm=sum(x218),c.sum=(x220)),by=x2]
   smp.pharm.vars[,.(sum(x201),sum(x218),sum(x220)),on="x2"]
   smp.pharm.ratio<-smp.pharm.vars[,.(sum(x201),sum(x218),sum(x220)),by=x2][,.(x2,(V2+V3)/V1)]
   smp.pharm.ratio$hos<-smp.names
   
   pharm.score.county<-smp.pharm.ratio[1:30,] %>% arrange(-V2) 
   pharm.score.urban<-smp.pharm.ratio[31:40,] %>% arrange(-V2) 
   pharm.score.prov<-smp.pharm.ratio[41:50,] %>% arrange(-V2) 
   pharm.score.county$pscore<-c(29:0)
   pharm.score.urban$pscore<-c(9:0)
   pharm.score.prov$pscore<-c(9:0)
   pharm.score<-rbind(pharm.score.prov,pharm.score.urban,pharm.score.county)
   wil.pharm.score<-merge(wil.score,pharm.score,by="hos",sort=FALSE)
   wil.pharm.score.14<-wil.pharm.score %>% select(hos,socre14,score15,pscore,V2)
   save(wil.pharm.score.14,file="./pro1/wil_pharm_score_14.rdata")
   
   ## pharm y15
   smp.pharm.vars<-get.vars(smp.names,"y15","x2,x3,x201,x218,x219,x220")
   #save(smp.pharm.vars,file="./pro1/smp_pharm_vars.rdata")
   #load("./pro1/smp_pharm_vars.rdata")
   outcols=c("x201","x218","x219","x220")
   smp.pharm.vars[,c(outcols):=lapply(.SD,as.character),
                  .SDcols=c("x201","x218","x219","x220")]
   smp.pharm.vars[,c(outcols):=lapply(.SD,as.numeric),
                  .SDcols=c("x201","x218","x219","x220")]
   smp.pharm.vars<-smp.pharm.vars[x201!="0"]
   smp.pharm.vars<-smp.pharm.vars[!is.na(x201)]
   smp.pharm.vars[is.na(x218),x218:=0]
   smp.pharm.vars[is.na(x220),x220:=0]  ##if is.na(x220),then x220<-0
   
   smp.pharm.vars[,.(sum.fee=sum(x201),w.phm=sum(x218),c.sum=(x220)),by=x2]
   smp.pharm.vars[,.(sum(x201),sum(x218),sum(x220)),on="x2"]
   smp.pharm.ratio<-smp.pharm.vars[,.(sum(x201),sum(x218),sum(x220)),by=x2][,.(x2,(V2+V3)/V1)]
   smp.pharm.ratio$hos<-smp.names
   
   pharm.score.county<-smp.pharm.ratio[1:30,] %>% arrange(-V2) 
   pharm.score.urban<-smp.pharm.ratio[31:40,] %>% arrange(-V2) 
   pharm.score.prov<-smp.pharm.ratio[41:50,] %>% arrange(-V2) 
   pharm.score.county$pscore<-c(29:0)
   pharm.score.urban$pscore<-c(9:0)
   pharm.score.prov$pscore<-c(9:0)
   pharm.score<-rbind(pharm.score.prov,pharm.score.urban,pharm.score.county)
   wil.pharm.score<-merge(wil.score,pharm.score,by="hos",sort=FALSE)
   wil.pharm.score.15<-wil.pharm.score %>% select(hos,socre14,score15,pscore,V2)
   save(wil.pharm.score.15,file="./pro1/wil_pharm_score_15.rdata")
   load("./pro1/wil_pharm_score_15.rdata")
   load("./pro1/wil_pharm_score_14.rdata")
   wil.pharm.score.15
   wil.pharm.score.14$hos<-str_conv(wil.pharm.score.14$hos,"utf-8")
   wil.pharm.score.14
   wil.pharm.score.15$hos<-str_conv(wil.pharm.score.15$hos,"utf-8")
   save(wil.pharm.score.15,file="wil_pharm_score_15_gbk.rdata")
   save(wil.pharm.score.14,file="wil_pharm_score_14_gbk.rdata")
   ##debug start...
   load("./pro1/hos_names_15.rdata")
   load("./pro1/hos_names_14.rdata")
   hos.code.15<-names(hos_names_15)
   hos.code.14<-names(hos_names_14)
   hos.name.15<-str_conv(hos_names_15,"utf-8")
   hos.name.14<-str_conv(hos_names_14,"utf-8")
   names(hos.name.15)<-hos.code.15
   names(hos.name.14)<-hos.code.14
   save(hos.name.14,file="./pro1/hos_name_14_gbk.rdata")
   save(hos.name.15,file="./pro1/hos_name_15_gbk.rdata")
##=========================And ....========================##
   load("./pro1/hos_name_14_gbk.rdata")
   which(hos.name.14 %in% "平顶山市鲁山县人民医院")
   load(paste("./sec14/",hos.name.14[45],".rdata",sep=""))
   tbl<-tstrsplit(tbl,split=",",fixed=TRUE)
   hos<-data.frame(tbl)
   #str(hos)
   colnames(hos)<-paste("x",c(1:ncol(hos)),sep="")
   hos<-filter(hos,x3==names(hos.name.14)[45]) ##2015 x3
   hos<-tbl_df(hos)
   dt<-data.table(hos)
   #dt<-get_dt(dt)
   outcols<-paste("x",c(210:239),sep="") 
   i<-2
   incols<-paste("x",c((i+210):(i+239)),sep="") 
   dt[x210=="",c(outcols):=.SD,.SDcols=incols]
   dt[x210] 
   dt[x210=="" & x211==""] 
   debug(get_dt)
   
   bcle<-hos %>% select(x210:x241) 
   bcle %>% filter(!is.na(x262))
   bcle<-bcle %>% filter(x210=="") 
   
   
   
   load("./cledata/平顶山市鲁山县人民医院_y14.rdata")
   hos1<-hos %>% select(x201:x230)   
   hos1<-lapply(hos1,as.character)
   hos1<-lapply(hos1,as.numeric)
   
   hos1 %>% filter(x201=="") 
   
   m.hos1<-melt(hos1,id.vars="x201")
   m.hos1<-melt(hos1)
   m.hos2<-m.hos1 %>% group_by(L1) %>% summarise(sum=sum(value))
   floor(m.hos2$sum)
   sum(m.hos2$sum[3:30])-m.hos2$sum[1]-m.hos2$sum[13]##-sum(m.hos2$sum[3:6])
   sum(m.hos2$sum[4:6])

   
##====================next part,coming....==================================
   fina.pharm.hos.14<-read.csv("pharm_hos_14.csv")
   fina.pharm.hos.15<-read.csv("pharm_hos_15.csv")
   fina.pharm.hos<-merge(fina.pharm.hos.14,fina.pharm.hos.15,by=c("单位名称"),sort=FALSE)
   fina.pharm.hos<-fina.pharm.hos %>% select(-单位代码.y)
   names(fina.pharm.hos)<-c("hosname","code","pharm_fee14","pharm_med14","mat_fee14",
                                 "pharm_fee15","pharm_med15","mat_fee15")
   fina.pharm.hos
   
    get.abbr.hosname<-function(strhosnames){
        strhosnames<-as.character(strhosnames)
        ##去掉“省”
        remove_pr<-function(x) (substr(x,4,nchar(x)))
        prov<-strhosnames[grep("(河南省\\w+市)|(河南省\\w+县)",strhosnames)]
        strhosnames[grep("(河南省\\w+市)|(河南省\\w+县)",strhosnames)]<-remove_pr(prov)
        ##去掉"市"（2字）
        remove_u2<-function(x) (substr(x,4,nchar(x)))
        urban2<-strhosnames[grep("(\\b\\w{2}市\\w+县)|(\\b\\w{2}市\\w+市)|(\\b\\w{2}市\\w+区)"
                                       ,strhosnames)]
        strhosnames[grep("(\\b\\w{2}市\\w+县)|(\\b\\w{2}市\\w+市)|(\\b\\w{2}市\\w+区)",
                               strhosnames)]<-remove_u2(urban2)
        ##去掉"市"（3字）
        remove_u3<-function(x) (substr(x,5,nchar(x)))
        urban3<-strhosnames[grep("(\\b\\w{3}市\\w+县)|(\\b\\w{3}市\\w+市)|(\\b\\w{3}市\\w+区)",
                                       strhosnames)]
        strhosnames[grep("(\\b\\w{3}市\\w+县)|(\\b\\w{3}市\\w+市)|(\\b\\w{3}市\\w+区)",
                               strhosnames)]<-remove_u3(urban3)
        ##去掉两个字的“市”，如“南阳”
        remove_u<-function(x) (substr(x,3,nchar(x)))
        u<-strhosnames[grep("(\\b\\w{2}[^市县]\\w+县)|(\\b\\w{2}[^市县]\\w+市)",strhosnames)]
        strhosnames[grep("(\\b\\w{2}[^市县]\\w+县)|(\\b\\w{2}[^市县]\\w+市)",strhosnames)]<-remove_u(u)
        return(strhosnames)         
                 }   
   fina.pharm.hos$hos.abbr<-get.abbr.hosname(fina.pharm.hos$hosname)
   
     
   save(fina.pharm.hos,file="./pro1/fina_pharm_hos_gbk.rdata")
   load("./pro1/fina_pharm_hos_gbk.rdata")
##==========================new part,going....==========================##
   load("wil_pharm_score_14_gbk.rdata")
   load("wil_pharm_score_15_gbk.rdata")
   ##y15
   names(wil.pharm.score.15)
   wil.pharm.score.15<-wil.pharm.score.15 %>% select(hos,score15,pscore,V2)
   names(wil.pharm.score.15)<-c("hos","score15","pharm_score15","pharm_fee15")
   ##y14
   names(wil.pharm.score.14)
   wil.pharm.score.14<-wil.pharm.score.14 %>% select(hos,socre14,pscore,V2)
   names(wil.pharm.score.14)<-c("hos","score14","pharm_score14","pharm_fee14")
   ##merge y14 y15
   wil.pharm.score<-merge(wil.pharm.score.14,wil.pharm.score.15,sort=FALSE)
   
   ##merge by "hos.abbr"
   wil.pharm.score$hos.abbr<-get.abbr.hosname(wil.pharm.score$hos)
   wil.pharm.score$hos.abbr[18]<-"信阳市第一人民医院"
   wil.pharm.score$hos.abbr[20]<-"商丘市第一人民医院"
   wil.pharm.score$hos.abbr[24]<-"西华县人民医院"
   save(wil.pharm.score,file="wil_pharm_score_gbk.rdata")
   load("wil_pharm_score_gbk.rdata")
   names(wil.pharm.score)[2:7]<-paste("wil_", names(wil.pharm.score)[2:7],sep="")
   
   fina.wil.pharm<-merge(wil.pharm.score,fina.pharm.hos,by="hos.abbr",sort=FALSE)
   names(fina.wil.pharm)
   
   fina.wil.pharm<-fina.wil.pharm %>% select(hos,hos.abbr,code,wil_score14:wil_pharm_fee15,
                               pharm_fee14:mat_fee15)
   fina.wil.pharm.ratio<-fina.wil.pharm %>% select(hos,wil_score14,wil_pharm_fee14,pharm_fee14,
                                       wil_score15,wil_pharm_fee15,pharm_fee15)
   save(fina.wil.pharm,file="./pro1/fina_wil_pharm.rdata")
   save(fina.wil.pharm.ratio,file="./pro1/fina_wil_pharm_ratio.rdata")
##smp.names  updating...
   load("./pro1/hos_name_15_gbk.rdata")
   load("./pro1/hos_name_14_gbk.rdata")
   hos.name.14
   load("./pro1/fina_wil_pharm_ratio.rdata")
   cor.test(fina.wil.pharm.ratio$wil_score14,fina.wil.pharm.ratio$wil_score15)
   load("./pro1/smp_names.rdata")
   smp.codes<-names(smp.names)
   smp.names<-str_conv(smp.names,"utf-8")
   names(smp.names)<-smp.codes
   
   smp.names[5]<-hos.name.14["410000003184"] ##光山县人民医院
   names(smp.names)[5]<-"410000003184"
   smp.names[44]<-hos.name.14["410000000990"] ##河南大学淮河医院
   names(smp.names)[44]<-"410000000990"
   save(smp.names,file="./pro1/smp_names_gbk.rdata")
   load("./pro1/smp_names_gbk.rdata")
##get vars from new smaple   
   var.list<-read.csv("./pro1/var_list_gbk.csv")
   vars<-paste("x",c(1:3,201:230),sep="")
   vars<-paste(vars,collapse = ",")
   smp.fee.14<-get.vars(smp.names,"y14",vars)
   save(smp.fee.14,file="./pro1/smp_fee_14.rdata")
   smp.fee.14<-smp.fee.14[!is.na(x201)]
   smp.fee.14[x201==""] %>% group_by(x3) %>% summarise(n=n())
   
   
   
   
   