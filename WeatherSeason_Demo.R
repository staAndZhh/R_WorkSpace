library(tidyverse)
library(data.table)
library(RMySQL)
library(TTR)
library(lubridate)

# before deal----
con <- dbConnect(MySQL(),host="127.0.0.1",dbname="smnews",user="root",password="") 
dbSendQuery(con,'SET NAMES gbk')
history_tem <- as.data.table(dbReadTable(con,'history_tem'))
str(history_tem)
history_tem[,":="(ave_tm = as.numeric(ave_tm),max_tm = as.numeric(max_tm),min_tm = as.numeric(min_tm)),]
history_tem[,":="(provi=NULL,area=NULL,s_area=NULL,area_name = NULL),]
history_tem[,.N,year]
history_tem_copy <- copy(history_tem)

history_tem <- history_tem_copy[!duplicated(history_tem_copy),,]
ppb_city <- fread('C:\\Users\\hasee\\Desktop\\ppb.txt',encoding = 'UTF-8')

history_tem <- history_tem %>% left_join(ppb_city,by=c('city')) %>% as.data.table()
history_tem[is.na(area),,]
history_tem[,.N,year]
# data----
day_provi_normal_season <- history_tem[(ymd>=20150101)&(ymd<=20151231)&(!provi%in%c("台湾","香港","澳门")),.(ave_tm  = mean(ave_tm,na.rm = TRUE)),.(ymd,provi,area)][order(area,provi,ymd)]
day_provi_normal_season <- day_provi_normal_season[provi!="",,]
day_provi_normal_season[,":="(md = str_sub(ymd,5,8),month = str_sub(ymd,5,6)),]
day_provi_normal_season <- day_provi_normal_season[md!='0229',,]
day_provi_normal_season <- day_provi_normal_season[provi!='?????',,] 



day_provi_normal_season[provi=='?????',,]
day_provi_normal_season[area=='????????',,]
day_provi_normal_season[,.N,provi]
# day_provi_normal_season[md=='0229',.N,md]
# tem_var <- day_provi_normal_season[,.N,ave_tm]
# tem_var[,':='(cut_ave = cut(ave_tm,3)),]
# tem_var[,.N,cut_ave]
# rm(tem_var)

mean_day_provi_normal_season <- day_provi_normal_season[,.(ave_tm = mean(ave_tm,na.rm = TRUE)),.(md,month,provi,area)][order(area,provi,md,month)]
setcolorder(mean_day_provi_normal_season,c('area','provi','md','month','ave_tm'))
mean_day_provi_normal_season <- mean_day_provi_normal_season[,,][order(area,provi,md,month)]
mean_day_provi_normal_season <- mean_day_provi_normal_season[,":="(sma_five = (SMA(ave_tm,n=5))),.(area,provi)]  # average five 

nzb_pp <- fread("C:\\Users\\hasee\\Desktop\\新的天气报表\\匹配表.txt")
data_pp <-  data.table(md=sort(unique(mean_day_provi_normal_season$md)),x_lab = c(1:365))

mean_day_provi_normal_season <- mean_day_provi_normal_season %>% left_join(data_pp,by=c("md")) %>% as.data.table()
mean_day_provi_normal_season <- mean_day_provi_normal_season %>% left_join(nzb_pp,by=c('provi')) %>% as.data.table() 


mean_day_provi_normal_season[,":="(spring_sep = (if_else(sma_five >=10&sma_five<22,1,0,0)),
                                   summer_sep = (if_else(sma_five >=22,1,0,0)),
                                   autumn_sep = (if_else(sma_five <22&sma_five>=10,1,0,0)),
                                   winter_sep = (if_else(sma_five <10,1,0,0))
),.(area,provi)]  # count season

mean_day_provi_normal_season[,":="(spring_sep_sma_count = SMA(spring_sep,n=5)*5,
                                   summer_sep_sma_count = SMA(summer_sep,n=5)*5,
                                   auttumn_sep_sma_count = SMA(autumn_sep,n=5)*5,
                                   winter_sep_sma_count = SMA(winter_sep,n=5)*5 
),.(area,provi)]


# not four season----
all_spring_autter <- mean_day_provi_normal_season[spring_sep_sma_count==5,.N,provi][order(provi)]
all_summer_autter <- mean_day_provi_normal_season[summer_sep_sma_count==5,.N,provi][order(provi)]
all_winter_autter <- mean_day_provi_normal_season[winter_sep_sma_count==5,.N,provi][order(provi)]
base_not_four <- mean_day_provi_normal_season[,.N,provi]

setnames(all_spring_autter,names(all_spring_autter),c('provi','spring'))
setnames(all_summer_autter,names(all_summer_autter),c('provi','summer'))
setnames(all_winter_autter,names(all_winter_autter),c('provi','winter'))
base_filter_weather_four <- base_not_four %>% left_join(all_spring_autter,by=c('provi')) %>% left_join(all_summer_autter,by=c('provi'))%>% left_join(all_winter_autter,by=c('provi')) %>% as.data.table()
rm(all_spring_autter,all_summer_autter,all_winter_autter,base_not_four,data_pp,nzb_pp)

base_filter_weather_four[!complete.cases(base_filter_weather_four)]  # not four season
base_filter_weather_four[complete.cases(base_filter_weather_four)]

all_winter_area <- base_filter_weather_four[N==winter]%>% unlist() %>% as.vector()
all_summer_area <- base_filter_weather_four[N==summer]%>% unlist() %>% as.vector()
all_spring_area <- base_filter_weather_four[N==spring]%>% unlist() %>% as.vector()

all_season_area <- purrr::reduce(list(all_winter_area,all_summer_area,all_spring_area),function(x,y){dplyr::union(x,y)})

no_summer_area <- base_filter_weather_four[!complete.cases(base_filter_weather_four)][is.na(summer),,][,.(provi),] %>% unlist() %>% as.vector()
no_winter_area <- base_filter_weather_four[!complete.cases(base_filter_weather_four)][is.na(winter),,][,.(provi),] %>% unlist() %>% as.vector()

not_four_area <- reduce(list(all_winter_area,all_summer_area,all_spring_area,no_summer_area,no_winter_area),function(x,y){return(dplyr::union(x,y))})
rm(all_winter_area,all_summer_area,all_spring_area)
four_area <- setdiff(base_filter_weather_four$provi,not_four_area)
#-----four season day------
four_mean_day_provi_normal_season <- mean_day_provi_normal_season[provi%in%four_area,,]
spring_day <- four_mean_day_provi_normal_season[spring_sep_sma_count==5,.(spring_day= min(md)),.(area,provi)]

four_mean_day_provi_normal_season[,":="(max_tem= max(ave_tm)),.(area,provi)]
max_tem_day <- four_mean_day_provi_normal_season[ave_tm == max_tem,.(max_day = md),.(area,provi)]
four_mean_day_provi_normal_season <- 
  four_mean_day_provi_normal_season %>% left_join(spring_day[,.(provi,spring_day),],by=c('provi'))  %>% left_join(max_tem_day[,.(provi,max_day),],by=c('provi')) %>% as.data.table()
summer_day <- four_mean_day_provi_normal_season[md>=spring_day&md<=max_day&summer_sep_sma_count==5,.(summer_day= min(md)),.(area,provi)]

four_mean_day_provi_normal_season <- 
  four_mean_day_provi_normal_season %>% left_join(summer_day[,.(provi,summer_day),],by=c('provi')) %>% as.data.table()
autter_day <- four_mean_day_provi_normal_season[md>=summer_day&md>=max_day&auttumn_sep_sma_count==5,.(autter_day= min(md)),.(area,provi)]

four_mean_day_provi_normal_season <- 
  four_mean_day_provi_normal_season %>% left_join(autter_day[,.(provi,autter_day),],by=c('provi')) %>% as.data.table()
winter_day <- four_mean_day_provi_normal_season[md>=autter_day&md>=max_day&winter_sep_sma_count==5,.(winter_day= min(md)),.(area,provi)]


four_season_day_standrad <- purrr::reduce(list(spring_day,summer_day,autter_day,winter_day),function(x,y){return(left_join(x,y,by=c('area','provi')))}) %>% as.data.table()
four_season_day_standrad[,":="(area=NULL),]
rm(spring_day,summer_day,autter_day,winter_day)

#---- not four season----
not_four_mean_day_provi_normal_season <- mean_day_provi_normal_season[provi%in%not_four_area,,]

not_four_area <- setdiff(not_four_area,all_season_area)
not_four_mean_day_provi_normal_season_no_winter <- not_four_mean_day_provi_normal_season[provi%in%no_winter_area,,]
not_four_mean_day_provi_normal_season_no_summer <- not_four_mean_day_provi_normal_season[provi%in%no_summer_area,,]

not_four_season_day_standrad <- data.table(provi =all_season_area,spring_day=rep(('0000'),length(all_season_area)),
                                           spring_day=rep(('0000'),length(all_season_area)),
                                           summer_day=rep(('0000'),length(all_season_area)),
                                           autter_day=rep(('0000'),length(all_season_area)),
                                           winter_day=rep(('0000'),length(all_season_area)))

if (length(no_winter_area)>0){
  # only summer and autter
  not_four_mean_day_provi_normal_season_no_winter[,":="(max_tem= max(ave_tm)),.(area,provi)]
  max_tem_day <- not_four_mean_day_provi_normal_season_no_winter[ave_tm == max_tem,.(max_day = md),.(area,provi)]
  not_four_mean_day_provi_normal_season_no_winter <- not_four_mean_day_provi_normal_season_no_winter  %>% left_join(max_tem_day[,.(provi,max_day),],by=c('provi')) %>% as.data.table()
  summer_day <- not_four_mean_day_provi_normal_season_no_winter[md<=max_tem&summer_sep_sma_count==5,.(summer_day= min(md)),.(area,provi)]
  
  not_four_mean_day_provi_normal_season_no_winter <- 
    not_four_mean_day_provi_normal_season_no_winter %>% left_join(summer_day[,.(provi,summer_day),],by=c('provi')) %>% as.data.table()
  autter_day <- not_four_mean_day_provi_normal_season_no_winter[md>=summer_day&md>=max_day&auttumn_sep_sma_count==5,.(autter_day= min(md)),.(area,provi)]
  
  not_four_season_day_standrad_no_winter <- purrr::reduce(list(summer_day,autter_day),function(x,y){return(left_join(x,y,by=c('area','provi')))}) %>% as.data.table()
  not_four_season_day_standrad_no_winter[,":="(spring_day='0101',winter_day='1231'),]
  setcolorder(not_four_season_day_standrad_no_winter,c('area','provi','spring_day','summer_day','autter_day','winter_day'))
  not_four_season_day_standrad_no_winter[,":="(area=NULL),]
  rm(summer_day,autter_day)
  }
if (length(no_summer_area)>0){
  not_four_mean_day_provi_normal_season_no_summer[,":="(max_tem= max(ave_tm)),.(area,provi)]
  
  spring_day <- not_four_mean_day_provi_normal_season_no_summer[spring_sep_sma_count==5,.(spring_day= min(md)),.(area,provi)]
  
  not_four_mean_day_provi_normal_season_no_summer[,":="(max_tem= max(ave_tm)),.(area,provi)]
  max_tem_day <- not_four_mean_day_provi_normal_season_no_summer[ave_tm == max_tem,.(max_day = md),.(area,provi)]
  not_four_mean_day_provi_normal_season_no_summer <- 
    not_four_mean_day_provi_normal_season_no_summer %>% left_join(spring_day[,.(provi,spring_day),],by=c('provi'))  %>% left_join(max_tem_day[,.(provi,max_day),],by=c('provi')) %>% as.data.table()

  winter_day <- not_four_mean_day_provi_normal_season_no_summer[md>=spring_day&md>=max_day&winter_sep_sma_count==5,.(winter_day= min(md)),.(area,provi)]
  autter_day <- max_tem_day 
  autter_day <- autter_day %>% setnames('max_day','autter_day')
  
  not_four_season_day_standrad_no_summer <- purrr::reduce(list(spring_day,autter_day,winter_day),function(x,y){return(left_join(x,y,by=c('area','provi')))}) %>% as.data.table()
  not_four_season_day_standrad_no_summer[,":="(area=NULL,summer_day='0000'),]
  setcolorder(not_four_season_day_standrad_no_summer,c('provi','spring_day','summer_day','autter_day','winter_day'))
  rm(spring_day,summer_day,autter_day,winter_day)
  
}

four_season_day_standrad[,":="(type='four_season'),]
not_four_season_day_standrad_no_summer[,":="(type='no_summer'),]
not_four_season_day_standrad_no_winter[,":="(type='no_winter'),]
all_season_standrad <- bind_rows(four_season_day_standrad,not_four_season_day_standrad_no_summer,not_four_season_day_standrad_no_winter) 
str(not_four_season_day_standrad_no_winter)
mean_day_provi_normal_season
xlsx::write.xlsx(all_season_standrad,'C:\\Users\\hasee\\Desktop\\wea_xiuzheng\\years_seasons_2015.xlsx')


#---- min & max
xlsx::write.xlsx(mean_day_provi_normal_season[!is.na(sma_five),,],'C:\\Users\\hasee\\Desktop\\wea_xiuzheng\\years_seasons_ori_2017.xlsx')
# mean_day_provi_normal_season[!is.na(sma_five),,][,.N,provi]
mean_day_provi_normal_season[!is.na(sma_five),,][,.N,provi]
