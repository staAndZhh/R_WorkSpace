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
day_provi_normal_season <- history_tem[(ymd>=20150101)&(ymd<=20171231)&(!provi%in%c("台湾","香港","澳门")),.(ave_tm  = mean(ave_tm,na.rm = TRUE)),.(ymd,provi,area)][order(area,provi,ymd)]
day_provi_normal_season <- day_provi_normal_season[provi!="",,]
day_provi_normal_season[,":="(md = str_sub(ymd,5,8),month = str_sub(ymd,5,6)),]
day_provi_normal_season <- day_provi_normal_season[md!='0229',,]
day_provi_normal_season <- day_provi_normal_season[provi!='?????',,] 



day_provi_normal_season[provi=='?????',,]
day_provi_normal_season[area=='????????',,]
day_provi_normal_season[,.N,provi]
day_provi_normal_season[md=='0229',.N,md]
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

# test <- cut(c(10,20,30,40),breaks = c(-Inf,10,22,Inf),labels=c('winter','spring-autter','summer'),ordered = TRUE)

mean_day_provi_normal_season[,":="(sma_five_cut = base::cut(sma_five,breaks = c(-Inf,10,22,Inf),labels=c('winter','spring-autter','summer'),ordered = TRUE)),]

mean_day_provi_normal_season[,":="(spring_sep_sma_count = SMA(spring_sep,n=5)*5,
                                   summer_sep_sma_count = SMA(summer_sep,n=5)*5,
                                   auttumn_sep_sma_count = SMA(autumn_sep,n=5)*5,
                                   winter_sep_sma_count = SMA(winter_sep,n=5)*5 
),.(area,provi)]

# test
mean_day_provi_normal_season[1:10]
mean_day_provi_normal_season[spring_sep_sma_count==5,.N,sma_five_cut]
mean_day_provi_normal_season[summer_sep_sma_count==5,.N,sma_five_cut]
mean_day_provi_normal_season[auttumn_sep_sma_count==5,.N,sma_five_cut]
mean_day_provi_normal_season[winter_sep_sma_count==5,.N,sma_five_cut]
#  data des----
mean_day_provi_normal_season_des <- copy(mean_day_provi_normal_season)
mean_day_provi_normal_season_des <- mean_day_provi_normal_season_des[!is.na(sma_five),,]
mean_day_provi_normal_season_des[,.N,provi]
# 全国
ggplot(data = mean_day_provi_normal_season_des[,.(provi,sma_five,x_lab),],aes(x=x_lab,y=sma_five,color=as.factor(provi))) + geom_line()
# 无四季区域
ggplot(data = mean_day_provi_normal_season_des[provi%in%not_four_area,.(provi,sma_five,x_lab),],aes(x=x_lab,y=sma_five,color=as.factor(provi))) + geom_line()
# 无夏区
ggplot(data = mean_day_provi_normal_season_des[provi%in%no_summer_area,.(provi,sma_five,x_lab),],aes(x=x_lab,y=sma_five,color=as.factor(provi))) + geom_line()
# 无冬区
ggplot(data = mean_day_provi_normal_season_des[provi%in%no_winter_area,.(provi,sma_five,x_lab),],aes(x=x_lab,y=sma_five,color=as.factor(provi))) + geom_line()
# 四季分明区域
ggplot(data = mean_day_provi_normal_season_des[provi%in%four_area,.(provi,sma_five,x_lab),],aes(x=x_lab,y=sma_five,color=as.factor(provi))) + geom_line()
ggplot(data = mean_day_provi_normal_season_des[provi%in%c('海南'),.(provi,sma_five,x_lab),],aes(x=x_lab,y=sma_five,color=as.factor(provi))) + geom_line()

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
# base max_tem and filter data not wrong
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
# test data get da
four_season_day_standrad[,":="(spring_month = as.integer(str_sub(spring_day,1,2)),summer_month = as.integer(str_sub(summer_day,1,2)),autter_month = as.integer(str_sub(autter_day,1,2)),winter_month = as.integer(str_sub(winter_day,1,2))),]
four_season_day_standrad[,.(spring_month,summer_month,autter_month,winter_month),]
four_season_day_standrad[,':='(spring_months = as.integer(summer_month - spring_month),summer_months = as.integer(autter_month - summer_month) ,autter_months = as.integer(winter_month - autter_month) ,winter_months = as.integer(12 - winter_month + spring_month)),]
four_season_day_standrad[,.(provi,spring_months,summer_months,autter_months,winter_months),]

# plot
date_plot_reshape <- four_season_day_standrad[,.(provi,spring_months,summer_months,autter_months,winter_months),]
date_plot <- melt(four_season_day_standrad[,.(provi,spring_months,summer_months,autter_months,winter_months),],id.vars = 'provi',variable.name = 'season',value.name = 'month_length')
ggplot(date_plot,mapping = aes(x=factor(provi),y=month_length,fill=season))+geom_bar(stat = 'identity')

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
  spring_day <- not_four_mean_day_provi_normal_season_no_winter[spring_sep_sma_count==5,.(spring_day= min(md)),.(area,provi)]

  not_four_mean_day_provi_normal_season_no_winter <- 
    not_four_mean_day_provi_normal_season_no_winter %>% left_join(spring_day[,.(provi,spring_day),],by=c('provi'))  %>% left_join(max_tem_day[,.(provi,max_day),],by=c('provi')) %>% as.data.table()
  
  
  summer_day <- not_four_mean_day_provi_normal_season_no_winter[md<=max_tem&md>=spring_day&summer_sep_sma_count==5,.(summer_day= min(md)),.(area,provi)]
  
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
  not_four_season_day_standrad_no_summer[,":="(area=NULL,summer_day= autter_day),]
  setcolorder(not_four_season_day_standrad_no_summer,c('provi','spring_day','summer_day','autter_day','winter_day'))
  rm(spring_day,summer_day,autter_day,winter_day)
  
}

four_season_day_standrad[,":="(type='four_season'),]
not_four_season_day_standrad_no_summer[,":="(type='no_summer'),]
not_four_season_day_standrad_no_winter[,":="(type='no_winter'),]
all_season_standrad <- bind_rows(four_season_day_standrad %>% select(names(not_four_season_day_standrad_no_summer)),not_four_season_day_standrad_no_summer,not_four_season_day_standrad_no_winter) 
str(not_four_season_day_standrad_no_winter)
# mean_day_provi_normal_season
xlsx::write.xlsx(all_season_standrad,'C:\\Users\\hasee\\Desktop\\years_seasons_2015-2017.xlsx')
all_season_standrad

#---- min & max
# xlsx::write.xlsx(mean_day_provi_normal_season[!is.na(sma_five),,],'C:\\Users\\hasee\\Desktop\\wea_xiuzheng\\years_seasons_ori_2017.xlsx')
# # mean_day_provi_normal_season[!is.na(sma_five),,][,.N,provi]
# mean_day_provi_normal_season[!is.na(sma_five),,][,.N,provi]

# -----plot and show ----
# test data get da
all_season_standrad[,":="(spring_month = as.integer(str_sub(spring_day,1,2)),summer_month = as.integer(str_sub(summer_day,1,2)),autter_month = as.integer(str_sub(autter_day,1,2)),winter_month = as.integer(str_sub(winter_day,1,2))),]
all_season_standrad[,.(spring_month,summer_month,autter_month,winter_month),]
all_season_standrad[,':='(spring_months = as.integer(summer_month - spring_month),summer_months = as.integer(autter_month - summer_month) ,autter_months = as.integer(winter_month - autter_month) ,winter_months = as.integer(12 - winter_month + spring_month)),]
all_season_standrad[,.(provi,type,spring_months,summer_months,autter_months,winter_months),]

# plot season months percents
date_plot_reshape <- all_season_standrad[,.(provi,type,spring_months,summer_months,autter_months,winter_months),]
date_plot_reshape[type=='no_winter',":="(autter_months = autter_months + winter_months,winter_months=0),]
date_plot_reshape[type=='no_winter',,]
date_plot <- melt(date_plot_reshape[,.(provi,type,spring_months,summer_months,autter_months,winter_months),],id.vars = 1:2,variable.name = 'season',value.name = 'month_length') %>% as.data.table() %>% setorder(type,month_length)
ggplot(date_plot,mapping = aes(x=factor(provi),y=month_length,fill=season))+geom_bar(stat = 'identity')  + facet_grid(type~.)
# plot days
date_plot_day_reshape <- melt(all_season_standrad[,.(provi,type,spring_day,summer_day,autter_day,winter_day),],id.vars = 1:2,value.name = 'md')
data_pp <-  data.table(md=sort(unique(mean_day_provi_normal_season$md)),x_lab = c(1:365))
date_plot_day_reshape <- date_plot_day_reshape %>% left_join(data_pp,by=c('md')) %>% select(c(1:3,5))  %>% as.data.table()

date_plot_day <- dcast(date_plot_day_reshape,provi+type ~ variable,fun.aggregate = sum,value.var = 'x_lab') %>% as.data.table()  %>% setorder(type,provi)
date_plot_day[,':='(spring_days = as.integer(summer_day - spring_day),summer_days = as.integer(autter_day - summer_day) ,autter_days = as.integer(winter_day - autter_day) ,winter_days = as.integer(365 - winter_day + spring_day)),]
date_plot_day[type=='no_winter',":="(autter_days = autter_days + winter_days,winter_days=0),]
date_plot_day[type=='no_winter',,]
date_plot_day[type=='no_summer',,]
date_plot_day_shape <- melt(date_plot_day[,.(provi,type,spring_days,summer_days,autter_days,winter_days),],id=1:2,value.name = 'days',variable.name = 'season')
ggplot(date_plot_day_shape[days!=0,,],mapping = aes(x=factor(provi),y=days,fill=season))+geom_bar(stat = 'identity',position = 'stack') + geom_text(mapping = aes(label = days),size = 5, colour = 'black', vjust = 1.5, hjust = .5, position = position_stack())+ labs(x='省份',y='天数',title='省份四季划分')
ggplot(date_plot_day_shape[days!=0,,],mapping = aes(x=factor(provi),y=days,fill=season))+geom_bar(stat = 'identity',position = 'stack') + geom_text(mapping = aes(label = days),size = 5, colour = 'black', vjust = 0.5, hjust = 1.5, position = position_stack())+ labs(x='省份',y='天数',title='省份四季划分') + coord_flip()

write.csv(date_plot_day_shape,'clipboard')

# ave tem
# deal data 
tema <- mean_day_provi_normal_season[,c(1:6,7:8,13),]
temb <- all_season_standrad[,c(1:6),]
ave_tem_ori_da <- tema %>% left_join(temb[,c(1:2,6),],by=c('provi')) %>% left_join(temb[,c(1,3),],by=c('provi'))%>% left_join(temb[,c(1,4),],by=c('provi'))%>% left_join(temb[,c(1,5),],by=c('provi')) %>% as.data.table()
rm(tema,temb)
ave_tem_ori_da[md>=spring_day&md<summer_day,":="(season='spring'),]
ave_tem_ori_da[md>=summer_day&md<autter_day,":="(season='summer'),]
ave_tem_ori_da[md>=autter_day&md<winter_day,":="(season='autter'),]
ave_tem_ori_da[md>=winter_day|md<spring_day,":="(season='winter'),]
# des
# avetem season four 
ave_tem_ori_da[,.(ave_tm = mean(ave_tm,na.rm = TRUE),min_tem = min(ave_tm,na.rm = TRUE),max_tem = max(ave_tm,na.rm = TRUE)),.(provi,season)]
write.csv(ave_tem_ori_da[,.(ave_tm = mean(ave_tm,na.rm = TRUE),min_tem = min(ave_tm,na.rm = TRUE),max_tem = max(ave_tm,na.rm = TRUE)),.(provi,season)],'clipboard')
# avetem year three
ave_tem_ori_da[,.(ave_tm = mean(ave_tm,na.rm = TRUE),min_tem = min(ave_tm,na.rm = TRUE),max_tem = max(ave_tm,na.rm = TRUE)),.(provi)]
write.csv(
  ave_tem_ori_da[,.(ave_tm = mean(ave_tm,na.rm = TRUE),min_tem = min(ave_tm,na.rm = TRUE),max_tem = max(ave_tm,na.rm = TRUE)),.(provi)],'clipboard')
# avetem year three day
tem_right_pp <- ave_tem_ori_da[,.(ave_tm = mean(ave_tm,na.rm = TRUE),min_tem = min(ave_tm,na.rm = TRUE),max_tem = max(ave_tm,na.rm = TRUE)),.(provi)]

setnames(tem_right_pp,names(tem_right_pp),c('provi','ave_tem_pp','min_tem_pp','max_tem_pp'))
ave_tem_ori_da <- ave_tem_ori_da %>% left_join(tem_right_pp[,1:2,],by=c('provi')) %>% left_join(tem_right_pp[,c(1,3),],by=c('provi')) %>% left_join(tem_right_pp[,c(1,4),],by=c('provi')) %>% as.data.table()
ave_tem_ori_da[ave_tm == min_tem_pp,.(area,provi,md),]
ave_tem_ori_da[ave_tm == max_tem_pp,.(area,provi,md),]
write.csv(ave_tem_ori_da[ave_tm == max_tem_pp,.(area,provi,md),],'clipboard')
# plot
ggplot(data = ave_tem_ori_da,aes(x=x_lab,y=sma_five,color=as.factor(provi))) + geom_line() + facet_grid(.~type)+labs(x='日期',y='5日平滑温度',title='不同类型温度')  + guides(fill = guide_legend(reverse= TRUE))

ggplot(data = ave_tem_ori_da,aes(x=factor(provi),y=sma_five,color=as.factor(provi),fill=factor(type))) + geom_violin()+labs(x='省份',y='5日平滑温度',title='省份温度小提琴图')  + guides(fill = guide_legend(reverse= TRUE)) +theme(axis.text.x=element_text(angle=50,hjust=0.5, vjust=0.5)) +theme(legend.position="none") 

# month ave
ave_tem_ori_da[,.(ave_tm = mean(ave_tm,na.rm = TRUE)),.(provi,month)][,.N,provi]
ggplot(data =ave_tem_ori_da[,.(ave_tm = mean(ave_tm,na.rm = TRUE)),.(provi,month,type)],aes(x=month,y=ave_tm,group = factor(provi),color= factor(provi)))+ geom_line() + facet_grid(.~type)
write.csv(ave_tem_ori_da[,.(ave_tm = mean(ave_tm,na.rm = TRUE)),.(provi,month)],'clipboard')

# lingshou data bind tem ----
# deal
provi_ls_day <- fread('C:\\Users\\hasee\\Desktop\\wea_xiuzheng\\汇报天气特征\\data_ori\\省天产品季.csv',skip=19,encoding='UTF-8')
setnames(provi_ls_day,names(provi_ls_day),c('week','ymd','provi','season','lssz','lsje','lssl'))
pp_right <- fread('C:\\Users\\hasee\\Desktop\\wea_xiuzheng\\汇报天气特征\\data_ori\\pp.txt')
provi_ls_day <- provi_ls_day %>% left_join(pp_right,by=c('season')) %>% as.data.table()

provi_ls_day[,':='(md = str_sub(ymd,5,8),month = str_sub(ymd,5,6)),]
data_pp <-  data.table(md=sort(unique(provi_ls_day$md)),x_lab = c(1:366))
provi_ls_day[md=='0229',':='(md=0228),]

# dealed provi
provi_ls_day_season <- provi_ls_day[,.(lssl = sum(lssl,na.rm = TRUE)),.(provi,md,month,season_chanpin)]
ave_tem_right <- ave_tem_ori_da[,.(area,provi,md,month,ave_tm,season),] %>% setnames('season','season_tem')
provi_ls_day_season <- provi_ls_day_season %>% left_join(ave_tem_right[,.(provi,md,area,ave_tm,season_tem),],by = c("provi", "md")) %>% as.data.table()
provi_ls_day_season[,.(lssl = sum(lssl,na.rm = TRUE)),.(provi,season_tem,season_chanpin)]

provi_season_season_ls<- provi_ls_day_season[!is.na(season_tem),.(lssl = sum(lssl,na.rm = TRUE)),.(provi,season_tem,season_chanpin)]
provi_season_season_ls[,":="(lssl_all = sum(lssl,na.rm=TRUE)),.(provi,season_tem)]
provi_season_season_ls[,":="(lssl_zb = lssl/lssl_all),]
write.csv(provi_season_season_ls,'clipboard')

# plot
provi_ls_day_season_plot <- provi_ls_day_season[!is.na(season_tem),,]
ggplot(data = provi_ls_day_season_plot,aes(x=ave_tm,y=lssl,color= factor(provi)))+ geom_point()+ facet_grid(season_tem~season_chanpin) + guides(fill = guide_legend(reverse= TRUE)) 
ggplot(data = provi_ls_day_season_plot,aes(x=ave_tm,y=lssl,color= factor(provi)))+ geom_point()+ facet_grid(season_chanpin~season_tem) + guides(fill = guide_legend(reverse= TRUE))
ggplot(data = provi_ls_day_season_plot,aes(x=ave_tm,y=lssl,color= factor(season_chanpin)))+ geom_point()+  guides(fill = guide_legend(reverse= TRUE)) + facet_grid(.~season_chanpin)

# analysis model

