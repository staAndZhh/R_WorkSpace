library(data.table)
library(tidyverse)
#----read da----
base_path <- 'F:\\SpiderPy3\\page0904\\aroundbind\\'
all_paths <- map_chr(list.files(base_path),function(x){return (str_c(base_path,x))})
test <- read.csv(all_paths[1])
names(test)
mode(test)

all_changdu<- all_paths %>% map(function(x){return(names(fread(x)))}) %>% reduce(function(x,y){return(intersect(x,y))})
test %>% dplyr::select(setdiff(all_changdu,c('V1')))
all_files <- all_paths %>% map_df(function(x){return(read.csv(x,colClasses = c(gridcode = 'character'))%>%dplyr::select(setdiff(all_changdu,c('V1'))) )}) %>% as.data.table()
data <- all_files
fwrite(data,'C:\\Users\\hasee\\Desktop\\youyiku\\youyiku.csv')
#----- before deal----
str(data)
data[,.N,filename]
data[,':='(shop_id = sapply(filename,get_shop_id)),]
data <- as.data.table(data)
get_shop_id <- function(a){
  str_split(a,'_')[[1]][1]
}
data[,.N,.(shop_id)]
str(data)
get_shop_id('440_22')
data[,.N,pname]
# write.csv(data[,.N,name],'C:\\Users\\hasee\\Desktop\\youyiku\\stock_name.csv')
str(data)
data[,,]
pp <- read.csv('C:\\Users\\hasee\\Desktop\\youyiku\\stock_name.csv')
pp <- as.data.table(pp)[,.(name,name_used),]
str(data)
data_n <- data[,.(cityname,pname,citycode,type,typecode,filename,shop_id,business_area,name),]
data_n <- data_n%>%left_join(pp,by=c('name')) %>% as.data.table()
data_n[name_used=='优衣库',,]

city_rank <- fread('C:\\Users\\hasee\\Desktop\\youyiku\\城市级别匹配表.txt')
setnames(city_rank,names(city_rank),c('provi_z','cityname','city_rank_z','xingzheng_z'))
pp_new <- city_rank[,.(cityname,city_rank_z),] 
PP_new <- pp_new[!duplicated(pp_new)]

setdiff(as.vector(unique(data_n$cityname)),as.vector(unique(PP_new$cityname)))
data_n <- data_n %>% left_join(pp_new,by=c('cityname')) %>% as.data.table()


data_youyiku <- fread('F:\\SpiderPy3\\page0904\\re\\uniqlo_around.csv',encoding = 'UTF-8')
data_youyiku <- data_youyiku %>% left_join(pp_new,by=c('cityname')) %>% as.data.table()
data_youyiku <- data_youyiku[!duplicated(data_youyiku)]
#---- data_youyiku----
data_youyiku[,.N,pname]
data_youyiku[pname=='上海市'&name=='优衣库(塘桥巴黎春天店)',,]
data_youyiku[,.N,city_rank_z]
data_youyiku[city_rank_z=='五线城市',,]
write.csv(data_youyiku[,.N,city_rank_z]
          
  ,'clipboard')
#---- data_circle----
data_n[,.N,type][order(-N)]
fwrite(data_n[type=='购物服务;商场;购物中心',.N,name_used][order(-N)],
       'C:\\Users\\hasee\\Desktop\\youyiku\\zhengti_tem.csv')
temaa <- data_n[city_rank_z %in% c('一线城市','二线城市'),.N,type][order(-N)]
tema <- data_n[city_rank_z %in% c('一线城市','二线城市')&type=='购物服务;购物相关场所;购物相关场所',.N,name_used][order(-N)]
temb <-data_n[city_rank_z %in% c('一线城市','二线城市')&type=='购物服务;便民商店/便利店;便民商店/便利店',.N,name_used][order(-N)]
temc <-data_n[city_rank_z %in% c('一线城市','二线城市')&type=='购物服务;服装鞋帽皮具店;品牌服装店',.N,name_used][order(-N)]
temd <-data_n[city_rank_z %in% c('一线城市','二线城市')&type=='购物服务;商场;购物中心',.N,name_used][order(-N)]
fwrite(temaa,
       'C:\\Users\\hasee\\Desktop\\youyiku\\12_aa.csv')
fwrite(tema,
       'C:\\Users\\hasee\\Desktop\\youyiku\\12_a.csv')
fwrite(temb,
       'C:\\Users\\hasee\\Desktop\\youyiku\\12_b.csv')
fwrite(temc,
       'C:\\Users\\hasee\\Desktop\\youyiku\\12_c.csv')
fwrite(temd,
       'C:\\Users\\hasee\\Desktop\\youyiku\\12_d.csv')
temaa <- data_n[city_rank_z %in% c('三线城市','四线城市'),.N,type][order(-N)]
tema <- data_n[city_rank_z %in% c('三线城市','四线城市')&type=='购物服务;购物相关场所;购物相关场所',.N,name_used][order(-N)]
temb <-data_n[city_rank_z %in% c('三线城市','四线城市')&type=='购物服务;便民商店/便利店;便民商店/便利店',.N,name_used][order(-N)]
temc <-data_n[city_rank_z %in% c('三线城市','四线城市')&type=='购物服务;服装鞋帽皮具店;品牌服装店',.N,name_used][order(-N)]
temd <-data_n[city_rank_z %in% c('三线城市','四线城市')&type=='购物服务;商场;购物中心',.N,name_used][order(-N)]
fwrite(temaa,
       'C:\\Users\\hasee\\Desktop\\youyiku\\34_aa.csv')
fwrite(tema,
       'C:\\Users\\hasee\\Desktop\\youyiku\\34_a.csv')
fwrite(temb,
       'C:\\Users\\hasee\\Desktop\\youyiku\\34_b.csv')
fwrite(temc,
       'C:\\Users\\hasee\\Desktop\\youyiku\\34_c.csv')
fwrite(temd,
       'C:\\Users\\hasee\\Desktop\\youyiku\\34_d.csv')

temaa <- data_n[city_rank_z %in% c('新一线城市'),.N,type][order(-N)]
tema <- data_n[city_rank_z %in% c('新一线城市')&type=='购物服务;购物相关场所;购物相关场所',.N,name_used][order(-N)]
temb <-data_n[city_rank_z %in% c('新一线城市')&type=='购物服务;便民商店/便利店;便民商店/便利店',.N,name_used][order(-N)]
temc <-data_n[city_rank_z %in% c('新一线城市')&type=='购物服务;服装鞋帽皮具店;品牌服装店',.N,name_used][order(-N)]
temd <-data_n[city_rank_z %in% c('新一线城市')&type=='购物服务;商场;购物中心',.N,name_used][order(-N)]
fwrite(temaa,
       'C:\\Users\\hasee\\Desktop\\youyiku\\new1_aa.csv')
fwrite(tema,
       'C:\\Users\\hasee\\Desktop\\youyiku\\new1_a.csv')
fwrite(temb,
       'C:\\Users\\hasee\\Desktop\\youyiku\\new1_b.csv')
fwrite(temc,
       'C:\\Users\\hasee\\Desktop\\youyiku\\new1_c.csv')
fwrite(temd,
       'C:\\Users\\hasee\\Desktop\\youyiku\\new1_d.csv')

