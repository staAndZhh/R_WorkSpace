library(tidyverse)
library(data.table)
library(dplyr)

#----------------------funciton-------------------------------
get_all_file_names <- function(base_path){
  '
  得到路径下所有底层文件的路径
  '
  path_save_list <- vector()
  # base_path <- 'F:/0514/情报通数据/数据1'
  file_paths <- str_c(base_path,dir(base_path),sep='/')
  for (i in c(1:length(file_paths))){
    # 如果是文件 入库
    tem_father_path = file_paths[i]
    
    if(file_test('-d',tem_father_path) == FALSE){
      tem_path_file <- str_c(tem_father_path,dir(tem_father_path),sep = '/')
      path_save_list <- c(path_save_list,tem_path_file)
    }else if(file_test('-d',tem_father_path)){
      # 如果是文件夹 继续遍历
      tem_path_file <- get_all_file_names(tem_father_path)
      path_save_list <- c(path_save_list,tem_path_file)
    }
    
  }
  return(path_save_list)
  
}

get_name_stock<- function(x){return (str_split(x,".xls")[[1]][1])}

read_xls_from_file_QBT <- function(tem_all_paths){
  '
  根据一个路径读取文件
  '
  return (readxl::read_xls(stringi::stri_conv(tem_all_paths,"UTF-8","GBK"),sheet = 'QBT') %>% select(-starts_with('环比'),-starts_with('总计'),-starts_with('涨幅'))
          # %>% filter(名称!='总计')
  )
}



#--------------------------------------main-----------------------
base_path <- 'C:\\Users\\hasee\\Desktop\\chao_dalao\\715\\tb\\qushi\\'
all_paths <- get_all_file_names(base_path)
str(all_paths)
file_da <- data.table("all_path" = as.vector(all_paths),"name" = map_chr(basename(all_paths),get_name_stock),
           "stock" =basename(dirname(all_paths)), "sex" = basename(dirname(dirname(all_paths))),"type" = basename(dirname(dirname(dirname(all_paths)))))


basename(dirname(dirname(dirname(all_paths[1]))))
#------------------ e boy----------------
type_sel <-'e'
# l
sex_sel <- 'girl'
# girl
stock_sel <- 'clot'
# hangye boylondon burandoeno,npc,viishow,clot

paths_used_all <- file_da[(stock==stock_sel) &(sex==sex_sel)&(type==type_sel),all_path,]
paths_used_type <- file_da[stock==stock_sel &sex==sex_sel&type==type_sel,type,]
paths_used_sex <- file_da[stock==stock_sel &sex==sex_sel&type==type_sel,sex,]
paths_used_sku <- file_da[stock==stock_sel &sex==sex_sel&type==type_sel,stock,]

# i=1
# str(test1)
# read_xls_from_file_QBT(normalizePath(paths_used_all[3])) 
test1 <- read_xls_from_file_QBT(normalizePath(paths_used_all[1])) 
test2 <- read_xls_from_file_QBT(normalizePath(paths_used_all[2])) 
test3 <- read_xls_from_file_QBT(normalizePath(paths_used_all[3])) 

ori_DF <- data.table("名称"=union(union(test1$名称,test2$名称),test3$名称))
Final_da <- as.data.table(left_join(left_join(left_join(ori_DF,test1,by=c("名称")),test2,by=c("名称")),test3,by=c("名称")))
# setcolorder(Final_da,c("2017-03销售额"," 2017-04销售额"," 2017-05销售额"," 2017-06销售额"," 2017-07销售额"," 2017-08销售额",
#                        "2017-09销售额","2017-10销售额","2017-11销售额","2017-12销售额","2018-01销售额","2018-02销售额",
#                        "2018-03销售额","2018-04销售额","2018-05销售额","2018-06销售额","2018-07销售额"))
Final_da <- Final_da %>% add_column(type =paths_used_type[1],sex = paths_used_sex[1],sku = paths_used_sku[1])
write.csv(Final_da,'clipboard')
# test1 <- read_xls_from_file_QBT(normalizePath(paths_used_all[2])) 
# # read_xls_from_file_QBT(normalizePath(paths_used_all[1])) 
# # read_xls_from_file_QBT(normalizePath(paths_used_all[2]))
# # read_xls_from_file_QBT(normalizePath(paths_used_all[3])) 
# # %>% add_column(type =paths_used_type[i],sex = paths_used_sex[i],sku = paths_used_sku[i] )
# for (i in c(2:length(paths_used_all))){
#   # test_tem <- read_xls_from_file_QBT(normalizePath(paths_used_all[i])) %>% add_column(type =paths_used_type[i],sex = paths_used_sex[i],sku = paths_used_sku[i])
#   test_tem <- read_xls_from_file_QBT(normalizePath(paths_used_all[i]))
#   # if(ncol(test1) >=ncol(test_tem)){
#   #   test1 <- left_join(test1,test_tem,by=c("名称"))
#   # }else{
#   #   test1 <- right_join(test1,test_tem,by=c("名称"))
#   # }
#   test1 <- left_join(test1,test_tem,by=c("名称"))
#   
 
}
test1 <-  test1%>% add_column(type =paths_used_type[i],sex = paths_used_sex[i],sku = paths_used_sku[i] )

write.csv(test1,'clipboard')

#------------------ e girl----------------
type_sel <-'e'
sex_sel <- 'girl'

paths_used_all <- file_da[sheet_type==type_sel &sex==sex_sel,all_path,]
paths_used_type <- file_da[sheet_type==type_sel &sex==sex_sel,sheet_type,]
paths_used_sex <- file_da[sheet_type==type_sel &sex==sex_sel,sex,]
paths_used_sku <- file_da[sheet_type==type_sel &sex==sex_sel,name,]

i=1
test2 <- read_xls_from_file_QBT(normalizePath(paths_used_all[i])) %>% add_column(type =paths_used_type[i],sex = paths_used_sex[i],sku = paths_used_sku[i] )
for (i in c(2:length(paths_used_all))){
  test_tem <- read_xls_from_file_QBT(normalizePath(paths_used_all[i])) %>% add_column(type =paths_used_type[i],sex = paths_used_sex[i],sku = paths_used_sku[i] )
  test2 <- bind_rows(test2,test_tem)
}

#------------------ l boy----------------
type_sel <-'l'
sex_sel <- 'boy'

paths_used_all <- file_da[sheet_type==type_sel &sex==sex_sel,all_path,]
paths_used_type <- file_da[sheet_type==type_sel &sex==sex_sel,sheet_type,]
paths_used_sex <- file_da[sheet_type==type_sel &sex==sex_sel,sex,]
paths_used_sku <- file_da[sheet_type==type_sel &sex==sex_sel,name,]

i=1
test3 <- read_xls_from_file_QBT(normalizePath(paths_used_all[i])) %>% add_column(type =paths_used_type[i],sex = paths_used_sex[i],sku = paths_used_sku[i] )
for (i in c(2:length(paths_used_all))){
  test_tem <- read_xls_from_file_QBT(normalizePath(paths_used_all[i])) %>% add_column(type =paths_used_type[i],sex = paths_used_sex[i],sku = paths_used_sku[i] )
  test3 <- bind_rows(test3,test_tem)
}
#------------------ l girl----------------
type_sel <-'l'
sex_sel <- 'girl'

paths_used_all <- file_da[sheet_type==type_sel &sex==sex_sel,all_path,]
paths_used_type <- file_da[sheet_type==type_sel &sex==sex_sel,sheet_type,]
paths_used_sex <- file_da[sheet_type==type_sel &sex==sex_sel,sex,]
paths_used_sku <- file_da[sheet_type==type_sel &sex==sex_sel,name,]

i=1
test4 <- read_xls_from_file_QBT(normalizePath(paths_used_all[i])) %>% add_column(type =paths_used_type[i],sex = paths_used_sex[i],sku = paths_used_sku[i] )
for (i in c(2:length(paths_used_all))){
  test_tem <- read_xls_from_file_QBT(normalizePath(paths_used_all[i])) %>% add_column(type =paths_used_type[i],sex = paths_used_sex[i],sku = paths_used_sku[i] )
  test4 <- bind_rows(test4,test_tem)
}
#-----------------main---------------------
final_da1 <- bind_rows(bind_rows(bind_rows(test1,test2),test3),test4) %>% as.data.table()
final_da1 <- bind_rows(test1,test2) %>% as.data.table()
final_da2 <- bind_rows(test3,test4) %>% as.data.table()
write.csv(final_da2,"C:\\Users\\hasee\\Desktop\\da\\final_da2.csv")
write.csv(final_da1,"C:\\Users\\hasee\\Desktop\\da\\final_da1.csv")

deal_fengge <- function(sheet_index= 1,vect_col = c(1,5)){
  sheet_table1 <- readxl::read_excel("C:\\Users\\hasee\\Desktop\\chao_dalao\\情报通数据\\ori_da\\e\\boy_T\\boyT.xlsx",sheet =sheet_index,skip=1) %>% select(vect_col) %>%as.data.table()
  setnames(sheet_table1,names(sheet_table1)[2],str_c(colnames(sheet_table1)[2],"_",sheet_index))
  return(sheet_table1)
}
deal_two_table <- function(x,y){return(as.data.table(left_join(x,y,by=c("X__1"))))}
tabtest <-reduce(map(c(1:17),deal_fengge),deal_two_table)

write.csv(tabtest,'clipboard')
