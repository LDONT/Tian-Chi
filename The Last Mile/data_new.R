setwd("E:/workspace/最后一公里")
library(readr)
library(readr)
site <- read_csv("new_1.csv",col_names = F)
spot <- read_csv("new_2.csv",col_names = F)
shop <- read_csv("new_3.csv",col_names = F)
order_shop <- read_csv("new_4.csv",col_names = F)
order_o2o <- read_csv("new_5.csv",col_names = F)
courier_id <- read_csv("new_6.csv",col_names = F)
site_yuan <- read_csv("1 (2).csv")
spot_yuan <- read_csv("2 (2).csv")
shop_yuan <- read_csv("3 (2).csv")
order_shop_yuan <- read_csv("4 (2).csv")
order_o2o_yuan <- read_csv("5 (2).csv")
courier_id_yuan <- read_csv("6 (2).csv") 
colnames(site) <- colnames(site_yuan)
colnames(spot) <- colnames(spot_yuan)
colnames(shop) <- colnames(shop_yuan)
colnames(order_shop) <- colnames(order_shop_yuan)
colnames(order_o2o) <- colnames(order_o2o_yuan)
colnames(courier_id) <- colnames(courier_id_yuan)
# a <- site$id[!site$id %in% order_shop$Site_id] 
# b <- spot$id[!spot$id %in% order_shop$Spot_id] 
# c <- spot$id[!spot$id %in% order_o2o$Spot_id]
# e <- unique(order_o2o$Spot_id)  
# d <- shop$id[!shop$Shop_id %in% order_o2o$Shop_id] 9214个spot，613个shop，124个site。电商一对一，o2o只用1138个spot
order_1 <- merge(order_shop,site,by.x = "Site_id",by.y = 'id')
order_1 <- merge(order_1,spot,by.x ="Spot_id",by.y = 'id')
order_2 <- merge(order_o2o,shop,by.x = "Shop_id",by.y = 'id')
order_2 <- merge(order_2,spot,by.x = "Spot_id",by.y = 'id')
order_2$new_Pickup_time <- as.numeric(difftime(strptime(order_2$Pickup_time,"%H:%M"),strptime("8:00","%H:%M"),units = "mins"))
order_2$new_Delivery_time <- as.numeric(difftime(strptime(order_2$Delivery_time,"%H:%M"),strptime("8:00","%H:%M"),units = "mins"))
# write.table(order_1,file = "电商订单.csv",row.names = F,quote = F,sep = ",")
# write.table(order_2,file = "o2o订单.csv",row.names = F,quote = F,sep = ",")
#计算任意两点的时间
location_all <- rbind(site,spot,shop)
run_time <- function(M,N){
  if(substr(M,1,1)=='E'){
    M <- substr(M,6,20)
  }
  if(substr(N,1,1)=='E'){
    N <- substr(N,6,20)
  }
  if(substr(M,1,1)=='H'){
    substr(M,1,1) <- 'B'
  }
  if(substr(N,1,1)=='H'){
    substr(N,1,1) <- 'B'
  }
  time <- 2*6378137*asin(sqrt((sin(pi/180*(location_all$Lat[location_all$id==M]-location_all$Lat[location_all$id==N])/2))^2+cos(pi/180*location_all$Lat[location_all$id==M])*cos(pi/180*location_all$Lat[location_all$id==N])*(sin(pi/180*(location_all$Lng[location_all$id==M]-location_all$Lng[location_all$id==N])/2))^2 ))/(15*1000/60)
  return(time)
}
#O2O和site配对
location_1 <- site
colnames(location_1) <- c('Site_id','Site_id_Lng','Site_id_Lat')
find_nearest <- function(Shop_id_Lng,Shop_id_Lat,Spot_id_Lng,Spot_id_Lat){
  site_id <- data.frame(Site_id=location_1$Site_id) 
  site_id$dis <- 2*6378137*asin(sqrt((sin(pi/180*(location_1$Site_id_Lat-Spot_id_Lat)/2))^2+cos(pi/180*location_1$Site_id_Lat)*cos(pi/180*Spot_id_Lat)*(sin(pi/180*(location_1$Site_id_Lng-Spot_id_Lng)/2))^2 ))+  2*6378137*asin(sqrt((sin(pi/180*(location_1$Site_id_Lat-Shop_id_Lat)/2))^2+cos(pi/180*location_1$Site_id_Lat)*cos(pi/180*Shop_id_Lat)*(sin(pi/180*(location_1$Site_id_Lng-Shop_id_Lng)/2))^2 ))
  site_id <- site_id[order(site_id$dis),]
  # print(head(site_id))
  min_site_id <- as.character(site_id$Site_id[1]) 
  # print(min_site_id)
  return (min_site_id)
}
o2o_site <- mapply(find_nearest, order_2$Lng.x,order_2$Lat.x,order_2$Lng.y,order_2$Lat.y)
o2o_site <- data.frame(o2o_Order_id=order_2$Order_id,Site_id=o2o_site)
order_2 <- merge(order_2,o2o_site,by.x = "Order_id",by.y = 'o2o_Order_id')
# write.table(order_2,file = "o2o订单.csv",row.names = F,quote = F,sep = ",")

site_all <- sort(unique(site$id))
shop_all <- sort(unique(shop$id))
A_group <- function (A0){
  A <- order_1$Spot_id[order_1$Site_id==A0]
  return(c(A0,A))
}
#得到disAB文件
AB_group <- apply(t(as.array(site_all)),  2, A_group)   #得到仓库和配送点之间的配对
# save(AB_group,file = 'AB_group.rData')
# time <- function (A){
#   t  <- 2*6378137*asin(sqrt((sin(pi/180*(location_all$Lat[location_all$id==B[j]]-location_all$Lat[location_all$id==A])/2))^2+
#                               cos(pi/180*location_all$Lat[location_all$id==B[j]])*cos(pi/180*location_all$Lat[location_all$id==A])*
#                               (sin(pi/180*(location_all$Lng[location_all$id==B[j]]-location_all$Lng[location_all$id==A])/2))^2 ))/(15*1000/60)
#   return(t)  
# } 
# time_all <- list()
# for(i in 1:124){
#   B <- as.array(AB_group[[i]])
#   time_all_i <- B
#   for(j in 1:length(B)){
#     
#     T <- apply(as.array(B), 1 , time)
#     time_all_i <- data.frame(time_all_i,T)
#   }
#   row.names(time_all_i) <- time_all_i[,1]
#   time_all_i <- time_all_i[,-1]
#   names(time_all_i) <- B
#   time_all <- c(time_all,list(time_all_i))
#   #print(i)
# }
# for(k in 1:124){
#   #   tmp <- c(1:ncol(time_all[[k]]))
#   #   colnames(time_all[[k]]) <- tmp
#   #   rownames(time_all[[k]]) <- tmp
#   write.table(time_all[[k]],file = paste("A",k,"_dis_AB2",".csv",sep = ""),quote = F,row.names = F,col.names = F,sep = ",")
# }
# 
# for(i in 1:124){
#   Num <- vector()
#   B <- as.array(AB_group[[i]])
#   for(j in 1:length(B)){
#     if(j==1){
#       Num[j] <- 0
#     }
#     else{
#       Num[j] <- order_1$Num[order_1$Site_id==B[1]&order_1$Spot_id==B[j]]
#     }
#     
#   }
#   num_all <- data.frame(B,Num)
#   write.table(num_all,file = paste("A",i,"_num_AB2",".csv",sep = ""),quote = F,row.names = F,col.names = F,sep = ",")
# }
# 
# solution <- data.frame()
# for (i in 1:124) {
#   solution_i <- read_csv(paste("./new_best_route/","A",i,"_best_route2.csv",sep = ""))
#   solution <- rbind(solution,solution_i)
# }
# write.table(solution,file = paste("./new_best_route/","solution_all.csv",sep = ""),quote = F,row.names = F,sep = ",",col.names = F)