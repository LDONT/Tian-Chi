#计算某种组合下的代价
value <- function(seq){
  seq <- seq[-length(seq)]   #在函数中删掉最后一个1，主程序中调用的时候没删
  arrive_time <- vector()
  depart_time <- vector()
  end_time <- vector()
  punish_time <- vector()
  temp_str1 <- vector()  #
  temp_str2 <- vector()
  arrive_time[1] <- 0
  depart_time[1] <- 0
  d <- 0
  load <- 0
  sig <- 0
  for (p in 2:length(seq)) {
    if(substr(seq[p],6,6)=='S') {
      temp_str1 <- c(temp_str1,substr(seq[p],1,5))  #把order id存进向量中
    }
    if(substr(seq[p],6,6)=='H'){
      temp_str2 <- c(temp_str2,substr(seq[p],1,5))#
    }
    if(length(temp_str1)!=0){
      sig <- 0
      for(r in 1:length(temp_str1)){
        if(!temp_str1[r]%in%temp_str2){
          sig <- 1
        }
      }
    }
    if((substr(seq[p],1,1)=='A')&&(sig==0)&&((arrive_time[p-1]>450)|(depart_time[p-1]>450))){
      temp_str1 <- vector() #
      temp_str2 <- vector()
      arrive_time[p] <- 0
      depart_time[p] <- 0
      d <- d+1
      end_time[d] <- depart_time[p-1]
      if(end_time[d]>720){
        punish_time <- c(punish_time,5*(end_time[d]-720))
      }
    } else{
      if(substr(seq[p],1,1)=='E'){
        if(substr(seq[p],6,6)=='S'){
          arrive_time[p] <- depart_time[p-1]+round(run_time(seq[p-1],seq[p]))
          temp <- o2o_i$new_Pickup_time[o2o_i$Order_id==substr(seq[p],1,5)]
          depart_time[p] <- max(temp,arrive_time[p])
          if(temp<arrive_time[p]){
            punish_time <- c(punish_time,5*(arrive_time[p]-temp))
          }
        }
        if(substr(seq[p],6,6)=='H'){
          arrive_time[p] <- depart_time[p-1] +round(run_time(seq[p-1],seq[p]))  
          temp <- o2o_i$new_Delivery_time[o2o_i$Order_id==substr(seq[p],1,5)]
          depart_time[p] <- arrive_time[p]+round(3*sqrt(o2o_i$Num[o2o_i$Order_id==substr(seq[p],1,5)])+5)
          if(temp<arrive_time[p]){
            punish_time <- c(punish_time,5*(arrive_time[p]-temp))
          }
        }
      }
      if(substr(seq[p],1,1)=='A'){
        arrive_time[p] <- depart_time[p-1] +round(run_time(seq[p-1],seq[p])) 
        depart_time[p] <- arrive_time[p]  
        if(720<arrive_time[p]){
          punish_time <- c(punish_time,5*(arrive_time[p]-720))
        }
      }
      if(substr(seq[p],1,1)=='B'){    #电商的spot没有重复，但o2o的spot和shop都有重复
        arrive_time[p] <- depart_time[p-1] +round(run_time(seq[p-1],seq[p])) 
        depart_time[p] <- arrive_time[p]+round(3*sqrt(dianshang_i$Num[dianshang_i$Spot_id==seq[p]])+5)
        if(720<depart_time[p]){
          punish_time <- c(punish_time,5*(depart_time[p]-720))
        }
      }
    }
    
    if(p==length(seq)){
      d <- d+1
      end_time[d] <- depart_time[p]
      # if(end_time[d]>720){
      #   punish_time <- c(punish_time,5*(end_time[d]-720))   #并不是这样
      # }
    }
    # if(substr(seq[p],1,1)=='A'){
    #   load <- 0
    # }
    # if(substr(seq[p],1,1)=='B'){
    #   load <- load+dianshang_i$Num[dianshang_i$Spot_id==seq[p]]
    # }
    # if(substr(seq[p],1,1)=='S'){
    #   load <- load+o2o_i$Num[o2o_i$Order_id==substr(seq[p],1,5)]
    # }
    # if(substr(seq[p],1,1)=='H'){
    #   load <- load-o2o_i$Num[o2o_i$Order_id==substr(seq[p],1,5)]
    # }
    # if(load>140){
    #   punish_time <- c(punish_time,1000000000)
    # }
    
    
  }
  total_time <- sum(end_time)+sum(punish_time)
  return(total_time)
  
}

#得到提交形式的数据
time_route <- function(seq){
  arrive_time <- vector()   
  depart_time <- vector()
  people <- vector()
  id <- vector()
  order <- vector()
  num <- vector()
  temp_str1 <- vector()  #
  temp_str2 <- vector()
  id[1] <- seq[1]
  arrive_time[1] <- 0
  depart_time[1] <- 0
  people[1] <- paste('D',formatC(D, flag = '0', width = 4),sep = '')
  order[1] <- NA
  num[1] <- NA
  sig <- 0
  for(p in 2:length(seq)){
    if(substr(seq[p],6,6)=='S') {
      temp_str1 <- c(temp_str1,substr(seq[p],1,5))  #把order id存进向量中
    }
    if(substr(seq[p],6,6)=='H'){
      temp_str2 <- c(temp_str2,substr(seq[p],1,5))#
    }
    if(length(temp_str1)!=0){
      sig <- 0
      for(r in 1:length(temp_str1)){
        if(!temp_str1[r]%in%temp_str2){
          sig <- 1
        }
      }
    }
    
    if((substr(seq[p],1,1)=='A')&&(sig==0)&&((arrive_time[p-1]>450)|(depart_time[p-1]>450))){
      temp_str1 <- vector() #
      temp_str2 <- vector()
      arrive_time[p] <- 0
      depart_time[p] <- 0
      id[p] <- seq[p]
      order[p] <- NA
      num[p] <- NA
      D <- D+1
      people[p] <- paste('D',formatC(D, flag = '0', width = 4),sep = '') 
    } else{
      if(substr(seq[p],1,1)=='E'){
        if(substr(seq[p],6,6)=='S'){
          arrive_time[p] <- depart_time[p-1]+round(run_time(seq[p-1],seq[p]))
          temp <- o2o_i$new_Pickup_time[o2o_i$Order_id==substr(seq[p],1,5)]
          depart_time[p] <- max(temp,arrive_time[p])
          id[p] <- substr(seq[p],6,9)
          people[p] <- paste('D',formatC(D, flag = '0', width = 4),sep = '')
          order[p] <- o2o_i$Order_id[o2o_i$Order_id==substr(seq[p],1,5)]
          num[p] <- o2o_i$Num[o2o_i$Order_id==substr(seq[p],1,5)]
        }
        if(substr(seq[p],6,6)=='H'){
          arrive_time[p] <- depart_time[p-1] +round(run_time(seq[p-1],seq[p]))  
          temp <- o2o_i$new_Delivery_time[o2o_i$Order_id==substr(seq[p],1,5)]
          depart_time[p] <- arrive_time[p]+round(3*sqrt(o2o_i$Num[o2o_i$Order_id==substr(seq[p],1,5)])+5)
          id[p] <- substr(seq[p],6,10)
          #substr(id[p],1,1) <- 'B' 暂时还不能变
          people[p] <- paste('D',formatC(D, flag = '0', width = 4),sep = '')
          order[p] <- o2o_i$Order_id[o2o_i$Order_id==substr(seq[p],1,5)]
          num[p] <- -o2o_i$Num[o2o_i$Order_id==substr(seq[p],1,5)]
        }
      }
      if(substr(seq[p],1,1)=='A'){
        arrive_time[p] <- depart_time[p-1] +round(run_time(seq[p-1],seq[p])) 
        depart_time[p] <- arrive_time[p]
        id[p] <- seq[p]
        people[p] <- paste('D',formatC(D, flag = '0', width = 4),sep = '')
        order[p] <- NA
        num[p] <- NA
      }
      if(substr(seq[p],1,1)=='B'){    #电商的spot没有重复，但o2o的spot和shop都有重复
        arrive_time[p] <- depart_time[p-1] +round(run_time(seq[p-1],seq[p])) 
        depart_time[p] <- arrive_time[p]+round(3*sqrt(dianshang_i$Num[dianshang_i$Spot_id==seq[p]])+5)
        id[p] <- seq[p]
        people[p] <- paste('D',formatC(D, flag = '0', width = 4),sep = '')
        order[p] <- dianshang_i$Order_id[dianshang_i$Spot_id==seq[p]]
        num[p] <- -dianshang_i$Num[dianshang_i$Spot_id==seq[p]]
      }
    }
    if(p==length(seq)){
      D <- D+1
    }
  }
  
  # a_numbf <- which(substr(seq,1,1)=='A')  #得到所有A点的位置
  # a_numbf <- c(a_numbf,length(seq))
  # temp_Af <- 1
  # load <- vector()
  # for(x in 1:length(seq)){
  #   if(substr(seq[x],6,6)=='S'){
  #     #load_s <- load_s+o2o_i$Num[o2o_i$Order_id==substr(seq[x],1,5)]
  #     load[x] <- load[x-1]+o2o_i$Num[o2o_i$Order_id==substr(seq[x],1,5)]
  #   }
  #   if(substr(seq[x],6,6)=='H'){
  #     #load_s <- load_s-o2o_i$Num[o2o_i$Order_id==substr(seq[x],1,5)]
  #     load[x] <- load[x-1]-o2o_i$Num[o2o_i$Order_id==substr(seq[x],1,5)]
  #   }
  #   if(substr(seq[x],1,1)=='A'){
  #     temp_Af <- temp_Af+1
  #     for(temp_af in a_numbf[temp_Af-1]:a_numbf[temp_Af]){
  #       if(substr(seq[temp_af],1,1)=='B'){
  #         load[x] <- load[x]+dianshang_i$Num[dianshang_i$Spot_id==seq[temp_af]]
  #       }
  #     }
  #   }
  #   
  #   if(substr(seq[x],1,1)=='B'){
  #     load <- load-dianshang_i$Num[dianshang_i$Spot_id==seq[x]]
  #   }
  # }
  # best_route2 <- data.frame(people=people,id=id,arrival_time=arrive_time,depart_time=depart_time,num=num,order=order,load=load)
  best_route2 <- data.frame(people=people,id=id,arrival_time=arrive_time,depart_time=depart_time,num=num,order=order)
  return(c(list(best_route2),list(D)))
}





D <- 1
for (i in 1:124){
  AB_seq <- read_csv(paste("./new_best_route/","A",i,"_best_route.csv",sep = ""),col_names = "index")
  AB_seq <- data.frame(index=as.integer(AB_seq[-nrow(AB_seq),]) ) 
  AB_time <- read_csv(paste("./new disAB/","A",i,"_dis_AB2.csv",sep = ""),col_names = F)
  AB_num <- read_csv(paste("./new numAB/","A",i,"_num_AB2.csv",sep = ""),col_names = F)
  AB_group_i <- AB_group[[i]]
  AB_group_i <- data.frame(index=(1:length(AB_group_i)),id=AB_group_i)
  for (n in 1:nrow(AB_seq)) {
    AB_seq$id[n] <- as.character(AB_group_i$id[AB_group_i$index==AB_seq$index[n]]) 
  }
  dianshang_i <- subset(order_1,Site_id==AB_seq$id[1])
  o2o_i <- subset(order_2,Site_id==AB_seq$id[1])
  substr(o2o_i$Spot_id,1,1) <- 'H' 
  AB_seq_id_list <- list()
  value_m <- vector()
  for (m in 1:(nrow(AB_seq)*nrow(o2o_i))) {
  #for (m in 1:((nrow(AB_seq)-1)*nrow(AB_seq))) {   #在2*nrow(AB_seq)个随机中取最优解
    AB_seq_id <- AB_seq$id
    
    if(nrow(o2o_i)!=0){
      for(k in 1:nrow(o2o_i)){
        # k1 <- floor(runif(1,min=2,max=(length(AB_seq_id))))
        # AB_seq_id <- c(AB_seq_id[1:k1],paste(o2o_i$Order_id[k],o2o_i$Shop_id[k],sep = ''),AB_seq_id[(k1+1):length(AB_seq_id)])
        # k2 <- floor(runif(1,min=(k1+1),max=(length(AB_seq_id))))
        # AB_seq_id <- c(AB_seq_id[1:k2],paste(o2o_i$Order_id[k],o2o_i$Spot_id[k],sep = ''),AB_seq_id[(k2+1):length(AB_seq_id)])
        
        repeat {
          tempt_140 <- 0
          load_s <- 0
          load <- 0
          temp_load <- 0
          temp_A <- 1
          AB_seq_id_k <- AB_seq_id 
          k1 <- floor(runif(1,min=2,max=(length(AB_seq_id))))
          AB_seq_id <- c(AB_seq_id[1:k1],paste(o2o_i$Order_id[k],o2o_i$Shop_id[k],sep = ''),AB_seq_id[(k1+1):length(AB_seq_id)])
          #k2 <- floor(runif(1,min=(k1+1),max=(length(AB_seq_id))))
          k2 <- floor(runif(1,min=(k1+1),max=min((k1+5),(length(AB_seq_id)))))  #缩小H到S的范围
          AB_seq_id <- c(AB_seq_id[1:k2],paste(o2o_i$Order_id[k],o2o_i$Spot_id[k],sep = ''),AB_seq_id[(k2+1):length(AB_seq_id)])
          a_numb <- which(substr(AB_seq_id,1,1)=='A')  #得到所有A点的位置
          a_numb <- c(a_numb,length(AB_seq_id))
          #print(a_numb)
          #print(AB_seq_id)
          for(x in 1:length(AB_seq_id)){
            if(substr(AB_seq_id[x],6,6)=='S'){
              load_s <- load_s+o2o_i$Num[o2o_i$Order_id==substr(AB_seq_id[x],1,5)]
              load <- load+o2o_i$Num[o2o_i$Order_id==substr(AB_seq_id[x],1,5)]
            }
            if(substr(AB_seq_id[x],6,6)=='H'){
              load_s <- load_s-o2o_i$Num[o2o_i$Order_id==substr(AB_seq_id[x],1,5)]
              load <- load-o2o_i$Num[o2o_i$Order_id==substr(AB_seq_id[x],1,5)]
            }
            if(substr(AB_seq_id[x],1,1)=='A'){
              temp_A <- temp_A+1
              for(temp_a in a_numb[temp_A-1]:a_numb[temp_A]){
                if(substr(AB_seq_id[temp_a],1,1)=='B'){
                  load <- load+dianshang_i$Num[dianshang_i$Spot_id==AB_seq_id[temp_a]]
                }
              }
            }

            if(substr(AB_seq_id[x],1,1)=='B'){
              load <- load-dianshang_i$Num[dianshang_i$Spot_id==AB_seq_id[x]]
            }
            if(load>140){
              tempt_140 <- 1
              AB_seq_id <- AB_seq_id_k
              break
            }
          }
          if(tempt_140==0){
            break
          }
        }
      }
      AB_seq_id_list <- c(AB_seq_id_list,list(AB_seq_id))
      value_m <- c(value_m,value(AB_seq_id))  
    }

  }
#在所有的可能中，选择代价最小的  
  if(nrow(o2o_i)!=0){
    num_min <- which.min(value_m) 
    best_route <- AB_seq_id_list[[num_min]]
    best_route <- best_route[-length(best_route)]
  }else {
    best_route <- AB_seq$id
    best_route <- best_route[-length(best_route)]
  }
  
  best_route2 <- time_route(best_route)[[1]]
  D <- time_route(best_route)[[2]]
  
  row1 <- which(is.na(best_route2$order))
  num_row1 <- length(row1)
  block <- list()
  new_best_route2 <- data.frame()
  for (j in 1:(num_row1-1)) {
    block[[j]] <- best_route2[(row1[j]+1):(row1[j+1]-1),]
    block[[j]] <- subset(block[[j]],substr(id,1,1)=='B')
    block[[j]]$id <- best_route2$id[row1[j]]
    block[[j]]$num <- -block[[j]]$num
    block[[j]]$arrival_time <- best_route2$arrival_time[row1[j]]
    block[[j]]$depart_time <- best_route2$depart_time[row1[j]]
    new_best_route2 <- rbind(new_best_route2,block[[j]],best_route2[(row1[j]+1):(row1[j+1]-1),])
  }
  block[[num_row1]] <- best_route2[(row1[num_row1]+1):(nrow(best_route2)),]
  block[[num_row1]] <- subset(block[[num_row1]],substr(id,1,1)=='B')
  block[[num_row1]]$id <- best_route2$id[row1[num_row1]]
  block[[num_row1]]$num <- -block[[num_row1]]$num
  block[[num_row1]]$arrival_time <- best_route2$arrival_time[[row1[num_row1]]]
  block[[num_row1]]$depart_time <- best_route2$depart_time[[row1[num_row1]]]
  new_best_route2 <- rbind(new_best_route2,block[[num_row1]],best_route2[(row1[num_row1]+1):(nrow(best_route2)),])
  new_best_route2$id <- gsub('H','B',new_best_route2$id)
  print(i)
  write.table(new_best_route2,file = paste("./new_best_route/","A",i,"_best_route2.csv",sep = ""),quote = F,row.names = F,sep = ",")
}



