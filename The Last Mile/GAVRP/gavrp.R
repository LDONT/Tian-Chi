# num_all <- list()
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
#   num_all_i <- data.frame(B,Num)
#   num_all <- c(num_all,list(num_all_i))
# 
# }
# save(num_all,file = "num_AB.RData")
setwd("E:/workspace/最后一公里")
jiaocha <- function(){
  P1 <- as.numeric(parent[i,]) 
  P2 <- as.numeric(parent[j,]) 
  x <- floor(runif(1,min = 0,max = 1)*(num_spot_id-2))+2
  y <- floor(runif(1,min = 0,max = 1)*(num_spot_id-2))+2
  if(x<y){
    change1 <- P1[x:y]
    change2 <- P2[x:y]
    p1 <- c(P1[(y+1):length(P1)],P1[1:(x-1)],change1)
    p2 <- c(P2[(y+1):length(P2)],P2[1:(x-1)],change2)
    p1 <- p1[!p1%in%change2]
    p2 <- p2[!p2%in%change1]
    P1 <- c(p1[(num_spot_id-y+1):length(p1)],change2,p1[1:(num_spot_id-y)])
    P2 <- c(p2[(num_spot_id-y+1):length(p2)],change1,p2[1:(num_spot_id-y)])
  }
  else{
    change1 <- P1[y:x]
    change2 <- P2[y:x]
    p1 <- c(P1[(x+1):length(P1)],P1[1:(y-1)],change1)
    p2 <- c(P2[(x+1):length(P2)],P2[1:(y-1)],change2)
    p1 <- p1[!p1%in%change2]
    p2 <- p2[!p2%in%change1]
    P1 <- c(p1[(num_spot_id-x+1):length(p1)],change2,p1[1:(num_spot_id-x)])
    P2 <- c(p2[(num_spot_id-x+1):length(p2)],change1,p2[1:(num_spot_id-x)])
  }
  print(i)
  print(j)
  children <- rbind(children,P1,P2)
  return(children)
  
  
}

best_route4 <- list()
best_fit4 <- vector()
load("dis_AB.RData")
load("num_AB.RData")

for(k in 1:1){

  dis_file <- time_all[[k]]
  num_file <- num_all[[k]]
  num_file <- num_file[-1,]
  n <- nrow(dis_file)
  num_spot_id <- n-1
  sup_parent <- c(1:num_spot_id)
  G <- 100
  parent <- data.frame()
  for(i in 1:G){
    parent <- rbind(parent,as.vector(sample(1:num_spot_id,num_spot_id,replace = FALSE)))
  }
  Pc=0.8   #交叉比率
  Pm=0.2   #变异比率
  species <- rbind(sup_parent,parent)
  children <- data.frame()
  print("初始化完成")
  
  # g <- floor(num_spot_id*1.7) #更新迭代次数
  g <- 10
  for(generation in 1:g){
    parent <- species
    children <- data.frame()
    n <- nrow(parent)
    for(i in 1:n){
      for(j in 1:n){
        if(i!=j&runif(1,min = 0,max = 1)<Pc){
          children <- jiaocha()
        }
          
      }
    }
    print("交叉完成")
    for(i in 1:n){
      if(runif(1,min = 0,max = 1)<Pm){
        parent_i <- as.numeric(parent[i,]) 
        x <- floor(runif(1,min = 0,max = 1)*num_spot_id)+1
        y <- floor(runif(1,min = 0,max = 1)*num_spot_id)+1
        z <- parent_i[x]
        parent_i[x] <- parent_i[y]
        parent_i[y] <- z
        children <- rbind(children,parent_i)
      }
    }
    print("变异完成")
    
    m <- nrow(children)
    n <- ncol(children)
    fitness_value_c <- vector()
    for(i in 1:m){
      l1=1
      fitness_value_c_i <- 0
      for(l2 in 1:n){
        if(sum(num_file[as.numeric(children[i,l1:l2]),2])>140){
          path <- as.numeric(children[i,l1:(l2-1)]) 
          l <- length(path)
          for(k in 1:l){
            if(k==1){
              fitness_value_c_i <- fitness_value_c_i+3*sqrt(num_file[path[1],2])+5+dis_file[1,path[1]+1]
            }
            else{
              fitness_value_c_i <- fitness_value_c_i+3*sqrt(num_file[path[k],2])+5+dis_file[path[k-1]+1,path[k]+1]
            }
          }
          fitness_value_c_i <- fitness_value_c_i+dis_file[1,path[l]+1]
          l1 <- l2
        }
        if(l2==n){
          l2 <- l2+1
          path <- as.numeric(children[i,l1:(l2-1)])
          l <- length(path)
          for(k in 1:l){
            if(k==1){
              fitness_value_c_i <- fitness_value_c_i+3*sqrt(num_file[path[1],2])+5+dis_file[1,path[1]+1]
            }
            else{
              fitness_value_c_i <- fitness_value_c_i+3*sqrt(num_file[path[k],2])+5+dis_file[path[k-1]+1,path[k]+1]
            }
          }
          fitness_value_c_i <- fitness_value_c_i+dis_file[1,path[l]+1]
          
        }
      }
      fitness_value_c <- c(fitness_value_c,fitness_value_c_i)
    }
    print("子代适应值")
    
    m <- nrow(parent)
    n <- ncol(parent)
    fitness_value_p <- vector()
    for(i in 1:m){
      l1=1
      fitness_value_p_i <- 0
      for(l2 in 1:n){
        if(sum(num_file[as.numeric(parent[i,l1:l2]),2] )>140){
          path <- as.numeric(parent[i,l1:(l2-1)]) 
          l <- length(path)
          for(k in 1:l){
            if(k==1){
              fitness_value_p_i <- fitness_value_p_i+3*sqrt(num_file[path[1],2])+5+dis_file[1,path[1]+1]
            }
            else{
              fitness_value_p_i <- fitness_value_p_i+3*sqrt(num_file[path[k],2])+5+dis_file[path[k-1]+1,path[k]+1]
            }
          }
          fitness_value_p_i <- fitness_value_p_i+dis_file[1,path[l]+1]
          l1 <- l2
        }
        if(l2==n){
          l2 <- l2+1
          path <- as.numeric(parent[i,l1:(l2-1)])
          l <- length(path)
          for(k in 1:l){
            if(k==1){
              fitness_value_p_i <- fitness_value_p_i+3*sqrt(num_file[path[1],2])+5+dis_file[1,path[1]+1]
            }
            else{
              fitness_value_p_i <- fitness_value_p_i+3*sqrt(num_file[path[k],2])+5+dis_file[path[k-1]+1,path[k]+1]
            }
          }
          fitness_value_p_i <- fitness_value_p_i+dis_file[1,path[l]+1]
          
        }
      }
      fitness_value_p <- c(fitness_value_p,fitness_value_p_i)
    }
    print("父代适应值")
    
    n <- sort(fitness_value_c,index.return=TRUE)$ix
    children <- children[n[1:G],]
    fitness_value_c <- fitness_value_c[n[1:G]]
    n <- sort(fitness_value_p,index.return=TRUE)$ix
    parent <- parent[n[1:G],]
    fitness_value_p <- fitness_value_p[n[1:G]]
    colnames(children) <- colnames(parent)
    species <- rbind(children,parent)
    fitness_value <- c(fitness_value_c,fitness_value_p)
    n <- sort(fitness_value,index.return=TRUE)$ix
    species <- species[n[1:G],]
    fitness_value <- fitness_value[n[1:G]]
  }
  print("迭代完成")
  best_route <- as.numeric(species[1,]) 
  best_fit <- fitness_value[1]
  best_route2 <- 0
  l1 <- 1
  for(l2 in 1:length(best_route)){
    if(sum(num_file[best_route[l1:l2],2])>140&l2<length(best_route)){
      best_route2 <- c(best_route2,best_route[l1:(l2-1)],0)
      l1 = l2
    }
    if(sum(num_file[best_route[l1:l2],2])<=140&l2==length(best_route)){
      best_route2 <- c(best_route2,best_route[l1:l2],0)
    }
    if(sum(num_file[best_route[l1:l2],2])>140&l2==length(best_route)){
      best_route2 <- c(best_route2,best_route[l1:(l2-1)],0,best_route[l2],0)
      l1 = l2
    }
  }
  best_route3 <- best_route2 +1
  best_fit2 <- 0
  for(l in 1:(length(best_route2)-1)){
    best_fit2 <- best_fit2+dis_file[best_route3[l],best_route3[l+1]]
  }
  # print(best_fit2)
#   print(best_route3)
#   print(best_fit)
#   print(k)
#   best_route4 <- c(best_route4,list(best_route3))
#   best_fit4 <- c(best_fit4,best_fit)
  

}







  