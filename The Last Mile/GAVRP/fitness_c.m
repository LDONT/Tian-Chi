path=children(i,l1:l2-1);
l=length(path);
%num_file(path,2);
sum(num_file(path,1));
for k=1:l
    if k==1
        fitness_value_c(i)=fitness_value_c(i)+3*sqrt(num_file(path(1),1))+5+dis_file(1,path(1)+1);
        %fitness_value_c(i)=fitness_value_c(i)+sum(data(path,3))*dis(1,path(1))*3;   % k=1时表示从仓库出发
    else
        fitness_value_c(i)=fitness_value_c(i)+3*sqrt(num_file(path(k),1))+5+dis_file(path(k-1)+1,path(k)+1);
        %fitness_value_c(i)=fitness_value_c(i)+sum(data(path(k:l),3))*dis(path(k-1),path(k))*3;
    end
     
         
end 
fitness_value_c(i)=fitness_value_c(i)+dis_file(1,path(l)+1);  
%fitness_value_c(i)=fitness_value_c(i)+dis(path(l),1)*2;