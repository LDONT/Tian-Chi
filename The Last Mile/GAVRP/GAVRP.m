%遗传算法 VRP 问题 Matlab实现 。实现电商订单的分配

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear;clc
for A_site=1:124
best_route4 = [];
dis_file=csvread (['A',num2str(A_site),'_dis_AB2.csv']);   %包括原点   site和spot之间的配对
num_file=csvread (['A',num2str(A_site),'_num_AB2.csv'],1,1);   %不包括原点
[m n]=size(dis_file);
num_spot_id=n-1;
Sup_Parent=[1:n-1];
%Sup_Parent=[3 6 7 17 18 5 8 16 19 15 26 25 4 27 29 2 9 14 20 28 30 31 13 22 21 24 11 12 23 10]-1;
G=100;%种群大小
Parent=rand(G,num_spot_id);
%Parent=rand(G,30);%随即父代
for i=1:G
[m n]=sort(Parent(i,:));                     %初始化数据。 对第i行数据排序，返回排序后向量m，和对应的序号n
Parent(i,:)=n;                               %把序号赋给第i行各元素
end                                          %即最后得到100行1-30的随机数组
Pc=0.8;%交叉比率
Pm=0.2;%变异比率
species=[Sup_Parent;Parent];%种群
children=[];%子代
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%g=input('更新时代次数');
h = waitbar(0,'Please wait...');%进度条
for generation=1:floor(num_spot_id*0.5)
%for generation=1:floor(num_spot_id*1.5)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Parent=species;%子代变成父代
children=[];%子代
%选择交叉父代
[n m]=size(Parent);      %返回parent的维数给n和m
% select=rand(1,n)<Pc;
% select=find(select==1);
                                                                                                                %交叉
for i=1:n
    for j=i:n
        if i~=j & rand<Pc             %i不等于j且任一随机数小于PC
            jiaocha
        end
    end
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
for i=1:n
    if rand<Pm
    parent=Parent(i,:);%变异个体
    X=floor(rand*num_spot_id)+1;
    Y=floor(rand*num_spot_id)+1;
    Z=parent(X);
    parent(X)=parent(Y);
    parent(Y)=Z;                                                                                                 %变异
    children=[children;parent];
    end
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%计算子代适应值
[m n]=size(children);
fitness_value_c=zeros(m,1);%子代适应值
for i=1:m
    l1=1;
    for l2=1:n
        if sum(num_file(children(i,l1:l2),1))>135             %135是载重量，保险起见取个比140小的数
            fitness_c
            l1=l2;
        end
        if l2==n
            l2=l2+1;
            fitness_c
        end
                                                                                                                  %计算适应值
        
    
    end
end
%计算父代适应值
[m n]=size(Parent);
fitness_value_P=zeros(m,1);%父代适应值
for i=1:m
    l1=1;
    for l2=1:n
        if sum(num_file(Parent(i,l1:l2),1))>135           % 135是载重量
            fitness_P
            l1=l2;
        end
        if l2==n
            l2=l2+1;
            fitness_P
        end
    end
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%淘汰子代
[m n]=sort(fitness_value_c);
children=children(n(1:G),:);   % G种群大小100，保留前100个
fitness_value_c=fitness_value_c(n(1:G));
%淘汰父代
[m n]=sort(fitness_value_P);
Parent=Parent(n(1:G),:);
fitness_value_P=fitness_value_P(n(1:G));
%淘汰种群
species=[children;Parent];
fitness_value=[fitness_value_c;fitness_value_P];
[m n]=sort(fitness_value);
species=species(n(1:G),:);
fitness_value=fitness_value(n(1:G));                                                                             %更新世代
%waitbar(generation/2,h)
waitbar(generation/floor(num_spot_id*0.5),h)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
end
best_route = species(1,:);%最优线路
best_fit = fitness_value(1);%最优费用
%fitness_value(1:10)
best_route2 = [0];
l1=1;
for l2 = 1:length(best_route)
    if (sum(num_file(best_route(l1:l2)))>135&l2<length(best_route))
        best_route2 = [best_route2,best_route(l1:l2-1),0];
        l1=l2;
    end
    if (sum(num_file(best_route(l1:l2)))<=135&l2==length(best_route))
        best_route2 = [best_route2,best_route(l1:l2),0];
    end
    if (sum(num_file(best_route(l1:l2)))>135&l2==length(best_route))
        best_route2 = [best_route2,best_route(l1:l2-1),0,best_route(l2),0];
        l1=l2;
    end    
end

best_route2;
best_route3 = best_route2+1;
csvwrite(['A',num2str(A_site),'_best_route.csv'],[best_route3';best_fit])
A_site    
end