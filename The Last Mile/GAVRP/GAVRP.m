%�Ŵ��㷨 VRP ���� Matlabʵ�� ��ʵ�ֵ��̶����ķ���

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear;clc
for A_site=1:124
best_route4 = [];
dis_file=csvread (['A',num2str(A_site),'_dis_AB2.csv']);   %����ԭ��   site��spot֮������
num_file=csvread (['A',num2str(A_site),'_num_AB2.csv'],1,1);   %������ԭ��
[m n]=size(dis_file);
num_spot_id=n-1;
Sup_Parent=[1:n-1];
%Sup_Parent=[3 6 7 17 18 5 8 16 19 15 26 25 4 27 29 2 9 14 20 28 30 31 13 22 21 24 11 12 23 10]-1;
G=100;%��Ⱥ��С
Parent=rand(G,num_spot_id);
%Parent=rand(G,30);%�漴����
for i=1:G
[m n]=sort(Parent(i,:));                     %��ʼ�����ݡ� �Ե�i���������򣬷������������m���Ͷ�Ӧ�����n
Parent(i,:)=n;                               %����Ÿ�����i�и�Ԫ��
end                                          %�����õ�100��1-30���������
Pc=0.8;%�������
Pm=0.2;%�������
species=[Sup_Parent;Parent];%��Ⱥ
children=[];%�Ӵ�
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%g=input('����ʱ������');
h = waitbar(0,'Please wait...');%������
for generation=1:floor(num_spot_id*0.5)
%for generation=1:floor(num_spot_id*1.5)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Parent=species;%�Ӵ���ɸ���
children=[];%�Ӵ�
%ѡ�񽻲游��
[n m]=size(Parent);      %����parent��ά����n��m
% select=rand(1,n)<Pc;
% select=find(select==1);
                                                                                                                %����
for i=1:n
    for j=i:n
        if i~=j & rand<Pc             %i������j����һ�����С��PC
            jiaocha
        end
    end
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
for i=1:n
    if rand<Pm
    parent=Parent(i,:);%�������
    X=floor(rand*num_spot_id)+1;
    Y=floor(rand*num_spot_id)+1;
    Z=parent(X);
    parent(X)=parent(Y);
    parent(Y)=Z;                                                                                                 %����
    children=[children;parent];
    end
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%�����Ӵ���Ӧֵ
[m n]=size(children);
fitness_value_c=zeros(m,1);%�Ӵ���Ӧֵ
for i=1:m
    l1=1;
    for l2=1:n
        if sum(num_file(children(i,l1:l2),1))>135             %135�����������������ȡ����140С����
            fitness_c
            l1=l2;
        end
        if l2==n
            l2=l2+1;
            fitness_c
        end
                                                                                                                  %������Ӧֵ
        
    
    end
end
%���㸸����Ӧֵ
[m n]=size(Parent);
fitness_value_P=zeros(m,1);%������Ӧֵ
for i=1:m
    l1=1;
    for l2=1:n
        if sum(num_file(Parent(i,l1:l2),1))>135           % 135��������
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
%��̭�Ӵ�
[m n]=sort(fitness_value_c);
children=children(n(1:G),:);   % G��Ⱥ��С100������ǰ100��
fitness_value_c=fitness_value_c(n(1:G));
%��̭����
[m n]=sort(fitness_value_P);
Parent=Parent(n(1:G),:);
fitness_value_P=fitness_value_P(n(1:G));
%��̭��Ⱥ
species=[children;Parent];
fitness_value=[fitness_value_c;fitness_value_P];
[m n]=sort(fitness_value);
species=species(n(1:G),:);
fitness_value=fitness_value(n(1:G));                                                                             %��������
%waitbar(generation/2,h)
waitbar(generation/floor(num_spot_id*0.5),h)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
end
best_route = species(1,:);%������·
best_fit = fitness_value(1);%���ŷ���
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