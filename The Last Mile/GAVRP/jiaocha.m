%OX˳�򽻲����
P1=Parent(i,:);
P2=Parent(j,:);

%ѡ���е�,�����м䲿�� ���� �޸�����
X=floor(rand*(num_spot_id-2))+2;
Y=floor(rand*(num_spot_id-2))+2;
%X=floor(rand*28)+2;
%Y=floor(rand*28)+2;
if X<Y
    change1=P1(X:Y);
    change2=P2(X:Y);
    %P1(X:Y)=change2;
    %P2(X:Y)=change1;
    %��ʼ�޸� Order Crossover
    %1.�г����� 
    p1=[P1(Y+1:end),P1(1:X-1),change1];
    p2=[P2(Y+1:end),P2(1:X-1),change2];
    %2.1ɾ�����ɻ��� P1
    for i_gern=1:length(change2)
        p1(find(p1==change2(i_gern)))=[];
    end
    %2.2ɾ�����ɻ��� P2
    for i_gern=1:length(change1)
        p2(find(p2==change1(i_gern)))=[];
    end
    %3.1�޸� P1
    P1=[p1(num_spot_id-Y+1:end),change2,p1(1:num_spot_id-Y)];
    %3.1�޸� P2
    P2=[p2(num_spot_id-Y+1:end),change1,p2(1:num_spot_id-Y)];
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
else
    change1=P1(Y:X);
    change2=P2(Y:X);
    %P1(Y:X)=change2;
    %P2(Y:X)=change1;
    %��ʼ�޸� Order Crossover
    %1.�г����� 
    p1=[P1(X+1:end),P1(1:Y-1),change1];
    p2=[P2(X+1:end),P2(1:Y-1),change2];
    %2.1ɾ�����ɻ��� P1
    for i_gern=1:length(change2)
        p1(find(p1==change2(i_gern)))=[];
    end
    %2.2ɾ�����ɻ��� P2
    for i_gern=1:length(change1)
        p2(find(p2==change1(i_gern)))=[];
    end
    %3.1�޸� P1
    P1=[p1(num_spot_id-X+1:end),change2,p1(1:num_spot_id-X)];
    %3.1�޸� P2
    P2=[p2(num_spot_id-X+1:end),change1,p2(1:num_spot_id-X)];
end
%�����Ӵ�
children=[children;P1;P2];
    