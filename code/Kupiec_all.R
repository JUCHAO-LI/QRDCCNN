###��������###
rm(list=ls())#������б���
cat("\014") #clear console
setwd("C:\\Users\\14867\\Desktop\\����")

library(rJava)

library(xlsxjars)

library(xlsx)
data<-read.xlsx('Kupiec���ܰ�.xlsx',sheetName ='ʧ������_all')
data<-data[,-1]

pp=NULL
pp1=NULL
for (i in 1:ncol(data)){
  for (j in 1:nrow(data)){
    n<-2448;  #�����ܸ���
    m<-data[j,i] #ʧ����������������С��VaR�������ܺ�
    p<-0.05;   # ���ⷢ���ĸ���
    conf<-1-p;  # chi^2 �ֲ�������ˮƽ
    qchisq(conf,1);
    
    # LR=-2*log(((1-p)^(n-m) )* (p^m))+2*log(((1-(m/n))^(n-m))*((m/n)^m));
    LR=2*log((((1-(m/n))^(n-m))*((m/n)^m))/(((1-p)^(n-m) )* (p^m)));
    LR
    p1=1-pchisq(LR,1) ##p1Ϊ������ˮƽ��Pֵ
    pp=rbind(pp,p1)
  }
}
pp1<-matrix(as.numeric(pp), ncol=ncol(data)) #Pֵ
# write.csv(pp1,file = "C:/Users/14867/Desktop/all.csv")
apply(pp1,2,mean)