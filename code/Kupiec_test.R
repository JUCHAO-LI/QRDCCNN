###导入数据###
rm(list=ls())#清除所有变量
cat("\014") #clear console
setwd("C:\\Users\\14867\\Desktop\\附件")

library(rJava)

library(xlsxjars)

library(xlsx)
data<-read.xlsx('Kupiec汇总版.xlsx',sheetName ='失败天数_test')
data<-data[,-1]

pp=NULL
pp1=NULL
for (i in 1:ncol(data)){
  for (j in 1:nrow(data)){
    n<-123;  #样本总个数
    m<-data[j,i] #失败天数，即收益率小于VaR个数的总和
    p<-0.05;   # 例外发生的概率
    conf<-1-p;  # chi^2 分布的置信水平
    qchisq(conf,1);
    
    # LR=-2*log(((1-p)^(n-m) )* (p^m))+2*log(((1-(m/n))^(n-m))*((m/n)^m));
    LR=2*log((((1-(m/n))^(n-m))*((m/n)^m))/(((1-p)^(n-m) )* (p^m)));
    LR
    p1=1-pchisq(LR,1) ##p1为显著性水平的P值
    pp=rbind(pp,p1)
  }
}
pp1<-matrix(as.numeric(pp), ncol=ncol(data)) #P值
# write.csv(pp1,file = "C:/Users/14867/Desktop/test.csv")
apply(pp1,2,mean)