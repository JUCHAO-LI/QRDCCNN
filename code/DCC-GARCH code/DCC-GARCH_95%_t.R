###��������###
rm(list=ls())#������б���
cat("\014") #clear console
setwd("D:/�����ļ�/�ӳ�QCNN����/����/������")

library(readxl)
data1<-read_excel('���ݺϲ�����.xlsx',sheet ='��ʧ��+�ͺ�')
data<-as.data.frame(data1[,2:40])



m<-nrow(data)
n<-ncol(data)




coef1<-NULL
sigma1<-NULL  
residual1<-NULL  # ������ϵĲв�
z1<-NULL
p1<-NULL#����XX1�Ƿ����ھ��ȷֲ���P>0.05,����ԭ����  ���ھ��ȷֲ�
XX1<-NULL#ת����ĸ��ʷֲ�
optimal_LL<-NULL

LL1<-NULL
AIC1<-NULL
copuladcc1<-NULL
sigma1<-NULL
rho<-NULL
library(fGarch)
library(rugarch)
library(rmgarch)


for (i in 1:n){
  a<-data[,i]
  GARCH.spec<- ugarchspec(variance.model=list(model='fGARCH', garchOrder=c(1,1), submodel='TGARCH'), 
                          mean.model=list(armaOrder=c(1,1), archpow = 1,include.mean=TRUE, archm=FALSE),
                          distribution.model='std')
  GARCH.fit<- ugarchfit(GARCH.spec, data=a,solver = 'solnp',trunclag = 1000000)
  GARCH.fit
  LL1<-cbind(LL1,as.matrix(likelihood(GARCH.fit)))
  AIC1<-cbind(AIC1,as.matrix(infocriteria(GARCH.fit)[1]))
  # coef2<-as.matrix(GARCH.fit@fit[["coef"]])
  # coef1[1:length(coef2),i]<-coef2
  sigma2<-GARCH.fit@fit[["sigma"]]
  sigma1<-cbind(sigma1,sigma2)
  residual1<-cbind(residual1,GARCH.fit@fit[["residuals"]])
  z2<-as.matrix(GARCH.fit@fit[["z"]])
  z1<-cbind(z1,z2)
}


for (j in 2:n){
  b<-data[,c(1,j)]
  dcc.garch.spec1_1 <- dccspec(uspec = multispec(replicate(2, GARCH.spec)),
                               dccOrder = c(1,1),
                               distribution = "mvt")
  dcc.fit1_1 <- dccfit(dcc.garch.spec1_1, data = b,solver = "solnp")   #����˫����DCCGARCHģ��
  dcc.fit1_1
  coef2<-as.matrix(coef(dcc.fit1_1))
  coef1<-cbind(coef1,coef2)
  cor1_1<-data.frame(dcc.fit1_1@mfit$R)
  cor1_1<-t(cor1_1)
  cor1_1<-cor1_1[,1]
  del1_1<-seq(1,length(cor1_1),by=2)
  cor1_1<-cor1_1[-del1_1]
  rho1_1 <- as.numeric(cor1_1)
  rho<-cbind(rho,rho1_1)
}

VaR=NULL
for (i in 2:ncol(data)){ 
  VaR=cbind(VaR,(qnorm(0.95)*sigma1[,i]))
}

CoVaR=NULL
for (i in 2:ncol(data)){ 
  CoVaR=cbind(CoVaR,(qnorm(0.95)*sigma1[,1]*sqrt(1-rho[,i-1]^2)+qnorm(0.95)*rho[,i-1]*sigma1[,i]))
}


mm_VaR=NULL
mm_CoVaR=NULL
pp_VaR=NULL
pp_CoVaR=NULL
for (i in 2:ncol(data)){ 
  n1<-nrow(data);  #�����ܸ���
  
  aaa_VaR<-data[,i]>VaR[,(i-1)]
  m_VaR<-length(aaa_VaR[aaa_VaR==TRUE])#ʧ����������������С��VaR�������ܺ�
  m_VaR
  mm_VaR<-cbind(mm_VaR,m_VaR)
  
  aaa_CoVaR<-data[,i]>CoVaR[,(i-1)]
  m_CoVaR<-length(aaa_CoVaR[aaa_CoVaR==TRUE])#ʧ����������������С��CoVaR�������ܺ�
  m_CoVaR
  mm_CoVaR<-cbind(mm_CoVaR,m_CoVaR)
  
  p<-0.05;   # ���ⷢ���ĸ���
  conf<-1-p;  # chi^2 �ֲ�������ˮƽ
  qchisq(conf,1);
  
  LR_VaR=2*log((((1-(m_VaR/n1))^(n1-m_VaR))*((m_VaR/n1)^m_VaR))/(((1-p)^(n1-m_VaR) )* (p^m_VaR)));
  LR_VaR
  p1=1-pchisq(LR_VaR,1) ##p1Ϊ������ˮƽ��Pֵ
  p1
  pp_VaR<-cbind(pp_VaR,p1)
  
  LR_CoVaR=2*log((((1-(m_CoVaR/n1))^(n1-m_CoVaR))*((m_CoVaR/n1)^m_CoVaR))/(((1-p)^(n1-m_CoVaR) )* (p^m_CoVaR)));
  LR_CoVaR
  p1=1-pchisq(LR_CoVaR,1) ##p1Ϊ������ˮƽ��Pֵ
  p1
  pp_CoVaR<-cbind(pp_CoVaR,p1)
}


mean(pp_VaR)
pp_CoVaR[which(is.nan(pp_CoVaR)==TRUE)]=0
mean(pp_CoVaR)

result<-cbind(t(mm_VaR),t(mm_CoVaR),t(pp_VaR),t(pp_CoVaR))
# write.csv(result,file = "C:/Users/lijuchao/����/DCC-GARCH_Kupiec���_95_t.csv")