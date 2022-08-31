rm(list = ls())
library(readxl)
library(tidyverse)
library(ggplot2)

All <- read_excel("Documents/NC Wells/Final Databases/Pb in NC Database_6.22.22.xlsx", sheet = 4)
Elements <- All[,2:18]
Elements <- sapply(Elements,as.numeric)
Elements <- data.frame(Elements)
Names <- All[1:17,24]

####summary stats####
MeanMat <- rep(0,17)
for(i in 1:ncol(Elements)){
  MeanX <- mean(Elements[ ,i], na.rm=TRUE)
  MeanMat[i] <- MeanX
}

MedianMat <- rep(0,17)
for(i in 1:ncol(Elements)){
  MedianX <- median(Elements[ ,i],na.rm=TRUE)
  MedianMat[i] <- MedianX
}

MaxMat <- rep(0,17)
for(i in 1:ncol(Elements)){
  MaxX <- max(Elements[ ,i],na.rm=TRUE)
  MaxMat[i] <- MaxX
}

MinMat <- rep(0,17)
for(i in 1:ncol(Elements)){
  MinX <- min(Elements[ ,i],na.rm=TRUE)
  MinMat[i]<-MinX
}

ninetyMat <- rep(0,17)
for(i in 1:ncol(Elements)){
  ninetyX <- quantile(Elements[,i], probs=0.90, na.rm=TRUE)
  ninetyMat[i] <- ninetyX
}

Summary<-cbind(Names,MeanMat,MedianMat,MaxMat,MinMat,ninetyMat)
View(Summary)

length(which(All$Pb=="NA"))
length(which(Elements$Pb>=1 & Elements$Pb<5))
length(which(Elements$Pb>=5))

length(which(Elements$PbX>Elements$Pb))

quantile(Elements$PbX,probs=0.95,na.rm=TRUE)

####fingerprinting####
cor.test(Elements$Pb,Elements$Zn,method="spearman")   
cor.test(Elements$Pb,Elements$Cu,method="spearman")
cor.test(Elements$Pb, Elements$Cd,method="spearman")
cor.test(Elements$Pb,Elements$Fe,method="spearman")
cor.test(Elements$Pb,Elements$Sn,method="spearman")
length(which(Elements$Cd))

cor.test(Elements$PbX,Elements$ZnX,method="spearman")
cor.test(Elements$PbX,Elements$CuX,method="spearman")
cor.test(Elements$PbX,Elements$pH,method="spearman")
cor.test(Elements$PbX,Elements$CdX,method="spearman")
cor.test(Elements$PbX,Elements$FeX,method="spearman")
cor.test(Elements$PbX,Elements$SnX,method="spearman")
length(which(Elements$CdX>=0.1))
length(which(Elements$SnX>=0.1))


####systems####
Drilled <- filter(All,All$`System type`=="Drilled well")
length(which(Drilled$`Catorgized well depth`=="Shallow"))
length(which(Drilled$`Catorgized well depth`=="Very shallow"))
Drilled <- sapply(Drilled,as.numeric)
Drilled <- data.frame(Drilled)
median(Drilled$pH, na.rm=TRUE)
median(Drilled$PbX,na.rm=TRUE)

Dug <- filter(All,All$`System type`=="Dug/bored well"|All$`System type`=="Sand point/wash down well")
length(which(Dug$`Catorgized well depth`=="Shallow"))
length(which(Dug$`Catorgized well depth`=="Very shallow"))
Dug <- sapply(Dug,as.numeric)
Dug <- data.frame(Dug)
median(Dug$pH,na.rm=TRUE)
median(Dug$PbX,na.rm=TRUE)

Shallow <- filter(All,All$`Catorgized well depth`=="Shallow" | All$`Catorgized well depth`=="Very shallow")
Shallow <- sapply(Shallow,as.numeric)
Shallow <- data.frame(Shallow)
median(Shallow$PbX,na.rm=TRUE)
median(Shallow$pH, na.rm=TRUE)

Deep <- filter(All,All$`Catorgized well depth`=="Deep")
Deep <- sapply(Deep,as.numeric)
Deep <- data.frame(Deep)
median(Deep$PbX,na.rm=TRUE)
median(Deep$pH,na.rm=TRUE)


####BRP####
BRP <- filter(All,All$Geology=="BRP")
BRP <- sapply(BRP,as.numeric)
BRP <- data.frame(BRP)
median(BRP$PbX,na.rm=TRUE)
median(BRP$pH,na.rm=TRUE)

BDrilled <- filter(BRP,BRP$`System type`=="Drilled well")
length(which(BDrilled$`Catorgized well depth`=="Shallow"))
length(which(BDrilled$`Catorgized well depth`=="Very shallow"))
BDrilled <- sapply(BDrilled,as.numeric)
BDrilled <- data.frame(BDrilled)
median(BDrilled$pH, na.rm=TRUE)
median(BDrilled$PbX,na.rm=TRUE)

BDug <- filter(BRP,BRP$`System type`=="Dug/bored well" | BRP$`System type`=="Sand point/wash down well")
length(which(BDug$`Catorgized well depth`=="Shallow"))
length(which(BDug$`Catorgized well depth`=="Very shallow"))
BDug <- sapply(BDug,as.numeric)
BDug <- data.frame(BDug)
median(BDug$pH,na.rm=TRUE)
median(BDug$PbX,na.rm=TRUE)

BShallow <- filter(BRP,BRP$`Catorgized well depth`=="Shallow" | BRP$`Catorgized well depth`=="Very shallow")
BShallow <- sapply(BShallow,as.numeric)
BShallow <- data.frame(BShallow)
median(BShallow$PbX,na.rm=TRUE)
median(BShallow$pH, na.rm=TRUE)

BDeep <- filter(BRP,BRP$`Catorgized well depth`=="Deep")
BDeep <- sapply(BDeep,as.numeric)
BDeep <- data.frame(BDeep)
median(BDeep$PbX,na.rm=TRUE)
median(BDeep$pH,na.rm=TRUE)


####CP####
CP <- filter(All,All$Geology=="CP")
CP <- sapply(CP,as.numeric)
CP <- data.frame(CP)
median(CP$PbX,na.rm=TRUE)
median(CP$pH,na.rm=TRUE)

CDrilled <- filter(CP,CP$`System type`=="Drilled well")
length(which(CDrilled$`Catorgized well depth`=="Shallow"))
length(which(CDrilled$`Catorgized well depth`=="Very shallow"))
CDrilled <- sapply(CDrilled,as.numeric)
CDrilled <- data.frame(CDrilled)
median(CDrilled$pH, na.rm=TRUE)
median(CDrilled$PbX,na.rm=TRUE)

CDug <- filter(CP,CP$`System type`=="Dug/bored well"|CP$`System type`=="Sand point/wash down well")
length(which(CDug$`Catorgized well depth`=="Shallow"))
length(which(CDug$`Catorgized well depth`=="Very shallow"))
CDug <- sapply(CDug,as.numeric)
CDug <- data.frame(CDug)
median(CDug$pH,na.rm=TRUE)
median(CDug$PbX,na.rm=TRUE)

CShallow <- filter(CP,CP$`Catorgized well depth`=="Shallow" | CP$`Catorgized well depth`=="Very shallow")
CShallow <- sapply(CShallow,as.numeric)
CShallow <- data.frame(CShallow)
median(CShallow$PbX,na.rm=TRUE)
median(CShallow$pH, na.rm=TRUE)

CDeep <- filter(CP,CP$`Catorgized well depth`=="Deep")
CDeep <- sapply(CDeep,as.numeric)
CDeep <- data.frame(CDeep)
median(CDeep$PbX,na.rm=TRUE)
median(CDeep$pH,na.rm=TRUE)

wilcox.test(BRP$PbX,CP$PbX)
wilcox.test(CShallow$pH,CDeep$pH)

####Race####
BIPOC <- filter(All,All$Race=="BIPOC")
White <- filter(All,All$Race=="White")

BIPOC <- sapply(BIPOC, as.numeric)
BIPOC <- data.frame(BIPOC)

White <- sapply(White,as.numeric)
White <- data.frame(White)

wilcox.test(BIPOC$PbX,White$PbX)
median(BIPOC$PbX,na.rm=TRUE)
median(White$PbX,na.rm=TRUE)

####bootstrapping####
bootstrap_fun<-function(
  n_iterations = 1000, #number of bootstraps
  n_pop =100, #population size
  p_bipoc #percent of population from bipoc
){
  
  #Determine number of samples
  n_bipoc<-round(p_bipoc/100*n_pop,0)
  n_white<-n_pop-n_bipoc  
  
  #Create inner function
  inner_fun<-function(n){
    bind_rows(
      sample_n(White, n_white),
      sample_n(BIPOC, n_bipoc)
    ) %>% 
      #Estimate 90& from sample population
      summarise(PbX=quantile(PbX, 0.9,na.rm=TRUE)) %>% 
      #Add Unique identifier for iternation
      mutate(n=n)
  }
  
  output<-lapply(seq(1, n_iterations), inner_fun) %>% bind_rows()
  
  output<-output %>% 
    summarise(
      mean = mean(PbX,na.rm=TRUE), 
      upr = quantile(PbX, 0.975), 
      lwr = quantile(PbX, 0.025)) %>% 
    #Add % bipoc to summary
    mutate(p_bipoc = p_bipoc)
  
  #Export results
  output
}

wrapper_fun<-function(n){bootstrap_fun(p_bipoc=n)}

#Apply wrapper function
df<-lapply(seq(0,100,10), wrapper_fun) %>% bind_rows()

####state####
bootstrap_fun<-function(
  n_iterations = 1000, #number of bootstraps
  n_pop =100, #population size
  p_bipoc #percent of population from bipoc
){
  
  #Determine number of samples
  n_bipoc<-round(p_bipoc/100*n_pop,0)
  n_white<-n_pop-n_bipoc  
  
  #Create inner function
  inner_fun<-function(n){
    bind_rows(
      sample_n(White, n_white),
      sample_n(BIPOC, n_bipoc)
    ) %>% 
      #Estimate 90& from sample population
      summarise(PbX=quantile(PbX, 0.9,na.rm=TRUE)) %>% 
      #Add Unique identifier for iternation
      mutate(n=n)
  }
  
  output<-lapply(seq(1, n_iterations), inner_fun) %>% bind_rows()
  
  output<-output %>% 
    summarise(
      mean = mean(PbX,na.rm=TRUE), 
      upr = quantile(PbX, 0.975), 
      lwr = quantile(PbX, 0.025)) %>% 
    #Add % bipoc to summary
    mutate(p_bipoc = p_bipoc)
  
  #Export results
  output
}

wrapper_fun<-function(n){bootstrap_fun(p_bipoc=n)}

#Apply wrapper function
Sdf<-lapply(seq(26.8,26.9,0.1), wrapper_fun) %>% bind_rows()
####Iredell####
Iredell <- filter(All,All$County=="Iredell")
IWhite <- filter(Iredell,Iredell$Race=="White")
IWhite <- sapply(IWhite, as.numeric)
IWhite <- data.frame(IWhite)
Ibipoc <- filter(Iredell,Iredell$Race=="BIPOC")
Ibipoc <- sapply(Ibipoc,as.numeric)
Ibipoc <- data.frame(Ibipoc)

bootstrap_fun<-function(
  n_iterations = 1000, #number of bootstraps
  n_pop =100, #population size
  p_bipoc #percent of population from bipoc
){
  
  #Determine number of samples
  n_bipoc<-round(p_bipoc/100*n_pop,0)
  n_white<-n_pop-n_bipoc  
  
  #Create inner function
  inner_fun<-function(n){
    bind_rows(
      sample_n(White, n_white),
      sample_n(BIPOC, n_bipoc)
    ) %>% 
      #Estimate 90& from sample population
      summarise(PbX=quantile(PbX, 0.9,na.rm=TRUE)) %>% 
      #Add Unique identifier for iternation
      mutate(n=n)
  }
  
  output<-lapply(seq(1, n_iterations), inner_fun) %>% bind_rows()
  
  output<-output %>% 
    summarise(
      mean = mean(PbX,na.rm=TRUE), 
      upr = quantile(PbX, 0.975), 
      lwr = quantile(PbX, 0.025)) %>% 
    #Add % bipoc to summary
    mutate(p_bipoc = p_bipoc)
  
  #Export results
  output
}

wrapper_fun<-function(n){bootstrap_fun(p_bipoc=n)}

#Apply wrapper function
Idf<-lapply(seq(22.6,22.7,0.1), wrapper_fun) %>% bind_rows()

####Northampton####
Northampton <- filter(All,All$County=="Northampton")
NWhite <- filter(Northampton,Northampton$Race=="White")
NWhite <- sapply(NWhite, as.numeric)
NWhite <- data.frame(NWhite)
Nbipoc <- filter(Northampton,Northampton$Race=="BIPOC")
Nbipoc <- sapply(Nbipoc,as.numeric)
Nbipoc <- data.frame(Nbipoc)

bootstrap_fun<-function(
  n_iterations = 1000, #number of bootstraps
  n_pop =100, #population size
  p_bipoc #percent of population from bipoc
){
  
  #Determine number of samples
  n_bipoc<-round(p_bipoc/100*n_pop,0)
  n_white<-n_pop-n_bipoc  
  
  #Create inner function
  inner_fun<-function(n){
    bind_rows(
      sample_n(White, n_white),
      sample_n(BIPOC, n_bipoc)
    ) %>% 
      #Estimate 90& from sample population
      summarise(PbX=quantile(PbX, 0.9,na.rm=TRUE)) %>% 
      #Add Unique identifier for iternation
      mutate(n=n)
  }
  
  output<-lapply(seq(1, n_iterations), inner_fun) %>% bind_rows()
  
  output<-output %>% 
    summarise(
      mean = mean(PbX,na.rm=TRUE), 
      upr = quantile(PbX, 0.975), 
      lwr = quantile(PbX, 0.025)) %>% 
    #Add % bipoc to summary
    mutate(p_bipoc = p_bipoc)
  
  #Export results
  output
}

wrapper_fun<-function(n){bootstrap_fun(p_bipoc=n)}

#Apply wrapper function
Ndf<-lapply(seq(61.2,61.3,0.1), wrapper_fun) %>% bind_rows()

####Chatham####
Chatham <- filter(All,All$County=="Chatham")
Cwhite <- filter(Chatham,Chatham$Race=="White")
CWhite <- sapply(CWhite, as.numeric)
CWhite <- data.frame(CWhite)
Cbipoc <- filter(Chatham,Chatham$Race=="BIPOC")
Cbipoc <- sapply(Cbipoc,as.numeric)
Cbipoc <- data.frame(Cbipoc)

bootstrap_fun<-function(
  n_iterations = 1000, #number of bootstraps
  n_pop =100, #population size
  p_bipoc #percent of population from bipoc
){
  
  #Determine number of samples
  n_bipoc<-round(p_bipoc/100*n_pop,0)
  n_white<-n_pop-n_bipoc  
  
  #Create inner function
  inner_fun<-function(n){
    bind_rows(
      sample_n(White, n_white),
      sample_n(BIPOC, n_bipoc)
    ) %>% 
      #Estimate 90& from sample population
      summarise(PbX=quantile(PbX, 0.9,na.rm=TRUE)) %>% 
      #Add Unique identifier for iternation
      mutate(n=n)
  }
  
  output<-lapply(seq(1, n_iterations), inner_fun) %>% bind_rows()
  
  output<-output %>% 
    summarise(
      mean = mean(PbX,na.rm=TRUE), 
      upr = quantile(PbX, 0.975), 
      lwr = quantile(PbX, 0.025)) %>% 
    #Add % bipoc to summary
    mutate(p_bipoc = p_bipoc)
  
  #Export results
  output
}

wrapper_fun<-function(n){bootstrap_fun(p_bipoc=n)}

#Apply wrapper function
Cdf<-lapply(seq(28.8,28.9,0.1), wrapper_fun) %>% bind_rows()


State <- read_excel("Documents/NC Wells/NCwelldata_individual_well_tests.xlsx",sheet=2)
State$Lead_ppm[State$Lead_ppm=="NA"] <- 0
PbTest <- filter(State,State$Lead_ppm>0)

PbTest <- sapply(PbTest,as.numeric)
PbTest <- data.frame(PbTest)

PbTest$Pb <- PbTest$Lead_ppm * 1000
PbTest$Cd <- PbTest$Cadmium_ppm *1000
PbTest$Cu <- PbTest$Copper_ppm *1000
PbTest$Zn <- PbTest$Zinc_ppm * 1000
PbTest$Fe <- PbTest$Iron_ppm * 1000
PbTest$Ni <- PbTest$Nickel_ppm * 1000

Data <- PbTest[,8:13]
Data <- sapply(Data,as.numeric)
Data <- data.frame(Data)

####summary stats####
MeanMat <- rep(0,6)
for(i in 1:ncol(Data)){
  MeanX <- mean(Data[ ,i],na.rm=TRUE)
  MeanMat[i] <- MeanX
}

MedianMat <- rep(0,6)
for(i in 1:ncol(Data)){
  MedianX <- median(Data[ ,i],na.rm=TRUE)
  MedianMat[i] <- MedianX
}

MaxMat <- rep(0,6)
for(i in 1:ncol(Data)){
  MaxX <- max(Data[ ,i],na.rm=TRUE)
  MaxMat[i] <- MaxX
}

MinMat <- rep(0,6)
for(i in 1:ncol(Data)){
  MinX <- min(Data[ ,i],na.rm=TRUE)
  MinMat[i]<-MinX
}

ninetyMat <- rep(0,6)
for(i in 1:ncol(Data)){
  ninetyX <- quantile(Data[,i], probs=0.90, na.rm=TRUE)
  ninetyMat[i] <- ninetyX
}

Summary<-cbind(MeanMat,MedianMat,ninetyMat)
View(Summary)

####corrosion####
Lead <- filter(Data,Data[,1]>5)
cor.test(Lead[,1],Lead[,4],method="spearman") #Pb Zn
cor.test(Lead[,1],Lead[,3],method="spearman") #Pb Cu
length(which(Lead[,2]>1))/8422           #Cd
cor.test(Lead[,2],Lead[,6],method="spearman")

####VT comparison####
ks.test(Lead5[,17],Lead[,1])
wilcox.test(Lead5[,17],Lead[,1])
median(Lead5[,17],na.rm=TRUE)
median(Lead[,1],na.rm=TRUE)
