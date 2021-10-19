### Code Updated 13 Oct 2021
### ***DAY LOCAL MIN DATA with FACS***
### This code runs the data from the deadline stress data experiments
### It involved 10 subjects that participated in an experiment that lasted 4 days.
### Two days were 1 before and the other ON the deadline and 2 more vanilla days

library(MASS)
library(car)
library(sjPlot)

### set the path to the folder that has the data
#setwd("....../..../......")

### Here we will load the LOCAL MIN DAY data
tmpData0<-read.csv("Local_min_day_model_data+FACS.csv",header = TRUE,sep = ",") ### DATA WITH FACS
dim(tmpData0)
tmpD0<-tmpData0
tmpD0<-tmpD0[,-c(1,3,34:42)]
### next we add three columns at the end with the "FACS_positive","FACS_negative"
### and "FACS_neutral", where for positive we use only happy and for negative the
### other 5 emotions and neutral by itself
tmpD0<-cbind(tmpD0,tmpData0[,37],rowSums(tmpData0[,c(34:36,38:39)]),tmpData0[,40])
dim(tmpD0)

Y<-tmpD0[,2]  ### The response variable is PP

VarNames0<-c("Day","PP","G","R","TA","B5_A","B5_C","B5_E","B5_N","B5_O","AB","T_D",
             "SA_B","SA_E","N_MD","N_PD","N_TD","N_P","N_E","N_F","RW","SA","SP","Out",
             "I","tOut","fOut","T_RW_APPS","T_EA","T_PA","T_VC","FACS_pos","FACS_neg",
             "FACS_neut")
length(VarNames0)

### The following are the categorical explanatory variables
tmpCAT<-c(1,3,4)

### The following are the numerical explanatory variables
tmpNUM<-c(5:34)

for (i in tmpCAT){
  assign(paste(VarNames0[i]),as.factor(tmpD0[,i]))
}
for (i in tmpNUM){
  assign(paste(VarNames0[i]),tmpD0[,i])
}



### Here we will fit the models

### This is the model without any terms 
NullModel<-lm(Y~1)
summary(NullModel)

### FACS_neut  is collinear with FACS_pos & FACS_neg 
### (as they sum to 1 and so it was left outside of the model)
FullModel0<-lm(Y~Day+G+R+TA+B5_A+B5_C+B5_E+B5_N+B5_O+AB+T_D+SA_B+SA_E+
                 N_MD+N_PD+N_TD+N_P+N_E+N_F+RW+SA+SP+Out+I+tOut+fOut+
                 T_RW_APPS+T_EA+T_PA+T_VC+FACS_pos+FACS_neg)
summary(FullModel0)
### From the last output we observe that we cannot estimate the coefficients 
### for the terms "B5_O" and "AB", as they appear to be collinear with
### existing terms in the model and precisely with the following:
### (G2, R2, R3, R4, TA, B5_A, B5_C, B5_E, B5_N)

### We will remove "B5_O" and "AB" and we will examine the correlation 
### matrix to find which other explanatory variables are highly correlated
cor(tmpD0[,tmpNUM])


### Next we removed the variables with correlation that exceeds 0.7 and 
### we are left with the following model
FullModel1<-lm(Y~Day+G+R+TA+B5_A+B5_C+B5_E+B5_N+T_D+SA_B+SA_E+
                 N_MD+N_PD+N_P+RW+SA+SP+I+tOut+fOut+FACS_neg)
summary(FullModel1)

vif(FullModel1) ### check VIFs for collinearity

### Next we removed the variable B5_A as its VIF exceeded the value of 10
FullModel2<-lm(Y~Day+G+R+TA+B5_C+B5_E+B5_N+T_D+SA_B+SA_E+
                 N_MD+N_PD+N_P+RW+SA+SP+I+tOut+fOut+FACS_neg)
summary(FullModel2)

vif(FullModel2)

### In the last model all the VIFs are below 10 and so we will keep these
### variables in the model. 

### Next we will also check the model diagnostics for this full model
par(mfrow=c(2,2))
plot(FullModel2)
title("Full Model diagnostic plots",outer=TRUE, cex.main=1.5, col.main="blue",line=-2)
### from the above case 39 appears as possible outlying/influential point

### We will rerun the Full model to start searching for the best sub-model
FullModel<-lm(Y~Day+G+R+TA+B5_C+B5_E+B5_N+T_D+SA_B+SA_E+
                N_MD+N_PD+N_P+RW+SA+SP+I+tOut+fOut+FACS_neg)
summary(FullModel)
anova(FullModel)

### Here we will run the Forward Selection
FSmodel<-stepAIC(NullModel,scope=list(lower=formula(NullModel),upper=formula(FullModel)),
                 direction="forward",trace = FALSE)
FSmodel$anova
summary(FSmodel)
vif(FSmodel)
anova(NullModel,FSmodel,FullModel,test="LRT")


### Here we will run the Step-Wise method
SWmodel<-stepAIC(NullModel,scope=list(lower=formula(NullModel),upper=formula(FullModel)),
                 direction="both",trace = FALSE)
SWmodel$anova
summary(SWmodel)
vif(SWmodel)
anova(NullModel,FSmodel,SWmodel,FullModel,test="LRT")


### Here we will run the Backward Elimination
BEmodel<-stepAIC(FullModel, direction="backward",trace=FALSE)
BEmodel$anova
summary(BEmodel)
vif(BEmodel)
anova(NullModel,FSmodel,SWmodel,BEmodel,FullModel,test="LRT")



### Diagnostic plots for all the models
par(mfrow=c(2,2))
plot(FSmodel)
title("Forward Selection Model diagnostic plots",outer=TRUE, cex.main=1.5, col.main="blue",line=-2)
plot(SWmodel)
title("Stepwise Model diagnostic plots",outer=TRUE, cex.main=1.5, col.main="blue",line=-2)
plot(BEmodel)
title("Backward Elimination Model diagnostic plots",outer=TRUE, cex.main=1.5, col.main="blue",line=-2)
plot(FullModel)
title("Full Model diagnostic plots",outer=TRUE, cex.main=1.5, col.main="blue",line=-2)

anova(NullModel,FSmodel,SWmodel,BEmodel,FullModel,test="LRT")

### The FS and SW proposals lead to the same model, while the BE is considerably
### bigger model. From the anova tests performed across the different models
### we observe that there is no significant improvement when we move from the 
### FS/SW model to the BE model or to the Full model. Thus we will adopt the 
### FS/SW model Y ~ fOut + RW + SP

### Next we performed a detailed supervised search in various sub-models  
### that contain several interaction terms and we were able to identify 
### a model which improves further what we got from the algorithmic methods

fit0<-lm(formula = Y ~ fOut + RW + SP)
fit1<-lm(formula = Y ~ fOut + RW + R*SP)
summary(fit1)
vif(fit1)
anova(fit0,fit1,test="LRT")

### From the above we observe that we lose significant information when 
### when we do not consider the interaction, so the final model will be:

FM<-lm(formula = Y ~ fOut + RW + R*SP)
summary(FM)
anova(FM)

### Here we will plot the fixed effects
plot_model(FM,"est",sort=TRUE, show.values = TRUE, value.offset = .3,xlab="")+font_size(labels.x=15,labels.y=15,title = 0,axis_title.x =0)

### Predicted values (marginal effects) for all of model terms
plot_model(FM,"pred") 

### Predicted values (marginal effects) for each of model terms
plot_model(FM,"pred")$fOut+font_size(labels.x=15,labels.y=15,title = 0,axis_title.x = 15,axis_title.y = 15)
plot_model(FM,"pred")$RW+font_size(labels.x=15,labels.y=15,title = 0,axis_title.x = 15,axis_title.y = 15)
plot_model(FM,"pred")$R+font_size(labels.x=15,labels.y=15,title = 0,axis_title.x = 15,axis_title.y = 15)
plot_model(FM,"pred")$SP+font_size(labels.x=15,labels.y=15,title = 0,axis_title.x = 15,axis_title.y = 15)

### Plot for the 2-way interactions (only R*SP was significant)
### Predicted values (marginal effects) for each of model terms
### and each level of the categorical variable Rank (R)
plot_model(FM,"pred",terms=c("SP","R"))
plot_model(FM,"pred",terms=c("fOut","R"))
plot_model(FM,"pred",terms=c("RW","R"))



