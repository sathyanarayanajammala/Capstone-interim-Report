
##################################
# This files contians Diabetes data analysis
##############################

# import libraries
#install.packages('lmtest')
#install.packages('caTools')
#install.packages('pscl')
#install.packages('ROSE')
#install.packages('ROCR')
#install.packages('party')
#install.packages('randomForest')
# install.packages('caret', dependencies = TRUE)
# install.packages(" pbkrtest")
library(lmtest)
library(caTools)
library(pscl)
library(ROSE)
library(ROCR)
library(party)
library(randomForest)
#library(caret)

# Data imported to Environment 
srcmydata=read.csv(file.choose())

mydata=srcmydata
attach(mydata)

#######################################
#Converting catogorical data to factors
######################################

mydata$ALIVE...DEAD=as.numeric(factor(mydata$ALIVE...DEAD))
mydata$RELIGION=as.numeric(factor(mydata$RELIGION))
mydata$GENDER=as.numeric(factor(mydata$GENDER))
mydata$FINANCIAL.GROUP=as.numeric(factor(mydata$FINANCIAL.GROUP))
#mydata$FAMILY.HISTORY=as.numeric(factor(mydata$FAMILY.HISTORY))
#mydata$Blood.Group = as.numeric(factor(mydata$Blood.Group))
mydata$STAPLE.FOOD= as.numeric(factor(mydata$STAPLE.FOOD))
mydata$ALCOHOL = as.numeric(factor(mydata$ALCOHOL))
mydata$Known.H.o.Smoking=as.numeric(factor(mydata$Known.H.o.Smoking))
mydata$Tobacco.Consumption.in.any.other.way=as.numeric(factor(mydata$Tobacco.Consumption.in.any.other.way))
mydata$PhysicalActivity=as.numeric(factor(mydata$PhysicalActivity))

#create training and validation data from given data
str(mydata)



###############################################
# Remove #N/A cells with mean values of the column
################################################

mydata$BP.Systolic <- ifelse(is.na(mydata$BP.Systolic),mean(mydata$BP.Systolic, na.rm = TRUE),mydata$BP.Systolic)
mydata$BP.Diastolic <- ifelse(is.na(mydata$BP.Diastolic),mean(mydata$BP.Diastolic, na.rm = TRUE),mydata$BP.Diastolic)
mydata$BMI <- ifelse(is.na(mydata$BMI),mean(mydata$BMI, na.rm = TRUE),mydata$BMI)
#mydata[is.na(mydata[,8]), 8] <- mean(data[,8], na.rm = TRUE)
mydata <- mydata[!(is.na(mydata$PhysicalActivity)),]




############################################
# Dependent variables to be converted as factors
############################################



mydata$CVC <- as.factor(mydata$CVC)
mydata$Diabetic <- as.factor(mydata$Diabetic)


########################################################
#  ROse package to normalize/balanced data for accurate prediction
#######################################################

mydata1 <- ROSE(Diabetic ~ .,data=mydata, seed = 123)$data

############################
#Elimaniting wrong data colums
###########################
mydata1$S.NO.<- NULL
#mydata1$Year<- NULL
#mydata1$LOCATION<- NULL
#mydata1$RELIGION<- NULL
mydata1$ALIVE...DEAD<- NULL
mydata1$Blood.Group <- NULL

names(mydata1)
par(mar = rep(2, 4))
par(mfrow=c(4,3))
hist(mydata1$HBA1C,probability = T,main="BoxPlot of HBA1c",xlab = "HBA1c")

hist(mydata1$AGE.AT.WHICH.THE.AILMENT.WAS.DIAGNOSED,probability = T,
     main="BoxPlot of AGE.AT.WHICH.DIAGNOSED",xlab = "AGE.AT.WHICH.DIAGNOSED")

hist(mydata1$ALCOHOL,probability = T,
     main="BoxPlot of ALCOHOL",xlab = "ALCOHOL")

hist(mydata1$AGE.AT.WHICH.THE.AILMENT.WAS.DIAGNOSED,probability = T,
     main="BoxPlot of Tobacco",xlab = "Tobacco")

hist(mydata1$AGE.AT.WHICH.THE.AILMENT.WAS.DIAGNOSED,probability = T,
     main="BoxPlot of Random.Blood.Sugar",xlab = "Random.Blood.Sugar")

hist(mydata1$PhysicalActivity,probability = T,
     main="BoxPlot of PhysicalActivity",xlab = "PhysicalActivity")
hist(mydata1$PhysicalActivity,probability = T,
     main="BoxPlot of BMI",xlab = "BMI")

hist(mydata1$PhysicalActivity,probability = T,
     main="BoxPlot of Known.H.o.Smoking",xlab = "Known.H.o.Smoking")
hist(mydata1$PhysicalActivity,probability = T,
     main="BoxPlot of STAPLE.FOOD",xlab = "STAPLE.FOOD")
hist(mydata1$PhysicalActivity,probability = T,
     main="BoxPlot of BP",xlab = "BP")


###############################Random.Blood.Sugar
# Data partition
#############################

set.seed(88)
split <- sample.split(mydata1, SplitRatio = 0.70)
trainData <- subset(mydata1, split == TRUE)
testData <- subset(mydata1, split == FALSE)
View(trainData)
View(testData)
str(mydata1)
str(trainData)

########################
# logistic regression
##########################

logit=glm(Diabetic~.,data=trainData,family = binomial)

summary(logit)

# Interpretation of summary(logit)

# Only Random.Blood.Sugar,HBA1C,BMI,Total.Cholesterol and BP
# toward Diabetes is significant.
# Compared to Null deviance, Residual deviance is less. This means fitting of the model with 
# independent variables is higher than fitting of the model without independent variables

# #Call:
# glm(formula = Diabetic ~ ., family = binomial, data = trainData)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.0540  -0.4184  -0.1040   0.1358   3.6744  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                            -1.284e+02  7.500e+02  -0.171  0.86403    
# Year                                    5.477e-02  3.720e-01   0.147  0.88294    
# RELIGION                               -1.430e-01  1.071e-01  -1.335  0.18196    
# GENDER                                 -6.544e-02  1.363e-01  -0.480  0.63105    
# LOCATIONHyderabad                       1.232e+00  4.166e-01   2.957  0.00310 ** 
#   LOCATIONKolkata                        -2.297e-01  4.675e-01  -0.491  0.62317    
# AGE.AT.WHICH.THE.AILMENT.WAS.DIAGNOSED  4.145e-02  6.375e-03   6.502 7.95e-11 ***
#   BP.Systolic                            -6.249e-03  5.097e-03  -1.226  0.22025    
# BP.Diastolic                            2.483e-02  7.545e-03   3.290  0.00100 ** 
#   FINANCIAL.GROUP                         4.107e-01  2.868e-01   1.432  0.15217    
# FAMILY.HISTORYDiabetic                  2.715e-01  1.407e+00   0.193  0.84697    
# FAMILY.HISTORYNO                        1.162e+00  1.313e+00   0.885  0.37637    
# FAMILY.HISTORYYES                       1.130e+00  1.372e+00   0.824  0.40993    
# STAPLE.FOOD                             7.698e-02  6.274e-02   1.227  0.21983    
# ALCOHOL                                 2.309e-02  1.204e-01   0.192  0.84785    
# Known.H.o.Smoking                      -8.615e-02  1.533e-01  -0.562  0.57424    
# Tobacco.Consumption.in.any.other.way   -9.211e-02  1.586e-01  -0.581  0.56149    
# Total.Cholesterol                       3.329e-03  1.364e-03   2.440  0.01468 *  
#   Random.Blood.Sugar                      1.090e-02  1.505e-03   7.248 4.22e-13 ***
#   HBA1C                                   1.672e+00  9.656e-02  17.315  < 2e-16 ***
#   PhysicalActivity                       -3.861e-03  2.503e-02  -0.154  0.87740    
# BMI                                     4.664e-02  1.516e-02   3.077  0.00209 ** 
#   CVC1                                    1.812e+01  3.525e+02   0.051  0.95901    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 3031.6  on 2186  degrees of freedom
# Residual deviance: 1135.8  on 2164  degrees of freedom
# AIC: 1181.8
# 
# Number of Fisher Scoring iterations: 18
#


lrtest(logit)

# 
# Likelihood ratio test
# 
# Model 1: Diabetic ~ Year + RELIGION + GENDER + LOCATION + AGE.AT.WHICH.THE.AILMENT.WAS.DIAGNOSED + 
#   BP.Systolic + BP.Diastolic + FINANCIAL.GROUP + FAMILY.HISTORY + 
#   STAPLE.FOOD + ALCOHOL + Known.H.o.Smoking + Tobacco.Consumption.in.any.other.way + 
#   Total.Cholesterol + Random.Blood.Sugar + HBA1C + PhysicalActivity + 
#   BMI + CVC
# Model 2: Diabetic ~ 1
# #Df   LogLik  Df  Chisq Pr(>Chisq)    
# 1  23  -567.88                          
# 2   1 -1515.81 -22 1895.9  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#The overall test of model significance based on the chisq test above is
#overwhelming significant indicating the likelihood of diabetes depends upon
#independent variables. Using statistical language, this implies the null hypothesis
#of all Betas are zero is rejected and we conclude that at least one Beta is nonzero


pR2(logit)

# llh       llhNull            G2      McFadden          r2ML          r2CU 
# -567.8834751 -1515.8120593  1895.8571684     0.6253602     0.5797375     0.7730071

#Interpretation:

# Based on McFadden R Square, we conclude that 62.5 percent of the uncertainty of the 
#intercept only model(model 1) has been explained by the full model(model2). Thus the goodness
# of the filt is not robust


predict= predict(logit,testData,type="response")

#confusion matrix with test data
table(testData$Diabetic, predict > 0.5)

# FALSE TRUE
# 0   446   26
# 1    77  387


confint(logit)
exp(coef(logit))
exp(confint(logit))
predict(logit,type="response")
Pred=fitted(logit)
data.frame(trainData$Diabetic,Pred)
gg1=floor(Pred+0.50)

#confusion matrix with train data
tab=table(Actual=trainData$Diabetic,Prediction=gg1)
tab

#         Prediction
# Actual    0    1
#       0 1034   70
#       1  138  945

sum(diag(tab))/sum(tab)
# [1] 0.9048925


# 
# model performance evaluation
#ROCR Curve

predict= predict(logit,type="response")
ROCRpred <- prediction(predict, trainData$Diabetic)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

# Identify best values
max=which.max(slot(ROCRperf,"y.values")[[1]])
max
#max 2115
acc= slot(ROCRperf,"y.values")[[1]][max]
#acc 1
cutoff=slot(ROCRperf,"x.values")[[1]][max]
#cutoff 0.9048925

print(c(Accuracy=acc,Cutoff=cutoff))
# Accuracy    Cutoff 
# 1.0000000 0.9990942 

abline(h=acc,v=cutoff)
abline(a=0,b=1)

# Area under the curve(AUC)
#As a rule of thumb, a model with good predictive ability should have an 
#AUC(Area under the curve) closer to 1 (1 is ideal) than to 0.5.

auc =performance(ROCRpred, measure = "auc")
auc = auc@y.values[[1]]
auc = unlist(slot(auc,"y.values"))

###################################
# Decision tree model
##################################
tree<- ctree(Diabetic~.,data=trainData,controls = ctree_control(mincriterion = 0.99,minsplit = 60))
tree
plot(tree)
treePredict<-predict(tree,testData,type="prob")

tab<-table(predict(tree),trainData$Diabetic)
print(tab)

##########################
# confusion matrix for train data
#######################
#confusion matrix with train data


#    0    1
# 0 1086  134
# 1   18  949

sum(diag(tab))/sum(tab)
###########
# accuracy
##########

# [1] 0.9304984


####################################
# Random forest
##################################\

RF <- randomForest(Diabetic ~ ., data = trainData[,-20], 
                   ntree=500, mtry = 5, nodesize = 10,
                   importance=TRUE)

print(RF)

# randomForest(formula = Diabetic ~ ., data = trainData[, -20],      ntree = 500, mtry = 5, nodesize = 10, importance = TRUE) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 5
# 
# OOB estimate of  error rate: 5.85% (Accurecy is about 94%)
# Confusion matrix:
#   0    1 class.error
# 0 1059   45  0.04076087
# 1   83 1000  0.07663897

###############################
# prediction and confusion matrix test 
################################

p1=predict(RF,data=testData[,-20])
print(p1)
tab<-table(p1,testData$Diabetic)
print(tab)
sum(diag(tab))/sum(tab)
#[1] 0.9465021
plot(RF)

tRF= tuneRF(trainData[,-19],trainData[,19],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 1000,
            trace = TRUE,
            improve = 0.05
            )


# tRF= tuneRF(trainData[,-19],trainData[,19],
#             +             stepFactor = 0.5,
#             +             plot = TRUE,
#             +             ntreeTry = 1000,
#             +             trace = TRUE,
#             +             improve = 0.05
#             +             )
# mtry = 4  OOB error = 5.53% 
# Searching left ...
# mtry = 8 	OOB error = 5.21% 
# 0.05785124 0.05 
# mtry = 16 	OOB error = 5.26% 
# -0.00877193 0.05 
# Searching right ...
# mtry = 2 	OOB error = 6.17% 
# -0.1842105 0.05 

#mtry = 8 	OOB error = 5.21%  is having less ))B error

RF1 <- randomForest(Diabetic ~ ., data = trainData[,-20], 
                   ntree=1000, mtry = 8,importance=TRUE,
                   proximity=TRUE)
print(RF1)
hist(treesize(RF),
     main="No.Of Nodes for the trees",
     col="green")
# variable importance
varImpPlot(RF1,sort = T,n.var = 10,main = "TOp 10 variable.names")

# Multi diamentional scaling plot
MDSplot(RF1,trainData$Diabetic)
