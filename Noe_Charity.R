############################################################################PREDICT 422 Practical Machine Learning

###load the data
charity <- read.csv(file.path("C:/Users/Noe/Downloads/charity.csv"),sep=",")

###data check
str(charity)
summary(charity[2:23])


###Check str
str(charity, list.len=nrow(charity))
summary(charity[11:21])

###Plot histograms on integer data
par(mfrow = c(3,4))
hist(charity$avhv, col = "mediumspringgreen",main = "avhv" , xlab = "Hist") 
hist(charity$incm, col = "mediumspringgreen",main = "incm" , xlab = "Hist") 
hist(charity$inca, col = "mediumspringgreen",main = "inca" , xlab = "Hist") 
hist(charity$plow, col = "mediumspringgreen",main = "plow" , xlab = "Hist") 
hist(charity$npro, col = "mediumspringgreen",main = "npro" , xlab = "Hist") 
hist(charity$tgif, col = "mediumspringgreen",main = "tgif" , xlab = "Hist") 
hist(charity$lgif, col = "mediumspringgreen",main = "lgif" , xlab = "Hist") 
hist(charity$rgif, col = "mediumspringgreen",main = "rgif" , xlab = "Hist") 
hist(charity$tdon, col = "mediumspringgreen",main = "tdon" , xlab = "Hist") 
hist(charity$tlag, col = "mediumspringgreen",main = "tlag" , xlab = "Hist") 
hist(charity$agif, col = "mediumspringgreen",main = "agif" , xlab = "Hist") 

###Plot boxplots on integer data
par(mfrow = c(3,4))
boxplot(charity$avhv, col = "mediumspringgreen",main = "avhv" , xlab = "Hist") 
boxplot(charity$incm, col = "mediumspringgreen",main = "incm" , xlab = "Hist") 
boxplot(charity$inca, col = "mediumspringgreen",main = "inca" , xlab = "Hist") 
boxplot(charity$plow, col = "mediumspringgreen",main = "plow" , xlab = "Hist") 
boxplot(charity$npro, col = "mediumspringgreen",main = "npro" , xlab = "Hist") 
boxplot(charity$tgif, col = "mediumspringgreen",main = "tgif" , xlab = "Hist") 
boxplot(charity$lgif, col = "mediumspringgreen",main = "lgif" , xlab = "Hist") 
boxplot(charity$rgif, col = "mediumspringgreen",main = "rgif" , xlab = "Hist") 
boxplot(charity$tdon, col = "mediumspringgreen",main = "tdon" , xlab = "Hist") 
boxplot(charity$tlag, col = "mediumspringgreen",main = "tlag" , xlab = "Hist") 
boxplot(charity$agif, col = "mediumspringgreen",main = "agif" , xlab = "Hist")

###Cap outliers at 99%
###################################avhv
q99avhv <- quantile(charity$avhv, 0.99)
q99avhv 
sum(charity$avhv > 427)
###Impute avhv
charity$avhv[charity$avhv > 427] <- 427
hist(charity$avhv)

###################################incm
q99incm <- quantile(charity$incm, 0.99)
q99incm 
sum(charity$incm > 127)
###Impute incm
charity$incm[charity$incm > 127] <- 127
hist(charity$incm)

###################################inca
q99inca <- quantile(charity$inca, 0.99)
q99inca 
sum(charity$inca > 145)
###Impute inca
charity$inca[charity$inca > 145] <- 145
hist(charity$inca)
summary(charity$inca)

###################################plow
q99plow <- quantile(charity$plow, 0.99)
q99plow 
sum(charity$plow > 58)
###Impute plow
charity$plow[charity$plow > 58] <- 58
hist(charity$plow)
summary(charity$plow)

###################################npro
q99npro <- quantile(charity$npro, 0.99)
q99npro 
sum(charity$npro > 130)
###Impute plow
charity$npro[charity$npro > 130] <- 130
hist(charity$npro)
summary(charity$npro)

###################################tgif
q99tgif <- quantile(charity$tgif, 0.99)
q99tgif 
sum(charity$tgif > 373)
###Impute plow
charity$tgif[charity$tgif > 373] <- 373
hist(charity$tgif)
summary(charity$tgif)

###################################lgif
q99lgif <- quantile(charity$lgif, 0.99)
q99lgif 
sum(charity$lgif > 147)
###Impute plow
charity$lgif[charity$lgif > 147] <- 147
hist(charity$lgif)
summary(charity$lgif)

###################################rgif
q99rgif <- quantile(charity$rgif, 0.99)
q99rgif 
sum(charity$rgif > 60)
###Impute plow
charity$rgif[charity$rgif > 60] <- 60
hist(charity$rgif)
summary(charity$rgif)

###################################tdon
q99tdon <- quantile(charity$tdon, 0.99)
q99tdon 
sum(charity$tdon > 38)
###Impute plow
charity$tdon[charity$tdon > 38] <- 38
hist(charity$tdon)
summary(charity$tdon)

###################################tlag
q99tlag <- quantile(charity$tlag, 0.99)
q99tlag 
sum(charity$tlag > 21)
###Impute plow
charity$tlag[charity$tlag > 21] <- 21
hist(charity$tlag)
summary(charity$tlag)

###################################agif
q99agif <- quantile(charity$agif, 0.99)
q99agif
sum(charity$agif > 34)
###Impute plow
charity$agif[charity$agif > 34] <- 34
hist(charity$agif)
summary(charity$agif)

str(charity)
summary(charity)

####Hist of 99% cap predictor variables
par(mfrow = c(3,4))
hist(charity$avhv,col = "mediumspringgreen",main = "avhv" , xlab = "Hist") 
hist(charity$incm,col = "mediumspringgreen",main = "incm" , xlab = "Hist") 
hist(charity$inca,col = "mediumspringgreen",main = "inca" , xlab = "Hist") 
hist(charity$plow,col = "mediumspringgreen",main = "plow" , xlab = "Hist") 
hist(charity$npro,col = "mediumspringgreen",main = "npro" , xlab = "Hist") 
hist(charity$tgif,col = "mediumspringgreen",main = "tgif" , xlab = "Hist") 
hist(charity$lgif,col = "mediumspringgreen",main = "lgif" , xlab = "Hist") 
hist(charity$rgif,col = "mediumspringgreen",main = "rgif" , xlab = "Hist") 
hist(charity$tdon,col = "mediumspringgreen",main = "tdon" , xlab = "Hist") 
hist(charity$tlag,col = "mediumspringgreen",main = "tlag" , xlab = "Hist")
hist(charity$agif,col = "mediumspringgreen",main = "agif" , xlab = "Hist") 

str(charity)

# predictor transformations
charity.t <- charity
charity.t$avhv <- log(charity.t$avhv)
charity.t$tgif <- log(charity.t$tgif)
charity.t$lgif <- log(charity.t$lgif)


summary(charity.t)
str(charity.t)

####Hist of log transformed predictor variables
par(mfrow = c(3,4))
hist(charity.t$avhv,col = "mediumspringgreen",main = "avhv" , xlab = "Hist") 
hist(charity.t$tgif,col = "mediumspringgreen",main = "tgif" , xlab = "Hist") 
hist(charity.t$lgif,col = "mediumspringgreen",main = "lgif" , xlab = "Hist") 

###Comparison
par(mfrow = c(1,4))
hist(charity$avhv, col = "mediumspringgreen",main = "avhv" , xlab = "Hist")
hist(charity.t$avhv, col = "mediumspringgreen",main = "log_avhv" , xlab = "Hist")
hist(charity$avhv, col = "mediumspringgreen",main = "tgif" , xlab = "Hist")
hist(charity.t$tgif, col = "mediumspringgreen",main = "log_tgif" , xlab = "Hist")

dev.off()

####Correlation Table
cols <- colnames(charity.t[, sapply(charity.t, is.numeric)])
df_temp <- t(cor(charity.t["damt"], charity.t[cols], use="complete"))
colnames(df_temp) <- "Corr"
round(df_temp, digits=3)

####Correlation Table
cols <- colnames(charity.t[, sapply(charity.t, is.numeric)])
df_temp <- t(cor(charity.t["damt"], charity.t[cols], use="complete"))
colnames(df_temp) <- "Corr"
round(df_temp, digits=3)

######################################################################set up data for analysis
##############################################################################Training Dataset
data.train <- charity.t[charity$part=="train",]
dim(data.train)#3984 #24
###Break data into independent variables
x.train <- data.train[,2:21]
dim(x.train)#3984 #20
###Seperate DONR
c.train <- data.train[,22] # donr
n.train.c <- length(c.train) # 3984
n.train.c
###New response variable_t
y.train <- data.train[c.train==1,23]# damt for observations with donr=1
y.train
n.train.y <- length(y.train) # 1995
n.train.y 

#################################################################################Validation data
data.valid <- charity.t[charity$part=="valid",]
x.valid <- data.valid[,2:21]
c.valid <- data.valid[,22] # donr
n.valid.c <- length(c.valid) # 2018
y.valid <- data.valid[c.valid==1,23] # damt for observations with donr=1
n.valid.y <- length(y.valid) # 999
###Test
data.test <- charity.t[charity$part=="test",]
n.test <- dim(data.test)[1] # 2007
x.test <- data.test[,2:21]

x.train.mean <- apply(x.train, 2, mean)
x.train.sd <- apply(x.train, 2, sd)
x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) # standardize to have zero mean and unit sd
apply(x.train.std, 2, mean) # check zero mean
apply(x.train.std, 2, sd) # check unit sd
data.train.std.c <- data.frame(x.train.std, donr=c.train) # to classify donr
data.train.std.y <- data.frame(x.train.std[c.train==1,], damt=y.train) # to predict damt when donr=1

x.valid.std <- t((t(x.valid)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.valid.std.c <- data.frame(x.valid.std, donr=c.valid) # to classify donr
data.valid.std.y <- data.frame(x.valid.std[c.valid==1,], damt=y.valid) # to predict damt when donr=1

x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.test.std <- data.frame(x.test.std)

#######################################################################################Frequency tables using train
###Frequency table Donor x Home
table(data.train$home, data.train$donr)
###Frequency table Donor x Children
table(data.train$chld, data.train$donr)
###Frequency table Donor x HINC
table(data.train$hinc, data.train$donr)
###Frequency table Donor x HINC
table(data.train$wrat, data.train$donr)
###Frequency table Donor x Gender
table(data.train$genf, data.train$donr)
###Frequency table Donor x Reg1
table(data.train$reg1, data.train$donr)
###Frequency table Donor x Reg2
table(data.train$reg2, data.train$donr)
###Frequency table Donor x Reg3
table(data.train$reg3, data.train$donr)
###Frequency table Donor x Reg4
table(data.train$reg4, data.train$donr)
##################################################################################### Part 2: Classification model
# Load required libraries.
require(MASS)
require(ISLR)
require(class)

####################################################################logistic regression check variable significance
m.glm1 <- glm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc 
                  + I(hinc^2) + genf + wrat + avhv + incm + inca + plow + npro + tgif 
                  + lgif + rgif + tdon + tlag + agif, data.train.std.c, family=binomial("logit"))
summary(m.glm1)
#########################################################Logistic only variables listed as statistically significant 
### Error     = 0.1177209
### Pred      = 1794/1995 
m.glm3 <- glm(donr ~ reg1 + reg2 + home*chld + I(hinc^2) + wrat
                  + incm * tgif + tdon + tlag, data.train.std.c,
                  family=binomial("logit"))
summary(m.glm3)

###predictions for training 
m.glm3prob <- predict(m.glm3, type = "response")
m.glm3pred = rep("0", 3984)
m.glm3pred[m.glm3prob > .5] = "1"
table(m.glm3pred, c.train)
m.glm3Error.train3 <- mean(m.glm3pred != c.train)
m.glm3Error.train3

##################################################################################################################
###Model validation 
###Run m.glm3 on validation set.
### Error     = 0.12140
### Pred      = 893/999 
m.glm3.valid <- glm(donr ~ reg1 + reg2 + home*chld + hinc + I(hinc^2) +
                          wrat + incm*plow + npro + tdon + tlag,
                        data.valid.std.c, family = binomial("logit"))
summary(m.glm3.valid)

###predictions for validation 
m.glm3prob.val <- predict(m.glm3.valid, type = "response")
m.glm3pred.val = rep("0", 2018)
m.glm3pred.val[m.glm3prob.val > .5] = "1"
table(m.glm3pred.val, c.valid)
m.glm3Error.val <- mean(m.glm3pred.val != c.valid)
m.glm3Error.val

#####################################################################################   ##########GAM page 294
require(gam)
### Error     = 0.1576305
### Pred      = 1719/1995 
m.gam1 <- gam(I(donr == 1) ~ reg1 + reg2 + reg3 + reg4 + home
                  + chld + hinc + genf + wrat + avhv + incm + inca + plow + npro
                  + tgif + lgif + rgif + tdon + tlag + agif, family = binomial,
                  data = data.train.std.c)
summary(m.gam1)

###confusion matrix 
m.gam1pred.1 <- rep("0", 3984)
m.gam1pred.1[m.gam1$fitted.values > .5] = "1"
table(m.gam1pred.1, c.train)
mean(m.gam1pred.1 != c.train)
##################################################################################################################
require(splines)
###Include natural spline for tlag. Include only statistically significant variables
### Error     = 0.1591365
### Pred      = 1713/1995 
m.gam2 <- gam(I(donr==1) ~ reg2 + home*chld + wrat + avhv + incm +
                    npro + tgif + tdon + bs(tlag, knots = c(2, 7, 11)),
                  family = binomial, data = data.train.std.c)
summary(m.gam2)

###confusion matrix 
m.gam2pred.2 <- rep("0", 3984)
m.gam2pred.2[m.gam2$fitted.values > .5] = "1"
table(m.gam2pred.2, c.train)
mean(m.gam2pred.2 != c.train)
#################################################################################################################
###m.gam1 on validation set.
### Error       = 0.1640238
### Pred        = 849/999 donors
m.gam1.valid <- gam(I(donr == 1) ~ reg1 + reg2 + reg3 + reg4 + home
                        + chld + hinc + genf + wrat + avhv + incm + inca
                        + plow + npro + tgif + lgif + rgif + tdon + tlag
                        + agif, family = binomial, data = data.valid.std.c)
summary(m.gam1.valid)

###confusion matrix
m.gam1pred.val.1 <- rep("0", 2018)
m.gam1pred.val.1[m.gam1.valid$fitted.values > .5] = "1"
table(m.gam1pred.val.1, c.valid)
m.gam1Error.val <- mean(m.gam1pred.val.1 != c.valid)
m.gam1Error.val

#############################################################LDA (Linear discriminite analysis) from example code 422)
### Error       = 0.124749
### Pred        = 1855/1995 donors
m.lda1 <- lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c) # include additional terms on the fly using I()

m.lda1

###confusion matrix.
m.lda1fit.pred <- predict(m.lda1, data.train.std.c)
m.lda1fit.pred.class <- m.lda1fit.pred$class
table(m.lda1fit.pred.class, c.train)
mean(m.lda1fit.pred.class != c.train)
m.lda1fit.pred.posterior <- m.lda1$posterior[,2]

#################################################################################################################
### LDA (Linear discriminite analysis) reduced variables
### Error       = 0.1300201
### Pred        = 1827/1995 donors
m.lda3 <- lda(donr ~ reg1 + reg2 + home*chld + hinc + I(hinc^2) +
                  wrat + incm*plow + npro + tdon + tlag, data = data.train.std.c)
m.lda3

###confusion matrix.
m.lda3fit.pred <- predict(m.lda3, data.train.std.c)
m.lda3fit.pred.class <- m.lda3fit.pred$class
table(m.lda3fit.pred.class, c.train)
mean(m.lda3fit.pred.class != c.train)
m.lda2fit.pred.posterior <- m.lda3$posterior[,2]
##################################################################################################################
###Model.ldal validation Sample code
### Error       = 0.1333003
### Pred        = 921/999 donors
m.lda1.valid <- lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                data = data.valid.std.c) # include additional terms on the fly using I()

m.lda1.valid

###confusion matrix.
m.lda1fit.valid.pred <- predict(m.lda1.valid, data.valid.std.c)
m.lda1fit.valid.pred.class <- m.lda1fit.valid.pred$class
table(m.lda1fit.valid.pred.class, c.valid)
m.lda1Error.val <- mean(m.lda1fit.valid.pred.class != c.valid)
m.lda1Error.val

m.lda1fit.valid.pred.post <- m.lda1fit.valid.pred$posterior
###############################################################################################################
###Mode3.ldal validation reduced variables
### Error       = 0.135778
### Pred        = 906/999 donors
m.lda3.valid <- lda(donr ~ reg1 + reg2 + home*chld + hinc + I(hinc^2) +
                wrat + incm*plow + npro + tdon + tlag, data = data.valid.std.c)

m.lda3.valid

###confusion matrix.
m.lda3fit.valid.pred <- predict(m.lda3.valid, data.valid.std.c)
m.lda3fit.valid.pred.class <- m.lda3fit.valid.pred$class
table(m.lda3fit.valid.pred.class, c.valid)
m.lda3Error <- mean(m.lda3fit.valid.pred.class != c.valid)
m.lda3Error
m.lda3fit.valid.pred.post <- m.lda3fit.valid.pred$posterior
########################################################################################################QDA
###QDA all variables
### Error       = 0.1573795
### Pred        = 1772/1995 donors
m.qda1 <- qda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc 
                + genf + wrat + avhv + incm + inca + plow + npro
                + tgif + lgif + rgif + tdon + tlag + agif,
                data = data.train.std.c)
m.qda1

###confusion matrix.
m.qda1.fit.pred <- predict(m.qda1, data.train.std.c)
m.qda1.fit.pred.class <- m.qda1.fit.pred$class
table(m.qda1.fit.pred.class, c.train)
mean(m.qda1.fit.pred.class != c.train)
m.qda1.fit.pred.posterior <- m.qda1$posterior[,2]


##################################################################################################################
###QDA m.qda1 validation all variables
### Error       = 0.1506442
### Pred        = 930/999 donors
m.qda1.valid <- qda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc 
                      + genf + wrat + avhv + incm + inca + plow + npro
                      + tgif + lgif + rgif + tdon + tlag + agif,
                      data = data.valid.std.c)

###confusion matrix.
m.qda1.valid.pred <- predict(m.qda1.valid, data.valid.std.c)
m.qda1.valid.pred.class <- m.qda1.valid.pred$class
table(m.qda1.valid.pred.class, c.valid)
m.qdaError1.val <- mean(m.qda1.valid.pred.class != c.valid)
m.qdaError1.val

m.qda1.valid.pred.post <- m.qda1.valid.pred$posterior


#######################################Perform K-nearest neighbors classification and test on validation set.
require(class)
require(caret)
train.charity.X <- cbind(data.train.std.c$reg1, data.train.std.c$reg2,
                         data.train.std.c$reg3, data.train.std.c$reg4,
                         data.train.std.c$home, data.train.std.c$chld,
                         data.train.std.c$hinc, data.train.std.c$genf, 
                         data.train.std.c$wrat, data.train.std.c$avhv, 
                         data.train.std.c$incm, data.train.std.c$inca, 
                         data.train.std.c$plow, data.train.std.c$npro, 
                         data.train.std.c$tgif, data.train.std.c$lgif, 
                         data.train.std.c$rgif, data.train.std.c$tdon, 
                         data.train.std.c$tlag, data.train.std.c$agif)
test.charity.X <- cbind(data.valid.std.c$reg1, data.valid.std.c$reg2, 
                        data.valid.std.c$reg3, data.valid.std.c$reg4, 
                        data.valid.std.c$home, data.valid.std.c$chld, 
                        data.valid.std.c$hinc, data.valid.std.c$genf, 
                        data.valid.std.c$wrat, data.valid.std.c$avhv, 
                        data.valid.std.c$incm, data.valid.std.c$inca, 
                        data.valid.std.c$plow, data.valid.std.c$npro, 
                        data.valid.std.c$tgif, data.valid.std.c$lgif, 
                        data.valid.std.c$rgif, data.valid.std.c$tdon,
                        data.valid.std.c$tlag, data.valid.std.c$agif)

######################################################################################################kNN k = 1
train.donr = data.train.std.c$donr
knnpred=knn(train.charity.X,test.charity.X,train.donr,k=1)

###KNN table
table(knnpred, c.valid)

###Check correct percentage
(738 + 836)/2018 
###correct = .78

###Check incorrect percentage
(281 + 163)/2018 
###incorrect = .220

###error
knnError <- mean(knnpred != c.valid)
knnError
###error = .22

########################################################################################################KNN k = 3
knnpred3=knn(train.charity.X,test.charity.X,train.donr,k=3)

###KNN table
table(knnpred3, c.valid)

###Check correct percentage
(733 + 894)/2018 
###correct = .809

###Check incorrect percentage
(286 + 105)/2018 
###incorrect = .191

###error
knnError3 <- mean(knnpred3 != c.valid)
knnError3
###error = .193

######################################################################################################KNN k = 5
knnpred5=knn(train.charity.X,test.charity.X,train.donr,k=5)

###KNN table
table(knnpred5, c.valid)

###Check correct percentage
(721 + 908)/2018 
###correct = .807

###Check incorrect percentage
(298 + 91)/2018 
###incorrect = .182

###error
knnError5 <- mean(knnpred5 != c.valid)
knnError5
###error = .1927

############################################################################################decision tree Chapter 8
require(tree)
###Create dataset
dtree.train = data.train.std.c
dtree.train$donr = as.factor(dtree.train$donr)
dtree.valid = data.valid.std.c
dtree.valid$donr = as.factor(dtree.valid$donr)
# Fit tree and test.
dt.donr = tree(donr ~., dtree.train)
plot(dt.donr)
text(dt.donr, pretty=0)
tree.pred = predict(dt.donr, dtree.valid, type = "class")
table(tree.pred, c.valid)

###Check correct percentage
(783 + 929)/2018 
###correct = .848

###Check incorrect percentage
(236 + 70)/2018 
###incorrect = .152

###Error 
treeError <- mean(tree.pred != c.valid)
treeError
###Error 0.1516353
##################################################################################################################
###cross validation 
cv.dt.donr <- cv.tree(dt.donr, FUN=prune.misclass)
plot(cv.dt.donr$size, cv.dt.donr$dev, type = "b")
plot(cv.dt.donr$k, cv.dt.donr$dev, type = "b")

####Biggest reduction at 5 lower at 9 and lowest at 15/16.
prune.tree = prune.misclass(dt.donr, best=5)
tree.pred2 = predict(prune.tree, dtree.valid, type="class")
table(tree.pred2, c.valid)

###Check correct percentage
(794 + 853)/2018 
###correct = .816

###Check incorrect percentage
(225 + 146)/2018 
###incorrect = .183

###Error2 
treeError2 <- mean(tree.pred2 != c.valid)
treeError2
###Error 0.1838454

####Reduction at 9
prune.tree = prune.misclass(dt.donr, best=9)
tree.pred3 = predict(prune.tree, dtree.valid, type="class")
table(tree.pred3, c.valid)

###Check correct percentage
(882 + 825)/2018 
###correct = .846

###Check incorrect percentage
(137 + 174)/2018 
###incorrect = .154

###Error3 
treeError3 <- mean(tree.pred3 != c.valid)
treeError3
###Error 0.154113


####Reduction at 15
prune.tree = prune.misclass(dt.donr, best=15)
tree.pred4 = predict(prune.tree, dtree.valid, type="class")
table(tree.pred4, c.valid)

###Check correct percentage
(783 + 929)/2018 
###correct = .848

###Check incorrect percentage
(236 + 70)/2018 
###incorrect = .152

###Error4 
treeError4 <- mean(tree.pred4 != c.valid)
treeError4
###Error 0.1838454

##########################################################################################Random Forrest Chapter 8
require(randomForest)
set.seed(1) 
bagdonr = randomForest(donr ~ ., data=dtree.train, mtry=20, 
                        importance=TRUE, type="classification")
importance(bagdonr)
set.seed(1)
bagpred = predict(bagdonr, newdata=dtree.valid)

table(bagpred, c.valid)

###Check correct percentage
(904)/999 
###correct = .905

###Error Bag
bagError1 = mean(bagpred != c.valid)
bagError1
###Error = .1114965

###Var importance
importance(bagdonr)

###Plot
varImpPlot(bagdonr)

#####################################################################################################Boost Chapter 8
require(gbm)
set.seed(1)
boostdonr = gbm(donr ~ ., data = data.train.std.c, distribution = "bernoulli",
                 n.trees = 5000, interaction.depth = 4)
set.seed(1)
boost.prob = predict.gbm(boostdonr, newdata = data.valid.std.c,
                          n.trees = 5000, type = "response")
boost.pred = rep("0", 2018)
boost.pred[boost.prob > .5] = "1"

####Table
table(boost.pred , c.valid)

###Check correct percentage
(923)/999 
###correct = .94

###Error Boost
boostError1 <- mean(boost.pred != c.valid)
boostError1
###Error = .1080275

#################################################################################Support Vector Machine Chapter 9
require(e1071)

###cross validation with tune
set.seed(1)
tune.out = tune(svm, donr ~., data = dtree.train, kernel = "linear",
                ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10)))
summary(tune.out)

###Best cost = 5
set.seed(1)
svm.fit1 = svm(donr ~., data = dtree.train, kernel = "linear",
               cost = 5, scale = FALSE)
###Pred
bestmod = tune.out$best.model
summary(bestmod)
set.seed(1)

###Table
svm.pred = predict(bestmod, dtree.valid)
table(svm.pred, c.valid)

###Check correct percentage
(866)/999 
###correct = .86

###Error SVM
svmError1 <- mean(svm.pred != c.valid)
svmError1
###Error 0.1635282

##################################################################################################Model Summary
Error <- c(m.glm3Error.val, m.gam1Error.val, m.lda1Error.val, m.qdaError1.val,
           knnError5, treeError3, bagError1, boostError1, svmError1)


Model = c("Logistic", "GAM", "LDA", "QDA", "KNN", "DTree", "Bag", "Boost", "SVM")
ErrorTable <- as.data.frame(cbind(Model, Error))
View(ErrorTable)


############################################################################################################Cutoff
##################################################################################################log model cutoff
cutoff.log <- sort(m.glm3prob.val, decreasing = T)[n.mail.valid + 1]
chat.valid.log <- ifelse(m.glm3prob.val > cutoff.log, 1, 0)
table(chat.valid.log, c.valid)
###316 + 983 = 1299 
###(14.5 * 983) - (2 * 1299) = 11,655.5
logdonors <- (316 + 983)
logdonors

logprofit <- (14.5 * 983) - (2 * 1299)
logprofit

#################################################################################################GAM model cutoff
cutoff.gam <- sort(m.gam1.valid$fitted.values, decreasing = T)[n.mail.gam + 1]
chat.valid.gam <- ifelse(m.gam1.valid$fitted.values > cutoff.gam, 1, 0)
table(chat.valid.gam, c.valid)
###444 + 986 
##(14.5 * 986) - (2 * 1430) = 11,437
gamdonors <- (444 + 986)
gamdonors

gamprofit <- (14.5 * 986) - (2 * 1430)
gamprofit

##############################################################################################################LDA
cutoff.lda <- sort(m.lda1fit.valid.pred.post[,2], decreasing = T)[n.mail.lda + 1]
chat.valid.lda <- ifelse(m.lda1fit.valid.pred.post[,2] > cutoff.lda, 1, 0)
table(chat.valid.lda, c.valid)

###339 + 987 = 1326 
###(14.5 * 987) - (2 * 1326) = 11,665.5
ldadonors <- (339 + 987)
ldadonors

ldaprofit <- (14.5 * 987) - (2 * 1326)
ldaprofit

###############################################################################################################QDA
cutoff.qda <- sort(m.qda1.valid.pred.post[,2], decreasing = T)[n.mail.lda + 1]
chat.valid.qda <- ifelse(m.qda1.valid.pred.post[,2] > cutoff.lda, 1, 0)
table(chat.valid.qda, c.valid)

###320 + 969 = 1289 
###(14.5 * 969) - (2 * 1289) = 11,472.50
qdadonors <- (320 + 969)
qdadonors

qdaprofit <- (14.5 * 969) - (2 * 1289)
qdaprofit

#############################################################################################################boost
cutoff.boost <- sort(boost.prob, decreasing = T)[n.mail.boost + 1]
chat.valid.boost <- ifelse(boost.prob > cutoff.boost, 1, 0)
table(chat.valid.boost, c.valid)

###282 + 992 = 1274 
###(14.5 * 992) - (2*1274) = 11,836

boostdonors <- 282 + 992
boostdonors

boostprofit <- (14.5 * 992) - (2*1274)
boostprofit 

############################################################################################################KNN5
table(knnpred5, c.valid)

knndonors <- 298 + 908
knndonors

knnprofit <- (14.5 * 908) - (2 * 1206)
knnprofit

############################################################################################################Tree
table(tree.pred3, c.valid)

treedonors <- (137 + 825)
treedonors

treeprofit <- (14.5 * 825) - (2 * 962)
treeprofit

############################################################################################################Tree
table(boost.pred , c.valid)

bagdonors <- (142 + 923)
bagdonors

bagprofit <- (14.5 * 923) - (2 * 1065)
bagprofit

############################################################################################################Tree
table(svm.pred, c.valid)

svmdonors <- (191 + 866)
svmdonors

svmprofit <- (14.5 * 866) - (2 * 1057)
svmprofit

#######################################################################################################Profitcomp
donors <- c(logdonors,gamdonors,ldadonors,qdadonors,boostdonors,knndonors,treedonors,bagdonors,svmdonors)

profits <- c(logprofit,gamprofit,ldaprofit,qdaprofit,boostprofit,knnprofit,treeprofit,bagprofit,svmprofit)

Model = c("Logistic Model", "GAM Model", "LDA Model", "QDA Model", "Boosting","KNN", "Decision Tree", "Baggimg", "SVM")

profittable <- as.data.frame(cbind(Model, donors, profits))

View(profittable)



###Boosted model comes out on top
set.seed(1)
post.test = predict.gbm(boostdonr, newdata = data.test.std,
                        n.trees = 5000, type = "response")

###Oversampling adjustment for calculating number of mailings for test set
n.mail.valid = which.max(profit.boost)

tr.rate <- .1 

vr.rate <- .5 

n.valid.c <- length(c.valid)
adj.test.1 <- (n.mail.valid/n.valid.c)/(vr.rate/tr.rate)
adj.test.0 <- ((n.valid.c-n.mail.valid)/n.valid.c)/((1-vr.rate)/(1-tr.rate))
adj.test <- adj.test.1/(adj.test.1+adj.test.0) 

###Calculate mailings for test set
n.mail.test <- round(n.test*adj.test, 0) ###321
n.mail.test

###Set cutoff.
cutoff.test <- sort(post.test, decreasing=T)[n.mail.test+1] # set cutoff based on n.mail.test
chat.test <- ifelse(post.test>cutoff.test, 1, 0) # mail to everyone above the cutoff
table(chat.test)

###########################################################################################Predition Model for DAMT
###################################################################################################################
#############################################################################Linear model with all variables Source
m.lin1 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                   avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                 data.train.std.y)
summary(m.lin1)


###Run model on validation
m.lin1.valid <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                     avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,
                       data.valid.std.y)

summary(m.lin1.valid)

###predictions for validation
pred.valid.m.lin1 <- predict(m.lin1, data.valid.std.y)
mpe1 <- mean((y.valid - pred.valid.m.lin1)^2)
mpe1
stderror1 <- sd((y.valid - pred.valid.m.lin1)^2)/sqrt(n.valid.y)
stderror1
#####################################################################Linear model with only significant variables
m.lin2 <- lm(damt ~ reg2 + reg3 + reg4 + home + chld + hinc + incm + plow + lgif + rgif + agif, 
             data.train.std.y)

summary(m.lin2)


###Run model on validation 
m.lin2.valid <- lm(damt ~ reg2 + reg3 + reg4 + home + chld + hinc + incm + plow + lgif + rgif + agif,
                   data.valid.std.y)
summary(m.lin2.valid)

###predictions for validation 
pred.valid.m.lin2 <- predict(m.lin2, data.valid.std.y)
mpe2 <- mean((y.valid - pred.valid.m.lin2)^2)
mpe2
stderror2 <- sd((y.valid - pred.valid.m.lin2)^2)/sqrt(n.valid.y)
stderror2

#####################################################################Linear model with Forward Selection page 247
require(leaps)
regfit.fwd=regsubsets(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + wrat + genf + avhv + incm + inca 
                      + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, data.train.std.y, nvmax = 20)

reg.summary = summary(regfit.fwd)
names(reg.summary)
which.min(reg.summary$bic)
plot(regfit.full, scale = "bic")

coef(regfit.fwd,11)
####################################################################################Linear model with lowest BIC
m.lin3 <- lm(damt ~ reg3 + reg4 + home + chld + hinc + incm + plow + tgif + lgif + rgif + agif, data.train.std.y)

summary(m.lin3)

###Run model on validation 
m.lin3.valid <- lm(damt ~ reg3 + reg4 + home + chld + hinc + incm + plow + tgif + lgif + rgif + agif,
                   data.valid.std.y)

summary(m.lin3.valid)

###predictions for validation 
pred.valid.m.lin3 <- predict(m.lin3, data.valid.std.y)
mpe3 <- mean((y.valid - pred.valid.m.lin3)^2)
mpe3
stderror3 <- sd((y.valid - pred.valid.m.lin3)^2)/sqrt(n.valid.y)
stderror3

######################################################################################Ridge regression page 251
require(glmnet)
set.seed(1)
grid = 10^seq(10, -2, length=100)
matrix.train <- data.matrix(data.train.std.y)
matrix.train <- matrix.train[,-21]

m.ridge = glmnet(matrix.train, y.train, alpha=0, lambda=grid,thresh=1e-12)

###choose lambda.
set.seed(1)
cv.out = cv.glmnet(matrix.train, y.train, alpha=0)
plot(cv.out)
lamda = cv.out$lambda.min
lamda

###predictions for validation 
matrix.val = as.matrix(data.valid.std.y)
matrix.val <- matrix.val[,-21]
set.seed(1)
m.ridge.pred = predict(m.ridge, s=lamda, newx=matrix.val)

mpe4 <- mean((y.valid - m.ridge.pred)^2)
mpe4
stderror4 <- sd((y.valid - m.ridge.pred)^2)/sqrt(n.valid.y)
stderror4
############################################################################################The lasso Chapter 6
set.seed(1)
###Use matrices and lambda grid created for ridge regression.
m.lasso = glmnet(matrix.train, y.train, alpha=1, lambda=grid)

###select lambda.
set.seed(1)
cv.lasso = cv.glmnet(matrix.train, y.train, alpha=1)
plot(cv.lasso)
lambdalasso = cv.lasso$lambda.min
lambdalasso

set.seed(1)
lasso.pred = predict(m.lasso, s=lambdalasso, newx=matrix.val)

mpe5 <- mean((y.valid - lasso.pred)^2)
mpe5
stderror5 <- sd((y.valid - lasso.pred)^2)/sqrt(n.valid.y)
stderror5
##########################################################################################Decision tree page 324
set.seed(1)# Fit tree and test.
tree.damt = tree(damt ~., data.train.std.y)
plot(tree.damt)
text(tree.damt, pretty=0)
summary(tree.damt)

set.seed(1)
tree.pred = predict(tree.damt, data.valid.std.y)

mpe6 <- mean((y.valid - tree.pred)^2)
mpe6
stderror6 <- sd((y.valid - tree.pred)^2)/sqrt(n.valid.y)
stderror6

#################################################################################################Bagging page 329
require(randomForest)
set.seed(1)
bag.damt = randomForest(damt ~., data = data.train.std.y, mtry=20,importance=TRUE)
bag.damt
set.seed(1)
bag.pred <- predict(bag.damt, newdata=data.valid.std.y)

mpe7 <- mean((y.valid - bag.pred)^2)
mpe7
stderror7 <- sd((y.valid - bag.pred)^2/sqrt(n.valid.y))
stderror7

####################################################################################################Random forest
set.seed(1)
rf.damt = randomForest(damt ~., data=data.train.std.y, importance=TRUE)
rf.damt

set.seed(1)
rf.pred <- predict(rf.damt, newdata = data.valid.std.y)

mpe8 <- mean((y.valid - rf.pred)^2)
mpe8
stderror8 <- sd((y.valid - rf.pred)^2/sqrt(n.valid.y))
stderror8

##########################################################################################################Boosting
set.seed(1)
boost.damt = gbm(damt ~., data = data.train.std.y, distribution = "gaussian",
                 n.trees=5000, interaction.depth=4)
summary(boost.damt)

set.seed(1)
boost.pred <- predict(boost.damt, newdata = data.valid.std.y, n.trees = 5000)

mpe9 <- mean((y.valid - boost.pred)^2)
mpe9
stderror9 <- sd((y.valid - boost.pred)^2/sqrt(n.valid.y))
stderror9

#######################################################################################################damtcomp
Models <- c("OLS Regression", "OLS Best Variable Selection", "Forward Selection", "Ridge Regression", 
            "Lasso Regression","Decision Tree","Bagging", "Random Forest", "Boosting")

ModelMPE <- c(mpe1, mpe2, mpe3, mpe4, mpe5, mpe6, mpe7, mpe8, mpe9)

ModelSE <- c(stderror1, stderror2, stderror3,stderror4, stderror5,stderror6,
                stderror7, stderror8, stderror9)

comp <- as.data.frame(cbind(Models, ModelMPE, ModelSE))

comp[order(comp$ModelMPE),]

View(comp[order(comp$ModelMPE),])

####Create predictions for test set.
set.seed(1)
yhat.test <- predict(boost.damt, data.test.std, n.trees = 5000)

###Final results

###Check lengths of "hat" vectors. Both are 2007.
length(chat.test)
length(yhat.test)
###Check that content of "hat" vectors is plausible for goals.
chat.test[1:10] 
yhat.test[1:10]
###View summary of final predictions.
table(chat.test)
summary(yhat.test)

###Create dataframe with two columns,one for the classification and one for the dollar amount.
final <- data.frame(chat = chat.test, yhat = yhat.test)

###Check expected profit from mailing.
PredictedDonors <- subset(final, chat==1)
FinalProfit <- sum(PredictedDonors$yhat)
FinalProfit #4657.43

###Check for missing values.
sum(is.na(final))

###Save to CSV.
write.csv(final, "NoeFlores_Pred_422_56.csv", row.names = FALSE)














