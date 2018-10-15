##################################################################################Start
require(ggplot2)
require(forecast)
require(psych)
require(readr)
require(plyr)
require(dplyr)
require(corrplot)
require(outliers)
require(caret)
require(skimr)
require(RColorBrewer)
require(knitr)
require(bestglm)
require(tree)
require(rpart)
require(rattle)	
require(rpart.plot)			
require(RColorBrewer)				
require(party)					
require(partykit)			
### Define path and file;
my.path <- 'C:\\Users\\Noe\\Downloads\\';

### copy to file
my.file <- paste(my.path,'credit_card_default.RData',sep='');

### Read the RData object using readRDS();
credit_card_default <- readRDS(my.file)

### Show dataframe structure;
str(credit_card_default)

### Here is the observation count in each data set;
table(credit_card_default$data.group)

###Remap datagroup
credit_card_default$data.group <- mapvalues(credit_card_default$data.group, 
                                            from=c("1","2","3"), 
                                            to=  c("train","test","validate"))

### Check table in each data set;
table(credit_card_default$data.group)

### Show top of data frame with some values;
head(credit_card_default)

###Summary Stats and output
summary(credit_card_default[2:24])

skimr::skim(credit_card_default[2:24])

###rename Data file for simplicity
names(credit_card_default)[names(credit_card_default) == 'LIMIT_BAL'] <- 'c.limit'
names(credit_card_default)[names(credit_card_default) == 'SEX'] <- 'sex'
names(credit_card_default)[names(credit_card_default) == 'EDUCATION'] <- 'edu'
names(credit_card_default)[names(credit_card_default) == 'MARRIAGE'] <- 'mar.s'
names(credit_card_default)[names(credit_card_default) == 'AGE'] <- 'age'
names(credit_card_default)[names(credit_card_default) == 'PAY_0'] <- 'pay.1'
names(credit_card_default)[names(credit_card_default) == 'PAY_2'] <- 'pay.2'
names(credit_card_default)[names(credit_card_default) == 'PAY_3'] <- 'pay.3'
names(credit_card_default)[names(credit_card_default) == 'PAY_4'] <- 'pay.4'
names(credit_card_default)[names(credit_card_default) == 'PAY_5'] <- 'pay.5'
names(credit_card_default)[names(credit_card_default) == 'PAY_6'] <- 'pay.6'
names(credit_card_default)[names(credit_card_default) == 'BILL_AMT1'] <- 'bill.amt1'
names(credit_card_default)[names(credit_card_default) == 'BILL_AMT2'] <- 'bill.amt2'
names(credit_card_default)[names(credit_card_default) == 'BILL_AMT3'] <- 'bill.amt3'
names(credit_card_default)[names(credit_card_default) == 'BILL_AMT4'] <- 'bill.amt4'
names(credit_card_default)[names(credit_card_default) == 'BILL_AMT5'] <- 'bill.amt5'
names(credit_card_default)[names(credit_card_default) == 'BILL_AMT6'] <- 'bill.amt6'
names(credit_card_default)[names(credit_card_default) == 'PAY_AMT1'] <- 'pay.amt1'
names(credit_card_default)[names(credit_card_default) == 'PAY_AMT2'] <- 'pay.amt2'
names(credit_card_default)[names(credit_card_default) == 'PAY_AMT3'] <- 'pay.amt3'
names(credit_card_default)[names(credit_card_default) == 'PAY_AMT4'] <- 'pay.amt4'
names(credit_card_default)[names(credit_card_default) == 'PAY_AMT5'] <- 'pay.amt5'
names(credit_card_default)[names(credit_card_default) == 'PAY_AMT6'] <- 'pay.amt6'
names(credit_card_default)[names(credit_card_default) == 'DEFAULT'] <- 'default'

###check structire
str(credit_card_default)

###Check Variable
summary(credit_card_default[2:25])

###Hist boxplots on mar.s and edu
par(mfrow = c(2,2))
hist(credit_card_default$mar.s, col = "cyan",main = "mar.s" , xlab = "Hist") 
hist(credit_card_default$edu, col = "cyan",main = "edu" , xlab = "Hist") 

###edu Outliers
sum(credit_card_default$edu > 4)

sum(credit_card_default$edu < 1)


###Impute edu
credit_card_default$edu[credit_card_default$edu > 4] <- 4

credit_card_default$edu[credit_card_default$edu < 1] <- 4


###mar.s Outliers
sum(credit_card_default$mar.s < 1)

###Impute mar.s
credit_card_default$mar.s[credit_card_default$mar.s < 1] <- 3

###Hist boxplots on mar.s and edu
par(mfrow = c(2,2))
hist(credit_card_default$mar.s, col = "cyan",main = "mar.s" , xlab = "Hist") 
hist(credit_card_default$edu, col = "cyan",main = "edu" , xlab = "Hist") 


dev.off()

###Hist boxplots on PAY_0 to pay.6
par(mfrow = c(2,3))
hist(credit_card_default$pay.1, col = "cyan",main = "pay.1" , xlab = "Hist") 
hist(credit_card_default$pay.2, col = "cyan",main = "pay.2" , xlab = "Hist") 
hist(credit_card_default$pay.3, col = "cyan",main = "pay.3" , xlab = "Hist") 
hist(credit_card_default$pay.4, col = "cyan",main = "pay.4" , xlab = "Hist") 
hist(credit_card_default$pay.5, col = "cyan",main = "pay.5" , xlab = "Hist") 
hist(credit_card_default$pay.6, col = "cyan",main = "pay.6" , xlab = "Hist") 

###Frequency Table for pay.1
table_pay.1 = table(credit_card_default$pay.1)
table_pay.1

###Frequency Table for pay.2
table_pay.2 = table(credit_card_default$pay.2)
table_pay.2

###Frequency Table for pay.3
table_pay.3 = table(credit_card_default$pay.3)
table_pay.3

###Frequency Table for pay.4
table_pay.4 = table(credit_card_default$pay.4)
table_pay.4

###Frequency Table for pay.5
table_pay.5 = table(credit_card_default$pay.5)
table_pay.5

###Frequency Table for pay.6
table_pay.6 = table(credit_card_default$pay.6)
table_pay.6

####################################################################################################Remap pay.1
credit_card_default$pay.1 <- mapvalues(credit_card_default$pay.1, 
                                       from=c("-2","-1","0","1","2","3","4","5","6","7","8"), 
                                       to=  c("-2","-1","0","1","2","3","4","5","6","7","7"))

table_pay.1 = table(credit_card_default$pay.1)
table_pay.1

###Remap pay.2
credit_card_default$pay.2 <- mapvalues(credit_card_default$pay.2, 
                                       from=c("-2","-1","0","1","2","3","4","5","6","7","8"), 
                                       to=  c("-2","-1","0","1","2","3","4","5","6","7","7"))

table_pay.2 = table(credit_card_default$pay.2)
table_pay.2

###Remap pay.3
credit_card_default$pay.3 <- mapvalues(credit_card_default$pay.3, 
                                       from=c("-2","-1","0","1","2","3","4","5","6","7","8"), 
                                       to=  c("-2","-1","0","1","2","3","4","5","6","7","7"))

table_pay.3 = table(credit_card_default$pay.3)
table_pay.3

###Remap pay.4
credit_card_default$pay.4 <- mapvalues(credit_card_default$pay.4, 
                                       from=c("-2","-1","0","1","2","3","4","5","6","7","8"), 
                                       to=  c("-2","-1","0","1","2","3","4","5","6","7","7"))

table_pay.4 = table(credit_card_default$pay.4)
table_pay.4

###Remap pay.5
credit_card_default$pay.5 <- mapvalues(credit_card_default$pay.5, 
                                       from=c("-2","-1","0","2","3","4","5","6","7","8"), 
                                       to=  c("-2","-1","0","1","2","3","4","5","6","7"))

table_pay.5 = table(credit_card_default$pay.5)
table_pay.5

###Remap pay.6
credit_card_default$pay.6 <- mapvalues(credit_card_default$pay.6, 
                                       from=c("-2","-1","0","2","3","4","5","6","7","8"), 
                                       to=  c("-2","-1","0","1","2","3","4","5","6","7"))

table_pay.6 = table(credit_card_default$pay.6)
table_pay.6


########################################################Data split
table(credit_card_default$data.group)

str(credit_card_default)

################################################################################################################
###Feature engineering
###Ratio of amount billed to credit limit
credit_card_default <- credit_card_default %>% mutate(cr.usage1 = pmin(abs(bill.amt1/c.limit), 1))
credit_card_default <- credit_card_default %>% mutate(cr.usage2 = pmin(abs(bill.amt2/c.limit), 1))
credit_card_default <- credit_card_default %>% mutate(cr.usage3 = pmin(abs(bill.amt3/c.limit), 1))
credit_card_default <- credit_card_default %>% mutate(cr.usage4 = pmin(abs(bill.amt4/c.limit), 1))
credit_card_default <- credit_card_default %>% mutate(cr.usage5 = pmin(abs(bill.amt5/c.limit), 1))
credit_card_default <- credit_card_default %>% mutate(cr.usage6 = pmin(abs(bill.amt6/c.limit), 1))

credit_card_default$cr.usage1[credit_card_default$cr.usage1 < 0.00001] <- 0
credit_card_default$cr.usage2[credit_card_default$cr.usage2 < 0.00001] <- 0
credit_card_default$cr.usage3[credit_card_default$cr.usage3 < 0.00001] <- 0
credit_card_default$cr.usage4[credit_card_default$cr.usage4 < 0.00001] <- 0
credit_card_default$cr.usage5[credit_card_default$cr.usage5 < 0.00001] <- 0
credit_card_default$cr.usage6[credit_card_default$cr.usage6 < 0.00001] <- 0

str(credit_card_default)

summary(credit_card_default$cr.usage1)

#write.csv(credit_card_default,"ccsummary1.csv")

#################################################################################################################
###Payment Ratio of amount billed - Payment / limit
credit_card_default <- credit_card_default %>% mutate(act.usage1 = pmin(abs((bill.amt2 - pay.amt1)/c.limit), 1))
credit_card_default <- credit_card_default %>% mutate(act.usage2 = pmin(abs((bill.amt3 - pay.amt2)/c.limit), 1))
credit_card_default <- credit_card_default %>% mutate(act.usage3 = pmin(abs((bill.amt4 - pay.amt3)/c.limit), 1))
credit_card_default <- credit_card_default %>% mutate(act.usage4 = pmin(abs((bill.amt5 - pay.amt4)/c.limit), 1))
credit_card_default <- credit_card_default %>% mutate(act.usage5 = pmin(abs((bill.amt6 - pay.amt5)/c.limit), 1))

credit_card_default$act.usage1[credit_card_default$act.usage1 < 0.00001] <- 0
credit_card_default$act.usage2[credit_card_default$act.usage2 < 0.00001] <- 0
credit_card_default$act.usage3[credit_card_default$act.usage3 < 0.00001] <- 0
credit_card_default$act.usage4[credit_card_default$act.usage4 < 0.00001] <- 0
credit_card_default$act.usage5[credit_card_default$act.usage5 < 0.00001] <- 0

str(credit_card_default)

summary(credit_card_default$act.usage1)

#write.csv(credit_card_default,"ccsummary2.csv")

##################################################################################################################
###Change In Utilization over time period
credit_card_default <- credit_card_default %>% mutate(use.change = cr.usage1 - cr.usage6)

summary(credit_card_default$use.change)

str(credit_card_default)

skimr::skim(credit_card_default$use.change)

##################################################################################################################
###Payment Ratio1
credit_card_default$pay.ratio1 <-
  
  ifelse( credit_card_default$bill.amt2<=0,
          1,
          ifelse(credit_card_default$pay.amt1==0,
                 0,
                 pmin(credit_card_default$pay.amt1/credit_card_default$bill.amt2,1)
          )
  );
###Payment Ratio2
credit_card_default$pay.ratio2 <-
  
  ifelse( credit_card_default$bill.amt3<=0,
          1,
          ifelse(credit_card_default$pay.amt2==0,
                 0,
                 pmin(credit_card_default$pay.amt2/credit_card_default$bill.amt3,1)
          )
  );
###Payment Ratio3
credit_card_default$pay.ratio3 <-
  
  ifelse( credit_card_default$bill.amt4<=0,
          1,
          ifelse(credit_card_default$pay.amt3==0,
                 0,
                 pmin(credit_card_default$pay.amt3/credit_card_default$bill.amt4,1)
          )
  );
###Payment Ratio4
credit_card_default$pay.ratio4 <-
  
  ifelse( credit_card_default$bill.amt5<=0,
          1,
          ifelse(credit_card_default$pay.amt4==0,
                 0,
                 pmin(credit_card_default$pay.amt4/credit_card_default$bill.amt5,1)
          )
  );
###Payment Ratio5
credit_card_default$pay.ratio5 <-
  
  ifelse( credit_card_default$bill.amt6<=0,
          1,
          ifelse(credit_card_default$pay.amt5==0,
                 0,
                 pmin(credit_card_default$pay.amt5/credit_card_default$bill.amt6,1)
          )
  );


str(credit_card_default)

skimr::skim(credit_card_default$pay.ratio1)

credit_card_default$pay.ratio1[credit_card_default$pay.ratio1 < 0.00001] <- 0
credit_card_default$pay.ratio2[credit_card_default$pay.ratio2 < 0.00001] <- 0
credit_card_default$pay.ratio3[credit_card_default$pay.ratio3 < 0.00001] <- 0
credit_card_default$pay.ratio4[credit_card_default$pay.ratio4 < 0.00001] <- 0
credit_card_default$pay.ratio5[credit_card_default$pay.ratio5 < 0.00001] <- 0

summary(credit_card_default$pay.ratio1)

str(credit_card_default)

#write.csv(credit_card_default,"ccsummary3.csv")

skimr::skim(credit_card_default[2:47])

summary(credit_card_default[2:47])
#write.csv(results, "myCSV.csv")
##################################################################################################################
summary(credit_card_default$age)

formula <- default ~ age

##Creating a decision tree
tree2 <- rpart(formula, data = credit_card_default, control=rpart.control(minsplit=10,cp=0))

fancyRpartPlot(tree2)

credit_card_default <- credit_card_default %>% mutate(agegroup.1 = (age <= 26)) 
credit_card_default <- credit_card_default %>% mutate(agegroup.2 = (age >= 27 & age <= 46)) 
credit_card_default <- credit_card_default %>% mutate(agegroup.3 = (age >= 47 & age <= 60)) 
credit_card_default <- credit_card_default %>% mutate(agegroup.4 = (age >= 61 & age <= 72)) 
credit_card_default <- credit_card_default %>% mutate(agegroup.5 = (age >= 73 & age <= 79)) 

#str(credit_card_default)

credit_card_default$ageGroups <- cut(credit_card_default$age, breaks = c(0,26,46,60,79), 
                                     labels = c("Millenials", "Xennials","BabyBoom","Silent Gen"))

str(credit_card_default)

#summary(credit_card_default$ageGroups)
#write.csv(credit_card_default,"ccsummary5.csv")
##################################################################################################################
###Hist boxplots on PAY_0 to pay.6
par(mfrow = c(2,3))
hist(credit_card_default$bill.amt1, col = "cyan",main = "bill.amt1" , xlab = "Hist") 
hist(credit_card_default$pay.amt1, col = "cyan",main = "pay.amt1" , xlab = "Hist") 
hist(credit_card_default$c.limit, col = "cyan",main = "c.limit" , xlab = "Hist") 
hist(credit_card_default$act.usage1, col = "cyan",main = "act.usage1" , xlab = "Hist") 
hist(credit_card_default$cr.usage1, col = "cyan",main = "cr.usage1" , xlab = "Hist") 
hist(credit_card_default$use.change, col = "cyan",main = "use.change" , xlab = "Hist") 

###Frequency table default
table(credit_card_default$default)

###Map Gender to MALE and Female
credit_card_default$sex <- mapvalues(credit_card_default$sex, 
                                     from=c("1","2"), 
                                     to=  c("MALE","FEMALE"))

###Frequency table default sex
table(credit_card_default$sex, credit_card_default$default)

###Creating Bar Plots for Gender as factors for fill and default as factor on x-axis
g1=ggplot(data = credit_card_default, aes(x = default)) + 
  geom_bar(aes (fill = sex), position = "dodge") + 
  ggtitle("default by Gender")+
  xlab("")+ylab("")
g1

###Map edu to 1 = graduate school; 2 = university; 3 = high school; 4 = others
credit_card_default$edu <- mapvalues(credit_card_default$edu, 
                                     from=c("1","2","3","4"), 
                                     to=  c("Grad SChool","University","High School","Other"))

###Frequency table default
table(credit_card_default$edu, credit_card_default$default)

###Creating Bar Plots for edu as factors for fill and default as factor on x-axis
g2=ggplot(data = credit_card_default, aes(x = default)) + 
  geom_bar(aes (fill = edu), position = "dodge") + 
  ggtitle("default by edu")+
  xlab("")+ylab("")
g2

###Map Mariage Status to Marital status (1 = married; 2 = single; 3 = others).
credit_card_default$mar.s <- mapvalues(credit_card_default$mar.s, 
                                       from=c("1","2","3"), 
                                       to=  c("Married","Single","Other"))

###Frequency table default
table(credit_card_default$pay.1, credit_card_default$default)

###Creating Bar Plots for edu as factors for fill and default as factor on x-axis
g3=ggplot(data = credit_card_default, aes(x = default)) + 
  geom_bar(aes (fill = mar.s), position = "dodge") + 
  ggtitle("default by Marital Status")+
  xlab("")+ylab("")
g3


###Frequency table default
table(credit_card_default$c.limit, credit_card_default$default)

###Creating Bar Plots for edu as factors for fill and default as factor on x-axis
g4=ggplot(data = credit_card_default, aes(x = c.limit)) + 
  geom_bar(aes (fill = default), position = "dodge") + 
  ggtitle("default by Credit Limit")+
  xlab("")+ylab("")
g4

###Creating Bar Plots for edu as factors for fill and default as factor on x-axis
g5=ggplot(data = credit_card_default, aes(x = ageGroups)) + 
  geom_bar(aes (fill = default), position = "dodge") + 
  ggtitle("default by agegroups")+
  xlab("")+ylab("")
g5

table(credit_card_default$ageGroups, credit_card_default$default)


###Heat Maps
credit_card_default %>% group_by(edu,age) %>% summarise(LIMIT=mean(c.limit)) -> df
ggplot(df, aes(edu, age, fill=LIMIT)) + geom_tile() + scale_fill_gradient(low="white", high="steelblue")

credit_card_default %>% group_by(edu,mar.s) %>% summarise(LIMIT=mean(c.limit)) -> df
ggplot(df, aes(edu, mar.s, fill=LIMIT)) + geom_tile() + scale_fill_gradient(low="white", high="mediumturquoise")

credit_card_default %>% group_by(edu,sex) %>% summarise(LIMIT=mean(c.limit)) -> df
ggplot(df, aes(edu, sex, fill=LIMIT)) + geom_tile() + scale_fill_gradient(low="white", high="mediumpurple")

################################################################################################################
str(credit_card_default)
###Map ageGroup.1 - agegroup.5
credit_card_default$agegroup.1 <- mapvalues(credit_card_default$agegroup.1, 
                                            from=c("TRUE","FALSE"), 
                                            to=  c("1","0"))

credit_card_default$agegroup.2 <- mapvalues(credit_card_default$agegroup.2, 
                                            from=c("TRUE","FALSE"), 
                                            to=  c("2","0"))

credit_card_default$agegroup.3 <- mapvalues(credit_card_default$agegroup.3, 
                                            from=c("TRUE","FALSE"), 
                                            to=  c("3","0"))

credit_card_default$agegroup.4 <- mapvalues(credit_card_default$agegroup.4, 
                                            from=c("TRUE","FALSE"), 
                                            to=  c("4","0"))

credit_card_default$agegroup.5 <- mapvalues(credit_card_default$agegroup.5, 
                                            from=c("TRUE","FALSE"), 
                                            to=  c("5","0"))


###Map Mariage Status to Marital status (1 = married; 2 = single; 3 = others).
credit_card_default$mar.s <- mapvalues(credit_card_default$mar.s, 
                                       from=c("Married","Single","Other"), 
                                       to=  c("1","2","3"))

###Map edu to 1 = graduate school; 2 = university; 3 = high school; 4 = others
credit_card_default$edu <- mapvalues(credit_card_default$edu, 
                                     from=c("Grad SChool","University","High School","Other"), 
                                     to=  c("1","2","3","4"))
###Map Gender to MALE and Female
credit_card_default$sex <- mapvalues(credit_card_default$sex, 
                                     from=c("MALE","FEMALE"), 
                                     to=  c("1","2"))


str(credit_card_default)

###cONVERT TO NUMERIC
###default
credit_card_default$default <- as.integer((credit_card_default$default))

### Age
credit_card_default$age <- as.numeric((credit_card_default$age))

###PAY_
credit_card_default$pay.1 <- as.integer((credit_card_default$pay.1))
credit_card_default$pay.2 <- as.integer((credit_card_default$pay.2))
credit_card_default$pay.3 <- as.integer((credit_card_default$pay.3))
credit_card_default$pay.4 <- as.integer((credit_card_default$pay.4))
credit_card_default$pay.5 <- as.integer((credit_card_default$pay.5))
credit_card_default$pay.6 <- as.integer((credit_card_default$pay.6))

###bill_
credit_card_default$bill.amt1 <- as.numeric((credit_card_default$bill.amt1))
credit_card_default$bill.amt2 <- as.numeric((credit_card_default$bill.amt2))
credit_card_default$bill.amt3 <- as.numeric((credit_card_default$bill.amt3))
credit_card_default$bill.amt4 <- as.numeric((credit_card_default$bill.amt4))
credit_card_default$bill.amt5 <- as.numeric((credit_card_default$bill.amt5))
credit_card_default$bill.amt6 <- as.numeric((credit_card_default$bill.amt6))

###PAY_
credit_card_default$pay.amt1 <- as.numeric((credit_card_default$pay.amt1))
credit_card_default$pay.amt2 <- as.numeric((credit_card_default$pay.amt2))
credit_card_default$pay.amt3 <- as.numeric((credit_card_default$pay.amt3))
credit_card_default$pay.amt4 <- as.numeric((credit_card_default$pay.amt4))
credit_card_default$pay.amt5 <- as.numeric((credit_card_default$pay.amt5))
credit_card_default$pay.amt6 <- as.numeric((credit_card_default$pay.amt6))


###agegroup
credit_card_default$agegroup.1 <- as.integer((credit_card_default$agegroup.1))
credit_card_default$agegroup.2 <- as.integer((credit_card_default$agegroup.2))
credit_card_default$agegroup.3 <- as.integer((credit_card_default$agegroup.3))
credit_card_default$agegroup.4 <- as.integer((credit_card_default$agegroup.4))
credit_card_default$agegroup.5 <- as.integer((credit_card_default$agegroup.5))

###edu
credit_card_default$edu <- as.factor((credit_card_default$edu))

###SEC
credit_card_default$sex <- as.factor((credit_card_default$sex))

###mar.s
credit_card_default$mar.s <- as.factor((credit_card_default$mar.s))

####
credit_card_default$default <- as.factor((credit_card_default$default))

###check structure
str(credit_card_default)

####Correlation Table
cols <- colnames(credit_card_default[, sapply(credit_card_default, is.numeric)])
df_temp <- t(cor(credit_card_default["default"], credit_card_default[cols], use="complete"))
colnames(df_temp) <- "Corr"
round(df_temp, digits=3)

corrplot(df_temp)

#write.csv(df_temp, "myCSV.csv")
###################################################################################################### Normalize
###Normalize  limit
credit_card_default$c.limit <- (credit_card_default$c.limit - mean(credit_card_default$c.limit))/
  sd(credit_card_default$c.limit)

###Normalize Bill
credit_card_default$bill.amt1 <- (credit_card_default$bill.amt1 - mean(credit_card_default$bill.amt1))/
  sd(credit_card_default$bill.amt1)
credit_card_default$bill.amt2 <- (credit_card_default$bill.amt2 - mean(credit_card_default$bill.amt2))/
  sd(credit_card_default$bill.amt2)
credit_card_default$bill.amt3 <- (credit_card_default$bill.amt3 - mean(credit_card_default$bill.amt3))/
  sd(credit_card_default$bill.amt3)
credit_card_default$bill.amt4 <- (credit_card_default$bill.amt4 - mean(credit_card_default$bill.amt4))/
  sd(credit_card_default$bill.amt4)
credit_card_default$bill.amt5 <- (credit_card_default$bill.amt5 - mean(credit_card_default$bill.amt5))/
  sd(credit_card_default$bill.amt5)
credit_card_default$bill.amt6 <- (credit_card_default$bill.amt6 - mean(credit_card_default$bill.amt6))/
  sd(credit_card_default$bill.amt6)

###Normalize Pay_Amt
credit_card_default$pay.amt1 <- (credit_card_default$pay.amt1 - mean(credit_card_default$pay.amt1))/
  sd(credit_card_default$pay.amt1)
credit_card_default$pay.amt2 <- (credit_card_default$pay.amt2 - mean(credit_card_default$pay.amt2))/
  sd(credit_card_default$pay.amt2)
credit_card_default$pay.amt3 <- (credit_card_default$pay.amt3 - mean(credit_card_default$pay.amt3))/
  sd(credit_card_default$pay.amt3)
credit_card_default$pay.amt4 <- (credit_card_default$pay.amt4 - mean(credit_card_default$pay.amt4))/
  sd(credit_card_default$pay.amt4)
credit_card_default$pay.amt5 <- (credit_card_default$pay.amt5 - mean(credit_card_default$pay.amt5))/
  sd(credit_card_default$pay.amt5)
credit_card_default$pay.amt6 <- (credit_card_default$pay.amt6 - mean(credit_card_default$pay.amt6))/
  sd(credit_card_default$pay.amt6)

###Normalize cr.usage
credit_card_default$cr.usage1 <- (credit_card_default$cr.usage1 - mean(credit_card_default$cr.usage1))/
  sd(credit_card_default$cr.usage1)
credit_card_default$cr.usage2 <- (credit_card_default$cr.usage2 - mean(credit_card_default$cr.usage2))/
  sd(credit_card_default$cr.usage2)
credit_card_default$cr.usage3 <- (credit_card_default$cr.usage3 - mean(credit_card_default$cr.usage3))/
  sd(credit_card_default$cr.usage3)
credit_card_default$cr.usage4 <- (credit_card_default$cr.usage4 - mean(credit_card_default$cr.usage4))/
  sd(credit_card_default$cr.usage4)
credit_card_default$cr.usage5 <- (credit_card_default$cr.usage5 - mean(credit_card_default$cr.usage5))/
  sd(credit_card_default$cr.usage5)
credit_card_default$cr.usage6 <- (credit_card_default$cr.usage6 - mean(credit_card_default$cr.usage6))/
  sd(credit_card_default$cr.usage6)

###Normalize ac.usage
credit_card_default$act.usage1 <- (credit_card_default$act.usage1 - mean(credit_card_default$act.usage1))/
  sd(credit_card_default$act.usage1)
credit_card_default$act.usage2 <- (credit_card_default$act.usage2 - mean(credit_card_default$act.usage2))/
  sd(credit_card_default$act.usage2)
credit_card_default$act.usage3 <- (credit_card_default$act.usage3 - mean(credit_card_default$act.usage3))/
  sd(credit_card_default$act.usage3)
credit_card_default$act.usage4 <- (credit_card_default$act.usage4 - mean(credit_card_default$act.usage4))/
  sd(credit_card_default$act.usage4)
credit_card_default$act.usage5 <- (credit_card_default$act.usage5 - mean(credit_card_default$act.usage5))/
  sd(credit_card_default$act.usage5)

###Normalize pay.ratio
credit_card_default$pay.ratio1 <- (credit_card_default$pay.ratio1 - mean(credit_card_default$pay.ratio1))/
  sd(credit_card_default$pay.ratio1)
credit_card_default$pay.ratio2 <- (credit_card_default$pay.ratio2 - mean(credit_card_default$pay.ratio2))/
  sd(credit_card_default$pay.ratio2)
credit_card_default$pay.ratio3 <- (credit_card_default$pay.ratio3 - mean(credit_card_default$pay.ratio3))/
  sd(credit_card_default$pay.ratio3)
credit_card_default$pay.ratio4 <- (credit_card_default$pay.ratio4 - mean(credit_card_default$pay.ratio4))/
  sd(credit_card_default$pay.ratio4)
credit_card_default$pay.ratio5 <- (credit_card_default$pay.ratio5 - mean(credit_card_default$pay.ratio5))/
  sd(credit_card_default$pay.ratio5)

###use change
credit_card_default$use.change <- (credit_card_default$use.change - mean(credit_card_default$use.change))/
  sd(credit_card_default$use.change)

###Structure
str(credit_card_default)

###Skim for NA's
skimr::skim(credit_card_default)

#write.csv(credit_card_default,"ccsummary6.csv")

#####################################################################set up data for analysis
#############################################################################Training Dataset
credit_default <- credit_card_default[, c('ID','c.limit','sex','edu','mar.s','age','pay.1','pay.2','pay.3',
                                          'pay.4','pay.5','pay.6','bill.amt1','bill.amt2','bill.amt3','bill.amt4',
                                          'bill.amt5','bill.amt6','pay.amt1','pay.amt2','pay.amt3','pay.amt4',
                                          'pay.amt5','pay.amt6','cr.usage1','cr.usage2','cr.usage3','cr.usage4',
                                          'cr.usage5','cr.usage6','act.usage1','act.usage2','act.usage3',
                                          'act.usage4','act.usage5','use.change','pay.ratio1','pay.ratio2',
                                          'pay.ratio3','pay.ratio4','pay.ratio5','agegroup.1','agegroup.2',
                                          'agegroup.3','agegroup.4','agegroup.5','train','test','validate',
                                          'data.group','u','default')]

str(credit_default)

skimr::skim(data.train.c)

#write.csv(credit_default,"ccsummary7.csv")

##########################################################################################################
data.train <- credit_default[credit_default$data.group=="train",]
dim(data.train)#15180 #52
str(data.train)

###Break data into independent variables
x.train <- data.train[,2:46]
dim(x.train)#15180 #45
str(x.train)

###Seperate default
c.train <- data.train[,52] # default
n.train.c <- length(c.train) # 15180
str(n.train.c)

data.train.c <- data.frame(x.train, default=c.train) # to classify default
str(data.train.c)
#################################################################################Validation data
data.valid <- credit_default[credit_default$data.group=="validate",]
dim(data.valid)#7497 #52
str(data.valid)

###Break data into independent variables
x.valid <- data.valid[,2:46]
dim(x.valid)#7497 #45
str(x.valid)

###Seperate default
c.valid <- data.valid[,52] # default
n.valid.c <- length(c.valid) # 7497
str(n.valid.c)

data.valid.c <- data.frame(x.valid, default=c.valid) # to classify default
str(data.valid.c)

##########################################################################################3###Test
data.test <- credit_default[credit_default$data.group=="test",]
n.test <- dim(data.test)[1] # 7323 52
x.test <- data.test[,2:46]

################################################################################# Test data
data.test <- credit_default[credit_default$data.group=="test",]
dim(data.test)#7323 #52
str(data.test)

###Break data into independent variables
x.test <- data.test[,2:46]
dim(x.test)#7323 #45
str(x.test)

###Seperate default
c.test <- data.test[,52] # default
n.test.c <- length(c.test) # 7323
str(n.test.c)

data.test.c <- data.frame(x.test, default=c.test) # to classify default
str(data.test.c)


################################################################################################## Decision Tree 
### Decision Tree
dtree.train = data.train.c
dtree.train$default = as.factor(dtree.train$default)
dtree.test = data.test.c
dtree.test$default = as.factor(dtree.test$default)


form <- as.formula(default ~ c.limit + sex + edu + mar.s + age + agegroup.1 + agegroup.2 + agegroup.3 +
                     agegroup.4 + agegroup.5 + pay.1 + pay.2 + pay.3 + pay.4 + pay.5 + pay.6 + bill.amt1 + 
                     bill.amt2 + bill.amt3 + bill.amt4 + bill.amt5 + bill.amt6 + pay.amt1 + pay.amt2 + 
                     pay.amt3 + pay.amt4 + pay.amt5 + pay.amt6 + cr.usage1 + cr.usage2 + cr.usage3 + 
                     cr.usage4 + cr.usage5 + cr.usage6 + act.usage1 + act.usage2 + act.usage3 + 
                     act.usage4 + act.usage5 + use.change + pay.ratio1 + pay.ratio2 + pay.ratio3 + 
                     pay.ratio4 + pay.ratio5)

tree.1 <- rpart(form,data=credit_default,control=rpart.control(minsplit=5,cp=0))

plot(tree.1)
text(tree.1)

#####################################################################################
### A more reasonable tree
tree.2 <- rpart(form,credit_default)			
prp(tree.2)               # A fast plot													
fancyRpartPlot(tree.2)		# A fancy plot from rattle


######################################################### Fit tree on training data
dt.default = rpart(form,credit_default)
### plot(dt.default)
### text(dt.default, pretty=0)
tree.pred = predict(dt.default, dtree.train, type = "class")
table(tree.pred, c.train)

caret::confusionMatrix(tree.pred, c.train, positive="1", mode="everything")


###Error for treee on training data 
treeError1 <- mean(tree.pred != c.train)
treeError1
###Error 0.1828

############################################################# Fit tree on test.
dt.default = rpart(form,credit_default)
### plot(dt.default)
### text(dt.default, pretty=0)
tree.pred = predict(dt.default, dtree.test, type = "class")
table(tree.pred, c.test)

###Error 
treeError2 <- mean(tree.pred != c.test)
treeError2
###Error 0.1848


###Hist boxplots pay.1 and pay.2
table(credit_card_default$pay.1, credit_card_default$default)

caret::confusionMatrix(tree.pred, c.test, positive="1", mode="everything")

##########################################################################################Random Forrest Chapter 8
require(randomForest)
set.seed(1) 
Random.Forest = randomForest(default ~ ., data=data.train.c, importance=TRUE, type="classification")

###Var importance
importance(Random.Forest)

###Plot
varImpPlot(Random.Forest)

###############################################################Table
set.seed(1)
bagpred = predict(Random.Forest, newdata=dtree.train)
table(bagpred, c.train)

###Error Bag
rferror1 = mean(bagpred != c.train)
rferror1
###Error = .0076

caret::confusionMatrix(bagpred, c.train, positive="1", mode="everything")


###############################################################Table
set.seed(1)
bagpred2 = predict(Random.Forest, newdata=dtree.test)
table(bagpred2, c.test)

###Error Bag
rferror2 = mean(bagpred2 != c.test)
rferror2
###Error = .1801

caret::confusionMatrix(bagpred2, c.test, positive="1", mode="everything")

#####################################################################################################Boost Chapter 8
require(gbm)
set.seed(1)
Gradient.Boost = gbm((unclass(default)-1) ~ ., data=data.train.c, distribution= "gaussian",
                     n.trees =5000, interaction.depth = 4)

summary(Gradient.Boost)

#####Table Train
set.seed(1)
boost.prob = predict.gbm(Gradient.Boost, newdata = data.train.c,
                         n.trees = 5000, type = "response")

boost.pred = rep("0", 15180)
boost.pred[boost.prob > .5] = "1"


table(boost.pred , c.train)

###Error Boost
boostError1 <- mean(boost.pred != c.train)
boostError1
###Error = .1855409

caret::confusionMatrix(boost.pred, c.train, positive="1", mode="everything")

######################Table Test
set.seed(1)
boost.prob = predict.gbm(Gradient.Boost, newdata = data.test.c,
                         n.trees = 5000, type = "response")

boost.pred = rep("0", 7323)
boost.pred[boost.prob > .5] = "1"

####Table
table(boost.pred , c.test)


###Error Boost
boostError2 <- mean(boost.pred != c.test)
boostError2
###Error = .1855409

caret::confusionMatrix(boost.pred, c.test, positive="1", mode="everything")

####################################################################logistic regression check variable significance
m.glm <- glm(default ~  c.limit + sex + edu + mar.s + age + pay.1 + pay.2 + pay.3 + pay.4 + pay.5 + pay.6 + bill.amt1 +
               bill.amt2 + bill.amt3 + bill.amt4 + bill.amt5 + bill.amt6 + pay.amt1 + pay.amt2 + pay.amt3 + pay.amt4 + 
               pay.amt5 + pay.amt6 + cr.usage1 + cr.usage2 + cr.usage3 + cr.usage4 + cr.usage5, data.train.c, 
             family=binomial("logit"),control = list(maxit = 50))

summary(m.glm)
### AIC 14837
#########################################################Logistic only variables listed as statistically significant 
### Error     = 0.1177209
### Pred      = 1794/1995 
### AIC 14822

full <- glm(default ~  c.limit + sex + edu + mar.s + age + pay.1 + pay.2 + pay.3 + pay.5 + 
              bill.amt1  + pay.amt1 + pay.amt2 + pay.amt3 + pay.amt4 + pay.amt5 + pay.amt6 + cr.usage1 + 
              cr.usage2 + cr.usage3 + cr.usage4 + cr.usage5 + cr.usage6 + act.usage1 + 
              act.usage2 + pay.ratio1 + pay.ratio2 + pay.ratio3 + pay.ratio4 + pay.ratio5, 
            data.train.c, family=binomial("logit"),control = list(maxit = 50))

summary(full)

#################################################Automated selection
step <- stepAIC(full, trace = FALSE)

step$anova

#######New model
m.glm2 <- glm(default ~ c.limit + sex + edu + mar.s + age + pay.1 + pay.2 + pay.3 + pay.5 + bill.amt1 + 
                pay.amt1 + pay.amt2 + pay.amt4 +   pay.amt5 + pay.amt6 + cr.usage1 + cr.usage3 + cr.usage6 + 
                act.usage1 + act.usage2 + pay.ratio1 + pay.ratio2 + pay.ratio4 +pay.ratio5, 
              data.train.c, family=binomial("logit"),control = list(maxit = 50))

summary(m.glm2)


###predictions for training 
m.glm3prob <- predict(m.glm2, type = "response")
m.glm3pred = rep("0", 15180)
m.glm3pred[m.glm3prob > .5] = "1"
table(m.glm3pred, c.train)
m.glm3Error.train3 <- mean(m.glm3pred != c.train)
m.glm3Error.train3


caret::confusionMatrix(m.glm3pred, c.train, positive="1", mode="everything")

#######Test set model
m.glm5 <- glm(default ~ c.limit + sex + edu + mar.s + age + pay.1 + pay.2 + pay.3 + pay.5 + bill.amt1 + 
                pay.amt1 + pay.amt2 + pay.amt4 +   pay.amt5 + pay.amt6 + cr.usage1 + cr.usage3 + cr.usage6 + 
                act.usage1 + act.usage2 + pay.ratio1 + pay.ratio2 + pay.ratio4 +pay.ratio5, 
              data.test.c, family=binomial("logit"),control = list(maxit = 50))

summary(m.glm5)



###predictions for training 
m.glm4prob <- predict(m.glm5, type = "response")
m.glm4pred = rep("0", 7323)
m.glm4pred[m.glm4prob > .5] = "1"
table(m.glm4pred, c.test)
m.glm4Error.train4 <- mean(m.glm4pred != c.test)
m.glm4Error.train4


caret::confusionMatrix(m.glm4pred, c.test, positive="1", mode="everything")

#############################################################LDA (Linear discriminite analysis) from example code 422)
### Error       = 0.124749
### Pred        = 1855/1995 donors
m.lda1 <- lda(default ~ c.limit + sex + edu + mar.s + age + pay.1 + pay.2 + pay.3 + pay.5 + bill.amt1 + 
                pay.amt1 + pay.amt2 + pay.amt4 +   pay.amt5 + pay.amt6 + cr.usage1 + cr.usage3 + cr.usage6 + 
                act.usage1 + act.usage2 + pay.ratio1 + pay.ratio2 + pay.ratio4 +pay.ratio5, 
              data.test.c)

m.lda1

###confusion matrix.
m.lda1fit.pred <- predict(m.lda1, data.train.c)
m.lda1fit.pred.class <- m.lda1fit.pred$class
table(m.lda1fit.pred.class, c.train)
mean(m.lda1fit.pred.class != c.train)
m.lda1fit.pred.posterior <- m.lda1$posterior[,2]

caret::confusionMatrix(m.lda1fit.pred, c.test, positive="1", mode="everything")

###confusion matrix.
m.lda1fit.pred <- predict(m.lda1, data.test)
m.lda1fit.pred.class <- m.lda1fit.pred$class
table(m.lda1fit.pred.class, c.test)
mean(m.lda1fit.pred.class != c.test)
m.lda1fit.pred.posterior <- m.lda1$posterior[,2]

caret::confusionMatrix(m.glm4pred, c.test, positive="1", mode="everything")



predict1<-predict(m.lda1, data.test, type = "response")
predict1<-data.frame(Tag=test$Tag,default = m.lda1)
predict1<-select(predict1, -default.0)





