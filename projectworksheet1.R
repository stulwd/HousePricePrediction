## take home
## 1. Kaggle Submission
## 2. First taste of feature engineering
## 3. Linear Regression and Earth(Mars) Modelling




# change to your own directory
# setwd('D:/OneDrive/OneDrive - The Hong Kong Polytechnic University/ama563/kaggle')
library(dplyr)
library(data.table)
library(earth)
library(ggplot2)
train = fread('train.csv')
test = fread('test.csv')
test$SalePrice = NA       # 添加列
dat = rbind(train,test)   # 组合数据

allMSCls = table(dat$MSSubClass)
allMSClsINtrain = table(train$MSSubClass)

train$MSSubClass2 = train$MSSubClass
train$MSSubClass2[train$MSSubClass2 %in% unique(c(names(allMSCls[allMSCls<30]),
setdiff(names(allMSCls),names(allMSClsINtrain))))] = 'Other'
test$MSSubClass2 = test$MSSubClass
test$MSSubClass2[test$MSSubClass2 %in% unique(c(names(allMSCls[allMSCls<30]),
setdiff(names(allMSCls),names(allMSClsINtrain))))] = 'Other'

# rmse 
rmse = function(x,y) sqrt(mean((log(x)-log(y))^2))

# linear regression
lm1 = lm(SalePrice ~ MSSubClass2,train)
summary(lm1)
test$SalePrice = predict(lm1,test)
rmse(predict(lm1),train$SalePrice)
output = dplyr::select(test,Id,SalePrice)
fwrite(output,'submission.csv') # 0.35042

# use average of subgroup as covariate/feature
tempdf = aggregate(SalePrice ~ MSSubClass, train, mean)
tempdf
names(tempdf) = c('MSSubClass','AVGSPbyMSCls')
train = train %>% left_join(tempdf)
test = test %>% left_join(tempdf)
sum(is.na(test$AVGSPbyMSCls)) # one missing data due to limited sample
test$AVGSPbyMSCls[is.na(test$AVGSPbyMSCls)] = mean(train$SalePrice,na.rm=T)

lm2 = lm(SalePrice ~ 0 + AVGSPbyMSCls,train) # no intercept
summary(lm2)
test$SalePrice = predict(lm2,test)
rmse(predict(lm2),train$SalePrice)
output = dplyr::select(test,Id,SalePrice)
fwrite(output,'submission.csv') # 0.34922

# two variables
cor(train$LotArea,train$SalePrice)
lm3 = lm(SalePrice ~ AVGSPbyMSCls + LotArea,train)
summary(lm3)
test$SalePrice = predict(lm3,test)
rmse(predict(lm3),train$SalePrice)
output = dplyr::select(test,Id,SalePrice)
fwrite(output,'submission.csv') # 0.34070

# two variables + log-scale
cor(train$LotArea,train$SalePrice)
lm4 = lm(log(SalePrice) ~ log(AVGSPbyMSCls) + log(LotArea),train)
summary(lm4)
test$SalePrice = exp(predict(lm4,test))
rmse(exp(predict(lm4)),train$SalePrice)
output = dplyr::select(test,Id,SalePrice)
fwrite(output,'submission.csv')  # 33526

ggplot(data = train, aes(x = log(LotArea), y = log(SalePrice))) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)

# two variables + earth
# http://www.milbo.org/doc/earth-notes.pdf
e1 = earth(log(SalePrice) ~ log(AVGSPbyMSCls)+log(LotArea),train)
summary(e1)
test$SalePrice = exp(predict(e1,test))
rmse(exp(predict(e1)),train$SalePrice)
output = dplyr::select(test,Id,SalePrice)
fwrite(output,'submission.csv') # 0.32221







