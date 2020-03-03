setwd('D:/000File/Current/590 Method for Data Science/house-prices-advanced-regression-techniques')

# -------- Naive OLS

train.df = read.csv('train_clean.csv')
test.df = read.csv('test_clean.csv')

lm1 = lm(SalePrice~., data=train.df)
lm.pred = predict(lm1, test.df)

lm.submit = as.data.frame(cbind(c(1461:2919),lm.pred))
colnames(lm.submit) = c('Id','SalePrice')
write.csv(lm.submit,'lm_sub.csv',row.names = F)

# ----------CV OLS
library(glmnet)
set.seed(1)

ori.train = read.csv('train_2.csv')
ori.test = read.csv('test_2.csv')

train = sample(nrow(ori.train),0.7*nrow(ori.train))

# convert each col type to numeric
X = sapply(ori.train[,1:54], as.numeric)
y = ori.train[,55]
testX = sapply(ori.test[,1:54], as.numeric)

grid=10^seq(5,-2,length=100) 
cv.out=cv.glmnet(X[train,], y[train], alpha=1, lambda = grid) 
# 默认K=10, alpha=0 为ridge, alpha=1 为 LASSO
plot(cv.out)
bestlam=cv.out$lambda.min # 通过cv找出最优lambda

ridge.pred=predict(cv.out,s=bestlam*10 ,newx=X[-train,]) 
ridge.pred=predict(cv.out,s=bestlam*10 ,newx=testX) 
mse(ridge.pred, y[-train])

lasso.pred=predict(cv.out,s=bestlam/5 ,newx=X[-train,]) 
mse(lasso.pred, y[-train])
lasso.pred=predict(cv.out,s=bestlam/5 ,newx=testX) 


ridge.sub = as.data.frame(cbind(c(1461:2919),lasso.pred))
colnames(ridge.sub) = c('Id','SalePrice')
write.csv(ridge.sub,'lasso_bestlam.csv',row.names = F)


# ----------------------- PCR and PLS

# Before running regression, we need to remove a factor
# which has new levels in test dataset, if untreated the error would be
# Error in newX %*% B[-1, , i] : non-conformable arguments

# Find out the difference
cols = colnames(ori.train)
for (col in cols){
  if(class(ori.train[[col]]) == 'factor'){
    print(summary(ori.train[[col]]))
    print(summary(ori.test[[col]]))
  }
}

ori.test$KitchenQual[ori.test$KitchenQual=='Ex']='Gd'

write.csv(ori.test,'test_22.csv',row.names = F)
ori.test=read.csv('test_22.csv')


library(pls)
pcr.fit = pcr(SalePrice~.,data=train.df,scale=T,validation='CV')
summary(pcr.fit)
validationplot(pcr.fit ,val.type="MSEP") 
pcr.pred=predict(pcr.fit ,test.df, ncomp =6)
mse(pcr.pred,test.df$SalePrice)


pcr.pred = predict(pcr.fit, ori.test, ncomp=6)


pcr.sub = as.data.frame(cbind(c(1461:2919),pcr.pred))
colnames(pcr.sub) = c('Id','SalePrice')


write.csv(pcr.sub,'pcr_6comp.csv',row.names = F)


# --------------------- PLS

pls.fit=plsr(SalePrice~.,data=train.df,scale=T,validation='CV')
validationplot(pls.fit ,val.type="MSEP")
summary(pls.fit)
pls.pred=predict(pls.fit ,test.df, ncomp =6)
mse(pls.pred, test.df$SalePrice)
pls.pred=predict(pls.fit ,ori.test , ncomp =6)

pls.sub = as.data.frame(cbind(c(1461:2919),pls.pred))
colnames(pls.sub) = c('Id','SalePrice')
write.csv(pls.sub,'pls_6comp.csv',row.names = F)


# -------------------- SVR -----------------
install.packages('e1071')

library(e1071)
set.seed(1)

setwd('D:/000File/Current/590 Method for Data Science/house-prices-advanced-regression-techniques')

ori.train = read.csv('train_2.csv')
ori.test = read.csv('test_22.csv')

train = sample(nrow(ori.train),0.7*nrow(ori.train))

train.df = ori.train[train,]
test.df = ori.train[-train,]

svm1 = svm(SalePrice ~., data=train.df,kernel ="linear", cost =25,
           scale =FALSE)
svm.pred = predict(svm1,test.df)
svm.pred = predict(tune.out$best.model,test.df)
mse(svm.pred, test.df$SalePrice)

tune.out=tune(svm, SalePrice~., data=train.df ,kernel ="linear",
              ranges=list(cost=c(10,25,50,75.100) ))

tune.out$best.parameters
plot(tune.out)

svm.pred = predict(tune.out$best.model,test.df)
mse(svm.pred, test.df$SalePrice)
svm.pred = predict(tune.out$best.model,ori.test)


svm.sub = as.data.frame(cbind(c(1461:2919),svm.pred))
colnames(svm.sub) = c('Id','SalePrice')
write.csv(svm.sub,'svm_best25.csv',row.names = F)

# linear: 1 ~ 10, mean = 937785875
# radial: 1 ~ 10, mean = 1137798404
# linear 0.1 = 943265506, 1 = 937321836, 5=937590724
# linear 0.5= 947039945 0.8=  1.2=  2= 946118819 3 932777337
plot(tune.out)
summary(tune.out)




svm.pred = predict(svm1, test.df)
sqrt(mse(svm.pred, test.df$SalePrice))



mse = function(v1,v2){
  if(length(v1)==length(v2)){
    return (mean((v1-v2)^2))
  }
  else{
    print('Unequal length of two vectors!')
    return (NULL)
  }
}

