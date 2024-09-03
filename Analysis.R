#Project 1
#author: "Rohan.P| Mithil.G| Shiva Sai.V| Koushik.K"
#date: "2023-04-22"

library(corrplot)
library(caTools)
library(readxl)
library(dplyr)
library(ggplot2)
library(corrgram)
library(corrplot)
library(caTools)
library(patchwork)
library(randomForest)
library(gam)
library(boot)
library (pls)
library (gbm)
library(glmnet)
library(e1071)
set.seed(365)
results_model = data.frame(matrix(nrow=0, ncol=3))
colnames(results_model) = c("Model_Name", "Train_MSE", "Test_MSE")
data <- read_xlsx("Health_data.xlsx")
head(data)
summary(data)
any(is.na(data))
corrplot.mixed(cor(data), order = 'AOE')
pairs(data)
x1 = ggplot(data,aes(x=X1))+geom_histogram(bins=20,alpha=0.5,fill='blue')
x2 = ggplot(data,aes(x=X2))+geom_histogram(bins=20,alpha=0.5,fill='blue')
x3 = ggplot(data,aes(x=X3))+geom_histogram(bins=20,alpha=0.5,fill='blue')
x4 = ggplot(data,aes(x=X4))+geom_histogram(bins=20,alpha=0.5,fill='blue')
(x1 | x2) / (x3 | x4)
X_std = rapply(data,scale,c("numeric","integer"),how="replace")
x1 = ggplot(X_std,aes(x=X1))+geom_histogram(bins=20,alpha=0.5,fill='blue')
x2 = ggplot(X_std,aes(x=X2))+geom_histogram(bins=20,alpha=0.5,fill='blue')
x3 = ggplot(X_std,aes(x=X3))+geom_histogram(bins=20,alpha=0.5,fill='blue')
x4 = ggplot(X_std,aes(x=X4))+geom_histogram(bins=20,alpha=0.5,fill='blue')
(x1 | x2) / (x3 | x4)
par(mfrow=c(2,2))
x <- data$X1
y <- data$X2+data$X3+data$X4+data$X5
plot(x,y)
log_y <- log(data$X2+data$X3+data$X4+data$X5)
plot(x,log_y)
sqrt_y <- sqrt(data$X2+data$X3+data$X4+data$X5)
plot(x,sqrt_y)
par(mfrow=c(1,1))
set.seed(195)
rand_sample <- sample.split(X_std$X1, SplitRatio = 0.8)
tr_data = subset(data, rand_sample == TRUE)
test_data = subset(data, rand_sample == FALSE)
model <- lm(X1 ~ ., data = tr_data)
summary(model)
res <- residuals(model)
res_df <- as.data.frame(res)
head(res)
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))
#train_pred
tr_pred = predict(model,tr_data)
tr_results = cbind(tr_data$X1,tr_pred)
colnames(tr_results) = c("actual","predicted")
tr_results <- as.data.frame(tr_results)
tr_mse <- mean((tr_results$actual - tr_results$predicted)^2)
tr_ssr <- sum((tr_results$actual - tr_results$predicted)^2)
tr_sst <- sum((mean(tr_data$X1) - tr_results$predicted)^2)
tr_R2 <- 1 -(tr_ssr/tr_sst)
#test_pred
pred = predict(model,test_data)
results = cbind(test_data$X1,pred)
colnames(results) = c("actual","predicted")
results <- as.data.frame(results)
head(results)
mse <- mean((results$actual - results$predicted)^2)
ssr <- sum((results$actual - results$predicted)^2)
sst <- sum((mean(test_data$X1) - results$predicted)^2)
R2 <- 1 -(ssr/sst)
results_model = rbind(results_model, data.frame(Model_Name='Linear Model', Train_MSE=tr_mse, Test_MSE=mse))
x2_mod <- lm(X1 ~ poly(X2,degree = 2), data = tr_data)
summary(x2_mod)
x3_mod <- lm(X1 ~ poly(X3,degree = 2), data = tr_data)
summary(x3_mod)
X4_mod <- lm(X1 ~ poly(X4,degree = 2), data = tr_data)
summary(X4_mod)
x5_mod <- lm(X1 ~ poly(X5,degree = 2), data = tr_data)
summary(x5_mod)
x2 <- tr_data$X2
log_X3 <- log(tr_data$X3)
log_X5 <- log(tr_data$X5)
log_X4 <- log(tr_data$X4)
poly_model <- lm(X1 ~ X2 + poly(log_X3,log_X4,degree = 1) + poly(log_X5,degree = 3), data = tr_data)
summary(poly_model)
res_poly <- residuals(poly_model)
res_df_poly <- as.data.frame(res)
head(res_poly)
par(mfrow=c(2,2))
plot(poly_model)
par(mfrow=c(1,1))
#train_pred
tr_pred_poly = predict(poly_model,tr_data)
tr_results_poly = cbind(tr_data$X1,tr_pred_poly)
colnames(tr_results_poly) = c("actual","predicted")
tr_results_poly <- as.data.frame(tr_results_poly)
tr_mse_poly <- mean((tr_results_poly$actual - tr_results_poly$predicted)^2)
tr_ssr_poly <- sum((tr_results_poly$actual - tr_results_poly$predicted)^2)
tr_sst_poly <- sum((mean(tr_data$X1) - tr_results_poly$predicted)^2)
tr_R2_poly <- 1 -(tr_ssr_poly/tr_sst_poly)
#test_pred
test_pred_poly = predict(poly_model,new_data = test_data)
test_results_poly = cbind(test_data$X1,test_pred_poly)
colnames(test_results_poly) = c("actual","predicted")
test_results_poly <- as.data.frame(test_results_poly)
head(test_results_poly)
mse_poly<- mean((test_results_poly$actual - test_results_poly$predicted)^2)
ssr_poly <- sum((test_results_poly$actual - test_results_poly$predicted)^2)
sst_poly <- sum((mean(test_data$X1) - test_results_poly$predicted)^2)
R2_poly <- 1 -(ssr_poly/sst_poly)
results_model = rbind(results_model, data.frame(Model_Name='Polynomial Model', Train_MSE=tr_mse_poly, Test_MSE=mse_poly))
rf_model = randomForest(X1 ~ .,data = tr_data,mtry=4,importance =TRUE)
rf_model
summary(rf_model)
importance(rf_model)
varImpPlot(rf_model)
rf_tr_pred <- predict(rf_model,data=tr_data)
tr_mse_rf <- mean((rf_tr_pred - tr_data$X1)^2)
rf_pred = predict(rf_model,newdata = test_data)
mse_rf <- mean((rf_pred - test_data$X1)^2)
plot(rf_model,data=tr_data)
ImpData <- as.data.frame(importance(rf_model))
ImpData$Var.Names <- row.names(ImpData)
ggplot(ImpData, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="blue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.8) +
  #theme_light() +
  #coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )
results_model = rbind(results_model, data.frame(Model_Name='Random Forest', Train_MSE=tr_mse_poly, Test_MSE=mse_poly))
gam.m1 = gam(X1~s(X2,4)+s(X3,4)+s(X4,4)+s(X5,4), data = tr_data)
gam.m2 = gam(X1~s(X2,2)+s(X3,2)+s(X4,5)+s(X5,5), data = tr_data)
gam.m3 = gam(X1~s(X2,2)+ s(X3,2)+s(X4,2)+s(X5,5), data = tr_data)
gam.m4 = gam(X1~lo(X2,X3,X4,X5,span=0.5), data = tr_data)
par(mfrow = c(1,4))
plot(gam.m3,se=T,col = 'blue')
summary(gam.m1)
summary(gam.m2)
summary(gam.m3)
summary(gam.m4)
anova(gam.m1,gam.m2,gam.m3,test="F")
pred_tr_gam <- predict(gam.m3,data=tr_data)
tr_mse_gam <- mean((pred_tr_gam - tr_data$X1)^2)
pred_test_gam <- predict(gam.m3,newdata=test_data)
mse_gam <- mean((pred_test_gam - test_data$X1)^2)
ssr_gam <- sum((test_data$X1 - pred_test_gam)^2)
sst_gam <- sum((mean(test_data$X1) - pred_test_gam)^2)
R2_gam <- 1 -(ssr_gam/sst_gam)
results_model = rbind(results_model, data.frame(Model_Name='GAM', Train_MSE=tr_mse_gam, Test_MSE=mse_gam))
cv.error <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(X1 ~ X2 + poly(X3,i) + X4+poly(X5 , i), data = data)
  cv.error[i] <- cv.glm(data , glm.fit)$delta [1]
}
cv.error
set.seed (99)
cv.error.10 <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(X1 ~ X2 + poly(X3,i) + X4+poly(X5 , i), data = data)
  cv.error.10[i] <- cv.glm(data , glm.fit , K = 10)$delta [1]
}
cv.error.10
set.seed (99)
cv.error.5 <- rep(0, 5)
for (i in 1:10) {
  glm.fit <- glm(X1 ~ X2 + poly(X3,i) + X4+poly(X5 , i), data = data)
  cv.error.5[i] <- cv.glm(data , glm.fit , K = 5)$delta [1]
}
cv.error.5 
results_model = rbind(results_model, data.frame(Model_Name='GLM(LOOCV)', Train_MSE='2.60', Test_MSE=cv.error[1]))
set.seed(365)
rand_sample <- sample.split(data$X1, SplitRatio = 0.8)
tr_data = subset(data, rand_sample == TRUE)
test_data = subset(data, rand_sample == FALSE)
train_svr_d = data.frame(tr_data)
sst = sum((tr_data$X1 - mean(tr_data$X1))^2)
descriptors_train_svr = tr_data[,! names(tr_data) %in% c("X1")]
descriptors_test_svr = test_data[,! names(tr_data) %in% c("X1")]
descriptors_train_svr = as.matrix(descriptors_train_svr)
descriptors_test_svr = as.matrix(descriptors_test_svr)
properties_train_svr = tr_data$X1
properties_test_svr = test_data$X1
model_svr = tune(svm, properties_train_svr ~ descriptors_train_svr, ranges=list(epsilon=seq(0,1,0.1),                    cost=1:10, gamma=c(0.1, 1, 5, 10, 100)))
best_model_svr = model_svr$best.model
summary(best_model_svr)
kernel = 'radial'
cost = best_model_svr$cost
gamma = best_model_svr$gamma
epsilon = best_model_svr$epsilon
svm_fit = svm(tr_data$X1 ~ ., data = tr_data, method = 'eps-regression', kernel = kernel, cost = cost,                gamma = gamma, epsilon = epsilon)  
pred_train_svr = predict(svm_fit, data = descriptors_train_svr)
pred_test_svr = predict(svm_fit, newdata = descriptors_test_svr)
tr_mse_svr = mean((pred_train_svr - tr_data$X1)^2)
mse_svr = mean((pred_test_svr - test_data$X1)^2)
ssr_svr = sum((pred_train_svr - tr_data$X1)^2)
R2_svr <- 1 -(ssr_svr/sst)
kernel = 'radial'
cost = 5
gamma = 5.5
epsilon = 0.4 
svm_fit = svm(tr_data$X1 ~ ., data = tr_data, method = 'eps-regression', kernel = kernel, cost = cost,                gamma = gamma, epsilon = epsilon) 
pred_train_svr = predict(svm_fit, data = descriptors_train_svr)
pred_test_svr = predict(svm_fit, newdata = descriptors_test_svr)
tr_mse_svr = mean((pred_train_svr - tr_data$X1)^2)
mse_svr = mean((pred_test_svr - test_data$X1)^2)
ssr_svr = sum((pred_train_svr - tr_data$X1)^2)
R2_svr <- 1 -(ssr_svr/sst)
results_model = rbind(results_model, data.frame(Model_Name='SVR', Train_MSE=tr_mse_svr, Test_MSE=mse_svr))
results_model
##############################################################################################################################################################

# Airfoil Self-Noise Data Set
Pilot_data_set = read.table("~/airfoil_self_noise.dat", sep="\t")
View(Pilot_data_set)
head(Pilot_data_set)
summary(Pilot_data_set)
any(is.na(Pilot_data_set))
cor_data = cor(Pilot_data_set)
cor_data
library(corrplot)
cor_data = cor(Pilot_data_set)
corrplot.mixed(cor(Pilot_data_set), order = 'AOE')
#corrplot(cor_data, method = 'color')
pairs(Pilot_data_set)
plot(Pilot_data_set$V1, Pilot_data_set$V6)
plot(Pilot_data_set$V2, Pilot_data_set$V6)
plot(Pilot_data_set$V3, Pilot_data_set$V6)
plot(Pilot_data_set$V4, Pilot_data_set$V6)
plot(Pilot_data_set$V5, Pilot_data_set$V6)
scale_data <-as.data.frame(scale(Pilot_data_set))
summary(scale_data)
plot(scale_data$V1, scale_data$V6)
plot(scale_data$V2, scale_data$V6)
plot(scale_data$V5, scale_data$V6)
plot(scale_data$V3, scale_data$V6)
plot(scale_data$V4, scale_data$V6)
x = scale_data$V1+scale_data$V2+scale_data$V3+scale_data$V4+scale_data$V5
plot(x, scale_data$V6)
#abline(Mul_linear_fit, lwd = 3, col = "red")
scale_data$V4 <- as.factor(scale_data$V4)
scale_data$V3 <- as.factor(scale_data$V3)
plot(scale_data$V4, scale_data$V6)
plot(scale_data$V3, scale_data$V6)
boxplot(scale_data$V1)
boxplot(scale_data$V2)
boxplot(scale_data$V3)
boxplot(scale_data$V4)
boxplot(scale_data$V5)
#make this example reproducible
set.seed(1)
#use 70% of dataset as training set and 30% as test set
sample <- sample.split(scale_data$V6, SplitRatio = 0.7)
train  <- subset(scale_data, sample == TRUE)
test   <- subset(scale_data, sample == FALSE)
prop.table(table(train$V3))
prop.table(table(test$V3))
mul_linear_fit = lm(V6~., data = train)
summary(mul_linear_fit)
par(mfrow = c(2,2))
plot(mul_linear_fit)
w <- abs(rstudent(mul_linear_fit)) < 3 & abs(cooks.distance(mul_linear_fit)) < 4/nrow(mul_linear_fit$model)
LR_updated <- update(mul_linear_fit, weights=as.numeric(w))
summary(LR_updated)
par(mfrow = c(2,2))
plot(LR_updated)
test_result = predict(mul_linear_fit,train)
predictions = test_result
actual = train$V6
mean((predictions - actual)^2)
test_result = predict(mul_linear_fit,test)
predictions = test_result
actual = test$V6
mean((predictions - actual)^2)
glm.fit <- glm (V6 ~ ., data = train)
cv.err <- cv.glm(train , glm.fit)
cv.err$delta
test_result = predict(glm.fit,test)
predictions = test_result
actual = test$V6
mean((predictions - actual)^2)
glm.fit <- glm (V6 ~ ., data = train)
cv.err <- cv.glm(train , glm.fit, K = 5)
cv.err$delta
glm.fit <- glm (V6 ~ ., data = train)
cv.err <- cv.glm(train , glm.fit, K = 10)
cv.err$delta
#k=5 fold
set.seed (1)
cv.error.10 <- rep (0, 10)
for (i in 1:10) {
  glm.fit <- glm (V6 ~., data = train)
  cv.error.10[i] <- cv.glm(train , glm.fit , K = 5)$delta[1]
}
cv.error.10
#k = 10 fold
set.seed (1)
cv.error.10 <- rep (0, 10)
for (i in 1:10) {
  glm.fit <- glm (V6 ~ ., data = train)
  cv.error.10[i] <- cv.glm(train , glm.fit , K = 10)$delta[1]
}
cv.error.10
cv.error <- rep (0, 4)
for (i in 1:4) {
  glm.fit <- glm (V6 ~ poly (V2 , i), data = train)
  cv.error[i] <- cv.glm (train , glm.fit)$delta[1]
}
cv.error
cv.error <- rep (0, 4)
for (i in 1:4) {
  glm.fit <- glm (V6 ~ poly (V3 , i), data = train)
  cv.error[i] <- cv.glm (train , glm.fit)$delta[1]
}
cv.error
cv.error <- rep (0, 3)
for (i in 1:3) {
  glm.fit <- glm (V6 ~ poly (V4 , i), data = train)
  cv.error[i] <- cv.glm (train , glm.fit)$delta[1]
}
cv.error
cv.error <- rep (0, 4)
for (i in 1:4) {
  glm.fit <- glm (V6 ~ poly (V5 , i), data = train)
  cv.error[i] <- cv.glm(train , glm.fit)$delta[1]
}
cv.error
mul_non_linear = lm(V6 ~ V4+V2+V3+poly(V1,2,raw=T)+V5, data=train)
summary(mul_non_linear)
test_result_nl = predict(mul_non_linear,test)
predictions = test_result_nl
actual = test$V6
mean((predictions - actual)^2)
library (boot)
glm.fit <- glm (V6~V4+V2+V3+poly(V1,2,raw=T)+V5, data=train)
cv.err <- cv.glm(train , glm.fit)
cv.err$delta
set.seed (1)
glm.fit <- glm (V6~V4+V2+V3+poly(V1,2,raw=T)+V5, data=train)
cv.error <- cv.glm(train , glm.fit , K = 5)$delta[1]
cv.error
set.seed (1)
glm.fit <- glm (V6~V4+V2+V3+poly(V1,2,raw=T)+V5, data=train)
cv.error <- cv.glm(train , glm.fit , K = 10)$delta[1]
cv.error
x <- model.matrix (V6 ~ ., data = scale_data)[,-1]
y <- scale_data$V6
set.seed (1)
train <- sample (1:nrow(x),nrow(x)/2)
test <- (-train)
y.test <- y[test]
ridge.mod <- glmnet(x[train,], y[train], alpha = 0, thresh=1e-12)
ridge.pred <- predict(ridge.mod,s=4,newx = x[test,])
mean((ridge.pred-y.test)^2)
mean((mean(y[train])- y.test)^2)
ridge.pred <- predict (ridge.mod , s = 1e10 , newx = x[test , ])
mean ((ridge.pred - y.test)^2)
ridge.pred <- predict (ridge.mod , s = 0, newx = x[test , ],exact = T, x = x[train , ], y = y[train])
mean((ridge.pred - y.test)^2)
lm(y ~ x, subset = train)
predict (ridge.mod , s = 0, exact = T, type = "coefficients",
         x = x[train , ], y = y[train])[1:6, ]
library(glmnet)
set.seed (1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot (cv.out)
bestlam <- cv.out$lambda.min
bestlam

ridge.pred <- predict (ridge.mod , s = bestlam ,
                       newx = x[test , ])
mean((ridge.pred - y.test)^2)
out <- glmnet (x, y, alpha = 0)
predict (out , type = "coefficients", s = bestlam)[1:6, ]
#LASSO_Regression

lasso.mod <- glmnet (x[train , ], y[train], alpha = 1)
plot (lasso.mod)
set.seed (1)
cv.out <- cv.glmnet (x[train , ], y[train], alpha = 1)
plot (cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict (lasso.mod , s = bestlam ,
                       newx = x[test , ])
mean ((lasso.pred - y.test)^2)
#LAsso Regression has a mean = 0.481
out <- glmnet (x, y, alpha = 1)
lasso.coef <- predict (out , type = "coefficients",
                       s = bestlam)[1:6, ]
lasso.coef
#PCR model
#install.packages("pls")
set.seed (2)
pcr.fit <- pcr(V6 ~ ., data = scale_data , scale = TRUE ,validation = "CV")
summary(pcr.fit)
validationplot (pcr.fit , val.type = "MSEP")
set.seed (1)
pcr.fit <- pcr(V6 ~., data = scale_data , subset = train ,scale = TRUE , validation = "CV")
validationplot (pcr.fit , val.type = "MSEP")
pcr.pred <- predict (pcr.fit , x[test , ], ncomp = 5)
mean((pcr.pred - y.test)^2)
pcr.fit <- pcr(y ~ x, scale = TRUE , ncomp = 5)
summary (pcr.fit)
library (randomForest)
set.seed (1)
bag.airfoil <- randomForest(V6 ~ .,data = scale_data,subset = train,mtry =5, importance = TRUE)
bag.airfoil
yhat.bag <-predict(bag.airfoil,newdata = scale_data[-train , ])
airfoil.test <- scale_data[-train , "V6"]
plot(yhat.bag,airfoil.test)
abline(0, 1)
mean((yhat.bag - airfoil.test)^2)
#MSE Test = 0.1101373 for mtry = 5
importance(bag.airfoil)
#Every variable is important if removed one variable also test error increases.
bag.airfoil_2 <- randomForest(V6 ~ V1+V2+V3+V5,data = scale_data,subset = train,mtry =4, importance = TRUE)
yhat.bag <-predict(bag.airfoil_2,newdata = scale_data[-train , ])
airfoil.test <- scale_data[-train , "V6"]
mean((yhat.bag - airfoil.test)^2)
varImpPlot(bag.airfoil)
bag.airfoil_1 <- randomForest(V6 ~ .,data = scale_data,subset = train,mtry =2, importance = TRUE)
yhat.bag <-predict(bag.airfoil_1,newdata = scale_data[-train , ])
airfoil.test <- scale_data[-train , "V6"]
plot(yhat.bag,airfoil.test)
abline(0, 1)
mean((yhat.bag - airfoil.test)^2)
bag.airfoil_3 <- randomForest(V6 ~ .,data = scale_data,subset = train,mtry =5,ntree = 50)
yhat.bag <-predict(bag.airfoil_3,newdata = scale_data[-train , ])
airfoil.test <- scale_data[-train , "V6"]
mean((yhat.bag - airfoil.test)^2)
#BOOSTING
set.seed (1)
boost.airfoil <- gbm(V6 ~ ., data = scale_data[train , ],distribution = "gaussian", n.trees = 5000,interaction.depth = 4)
summary (boost.airfoil)
yhat.boost <- predict (boost.airfoil,newdata = scale_data[-train , ],n.trees = 5000)
mean((yhat.boost - airfoil.test)^2)
library(BART)
x <- scale_data[, 1:5]
y <- scale_data[, "V6"]
xtrain <- x[train , ]
ytrain <- y[train]
xtest <- x[-train , ]
ytest <- y[-train]
set.seed (1)
bartfit <- gbart (xtrain , ytrain , x.test = xtest)
yhat.bart <- bartfit$yhat.test.mean
mean((ytest - yhat.bart)^2)

