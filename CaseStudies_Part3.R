library(dplyr)
library(stats)
library(readr)
library(ranger)
library(Metrics)
library(caret)
library(xtable)
library(ggplot2)
library(e1071)
library(lattice)
#Data Preparation
X2022_02 <- read_csv(file ="2022-02.csv")
X2022_modif <- X2022_02[13:757,
                        c("sasdate", "CUMFNS","UNRATE","CPIAUCSL","FEDFUNDS","M2REAL","S&P 500")]

X2022_mod_dif <- X2022_modif[,c("CPIAUCSL","M2REAL","S&P 500")] %>% 
  mutate_all( function(x) 100*(x-lag(x))/lag(x))

X2022_mod_v2 <- cbind(X2022_modif,X2022_mod_dif)
colnames(X2022_mod_v2) <- c(colnames(X2022_modif),paste0("%",colnames(X2022_modif[,c("CPIAUCSL","M2REAL","S&P 500")])))

X2022_02_M <- X2022_mod_v2[,c("sasdate", "CUMFNS","UNRATE","%CPIAUCSL","FEDFUNDS","%M2REAL","%S&P 500")]

rm(X2022_mod_dif,X2022_mod_v2,X2022_modif)

#GDP
X2022_02_Q <- read_csv(file ="2022-02_Q.csv")

X2022_02_Q <- X2022_02_Q[3:254,c(1,2,61,121,160,244,35,145)]


project1data_diff <- X2022_02_Q[,c(2,4:6)] %>% 
  mutate_all( function(x) 100*(x-lag(x))/lag(x))

colnames(project1data_diff) <- paste0("%",colnames(project1data_diff))
project1data <- cbind(X2022_02_Q,project1data_diff)
X2022_02_Q <- project1data[,c("sasdate", "%GDPC1")]
rm(project1data_diff, project1data)


for (j in 1:4) {
  example<- assign(paste0("lags",as.character(j), sep = ""),X2022_02_M[-1,2:7])
  for (i in 2:7) {
    
    example[,i-1]<-lag(X2022_02_M[-1,i],n=j)
    
  }
  assign(paste0("lags",as.character(j), sep = ""),example)
}

#GDP
example<- assign(paste0("GDPlags",as.character(1), sep = ""),X2022_02_Q[,2])
example<-lag(X2022_02_Q[,2],n=1)
assign(paste0("GDPlags",as.character(1), sep = ""),example)
X2022_02_Q <- X2022_02_Q[-1,]
rownames(X2022_02_Q) <- 1:nrow(X2022_02_Q)

model_data <- data.frame(X2022_02_M[-1,], lags1, lags2, lags3, lags4)
rownames(model_data) <- 1:nrow(model_data)
rm(lags1,lags2,lags3,lags4, i,j,example, GDPlags1)

#Task a
#K=3
x3 <- model_data[seq(5,743, by=3),2:25]
y <- X2022_02_Q$`%GDPC1`[5:251]
y_dep <- X2022_02_Q$`%GDPC1`[4:250]
try3 <- data.frame(y_dep, x3)
Y <- as.matrix(y)
#rm( y)
nowcast_k3_lm <- lm(Y ~ ., data=try3)
summary(nowcast_k3_lm)

intercept <- rep(1, nrow(x3))
X3 <- as.matrix(cbind(intercept, y_dep, x3))
A3 <- solve(t(X3) %*% X3)
betas3 <- A3 %*% t(X3) %*% Y
res3<- Y-predict(nowcast_k3_lm,data=try3)
sigma3 <- sum(res3^2)/length(Y)

#K=2
x2 <- model_data[seq(5,743, by=3),2:19]
try2 <- data.frame(y_dep, x2)
#rm(x2)
nowcast_k2_lm <- lm(Y ~ ., data=try2)
summary(nowcast_k2_lm)

X2 <- as.matrix(cbind(intercept, y_dep, x2))
A2 <- solve(t(X2) %*% X2)
res2<- Y-predict(nowcast_k2_lm,data=try2)
sigma2 <- sum(res2^2)/length(Y)

#K=1
x1 <- model_data[seq(5,743, by=3),2:13]
try1 <- data.frame(y_dep, x1)
rm(x1)
nowcast_k1_lm <- lm(Y ~ ., data=try1)
summary(nowcast_k1_lm)
BIC(nowcast_k1_lm) 

X1 <- as.matrix(cbind(intercept, y_dep, x1))
A1 <- solve(t(X1) %*% X1)
res1<- Y-predict(nowcast_k1_lm,data=try1)
sigma1 <- sum(res1^2)/length(Y)


#From page 58 Modern Econometrics
BIC_m1_3 <- dim(A3)[1]/length(Y)*log(length(Y))+log(sigma3)
BIC_m1_2 <- dim(A2)[1]/length(Y)*log(length(Y))+log(sigma2)
BIC_m1_1 <- dim(A1)[1]/length(Y)*log(length(Y))+log(sigma1) 

#Nowcasting with K=3 (task a)
try3_check_a <- data.frame(model_data[seq(5,743, by=3),1], try3)
rownames(try3_check_a) <- 1:nrow(try3_check_a)
colnames(try3_check_a) <- c("sasdate", colnames(try3_check_a[,-1]))
rownames(try3) <- 1:nrow(try3)
X2022_02_Q_md <- X2022_02_Q[5:251, ]
rownames(X2022_02_Q_md) <- 1:nrow(X2022_02_Q_md)
nowcasts_1QA_k3_a <- c()
for (i in 26:246) {
  Y_a <- as.matrix(X2022_02_Q_md[1:i,2])
  nowcast_k3_model_a <- lm(Y_a ~ ., data=try3[1:i,])
  predict_lm_k3_a <- predict(nowcast_k3_model_a, try3[i+1,])
  nowcasts_1QA_k3_a <- append(nowcasts_1QA_k3_a,predict_lm_k3_a)
}

nowcasts_1QA_k3_a <- data.frame(nowcasts_1QA_k3_a)
rm(nowcast_k3_model_a, predict_lm_k3_a, Y_a,i)

rm(try3_check_a, try1, try2, try3)
#Task b
#K=3
x3_b <- model_data[seq(6,744, by=3),2:31]
try3_b <- data.frame(y_dep, x3_b)
nowcast_k3_lm_b <- lm(Y ~ ., data=try3_b)
summary(nowcast_k3_lm_b)

X3_b <- as.matrix(cbind(intercept, y_dep, x3_b))
A3_b <- solve(t(X3_b) %*% X3_b)
res3_b<- Y-predict(nowcast_k3_lm_b,data=try3_b)
sigma3_b <- sum(res3_b^2)/length(Y)


#K=2
x2_b <- model_data[seq(6,744, by=3),2:25]
try2_b <- data.frame(y_dep, x2_b)
nowcast_k2_lm_b <- lm(Y ~ ., data=try2_b)
summary(nowcast_k2_lm_b)

X2_b <- as.matrix(cbind(intercept, y_dep, x2_b))
A2_b <- solve(t(X2_b) %*% X2_b)
res2_b<- Y-predict(nowcast_k2_lm_b,data=try2_b)
sigma2_b <- sum(res2_b^2)/length(Y)

#K=1
x1_b <- model_data[seq(6,744, by=3),2:19]
try1_b <- data.frame(y_dep, x1_b)
nowcast_k1_lm_b <- lm(Y ~ ., data=try1_b)
summary(nowcast_k1_lm_b)

X1_b <- as.matrix(cbind(intercept, y_dep, x1_b))
A1_b <- solve(t(X1_b) %*% X1_b)
res1_b<- Y-predict(nowcast_k1_lm_b,data=try1_b)
sigma1_b <- sum(res1_b^2)/length(Y)

#From page 58 Modern Econometrics
BIC_m2_3 <-dim(A3_b)[1]/length(Y)*log(length(Y))+log(sigma3_b)
BIC_m2_2 <-dim(A2_b)[1]/length(Y)*log(length(Y))+log(sigma2_b)
BIC_m2_1 <-dim(A1_b)[1]/length(Y)*log(length(Y))+log(sigma1_b)

BIC_m1 <- data.frame(BIC_m1_1,BIC_m1_2, BIC_m1_3)
colnames(BIC_m1) <- c("K=1", "K=2", "K=3")
BIC_m2 <- data.frame(BIC_m2_1,BIC_m2_2, BIC_m2_3)
colnames(BIC_m2) <- c("K=1", "K=2", "K=3")
BIC_table <- cbind(c("Model 1", "Model 2"), rbind(BIC_m1, BIC_m2))

colnames(BIC_table) <- c("Model","K=1", "K=2", "K=3")
xtable(BIC_table, digits = 4)

#Nowcasting with K=3 (task b)
try3_check_b <- data.frame(model_data[seq(6,744, by=3),1], try3_b)
rownames(try3_check_b) <- 1:nrow(try3_check_b)
colnames(try3_check_b) <- c("sasdate", colnames(try3_check_b[,-1]))
rownames(try3_b) <- 1:nrow(try3_b)
nowcasts_1QA_k3_b <- c()
for (i in 32:246) {
  Y_b <- as.matrix(X2022_02_Q_md[1:i,2])
  nowcast_k3_model_b <- lm(Y_b ~ ., data=try3_b[1:i,])
  predict_lm_k3_b <- predict(nowcast_k3_model_b, try3_b[i+1,])
  nowcasts_1QA_k3_b <- append(nowcasts_1QA_k3_b,predict_lm_k3_b)
}

nowcasts_1QA_k3_b <- data.frame(nowcasts_1QA_k3_b)

#rm(try3_check_b, try1_b, try2_b, try3_b)

#Task c
summary(nowcast_k3_lm)$coefficients[summary(nowcast_k3_lm)$coefficients[,4]< 0.05,1]
#CUMFNS  UNRATE CUMFNS.1 X.S.P.500.1 UNRATE.2 FEDFUNDS.2  CUMFNS.3  FEDFUNDS.3 
x3_c <- model_data[seq(5,743, by=3),c("CUMFNS", "UNRATE",
                                      "CUMFNS.1", "X.S.P.500.1", "UNRATE.2",
                                      "FEDFUNDS.2", "CUMFNS.3", "FEDFUNDS.3")]
try3_c <- data.frame(y_dep, x3_c)

summay_k3_m1 <- summary(nowcast_k3_lm)$coefficients
xtable(summay_k3_m1, digits=3)

nowcast_k3_lm_c <- lm(Y ~ ., data=try3_c)
summary(nowcast_k3_lm_c)

try3_check_c <- data.frame(model_data[seq(5,743, by=3),1], try3_c)
rownames(try3_check_c) <- 1:nrow(try3_check_c)
colnames(try3_check_c) <- c("sasdate", colnames(try3_check_c[,-1]))
rownames(try3_c) <- 1:nrow(try3_c)
nowcasts_1QA_k3_c <- c()
for (i in 10:246) {
  Y_c <- as.matrix(X2022_02_Q_md[1:i,2])
  nowcast_k3_model_c <- lm(Y_c ~ ., data=try3_c[1:i,])
  predict_lm_k3_c <- predict(nowcast_k3_model_c, try3_c[i+1,])
  nowcasts_1QA_k3_c <- append(nowcasts_1QA_k3_c,predict_lm_k3_c)
}

nowcasts_1QA_k3_c <- data.frame(nowcasts_1QA_k3_c)

rm(nowcast_k3_model_c,predict_lm_k3_c, x3_c, Y_c, try3_c, try3_check_c)

plot(as.Date(X2022_02_Q_md$sasdate[11:247], "%m/%d/%Y"),  X2022_02_Q_md$`%GDPC1`[11:247],type = "l", lty=2,col=9 ,xlab="Dates", ylab="One-quarter GDP Growth (%)", ylim=c(-10,10))
#K=3 model nowcast 1966Q4-2021Q4 
lines(as.Date(X2022_02_Q_md$sasdate[27:247], "%m/%d/%Y"),  nowcasts_1QA_k3_a$nowcasts_1QA_k3_a,type = "l", lty=1, col=2)
lines(as.Date(X2022_02_Q_md$sasdate[11:247], "%m/%d/%Y"),  nowcasts_1QA_k3_c$nowcasts_1QA_k3_c,type = "l", lty=1, col=4)
legend(x="bottomleft", y=-5, legend=c("Actual GDP growth" ,"Model 1","Model 1 (Reduced)" )
       , col=c(9,2,4),lty=c(2,1,1), bty = "n")

#Task d
summary(nowcast_k3_lm_b)$coefficients[summary(nowcast_k3_lm_b)$coefficients[,4]< 0.05,1]
# CUMFNS    UNRATE.1    CUMFNS.2  FEDFUNDS.2 X.S.P.500.2  UNRATE.3 FEDFUNDS.3    CUMFNS.4  FEDFUNDS.4 
#K=3
x3_d <- model_data[seq(6,744, by=3),c("CUMFNS", "UNRATE.1", "CUMFNS.2", "FEDFUNDS.2", 
                                      "X.S.P.500.2", "UNRATE.3", "FEDFUNDS.3", "CUMFNS.4",
                                      "FEDFUNDS.4" )]


summary_k3_m2 <- summary(nowcast_k3_lm_b)$coefficients
xtable(summary_k3_m2, digits=3)


try3_d <- data.frame(y_dep, x3_d)

nowcast_k3_lm_d <- lm(Y ~ ., data=try3_d)
summary(nowcast_k3_lm_d)
BIC(nowcast_k3_lm_d) 

try3_check_d <- data.frame(model_data[seq(5,743, by=3),1], try3_d)
rownames(try3_check_d) <- 1:nrow(try3_check_d)
colnames(try3_check_d) <- c("sasdate", colnames(try3_check_d[,-1]))
rownames(try3_d) <- 1:nrow(try3_d)
nowcasts_1QA_k3_d <- c()

for (i in 11:246) {
  Y_d <- as.matrix(X2022_02_Q_md[1:i,2])
  nowcast_k3_model_d <- lm(Y_d ~ ., data=try3_d[1:i,])
  predict_lm_k3_d <- predict(nowcast_k3_model_d, try3_d[i+1,])
  nowcasts_1QA_k3_d <- append(nowcasts_1QA_k3_d,predict_lm_k3_d)
}

nowcasts_1QA_k3_d <- data.frame(nowcasts_1QA_k3_d)

rm(nowcast_k3_model_d,predict_lm_k3_d, x3_d, Y_d, try3_d, try3_check_d, i)


plot(as.Date(X2022_02_Q_md$sasdate[12:247], "%m/%d/%Y"),  X2022_02_Q_md$`%GDPC1`[12:247],type = "l", lty=2,col=9, lwd=1, xlab="Dates", ylab="One-quarter GDP Growth (%)", ylim=c(-10,10))
#K=3 model nowcast 1968Q2-2021Q4 
lines(as.Date(X2022_02_Q_md$sasdate[33:247], "%m/%d/%Y"),  nowcasts_1QA_k3_b$nowcasts_1QA_k3_b,type = "l", lty=1, col="red" , lwd=1)
lines(as.Date(X2022_02_Q_md$sasdate[12:247], "%m/%d/%Y"),  nowcasts_1QA_k3_d$nowcasts_1QA_k3_d,type = "l", lty=1, col=4, lwd=1)
legend(x="bottomleft", y=-5, legend=c("Actual GDP growth" ,"Model 2","Model 2 (Reduced)" )
       , col=c(9,2,4),lty=c(2,1,1), bty = "n")
#Task e

## RF with all variables in task a
try3_e <- data.frame(y_dep, x3)
try3_check_e <- data.frame(model_data[seq(5,743, by=3),1], try3_e)
rownames(try3_check_e) <- 1:nrow(try3_check_e)
colnames(try3_check_e) <- c("sasdate", colnames(try3_check_e[,-1]))
rownames(try3_e) <- 1:nrow(try3_e)

set.seed(420)

nowcasts_1QA_ranger_k3_e <- c()

for (i in 26:246) {
  Y_e <- as.matrix(X2022_02_Q_md[1:i,2])
  nowcast_k3_ranger_e <- ranger(formula = Y_e ~ . , data=try3_e[1:i,], importance="permutation", seed=420, num.tree=1000)
  predict_ranger_k3_e <- predict(nowcast_k3_ranger_e, try3_e[i+1,],type="response")
  nowcasts_1QA_ranger_k3_e <- append(nowcasts_1QA_ranger_k3_e,predict_ranger_k3_e$predictions)
}

nowcasts_1QA_ranger_k3_e <- data.frame(nowcasts_1QA_ranger_k3_e)

rm(nowcast_k3_ranger_e,predict_ranger_k3_e, try3_check_e, i)

#For hyperparameter tuning
#Expanding window

tunegrid2 <- expand.grid(mtry = c(2, 5, 8,10, 25),
                         splitrule = c("variance"),
                         min.node.size = c(5, 10, 15)
)
nowcasts_1QA_ranger_k3_e_tuned_2 <- c()
tune_2 <c()
for (i in 26:246) {
  Y_e <- as.matrix(X2022_02_Q_md[1:i,2])
  print(i)
  tune_2 <- train(try3_e[1:i,], as.numeric(Y_e),
                  method = "ranger", # Requires: e1071, ranger, dplyr
                  metric = "RMSE",
                  tuneGrid = tunegrid2,
                  ntree = 1000,
                  trControl = trainControl(
                    method = "timeslice",
                    initialWindow = 25,
                    horizon = 1,
                    fixedWindow = FALSE )
  )
  nowcast_k3_ranger_e <- ranger(formula = Y_e ~ . , data=try3_e[1:i,], 
                                importance="permutation", seed=420,
                                mtry = tune_2$results[tune_2$results$RMSE == min(tune_2$results$RMSE),1],
                                splitrule = "variance", 
                                min.node.size = tune_2$results[tune_2$results$RMSE == min(tune_2$results$RMSE),3])
  predict_ranger_k3_e <- predict(nowcast_k3_ranger_e, try3_e[i+1,],type="response")
  nowcasts_1QA_ranger_k3_e_tuned_2 <- append(nowcasts_1QA_ranger_k3_e_tuned_2,predict_ranger_k3_e$predictions)
}

nowcasts_1QA_ranger_k3_e_tuned_2 <- data.frame(nowcasts_1QA_ranger_k3_e_tuned_2)

plot(as.Date(X2022_02_Q_md$sasdate[11:247], "%m/%d/%Y"),  X2022_02_Q_md$`%GDPC1`[11:247],type = "l", lty=2,col=9 ,xlab="Dates", ylab="One-quarter GDP Growth (%)", ylim=c(-10,10))
#K=3 model nowcast 1966Q4-2021Q4 
lines(as.Date(X2022_02_Q_md$sasdate[11:247], "%m/%d/%Y"),  nowcasts_1QA_k3_c$nowcasts_1QA_k3_c,type = "l", lty=1, col="blue")
lines(as.Date(X2022_02_Q_md$sasdate[27:247], "%m/%d/%Y"),  nowcasts_1QA_ranger_k3_e_tuned_2$nowcasts_1QA_ranger_k3_e_tuned_2,type = "l", lty=1, col="darkred")
legend(x="bottomleft", y=-5, legend=c("Actual GDP growth" ,"Model 1 (Reduced)","Random Forest 1" )
       , col=c(9,"blue",  "darkred"),lty=c(2,1,1), bty = "n")

rmse_RF_1 <- rmse(X2022_02_Q_md$`%GDPC1`[27:247],nowcasts_1QA_ranger_k3_e$nowcasts_1QA_ranger_k3_e)
rmse_RF_1_tuned <- rmse(X2022_02_Q_md$`%GDPC1`[27:247],nowcasts_1QA_ranger_k3_e_tuned_2$nowcasts_1QA_ranger_k3_e_tuned_2)


#Task f

## RF with all variables in task b
set.seed(420)
try3_f <- data.frame(y_dep, x3_b)
try3_check_f <- data.frame(model_data[seq(6,744, by=3),1], try3_f)
rownames(try3_check_f) <- 1:nrow(try3_check_f)
colnames(try3_check_f) <- c("sasdate", colnames(try3_check_f[,-1]))
rownames(try3_check_f) <- 1:nrow(try3_check_f)

nowcasts_1QA_ranger_k3_f <- c()

for (i in 32:246) {
  Y_f <- as.matrix(X2022_02_Q_md[1:i,2])
  nowcast_k3_ranger_f <- ranger(formula = Y_f ~ . , data=try3_f[1:i,], importance="permutation", seed=420, num.trees = 1000)
  predict_ranger_k3_f <- predict(nowcast_k3_ranger_f, try3_f[i+1,],type="response")
  nowcasts_1QA_ranger_k3_f <- append(nowcasts_1QA_ranger_k3_f,predict_ranger_k3_f$predictions)
}

nowcasts_1QA_ranger_k3_f <- data.frame(nowcasts_1QA_ranger_k3_f)

#Expanding window

tunegrid2_f <- expand.grid(mtry = c(2, 6, 10, 25),
                           splitrule = c("variance"),
                           min.node.size = c(5, 10, 15)
)
nowcasts_1QA_ranger_k3_f_tuned_2 <- c()
tune_1_f <c()
for (i in 32:246) {
  Y_f <- as.matrix(X2022_02_Q_md[1:i,2])
  print(i)
  tune_1_f <- train(try3_f[1:i,], as.numeric(Y_f),
                    method = "ranger", # Requires: e1071, ranger, dplyr
                    metric = "RMSE",
                    tuneGrid = tunegrid2_f,
                    ntree = 1000,
                    trControl = trainControl(
                      method = "timeslice",
                      initialWindow = 31,
                      horizon = 1,
                      fixedWindow = FALSE )
  )
  nowcast_k3_ranger_f <- ranger(formula = Y_f ~ . , data=try3_f[1:i,], 
                                importance="permutation", seed=420,
                                mtry = tune_1_f$results[tune_1_f$results$RMSE == min(tune_1_f$results$RMSE),1],
                                splitrule = "variance", 
                                min.node.size = tune_1_f$results[tune_1_f$results$RMSE == min(tune_1_f$results$RMSE),3])
  predict_ranger_k3_f <- predict(nowcast_k3_ranger_f, try3_f[i+1,],type="response")
  nowcasts_1QA_ranger_k3_f_tuned_2 <- append(nowcasts_1QA_ranger_k3_f_tuned_2,predict_ranger_k3_f$predictions)
}



nowcasts_1QA_ranger_k3_f_tuned_2 <- data.frame(nowcasts_1QA_ranger_k3_f_tuned_2)


plot(as.Date(X2022_02_Q_md$sasdate[12:247], "%m/%d/%Y"),  X2022_02_Q_md$`%GDPC1`[12:247],type = "l", lty=2,col=9 ,xlab="Dates", ylab="One-quarter GDP Growth (%)", ylim=c(-10,10))
#K=3 model nowcast 1966Q4-2021Q4 
lines(as.Date(X2022_02_Q_md$sasdate[12:247], "%m/%d/%Y"),  nowcasts_1QA_k3_d$nowcasts_1QA_k3_d,type = "l", lty=1, col="blue")
lines(as.Date(X2022_02_Q_md$sasdate[33:247], "%m/%d/%Y"),  nowcasts_1QA_ranger_k3_f_tuned_2$nowcasts_1QA_ranger_k3_f_tuned_2,type = "l", lty=1, col="darkred")
legend(x="bottomleft", y=-5, legend=c("Actual GDP growth" ,"Model 2 (Reduced)","Random Forest 2" )
       , col=c(9,"blue", "darkred"),lty=c(2,1,1), bty = "n")

rmse_RF_2 <- rmse(X2022_02_Q_md$`%GDPC1`[33:247],nowcasts_1QA_ranger_k3_f$nowcasts_1QA_ranger_k3_f)
rmse_RF_2_tuned <- rmse(X2022_02_Q_md$`%GDPC1`[33:247],nowcasts_1QA_ranger_k3_f_tuned_2$nowcasts_1QA_ranger_k3_f_tuned_2)

rm(nowcast_k3_ranger_f, predict_ranger_k3_f, Y_f,i, try3_f, try3_check_f)
rm(Y_e)

#RMSFEs
forecasts_m1 <- data.frame(X2022_02_Q_md$sasdate[27:247], nowcasts_1QA_k3_a)
colnames(forecasts_m1) <- c("sasdate", "model1")
forecasts_m2 <- data.frame(X2022_02_Q_md$sasdate[33:247], nowcasts_1QA_k3_b)
colnames(forecasts_m2) <- c("sasdate", "model2")
forecasts_m1_reduced <- data.frame(X2022_02_Q_md$sasdate[11:247], nowcasts_1QA_k3_c)
colnames(forecasts_m1_reduced) <- c("sasdate", "model1_reduced")
forecasts_m2_reduced <- data.frame(X2022_02_Q_md$sasdate[12:247], nowcasts_1QA_k3_d)
colnames(forecasts_m2_reduced) <- c("sasdate", "model2_reduced")
forecasts_UMIDAS <- merge(forecasts_m1_reduced, forecasts_m2_reduced, by = "sasdate", all=TRUE) 
forecasts_UMIDAS<- merge(forecasts_UMIDAS,forecasts_m1, by = "sasdate", all=TRUE) 
forecasts_UMIDAS<- merge(forecasts_UMIDAS, forecasts_m2, by = "sasdate", all = TRUE) 
forecasts_UMIDAS<- merge(forecasts_UMIDAS, X2022_02_Q_md, by = "sasdate", all = TRUE) 
forecasts_UMIDAS$sasdate <- as.Date(forecasts_UMIDAS$sasdate, "%m/%d/%Y")


forecasts_AR_VAR <- read_csv(file = "AR-VAR-forecasts.csv")
forecasts_AR_VAR <- forecasts_AR_VAR[, c(1,3,6,9)]
colnames(forecasts_AR_VAR) <- c("sasdate", "AR(1)", "VAR(1)", "VAR(3)")
forecasts_rf <- read_csv(file = "forecast_ranger.csv")
colnames(forecasts_rf) <- c("sasdate", "RF_GDP", "RF_All")


forecasts_UMIDAS<- merge(forecasts_UMIDAS, forecasts_AR_VAR, by = "sasdate", all = TRUE) 
forecasts_UMIDAS<- merge(forecasts_UMIDAS, forecasts_rf, by = "sasdate", all = TRUE) 


forecasts_UMIDAS <- forecasts_UMIDAS[order(as.Date(forecasts_UMIDAS$sasdate, format="%m/%d/%Y")),]
colnames(forecasts_UMIDAS) <- c("date", "model1_reduced", "model2_reduced", "model1", "model2", "GDP",
                                "AR(1)", "VAR(1)", "VAR(3)", "RF_GDP", "RF_All")
rownames(forecasts_UMIDAS) <- 1:nrow(forecasts_UMIDAS)
forecasts_UMIDAS[is.na(forecasts_UMIDAS)] <- 0


rmse_model1 <- rmse(forecasts_UMIDAS$GDP[32:252],forecasts_UMIDAS$model1[32:252])
rmse_model1_reduced <- rmse(forecasts_UMIDAS$GDP[16:252],forecasts_UMIDAS$model1_reduced[16:252])
rmse_model2 <- rmse(forecasts_UMIDAS$GDP[38:252],forecasts_UMIDAS$model2[38:252])
rmse_model2_reduced <- rmse(forecasts_UMIDAS$GDP[17:252],forecasts_UMIDAS$model2_reduced[17:252])
rmse_ar1 <- rmse(forecasts_UMIDAS$GDP[5:252],forecasts_UMIDAS$`AR(1)`[5:252])
rmse_var1 <- rmse(forecasts_UMIDAS$GDP[11:252],forecasts_UMIDAS$`VAR(1)`[11:251])
rmse_var3 <- rmse(forecasts_UMIDAS$GDP[27:252],forecasts_UMIDAS$`VAR(3)`[27:252])
rmse_rf_gdp <- rmse(forecasts_UMIDAS$GDP[13:252],forecasts_UMIDAS$RF_GDP[13:252])
rmse_rf_all <- rmse(forecasts_UMIDAS$GDP[13:252],forecasts_UMIDAS$RF_All[13:252])

RMSE <- data.frame("RMSE",rmse_model1, rmse_model1_reduced, rmse_model2, rmse_model2_reduced, rmse_ar1, rmse_var1, rmse_var3,
                   rmse_rf_gdp, rmse_rf_all, rmse_RF_1, rmse_RF_1_tuned, rmse_RF_2, rmse_RF_2_tuned)

colnames(RMSE)<- c("RMSE","Model 1","Model 1 (Reduced)", "Model 2","Model 2 (Reduced)",
                   "AR(1) Model","VAR(1) Model","VAR(3) Model", "RF_GDP", "RF_All", "RF 1", "RF 1 - Tuned",
                   "RF 2", "RF 2 - Tuned")

RMSE_RF <- RMSE[,11:14]
xtable(RMSE_RF, digits = 3)
  
library(data.table)
RMSE <- t(RMSE[,-1] )
RMSE <- setDT(data.frame(RMSE), keep.rownames = TRUE)[]
colnames(RMSE) <- c("Model", "RMSFE")
RMSE <- RMSE %>% arrange(.,by=desc(RMSFE) )
xtable(RMSE, digits = 3)



#RMSE graphs

#RMSFEs
rmse_model_2 <- c()
rmse_model_2_RF <- c()
for (i in 33:247){
  rmse_model_2<- append(rmse_model_2,rmse(X2022_02_Q_md$`%GDPC1`[33:i],nowcasts_1QA_k3_b[1:(i-32),]))
  rmse_model_2_RF<- append(rmse_model_2_RF,rmse(X2022_02_Q_md$`%GDPC1`[33:i],nowcasts_1QA_ranger_k3_f_tuned_2[1:(i-32),]))
}

rmse_dt_1 <- data.frame(X2022_02_Q_md$sasdate[33:247], rmse_model_2,rmse_model_2_RF)
colnames(rmse_dt_1) <- c("date", "rmse_2", "rmse_2_RF")
rmse_dt_1$date <- as.Date(rmse_dt_1$date, "%m/%d/%Y")

rmse_model_1 <- c()
rmse_model_1_RF <- c()

for (i in 27:247){
  rmse_model_1<- append(rmse_model_1,rmse(X2022_02_Q_md$`%GDPC1`[27:i],nowcasts_1QA_k3_a[1:(i-26),]))
  rmse_model_1_RF<- append(rmse_model_1_RF,rmse(X2022_02_Q_md$`%GDPC1`[27:i],nowcasts_1QA_ranger_k3_e_tuned_2[1:(i-26),]))
}
rmse_dt_2 <- data.frame(X2022_02_Q_md$sasdate[27:247], rmse_model_1,rmse_model_1_RF)
colnames(rmse_dt_2) <- c("date", "rmse_1", "rmse_1_RF")
rmse_dt_2$date <- as.Date(rmse_dt_2$date, "%m/%d/%Y")

rmse_model1_sign <- c()

for (i in 11:247){
  rmse_model1_sign<- append(rmse_model1_sign,rmse(X2022_02_Q_md$`%GDPC1`[11:i],nowcasts_1QA_k3_c[1:(i-10),]))
}

rmse_model2_sign <- c()
for (i in 12:247){
  rmse_model2_sign<- append(rmse_model2_sign,rmse(X2022_02_Q_md$`%GDPC1`[12:i],nowcasts_1QA_k3_d[1:(i-11),]))
}
rmse_dt_3 <- data.frame(X2022_02_Q_md$sasdate[11:247], rmse_model1_sign)
colnames(rmse_dt_3) <- c("date", "rmse_model1_sign")
rmse_dt_3$date <- as.Date(rmse_dt_3$date, "%m/%d/%Y")

rmse_dt_4 <- data.frame(X2022_02_Q_md$sasdate[12:247], rmse_model2_sign)
colnames(rmse_dt_4) <- c("date", "rmse_model2_sign")
rmse_dt_4$date <- as.Date(rmse_dt_4$date, "%m/%d/%Y")

plot(rmse_dt_3$date[-(1:23)],  rmse_dt_3$rmse_model1_sign[-(1:23)],type = "l", lty=1, xlab="Dates", ylab="RMSFE",ylim=c(0,2.1), col="red", yaxp = c(0, 2, 2))
lines(rmse_dt_4$date[-c(1:22)],  rmse_dt_4$rmse_model2_sign[-c(1:22)],type = "l", lty=1, ylim=c(0,2.1), col="blue")
lines(rmse_dt_1$date,  rmse_dt_1$rmse_2_RF,type = "l", lty=1, ylim=c(0,2.1), col="green")
lines(rmse_dt_2$date[-c(1:7)],  rmse_dt_2$rmse_1_RF[-c(1:7)],type = "l", lty=2, ylim=c(0,2.1), col="purple")
grid(ny = NULL, nx = NA, "grey", lwd = 1) # grid only in y-direction
legend(x="topright", y=-5, legend=c( "Model 1 (Reduced)","Model 2 (Reduced)","Random Forest (Model 2)","Random Forest (Model 1)"  )
       , col=c("red", "blue", "green", "purple"),lty=c(1,1,2,1), cex = 0.7)
