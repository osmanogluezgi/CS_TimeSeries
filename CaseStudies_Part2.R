#Project 2
library(readr)
library(dplyr)
library(rpart)
library(rpart.plot)
library(Metrics)
library(ranger)
library(ggplot2)
library(forcats) #fct_reorder

X2022_02 <- read_csv(file ="2022-02.csv")

X2022_02 <- X2022_02[3:254,c(1,2,61,121,160,244,35,145)]

#Growth Rates
project1data_diff <- X2022_02[,c(2,4:6)] %>% 
  mutate_all( function(x) 100*(x-lag(x))/lag(x))

project1data <- cbind(X2022_02,project1data_diff)
colnames(project1data_diff) <- paste0("%",colnames(project1data[,c(2,4:6)]))

project1data_final <- project1data[-1,c(1,3,7:12)]
head(project1data_final,10)

#Task a
for (j in 1:10) {
  example<- assign(paste0("lags",as.character(j), sep = ""),project1data_final[2:8])
  for (i in 2:8) {
    
    example[,i-1]<-lag(project1data_final[,i],n=j)
    
  }
  assign(paste0("lags",as.character(j), sep = ""),example)
}

model_data <- data.frame(project1data_final, lags1, lags2, lags3, lags4, lags5, lags6, lags7, lags8, lags9, lags10)
rownames(model_data) <- 1:nrow(model_data)

model1 <- rpart(formula = GDPC1 ~ GDPC1.1+GDPC1.2+GDPC1.3+GDPC1.4+GDPC1.5+GDPC1.6+GDPC1.7+GDPC1.8+GDPC1.9+GDPC1.10, 
                data=model_data[11:251,])
summary(model1)

model1_importance <- stack(model1$variable.importance)

colnames(model1_importance) <- c("Importance", "Variable Name")

xtable(model1_importance, digits = 3)

rpart.plot(model1)

#Task b
model2 <- rpart(formula = GDPC1 ~ . , data= model_data[11:251,-c(1:4,6:8)])
summary(model2)
rpart.plot(model2)

#Task c
rpart_1QA_m1 <- c()
for (i in 11:250) {
  rpart_m1 <- rpart(formula = GDPC1 ~GDPC1.1+GDPC1.2+GDPC1.3+GDPC1.4+GDPC1.5+GDPC1.6+GDPC1.7+GDPC1.8+GDPC1.9+GDPC1.10, data=model_data[11:i,])
  forecasts_rpart_m1 <- predict(rpart_m1, model_data[i+1,c(12,19,26,33,40,47,54,61,68,75)],type="vector")
  rpart_1QA_m1 <- append(rpart_1QA_m1,forecasts_rpart_m1)
}
rmse_model1 <- c()
for (i in 1:240){
  
  rmse_model1 <- append(rmse_model1,rmse(model_data[c(12:(i+11)),5],rpart_1QA_m1[1:i]))
}


rmse_dt_1 <- data.frame(project1data_final$sasdate[12:251], rmse_model1)
colnames(rmse_dt_1) <- c("dates", "rmse")
rmse_dt_1$dates <- as.Date(rmse_dt_1$dates, "%m/%d/%Y")


plot(rmse_dt_1$dates,  rmse_dt_1$rmse,type = "l", lty=1, xlab="Dates", ylab="RMSE_DT", ylim=c(0,4))

plot(as.Date(model_data$sasdate, "%m/%d/%Y"),  model_data$GDPC1,type = "l", lty=2, xlab="Dates", ylab="One-quarter GDP Growth (%)", ylim = c(-6,6))
lines(rmse_dt_1$dates,  rpart_1QA_m1,type = "l", lty=1, col="red")


#DT Model with ALL variables
rpart_1QA_m2 <- c()
for (i in 11:250) {
  rpart_m2 <- rpart(formula = GDPC1 ~ . , data=model_data[11:i,-c(1:4,6:8)])
  forecasts_rpart_m2 <- predict(rpart_m2, model_data[i+1,-c(1,5)])
  rpart_1QA_m2 <- append(rpart_1QA_m2,forecasts_rpart_m2)
}


rmse_model2 <- c()
for (i in 1:240){
  
  rmse_model2 <- append(rmse_model2,rmse(model_data[c(12:(i+11)),5],rpart_1QA_m2[1:i]))
}

rmse_dt_2 <- data.frame(project1data_final$sasdate[12:251], rmse_model2)
colnames(rmse_dt_2) <- c("dates", "rmse")
rmse_dt_2$dates <- as.Date(rmse_dt_2$dates, "%m/%d/%Y")


plot(as.Date(model_data$sasdate[12:251], "%m/%d/%Y"),  model_data$GDPC1[12:251],type = "l", lty=2, xlab="Dates", ylab="One-quarter GDP Growth (%)")
lines(rmse_dt_1$dates,  rpart_1QA_m1,type = "l", lty=1, col="red")
lines(rmse_dt_2$dates,  rpart_1QA_m2,type = "l", lty=1, col="blue")
legend(x="bottomleft", y=-5, legend=c("Actual Growth Rates", "Decision Tree (Only GDP)","Decision Tree (All Variables)")
       , col=c("darkgrey", "red", "blue"),lty=c(2,1,1), cex=0.8)


plot(rmse_dt_1$dates,  rmse_dt_1$rmse,type = "l", lty=1, xlab="Dates", ylab="RMSFE",ylim=c(0,4), col="red")
lines(rmse_dt_2$dates,  rmse_dt_2$rmse,type = "l", lty=1,  ylim=c(0,4), col = "blue")
legend(x="topleft", y=-5, legend=c( "Decision Tree (Only GDP)","Decision Tree (All Variables)")
       , col=c( "red", "blue"),lty=c(1,1), cex=0.8)


#Task d

## RF with all variables
set.seed(420)
model3 <- ranger(formula = GDPC1 ~ . , 
                 data= model_data[11:251,-c(1:4,6:8)], importance="permutation", seed=420, mtry = 23)

df <- stack(model3$variable.importance)
df_plot <- df%>%
  filter(values >= 0.0193 | values< 0) 
df_plot%>%
  mutate(ind = fct_reorder(ind, values)) %>%
  ggplot( aes(y=values, x=ind))+
  geom_col()+
  coord_flip()+
  ylab("Values")+
  xlab("Variables")

 
#Task e
#RF Forecasts
ranger_1QA_m2 <- c()
for (i in 11:250) {
  ranger_m2 <- ranger(formula = GDPC1 ~ . , data=model_data[11:i,-c(1:4,6:8)], importance="permutation", seed=420, mtry = 23)
  forecasts_ranger_m2 <- predict(ranger_m2, model_data[i+1,-c(1:4,6:8)],type="response")
  ranger_1QA_m2 <- append(ranger_1QA_m2,forecasts_ranger_m2$predictions)
}

forecast_ranger_all <- data.frame(ranger_1QA_m2)

ranger_1QA_m1 <- c()
for (i in 11:250) {
  ranger_m1 <- ranger(formula = GDPC1 ~ GDPC1.1+GDPC1.2+GDPC1.3+GDPC1.4+GDPC1.5+GDPC1.6+GDPC1.7+GDPC1.8+GDPC1.9+GDPC1.10, data=model_data[11:i,], importance="permutation", seed=420, mtry = 3)
  forecasts_ranger_m1 <- predict(ranger_m1, model_data[i+1,c(12,19,26,33,40,47,54,61,68,75)],type="response")
  ranger_1QA_m1 <- append(ranger_1QA_m1,forecasts_ranger_m1$predictions)
}

forecast_ranger_GDP <- data.frame(ranger_1QA_m1)

plot(as.Date(model_data$sasdate[12:251], "%m/%d/%Y"),  model_data$GDPC1[12:251],type = "l", lty=2, xlab="Dates", ylab="One-quarter GDP Growth (%)", ylim=c(-6,6))
lines(as.Date(model_data$sasdate[12:251], "%m/%d/%Y"),  forecast_ranger_GDP$ranger_1QA_m1,type = "l", lty=1, col="red")
lines(as.Date(model_data$sasdate[12:251], "%m/%d/%Y"),  forecast_ranger_all$ranger_1QA_m2,type = "l", lty=1, col="blue")
legend(x="bottomleft", y=-5, legend=c( "Actual Growth Rates", "Random Forest (Only GDP)","Random Forest (All Variables)")
       , col=c("darkgrey", "red", "blue"),lty=c(2,1,1), cex=0.8)

#RMSEs
rmse_model_rf_1 <- c()
for (i in 1:240){
  
  rmse_model_rf_1 <- append(rmse_model_rf_1,rmse(model_data[c(12:(i+11)),5],forecast_ranger_GDP[1:i,]))
}
rmse_dt_rf_1 <- data.frame(project1data_final$sasdate[12:251], rmse_model_rf_1)
colnames(rmse_dt_rf_1) <- c("dates", "rmse")
rmse_dt_rf_1$dates <- as.Date(rmse_dt_rf_1$dates, "%m/%d/%Y")

rmse_model_rf_2 <- c()
for (i in 1:240){
  
  rmse_model_rf_2 <- append(rmse_model_rf_2,rmse(model_data[c(12:(i+11)),5],forecast_ranger_all[1:i,]))
}

rmse_dt_rf_2 <- data.frame(project1data_final$sasdate[12:251], rmse_model_rf_2)
colnames(rmse_dt_rf_2) <- c("dates", "rmse")
rmse_dt_rf_2$dates <- as.Date(rmse_dt_rf_2$dates, "%m/%d/%Y")

plot(rmse_dt_1$dates,  rmse_dt_1$rmse,type = "l", lty=1,  xlab="Dates", ylab="RMSFE",ylim=c(0,4), col=1)
lines(rmse_dt_2$dates,  rmse_dt_2$rmse,type = "l", lty=1,  ylim=c(0,4), col = 2)
lines(rmse_dt_rf_1$dates,  rmse_dt_rf_1$rmse,type = "l", lty=1, ylim=c(0,4), col=3)
lines(rmse_dt_rf_2$dates,  rmse_dt_rf_2$rmse,type = "l", lty=1, ylim=c(0,4), col=7)
legend(x="topleft", y=-5, legend=c( "Decision Tree (Only GDP)","Decision Tree (All Variables)","Random Forest (Only GDP)","Random Forest (All Variables)" )
       , col=c(1,2,3,7),lty=c(1,1,1), cex=0.8)

#plot AR(1) and RF (all) RMSFE in the same plot to compare the best two models from the both points of view
rmse_var_df_1 <- read_csv(file = "rmse_var_df_1.csv")

plot(rmse_dt_2$dates,  rmse_dt_2$rmse,type = "l", lty=1,  xlab="Dates", ylab="RMSFE", ylim=c(0,4), col = "blue")
lines(rmse_var_df_1$dates,  rmse_var_df_1$rmse,type = "l", lty=2, ylim=c(0,4), col = "red")
legend(x="topleft", y=-5, legend=c( "Random Forest (All Variables)" , "VAR(1)")
       , col=c("blue","red"),lty=c(1,2), cex=0.72)





