# Project Tasks 

library(dplyr)
library(readr)
library(stargazer)
library(fpp2)
library(stats)
library(Metrics)
library(lubridate)
library(xts)
library(vars)
library(gridExtra)
library(xtable)



X2022_02 <- read_csv(file ="2022-02.csv")

X2022_02 <- X2022_02[3:254,c(1,2,61,121,160,244,35,145)]
colnames(X2022_02)
str(X2022_02)
#Growth Rates
project1data_diff <- X2022_02[,c(2,4:6)] %>% 
  mutate_all( function(x) 100*(x-lag(x))/lag(x))

colnames(project1data_diff) <- paste0("%",colnames(project1data[,c(2,4:6)]))

project1data <- cbind(X2022_02,project1data_diff)
project1data_final <- project1data[,c(1,3,7:12)]
head(project1data_final,10)

#Descriptive Statistics

stargazer(project1data_final, digits = 2, median = TRUE)


##CUMFNS
annotations1 <- data.frame(
  x = c(round(min(project1data_final$CUMFNS), 2), 
        round(mean(project1data_final$CUMFNS), 2), 
        round(max(project1data_final$CUMFNS), 2), 
        round(median(project1data_final$CUMFNS), 2)),
  label = c("Min:", "Mean:", "Max:", "Median:")
) 

hist1 <- ggplot(project1data_final, aes(x = CUMFNS))+
  geom_histogram(bins=20,color = "white", fill = "gray")+
  geom_text(data = annotations1, 
            aes(x = c(64,80,91,80), y = c(3,12,8,8),  label = paste(label, x)), size = 2.5, fontface = "bold")+
  geom_text(  aes(x = 69, y = -1,  label ="\U003BC - 2\u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = 90, y = -1,  label ="\U003BC + 2\u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = 64, y = 5,  label =as.character(project1data_final[which(round(project1data_final$CUMFNS,2)==63.76),]$sasdate)), size = 2.5)+
  geom_text(  aes(x = 91, y = 9.5,  label =as.character(project1data_final[which(round(project1data_final$CUMFNS,2)==91.57),]$sasdate)), size = 2.5)+
  geom_vline(aes(xintercept = mean(CUMFNS)), color = "#000000", size = 0.5) +
  geom_vline(aes(xintercept = mean(CUMFNS) + 2*sd(CUMFNS)), color = "#000000", size = 0.7, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(CUMFNS) - 2*sd(CUMFNS)), color = "#000000", size = 0.7, linetype = "dashed")+
  labs(
    title = "Capacity Utilization: Manufacturing (SIC)",
    subtitle ="(Percent of Capacity)",
    x = "CUMFNS",
    y = "Count"
  )+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(
    plot.title = element_text(color = "black", size = 10, face = "bold"),
    plot.subtitle = element_text(color = "black", size = 9, face = "bold"))

## UNRATESTx
annotations2 <- data.frame(
  x = c(round(min(project1data_final$UNRATESTx), 2), 
        round(mean(project1data_final$UNRATESTx), 2), 
        round(max(project1data_final$UNRATESTx), 2), 
        round(median(project1data_final$UNRATESTx), 2)),
  label = c("Min:", "Mean:", "Max:", "Median:")
) 

hist2 <- ggplot(project1data_final, aes(x = UNRATESTx))+
  geom_histogram(bins=20,color = "white", fill = "gray")+
  geom_text(data = annotations2, 
            aes(x = c(2.7,4.75,12,4.75), y = c(18,5,8,8),  label = paste(label, x)), size = 2.5, fontface = "bold")+
  geom_text(  aes(x = 2.5, y = 0,  label ="\U003BC - 2\u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = 7.3, y = 0,  label ="\U003BC + 2\u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = 2.7, y = 21,  label =as.character(project1data_final[which(round(project1data_final$UNRATESTx,2)==2.81),]$sasdate)), size = 2.5)+
  geom_text(  aes(x = 12, y = 11,  label =as.character(project1data_final[which(round(project1data_final$UNRATESTx,2)==12.25),]$sasdate)), size = 2.5)+
  geom_vline(aes(xintercept = mean(UNRATESTx)), color = "#000000", size = 0.5) +
  geom_vline(aes(xintercept = mean(UNRATESTx) + 2*sd(UNRATESTx)), color = "#000000", size = 0.7, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(UNRATESTx) - 2*sd(UNRATESTx)), color = "#000000", size = 0.7, linetype = "dashed")+
  labs(
    title = "Unemployment Rate less than 27 weeks (Percent)",
    x = "UNRATESTx",
    y = "Count"
  )+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(
    plot.title = element_text(color = "black", size = 10, face = "bold"))
##FEDFUNDS
annotations3 <- data.frame(
  x = c(round(min(project1data_final$FEDFUNDS), 2), 
        round(mean(project1data_final$FEDFUNDS), 2), 
        round(max(project1data_final$FEDFUNDS), 2), 
        round(median(project1data_final$FEDFUNDS), 2)),
  label = c("Min:", "Mean:", "Max:", "Median:")
) 

hist3 <- ggplot(project1data_final, aes(x = FEDFUNDS))+
  geom_histogram(bins=20,color = "white", fill = "gray")+
  geom_text(data = annotations3, 
            aes(x = c(0,4.8,17.5,4.8), y = c(18,5,8,8),  label = paste(label, x)), size = 2.5, fontface = "bold")+
  geom_text(  aes(x = -2.4, y = -1,  label ="\U003BC - 2\u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = 12, y = -1,  label ="\U003BC + 2\u03C3" ), size = 2, fontface = "bold")+
  #geom_text(  aes(x = 0, y = 21,  label =as.character(project1data_final[which(round(project1data_final$UNRATESTx,2)==0.06),]$sasdate)), size = 2.5)+
  #geom_text(  aes(x = 17.5, y = 11,  label =as.character(project1data_final[which(round(project1data_final$UNRATESTx,2)==17.78),]$sasdate)), size = 2.5)+
  geom_vline(aes(xintercept = mean(FEDFUNDS)), color = "#000000", size = 0.5) +
  geom_vline(aes(xintercept = mean(FEDFUNDS) + 2*sd(FEDFUNDS)), color = "#000000", size = 0.7, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(FEDFUNDS) - 2*sd(FEDFUNDS)), color = "#000000", size = 0.7, linetype = "dashed")+
  labs(
    title = "Effective Federal Funds Rate (Percent)",
    x = "FEDFUNDS",
    y = "Count"
  )+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(
    plot.title = element_text(color = "black", size = 10, face = "bold"))

##GrowthGDP

annotations4 <- data.frame(
  x = c(round(min(project1data_final[-1,]$GDPC1), 2), 
        round(mean(project1data_final[-1,]$GDPC1), 2), 
        round(max(project1data_final[-1,]$GDPC1), 2), 
        round(median(project1data_final[-1,]$GDPC1), 2)),
  label = c("Min:", "Mean:", "Max:", "Median:")
) 

hist4<- ggplot(project1data_final[-1,], aes(x = GDPC1))+
  geom_histogram(bins=20,color = "white", fill = "gray")+
  geom_text(data = annotations4, 
            aes(x = c(-9,0.7,7.5,0.7), y = c(23,12,8,19),  label = paste(label, x)), size = 2.5, fontface = "bold")+
  geom_text(  aes(x = -1.48, y = -1,  label ="\U003BC - 2\u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = 3, y = -1,  label ="\U003BC + 2\u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = -9, y = 28,  label =as.character(project1data_final[which(round(project1data_final$GDPC1,2)==-8.94),]$sasdate)), size = 2.5)+
  geom_text(  aes(x = 7.5, y = 13,  label =as.character(project1data_final[which(round(project1data_final$GDPC1,2)==7.55),]$sasdate)), size = 2.5)+
  geom_vline(aes(xintercept = mean(GDPC1)), color = "#000000", size = 0.5) +
  geom_vline(aes(xintercept = mean(GDPC1) + 2*sd(GDPC1)), color = "#000000", size = 0.7, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(GDPC1) - 2*sd(GDPC1)), color = "#000000", size = 0.7, linetype = "dashed")+
  labs(
    title = "Growth in GDP (percent)",
    x = "%GPDC1",
    y = "Count"
  )+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(
    plot.title = element_text(color = "black", size = 10, face = "bold"))

##GrowthCPIAUCSL
annotations5 <- data.frame(
  x = c(round(min(project1data_final[-1,]$CPIAUCSL), 2), 
        round(mean(project1data_final[-1,]$CPIAUCSL), 2), 
        round(max(project1data_final[-1,]$CPIAUCSL), 2), 
        round(median(project1data_final[-1,]$CPIAUCSL), 2)),
  label = c("Min:", "Mean:", "Max:", "Median:")
) 

hist5<-ggplot(project1data_final[-1,], aes(x = CPIAUCSL))+
  geom_histogram(bins=20,color = "white", fill = "gray")+
  geom_text(data = annotations5, 
            aes(x = c(-2.3,0.9,4,0.8), y = c(5,12,5,19),  label = paste(label, x)), size = 2.5, fontface = "bold")+
  geom_text(  aes(x = -0.6, y = -1,  label ="\U003BC - 2\u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = 2.4, y = -1,  label ="\U003BC + 2\u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = -2.3, y = 10,  label =as.character(project1data_final[which(round(project1data_final$CPIAUCSL,2)==-2.29),]$sasdate)), size = 2.5)+
  geom_text(  aes(x = 4, y = 10,  label =as.character(project1data_final[which(round(project1data_final$CPIAUCSL,2)==3.95),]$sasdate)), size = 2.5)+
  geom_vline(aes(xintercept = mean(CPIAUCSL)), color = "#000000", size = 0.5) +
  geom_vline(aes(xintercept = mean(CPIAUCSL) + 2*sd(CPIAUCSL)), color = "#000000", size = 0.7, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(CPIAUCSL) - 2*sd(CPIAUCSL)), color = "#000000", size = 0.7, linetype = "dashed")+
  labs(
    title = "Growth in CPI (percent)",
    x = "%CPIAUCSL",
    y = "Count"
  )+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(
    plot.title = element_text(color = "black", size = 10, face = "bold"))

##GrowthM1Real
annotations6 <- data.frame(
  x = c(round(min(project1data_final[-1,]$M1REAL), 2), 
        round(mean(project1data_final[-1,]$M1REAL), 2), 
        round(max(project1data_final[-1,]$M1REAL), 2), 
        round(median(project1data_final[-1,]$M1REAL), 2)),
  label = c("Min:", "Mean:", "Max:", "Median:")
) 

hist6<-ggplot(project1data_final[-1,], aes(x = M1REAL))+
  geom_histogram(bins=20,color = "white", fill = "gray")+
  geom_text(data = annotations6, 
            aes(x = c(-3.8,1.5,207.5,0.47), y = c(5,12,15,19),  label = paste(label, x)), size = 2.5, fontface = "bold")+
  geom_text(  aes(x = -25, y = -1,  label ="\U003BC - 2\u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = 28, y = -1,  label ="\U003BC + 2\u03C3" ), size = 2, fontface = "bold")+
  # geom_text(  aes(x = -3.8, y = 18,  label =as.character(project1data_final[which(round(project1data_final$M1REAL,2)==-3.82),]$sasdate)), size = 2.5)+
  geom_text(  aes(x = 207, y = 24,  label =as.character(project1data_final[which(round(project1data_final$M1REAL,2)==207.56),]$sasdate)), size = 2.5)+
  geom_vline(aes(xintercept = mean(M1REAL)), color = "#000000", size = 0.5) +
  geom_vline(aes(xintercept = mean(M1REAL) + 2*sd(M1REAL)), color = "#000000", size = 0.7, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(M1REAL) - 2*sd(M1REAL)), color = "#000000", size = 0.7, linetype = "dashed")+
  labs(
    title = "Growth in M1 (percent)",
    x = "%M1REAL",
    y = "Count"
  )+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(
    plot.title = element_text(color = "black", size = 10, face = "bold"))

##GrowthM1Real2
annotations7 <- data.frame(
  x = c(round(min(project1data_final[-c(1,246),]$M1REAL), 2), 
        round(mean(project1data_final[-c(1,246),]$M1REAL), 2), 
        round(max(project1data_final[-c(1,246),]$M1REAL), 2), 
        round(median(project1data_final[-c(1,246),]$M1REAL), 2)),
  label = c("Min:", "Mean:", "Max:", "Median:")
) 

hist7<-ggplot(project1data_final[-c(1,246),], aes(x = M1REAL))+
  geom_histogram(bins=20,color = "white", fill = "gray")+
  geom_text(data = annotations7, 
            aes(x = c(-3.82,0.67,33,0.47), y = c(7,12,5,19),  label = paste(label, x)), size = 2.5, fontface = "bold")+
  geom_text(  aes(x = -4.5, y = -1,  label ="\U003BC - 2\u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = 6, y = -1,  label ="\U003BC + 2\u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = -3.8, y = 13,  label =as.character(project1data_final[which(round(project1data_final$M1REAL,2)==-3.82),]$sasdate)), size = 2.5)+
  geom_text(  aes(x = 33, y = 10,  label =as.character(project1data_final[which(round(project1data_final$M1REAL,2)==33.56),]$sasdate)), size = 2.5)+
  geom_vline(aes(xintercept = mean(M1REAL)), color = "#000000", size = 0.5) +
  geom_vline(aes(xintercept = mean(M1REAL) + 2*sd(M1REAL)), color = "#000000", size = 0.7, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(M1REAL) - 2*sd(M1REAL)), color = "#000000", size = 0.7, linetype = "dashed")+
  labs(
    title = "Growth in M1 (percent) (2020Q2 removed)",
    x = "%M1REAL",
    y = "Count"
  )+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(
    plot.title = element_text(color = "black", size = 10, face = "bold"))
##GrowthS&P 500
annotations8 <- data.frame(
  x = c(round(min(project1data_final[-1,]$`S&P 500`), 2), 
        round(mean(project1data_final[-1,]$`S&P 500`), 2), 
        round(max(project1data_final[-1,]$`S&P 500`), 2), 
        round(median(project1data_final[-1,]$`S&P 500`), 2)),
  label = c("Min:", "Mean:", "Max:", "Median:")
) 

hist8<-ggplot(project1data_final[-1,], aes(x = `S&P 500`))+
  geom_histogram(bins=20,color = "white", fill = "gray")+
  geom_text(data = annotations8, 
            aes(x = c(-27.3,1.96,20.1,2), y = c(5,12,5,19),  label = paste(label, x)), size = 2.5, fontface = "bold")+
  geom_text(  aes(x = -10, y = -1,  label ="\U003BC - 2\u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = 13.5, y = -1,  label ="\U003BC + 2\u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = -27.3, y = 8,  label =as.character(project1data_final[which(round(project1data_final$`S&P 500`,2)==-27.33),]$sasdate)), size = 2.5)+
  geom_text(  aes(x = 20, y = 8,  label =as.character(project1data_final[which(round(project1data_final$`S&P 500`,2)==20.12),]$sasdate)), size = 2.5)+
  geom_vline(aes(xintercept = mean(`S&P 500`)), color = "#000000", size = 0.5) +
  geom_vline(aes(xintercept = mean(`S&P 500`) + 2*sd(`S&P 500`)), color = "#000000", size = 0.7, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(`S&P 500`) - 2*sd(`S&P 500`)), color = "#000000", size = 0.7, linetype = "dashed")+
  labs(
    title = "Growth in S&P 500 (percent)",
    x = "%S&P 500",
    y = "Count"
  )+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(
    plot.title = element_text(color = "black", size = 10, face = "bold"))

grid.arrange(hist1,hist2,hist3,hist4, nrow=2,ncol=2)

grid.arrange(hist5,hist8,hist6,hist7, nrow=2,ncol=2)


#Task a
plot_file<-ts(project1data_final[,2:8],start=1959, frequency = 4)


plot_1 <- autoplot(plot_file[,"UNRATESTx"])+
  ggtitle("Unemployment rate throught the years") +
  xlab("Year") + ylab("Unemployment rate")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(plot.title = element_text(size = 10, face = "bold"), plot.subtitle = element_text(size=9), axis.title=element_text(size=10))
  
  
plot_2<- autoplot(plot_file[,"CUMFNS"])+
  ggtitle("Capital Utilization", subtitle = "Capacity Utilization: Manufacturing (SIC) (Percent of Capacity)") +
  xlab("Year") + ylab("Percentage utilized")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(plot.title = element_text(size = 10, face = "bold"), plot.subtitle = element_text(size=9), axis.title=element_text(size=10))

plot_3 <- autoplot(plot_file[,"FEDFUNDS"])+
  ggtitle("Effective Federal Funds Rate") +
  xlab("Year") + ylab("Funds Rate")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(plot.title = element_text(size = 10, face = "bold"), plot.subtitle = element_text(size=9), axis.title=element_text(size=10))

plot_4 <- autoplot(plot_file[,"GDPC1"])+
  ggtitle("Growth of GDP", subtitle = "Real Gross Domestic Product, 3 Decimal (Billions of Chained 2012 Dollars)") +
  xlab("Year") + ylab("value Percentage")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(plot.title = element_text(size = 10, face = "bold"), plot.subtitle = element_text(size=9), axis.title=element_text(size=10))

plot_5 <- autoplot(plot_file[,"CPIAUCSL"])+
  ggtitle("Growth of CPI", subtitle = "Consumer Price Index for All Urban consumers") +
  xlab("Year") + ylab("value Percentage")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(plot.title = element_text(size = 10, face = "bold"), plot.subtitle = element_text(size=9), axis.title=element_text(size=10))

plot_6 <- autoplot(plot_file[,"M1REAL"])+
  ggtitle("Growth of M1REAL ", subtitle = "Real M1 Money Stock (billions of 1982-84 dollars)") +
  xlab("Year") + ylab("value Percentage")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(plot.title = element_text(size = 10, face = "bold"), plot.subtitle = element_text(size=9), axis.title=element_text(size=10))

plot_7 <- autoplot(plot_file[,"S&P 500"])+
  ggtitle("Growth of S&P 500 ", subtitle = "S&P’s Common Stock Price Index: Composite" ) +
  xlab("Year") + ylab("value Percentage")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(plot.title = element_text(size = 10, face = "bold"), plot.subtitle = element_text(size=9), axis.title=element_text(size=10))

plot_file2 <- ts(plot_file[-246,],start=1959, frequency = 4)
plot_8 <- autoplot(plot_file2[,"M1REAL"])+
  ggtitle("Growth of M1REAL (2020Q2 removed) ", subtitle = "Real M1 Money Stock (billions of 1982-84 dollars)") +
  xlab("Year") + ylab("value Percentage")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(plot.title = element_text(size = 10, face = "bold"), plot.subtitle = element_text(size=9), axis.title=element_text(size=10))


grid.arrange(plot_1, plot_2, plot_3, nrow=3, ncol=1)
grid.arrange(plot_4, plot_5, plot_7,  nrow=3, ncol=1)
grid.arrange(plot_6, plot_8,  nrow=2, ncol=1)

#Task b
project1data_model <- ts(project1data_final[2:252,5],start=c(1959,2), freq = 4)

#AR Model

arfc_ols <- c()
for (i in 3:250) {
  armod1_ols <- ar.ols(project1data_model[1:i], order.max =1, aic =FALSE)
  forecasts_ols <- predict(armod1_ols, n.ahead = 1)
  arfc_ols <- append(arfc_ols,forecasts_ols$pred)
}


ar_data <- data.frame(project1data_final$sasdate[5:252],project1data_final$`%GDPC1`[5:252], arfc_ols)
rmse_ar <- rmse(project1data_model[4:251],arfc_ols)

ARresult <- data.frame(armod1_ols$x.intercept, armod1_ols$x.mean,armod1_ols$ar, rmse(project1data_model[4:251],arfc_ols))

colnames(ARresult) <- c("Intercept","Mean","Coefficient","RMSE")

xtable(ARresult, digits = 3, type="text")

# task c
# VAR(1) model

GDPVAR <- xts( project1data_final[-1,-1],project1data_final$sasdate[-1],order.by=as.Date(project1data_final$sasdate[-1], format = "%m/%d/%Y"))

varfc <- c()
for (i in 9:250) {
  varmod <- VAR(GDPVAR[1:i], p = 1, type = "const", season = NULL, exog = NULL) 
  forecasts <- predict(varmod, n.ahead=1)
  varfc <- append(varfc,forecasts$fcst$X.GDPC1[1])
}

var_data <- data.frame(project1data_final$sasdate[11:252],project1data_final$`%GDPC1`[11:252],varfc)
xtable(varmod$varresult$GDPC1)

rmse_var1<-rmse(project1data_model[10:251],varfc)
# task d Granger

one_vec <- rep(1, (250))
z1 <- project1data_final$GDPC1[2:(251)]
z2 <- project1data_final$CUMFNS[2:(251)]
z3 <- project1data_final$UNRATESTx[2:(251)]
z4 <- project1data_final$CPIAUCSL[2:(251)]
z5 <- project1data_final$FEDFUNDS[2:(251)]
z6 <- project1data_final$M1REAL[2:(251)]
z7 <- project1data_final$`S&P 500`[2:(251)]
z <- cbind(one_vec,z1,z2,z3,z4,z5,z6,z7)
Z <- matrix(t(z), nrow = 8, ncol= 250 )
y1 <- project1data_final$GDPC1[3:(252)]
y2 <- project1data_final$CUMFNS[3:(252)]
y3 <- project1data_final$UNRATESTx[3:(252)]
y4 <- project1data_final$CPIAUCSL[3:(252)]
y5 <- project1data_final$FEDFUNDS[3:(252)]
y6 <- project1data_final$M1REAL[3:(252)]
y7 <- project1data_final$`S&P 500`[3:(252)]
y <- cbind(y1,y2,y3,y4,y5,y6,y7)
Y <- matrix(t(y), nrow = 7, ncol=250 )
A_hat <- Y %*% t(Z) %*% solve(Z %*% t(Z))
U <- Y - A_hat%*%Z
covar <- (U %*% t(U))*(1/242)
var <- kronecker(solve(Z %*% t(Z)),covar)
A_hat[1,2]/(sqrt(var[8,8])) #GDP
A_hat[1,3]/(sqrt(var[15,15])) #CUM
A_hat[1,4]/(sqrt(var[22,22])) #UNR
A_hat[1,5]/(sqrt(var[29,29])) #CPI
A_hat[1,6]/(sqrt(var[36,36])) #FED
A_hat[1,7]/(sqrt(var[43,43])) #M1
A_hat[1,8]/(sqrt(var[50,50])) #SP
qt(0.05/2, 241, lower.tail=FALSE)

varmod_gr <- VAR(GDPVAR[1:251], p = 1, type = "const", season = NULL, exog = NULL) 
xtable(varmod_gr$varresult$GDPC1)
#task e

lag_select <- VARselect(GDPVAR, type= "const", lag.max = 8)

xtable(lag_select$criteria)
varfc3 <- c()
for (i in 25:250) {
  varmod3 <- VAR(GDPVAR[1:i], p = 3, type = "const", season = NULL, exog = NULL) 
  forecasts3 <- predict(varmod3, n.ahead=1)
  varfc3 <- append(varfc3,forecasts3$fcst$X.GDPC1[1])
}

rmse_var3 <- rmse(project1data_model[26:251],varfc3)



xtable(summary(varmod3)$covres)

RMSE <- data.frame("RMSE",rmse_ar, rmse_var1, rmse_var3)

colnames(RMSE)<- c("RMSE","AR(1) Model","VAR(1) Model","VAR(3) Model")
xtable(RMSE, digits = 3)
##Graph for task e

varp_data <- data.frame(project1data_final$sasdate[27:252],project1data_final$`%GDPC1`[27:252],varfc3)
head(ar_data) #12/1/1959 3 0s
head(var_data) #6/1/1961 7 0s
head(varp_data) #23 0s

ar_missing <- data.frame(project1data_final$sasdate[1:4], project1data_final$`%GDPC1`[1:4], 0)
colnames(ar_missing) <- c("dates", "gdp_real", "ar_forecast")
colnames(ar_data) <- c("dates","gdp_real", "ar_forecast")
final_ar <- rbind(ar_data, ar_missing)
final_ar$dates <- as.Date(final_ar$dates, "%m/%d/%Y")
final_ar <- final_ar%>%
  arrange(ymd(final_ar$dates))

var_missing <- data.frame(project1data_final$sasdate[1:10], project1data_final$`%GDPC1`[1:10], 0)
colnames(var_missing) <- c("dates", "gdp_real", "var_forecast")
colnames(var_data) <- c("dates","gdp_real", "var_forecast")
final_var <- rbind(var_data,var_missing )
final_var$dates <- as.Date(final_var$dates, "%m/%d/%Y")
final_var <- final_var%>%
  arrange(ymd(final_var$dates))

varp_missing <- data.frame(project1data_final$sasdate[1:26], project1data_final$`%GDPC1`[1:26],0)
colnames(varp_missing) <- c("dates", "gdp_real", "varp_forecast")
colnames(varp_data) <- c("dates","gdp_real", "varp_forecast")
final_varp <- rbind(varp_data,varp_missing )
final_varp$dates <- as.Date(final_varp$dates, "%m/%d/%Y")
final_varp <- final_varp%>%
  arrange(ymd(final_varp$dates))

final_plot <- data.frame(final_ar, final_var, final_varp)
final_plot$dates <- as.Date(final_plot$dates, "%m/%d/%Y")

plot(final_plot$dates,final_plot$gdp_real, type = "l", lty=1, xlab="Dates", ylab="GDPC1", col= "darkgrey")
points(final_plot$dates,c(rep(NA,3),final_plot$ar_forecast[4:251]), type = "l", lty=2, cex=15)
legend(x=1960, y=-5, legend=c("Actual Growth Rates", "AR(1) Forecast"), col=c("darkgrey", "black"),lty=1:2, cex=0.8)

plot(final_plot$dates,final_plot$gdp_real, type = "l", lty=1,xlab="Dates", ylab="GDPC1", col= "darkgrey")
points(final_plot$dates,c(rep(NA, 9),final_plot$var_forecast[10:251]), type = "l", col = "black", lty = 2, cex=15)
legend(x=1960, y=-5, legend=c("Actual Growth Rates", "VAR(1) Forecast"), col=c("darkgrey", "black"),lty=1:2, cex=0.8)

plot(final_plot$dates,final_plot$gdp_real, type = "l", lty=1,xlab="Dates",col="darkgrey", ylab="GDPC1")
points(final_plot$dates,c(rep(NA, 25),final_plot$varp_forecast[26:251]), type = "l", lty = 2, cex=15)
legend(x=1960, y=-5, legend=c("Actual Growth Rates", "VAR(3) Forecast"), col=c("darkgrey", "black"),lty=1:2, cex=0.8)

write_csv(final_plot, "AR-VAR-forecasts.csv")
