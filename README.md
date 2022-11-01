# Case Studies for Time Series Data
Solutions for time series forecasting using ML and VAR methods

## Table of contents
* [Introduction](#introduction)
* [Project I](#project-i)
* [Project II](#project-ii)
* [Project III](#project-iii)
* [Key Findings and Visuals](#key-findings-and-visuals)

## Project I
**Name:** One-Quarter-Ahead Forecasts of US GDP Growth - A Vector Autoregression Approach
### Introduction
The project aims to forecast US real GDP growth by using the historical quarterly data between 1959 and 2021, and to compare different methods such as forecasting GDP growth using its own history, and forecasting GDP growth using the information from several different economic variables, and to decide which method results in the best performance. In line with the project objective, one-quarter ahead forecasts are calculated using autoregressive model and vector autoregressive models. Akaike’s Information Criteria (AIC) is to used to find the optimal lag for VAR model to avoid any Granger causality between the time series.
### Dataset
Our data set originally comes from the Federal Reserve Economic Database (FRED), a big macroeconomic database which is publicly accessible and updated real time and the quarterly series from 1959 Q1 to 2021 Q4 are available in FRED-QD database. Initial data set is taken from the FRED-QD, specifically from the file ”2022-02.csv”, which consists of 246 quarterly time series of several economic variables (McCracken and Ng, 2020).
### Analysis
We aim to model the growth in GDPC1, first by fitting an AR(1) model to the data from 1959Q2 to 2021Q3. While forecasting one-quarter ahead growths, it is important to note that all of the information from GDPC1 up to time t is used to fit an AR(1) model and to forecast the value at time t+1.
## Project II: One-Quarter-Ahead Forecasts of US GDP Growth - A Machine Learning Approach
### Introduction
### Dataset
### Analysis
## Project III: Nowcasting US GDP Growth - Unrestricted MIDAS
### Introduction
### Dataset
### Analysis
