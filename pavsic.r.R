library(dplyr)

euribor15 <- as.data.frame(t(read.csv('hist_EURIBOR_2015.csv')))
euribor16 <- as.data.frame(t(read.csv('hist_EURIBOR_2016.csv')))
euribor17 <- as.data.frame(t(read.csv('hist_EURIBOR_2017.csv')))

