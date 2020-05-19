#Further Analysis
library(depmixS4)
library(tidyr)
library(tibble)
library(plotly)
library(PerformanceAnalytics)
#library(rvest)
#library(tidyverse)
library(quantmod)
getSymbols("ASML", from = "2014-11-30",to="2019-11-30", src = 'yahoo', periodicity = 'monthly')
ASML_log_returns<-ASML%>%Ad()%>%dailyReturn(type='log')
ASML%>%Ad()%>%chartSeries()
ASML%>%chartSeries(TA='addBBands();addVo();addMACD()')
getSymbols("KLAC", from = "2014-11-30",to="2019-11-30", src = 'yahoo', periodicity = 'monthly')
KLAC_log_returns<-KLAC%>%Ad()%>%dailyReturn(type='log')
KLAC%>%Ad()%>%chartSeries()
KLAC%>%chartSeries(TA='addBBands();addVo();addMACD()')
getSymbols("LRCX", from = "2014-11-30",to="2019-11-30", src = 'yahoo', periodicity = 'monthly')
LRCX_log_returns<-LRCX%>%Ad()%>%dailyReturn(type='log')
LRCX%>%Ad()%>%chartSeries()
LRCX%>%chartSeries(TA='addBBands();addVo();addMACD()')

#Risk Reward Measure
Reward = c(mean(ASML_log_returns),mean(KLAC_log_returns),mean(LRCX_log_returns))
Risk = c(sd(ASML_log_returns),sd(KLAC_log_returns),sd(LRCX_log_returns))
Risk_Reward = as.data.frame(cbind(Reward,Risk))
row.names(Risk_Reward) = c("ASML","KLAC","LRCX")
fig <- plot_ly(x = ~Reward, y = ~Risk, type = "scatter",text = rownames(Risk_Reward))
fig

#Performance_Correlation
data<-cbind(diff(log(Cl(ASML))),diff(log(Cl(KLAC))),diff(log(Cl(LRCX))))
chart.Correlation(data)

