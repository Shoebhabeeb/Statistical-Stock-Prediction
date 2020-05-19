# Statistical-Stock-Prediction
Statistical Stock Analysis and Prediction using Monte Carlo Markov Chain - R Programming

***PLEASE READ THIS BEFORE RUNNING THE R CODE***
**FOLLOW THE FOLLOWING STEPS**

1)DOWNLOAD the ASML.csv, LRCX.csv and KLAC.csv files attached in the submission and place them in your R folder.

2)There are 4 R_code files. 3 files each for each company(ASML,LRCX and KLAC) and last file for stock comparisons. Run them in your R-Studio.

3) Change the locations of .csv files in RCode line 31 of of the RCODE Files to the location you have saved the .csv files.

4)Install the following packages 

List of Packages to install:
library(depmixS4)
library(tidyr)
library(tibble)
library(plotly)
library(PerformanceAnalytics)
library(HMM)
library(xts)
library(zoo)
library(quantmod)
library(nnet)
library(Rsolnp)
library(nlme)
library(TTR)
library(ggplot2) 
library(reshape2)
library(stringr)

5) Run any of the company named files. 

6)Line 14-29 use data directly from internet from 1900 to Nov 2014 for initial distribution calculation. Where 'i' gives count of hidden states.

7)Lines 31-44 use data from csv[Nov 2014-Nov 2019] to prepare a Hidden and Observed dataframe named 'Data_Final'

8)Next we calculate Transition Probability Matrix(TPM) and Emission Probability Matrix(EPM) named 'A' and 'B'.

You can follow comments added in the RCode for seeing what each block of code is doing and visualizations.


					********	  THANK YOU          *******
