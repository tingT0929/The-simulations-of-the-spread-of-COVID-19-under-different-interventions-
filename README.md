# The-simulations-of-the-spread-of-COVID-19-under-different-interventions-
Data-driven analysis of the simulations of the spread of COVID-19 under different interventions of China


## 1. Data 
### 1) Abstract
Our data source consists of cumulative confirmed cases, cumulative recovery cases and total deaths for Italy, Korea, and the United States were downloaded through May 31, 2020
from "nCov2019" package. Also, the reported numbers of cumulative confirmed cases, cumulative recovery cases, and total deaths from January 15 to February 12 in Wuhan, from January 21 to February 9 in Wenzhou, and from January 19 to February 6 in Shenzhen are collected.

### 2) Availability
The data to reproduce our results are available.

### 3) Description
The data incorporte 4 `.rdata` files.
- The cumulative confirmed cases in each of three countries were complied in each of three `.rdata` files (`Italy_all.rdata`; `korea_all.rdata`;`usa_all.rdata`)
- The cumulative confirmed cases, cumulative recovery cases and total deaths were saved in a `All_dat.rdata` file.  

### 4) Permissions
The data were orignially collected by the authors.

----
## 2. Code
### 1) Abstract
The codes incorported all the scripts to reproduce the analysis in the paper. 

### 2) Reporducibility
- The estimation of the parameters of the SICR model in the paper by runing `Running.R`, where the `region_mark` could be changed as 1-9, representing the running procedure for Wuhan, Wenzhou, Shenzhen, Korea (for policy migration), the United states (for policy migration), Italy (for policy migration), Italy (for the simulation of constant outbreak)£¬ Korea (for the simulation of constant outbreak) and the United States (for the simulation of constant outbreak), respectively.


----
## 3. Result


----
