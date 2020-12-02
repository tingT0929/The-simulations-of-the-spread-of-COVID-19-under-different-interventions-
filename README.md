# The-simulations-of-the-spread-of-COVID-19-under-different-interventions-
Data-driven analysis of the simulations of the spread of COVID-19 under different interventions of China


## 1. Data 
### 1) Abstract
Our data source consists of cumulative confirmed cases, cumulative recovery cases and total deaths for Italy, Korea, and the United States were downloaded through April 23, 2020
from "nCov2019" package. 

### 2) Availability
The data to reproduce our results are available.

### 3) Description
The data incorporte 3 `.rdata` files.
- The cumulative confirmed cases in each of three countries were complied in each of three `.rdata` files (`Italy_all.rdata`; `korea_all.rdata`;`usa_all.rdata`)

### 4) Permissions
The data were orignially collected by the authors.

----
## 2. Code
### 1) Abstract
The codes incorported all the scripts to reproduce the analysis in the paper. 

### 2) Reporducibility
- The estimation of Susceptible individuals (S), unidentified infectious (I), self-healing without being confirmed (H), and confirmed cases for the Equation (1) in the paper by runing `Modeling_function.R`.
- The simulation of resumption of busniess by runing `Resumption.R`.
- The evaluation of resumptiong of busniess for each of four states (New York, New Jersey, Connecticut, and California) inadividually by running `NY.R`;`NJ.R`;`CT.R`; and `CA.R`.

----
