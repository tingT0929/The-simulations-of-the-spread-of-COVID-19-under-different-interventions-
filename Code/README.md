## Model

1. Run the `Runing.R`.

2. Change the data through changing the part of “##Data import” in `Data_import.R`:
a. where "dat" refers to the cumulative cases per day (starting from the date of diagnosis);
b. "N" refers to the population of a region;
c. "region" is the name of the region;

3. The "K" in `Running.R` represents the number of parallel threads. It is recommended to use more cores.


-----
## Plot

`Predict_timeinvar.R` and `Policy_migration.R` are the codes for the simulaition and plot.