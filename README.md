# Spatial Bayesian Bus Ridership Model 

This project fits spatial Bayesian direct demand models of Metro Transit bus ridership. The model is a Conditional Autoregressive (CAR) model fit using Stan. 

The project started as a statistics honors thesis, which you can find in this repo at reports/2019. There is also a TRB poster in reports/ for an introduction/summary of the project. 

[This](https://mc-stan.org/users/documentation/case-studies/icar_stan.html) tutorial was very helpful for getting started with fitting the CAR models.

To re-run the analysis, there are some things to change in all of the files in scripts/. The main changes are: 

* change date ranges in queries in 01 and 02
* organize files for data, fits, etc. I had data/covid, data/2019, and data/covariates with model data, fit models, etc sorted into the appropriate years. There are lots of references to these folders throughout to check when you want to use a different year. 
* change the years for ACS and LODES/WAC data in 04. This can be a little tricky because LODES is released less frequently than ACS, so you need to be careful about matching years 


Contact: raven.mcknight@metc.mn.us / raven.i.mcknight@gmail.com

