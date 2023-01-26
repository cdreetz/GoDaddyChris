# GoDaddy Microbusiness Forecasting 

## Background: 
Data has been collected on the density of microbusinesses of individual counties around the country. Microbusiness density is determined by dividing the population of a county by the microbusinesses which are defined as businesses with an online presence and less than 10 employees.

## The goal: 
Analyze the provided data and develop a model ensemble that can forecast the following 6 months.  

## The plan:  
Starting off with basic preprocessing, check for missing data and for outlying data.  Followed by some basic exploration and data visuals. The data is to be organized in a way that each county will be assigned its own model.  Starting with the most basic lm model and evaluating the performance of the lm model for each county.  Followed by applying various model types to the list of counties and collecting the performance metrics.  Finally assigning the best performing model for each individual county.  

## Final:
The models will then be submitted against a private test set on Kaggle to determine final performance scored in SMAPE.

