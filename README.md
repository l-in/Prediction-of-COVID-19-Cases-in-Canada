# Prediction-of-COVID-19-Cases-in-Canada

This work presents an exploration of statistics from the stock market, flight and mobility patterns, weather, government policies, internet search queries, labour and retail surveys, Twitter and scientific publications being used to predict daily COVID-19 cases across Canada. While the relevance of these statistics to epidemiology may not be obvious, these sources depict both human responsiveness to and impact on the spread of disease. The presented methods are an attempt to identify a reliable disease prediction system that depends on multifaceted human behaviour instead of the traditional clinical ecosystem, which is subject to inefficiencies, reporting errors, miscommunication and mismanagement.

## The Data

Ten publicly available data sources were used, dated 1 January 2020 to 31 August 2020.

- stock market
    - NASDAQ, TSX, S&P 500 taken from Yahoo! Finance
- flight patterns
    - details on international and domestic flights going to and from Canada taken from the OpenSky Network
    - airports in Canada taken from SkyVector
- weather
    - humidity and temperature data for two cities/towns in each province and territory taken from ClimateData.ca, a website funded by Environment and Climate Change Canada
- mobility patterns
    - patterns extracted from location data generated smartphones were taken from the Google and Apple websites
- government policies
    - government responses to the pandemic quantified and published by the Oxford COVID-19 Government Response Tracker
- internet search queries
    - search popularity for 45 queries (determined individually) related to COVID-19 were taken from Google Trends 
    - search popularity for queries related to COVID-19 (determined by Microsoft Bing) were taken from the Bing search engine 
- Twitter
    - Tweets containing key words relevant to COVID-19 were collected by The Panacea Lab and processed into datasets of the number of tweets per day and the most popular terms, bigrams and trigrams per day
- scientific publications
    - publication details of scientific papers and other literature related to COVID-19
- labour statistics
    - monthly employment and unemployment numbers were obtained by the Canadian Labour Force Survey published by Statistics Canada
- retail ecommerce sales
    - monthly performance data of the retail trade, electronic shopping and mail-order houses, and retail e-commerce sales sectors published by Statistics Canada

## Data Wrangling

- the date variable in each dataset was converted to a Date object 
- datasets with missing observations were padded to have the full range of dates 
- missing values were imputed where applicable 

### Cases:
- up to the date with the first observed case, the days with missing observations were set to zero cases  
- after the first observed case, the missing observations were imputed with a linear algorithm 

### Stock Market:
- NASDAQ and S&P 500 values were converted to CAD
- missing values during weekends were imputed using the linear method

### Flight Patterns:
- details on flights going to and from Canada were extracted from the full dataset of global flights
- provincial labels were added to each flight observation using corresponding airport code
-	the total number of flights, international flights, and domestic flights landing in Canada per day were calculated and put into separate datasets

### Weather:
- Datasets for cities/towns from the same province were merged
- Provincial and national averages of the mean temperature and maximum and minimum humidity were computed
- Linear method was used for data imputation

### Mobility Patterns (Google):
- the full dataset had observations at each day for different locations in Canda, so the average of all locations per day was computed
- missing values were imputed using spline method

### Mobility Patterns (Apple):
- data values for Canada were extracted 
- missing values imputed using the linear method

### Government Policies:
- missing values imputed using linear algorithm

### Internet Search Queries (Google):
- observations with a relative search index of “<1” were assigned 0.5 so that the value would be numeric

### Internet Search Queries (Bing):
-	data for Canadian searches was extracted
- the number of unique queries generated per day and the overall popularity of COVID-19 related searches per day (where the popularity values of all original queries at each date are summed) were calculated from the raw data
- missing values imputed using linear algorithm

### Twitter
- n-grams with non-letter characters were removed from the daily lists of most popular n-grams
- weighted average polarity for each day’s most popular n-grams was calculated

### Publications
-	the title, journal name, abstract, and publication date were kept from the observation for each publication
-	polarity was calculated for the title and abstract of each publication

### Labour
- the monthly employment and unemployment values were repeated for the corresponding number of days in each month to produce daily values for the project’s time period 

### Retail E-commerce Sales
- the raw data was split into subsets for retail trade, electronic shopping and mail-order houses, and retail e-commerce sales
- same procedure as for labour data above was followed

## Methodology
-	min-max normalization was applied to all predictors
- cross correlation between daily cases and predictors was computed
- cases were predicted in two steps:
    1. Models predicting daily cases were generated separately for each data source to determine which predictors (and which of their lags, if any) were best at modelling daily cases
    2.The best predictors from the previous step were combined in three different ensemble models 
    - retrospective models
    - prospective models starting at day 1
    - prospective models starting at day 90
- model performance metrics used: 
    -	root mean squared error (RMSE)
    - root mean squared percentage error (RMSPE)
    - mean absolute percentage error (MAPE)
    - correlation between predicted and observed daily cases
