#installing the required packages in R

install.packages('rgl')
install.packages('forecast')
install.packages('lubridate')
install.packages('tidyverse')
library(tidyverse)
library(ggplot2)
library(MASS)
library(rgl)

#-------------------------------------------------------------------------------

#Reading the covid csv data into a variable

covid_main <- read.csv('owid-covid-data.csv')

#-------------------------------------------------------------------------------

#Creating sub dataset from the main data set

covid_submain <- data.frame(
  iso_code = covid_main$iso_code,
  continent = covid_main$continent,
  location = covid_main$location,
  date_1 = covid_main$date,
  total_cases = covid_main$total_cases,
  new_cases = covid_main$new_cases,
  new_cases_smoothed = covid_main$new_cases_smoothed,
  total_deaths = covid_main$total_deaths,
  new_deaths = covid_main$new_deaths,
  new_deaths_smoothed = covid_main$new_deaths_smoothed,
  new_tests = covid_main$new_tests,
  total_tests = covid_main$total_tests,
  new_tests_smoothed = covid_main$new_tests_smoothed,
  population = covid_main$population,
  population_density = covid_main$population_density,
  total_vaccinations = covid_main$total_vaccinations,
  people_vaccinated = covid_main$people_vaccinated,
  people_fully_vaccinated = covid_main$people_fully_vaccinated,
  human_development_index = covid_main$human_development_index,
  life_expectancy = covid_main$life_expectancy,
  stringency_index = covid_main$stringency_index
)

#------------------------------------------------------------------------------

#Converting "date_1" variable format into date format

covid_submain <- covid_submain %>% 
  mutate(date_1=as.POSIXct(covid_submain$date_1, format="%Y-%m-%d"))

#dividing date into date, month and year
covid_submain <- covid_submain %>% mutate(month=format(date_1,"%m"),
         year=format(date_1,"%Y"), date=format(date_1, "%d"))

View(covid_submain)
#------------------------------------------------------------------------------

#Creating and filtering required data set for the analysis

covid_dataset <- covid_submain

covid_dataset <- covid_dataset %>% filter(location=="United States" | location=="United Kingdom" | location=="India" 
                    | location=="Japan" | location=="Germany" |location=="China") %>%
                    filter(year != 2023)

View(covid_dataset)  

#------------------------------------------------------------------------------

#switching scientific notation, (default value is zero)
options(scipen=999)

#------------------------------------------------------------------------------

#Handling not applicable(NA) values from the dataset and saving it to new variable
covid_NA <- na.exclude(covid_dataset)

#Summarizing the data
summary(covid_dataset)

#---------------------------VISUALIZATIONS--------------------------------------


#------------------------Cases--------------------------------
#Total cases across all 6 countries, PIE Chart

ggplot(filter(covid_dataset, date_1=="2022-12-31"), 
       aes(x = "", y = total_cases, fill = location)) +
  geom_col(color = "black") +
  geom_label(aes(label = total_cases),
             color = "white",
             position = position_stack(vjust = 0.4),
             show.legend = FALSE)+
  theme_void() + 
  labs(fill = "Country\n", title =expression(underline("Total Covid Cases")) ,
       subtitle = "Across all the countires till Dec, 2022") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(colour = "black" , size = 17 , face = "bold" , hjust = 0.6 ),
        plot.subtitle = element_text(colour = "black" , size = 15 , face = "bold" , hjust = 0.6 ), 
        legend.title = element_text(colour = "black" , size = 16 , face = "bold" ) ,
        legend.text =  element_text(colour = "black" , size = 14) ,
  )


#New Cases Trends, Line Plot

ggplot(filter(covid_dataset, !is.na(new_cases_smoothed) ), #filtering data
       aes(x = date_1, y = new_cases_smoothed, fill = location)) +
  geom_area() + facet_wrap(~ location ) +
  theme_minimal() +
  labs(fill = "Country\n", title =expression(underline("New Cases")) ,
       subtitle = "across all countries, 2020-2022" , y = "New Cases" , x = "Date" , colour = "Country") +
  theme(plot.title = element_text(colour = "red" , size = 17 , face = "bold" , hjust = 0.6 ),
        plot.subtitle = element_text(colour = "black" , size = 15 , face = "bold" , hjust = 0.6 ), 
        legend.title = element_text(colour = "black" , size = 16 , face = "bold" ) ,
        legend.text =  element_text(colour = "black" , size = 14) ,
        strip.text.x = element_text(colour = "black" , size = 14 , face = "italic" )
  )


#new cases across new countires, Bar Graph
ggplot(filter(covid_dataset, location=="United States" | location=="United Kingdom" | location=="India"), 
       aes(fill=location, y=new_cases_smoothed, x=year)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_grid(. ~ location) +
  theme_classic() +
  labs(fill = "Country\n", title =expression(underline("New Cases")) ,
       subtitle = "across all countries, 2020-2022" , y = "New Cases" , x = "Date" , colour = "Country") +
  theme(plot.title = element_text(colour = "Black" , size = 17 , face = "bold" , hjust = 0.6 ),
        plot.subtitle = element_text(colour = "black" , size = 15 , face = "bold" , hjust = 0.6 ), 
        legend.title = element_text(colour = "black" , size = 16 , face = "bold" ) ,
        legend.text =  element_text(colour = "black" , size = 10) ,
        strip.text.x = element_text(colour = "black" , size = 10, face = "italic" )
  )


#new cases across new countires, Bar Graph
ggplot(filter(covid_dataset, location=="China" | location=="Japan" | location=="Germany"), 
       aes(fill=location, y=new_cases_smoothed, x=year)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_grid(. ~ location) +
  theme_classic() +
  labs(fill = "Country\n", title =expression(underline("New Cases")) ,
       subtitle = "across all countries, 2020-2022" , y = "New Cases" , x = "Date" , colour = "Country") +
  theme(plot.title = element_text(colour = "Black" , size = 17 , face = "bold" , hjust = 0.6 ),
        plot.subtitle = element_text(colour = "black" , size = 15 , face = "bold" , hjust = 0.6 ), 
        legend.title = element_text(colour = "black" , size = 16 , face = "bold" ) ,
        legend.text =  element_text(colour = "black" , size = 10) ,
        strip.text.x = element_text(colour = "black" , size = 10, face = "italic" )
  )


#new cases, Area Bar 
ggplot( filter(covid_dataset, !is.na(new_cases_smoothed)) )+ 
  geom_bar(aes(x=date_1, y=new_cases_smoothed), stat="identity", colour="#00bbf6", size=1.0)+
  facet_grid(. ~ location) +
  labs(title= "New Cases\n", x="date",y="New Cases")+
  theme_linedraw()+ 
  theme(plot.title = element_text(color = "black", size = 16, hjust=0.5, face = "bold"),
        strip.text.x = element_text(color = "White", size = 12),
        axis.title.x.bottom = element_text(color = "black", size = 12, face = "bold"),
        axis.title.y.left = element_text(color = "black", size = 12, face = "bold"),
        axis.text = element_text(color = "black", size = 10, face = "bold"))

#------------------------Vaccination--------------------------------

#Total vaccinations done in all countries, Scatterplot
ggplot(covid_dataset,
       aes(x = date_1, y = total_vaccinations, color = location  )) +
  geom_point(alpha= 0.4 , size = 1) + 
  labs(colour = "Country\n", title = "Comparing Total Vaccination\nacross all countires" , 
       x = "Date" , y = "Total Vaccination\n") + 
  scale_color_brewer(palette = "Dark2") +
  theme_linedraw() + 
  theme(plot.title = element_text(color = "black", size = 15, hjust=0.6, face = "bold"),
        strip.text.x = element_text(color = "black", size = 13),
        axis.title.x.bottom = element_text(color = "black", size = 13, face = "bold"),
        axis.title.y.left = element_text(color = "black", size = 13, face = "bold"),
        axis.text = element_text(color = "black", size = 10, face = "bold"),
        legend.title = element_text(colour = "black" , size = 15, face = "bold" ) ,
        legend.text =  element_text(colour = "black" , size = 14, face = "italic" ))

#------------------------Deaths--------------------------------

#Total deaths across all 6 countries, PIE Chart

ggplot(filter(covid_dataset, date_1=="2022-12-31"), 
       aes(x = "", y = total_deaths, fill = location)) +
  geom_col(color = "black") +
  geom_label(aes(label = total_deaths),
             color = "white",
             position = position_stack(vjust = 0.2),
             show.legend = FALSE)+
  theme_void() + 
  labs(fill = "Country\n", title =expression(underline("Total Covid Deaths")) ,
       subtitle = "Across all the countires till Dec, 2022") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(colour = "Red" , size = 17 , face = "bold" , hjust = 0.6 ),
        plot.subtitle = element_text(colour = "black" , size = 15 , face = "bold" , hjust = 0.6 ), 
        legend.title = element_text(colour = "black" , size = 16 , face = "bold" ) ,
        legend.text =  element_text(colour = "black" , size = 14) ,
  )

#new deaths across new countires, Bar Graph
ggplot(filter(covid_dataset, location=="India" | location=="United States" | location=="United Kingdom"), 
       aes(fill=location, y=new_deaths_smoothed, x=year)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_grid(. ~ location) +
  theme_classic() +
  labs(fill = "Country\n", title =expression(underline("New Cases")) ,
       subtitle = "across all countries, 2020-2022" , y = "New Deaths" , x = "Date" , colour = "Country") +
  theme(plot.title = element_text(colour = "Black" , size = 17 , face = "bold" , hjust = 0.6 ),
        plot.subtitle = element_text(colour = "black" , size = 15 , face = "bold" , hjust = 0.6 ), 
        legend.title = element_text(colour = "black" , size = 16 , face = "bold" ) ,
        legend.text =  element_text(colour = "black" , size = 10) ,
        strip.text.x = element_text(colour = "black" , size = 10, face = "italic" )
  )

#new deaths across new countires, Bar Graph
ggplot(filter(covid_dataset, location=="China" | location=="Japan" | location=="Germany"), 
       aes(fill=location, y=new_deaths_smoothed, x=year)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_grid(. ~ location) +
  theme_classic() +
  labs(fill = "Country\n", title =expression(underline("New Cases")) ,
       subtitle = "across all countries, 2020-2022" , y = "New Deaths" , x = "Date" , colour = "Country") +
  theme(plot.title = element_text(colour = "Black" , size = 17 , face = "bold" , hjust = 0.6 ),
        plot.subtitle = element_text(colour = "black" , size = 15 , face = "bold" , hjust = 0.6 ), 
        legend.title = element_text(colour = "black" , size = 16 , face = "bold" ) ,
        legend.text =  element_text(colour = "black" , size = 10) ,
        strip.text.x = element_text(colour = "black" , size = 10, face = "italic" )
  )

#--------------Total Test vs Total Deaths--------------------

#total cases vs total deaths, Scatterplot

ggplot(covid_dataset, 
       aes(total_cases, total_deaths, color = location)) +
  geom_point(size = 2) +
  theme_classic() +
  labs(fill = "Country\n", title =expression(underline("Total Deaths vs Total Cases")) ,
       subtitle = "across all countries, 2020-2022" , x = "Total Cases", y = "Total Deaths" , colour = "Country") +
  theme(plot.title = element_text(colour = "Black" , size = 17 , face = "bold" , hjust = 0.6 ),
        plot.subtitle = element_text(colour = "black" , size = 15 , face = "bold" , hjust = 0.6 ), 
        legend.title = element_text(colour = "black" , size = 16 , face = "bold" ) ,
        legend.text =  element_text(colour = "black" , size = 10) ,
        strip.text.x = element_text(colour = "black" , size = 10, face = "italic" )
  )

#------------------------Population--------------------------------

#population density, violin
ggplot(covid_dataset, aes(x=date_1, y=population_density, color = location)) +
  geom_violin() +
  labs(fill = "Country\n", title =expression(underline("Population Density")) ,
       subtitle = "across all countries, 2020-2022" , x = "Year", y = "Population Density" , colour = "Country") +
  theme(plot.title = element_text(colour = "Black" , size = 17 , face = "bold" , hjust = 0.6 ),
        plot.subtitle = element_text(colour = "black" , size = 15 , face = "bold" , hjust = 0.6 ), 
        legend.title = element_text(colour = "black" , size = 16 , face = "bold" ) ,
        legend.text =  element_text(colour = "black" , size = 10) ,
        strip.text.x = element_text(colour = "black" , size = 10, face = "italic" ))
        
#Dodge is used to preventing overlapping
#Stat Identity, if stat = identity, it will explicitly give the values of Y axis .


#---------------------------REGRESSION---------------------------------------------------

#Creating new dataset for regression

covid_reg_main <- data.frame(
  location = covid_dataset$location,
  year = covid_dataset$year,
  date = covid_dataset$date_1,
  total_cases = covid_dataset$total_cases,
  total_deaths = covid_dataset$total_deaths,
  new_cases = covid_dataset$new_cases,
  new_deaths = covid_dataset$new_deaths
)


#filtering the dataset for a single countries, United kingdom
covid_reg_submain <- filter(covid_reg_main, 
                    location=="United Kingdom" & 
                      year != 2023)


#Removing NA values from the regression dataset
covid_reg_submain <- na.exclude(covid_reg_submain)


#counting number of rows
nrow(covid_reg_submain)


#Creating train and test datasets for regression
covid_train_dataset <- covid_reg_submain[1:741,]
covid_test_dataset <- covid_reg_submain[742:1059,]


#Checking the covid train datasset
covid_train_dataset[1:10,]
summary(covid_train_dataset)


#Plotting scatterplot for train dataset
ggplot(
  data=covid_train_dataset,
  aes(x=new_cases, y=new_deaths)
) + geom_point()


#Calculating Pearson's correlation
cor.test(
  covid_train_dataset$new_cases,
  covid_train_dataset$new_deaths
)


#Calculating Linear regression 
mod_train_cases <- lm(
  formula=new_deaths~new_cases,  # predicting new deaths using new cases
  data=covid_train_dataset
)


#Checking the value of train dataset
summary(mod_train_cases)
coef(mod_train_cases)


#Regression:

#copying coeficient of the train dataset into a variable
coefs_train_cases <- coef(mod_train_cases)


#Plotting the fitine on the train dataset
ggplot(
  data=covid_train_dataset,
  aes(x=new_cases, y=new_deaths)
) +
  geom_point() +
  geom_abline(mapping=aes(
    slope=coefs_train_cases["new_cases"],
    intercept=coefs_train_cases["(Intercept)"]
  ), color='blue')


#Creating residual dataset
covid_resid_train <- covid_train_dataset  # make a copy of the dataset, to leave the original untouched
covid_resid_train$predicted <- predict(mod_train_cases) #adding predicted values 
covid_resid_train$residuals <- residuals(mod_train_cases) #adding residuals values


#checking the residual dataset
covid_resid_train[1:10,]


#plotting residual from prediction line
ggplot(
  data=covid_resid_train,
  aes(x=new_cases,y=new_deaths)
) +
  geom_point(size=2) +
  geom_point(size=2, aes(y=predicted), shape=4) +
  geom_segment(aes(xend=new_cases, yend=predicted), alpha=0.9, color='pink') +
  geom_abline(mapping=aes(
    slope=coefs_train_cases["new_cases"],
    intercept=coefs_train_cases["(Intercept)"]
  ), color='red')


#Residuals vs Fitted values
plot(
  mod_train_cases,
  which=1
)


#linear regression model, prediction
#R’s predict() function include calculating the 95% confidence intervals around these predictions
predict(
  mod_train_cases,
  newdata=covid_test_dataset,
  interval='confidence'
)


covid_test <- covid_test_dataset  # make a copy of the test data to leave the original unaltered
covid_test$predicted <- predict(mod_train_cases, newdata=covid_test)
covid_test$residuals <- covid_test$predicted - covid_test$new_deaths
covid_test


ggplot(
  data=covid_test,
  aes(x=new_cases,y=new_deaths)
) +
  geom_point(size=3) +  # make the actual values show up more clearly
  geom_point(size=2, aes(y=predicted), shape=4) +  # show the predicted values
  geom_segment(aes(xend=new_cases, yend=predicted), alpha=0.9, color='pink') +
  geom_abline(mapping=aes(
    slope=coefs_train_cases["new_cases"],
    intercept=coefs_train_cases["(Intercept)"]
  ), color='red')

sse_cases <- sum(covid_test$residuals**2)
sse_cases



--------------------------------------------------------------------------------------------




#TIME SERIES ANALYSIS

x <- covid_reg_submain$total_deaths

# library required for decimal_date() function
library(lubridate)
library(forecast)

# creating object for time series from 22 January, 2020.
mtts <- ts(x, start = decimal_date(ymd("2020-01-22")),
          frequency = 365.25 / 1)

# plotting the Daily data graph
plot(mtts, xlab ="Daily Data",
     ylab ="Total covid deaths",
     main ="COVID-19, Pandemic",
     col.main ="darkblue")


#forecasting time series model using Arima model

fit <- auto.arima(mtts)


#forcasting next 180 values
forecast(fit, 180)


#plotting the graph with next 6 months forecasted values
plot(forecast(fit, 180), xlab ="Daily Data",
     ylab ="Total Covid Deaths",
     main ="COVID-19 Pandemic", col.main ="darkblue")







