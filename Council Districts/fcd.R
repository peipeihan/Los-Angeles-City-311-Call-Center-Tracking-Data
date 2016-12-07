#install.packages("sparklyr")
#spark_install()
library(sparklyr)
library(dplyr)


### establish a connection with our local spark
SC <- spark_connect(master = "local")

### get the dataset
library(nycflights13)
dim(flights) 

# data = read.csv("flights.csv")

### get the data to spark
flights_data <- copy_to(dest = sc, df = flights, name = "flights") # 3 

### use dplyr to interact with spark
# a is not a data frame

a = flights_data %>%
    filter(dep_delay == 2)


### suppose that you want to move the data back into R, then you need to use the "collect" command

b = flights.data %>%
    filter(dep_deplay == 2) %>%
    collect


### using SQL

library(DBI)

dbGetQuery(sc,
           "SELECT * 
            FROM flights
            WHERE month == 2")



#### Machine Learning
?mtcars

## move the data to spark cluster

mtcars_data <- copy_to(sc, mtcars, "mtcars")

### split the data into training and testing

partitions <- mtcars_data %>%
  sdf_partition(training = 0.8,
                testing = 0.2,
                seed = 100)

# fit is done on spark
lm_fit <- partitions$training  %>%
  #ml_decision_tree()
          ml_linear_regression(response = "mpg",
                               features = c("wt","cyl"))


summary(lm_fit)



#### http://spark.rstudio.com/


