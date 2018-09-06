library(data.table)
library(lubridate)
library(ggplot2)
library(grid)
source("asianart_func.R")

# Read Visitors data ====

asianart_visitors <- readVisitorData()
head(asianart_visitors)
summary(asianart_visitors)
str(asianart_visitors)


# Read GA data