library(readr)
data_60 <- read_csv("~/Downloads/data_60.csv")
View(data_60)

"###Cleaning###"
summary(data_60)

###MCAR?

# Drop variable X1: constant
data_60$id <- NULL

data_60$v17[data_60$v17 >= 100] <- NA

summary(data_60) #2 outliers removed out of v17

"### Weird Variables###"
data60_v2 <- 