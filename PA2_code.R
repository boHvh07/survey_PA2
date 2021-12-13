library(readr)
data_60 <- read_csv("~/Downloads/data_60.csv")
View(data_60)

"###Cleaning###"
summary(data_60)

"### Weird Variables###"

#####Duplicates#####
data60_cl <- add_count(data_60, x1, x2, x3, x4, m1, m2, m3, m4, m5, y1, y2, y3, y4, y5, 
                                name = "duplicate") # Name of count variable is "duplicate"            

data60_cl<- data60_cl[order(-data60_cl$duplicate),]

data60_cl <- data60_cl[data60_cl$id != 13, ]
data60_cl <- data60_cl[data60_cl$id != 80, ]  #Duplicates cases removed

data60_cl$weird <- 0
data60_cl$weird[data60_cl$duplicate == 2] <- 1  #Add questionable duplicates to weird as 1

#####Flatliners#####
data60_cl$sd <- apply(data60_cl[2:15], 1 , sd)

data60_cl$weird[data60_cl$sd == 0] <- 2

data60_cl <- data60_cl[order(-data60_cl$weird, data60_cl$id),]

####Outliers####
data60_cl$x1[data60_cl$x1 > 5] <- NA
data60_cl$x2[data60_cl$x2 > 5] <- NA
data60_cl$x3[data60_cl$x3 > 5] <- NA
data60_cl$x4[data60_cl$x4 > 5] <- NA
data60_cl$m1[data60_cl$m1 > 5] <- NA
data60_cl$m2[data60_cl$m2 > 5] <- NA
data60_cl$m3[data60_cl$m3 > 5] <- NA
data60_cl$m4[data60_cl$m4 > 5] <- NA
data60_cl$m5[data60_cl$m5 > 5] <- NA
data60_cl$y1[data60_cl$y1 > 5] <- NA
data60_cl$y2[data60_cl$y2 > 5] <- NA
data60_cl$y3[data60_cl$y3 > 5] <- NA
data60_cl$y4[data60_cl$y4 > 5] <- NA
data60_cl$y5[data60_cl$y5 > 5] <- NA

summary(data60_cl) #Recoded incorrect data entries 

data60_cl$weird[data60_cl$v17 >= 10] <- 3  # outliers marked as weird in V17
data60_cl <- data60_cl[order(-data60_cl$weird, data60_cl$id),]


