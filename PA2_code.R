library(readr)
library(haven)
library(dplyr) 
library(psych)


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


### Check Skewness ###
skew_x1 <- round(skew(data60_cl$x1),2)  
skew_x2 <- round(skew(data60_cl$x2),2) 
skew_x3 <- round(skew(data60_cl$x3),2) 
skew_x4 <- round(skew(data60_cl$x4),2) 

skew_m1 <- round(skew(data60_cl$m1),2)  
skew_m2 <- round(skew(data60_cl$m2),2) 
skew_m3 <- round(skew(data60_cl$m3),2) 
skew_m4 <- round(skew(data60_cl$m4),2) 
skew_m5 <- round(skew(data60_cl$m5),2)

skew_y1 <- round(skew(data60_cl$y1),2)  
skew_y2 <- round(skew(data60_cl$y2),2) 
skew_y3 <- round(skew(data60_cl$y3),2) 
skew_y4 <- round(skew(data60_cl$y4),2) 
skew_y5 <- round(skew(data60_cl$y5),2)

data60_sk <- data60_cl

data60_sk$Z_x1 <- scale(data60_sk$x1, center=TRUE, scale=TRUE)
data60_sk$Z_x2 <- scale(data60_sk$x2, center=TRUE, scale=TRUE)
data60_sk$Z_x3 <- scale(data60_sk$x3, center=TRUE, scale=TRUE)
data60_sk$Z_x4 <- scale(data60_sk$x4, center=TRUE, scale=TRUE)

data60_sk$Z_m1 <- scale(data60_sk$m1, center=TRUE, scale=TRUE)
data60_sk$Z_m2 <- scale(data60_sk$m2, center=TRUE, scale=TRUE)
data60_sk$Z_m3 <- scale(data60_sk$m3, center=TRUE, scale=TRUE)
data60_sk$Z_m4 <- scale(data60_sk$m4, center=TRUE, scale=TRUE)
data60_sk$Z_m5 <- scale(data60_sk$m5, center=TRUE, scale=TRUE)

data60_sk$Z_y1 <- scale(data60_sk$y1, center=TRUE, scale=TRUE)
data60_sk$Z_y2 <- scale(data60_sk$y2, center=TRUE, scale=TRUE)
data60_sk$Z_y3 <- scale(data60_sk$y3, center=TRUE, scale=TRUE)
data60_sk$Z_y4 <- scale(data60_sk$y4, center=TRUE, scale=TRUE)
data60_sk$Z_y5 <- scale(data60_sk$y5, center=TRUE, scale=TRUE)

summary(data60_sk)


data60_cl$weird[data60_cl$v17 >= 10] <- 3  # outliers marked as weird in V17
data60_cl <- data60_cl[order(-data60_cl$weird, data60_cl$id),]


"#### Scale Construction ####"
#### Construct new Scales ####
data60_cl$X <- rowMeans(data60_cl[,c(2,3,4,5)])

data60_cl$M <- rowMeans(data60_cl[,c(6,8,9,10,22)])

data60_cl$Y <- rowMeans(data60_cl[,c(11,12,13,14,15)])


"#### Weird Variables ####"

dfX <- data.frame (first_column  = (data60_cl$x1),
                  second_column = (data60_cl$x2),
                  third_column = (data60_cl$x3),
                  fourth_column = (data60_cl$x4) 
                )

dfX <- na.exclude(dfX)
names(dfX) = c("x1", "x2", "x3", "x4")
round(cor(dfX), 2)
alpha(dfX)


dfM <- data.frame (first_column  = (data60_cl$m1),
                   second_column = (data60_cl$m2),
                   third_column = (data60_cl$m3),
                   fourth_column = (data60_cl$m4),
                   fifth_column = (data60_cl$m5)
)
dfM <- na.exclude(dfM)
names(dfM) = c("m1", "m2", "m3", "m4", "m5")
round(cor(dfM), 2)
alpha(dfM)


dfY <- data.frame (first_column  = (data60_cl$y1),
                   second_column = (data60_cl$y2),
                   third_column = (data60_cl$y3),
                   fourth_column = (data60_cl$y4),
                   fifth_column = (data60_cl$y5)
)
dfY <- na.exclude(dfY)
names(dfY) = c("1y", "y2", "y3", "y4", "y5")
round(cor(dfY), 2)               
alpha(dfY)

#### Solve M2 ####
data60_cl$m2r[data60_cl$m2 == 1] <- 5
data60_cl$m2r[data60_cl$m2 == 2] <- 4
data60_cl$m2r[data60_cl$m2 == 3] <- 3
data60_cl$m2r[data60_cl$m2 == 4] <- 2
data60_cl$m2r[data60_cl$m2 == 5] <- 1

dfM2 <- data.frame (first_column  = (data60_cl$m1),
                   second_column = (data60_cl$m2r),
                   third_column = (data60_cl$m3),
                   fourth_column = (data60_cl$m4),
                   fifth_column = (data60_cl$m5)
)
dfM2 <- na.exclude(dfM2)
names(dfM2) = c("m1", "m2r", "m3", "m4", "m5")
round(cor(dfM2), 2)
alpha(dfM2)


"#### Analyses ####"

