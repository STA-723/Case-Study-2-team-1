library(readr)
library(mice)
library(tidyverse)

# read in data
df <- read_csv("AB_NYC_2019.csv")
head(df)

# handle missing data
df = subset(df,select=-c(host_name,host_id,id))
df = df[!is.na(df$name),]
# set reviews == 0
df[is.na(df$reviews_per_month), ] = 0
df[is.na(df$number_of_reviews), ] = 0

# handle extreme values
df = df[df$price>0&df$price<9999,]

# create name length var
split_names = strsplit(df$name, " ")
df = lengths(split_names)

# popularity metric
df$number_of_reviews*df$reviews_per_month

# availability 365-
# indicator 0,1 and raw number


# CHI SQUARED TEST- QUESTION 3


# FREQUENTIST METHODS- QUESTION 2/3


# OBTAIN DESIGN MATRIX/ DATA MATRICES


# JAGS- LOG(PRICE)


# JAGS- NEG BINOMIAL

# MAP










