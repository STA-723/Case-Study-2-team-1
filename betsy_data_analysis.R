library(readr)
library(mice)
library(tidyverse)



# read in data
df <- read_csv("AB_NYC_2019.csv")
head(df)

# missing vars
colSums(is.na(df))
nrow(df)
# 1/5 review data missing- use mice to impute. continue exploring missing data though
# drop very small number of name and host name missing rows
df = df[!is.na(df$name),]
df = df[!is.na(df$host_name),]
# now use mice to impute review data
imp <- mice(df)
df <- mice::complete(imp)
##~~~~~!!! can't use mice on date 

# break up last review into year and month
df$last_review_yr = as.numeric(substr(df$last_review,1,4))
df$last_review_mth = as.numeric(substr(df$last_review,6,7))
# drop last review column
df = subset(df,select=-c(last_review))

# drop price = 0
# note- high extreme prices may not be outliers or mistakes:
# https://www.thevacationtimes.com/2017/09/expensive-airbnb-new-york-city/
df = df[df$price>0,]


# set review missing data to 0

# text analysis on df$name
bnb_name_df = df[,2,drop=F]
split_names = strsplit(df$name, " ")
bnb_name_df$name_length = lengths(split_names)

# explore unique names
# make all names lower case
split_names_vec = unlist(split_names) %>% tolower()
unq_names = unique(split_names_vec)
unq_names_count = sapply(unq_names,function(x)sum(split_names_vec==x))
common_names=data.frame(names=unq_names,count=unq_names_count) %>% 
  arrange(desc(count))
## most common words clear descriptors of the type of room "room, apartment",etc and
## location "Manhatten",etc
# probably more useful to do this once we have determined a set of 'popular' bnbs.


## need some kind of standardized number of reviews

# explore heterogeneity question
# ggplot(df,aes(x=neighbourhood_group,y=log(price))) +
#   geom_boxplot() +
#   ggtitle("Price by Burrows") + ylab("Price (log)") + xlab("Burrow")
ggplot(df,aes(x=room_type,y=log(price))) +
  geom_boxplot() +
  ggtitle("Price by room") + ylab("Price (log)") + xlab("Room Type")

ggplot(df,aes(x=room_type,y=log(price),col=neighbourhood_group)) +
  geom_boxplot() +
  ggtitle("Price by Burrows") + ylab("Price (log)") + xlab("Room Type")
ggplot(df,aes(x=neighbourhood_group,y=reviews_per_month)) +
  geom_boxplot() +
  ggtitle("Reviews by Burrows") + ylab("Monthly Reviews") + xlab("Burrow")

rooms = unique(df$room_type)
ggplot(df[df$room_type==rooms[3],],aes(x=neighbourhood,y=log(price))) +
  geom_boxplot() +
  ggtitle("Price by Neighbourhood") + ylab("Price (log)") + xlab("Neighbourhood")

ggplot(df,aes(x=neighbourhood,y=reviews_per_month)) +
  geom_boxplot() +
  ggtitle("Reviews by Neighbourhood") + ylab("Monthly Reviews") + xlab("Neighbourhood")

df %>% select(neighbourhood_group,room_type) %>% 
  group_by(neighbourhood_group,room_type) %>% 
  summarise(n())

ggplot(df,aes(x=neighbourhood_group,fill=room_type)) +
  geom_bar(position="fill") +
  ggtitle("Rooms by Burrow") + 
  xlab("Burrow") + ylab("Frequency")

ggplot(df,aes(x=neighbourhood,fill=room_type )) +
  geom_bar(position="fill") +
  ggtitle("Rooms by Neighbourhood") + 
  xlab("Neighbourhood") + ylab("Frequency")

# spatial model

# drop extreme min nights

# zero inflated modeling for reviews per month - assess popularity




