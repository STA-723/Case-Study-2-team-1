
## Handle Data 

# read in data
df <- read_csv("AB_NYC_2019.csv")
head(df)

# handle missing data
df = subset(df,select=-c(host_name,host_id,id))
df = df[!is.na(df$name),]
# set reviews == 0
df$reviews_per_month[is.na(df$reviews_per_month)] = 0
df$number_of_reviews[is.na(df$number_of_reviews)] = 0

# handle extreme values
df = df[df$price>=9&df$price<9999,]

# create name length var
split_names = strsplit(df$name," ")
df$name_length = lengths(split_names)

# availability 365-
# indicator 0,1 and raw number
#df$availability_indicator = ifelse(df$availability_365==0,0,1)
# drop if available 0 days per year
df = df[df$availability_365!=0,]

## CLEAN DATA HERE
# decide to only look at normal stays
# drop min stays greater than 2 weeks
df = df[df$minimum_nights<=14,]
# only dropping around 5000 obs

# popularity metric
df$number_of_months = ifelse(df$reviews_per_month==0,NA,
                             df$number_of_reviews*(df$reviews_per_month)^(-1))

# break up last review into year and month
df$last_review_yr = as.numeric(substr(df$last_review,1,4))
df$last_review_mth = as.numeric(substr(df$last_review,6,7))
# drop last review column
df = subset(df,select=-c(last_review))

# impute last review date and number of months
# imp = mice(df,m=2)
# df = mice::complete(imp)
df = df[df$reviews_per_month!=0, ]



# explore heterogeneity question
# ggplot(df,aes(x=neighbourhood_group,y=log(price))) +
#   geom_boxplot() +
#   ggtitle("Price by Burrows") + ylab("Price (log)") + xlab("Burrow")
ggplot(df,aes(x=room_type,y=log(price))) +
  geom_boxplot() +
  ggtitle("Price by room") + ylab("Price (log)") + xlab("Room Type")


ggplot(df,aes(x=neighbourhood_group,y=log(reviews_per_month))) +
  geom_boxplot() +
  ggtitle("Reviews by Burrows") + ylab("Monthly Reviews") + xlab("Burrow")+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



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


## Fig 1 used
rooms = unique(df$room_type)
ggplot(df[df$room_type==rooms[3],],aes(x=neighbourhood,y=log(price))) +
  geom_boxplot() +
  ggtitle("Price by Neighbourhood") + ylab("Price (log)") + xlab("Neighbourhood")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

## Fig2 used 
ggplot(df,aes(x=room_type,y=log(price),col=neighbourhood_group)) +
  geom_boxplot() +
  ggtitle("Price by Burrows") + ylab("Price (log)") + xlab("Room Type")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
