library(readr)
library(mice)
library(tidyverse)
library(MASS)
<<<<<<< HEAD
library(tidytext)
=======
library(R2jags)

>>>>>>> 9925b6c77f75acb93508bbfc79585b00d46e9823

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
imp = mice(df,m=2)
df = mice::complete(imp)


## EDA

# heterogeneity question
ggplot(df,aes(x=room_type,y=log(price))) +
  geom_boxplot() +
  ggtitle("Price by room") + ylab("Price (log)") + xlab("Room Type")

ggplot(df,aes(x=room_type,y=log(price),col=neighbourhood_group)) +
  geom_boxplot() +
  ggtitle("Price by Borough") + ylab("Price (log)") + xlab("Room Type")

rooms = unique(df$room_type)
ggplot(df[df$room_type==rooms[3],],aes(x=neighbourhood,y=log(price))) +
  geom_boxplot() +
  ggtitle("Price by Neighbourhood") + ylab("Price (log)") + xlab("Neighbourhood")

ggplot(df,aes(x=neighbourhood_group,fill=room_type)) +
  geom_bar(position="fill") +
  ggtitle("Rooms by Borough") + 
  xlab("Borough") + ylab("Frequency")


## Modeling 


# CHI SQUARED TEST- QUESTION 3


# FREQUENTIST METHODS- QUESTION 2/3


# write character variables as factor
df$neighbourhood = as.factor(df$neighbourhood)
df$neighbourhood_group = as.factor(df$neighbourhood_group)
df$room_type = as.factor(df$room_type)

# R lm on log(price)
model_price = lm(price ~ neighbourhood_group + reviews_per_month + 
                   room_type + calculated_host_listings_count + minimum_nights +
                   name_length + availability_indicator, data = df)
summary(model_price)
# everything important except calculated host listing count

# R Neg Bin
model_pop = glm(reviews_per_month ~ neighbourhood_group + price + 
               room_type + calculated_host_listings_count + minimum_nights +
               name_length + availability_indicator | number_of_months, data = df, family = binomial(link='log'))
summary(model_pop)
# important for determing popularity: almost everything in model


# OBTAIN DESIGN MATRIX/ DATA MATRICES
des_mat = model.matrix(lm(reviews_per_month ~ neighbourhood_group + price + 
                            room_type + calculated_host_listings_count + minimum_nights +
                            name_length + availability_indicator-1, data = df))
View(des_mat)


# JAGS- LOG(PRICE)


# JAGS- NEG BINOMIAL

nbmodel = function(){
  ## Likelihood
  for(i in 1:N){
    y[i] ~ dnegbin(p[i],r)
    p[i] <- r/(r+lambda[i]) 
    log(lambda[i]) <- mu[i]
    mu[i] <- inprod(beta[],X[i,])
  } 
  ## Priors
  beta ~ dmnorm(mu.beta,tau.beta)
  r ~ dunif(0,50)
}

forJags = list(X=cbind(1,df$minimum_nights),
                y=c(df$reviews_per_month),
                N=nrow(df),
                mu.beta=rep(0,2),
                tau.beta=diag(.0001,2))


ZSout = jags(forJags,model=nbmodel, inits=NULL,    
             parameters.to.save=c("r", "beta"), 
             n.iter=10000)



# MAP


# TEXT ANALYSIS

#Evan's note:  I'm confused about our name length, so here's another option
#This gives characters in the name, rather than words.  I'm just putting this 
#in front of the rest of the analysis.  
split2=strsplit(df$name,"")
df$name_char=lengths(split2)

#I need the transaction ID in the data, or so I think, so I'll replicate all
#the data cleaning except for that here
df2 <- read_csv("AB_NYC_2019.csv")

# handle missing data
df2 = subset(df2,select=-c(host_name,host_id))
df2 = df2[!is.na(df2$name),]
# set reviews == 0
df2$reviews_per_month[is.na(df2$reviews_per_month)] = 0
df2$number_of_reviews[is.na(df2$number_of_reviews)] = 0

# handle extreme values
df2 = df2[df2$price>=9&df2$price<9999,]

# create name length var
split_names = strsplit(df2$name," ")
df2$name_length = lengths(split_names)

# availability 365-
# indicator 0,1 and raw number
#df2$availability_indicator = ifelse(df2$availability_365==0,0,1)
# drop if available 0 days per year
df2 = df2[df2$availability_365!=0,]

## CLEAN DATA HERE
# decide to only look at normal stays
# drop min stays greater than 2 weeks
df2 = df2[df2$minimum_nights<=14,]




#Back to LDA/sentiment analysis/whatever
df2$backupname = df2$name
dftoken = df2 %>% unnest_tokens(word,name)
dftoken %>% count(word, sort=TRUE)
#Lots of room/bedroom/apt, plus borough

data(stop_words)

dftoken = dftoken %>% anti_join(stop_words,by="word")
dftoken %>% count(word, sort=TRUE)
#With common words removed, more of the same, less "to"

#Note:  I tried to vectorize this logical for an hour, gave up and did this.  Feel free to alter.
dftoken = dftoken %>% filter(word != "apt" & word != "apartment" & word != "manhattan" & word != "brooklyn" & word != "room" & word != "bedroom" & word != "nyc")
dftoken %>% count(word, sort=TRUE)

#adapting copied code from tidytextmining, Silge & Robinson
word_by_price <- dftoken %>% 
  group_by(word) %>% 
  summarise(price = median(price), uses = n()) %>%
  ungroup()

options(tibble.print_max=100)

word_by_price = word_by_price %>% 
  filter(uses >= 5) %>%
  arrange(desc(price))

print(word_by_price, n=25)

dftoken %>% 
  filter(word == "fee")

