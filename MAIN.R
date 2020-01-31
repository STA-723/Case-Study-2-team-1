library(readr)
library(mice)
library(tidyverse)
library(MASS)
library(R2jags)


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


## EDA

# heterogeneity question
ggplot(df,aes(x=room_type,y=log(price))) +
  geom_boxplot() +
  ggtitle("Price by room") + ylab("Price (log)") + xlab("Room Type")

ggplot(df,aes(x=room_type,y=log(price),col=neighbourhood_group)) +
  geom_boxplot() +
  ggtitle("Price by Burrows") + ylab("Price (log)") + xlab("Room Type")

rooms = unique(df$room_type)
ggplot(df[df$room_type==rooms[3],],aes(x=neighbourhood,y=log(price))) +
  geom_boxplot() +
  ggtitle("Price by Neighbourhood") + ylab("Price (log)") + xlab("Neighbourhood")

ggplot(df,aes(x=neighbourhood_group,fill=room_type)) +
  geom_bar(position="fill") +
  ggtitle("Rooms by Burrow") + 
  xlab("Burrow") + ylab("Frequency")


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
model_pop = glm.nb(reviews_per_month ~ neighbourhood_group + price + 
               room_type + calculated_host_listings_count + minimum_nights +
               name_length , data = df)
summary(model_pop)
# important for determing popularity: almost everything in model


# OBTAIN DESIGN MATRIX/ DATA MATRICES
des_mat = model.matrix(lm(reviews_per_month ~ price + room_type + 
                            calculated_host_listings_count + minimum_nights +
                            name_length + availability_365 + last_review_yr, data = df))
View(des_mat)

neighb_burrow_mat = unique(as.matrix(subset(df,select=c(neighbourhood,neighbourhood_group))))
neighbs = neighb_burrow_mat[,1]
X_neighbs = sapply(neighbs,function(x) ifelse(df$neighbourhood==x,1,0))
View(X_neighbs)

burrs = unique(neighb_burrow_mat[,2])
X_burrs = sapply(burrs,function(x) ifelse(df$neighbourhood_group==x,1,0))
View(X_burrs)

burrow_loc = rep(NA,length(neighbs))
for (i in 1:length(burrs)){
  burrow_loc[neighb_burrow_mat[,2]==burrs[i]] = i
}


# JAGS- LOG(PRICE)


# JAGS- NEG BINOMIAL

## subset df for ease
df = df[sample(1:nrow(df),500),]

nbmodel = function(){
  
  ## Likelihood
  for(i in 1:N){
    y[i] ~ dnegbin(p[i],r)
    p[i] <- r/(r+lambda[i]) 
    log(lambda[i]) <- mu[i]+exposure[i]
    mu[i] <- inprod(beta[],X[i,]) + inprod(beta_n[],X_n[i,])
  } 
  
  ## Hierarchy
  for (i in 1:p_n){
    mu.n[i] = beta_b[burrow_loc[i]]
  }
  beta_n ~ dmnorm(mu.n,tau.n)
  # burrows
  beta_b ~ dmnorm(mu.b,tau.b)
  
  ## Priors
  beta ~ dmnorm(mu.beta,tau.beta)
  r ~ dunif(0,50)
}

p = ncol(des_mat)
p_neighbs = ncol(X_neighbs)
p_burr = ncol(X_burrs)

forJags = list(X=des_mat,
               X_n = X_neighbs,
               y=c(df$number_of_reviews),
               exposure=log(df$reviews_per_month),
               N=nrow(df),
               mu.beta=rep(0,p),
               tau.beta=diag(.0001,p),
               mu.b = rep(0,p_burr),
               tau.b = diag(1,p_burr),
               tau.n = diag(1,p_neighbs),
               p_n = p_neighbs,
               burrow_loc = burrow_loc)


ZSout = jags(forJags,model=nbmodel, inits=NULL,    
             parameters.to.save=c("beta"), 
             n.iter=10000)


model_pop = glm.nb(number_of_reviews ~ price + room_type + 
                     calculated_host_listings_count + minimum_nights +
                     name_length + availability_365 + last_review_yr + 
                     offset(log(reviews_per_month)),
                data = df)

summary(model_pop)

# MAP










