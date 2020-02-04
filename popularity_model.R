library(readr)
library(mice)
library(tidyverse)
library(MASS)
library(tidytext)
library(R2jags)
library(xtable)

library(LatticeKrig)


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


par(mfrow= c(1,1))
#############################



## Modeling 




# write character variables as factor
df$neighbourhood = as.factor(df$neighbourhood)
df$neighbourhood_group = as.factor(df$neighbourhood_group)
df$room_type = as.factor(df$room_type)








df = df[1:500,]




# OBTAIN DESIGN MATRIX/ DATA MATRICES
des_mat = model.matrix(lm(reviews_per_month ~ price + room_type + 
                            calculated_host_listings_count + minimum_nights +
                            name_length + availability_365 + last_review_yr, data = df))





# data stuff needed for hierarchy
neighb_burrow_mat = unique(as.matrix(subset(df,select=c(neighbourhood,neighbourhood_group))))
neighbs = neighb_burrow_mat[,1]
X_neighbs = sapply(neighbs,function(x) ifelse(df$neighbourhood==x,1,0))
#View(X_neighbs)

burrs = unique(neighb_burrow_mat[,2])
X_burrs = sapply(burrs,function(x) ifelse(df$neighbourhood_group==x,1,0))
#View(X_burrs)

burrow_loc = rep(NA,length(neighbs))
for (i in 1:length(burrs)){
  burrow_loc[neighb_burrow_mat[,2]==burrs[i]] = i
}

N = dim(df)[1]



# JAGS- NEG BINOMIAL


nbmodel = function(){
  
  ## Likelihood
  for(i in 1:N){
    y[i] ~ dpois(lambda[i]*gamma[i]*exposure[i])
    gamma[i] ~ dgamma(1/phi,1/phi) 
    log(lambda[i]) <- mu[i]
    mu[i] <- inprod(beta[],X[i,]) + inprod(beta_n[],X_n[i,])
  } 
  phi ~ dgamma(2,1)
  
  ## Hierarchy
  for (i in 1:p_n){
    beta_n[i] ~ dnorm(beta_b[burrow_loc[i]],tau.n[i])
    tau.n[i] ~ dgamma(2,1)
  }
  
  # burrows
  for (i in 1:p_b){
    beta_b[i] ~ dnorm(mu.b,tau.b[i])
    tau.b[i] ~ dgamma(2,1)
  }
  
  ## Priors
  for ( i in 1:p){
    beta[i] ~ dnorm(mu.beta,tau.beta[i])
    tau.beta[i] ~ dgamma(2,1)
  }
  
}


p = ncol(des_mat)
p_neighbs = ncol(X_neighbs)
p_burr = ncol(X_burrs)

#nknots = ncol(K)
forJags = list(X=des_mat,
               X_n = X_neighbs,
               y=c(df$number_of_reviews),
               exposure=df$number_of_months,
               N=nrow(df),
               mu.beta=0,
               mu.b = 0,
               p_n = p_neighbs,
               p = p,
               p_b = p_burr,
               burrow_loc = burrow_loc)



ZSout2 = jags(forJags,model=nbmodel, inits=NULL,    
              parameters.to.save=c("beta","beta_b","beta_n","tau.beta","tau.n","tau.b"), 
              n.iter=10000)


beta_burr = ZSout$BUGSoutput$sims.list$beta_b
beta_neighb = ZSout$BUGSoutput$sims.list$beta_n

boxplot(beta_burr, horizontal=T,
        main = "Coefficient on Burrows")
axis(side=2,labels=burrs)

boxplot(beta_neighb[,burrow_loc==1], horizontal=T,
        main = paste0("Coef on Neighborhoods, ",burrs[1]))
axis(side=2,labels=F)
boxplot(beta_neighb[,burrow_loc==2], horizontal=T,
        main = paste0("Coef on Neighborhoods, ",burrs[2]))
boxplot(beta_neighb[,burrow_loc==3], horizontal=T,
        main = paste0("Coef on Neighborhoods, ",burrs[3]))
boxplot(beta_neighb[,burrow_loc==4], horizontal=T,
        main = paste0("Coef on Neighborhoods, ",burrs[4]))
boxplot(beta_neighb[,burrow_loc==5], horizontal=T,
        main = paste0("Coef on Neighborhoods, ",burrs[5]))



model_pop = glm.nb(number_of_reviews ~ price + room_type + 
                     calculated_host_listings_count + minimum_nights +
                     name_length + availability_365 + last_review_yr + 
                     offset(log(number_of_months)) + neighbourhood_group,
                   data = df)

summary(model_pop)


coefs = round(exp(coef(model_pop)),4) 
coef_notexp = round(coef(model_pop),4) 
coefs = coefs[-1]
out = cbind(colnames(coefs),coefs)
xtable(out)

