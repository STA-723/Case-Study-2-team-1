library(readr)
library(mice)
library(tidyverse)
library(MASS)
library(tidytext)
library(R2jags)

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

##############
#Spatial Stuff
##############

#set up knots for spatial component
summary(df$longitude)
summary(df$latitude)

quilt.plot(df$longitude,df$latitude,log(df$price))


lon = seq(-74.34,-73.61,.025)
lat = seq(40.4,41.01,.025)

knots = cbind(rep(lon,each=length(lat)),rep(lat,length(lon)))
dim(knots)

d = function(x1,y1,x2,y2){
  sqrt((x1-x2)^2 + (y1-y2)^2)
}

dist = matrix(NA,dim(df)[1],dim(knots)[1])
for(i in 1:dim(df)[1]){
  dist[i,] <- d(df$longitude[i],df$latitude[i],knots[,1],knots[,2]) 
}

mindist = apply(dist,2,min)
drop = which(mindist > .03)
plot(knots[-drop,])
dim(knots[-drop,])

dist2 = dist[,-drop]

plon = seq(-74.34,-73.61,.00625)
plat = seq(40.4,41.01,.00625)
ploc = cbind(rep(plon,each=length(plat)),rep(plat,length(plon)))

pd_dist = matrix(NA,dim(df)[1],dim(ploc)[1])
for(i in 1:dim(df)[1]){
  pd_dist[i,] <- d(df$longitude[i],df$latitude[i],ploc[,1],ploc[,2]) 
}

pmindist = apply(pd_dist,2,min)
pdrop = which(pmindist > .01)
points(ploc[-pdrop,],pch='.')
dim(ploc[-pdrop,])

pd_dist2 = pd_dist[,-pdrop]
dim(pd_dist2)

knots2 = knots[-drop,]
nknots = dim(knots2)[1]
ploc2 = ploc[-pdrop,]

pk_dist = matrix(NA,dim(ploc2)[1],dim(knots2)[1])
for(i in 1:dim(ploc2)[1]){
  pk_dist[i,] <- d(ploc2[i,1],ploc2[i,2],knots2[,1],knots2[,2]) 
}

dim(pk_dist)

pr = 1
K = data.frame(dnorm(dist2,0,.025*pr))
pK = data.frame(dnorm(pk_dist,0,.025*pr))

#fit with lm
test = lm(log(df$price)~.,data=K)
#predict at locations through domain
ptest = predict.lm(test,newdata = pK)

#can see impact of edge effects, probably not a huge concern, but means we need to be careful
#not actually sure if predictive locations are valuable given how dense the observations are and how
#we don't have coordinates/shapefiles for the neighborhoods
quilt.plot(ploc2[,1],ploc2[,2],ptest)
points(knots2)



par(mfrow = c(1,3)) #just use first two plots in slide for presentation
quilt.plot(df$longitude,df$latitude,log(df$price),zlim = c(3,max(log(df$price))))
quilt.plot(df$longitude,df$latitude,test$fitted.values,zlim = c(3,max(log(df$price))))
points(knots2)
quilt.plot(xlim= range(df$longitude),ylim=range(df$latitude),ploc2[,1],ploc2[,2],ptest,zlim = c(3,max(log(df$price))))
points(knots2)

#will calculate best location through sum of burrough effect, neighborhood effect, and spatial effect

par(mfrow= c(1,1))
#############################



## Modeling 




# write character variables as factor
df$neighbourhood = as.factor(df$neighbourhood)
df$neighbourhood_group = as.factor(df$neighbourhood_group)
df$room_type = as.factor(df$room_type)



# OBTAIN DESIGN MATRIX/ DATA MATRICES
des_mat = model.matrix(lm(reviews_per_month ~ price + room_type + 
                            calculated_host_listings_count + minimum_nights +
                            name_length + availability_365 + last_review_yr, data = df))
View(des_mat)

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


# JAGS- LOG(PRICE)


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
              parameters.to.save=c("beta","beta_b","beta_n"), 
              n.iter=8000)


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
                     offset(log(reviews_per_month)),
                   data = df)

summary(model_pop)


