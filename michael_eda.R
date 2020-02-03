data = read.csv('AB_NYC_2019.csv')
dim(data)
summary(data)
length(table(data$neighbourhood))
hist(data$number_of_reviews)
hist(data$reviews_per_month)
table(data$number_of_reviews)
table(data$reviews_per_month)

sum(data$number_of_reviews == 0) 
sum(is.na(data$reviews_per_month))
sum(data$number_of_reviews == 0 & is.na(data$reviews_per_month))

table(data$neighbourhood_group)
table(data[data$neighbourhood_group == 'Bronx',]$neighbourhood)

plot(density(data$reviews_per_month,na.rm=T))
plot(data$number_of_reviews,ifelse(is.na(data$reviews_per_month),0,data$reviews_per_month))
cor(data$number_of_reviews[data$number_of_reviews != 0],data$reviews_per_month[!is.na(data$reviews_per_month)])


plot(density(data$price))
plot(density(log(data$price)))

x = seq(2,10,.01)
m = mean(log(data$price[data$price != 0]))
sd = sqrt(var(log(data$price[data$price != 0])))

lines(x,dnorm(x,m,sd),col='red')

data[data$price == 0,]
table(data$room_type)
data[data$price > 2000,]

m = mean(data$reviews_per_month,na.rm=T)
var(data$reviews_per_month,na.rm=T)

var(data$number_of_reviews[data$number_of_reviews>0])

plot(density(data$reviews_per_month,na.rm=T))
table(data$reviews_per_month)
test = rexp(50000,1/m)
lines(density(test),col='red')


y <- na.omit(data$reviews_per_month)
#different results if using a different cutoff
#y <- data2$gestational_age[data2$gestational_age < 45]

miny <- min(y)
maxy <- max(y)
unity <- (y - miny) / (maxy-miny) + 1e-6

library(MASS)
bc = boxcox(unity~1)
lambda <- bc$x[which.max(bc$y)]

box_tran <- function(dat,lam){
  if( lam != 0){             ## use power transformation in lam != 0
    (dat^lam - 1) / lam
  }else{                     ## use log(data) if lam =0
    log(dat)
  }
}

boxy <- box_tran(unity,lambda)

s <- sd(boxy)
m <- mean(boxy)
y <- (boxy - m) / s
plot(density(y,adjust=1.5))
x = seq(-4,4,length = 1000)
#plot(density(y,adjust=2))
#x = seq(-3,3,length=1000)
lines(x,dnorm(x,0,1),col='red')
table(data$number_of_reviews)

#set up knots for spatial component
summary(data$longitude)
summary(data$latitude)

lon = seq(-74.34,-73.61,.025)
lat = seq(40.4,41.01,.025)

knots = cbind(rep(lon,each=length(lat)),rep(lat,length(lon)))
dim(knots)

d = function(x1,y1,x2,y2){
  sqrt((x1-x2)^2 + (y1-y2)^2)
}

dist = matrix(NA,dim(data)[1],dim(knots)[1])
for(i in 1:dim(data)[1]){
  dist[i,] <- d(data$lon[i],data$lat[i],knots[,1],knots[,2]) 
}

mindist = apply(dist,2,min)
drop = which(mindist > .03)
par(mfrow = c(1,1))
plot(knots[-drop,])
dim(knots[-drop,])

dist2 = dist[,-drop]

plon = seq(-74.34,-73.61,.0125)
plat = seq(40.4,41.01,.0125)
ploc = cbind(rep(plon,each=length(plat)),rep(plat,length(plon)))

pd_dist = matrix(NA,dim(data)[1],dim(ploc)[1])
for(i in 1:dim(data)[1]){
  pd_dist[i,] <- d(data$lon[i],data$lat[i],ploc[,1],ploc[,2]) 
}

mindist2 = apply(pd_dist,2,min)
drop2 = which(mindist2 > .01)
points(ploc[-drop2,],pch='.')
dim(ploc[-drop2,])

pk_dist = matrix(NA,dim(ploc)[1],dim(knots)[1])
for(i in 1:dim(ploc)[1]){
  pk_dist[i,] <- d(ploc[i,1],ploc[i,2],knots[,1],knots[,2]) 
}

pd_dist2 = pd_dist[,-drop2]
dim(pd_dist2)

pk_dist2 = pk_dist[-drop2,-drop]
dim(pk_dist2)


library(LatticeKrig)

p = 1.5
K = data.frame(dnorm(dist2,0,.025*p))
test = lm(log(data$price+1)~.,data=K)

pK = data.frame(dnorm(pk_dist2,0,.025*p))
ptest = predict.lm(test,newdata = pK)

par(mfrow = c(1,3))
quilt.plot(data$longitude,data$latitude,log(data$price+1),zlim = c(3,max(log(data$price+1))))
quilt.plot(data$longitude,data$latitude,test$fitted.values,zlim = c(3,max(log(data$price+1))))
points(knots[-drop,])
quilt.plot(xlim= range(data$lon),ylim=range(data$lat),ploc[-drop2,1],ploc[-drop2,2],ptest,zlim = c(3,max(log(data$price+1))))
points(knots[-drop,])


