#lmer models

#take df as specified in main script

df$lprice = log(df$price)
df$last_review  = 2019 - df$last_review_yr
df$months_available = df$availability_365*12/365

library(lme4)
###################
#price model
###################
mod_price = lmer(lprice ~  reviews_per_month + 
                   room_type + calculated_host_listings_count + minimum_nights + name_length + 
                   months_available + last_review +(1|neighbourhood_group/neighbourhood), data = df)

relgrad <- with(mod_price@optinfo$derivs,solve(Hessian,gradient))
max(abs(relgrad))
#suggests we're good despite warning

summary(mod_price)


library(lmerTest)
ranova(mod_price)

ran_est = ranef(mod_price)

n_b = ran_est$`neighbourhood:neighbourhood_group`
b = ran_est$neighbourhood_group
rownames(b)[order(b[[1]])]
rownames(n_b)[order(n_b[[1]])]

nb_names = rownames(n_b)
nb_split = strsplit(rownames(n_b),':')
b_order = sapply(nb_split,function(x) x[2])
b_num = as.numeric(as.factor(b_order))
b_eff = sapply(b_num, function(x) b[[1]][x])
nb_eff = n_b[[1]]+b_eff
nb_ord = order(nb_eff)
nb_eff_ord = data.frame(nb_eff[nb_ord])
rownames(nb_eff_ord) = nb_names[nb_ord]

#contains random effect for price for each neighborhood (added to appropriate burrough effect)
nb_eff_ord


#################
#popularity model
#################

#find overdispersion parameter by fitting model without random effect
overtest = glm.nb(number_of_reviews ~  price + 
                    room_type + calculated_host_listings_count + minimum_nights + name_length + 
                    months_available + last_review + offset(log(number_of_months)), data = df)
overtest$family
#overdispersion estimated as 1.733

mod_pop = glmer(number_of_reviews ~  price + 
                  room_type + calculated_host_listings_count + minimum_nights + name_length + 
                  months_available + last_review + offset(log(number_of_months))+
                  (1|neighbourhood_group/neighbourhood), data = df,family=MASS::negative.binomial(theta=1.733))

deviance(mod_pop)/df.residual(mod_pop) 
#about 1

relgrad <- with(mod_pop@optinfo$derivs,solve(Hessian,gradient))
max(abs(relgrad))
#probably good enough

summary(mod_pop)

ran_est2 = ranef(mod_pop)

n_b2 = ran_est2$`neighbourhood:neighbourhood_group`
b2 = ran_est2$neighbourhood_group
rownames(n_b2)[order(n_b2[[1]])]
rownames(b2)[order(b2[[1]])]

nb_names2 = rownames(n_b2)
nb_split2 = strsplit(rownames(n_b2),':')
b_order2 = sapply(nb_split2,function(x) x[2])
b_num2 = as.numeric(as.factor(b_order2))
b_eff2 = sapply(b_num2, function(x) b2[[1]][x])
nb_eff2 = n_b2[[1]]+b_eff2
nb_ord2 = order(nb_eff2)
nb_eff_ord2 = data.frame(nb_eff2[nb_ord2])
rownames(nb_eff_ord2) = nb_names2[nb_ord2]

#contains random effect for popularity for each neighborhood (added to appropriate burrough effect)
nb_eff_ord2

library(xtable)

#Price Fixed Effects
coef(summary(mod_price))[,-3]
print(xtable(coef(summary(mod_price))[,-3],digits=3))

#Price Random Variance
as.data.frame(VarCorr(mod_price))[,c(1,4,5)]
print(xtable(as.data.frame(VarCorr(mod_price))[,c(1,4,5)],digits=3))


#Popularity Fixed Effects
coef(summary(mod_pop))[,-3]
print(xtable(coef(summary(mod_pop))[,-3],digits=3))

#Popularity Random Variance
as.data.frame(VarCorr(mod_pop))[,c(1,4,5)]
print(xtable(as.data.frame(VarCorr(mod_pop))[,c(1,4,5)],digits=3))


#Most Expensive
print(xtable(as.data.frame(as.matrix(nb_eff_ord)[217:208,])))
#Least Expensive
print(xtable(as.data.frame(as.matrix(nb_eff_ord)[1:10,])))

#Most Popular
print(xtable(as.data.frame(as.matrix(nb_eff_ord2)[217:208,])))
#Least Popular
print(xtable(as.data.frame(as.matrix(nb_eff_ord2)[1:10,])))

cor(nb_eff,nb_eff2)
plot(nb_eff,nb_eff2,xlab='price',ylab='pop')
