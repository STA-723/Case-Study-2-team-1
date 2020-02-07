#price brms
library(brms)

df$lprice = log(df$price)
df$burr = as.numeric(df$neighbourhood_group)
df$neigh = as.numeric(df$neighbourhood)
df$last_review  = df$last_review_yr - 2011

mod1 = brm(lprice ~  reviews_per_month + 
            room_type + calculated_host_listings_count + minimum_nights +
            name_length + availability_365 + last_review +(1|burr/neigh), data = df,
            warmup=100,iter = 1100, chains = 2, seed = 1)
save(mod1,file='brms_mod1.Rdata')




library(lme4)
mod_price = lmer(lprice ~  reviews_per_month + 
            room_type + calculated_host_listings_count + minimum_nights + name_length + 
            availability_365 + last_review +(1|neighbourhood_group/neighbourhood), data = df)

relgrad <- with(mod_price@optinfo$derivs,solve(Hessian,gradient))
max(abs(relgrad))
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
nb_eff_ord

overtest = glm.nb(number_of_reviews ~  price + 
                    room_type + calculated_host_listings_count + minimum_nights + name_length + 
                    availability_365 + last_review + offset(log(number_of_months)), data = df)
#overdispersion estimated as 1.733

mod_pop = glmer(number_of_reviews ~  price + 
                   room_type + calculated_host_listings_count + minimum_nights + name_length + 
                   availability_365 + last_review + offset(log(number_of_months))+
                   (1|neighbourhood_group/neighbourhood), data = df,family=MASS::negative.binomial(theta=1.733))

deviance(mod_pop)/df.residual(mod_pop)

relgrad <- with(mod_pop@optinfo$derivs,solve(Hessian,gradient))
max(abs(relgrad))

#mod_pop1 = glmer(number_of_reviews ~  price + 
#              room_type + calculated_host_listings_count + minimum_nights + name_length + 
#              availability_365 + last_review + offset(log(number_of_months))+
#              (1|neighbourhood_group/neighbourhood), data = df,family=MASS::negative.binomial(theta=1))

#deviance(mod_pop1)/df.residual(mod_pop1)

#mod_pop2 = glmer(number_of_reviews ~  price + 
#                  room_type + calculated_host_listings_count + minimum_nights + name_length + 
#                  availability_365 + last_review + offset(log(number_of_months))+
#                  (1|neighbourhood_group/neighbourhood), data = df,family=MASS::negative.binomial(theta=2))

#deviance(mod_pop2)/df.residual(mod_pop2)


#mod_pop3 = glmer(number_of_reviews ~  price + 
#                   room_type + calculated_host_listings_count + minimum_nights + name_length + 
#                   availability_365 + last_review + offset(log(number_of_months))+
#                   (1|neighbourhood_group/neighbourhood), data = df,family=MASS::negative.binomial(theta=1.75))

#deviance(mod_pop3)/df.residual(mod_pop3)

summary(mod_pop)

ran_est2 = ranef(mod_pop)

n_b2 = ran_est_pop$`neighbourhood:neighbourhood_group`
b2 = ran_est_pop$neighbourhood_group
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
nb_eff_ord2
