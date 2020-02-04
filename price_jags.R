#attempt jags model for price
pricemodel = function(){
  for(i in 1:N){
    y[i] ~ dnorm(mu[i],ty)
    mu[i] = inprod(X[i,],beta[]) + beta_n[which_n[i]] + inprod(K[i,],beta_s[])
  } 
  
  ## Hierarchy
  for(i in 1:p_n){
    beta_n[i] ~ dnorm(beta_b[burrow_loc[i]],tn)
  }
  for(i in 1:p_b){
    beta_b[i] ~ dnorm(ybar,tb)
  }
  for(i in 1:p_k){
    beta_s[i] ~ dnorm(0,ts)
  }
  for(i in 1:p){
    beta[i] ~ dnorm(0,tx)
  }
  #variance priors
  
  tn ~ dgamma(2,1)
  s2n = 1/tn
  tb ~ dgamma(2,1)
  s2b = 1/tb
  tx ~ dgamma(2,1)
  s2x = 1/tx
  ts ~ dgamma(2,1)
  s2s = 1/ts
  ty ~ dgamma(2,1)
  s2y = 1/ty
  
}

nknots = dim(knots2)[1]
p = dim(des_mat)[2] - 2 #drop price and intercept
p_neighbs = ncol(X_neighbs)
p_burr = ncol(X_burrs)

which_n = apply(X_neighbs,1,function(x) which(x == 1))


jagsdata = list(X=des_mat[,-(1:2)],
                which_n = which_n,
                K = as.matrix(K),
                y=c(log(df$price)),
                N=nrow(df),
                p = p,
                p_k = nknots,
                p_b = p_burr,
                p_n = p_neighbs,
                burrow_loc = burrow_loc,
                ybar = mean(log(df$price))
)

priceout2 = jags(data=jagsdata,model=pricemodel, inits=NULL,    
                parameters.to.save=c("beta","beta_n","beta_b","beta_s","s2n","s2b","s2x","s2s","s2y"), 
                n.iter=2100,n.burnin = 100,n.thin = 1,n.chains=2)

save(priceout2, file="price_jags_output2.Rdata")
priceout2

