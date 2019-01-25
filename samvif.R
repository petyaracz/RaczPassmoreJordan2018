samvif <- function(mod){
  # mod is an mgcv object
  # this function calculates the variance inflation factors for GAM as no one else has written code to do it properly
  
  # this is used to summarise how well the GAM performed
  
  mod.sum <- summary(mod)
  s2 <- mod$sig2 # estimate of standard deviation of residuals
  # X <- mod$model # data used to fit the model
  X <- model.matrix(mod)
  n <- nrow(X) # how many observations were used in fitting?
  v <- -1 # omit the intercept term, it can't inflate variance
  varbeta <- mod.sum$p.table[v,2]^2 # variance in estimates
  varXj <- apply(X=X[,row.names(mod.sum$p.table)[v]],MARGIN=2, var) # variance of all the explanatory variables
  VIF <- varbeta/(s2/(n-1)*1/varXj) # the variance inflation factor, obtained by rearranging
  # var(beta_j) = s^2/(n-1) * 1/var(X_j) * VIF_j
  
  VIF.df <- data.frame(variable=names(VIF),
                       vif=VIF, 
                       row.names=NULL)
                       
  return(VIF.df)
}

