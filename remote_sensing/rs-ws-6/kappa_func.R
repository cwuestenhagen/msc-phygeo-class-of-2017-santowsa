#the comKappa function is taken from TNauss since package ctab wasn't as easy 
#as excpected to understand and use surely in the script. 
#the matrices are taken from the step otb trainimageclassifier (rf)

compKappa <- function(ctable){
  ctable <- ctable/sum(ctable)
  categories <- nrow(ctable)
  
  # Fraction of agreement
  pagrm <- 0
  for(i in seq(categories)){
    pagrm <- pagrm + ctable[i,i]
  }
  
  # Expected fraction of agreement subject to the observed distribution
  pexpct <- 0
  for(i in seq(categories)){
    pexpct <- pexpct + sum(ctable[i,]) * sum(ctable[,i])
  }
  
  # Kappa index
  kappa <- (pagrm - pexpct)/(1 - pexpct)
  
  return(kappa)
}
