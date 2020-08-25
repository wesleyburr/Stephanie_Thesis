# Creates a population of size N (r loci, s alleles - equal probs)
# p% is offenders database, remaining (1-p)% is non offendres
# finds full profile matches between the offenders and non offenders in proportion to the size of the population - N 


collisions <- function(N,p,r,s){
  
  # naming r loci: loci1, loci2, loci3, .... , locir  
  rloci <- vector(length = r)
  
  for(i in 1:r)    {
    
    rloci[i] <- paste("loci", i, sep="", collapse="")
    
  } 
  
  
  #creating pop
  population <- data.frame(matrix(nrow=N, ncol= r+1))
  colnames(population) <- c("ID", rloci)
  population[, 1] <- 1:N
  
  ## generating N profiles and filling in population database. each loci has 10 alleles
  for(j in 1:r) {
    allele <- sample(1:s, N, replace = TRUE)
    tmp_df <- data.frame(A1 = as.integer(sample(allele, N, 
                                                replace = TRUE)),
                         A2 = as.integer(sample(allele, N, 
                                                replace = TRUE))
    )
    population[, j+1] <- data.frame(as.integer(sample(allele, N, 
                                                      replace = TRUE)))
  }
  
  
  
  offenders <- sample_n(population, N*p, replace = FALSE)  ## offenders is randomly chosen from the population, not just the first p percent. 
  colnames(offenders) <- c("off ID", rloci)
  nonoffenders <- population[!(apply(population, 1, toString) %in% apply(offenders, 1, toString)), ] # populuation - offenders
  colnames(nonoffenders) <- c("non-off ID", rloci)
  
  # checking for duplicates between offenders and non-offenders
  false.matches <- inner_join(offenders, nonoffenders)
  
  ## probability = # of false detects / N
  return(nrow(false.matches)/N)
}

