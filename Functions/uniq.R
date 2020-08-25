# Finding the number of duplicate profiles in a database of size N
# r loci, s allleles (equal probs)
# used for validation 

uniq <- function(N,r,s){
  # naming r loci: loci1, loci2, loci3, .... , locir  
  rloci <- vector(length = r)
  
  for(i in 1:r)    {
    
    rloci[i] <- paste("loci", i, sep="", collapse="")
    
  } 
  
  
  
  #creating pop
  population <- data.frame(matrix(nrow=N, ncol= r+1))
  colnames(population) <- c("ID", rloci)
  population[, 1] <- 1:N
  
  ## generating N profiles and filling in population database. each loci has s alleles
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
  
  population.dups <- (population[duplicated(population[, -1]), ]) # -1 bc obviously ID isnt going to match
  num.population.dups <- (dim(population.dups)[1])   # dim returns [1] the number of rows and [2] the number of cols. we care about rows. 
  return(num.population.dups)
}