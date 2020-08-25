# LR Validation - P(O|G)

smatch <- function(N, p, m, r,s){
  # Generate a population size N
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
  
  #### now we loop over the rest r times to reuse population
  log.vec <- vector(length = m)
  for (i in 1:m) {
    
    offenders <- sample_n(population, N*p, replace = FALSE)  ## offenders is randomly chosen from the population
    
    # Generate crime profile (Chosen from dB)
    suspect <- sample_n(offenders, 1)
    
    # Now match offenders dB with suspect. How many times do we see exactly one match 
    dB_B <- dbConnect(RSQLite::SQLite(), ":memory:") ## new connection 
    dbWriteTable(dB_B, "offenders", offenders, overwrite = TRUE)
    dbWriteTable(dB_B, "suspect", suspect, overwrite = TRUE)
    
    matches <- dbGetQuery(dB_B, "SELECT suspect.ID AS CS_ID, offenders.ID AS OFF_ID
           FROM suspect 
           INNER JOIN offenders
           ON suspect.loci1 = offenders.loci1
           AND suspect.loci2 = offenders.loci2
           AND suspect.loci3 = offenders.loci3
           AND suspect.loci4 = offenders.loci4
           AND suspect.loci5 = offenders.loci5
           AND suspect.loci6 = offenders.loci6")
    dbDisconnect(dB_B)
    
    log.vec[i] <- nrow(matches) 
  }
  return(log.vec)
}