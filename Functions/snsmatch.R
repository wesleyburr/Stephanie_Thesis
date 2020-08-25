# LR Validation - P(A|G^C)

snsmatch <- function(N, p, m, r, s){
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
  
  # 2. Generate dB
  offenders <- sample_n(population, N*p, replace = FALSE)  ## offenders is randomly chosen from the population
  colnames(offenders) <- c("offID", rloci)
  nonoffenders <- population[!(apply(population, 1, toString) %in% apply(offenders, 1, toString)), ] # populuation -             offenders
  colnames(nonoffenders) <- c("nonoffID", rloci)
  
  ##### now we loop over the rest m times
  n <- vector(length = m)
  for (i in 1:m) {
    
    # Generate crime profile ( Chosen from dB)
    suspect <- sample_n(offenders, 1)
    
    # Now match offenders dB with suspect. How many times do we see exactly one match 
    dB_B <- dbConnect(RSQLite::SQLite(), ":memory:") ## different connection than what I was using last time (db_1)
    dbWriteTable(dB_B, "suspect", suspect, overwrite = TRUE)  
    dbWriteTable(dB_B, "nonoffenders", nonoffenders, overwrite = TRUE)
    
    matchesinpop <- dbGetQuery(dB_B, "SELECT suspect.offID AS CS_ID, nonoffenders.nonoffID AS OFF_ID
           FROM suspect 
           INNER JOIN nonoffenders
           ON suspect.loci1 = nonoffenders.loci1
           AND suspect.loci2 = nonoffenders.loci2
           AND suspect.loci3 = nonoffenders.loci3
           AND suspect.loci4 = nonoffenders.loci4
           AND suspect.loci5 = nonoffenders.loci5
           AND suspect.loci6 = nonoffenders.loci6")
    dbDisconnect(dB_B)
    
    n[i] <- ifelse(nrow(matchesinpop) > 0, 1, 0)  # is there at least one match in the nonoffenders database?
  }
  return(n)
}