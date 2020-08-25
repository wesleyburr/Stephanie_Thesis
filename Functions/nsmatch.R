# LR Validation - P(O|G^C)

nsmatch <- function(N, p, m, r, s){
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
  
  
  offenders <- sample_n(population, N*p, replace = FALSE)  ## offenders is randomly chosen from the population, not just the first p percent. 
  colnames(offenders) <- c("offID", rloci)
  nonoffenders <- population[!(apply(population, 1, toString) %in% apply(offenders, 1, toString)), ] # populuation - offenders
  colnames(nonoffenders) <- c("nonoffID", rloci)
  
  
  ##### now we loop over the rest r times
  n3 <- vector(length = m)
  for (i in 1:m) {
    
    # Generate crime profile ( Chosen from dB)
    suspect <- sample_n(offenders, 1)
    colnames(suspect) <- c("ID", rloci)
    
    dB_B <- dbConnect(RSQLite::SQLite(), ":memory:") ## new connection
    dbWriteTable(dB_B, "offenders", offenders, overwrite = TRUE)
    dbWriteTable(dB_B, "suspect", suspect, overwrite = TRUE)  
    dbWriteTable(dB_B, "nonoffenders", nonoffenders, overwrite = TRUE)
    
    
    matchesindB <- dbGetQuery(dB_B, "SELECT suspect.ID AS CS_ID, offenders.offID AS OFF_ID
           FROM suspect 
           INNER JOIN offenders
           ON suspect.loci1 = offenders.loci1
           AND suspect.loci2 = offenders.loci2
           AND suspect.loci3 = offenders.loci3
           AND suspect.loci4 = offenders.loci4
           AND suspect.loci5 = offenders.loci5
           AND suspect.loci6 = offenders.loci6")
    matchesinpop <- dbGetQuery(dB_B, "SELECT suspect.ID AS CS_ID, nonoffenders.nonoffID AS OFF_ID
           FROM suspect 
           INNER JOIN nonoffenders
           ON suspect.loci1 = nonoffenders.loci1
           AND suspect.loci2 = nonoffenders.loci2
           AND suspect.loci3 = nonoffenders.loci3
           AND suspect.loci4 = nonoffenders.loci4
           AND suspect.loci5 = nonoffenders.loci5
           AND suspect.loci6 = nonoffenders.loci6")
    dbDisconnect(dB_B)
    
    n1 <- ifelse(nrow(matchesindB) == 1, 1, 0)  # is there exactly one match in the offenders database?
    n2 <- ifelse(nrow(matchesinpop) > 0, 1, 0)  # is there at least one match in the nonoffenders database?
    n3[i] <- ifelse(n1+n2 == 2, 1, 0) # are both of these things true? 1 if yes, 0 if no
  }
  return(n3)
}