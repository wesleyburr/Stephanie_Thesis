# chpt 5.1.2 collisions & scaling 

DBcollision <- function(N,p,r,s,nmatch){
  stopifnot(N>100, r >= nmatch)
  
  
  # naming r loci: loci1, loci2, loci3, .... , locir  
  rloci <- vector(length = r)
  
  for(i in 1:r)    {
    rloci[i] <- paste("loci", i, sep="", collapse="")
  } 
  
  
  #creating pop
  population <- data.frame(matrix(nrow=N, ncol= r+1))
  colnames(population) <- c("ID", rloci)
  population[, 1] <- 1:N
  
  by2 <- rbeta(s, shape1 = 4, shape2 = 6)
  by <- by2/sum(by2)
  
  ## generating N profiles and filling in population database. each loci has 10 alleles
  for(j in 1:r) {
    allele <- sample(1:s, N, replace = TRUE, prob = by)
    tmp_df <- data.frame(A1 = as.integer(sample(allele, N, 
                                                replace = TRUE)),
                         A2 = as.integer(sample(allele, N, 
                                                replace = TRUE))
    )
    population[, j+1] <- data.frame(as.integer(sample(allele, N, 
                                                      replace = TRUE)))
  }
  
  
  offenders <- sample_n(population, N*p)
  
  DB1 <- offenders
  DB2 <- offenders
  
  colnames(DB1) <- c("DB1Id", rloci)
  colnames(DB2) <- c("DB2Id", rloci)
  
  n_loci <- r
  loci_match <- vector(mode= "list", length = n_loci)
  for(j in 1:n_loci) {
    loci_match[[j]] <- inner_join(DB1, DB2, by = rloci[j])[, c(1, r+2)]
  }
  combinedmatch <- do.call("rbind", loci_match)
  
  dB_1 <- dbConnect(RSQLite::SQLite(), ":memory:") ## "" 
  dbWriteTable(dB_1, "combinedmatch", combinedmatch, overwrite = TRUE)        
  ma <- dbGetQuery(dB_1, paste("SELECT DB1Id, DB2Id, COUNT(*) as Matches
                             FROM combinedmatch
                             WHERE DB1Id != DB2Id
                             GROUP BY DB1Id, DB2Id
                             HAVING Matches =(", nmatch,")
                             ORDER BY Matches DESC"))
  
  return(nrow(ma))
}