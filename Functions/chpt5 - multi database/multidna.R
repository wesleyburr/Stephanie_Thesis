multidna <- function(N,r,s,ndegrade,nmatch){
  # naming r loci: loci1, loci2, loci3, .... , locir  
  rloci <- vector(length = r)
  
  for(i in 1:r)    {
    rloci[i] <- paste("locus", i, sep="", collapse="")
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
  
  offenders <- sample_n(population, N)
  colnames(offenders) <- c("offID", rloci)
  ## sampling for the crime scene profile
  orig <- sample_n(population, 1)
  
  ############## DEGRADATION SECTION ####################################################################
  k <- ndegrade
  if(k == 0){
    cs_data_sample <- orig
    colnames(cs_data_sample)<- c("ID", rloci);
  } else if (1<=k & k<=r-1){
    degrade <- sample(orig[, which(names(orig) != "ID")], k)
    cs_data1 <- orig[, -which(names(orig) %in% names(degrade))]
    cs_data2<- data.frame(matrix(NA, nrow = 1, ncol = k))
    colnames(cs_data2) <- c(names(degrade))
    cs_data_sample <- cbind(cs_data1, cs_data2);
  } else {
    cs_data_sample <- ("Not valid entry")
  }
  
  #################################################################################################
  
  

  n_loci <- r
  loci_match <- vector(mode= "list", length = n_loci)
  for(j in 1:n_loci) {
    loci_match[[j]] <- inner_join(cs_data_sample, offenders, by = rloci[j])[, c(1, r+2)]
  }
  combinedmatch <- do.call("rbind", loci_match)
  ## k are free matches, so we wanna know how many match @ nmatch - ndegrade, bc non match at the ones that are degraded
  dB_1 <- dbConnect(RSQLite::SQLite(), ":memory:") ## "" 
  dbWriteTable(dB_1, "combinedmatch", combinedmatch, overwrite = TRUE)        
  ma <- dbGetQuery(dB_1, paste("SELECT offID, ID, COUNT(*)
                             FROM combinedmatch
                             GROUP BY offID
                             HAVING COUNT(*) =(", nmatch-k,")"))
  
  return(ma)
}