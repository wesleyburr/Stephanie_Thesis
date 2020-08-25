# Advance simulation outline ( chpt 5.1)
# chapter 5.1.1 Intersections and scaling

sccollision <- function(N,p,r,s,nmatch){
  stopifnot(N > 100, r >= nmatch)
  
  
  # Naming r loci: loci1, loci2, loci3, .... , locir  
  rloci <- vector(length = r)
  for(i in 1:r)    {
    rloci[i] <- paste("loci", i, sep="", collapse="")
  } 
  
  
  # Creating population of size N 
  population <- data.frame(matrix(nrow=N, ncol= r+1))
  colnames(population) <- c("ID", rloci)
  population[, 1] <- 1:N #Identification numbers
  
  
  by2 <- rbeta(s, shape1 = 4, shape2 = 6) # Beta probability distribution
  by <- by2/sum(by2) # Normalize
  
  ## Generate N profiles to complete the profiles in the population
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
  
  # Sample p% to be offenders
  offenders <- sample_n(population, N*p)
  
  ## Finding the non-offenders (population\offenders)
  dB_1 <- dbConnect(RSQLite::SQLite(), ":memory:") # First need to have a connection to RSQLite
  dbWriteTable(dB_1, "population", population, overwrite = TRUE) 
  dbWriteTable(dB_1,"offenders", offenders, overwrite=TRUE)
  nonoffenders <- dbGetQuery(dB_1, "SELECT *
                    FROM population 
                    WHERE ID NOT IN
                              (SELECT ID
                              FROM offenders)")
  
  colnames(offenders) <- c("offId", rloci) # Rename identification columns so they are distinct
  colnames(nonoffenders) <- c("nonoffId", rloci)
  
  # inner_join() at each loci, keep identification from both offenders and non-offenders. 
  # Rbind() resulting dataframes together to create one large dataframe "combined_match"
  n_loci <- r
  loci_match <- vector(mode= "list", length = n_loci)
  for(j in 1:n_loci) {
    loci_match[[j]] <- inner_join(offenders, nonoffenders, by = rloci[j])[, c(1, r+2)]
  }
  combined_match <- do.call("rbind", loci_match)
  
  # Count how many times each pair of identification numbers is in combined_match, each 
  # count means one match at a locus. We search for "nmatch" matches
  dbWriteTable(dB_1, "combined_match", combined_match, overwrite = TRUE)        
  ma <- dbGetQuery(dB_1, paste("SELECT offID, nonoffID, COUNT(*) as Matches
                             FROM combined_match
                             WHERE offID != nonoffID
                             GROUP BY offID, nonoffID
                             HAVING Matches =(", nmatch,")
                             ORDER BY Matches DESC"))
  
  # Function returns how many profiles were found to match at nmatch loci
  return(nrow(ma))
}