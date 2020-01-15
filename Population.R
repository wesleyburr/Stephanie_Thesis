
 ## POPULATION FUNCTION
popfun <- function(n_pop){
  popdt <- data.frame(ID = 1:n_pop, CSF1PO = NA, FGA = NA, TH01 = NA, 
                      TPOX = NA, VWA = NA, D3S1358 = NA,
                      D5S818 = NA, D7S820 = NA, D8S1179 = NA,
                      D13S317 = NA, D16S539 = NA, D18S51 = NA,
                      D21S11 = NA)
  
  ## generating n_pop profiles 
  for(j in 1:13) {
    allele_j <- Pop_Source[, (2*j-1):(2*j)]
    allele_j <- allele_j[1:max(which(!is.na(allele_j[, 2]))), ]
    tmp_df <- data.frame(A1 = as.character( sample(allele_j[[1]], n_pop, 
                                                   replace = TRUE, 
                                                   prob = allele_j[[2]]),
                                            digits = 1 ),
                         A2 = as.character( sample(allele_j[[1]], n_pop, 
                                                   replace = TRUE, 
                                                   prob = allele_j[[2]]),
                                            digits = 1 )
    )
    popdt[, j+1] <- apply(tmp_df, MAR = 1, FUN = function(x) { 
      paste(x, collapse = ", ") 
    }) 
  }
  return(popdt)
}





