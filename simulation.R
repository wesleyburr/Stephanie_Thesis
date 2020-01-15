## SIMS FUNCTION
sims <- function(n_pop, Pop_Source = NULL, n_match=13, n_degrade=k) {
  stopifnot(n_pop > 1000, is.numeric(n_pop), is.data.frame(Pop_Source),
            dim(Pop_Source)[2] == 26, !is.null(Pop_Source))
  
  #dtable <- (sample_n(popdt, n_database))
  ## choosing a sample at random from population, this will be the crime scene profile to try to match
  ## degradation : NONE;  ORIG = CS 
  
  orig <- (sample_n(popdt, 1))   # data.table function
  
  ############## DEGRADATION SECTION ####################################################################
  if(k == 0){
    cs_data_sample <- orig;
  } else if (1<=k & k<=12){
    degrade <- sample(orig[, which(names(orig) != "ID")], k)
    cs_data1 <- orig[, -which(names(orig) %in% names(degrade))]
    cs_data2<- data.frame(matrix(NA, nrow = 1, ncol = k))
    colnames(cs_data2) <- c(names(degrade))
    cs_data_sample <- cbind(cs_data1, cs_data2);
  } else {
    cs_data_sample <- ("Not valid entry")
  }
  
  #########################################################################
  
  dB_1 <- dbConnect(RSQLite::SQLite(), ":memory:") ## "" creates temporary on-disk database, will be deleted when connection is closed 
  dbWriteTable(dB_1, "cs_data_sample", orig)
  dbWriteTable(dB_1,"dtable", dtable, overwrite=TRUE)
  
  cs <- dbGetQuery(dB_1, paste0("SELECT * FROM cs_data_sample"))
  dbWriteTable(dB_1, "cs", cs, overwrite=TRUE)
  
  
  CSF1PO.match <- dbGetQuery(dB_1, "SELECT dt.ID, dt.CSF1PO, IFNULL(cs.CSF1PO, dt.CSF1PO) AS csCSF1PO
                             FROM dtable AS dt
                             INNER JOIN cs
                             ON dt.CSF1PO = csCSF1PO")
  
  FGA.match <- dbGetQuery(dB_1, "SELECT dt.ID, dt.FGA, IFNULL(cs.FGA, dt.FGA) AS csFGA
                          FROM dtable AS dt
                          INNER JOIN cs
                          ON dt.FGA = csFGA")
  
  TH01.match <- dbGetQuery(dB_1, "SELECT dt.ID, dt.TH01, IFNULL(cs.TH01, dt.TH01) AS csTH01
                           FROM dtable AS dt
                           INNER JOIN cs
                           ON dt.TH01 = csTH01")
  
  TPOX.match <- dbGetQuery(dB_1, "SELECT dt.ID, dt.TPOX, IFNULL(cs.TPOX, dt.TPOX) AS csTPOX
                           FROM dtable AS dt
                           INNER JOIN cs
                           ON dt.TPOX = csTPOX")
  
  VWA.match <- dbGetQuery(dB_1, "SELECT dt.ID, dt.VWA, IFNULL(cs.VWA, dt.VWA) AS csVWA
                          FROM dtable AS dt
                          INNER JOIN cs
                          ON dt.VWA = csVWA")
  
  D3S1358.match <- dbGetQuery(dB_1, "SELECT dt.ID, dt.D3S1358, IFNULL(cs.D3S1358, dt.D3S1358) AS csD3S1358
                              FROM dtable AS dt
                              INNER JOIN cs
                              ON dt.D3S1358 = csD3S1358")
  
  D5S818.match <- dbGetQuery(dB_1, "SELECT dt.ID, dt.D5S818, IFNULL(cs.D5S818, dt.D5S818) AS csD5S818
                             FROM dtable AS dt
                             INNER JOIN cs
                             ON dt.D5S818 = csD5S818")
  
  D7S820.match <- dbGetQuery(dB_1, "SELECT dt.ID, dt.D7S820, IFNULL(cs.D7S820, dt.D7S820) AS csD7S820
                             FROM dtable AS dt
                             INNER JOIN cs
                             ON dt.D7S820 = csD7S820")
  
  D8S1179.match <- dbGetQuery(dB_1, "SELECT dt.ID, dt.D8S1179, IFNULL(cs.D8S1179, dt.D8S1179) AS csD8S1179
                              FROM dtable AS dt
                              INNER JOIN cs
                              ON dt.D8S1179 = csD8S1179")
  
  D13S317.match <- dbGetQuery(dB_1, "SELECT dt.ID, dt.D13S317, IFNULL(cs.D13S317, dt.D13S317) AS csD13S317
                              FROM dtable AS dt
                              INNER JOIN cs
                              ON dt.D13S317 = csD13S317")
  
  D16S539.match <- dbGetQuery(dB_1, "SELECT dt.ID, dt.D16S539, IFNULL(cs.D16S539, dt.D16S539) AS csD16S539
                              FROM dtable AS dt
                              INNER JOIN cs
                              ON dt.D16S539 = csD16S539")
  
  D18S51.match <- dbGetQuery(dB_1, "SELECT dt.ID, dt.D18S51, IFNULL(cs.D18S51, dt.D18S51) AS csD18S51
                             FROM dtable AS dt
                             INNER JOIN cs
                             ON dt.D18S51 = csD18S51")
  
  D21S11.match <- dbGetQuery(dB_1, "SELECT dt.ID, dt.D21S11, IFNULL(cs.D21S11, dt.D21S11) AS csD21S11
                             FROM dtable AS dt
                             INNER JOIN cs
                             ON dt.D21S11 = csD21S11")
  
  # everytime an ID number appears in this database, it counts as one loci that matches the crime scene sample 
  dbWriteTable(dB_1, "csf1po", CSF1PO.match, overwrite=TRUE)
  dbWriteTable(dB_1, "fga", FGA.match, overwrite=TRUE)
  dbWriteTable(dB_1, "th01", TH01.match, overwrite=TRUE)
  dbWriteTable(dB_1, "tpox", TPOX.match, overwrite=TRUE)
  dbWriteTable(dB_1, "vwa", VWA.match, overwrite=TRUE)
  dbWriteTable(dB_1, "d3s1358", D3S1358.match, overwrite=TRUE)
  dbWriteTable(dB_1, "d5s818", D5S818.match,overwrite=TRUE)
  dbWriteTable(dB_1, "d7s820", D7S820.match, overwrite=TRUE)
  dbWriteTable(dB_1, "d8s1179", D8S1179.match, overwrite=TRUE)
  dbWriteTable(dB_1, "d13s317", D13S317.match, overwrite=TRUE)
  dbWriteTable(dB_1, "d16s539", D16S539.match, overwrite=TRUE)
  dbWriteTable(dB_1, "d18s51", D18S51.match, overwrite=TRUE)
  dbWriteTable(dB_1, "d21s11", D21S11.match, overwrite=TRUE)
  
  largedb <- dbGetQuery(dB_1, "
                        SELECT csf1po.ID FROM csf1po
                        UNION ALL
                        SELECT fga.ID FROM fga
                        UNION ALL 
                        SELECT th01.ID FROM th01
                        UNION ALL
                        SELECT tpox.ID FROM tpox
                        UNION ALL
                        SELECT vwa.ID FROM vwa
                        UNION ALL 
                        SELECT d3s1358.ID FROM d3s1358
                        UNION ALL
                        SELECT d5s818.ID FROM d5s818
                        UNION ALL 
                        SELECT d7s820.ID FROM d7s820
                        UNION ALL
                        SELECT d8s1179.ID FROM d8s1179
                        UNION ALL 
                        SELECT d13s317.ID FROM d13s317
                        UNION ALL 
                        SELECT d16s539.ID FROM d16s539
                        UNION ALL
                        SELECT d18s51.ID FROM d18s51
                        UNION ALL
                        SELECT d21s11.ID FROM d21s11")

  # function to find the probabilities of each "event" occuring
  dbWriteTable(dB_1, "tab", largedb, overwrite=TRUE)
  ma <- dbGetQuery(dB_1, paste("SELECT ID, COUNT(*) AS MatchedAlleles
                               FROM tab
                               GROUP BY ID
                               HAVING MatchedAlleles >=(", n_match,")"))
  dbDisconnect(dB_1)

    if(nrow(ma)==0){
        prob <- 0
    }else{
        prob <- 1
    }
  return(prob)
}

