# Manual / brute force method for finding partial matches in a DB - very particlar. 

partjoin <- function(DB1, DB2, nmatch){
  loci <- c("CSF1PO","FGA", "TH01", "TPOX", "VWA", "D3S1358",
            "D5S818", "D7S820", "D8S1179", "D13S317",
            "D16S539", "D18S51", "D21S11")
  
  
  colnames(DB1) <- c("offID", loci) 
  colnames(DB2) <- c("nonoffID", loci)
  
  dB_1 <- dbConnect(RSQLite::SQLite(), ":memory:")
  dbWriteTable(dB_1,"DB1", DB1, overwrite=TRUE)
  dbWriteTable(dB_1, "DB2", DB2, overwrite = TRUE)
  
  CSF1PO <- dbGetQuery(dB_1, "SELECT DB1.offID, DB2.nonoffID
                       FROM DB1
                       INNER JOIN DB2
                       ON DB1.CSF1PO = DB2.CSF1PO")
  
  FGA <- dbGetQuery(dB_1, "SELECT DB1.offID, DB2.nonoffID
                    FROM DB1
                    INNER JOIN DB2
                    ON DB1.FGA = DB2.FGA")
  
  TH01 <- dbGetQuery(dB_1, "SELECT DB1.offID, DB2.nonoffID
                     FROM DB1
                     INNER JOIN DB2
                     ON DB1.TH01 = DB2.TH01")
  
  TH01 <- dbGetQuery(dB_1, "SELECT DB1.offID, DB2.nonoffID
                     FROM DB1
                     INNER JOIN DB2
                     ON DB1.TH01 = DB2.TH01")
  
  TPOX <- dbGetQuery(dB_1, "SELECT DB1.offID, DB2.nonoffID
                     FROM DB1
                     INNER JOIN DB2
                     ON DB1.TPOX = DB2.TPOX")
  
  VWA <- dbGetQuery(dB_1, "SELECT DB1.offID, DB2.nonoffID
                    FROM DB1
                    INNER JOIN DB2
                    ON DB1.VWA = DB2.VWA")
  
  D3S1358 <- dbGetQuery(dB_1, "SELECT DB1.offID, DB2.nonoffID
                        FROM DB1
                        INNER JOIN DB2
                        ON DB1.D3S1358 = DB2.D3S1358")
  
  D5S818 <- dbGetQuery(dB_1, "SELECT DB1.offID, DB2.nonoffID
                       FROM DB1
                       INNER JOIN DB2
                       ON DB1.D5S818 = DB2.D5S818")
  
  D7S820 <- dbGetQuery(dB_1, "SELECT DB1.offID, DB2.nonoffID
                       FROM DB1
                       INNER JOIN DB2
                       ON DB1.D7S820 = DB2.D7S820")
  
  D8S1179 <- dbGetQuery(dB_1, "SELECT DB1.offID, DB2.nonoffID
                        FROM DB1
                        INNER JOIN DB2
                        ON DB1.D8S1179 = DB2.D8S1179")
  
  D13S317 <- dbGetQuery(dB_1, "SELECT DB1.offID, DB2.nonoffID
                        FROM DB1
                        INNER JOIN DB2
                        ON DB1.D13S317 = DB2.D13S317")
  
  D16S539 <- dbGetQuery(dB_1, "SELECT DB1.offID, DB2.nonoffID
                        FROM DB1
                        INNER JOIN DB2
                        ON DB1.D16S539 = DB2.D16S539")
  
  D18S51 <- dbGetQuery(dB_1, "SELECT DB1.offID, DB2.nonoffID
                       FROM DB1
                       INNER JOIN DB2
                       ON DB1.D18S51 = DB2.D18S51")
  
  D21S11 <- dbGetQuery(dB_1, "SELECT DB1.offID, DB2.nonoffID
                       FROM DB1
                       INNER JOIN DB2
                       ON DB1.D21S11 = DB2.D21S11")
  
  
  dbWriteTable(dB_1, "CSF1PO", CSF1PO, overwrite = TRUE)
  dbWriteTable(dB_1, "FGA", FGA, overwrite = TRUE)
  dbWriteTable(dB_1, "TH01", TH01, overwrite = TRUE)
  dbWriteTable(dB_1, "TPOX", TPOX, overwrite = TRUE)
  dbWriteTable(dB_1, "VWA", VWA, overwrite = TRUE)
  dbWriteTable(dB_1, "D3S1358", D3S1358, overwrite = TRUE)
  dbWriteTable(dB_1, "D5S818", D5S818, overwrite = TRUE)
  dbWriteTable(dB_1, "D7S820", D7S820, overwrite = TRUE)
  dbWriteTable(dB_1, "D8S1179", D8S1179, overwrite = TRUE)
  dbWriteTable(dB_1, "D13S317", D13S317, overwrite = TRUE)
  dbWriteTable(dB_1, "D16S539", D16S539, overwrite = TRUE)
  dbWriteTable(dB_1, "D18S51", D18S51, overwrite = TRUE)
  dbWriteTable(dB_1, "D21S11", D21S11, overwrite = TRUE)
  
  largedb <- dbGetQuery(dB_1, "SELECT * FROM CSF1PO
                        UNION ALL 
                        SELECT * FROM FGA
                        UNION ALL
                        SELECT * FROM TH01
                        UNION ALL
                        SELECT * FROM TPOX
                        UNION ALL
                        SELECT * FROM VWA
                        UNION ALL
                        SELECT * FROM D3S1358
                        UNION ALL
                        SELECT * FROM D5S818
                        UNION ALL
                        SELECT * FROM D7S820
                        UNION ALL
                        SELECT * FROM D8S1179
                        UNION ALL
                        SELECT * FROM D13S317
                        UNION ALL
                        SELECT * FROM D16S539
                        UNION ALL
                        SELECT * FROM D18S51
                        UNION ALL
                        SELECT * FROM D21S11")
  
  
  dbWriteTable(dB_1, "largedb", largedb, overwrite = TRUE)        
  ma <- dbGetQuery(dB_1, paste("SELECT offID, nonoffID, COUNT(*) as Matches
                             FROM largedb
                             WHERE offID != nonoffID
                             GROUP BY offID, nonoffID
                             HAVING Matches =(", nmatch,")
                             ORDER BY Matches DESC"))
  
  return(nrow(ma))
}

  