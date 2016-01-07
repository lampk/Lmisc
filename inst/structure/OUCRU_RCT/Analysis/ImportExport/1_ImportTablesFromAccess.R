##******************************************************************************
## Import data from Access database
## Note: ONLY WORK WITH R 32bit
## Initial version: ...
##******************************************************************************

##******************************************************************************
# set up  ----------------------------------------------------------------------
##******************************************************************************

## load RODBC
library(RODBC)

## function to import Access data into R & save as an R object (only work with R 32bit)
import.Access <- function(input, output) {
  ### open channel
  channel <- odbcConnectAccess(input)
  ### get table'name
  table_name <- subset(sqlTables(channel), TABLE_TYPE == "TABLE")$TABLE_NAME
  ### import
  for (i in table_name) assign(i, sqlFetch(channel, i, as.is = TRUE))
  ### close channel
  odbcClose(channel)
  ### save as an R object
  out <- c(table_name, "table_name")
  save(list = out, file = output)
}

##******************************************************************************
# import & save data ------------------------------------------------------
##******************************************************************************

import.Access(input = file.path("..", "..", "Data", "Raw", "xxx.mdb"),
              output = file.path("..", "..", "Data", "Imported", "xxx.Rdata"))


