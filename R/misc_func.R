
# function to convert a long data frame with derived variables into mids object
# source: http://stackoverflow.com/questions/26667162/perform-operation-on-each-imputed-dataset-in-rs-mice, mice
as.mids2 <- function(data2, .imp = 1, .id = 2, .idx){
  m <- max(as.numeric(levels(data2[,  .imp])))
  dat <- data2[data2[, .imp] == 0, -c(.imp, .id)]

  ## run mice
  ini <- mice::mice(dat, m = m, maxit=0)
  names  <- names(ini$imp)
  if (!is.null(.id)){
    rownames(ini$data) <- data2[data2[, .imp] == 0, .id]
  }

  ## create data frame to store imputed values for additional variables
  ini$imp[[names[.idx]]] <- as.data.frame(matrix(ncol = m, nrow = sum(is.na(dat[, names[.idx]]))))

  ## "correct" imputed values
  for (i in 1:length(names)){
    for(j in 1:m){
      if(!is.null(ini$imp[[i]])){
        indic <- data2[, .imp] == j &  is.na(data2[data2[, .imp]==0, names[i]])
        ini$imp[[names[i]]][j] <- data2[indic, names[i]]
      }
    }
  }

  ## output
  return(ini)
}