#' @export
inspect.data <- function(data, info, id, check_missing, plot = FALSE, prefix = "", outdir){
  if (missing(outdir)) outdir <- "."

  # select variables to check
  data <- data[,names(data)[names(data) %in% info$varname]]
  info <- info[match(names(data), info$varname),]

  # check missing
  if (missing(check_missing)) {
    info$check_missing <- TRUE
  } else {
    info$check_missing <- check_missing
  }

  ## check data
  output <- do.call("rbind", mapply(inspect.each,
                                    x = as.list(data),
                                    varname = info$varname,
                                    value = info$value,
                                    type = info$type,
                                    check_missing = info$check_missing))

  ## add id: use "=""...""" to avoid Excel to interpret string as Date
  if (!missing(id) & !is.null(output)) {
    output[, id] <- paste('"=""', as.character(data[output$index, id]), '"""', sep = "")
    output <- output[, c(id, "index", "error")]
  }

  ## plot distribution of data
  if (plot) {
    pdf(file = file.path(outdir, paste(prefix, "distribution.pdf", sep = "_")), width = 12, height = 10, family = "Helvetica", fonts = NULL, paper = "a4r")
    layout(matrix(c(1:12), ncol = 4, nrow = 3, byrow = TRUE), respect = TRUE)
    for (i in (1:length(info$varname))) {
      if (length(na.omit(data[, info$varname[i]])) == 0) {
        plot(x = 1:10, y = 1:10, main = info$varname[i], type = "n", xlab = "", ylab = "")
        text(x = 5, y = 5, labels = "No non-missing value")
      } else {
        if (mode(data[, info$varname[i]]) != "numeric" | is.factor(data[, info$varname[i]])) {
          barplot(table(data[, info$varname[i]]), main = info$varname[i])

        } else {
          if (is.POSIXct(data[, info$varname[i]])) {
            boxplot(as.Date(data[, info$varname[i]]), main = info$varname[i])
          } else {
            boxplot(data[, info$varname[i]], main = info$varname[i])
          }
        }
      }
    }
    dev.off()
  }

  ## write output
  if (!is.null(output)) {
    write.csv(output, file = file.path(outdir, paste(prefix, "error.csv", sep = "_")), quote = FALSE, row.names = FALSE)
    return(output)
  } else {
    cat("No error was found !")
  }
}