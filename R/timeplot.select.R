#' To systematically select id in a longitudinal dataset based on residuals from fitted smooth
#'
#' @param formula formula to supply to smoother (y ~ x)
#' @param id character specify name of id variable
#' @param data longitudinal data
#' @param smoother type of smooth
#' @param select quantile to choose
#' @param df degree of freedom if use spline as smoother (default = 4)
#' @param ... other arguments to supply to loess
#'
#' @return data frame contains id, max x, y at max x, and labels
#' @export
timeplot.select <- function(formula, id, data, smoother = c("loess", "spline"),
                            select=c(0, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 1),
                            df = 4, ...){
  # fit smoother
  if (length(smoother) > 1) smoother <- "loess"
  fit <- switch(smoother,
                "loess" = loess(formula, data = data, ...),
                "spline" = lm(paste(formula[2], formula[1], "splines::ns(", formula[3], ", df =", df, ")", sep = ""),
                              data = data))

  # get the residuals
  tmp <- data.frame(id = data[, id],
                    y = data[, as.character(formula[[2]])],
                    x = data[, as.character(formula[[3]])],
                    pred = predict(fit, newdata = data))
  names(tmp) <- c("id", "y", "x", "pred")
  tmp2 <- dplyr::summarise(dplyr::group_by(dplyr::mutate(tmp,
                                                         resid = y - pred),
                                           id),
                           resid = median(resid, na.rm = TRUE))

  # choose values
  crit <- quantile(abs(tmp2$resid), probs = select)
  names(crit) <- paste(select * 100, "%", sep = "")
  names(crit)[names(crit) == "0%"] <- "Min"
  names(crit)[names(crit) == "100%"] <- "Max"
  index <- unlist(lapply(crit, function(x){
    which(abs(tmp2$resid - x) == min(abs(tmp2$resid - x)))[1]
  }))
  out_id <- tmp2$id[index]
  out <- as.data.frame(do.call("rbind", lapply(out_id, function(z){
    out_x <- with(tmp, max(x[id == z & !is.na(y)]))
    out_y <- with(tmp, y[id == z & !is.na(y) & x == out_x])
    cbind(id = z, x = out_x, y = out_y)
  })))
  out$label <- names(crit)
  return(out)
}

#' @export
timeplot.notrend <- function(formula, data, smoother = c("loess", "spline"), df = 4, ...){
  # fit smoother
  if (length(smoother) > 1) smoother <- "loess"
  fit <- switch(smoother,
                "loess" = loess(formula, data = data, ...),
                "spline" = lm(paste(formula[2], formula[1], "splines::ns(", formula[3], ", df =", df, ")", sep = ""),
                              data = data))

  # get the residuals
  out <- data[, as.character(formula[[2]])] - predict(fit, newdata = data)

  # to remove unneccessary information
  if (is.null(dim(out))) {
    return(out)
  } else {
    return(subset(out)[, 1])
  }
}
