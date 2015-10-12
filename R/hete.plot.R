#' To produce heterogeneity plot
#'
#' @param formula outcome ~ independent variable
#' @param data required data
#' @param value value of independent variable
#'
#' @return graph
#' @export
#'
hete.plot <- function(formula, data, value = NULL) {
  ## get text
  tmptext <- hete.est(formula, data, value = NULL)
  ## plot
  rmeta::forestplot(labeltext = tmptext[, 1:4],
                    mean = c(NA, as.numeric(tmptext[-1, 4])),
                    lower = c(NA, as.numeric(tmptext[-1, 6])),
                    upper = c(NA, as.numeric(tmptext[-1, 7])),
                    zero = 0,
                    is.summary = c(TRUE, rep(FALSE, nrow(tmptext) - 1)))
}

hete.est <- function(formula, data, value = NULL) {
  # check formula
  if (length(formula) != 3) {stop("Please check formula !!!")}
  # data
  tmpdat <- model.frame(formula, data = data, na.action = NULL)
  # get summary table
  tmp <- table(tmpdat[,2], tmpdat[,1])
  n <- apply(tmp, 1, sum)
  p <- tmp[, "TRUE"]/n
  se_p <- sqrt(p * (1 - p) / n)
  tmp2 <- cbind(n,
                tmp[, "TRUE"],
                formatC(p, format = "f", digits = 2),
                se_p,
                p - 2 * se_p,
                p + 2 * se_p)
  tmp3 <- tmp2[tmp2[, 2] > 0,]
  if (is.null(value)) {
    value <- rownames(tmp3)
  }
  out <- rbind(c("Value", "n", "Freq", "Prop", "SE", "lo", "hi"),
               cbind(value, tmp3))
  dimnames(out) <- NULL
  return(out)
}

#' @export
hete.heatplot <- function(formula, data, name = NULL) {
  ## misc
  tmp <- R306::mySummary.simple(formula, data)
  tmp2 <- reshape2::melt(tmp[, seq(2, ncol(tmp) - 4, 4)])
  xtmp <- levels(tmp2$Var2); ytmp <- levels(tmp2$Var1)
  if (is.null(name)) {
    ylabel <- levels(tmp2$Var1)
  } else {ylabel <- name}

  ## color
  require(RColorBrewer)
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space = "Lab")

  ## plot
  ggplot(data = tmp2, aes(x = Var2, y = Var1, fill = value)) +
    geom_tile() +
    labs(x = "", y = "") +
    scale_fill_gradientn(colours = myPalette(100)) +
    scale_x_discrete(expand = c(0, 0), breaks = NULL) +
    scale_y_discrete(expand = c(0.1, 0), labels = ylabel) +
    annotate(geom = "text", y = rep(length(ytmp) + 1.5, length(xtmp)), x = 1:(length(xtmp)), size = 4, fontface = "bold",
             label = xtmp) +
    annotate(geom = "text", y = length(ytmp) + 1.5, x = length(xtmp) + 1, size = 4, fontface = "bold", hjust = 1,
             label = "I2") +
    annotate(geom = "text", y = 1:length(ytmp), x = length(xtmp) + 1, size = 4, hjust = 1,
             label = formatC(tmp[, "I2"], format = "f", digits = 0)) +
    theme_bw() +
    theme(axis.ticks = element_blank())
}