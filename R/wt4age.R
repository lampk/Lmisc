################################################################################
# Function to get weight-for-age boundaries (0 to 19 year-old)
# by: Phung Khanh Lam
# version: 15 September 2014
################################################################################
#' @export
wt4age <- function(age_year, sex = NULL){
  # load reference data
  load(file.path(system.file(package = "Lmisc"), "data", "wt4age.Rdata"))

  # function
  tmpfunc <- function(ref, age){
    with(ref[ref$age == age, ], c(min(m * exp(log(1 - 3 * s * l)/l)), max(m * exp(log(1 + 3 * s * l)/l))))
  }

  if (age_year <= 10){
    if (age_year <= 5){
      age <- round(age_year * 12 * 30.4375)
      ref <- w0_5
    } else {
      age <- round(age_year * 12)
      ref <- w5_10
    }

    if (is.null(sex)){ref <- ref} else {ref <- ref[ref$sex == sex, ]}

    ## output
    out <- tmpfunc(ref, age)

  } else {
    age <- round(pmin(age_year, 229/12) * 12)

    if (is.null(sex)){
      ref1 <- h5_19; ref2 <- b5_19
    } else {
      ref1 <- h5_19[h5_19$sex == sex, ]; ref2 <- b5_19[b5_19$sex == sex, ]
    }
    ## get min/max height-for-age
    tmp1 <- tmpfunc(ref1, age)/100
    ## get min/max bmi-for-age
    tmp2 <- tmpfunc(ref2, age)
    ## output
    out <- c((min(tmp1)^2) * min(tmp2), (max(tmp1)^2) * max(tmp2))
  }

  return(out)
}
