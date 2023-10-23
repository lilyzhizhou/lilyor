#' @title OR and 95 percent CI
#'
#' @description
#' Creates odds ratio with 95 percent confidence intervals
#'
#' @param coef coefficient
#' @param se standard error
#' @param siglevel significance level
#' @param roundto round number of digits of CI
#'
#' @return y
#' @author Lily Zhou
#' @export
#'
#' @examples
#' data.lrm <- lrm(y ~ x1 + x2, data = toydata, x = TRUE, y = TRUE)
#' OR_95CI(1.0055, 0.2534, 0.05, 2)

OR_95CI <- function(coef, se, siglevel, roundto){
  q <- 1 - siglevel / 2
  OR <- exp(coef)
  ORlcl <- exp(coef - qnorm(q) * se)
  ORucl <- exp(coef + qnorm(q) * se)
  ORresult <- paste0(format(round(OR, roundto), nsmall = roundto),
                     " (",
                     format(round(ORlcl, roundto), nsmall = roundto),
                     ", ",
                     format(round(ORucl, roundto), nsmall = roundto),
                     ")"
  )
  return(ORresult)
}
