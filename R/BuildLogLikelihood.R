#' Builds a log-likelihood function for a given pair of pdf and data.
#' @export
#' 
#' @param pdf A probability density function that expects a pair (x, theta) and perhaps a log flag.
#' @param theta A vector of parameters for pdf.
#' @param data A vector of data.
#' 
#' @return A log-likelihood function expecting a parameter vector.
BuildLogLikelihood <- function (pdf, data) {
  # The pdf has a log.p parameter?
  if (any(names(formals(pdf))=="log")) {
    retVal <- function (...) {
      return(sum(pdf(data, log = TRUE, ...)))
    }
  } else {
    retVal <- function (...) {
      return(sum(log(pdf(data, ...))))
    }
  }
  return(retVal)
}