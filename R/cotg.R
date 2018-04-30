#' Density function for the Co-Tangent Gumbel probability distribution.
#' @export
#' @family Co-Tangent-Gumbel distribution utilities
#'
#' @param par A vector of parameters. Only the first two will be used as alpha and beta.
#' @param x Point where to evaluate the density.
#' @return The sum of \code{x} and \code{y}
#' @examples
#' pdf_cotG(c(1, 1), 0)
#' pdf_cotG(c(0.1, 0.8), 0.5)
#' pdf_cotG(c(1.5, 1), 1)
#' pdf_cotG(c(1, 0.5), 1.5)
#' 
#' 
dcotg <- function(par, x){
  alpha      = par[1]
  beta       = par[2]
  # -(2/3)*pi*exp(-(exp(-(-x+alpha)/beta)*beta+alpha-x)/beta)*sin((1/3)*pi*(-1+exp(-exp(-(-x+alpha)/beta))))/(beta*cos((1/3)*pi*(-1+exp(-exp(-(-x+alpha)/beta))))^2)
  (-2*pi/3)*exp(-(exp((x-alpha)/beta)*beta+alpha-x)/beta)*sin((pi/3)*(exp(-exp((x-alpha)/beta))-1))/(beta*cos((pi/3)*(exp(-exp((x-alpha)/beta))-1))^2)
}
