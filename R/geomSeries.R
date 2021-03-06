#' Geometric series
#'
#' A function that given a base and a maximum value will calculate all the terms of 
#' a geometrical series
#' 
#' @param base Base of the series
#' @param max The maximum value of the series
#' @param n The length of the series
#' @keywords geometric sries
#' @export
#' @examples
#' geomSeries(2,512)
geomSeries <- function(base, max,n=10) {
serie <- max / base ^ (0:(n-1))
return(serie)
}