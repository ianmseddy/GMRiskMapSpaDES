# populationSearchDistance()
#
#' Distance vector for determining population search radius for positive trap locations.
#'
#' @param x A numeric vector of catch
#' @param type Character; shape of distance curve: \code{'straight'},\code{'square'}, \code{'linear'},
#'   \code{'logistic'}, or \code{'gaussian'}
#' @param maxCatch Numeric; Maximum trap catch where the search radius is at maxDist
#' @param minDist Numeric; minimum search radius for traps of the same population
#' @param maxDist Numeric; maximum search radius for traps of the same population. Must be defined.
#' @return A numeric vector of distance for each catch in \code{x}
#' @examples 
#' x <- seq(1, 10, 1)
#' 
#' # straight: distance doesn't change with trap catch
#' dist <- populationSearchDistance(x, type="straight", maxDist=500)
#' df <- data.frame(x, dist)
#' plot(df$x, df$dist, type="l", main="straight")
#' 
#' # square: distance = minDist until maxCatch, then maxDist
#' dist <- populationSearchDistance(x, type="square", maxCatch=10, minDist=200, maxDist=500)
#' plot(x, dist, type="l", main="square")
#' 
#' # linear: distance increases linearly from minDist to maxDist, then remains constant at maxDist
#' dist <- populationSearchDistance(x, type="linear", maxCatch=10, minDist=500, maxDist=1000)
#' plot(x, dist, type="l", main="linear")
#' 
#' logistic: distance increases logistically from minDist to maxDist, then remains constant at maxDist
#' dist <- populationSearchDistance(x, type="logistic", maxCatch=10, minDist=500, maxDist=1000)
#' plot(x, dist, type="l", main="logistic")
#' 
#' gaussian: distance increase follows gaussian curve from minDist to maxDist, then remains constant at maxDist
#' dist <- populationSearchDistance(x, type="gaussian", maxCatch=10, minDist=500, maxDist=1000)
#' plot(x, dist, type="l", main="gaussian")
#' 
#' @export
populationSearchDistance <- function(x, type, maxCatch, minDist, maxDist, cutoff=0.0001) {
  switch(type,
         straight = {
           dist <- maxDist
         },
         square = {
           dist <- ifelse(x <= maxCatch, minDist, maxDist)
         },
         linear = {
           dist <- ifelse(x <= maxCatch, (maxDist-minDist)/(maxCatch-1)*x+(minDist-(maxDist-minDist)/(maxCatch-1)), maxDist)
         },
         logistic = {
           dist <- ifelse(x <= maxCatch, minDist + (maxDist-minDist)/(1 + exp(log(cutoff)/(maxCatch/2) * (x-(maxCatch/2)))), maxDist)
         },
         gaussian = {
           dist <- ifelse(x <= maxCatch, (maxDist-minDist) * exp( - (x-maxCatch)^2 / ((2*(1/(sqrt((-4*log(cutoff))/(maxCatch^2)))))^2) ), maxDist-minDist)
           dist <- dist+minDist
         }
  )
  return(dist)
}
