# translateRisk()
#
#' Risk vector for different risk curves
#'
#' Calculates risk value based on distance for use in the \code{focalWeightRisk} 
#' function.
#'
#' @param x A numeric vector of distances
#' @param type Shape of risk curve: \code{'square'}, \code{'linear'}, \code{'twopartlinear'}, \code{'exponential'},
#'   \code{'logistic'}, or \code{'gaussian'}
#' @param a Numeric; maximum distance in raster units of risk 
#' @param b Numeric; distance in raster units of constant risk (for twopartlinear); must be 
#'   less than \code{a}
#' @param riskCutoff Numeric; cutoff where risk is assumed 0 for \code{type = 
#'   'logistic', 'exponential'} or \code{'gaussian'}. Number between 0 and 1
#' @return A numeric vector of risk for each distance in \code{x}
#' @examples 
#' x <- seq(0, 100, 0.01)
#' 
#' # square: Constant risk up to maximum distance a
#' risk <- translateRisk(x, type="square", a=6)
#' plot(x, risk, type="l", main="square")
#' 
#'# linear: Risk declines linearly up to maximum distance a
#' risk <- translateRisk(x, type="linear", a=50)
#' plot(x, risk, type="l", main="linear")
#' 
#' # twopartlinear: Constant risk up to distance b, then risk  
#' # decreases linearly from distance b to a
#' risk <- translateRisk(x, type="twopartlinear", a=50, b=30)
#' plot(x, risk, type="l", main="twopartlinear")
#' 
#' # exponential: Risk declines exponentially up to distance a 
#' # where risk = riskCutoff
#' risk <- translateRisk(x, type="exponential", a=50)
#' plot(x, risk, type="l", main="exponential")
#' 
#' # logistic: Risk declines logistically up to distance a 
#' # where risk = riskCutoff. Logistic curve midpoint = half 
#' # of distance a
#' risk <- translateRisk(x, type="logistic", a=50)
#' plot(x, risk, type="l", main="logistic")
#' 
#' # gaussian: Risk decline follows a gaussian curve up to 
#' # distance a  where risk = riskCutoff 
#' risk <- translateRisk(x, type="gaussian", a=50)
#' plot(x, risk, type="l", main="gaussian")
#' @export
translateRisk <- function(x, type, a=0, b=0, 
                            riskCutoff=0.01) {
 
  switch(type,
         square = {
           risk<-ifelse(x<=a,1,0)
         },
         linear = {
           risk<-ifelse(x<=a,1-(x*1/a),0)
         },
         exponential = {
           cutoff<-riskCutoff #theshold at which we assume there is no effect
           risk<-ifelse(x<=a,exp((log(cutoff)/a)*x),0)
         },
         logistic = { #set max distance, midpoint of curve at half of max distance
           cutoff<-riskCutoff 
           risk<-ifelse(x<=a,(1/(1 + exp(-log(cutoff)/(a/2) * (x-(a/2))))),0)
         },
         twopartlinear = { #risk = 1 up to distance b, then decreases linearly to max distance a
           risk<-ifelse(x <= b, 1,
                        ifelse(x > b & x <= a, (a/(a-b)) - (x*(1/(a-b))), 0))
         },
         gaussian = { #risk decreases with gaussian curve up to max distance a
           cutoff<-riskCutoff
           risk<-ifelse(x <= a, 1 * exp(-((x - 0)^2) / (2*(1/(sqrt( (-4*log(cutoff)) / (a^2) ))))^2),0)
         }
  )
}
