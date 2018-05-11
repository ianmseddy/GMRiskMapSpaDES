# focalWeightRisk()
#
#' Focal weights matrix for different risk curves
#'
#' Calculates focal ("moving window") weight matrix using \code{translateRisk} for 
#' use in the \code{focal} function from the raster package.
#' 
#' @param x Raster object
#' @param type Shape of risk curve: \code{'square'}, \code{'linear'}, \code{'twopartlinear'}, \code{'exponential'},
#'   \code{'logistic'}, or \code{'gaussian'}
#' @param a Numeric; maximum distance in raster units of risk 
#' @param b Numeric; distance in raster units of constant risk (for twopartlinear); must be 
#'   less than \code{a}
#' @param riskCutoff Numeric; cutoff where risk is assumed 0 for \code{type = 
#'   'logistic', 'exponential'} or \code{'gaussian'}. Number between 0 and 1
#' @return A matrix of weights that can be used in \code{raster::focal}
#' @details This funcation calls \code{raster::focalWeight} where type is set 
#'   to "circle" with a diameter equal to \code{a}.
#' @seealso \code{translateRisk} for examples
#' @export
focalWeightRisk <- function(x, type, a, b, riskCutoff=0.01){
  
  d <- a
  w <- raster::focalWeight(x,d,type="circle")
  size <- dim(w)[1]
  coords <- expand.grid(1:size,1:size)
  cent <- ceiling(size^2/2)
  #distvec<-as.matrix(dist(coords,upper=TRUE,diag=TRUE))[cent,]
  distvec <- sqrt((((coords[,1]-coords[cent,1])*raster::xres(x))^2)+(((coords[,2]-coords[cent,2])*raster::yres(x))^2))
  distmat <- matrix(distvec,nrow=size)
  risktype <- type
  wtmat <- matrix(translateRisk(as.vector(distmat),type=risktype, a=a, b=b, riskCutoff=riskCutoff), nrow=size, ncol=size)
  wtmat <- wtmat/max(wtmat) # scales max risk to 1 when using focal()
  return(wtmat)
}
