packages <- c('foreach',
              'doFuture',
              'xts',
              'TTR')

# +------------------------------------------------------------------
# | library and require load and attach add-on packages. Download and
# | install packages from CRAN-like repositories.
# +------------------------------------------------------------------

lapply(X = packages,
       FUN = function(package){
         if (!require(package = package,
                      character.only = TRUE))
         {
           install.packages(pkgs = package,
                            repos = "https://cloud.r-project.org")
           library(package = package,
                   character.only = TRUE)
         } else {
           library(package = package,
                   character.only = TRUE)    
         }
       })

MakeSMA_curve <- function(
  x,
  n.seq
)
{
  # +------------------------------------------------------------------
  # | Register the doFuture parallel adaptor to be used by the foreach 
  # | package.
  # +------------------------------------------------------------------
  
  registerDoFuture()
  
  # +------------------------------------------------------------------
  # | This function allows the user to plan the future, more 
  # | specifically, it specifies how future():s are resolved, e.g. 
  # | sequentially or in parallel. If multicore evaluation is supported,
  # | that will be used, otherwise multisession evaluation will be used.
  # +------------------------------------------------------------------
  
  plan(multiprocess)
  
  raw.curves <- foreach(j = n.seq, .combine = cbind.data.frame) %dopar%
  {
    # +------------------------------------------------------------------
    # | SMA calculates the arithmetic mean of the series over the past n
    # | observations.
    # +------------------------------------------------------------------
    
    return(SMA(x = x,
               n = j))
  }
  
  # +------------------------------------------------------------------
  # | Conversion functions to coerce data objects of arbitrary classes
  # | to class xts and back, without losing any attributes of the 
  # | original format. An S3 object of class xts is returned.
  # +------------------------------------------------------------------
  
  xts.curves <- na.omit(reclass(x = raw.curves,
                                match.to = x))
  
  raw.smooth.curves <- foreach(i = 1:nrow(xts.curves), .combine = rbind.data.frame) %dopar%
  {
    # +------------------------------------------------------------------
    # | Fits a cubic smoothing spline to the supplied data. The fitted
    # | values corresponding to x are returned.
    # +------------------------------------------------------------------
    
    return(smooth.spline(x = n.seq,
                         y = xts.curves[i, ])$y)
  }
  
  # +------------------------------------------------------------------
  # | Conversion functions to coerce data objects of arbitrary classes
  # | to class xts and back, without losing any attributes of the 
  # | original format. An S3 object of class xts is returned.
  # +------------------------------------------------------------------
  
  xts.smooth.curves <- reclass(x = raw.smooth.curves,
                               match.to = xts.curves)
  
  return(xts.smooth.curves)
}