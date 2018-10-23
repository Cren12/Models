packages <- c('foreach',
              'doFuture',
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

# +------------------------------------------------------------------
# | Sys.setenv sets environment variables.
# +------------------------------------------------------------------

Sys.setenv(TZ = 'UTC')

# +------------------------------------------------------------------
# | source() causes R to accept its input from the named file or URL
# | or connection or expressions directly.
# +------------------------------------------------------------------

# source('LOESS_trendIndicator.R')

# +------------------------------------------------------------------

RSI_dens <- function(
  ohlc, # Price series that is coercible to xts or matrix
  from.n = 5, # Minimum number of periods for moving averages
  to.n = 250 # Maximum number of periods for moving averages
)
{
  # +------------------------------------------------------------------
  # | Form row and column sums and means for numeric arrays (or data 
  # | frames). Conversion functions to coerce data objects of arbitrary
  # | classes to class xts and back, without losing any attributes of
  # | the original format. An S3 object of class xts is returned. 
  # +------------------------------------------------------------------
  
  price <- reclass(x = rowMeans(ohlc),
                   match.to = ohlc)
  
  trend.prob <- LOESS_trendIndicator(ohlc = ohlc)
  
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
  
  oversolds <- foreach(n = from.n:to.n, .combine = rbind.data.frame) %dopar%
  {
    rsi <- RSI(price = price,
               n = n)
    wpr <- WPR(HLC = ohlc[, c('High', 'Low', 'Close')],
               n = n)
    cci <- CCI(HLC = ohlc[, c('High', 'Low', 'Close')],
               n = n)
    value <- cbind.data.frame(as.numeric(last(rsi)),
                              as.numeric(last(wpr)),
                              as.numeric(last(cci)))
    colnames(value) <- c('RSI', 'WPR', 'CCI')
    return(value)
  }
  
  # +------------------------------------------------------------------
  # | The (S3) generic function density computes kernel density 
  # | estimates. Its default method does so with the given kernel and 
  # | bandwidth for univariate observations.
  # +------------------------------------------------------------------
  
  # rsis.dens <- density(oversolds$RSI)
  # wprs.dens <- density(oversolds$WPR)
  # ccis.dens <- density(oversolds$CCI)
  
  # rsi.under.30.dens <- sum(rsis.dens$y[rsis.dens$x < 30] * diff(rsis.dens$x[rsis.dens$x < 30]))
  # wpr.over.80.dens <- sum(wprs.dens$y[wprs.dens$x > .8] * diff(wprs.dens$x[wprs.dens$x > .8]))
  # cci.under.minus.100.dens <- sum(ccis.dens$y[ccis.dens$x < -100] * diff(ccis.dens$x[ccis.dens$x < -100]))
  rsi.under.30.dens <- length(oversolds$RSI[oversolds$RSI < 30]) / length(oversolds$RSI)
  wpr.over.80.dens <- length(oversolds$WPR[oversolds$WPR > .8]) / length(oversolds$WPR)
  cci.under.minus.100.dens <- length(oversolds$CCI[oversolds$CCI < -100]) / length(oversolds$CCI)
  oversold.prob <- 1 - (1 - rsi.under.30.dens) * (1 - wpr.over.80.dens) * (1 - cci.under.minus.100.dens)
  return(max(0, 2 * oversold.prob * trend.prob - 1))
}
