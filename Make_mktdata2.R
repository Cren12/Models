packages <- c('TTR',
              'foreach',
              'doFuture')

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

source('FindMins.R')

# +------------------------------------------------------------------

Make_mktdata2 <- function(
  ohlc # An OHLC object
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
  
  X <- foreach(n = 5:250) %dopar%
  {
    # +------------------------------------------------------------------
    # | Selected volatility estimators/indicators. A object of the same 
    # | class as OHLC or a vector (if try.xts fails) containing the 
    # | chosen volatility estimator values is returned.
    # +------------------------------------------------------------------
    
    volat <- TTR::volatility(OHLC = ohlc,
                             n = n,
                             calc = 'yang.zhang',
                             N = 252)
    
    # +------------------------------------------------------------------
    # | Calculate the (rate of) change of a series over n periods. A 
    # | object of the same class as x or a vector (if try.xts fails) 
    # | containing the rate-of-change (or return) values for ROC is 
    # | returned.
    # +------------------------------------------------------------------
    
    roc <- ROC(x = price,
               n = n)
    
    # +------------------------------------------------------------------
    # | Calculate simple moving average (MA) of a series.
    # +------------------------------------------------------------------
    
    sma <- price - SMA(x = price,
                       n = n)
    
    # +------------------------------------------------------------------
    # | The Relative Strength Index (RSI) calculates a ratio of the
    # | recent upward price movements to the absolute price movement.
    # +------------------------------------------------------------------
    
    rsi <- RSI(price = price,
               n = n)
    
    value <- cbind.data.frame(volat, roc, sma, rsi)
    colnames(value) <- paste0(c('volat', 'roc', 'sma', 'rsi'), '.', n)
    return(value)
  }
  
  # +------------------------------------------------------------------
  # | do.call constructs and executes a function call from a name or a
  # | function and a list of arguments to be passed to it.
  # +------------------------------------------------------------------
  
  X <- do.call(what = cbind.data.frame,
               args = X)
  
  value <- as.data.frame(X)
  
  # +------------------------------------------------------------------
  # | onversion functions to coerce data objects of arbitrary classes
  # | to class xts and back, without losing any attributes of the 
  # | original format. An S3 object of class xts is returned.
  # +------------------------------------------------------------------
  
  value <- reclass(x = value,
                   match.to = ohlc)
  return(value)
}