packages <- c('quantmod',
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

# source()

# +------------------------------------------------------------------

MinAvgDD_SMA <- function(
  ohlc,
  from.n = 5,
  to.n = 250,
  penalty = 4e-03
)
{
  # +------------------------------------------------------------------
  # | Form row means for the OHLC time series. Coerce data object to
  # | class xts. An S3 object of class xts is returned. 
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
  
  sma.dd.table <- foreach(n = from.n:to.n, .combine = rbind.data.frame) %dopar%
  {
    # +------------------------------------------------------------------
    # | Calculate various moving averages (MA) of the series. Simple 
    # | moving average is returned.
    # +------------------------------------------------------------------
    
    sma <- SMA(x = price,
               n = n)
    
    signal <- price >= sma
    R <- ifelse(signal, lag(ClCl(ohlc), -1), 0) - (signal != lag(signal)) * penalty
    
    # +------------------------------------------------------------------
    # | findDrawdowns will find the starting period, the ending period, 
    # | and the amount and length of the drawdown.
    # +------------------------------------------------------------------
    
    sma.dd.table <- (cbind(n, mean(Drawdowns(R = na.omit(R)))))
    
    colnames(sma.dd.table) <- c('n', 'AvgDrawdown')
    return(sma.dd.table)
  }
  
  # +------------------------------------------------------------------
  # | lm is used to fit linear models.
  # +------------------------------------------------------------------
  
  model <- lm(formula = sma.dd.table$AvgDrawdown ~ sma.dd.table$n + I(sma.dd.table$n ^ 2))
  
  # +------------------------------------------------------------------
  # | Determines the location, i.e., index of the (first) maximum of 
  # | the linear model fitted values.
  # +------------------------------------------------------------------
  
  best.n <- sma.dd.table$n[which.max(model$fitted.values)]
  
  # +------------------------------------------------------------------
  # | Simple moving average with the best period is returned.
  # +------------------------------------------------------------------
  
  return(as.numeric(last(SMA(x = price,
                             n = best.n))))
}
