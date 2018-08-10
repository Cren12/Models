packages <- c('foreach',
              'doFuture',
              'fOptions',
              'xts')

# +------------------------------------------------------------------
# | library() and require() load and attach add-on packages. Possibly,
# | download and install packages from a CRAN-like repositoriy.
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
# | source() causes R to accept its input from the named file or URL
# | or connection or expressions directly.
# +------------------------------------------------------------------

# source()

# +------------------------------------------------------------------

PriceOptionAlongPath <- function(
  TypeFlag, # A character string either "c" for a call option or a "p" for a put option
  X, # A numeric value, the exercise price
  Time, # The time to maturity measured in years, a numeric value
  r, # The annualized rate of interest, a numeric value; e.g. 0.25 means 25% pa
  b, # The annualized cost-of-carry rate, a numeric value; e.g. 0.1 means 10% pa
  sigma, # The annualized volatility of the underlying security, a numeric value; e.g. 0.3 means 30% volatility pa
  paths
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
  # | sequentially or in parallel. multiprocess: If multicore 
  # | evaluation is supported, that will be used, otherwise 
  # | multisession evaluation will be used.
  # +------------------------------------------------------------------
  
  plan(multiprocess)
  
  prices <- foreach(i = 1:length(paths)) %dopar%
  {
    S <- paths[[i]]
    tau <- Time - as.numeric(names(paths[[i]]))
    
    # +------------------------------------------------------------------
    # | GBSOption calculates the option price. Returns an object of class
    # | "fOption" and a numeric value with the value of the option.
    # +------------------------------------------------------------------
    
    path.prices <- GBSOption(TypeFlag = TypeFlag,
                             S = S,
                             X = X,
                             Time = tau,
                             r = r,
                             b = b,
                             sigma = sigma)@price
    
    # +------------------------------------------------------------------
    # | is.nan tests if a numeric value is NaN.
    # +------------------------------------------------------------------
    
    path.prices[is.nan(path.prices)] <- max(0, ifelse(TypeFlag == 'c', last(S) - X, X - last(S)))
    
    return(path.prices)
  }
  return(prices)
}