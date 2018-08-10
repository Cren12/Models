packages <- c()

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

source('MakePaths.R')
source('PriceOptionAlongPath.R')
source('CountOptPricesAtTarget.R')

# +------------------------------------------------------------------

OptionTouchProbability <- function(
  TypeFlag, # A character string either "c" for a call option or a "p" for a put option
  S, # The asset price, a numeric value
  X, # A numeric value, the exercise price
  Time, # The time to maturity measured in years, a numeric value
  r, # The annualized rate of interest, a numeric value; e.g. 0.25 means 25% pa 
  b, # The annualized cost-of-carry rate, a numeric value; e.g. 0.1 means 10% pa
  sigma, # The annualized volatility of the underlying security, a numeric value; e.g. 0.3 means 30% volatility pa
  n.sim,
  target
)
{
  paths <- MakePaths(S = S,
                     Time = Time,
                     r = r,
                     b = b,
                     sigma = sigma,
                     n.sim = n.sim)
  prices <- PriceOptionAlongPath(TypeFlag = TypeFlag,
                                 X = X,
                                 Time = Time,
                                 r = r,
                                 b = b,
                                 sigma = sigma,
                                 paths = paths)
  touch.prob <- CountOptPricesAtTarget(prices = prices,
                                       target = target)
  return(touch.prob)
}
