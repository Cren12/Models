packages <- c('quantmod')

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
# | source causes R to accept its input from the named file or URL or
# | connection or expressions directly. Input is read and parsed from
# | that file until the end of the file is reached, then the parsed 
# | expressions are evaluated sequentially in the chosen environment.
# +------------------------------------------------------------------

source('LongTermTrend.R')

PTI <- function(
  x,
  span = .99, # The parameter α which controls the degree of smoothing
  oversold.p = .05
)
{
  x <- log(x)
  trend <- LongTermTrend(x = x,
                         span = span)
  if (last(diff(trend)) > 0)
  {
    detrended.x <- x - trend
    
    # +------------------------------------------------------------------
    # | The generic function quantile produces sample quantiles 
    # | corresponding to the given probabilities. The smallest 
    # | observation corresponds to a probability of 0 and the largest to
    # | a probability of 1.
    # +------------------------------------------------------------------
    
    oversold <- quantile(x = detrended.x,
                         probs = oversold.p)
    
    if (last(detrended.x) < oversold)
    {
      # +------------------------------------------------------------------
      # | Fits a cubic smoothing spline to the supplied data. An object of 
      # | class "smooth.spline" is returned.
      # +------------------------------------------------------------------
      
      smoothed.x <- smooth.spline(x = index(detrended.x),
                                  y = detrended.x)
      
      # +------------------------------------------------------------------
      # | Function to find the valleys (bottoms) of a given series. A
      # | vector of integers corresponding to valleys is returned. As a 
      # | valley is defined as the lowest value in a series, the function 
      # | can only define it after a change in direction has occurred. This
      # | means that the function will always return the first period after 
      # | the valley of the data, so as not to accidentally induce a 
      # | look-ahead bias.
      # +------------------------------------------------------------------
      
      valleys.index <- findValleys(x = smoothed.x$y)
      
      if (last(valleys.index) == length(smoothed.x$y))
      {
        return(as.numeric(exp(last(x))))
      }
    }
  }
  return(0)
}