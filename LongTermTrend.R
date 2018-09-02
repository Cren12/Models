packages <- c('xts')

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

# source()

LongTermTrend <- function(
  x,
  span # The parameter α which controls the degree of smoothing
)
{
  # +------------------------------------------------------------------
  # | Generic functions for replacing each NA with interpolated values.
  # | An object of similar structure as object with NAs replaced by 
  # | interpolation. 
  # +------------------------------------------------------------------
  
  y <- na.approx(object = x)
  
  x <- as.numeric(index(y))
  
  # +------------------------------------------------------------------
  # | Fit a polynomial surface determined by one or more numerical 
  # | predictors, using local fitting. An object of class "loess" is
  # | returned.
  # +------------------------------------------------------------------
  
  local.reg <- loess(formula = y ~ x,
                     span = span)
  
  # +------------------------------------------------------------------
  # | Conversion functions to coerce data objects of arbitrary classes 
  # | to class xts and back, without losing any attributes of the 
  # | original format.
  # +------------------------------------------------------------------
  
  lt.trend <- reclass(x = local.reg$fitted,
                      match.to = y)
  
  return(lt.trend)
}