packages <- c('foreach',
              'doFuture',
              'xts')

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

WinDoPar <- function(
  x, # An xts object
  n, # Number of periods to apply rolling function window over
  w, # Either 'run' (rolling window) or 'exp' (expanding window)
  fun, # Item to match as function: a function, symbol or character string
  ... # Optional arguments to fun
)
{
  # +------------------------------------------------------------------
  # | When called inside functions that take a function as argument, 
  # | extract the desired function object while avoiding undesired 
  # | matching to objects of other types. A function matching FUN is 
  # | returned.
  # +------------------------------------------------------------------
  
  fn <- match.fun(FUN = fun)
  
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
  
  raw.result <- foreach(i = n:nrow(x), .combine = rbind) %dopar%
  {
    if (w == 'exp')
    {
      return(fn(x[1:i, ], ...))
    } else {
      return(fn(x[(i - n + 1):i, ], ...))
    }
  }
  na.to.add <- matrix(data = NA,
                      ncol = ncol(raw.result),
                      nrow = nrow(x) - nrow(raw.result))
  raw.result <- rbind(na.to.add, raw.result)
  
  # +------------------------------------------------------------------
  # | Conversion functions to coerce data objects of arbitrary classes
  # | to class xts and back, without losing any attributes of the 
  # | original format. An S3 object of class xts is returned.
  # +------------------------------------------------------------------
  
  xts.result <- xts(x = raw.result,
                    order.by = index(x))
  
  return(xts.result)
}
