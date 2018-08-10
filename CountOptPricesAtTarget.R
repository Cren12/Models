packages <- c('foreach',
              'doFuture')

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

CountOptPricesAtTarget <- function(
  prices,
  target
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
  
  prices.at.target <- foreach(j = 1:length(target), .combine = c) %:% 
    foreach(i = 1:length(prices), .combine = '+') %dopar%
  {
    path <- prices[[i]]
    if (any(path >= target[j]))
    {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  return(prices.at.target / length(prices))
}
