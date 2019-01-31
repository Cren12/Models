packages <- c('Rblpapi',
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

getSymbolsFromBloomberg <- function(
  securities,
  start.date
)
{
  # +------------------------------------------------------------------
  # | This function connects to the Bloomberg API.
  # +------------------------------------------------------------------
  
  con <- blpConnect()
  
  fields <- c('PX_OPEN', 'PX_HIGH', 'PX_LOW', 'PX_LAST')
  
  # +------------------------------------------------------------------
  # | This function uses the Bloomberg API to retrieve 'bdh' (Bloomberg
  # | Data History) queries. A list with as a many entries as there are
  # | entries in securities is returned; each list contains a 
  # | data.frame with one row per observations and as many columns as
  # | entries in fields. If the list is of length one, it is collapsed
  # | into a single data frame.
  # +------------------------------------------------------------------
  
  ts <- bdh(securities = securities,
            fields = fields,
            start.date = start.date)
  
  if (!is.data.frame(ts))
  {
    ts.xts <- lapply(X = ts,
                     FUN = function(ts){
                       
                       # +------------------------------------------------------------------
                       # | Constructor function for creating an extensible time-series 
                       # | object. xts is used to create an xts object from raw data inputs.
                       # | An S3 object of class xts is returned.
                       # +------------------------------------------------------------------
                       
                       ts.xts <- xts(ts[, -1],
                                     order.by = ts[, 1])
                       
                       colnames(ts.xts) <- c('Open', 'High', 'Low', 'Close')
                       return(ts.xts)
                     })
    for (security in securities)
    {
      # +------------------------------------------------------------------
      # | Assign a value to a name in an environment. This function is 
      # | invoked for its side effect, which is assigning value to the 
      # | variable x.
      # +------------------------------------------------------------------
      
      assign(x = security,
             value = ts.xts[[security]],
             envir = .GlobalEnv)
    }
  } else {
    
    # +------------------------------------------------------------------
    # | Constructor function for creating an extensible time-series 
    # | object. xts is used to create an xts object from raw data inputs.
    # | An S3 object of class xts is returned.
    # +------------------------------------------------------------------
    
    ts.xts <- xts(x = ts[, -1],
                  order.by = ts[, 1])
    
    colnames(ts.xts) <- c('Open', 'High', 'Low', 'Close')
    
    # +------------------------------------------------------------------
    # | Assign a value to a name in an environment. This function is 
    # | invoked for its side effect, which is assigning value to the 
    # | variable x.
    # +------------------------------------------------------------------
    
    assign(x = securities,
           value = ts.xts,
           envir = .GlobalEnv)
  }
  return(securities)
}