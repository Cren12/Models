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
# | Sys.setenv sets environment variables.
# +------------------------------------------------------------------

Sys.setenv(TZ = 'UTC')

# +------------------------------------------------------------------
# | source() causes R to accept its input from the named file or URL
# | or connection or expressions directly.
# +------------------------------------------------------------------

# source()

# +------------------------------------------------------------------

FindMins <- function(
  hl, # Object that is coercible to xts or matrix and contains either a High-Low price series, or a Close price series
  change # Minimum price movement in percent
)
{
  # +------------------------------------------------------------------
  # | Zig Zag higlights trends by removing price changes smaller than
  # | change and interpolating lines between the extreme points. A 
  # | object of the same class as HL or a vector (if try.xts fails)
  # | containing the Zig Zag indicator is returned.
  # +------------------------------------------------------------------
  
  zig.zag <- ZigZag(HL = hl,
                    change = change)
  
  # +------------------------------------------------------------------
  # | Functions to find valleys (bottoms) of a given series. A vector
  # | of integers corresponding to valleys is returned.
  # +------------------------------------------------------------------
  
  valleys.index <- findValleys(x = zig.zag)
  
  mins <- Lo(hl)[valleys.index - 1]
  return(mins)
}