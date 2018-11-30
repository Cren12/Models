packages <- c('forecast',
              'robets')

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

source('SmoothTimeSeries.R')

# +------------------------------------------------------------------

# stats::supsmu
SupsmuTrading <- function(ohlc)
{
  super.sm <- supsmu(x = as.numeric(index(ohlc)),
                     y = log(Cl(ohlc)))
  super.sm <- xts(x = super.sm$y,
                  order.by = index(ohlc))
  value <- super.sm %>%
    diff() %>%
    last() %>%
    as.numeric()
  return(value)
}

# forecast::arfima
ArfimaTrading <- function(ohlc, h)
{
  fcast <- ohlc %>%
    Cl() %>%
    log() %>%
    SmoothTimeSeries(spline = FALSE) %>%
    arfima(drange = c(0, .99)) %>%
    forecast(h = h, level = 99)
  value <- cbind(fcast$lower, fcast$upper) %>%
    last() %>%
    as.numeric() %>%
    exp()
  return(value)
}

# robets::
# MAPA::

ohlc <- `TLT US Equity`
ohlc <- `GLD US Equity`




ciao <- WinDoPar(x = ohlc,
                 n = 250, 
                 w = 'exp',
                 fun = SupsmuTrading)
chartSeries(ohlc)
addTA(ciao, col = 'deepskyblue')


chart.CumReturns(ClCl(ohlc) * ifelse(ciao > 0, 1, 0))
