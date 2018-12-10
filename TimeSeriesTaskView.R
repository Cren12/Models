packages <- c('robets',
              'forecast',
              'prophet',
              'smooth')

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

# robets::robets
# Si ottiene una serie stazionaria con degli spike potenzialmente
# lavorabili. Meno utile come indicatore di momentum ma da provare.
RobetsIndicator <- function(ohlc, h)
{
  fcast <- ohlc %>%
    rowMeans() %>%
    reclass(match.to = ohlc) %>%
    log() %>%
    as.vector() %>%
    robets() %>%
    forecast(h = h)
  fcast$mean %>%
    as.numeric() %>%
    diff() %>%
    last() %>%
    return()
}

# prophet::prophet
ProphetOscillator <- function(ohlc)
{
  price <- ohlc %>%
    rowMeans() %>%
    reclass(match.to = ohlc) %>%
    log()
  history <- data.frame(ds = index(ohlc),
                        y = price)
  m <- prophet(history)
  future <- make_future_dataframe(m = m,
                                  periods = 1)
  forecast <- predict(m, future)
  yhat <- forecast$yhat %>%
    xts(order.by = as.Date(forecast$ds))
  residuals <- (price - yhat) %>%
    scale() %>%
    last() %>%
    as.numeric() %>%
    return
}

ohlc <- `TLT US Equity`['2008/']
ohlc <- `HYG US Equity`
ohlc <- `SPY US Equity`

ciao <- WinDoPar(x = ohlc,
                 n = 250,
                 w = 'exp',
                 par = FALSE,
                 fun = ProphetOscillator)

chartSeries(ohlc)
addTA(ta = ciao, col = 'deepskyblue', on = 1)
addTA(ta = price - ciao * .9, col = 'coral')
addTA(ta = quant, on = 2)
zoomChart('2016/')
chart.CumReturns(ClCl(ohlc) * ifelse(ciao > 0, 1, 0))



### REPOSITORY ###

# GAS::UniGASFit
# UniGASFitIndicator <- function(ohlc)
# {
#   price <- ohlc %>%
#     rowMeans() %>%
#     reclass(match.to = ohlc)
#   cl.cl <- ohlc %>%
#     ClCl() %>%
#     na.omit()
#   op.op <- na.omit(Op(ohlc) / lag(Op(ohlc)) - 1)
#   op.cl <- ohlc %>%
#     OpCl() %>%
#     na.omit()
#   cl.op <- na.omit(Op(ohlc) / lag(Cl(ohlc)) - 1)
#   GASSpec <- UniGASSpec(Dist = 'norm',
#                         GASPar = list(location = TRUE,
#                                       scale = TRUE,
#                                       skewness = TRUE,
#                                       shape = TRUE,
#                                       shape2 = TRUE))
#   fit.clcl <- UniGASFit(GASSpec = GASSpec,
#                         data = cl.cl)
#   fit.opop <- UniGASFit(GASSpec = GASSpec,
#                         data = op.op)
#   fit.opcl <- UniGASFit(GASSpec = GASSpec,
#                         data = op.cl)
#   fit.clop <- UniGASFit(GASSpec = GASSpec,
#                         data = cl.op)
#   sigma.clcl <- fit.clcl@Estimates$Moments[, 2] %>%
#     sqrt() %>%
#     last() %>%
#     as.numeric()
#   sigma.opop <- fit.opop@Estimates$Moments[, 2] %>%
#     sqrt() %>%
#     last() %>%
#     as.numeric()
#   sigma.opcl <- fit.opcl@Estimates$Moments[, 2] %>%
#     sqrt() %>%
#     last() %>%
#     as.numeric()
#   sigma.clop <- fit.clop@Estimates$Moments[, 2] %>%
#     sqrt() %>%
#     last() %>%
#     as.numeric()
#   if (last(abs(cl.cl)) > 2 * sigma.clcl ||
#       last(abs(op.op)) > 2 * sigma.opop ||
#       last(abs(op.cl)) > 2 * sigma.opcl ||
#       last(abs(cl.op)) > 2 * sigma.clop)
#   {
#     last(price) %>%
#       as.numeric() %>%
#       return()
#   } else {
#     return(NA)
#   }
# }
# 
# UniGASFairValue <- function(ohlc)
# {
#   price <- ohlc %>%
#     rowMeans() %>%
#     reclass(match.to = ohlc)
#   fv <- WinDoPar(x = ohlc,
#                  n = 250,
#                  w = 'exp',
#                  fun = UniGASFitIndicator)
#   for (i in 2:nrow(fv))
#   {
#     if (is.na(fv[i, ]))
#       fv[i, ] <- fv[i - 1, ]
#   }
#   discount <- (price / fv - 1) * 100
#   quant <- WinDoPar(x = discount,
#                     n = 250,
#                     w = 'exp',
#                     fun = quantile,
#                     probs = .05,
#                     na.rm = TRUE)
#   return(quant)
# }