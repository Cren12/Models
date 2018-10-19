packages <- c('xts',
              'foreach',
              'doFuture')

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

LOESS_smoother <- function(ts.xts, span, n.sd, log = TRUE)
{
  
  # +------------------------------------------------------------------
  # | Fit a polynomial curve determined by time index as numerical 
  # | predictor, using local fitting.
  # +------------------------------------------------------------------
  
  if (log)
  {
    y <- na.omit(log(ts.xts))
  } else {
    y <- na.omit(ts.xts)
  }
  
  x <- 1:length(y)
  loess.model <- loess(formula = y ~ x,
                       span = span)
  
  # +------------------------------------------------------------------
  # | Predictions from the loess fit, with standard errors.
  # +------------------------------------------------------------------
  
  loess <- predict(object = loess.model,
                   se = TRUE)
  
  # +------------------------------------------------------------------
  # | xts is used to create an xts object from raw data inputs.
  # +------------------------------------------------------------------
  
  if (log)
  {
    smooth.high <- exp(loess$fit + n.sd * loess$se.fit)
    smooth <- exp(loess$fit)
    smooth.low <- exp(loess$fit - n.sd * loess$se.fit)
  } else {
    smooth.high <- loess$fit + n.sd * loess$se.fit
    smooth <- loess$fit
    smooth.low <- loess$fit - n.sd * loess$se.fit
  }
  
  return(xts(x = cbind(smooth, smooth.high, smooth.low),
             order.by = index(y)))
}

# +------------------------------------------------------------------

LOESS_trendIndicator <- function(
  ohlc
)
{
  # +------------------------------------------------------------------
  # | Form row and column sums and means for numeric arrays (or data 
  # | frames). Conversion functions to coerce data objects of arbitrary
  # | classes to class xts and back, without losing any attributes of
  # | the original format. An S3 object of class xts is returned. 
  # +------------------------------------------------------------------
  
  price <- reclass(x = rowMeans(ohlc),
                   match.to = ohlc)
  
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
  
  ten.wises.response <- foreach(i = seq(from = 5, to = 99, length.out = 10)) %dopar%
  {
    local.reg <- LOESS_smoother(ts.xts = price,
                                span = i / 100,
                                n.sd = 2)
    det.price.dist <- coredata(price - local.reg$smooth)
    p.det.price.dist <- det.price.dist[det.price.dist > 0]
    det.price <- last(det.price.dist)
    d.local.reg <- last(diff(log(local.reg$smooth)))
    return(list(trend = d.local.reg,
                p.det.price.dist = p.det.price.dist,
                det.price = max(0, det.price)))
  }
  trends <- foreach(i = 1:length(ten.wises.response), .combine = c) %do%
  {
    return(ten.wises.response[[i]]$trend)
  }
  p.det.price.dists <- foreach(i = 1:length(ten.wises.response), .combine = c) %do%
  {
    return(ten.wises.response[[i]]$p.det.price.dist)
  }
  det.prices <- foreach(i = 1:length(ten.wises.response), .combine = c) %do%
  {
    return(ten.wises.response[[i]]$det.price)
  }
  
  # +------------------------------------------------------------------
  # | The (S3) generic function density computes kernel density 
  # | estimates. Its default method does so with the given kernel and 
  # | bandwidth for univariate observations.
  # +------------------------------------------------------------------
  
  trend.dens <- density(trends)
  p.det.price.dens <- density(p.det.price.dists)
  det.prices.dens <- density(det.prices)
  
  det.prices.peak <- which.max(det.prices.dens$y)
  pos.dens <- sum(trend.dens$y[trend.dens$x >= 0] * diff(trend.dens$x[trend.dens$x >= 0]))
  neg.dens <- sum(p.det.price.dens$y[p.det.price.dens$x < p.det.price.dens$x[det.prices.peak]] * diff(p.det.price.dens$x[p.det.price.dens$x < p.det.price.dens$x[det.prices.peak]]))
  p <- max(0, pos.dens - neg.dens)
  return(2 * p - 1)
}
