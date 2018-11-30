package = c("timeSeries",
            "compiler",
            "xts")
for (this.package in package)
{
  if (!require(package = this.package,
               character.only = TRUE))
  {
    install.packages(pkgs = this.package,
                     repos = "https://cloud.r-project.org")
    library(package = this.package,
            character.only = TRUE)
  } else {
    library(package = this.package,
            character.only = TRUE)    
  }
}

# +------------------------------------------------------------------
# | Smoothes a 'timeSeries' object.
# +------------------------------------------------------------------

SmoothTimeSeries = cmpfun(function(x, spline = TRUE)
{
  if (spline)
  {
    x <- na.omit(x)
    x.ss <- smoothSpline(x = as.timeSeries(coredata(x)))
    value <- xts(x = x.ss,
                 order.by = index(x))
    return(value$spline)
  } else {
    super.sm <- supsmu(x = as.numeric(index(x)),
                       y = x)
    super.sm <- xts(x = super.sm$y,
                    order.by = index(x))
    return(super.sm)
  }
})
