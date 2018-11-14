packages <- c('foreach',
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

source('FindMins.R')

# +------------------------------------------------------------------

MakeSuppTable <- function(
  hl, # Object that is coercible to xts or matrix and contains either a High-Low price series, or a Close price series
  change, # Minimum price movement in percent
)
{
  min.points <- FindMins(hl = hl,
                         change = change)
  tolerance <- mean(Hi(hl) / Lo(hl) - 1)
  
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
  
  supp.table <- foreach(i = 1:nrow(min.points), .combine = rbind) %:%
    foreach(j = 1:nrow(min.points), .combine = rbind) %dopar%
    {
      if (i < j)
      {
        avg <- mean(c(min.points[i], min.points[j]))
        sse <- sum((c(min.points[i], min.points[j]) - avg) ^ 2)
        distance <- abs(as.numeric(s0) / avg - 1)
        from <- min(c(index(min.points[i])), c(index(min.points[j])))
        to <- max(c(index(min.points[i])), c(index(min.points[j])))
        duration <- index(last(hl)) - to
        return(cbind(avg, sse, distance, from, to, duration))
      } else {
        return(rep(NA, 6))
      }
    }
  
  # +------------------------------------------------------------------
  # | unique returns a data frame like x but with duplicate 
  # | elements/rows removed.
  # +------------------------------------------------------------------
  
  supp.table <- unique(na.omit(supp.table))
  
  rownames(supp.table) <- 1:nrow(supp.table)
  supp.table.df <- as.data.frame(supp.table)
  supp.table.df$from <- as.Date(supp.table.df$from)
  supp.table.df$to <- as.Date(supp.table.df$to)
  to.remove <- foreach(i = 1:nrow(supp.table.df), .combine = c) %dopar%
  {
    from <- supp.table.df$from[i]
    avg <- supp.table.df$avg[i]
    if (any(hl[paste0(from + 1, '/')] < avg * (1 - tolerance)))
    {
      return(i)
    } else {
      return(NA)
    }
  }
  supp.table.net <- supp.table.df[-na.omit(to.remove), ]
  supp.table.net <- supp.table.net[order(supp.table.net$sse * supp.table.net$duration / as.numeric((supp.table.net$to - supp.table.net$from))), ]
  return(supp.table.net)
}

