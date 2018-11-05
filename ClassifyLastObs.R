packages <- c('neuralnet')

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

source('Make_mktdata.R')

# +------------------------------------------------------------------

ClassifyLastObs <- function(
  data, # A data frame containing the variables
  classifier = c('nnet', 'logistic', 'svm')
)
{
  data <- na.omit(data)
  y <- data$Minimum.point
  X <- data[, -1]
  
  out <- tryCatch(
    expr = {
      
      # +------------------------------------------------------------------
      # | Performs a principal components analysis on the given data matrix
      # | and returns the results as an object of class prcomp.
      # +------------------------------------------------------------------
        
      pca <- prcomp(x = X,
                    scale. = TRUE)
        
      formula <- y ~ PC1 + PC2 + PC3 + PC4 + PC5
      
      if (classifier == 'nnet')
      {
        
        # +------------------------------------------------------------------
        # | neuralnet is used to train neural networks. neuralnet returns an
        # | object of class nn.
        # +------------------------------------------------------------------
        
        nnet <- neuralnet(formula = formula,
                          data = cbind.data.frame(y, pca$x[, 1:5]),
                          hidden = 5,
                          linear.output = FALSE)
        
        ## Complete model:
        # nnet1 <- neuralnet(formula = as.formula(substr(paste('y ~', paste(colnames(X), '+ ', collapse = '')), 1, nchar(paste('y ~', paste(colnames(X), '+ ', collapse = ''))) - 3)),
        #                    data = cbind.data.frame(y, X),
        #                    hidden = 2)
        
        # +------------------------------------------------------------------
        # | compute, a method for objects of class nn, typically produced by
        # | neuralnet. Computes the outputs of all neurons for specific 
        # | arbitrary covariate vectors given a trained neural network.
        # +------------------------------------------------------------------
        
        out <- compute(x = nnet,
                       covariate = pca$x[, 1:5])
        
        return(as.numeric(last(out$net.result)))
        
      } else if (classifier == 'logistic') {
        
        # +------------------------------------------------------------------
        # | glm is used to fit generalized linear models, specified by giving
        # | a symbolic description of the linear predictor and a description
        # | of the error distribution. glm returns an object of class 
        # | inheriting from "glm" which inherits from the class "lm".
        # +------------------------------------------------------------------
        
        model <- glm(formula = formula,
                     family = binomial(link = 'logit'),
                     data = cbind.data.frame(y, pca$x[, 1:5]))
        
        return(last(model$fitted.values))
        
      } else if (classifier == 'svm') {
        
        # +------------------------------------------------------------------
        # | This generic function tunes hyperparameters of statistical 
        # | methods using a grid search over supplied parameter ranges.
        # +------------------------------------------------------------------
        
        svm.tune <- tune(method = svm,
                         train.x = Minimum.point ~ .,
                         data = cbind.data.frame(y, pca$x[, 1:5]),
                         ranges = list(cost = 10 ^ (-1:2),
                                       gamma = c(.5, 1 ,2)))
        
        best.model.fitted <- svm.tune$best.model$fitted
        return(last(best.model.fitted))
      }
    },
    error = function(e){
      return(0)
    },
    finally = {}
  )
  return(out)
}
