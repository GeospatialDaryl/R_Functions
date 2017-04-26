
':=' <- function(lhs, rhs) {
        # http://stackoverflow.com/questions/1826519/how-to-assign-from-a-function-which-returns-more-than-one-value
        frame <- parent.frame()
        lhs <- as.list(substitute(lhs))
        if (length(lhs) > 1)
                lhs <- lhs[-1]
        if (length(lhs) == 1) {
                do.call(`=`, list(lhs[[1]], rhs), envir=frame)
                return(invisible(NULL)) 
        }
        if (is.function(rhs) || is(rhs, 'formula'))
                rhs <- list(rhs)
        if (length(lhs) > length(rhs))
                rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
        for (i in 1:length(lhs))
                do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
        return(invisible(NULL)) 
}

willow.survival.cloud <- function(inputDF, survived = inputDF$status14, col = inputDF$Ranch ){
        g <- ggplot(inputDF, aes(x = Z, y = survived))
        g + geom_jitter(aes(color = col))
}

willow.summary <- function(inputDF){
        library(Amelia)
        missmap(inputDF, main = "Missing values vs observed")
}

willow.examples <- function(inputDF, testP = "b14"){
        print(shapiro.test(inputDF$Z))
        qqnorm(inputDF$Z)
}

willow.logistic.regression <- function( inputDF, trainingProportion = 0.75 ){
        ## 75% of the sample size
        smp_size <- floor(trainingProportion * nrow(inputDF))
        
        ## set the seed to make your partition reproductible
        set.seed(123)
        train_ind <- sample(seq_len(nrow(inputDF)), size = smp_size)
        
        train <- inputDF[train_ind, ]
        test <- inputDF[-train_ind, ]
        return( list(train, test ) )
}
