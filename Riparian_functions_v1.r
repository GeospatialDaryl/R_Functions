# http://stackoverflow.com/questions/21815060/dplyr-how-to-use-group-by-inside-a-function
# For programming, group_by_ is the counterpart to group_by:
#         
#         library(dplyr)
# 
# mytable <- function(x, ...) x %>% group_by_(...) %>% summarise(n = n())
# mytable(iris, "Species")
# # or iris %>% mytable("Species")
# which gives:
#         
#         Species  n
# 1     setosa 50
# 2 versicolor 50
# 3  virginica 50
# Update At the time this was written dplyr used %.% which is what was originally used above but now %>% is favored so have changed above to that to keep this relevant.
# 
# Update 2 regroup is now deprecated, use group_by_ instead.
# 
# Update 3 group_by_(list(...)) now becomes group_by_(...) in new version of dplyr as per Roberto's comment.
# 
# Update 4 Added minor variation suggested in comments.

makeBarPlotByGroup <- function( iFactor, iGroup){
        # plot by Factor for each in Group
        colNum <- findThatColumn(dT, iGroup)
        vGroups <- as.character(unique(dT[,colNum]))
        print(vGroups)
        for ( grp in vGroups){
                #print(vGroups)
                print(grp)
                #flush.console()
                goodRows <- which(dT[,colNum] == grp  )
                this <- dT[goodRows, ]
                ggp <- ggplot(this, 
                               aes_string(deparse(substitute(iFactor)))) + geom_bar()
                print(ggp)
                          
                flush.console()
        }
}
#makeBarPlotByGroup("Species", "Ranch")
#makeBarPlotByGroup("Species", "stock_anal")
###############################################################

my_summarise2 <- function(df, expr) {
        expr <- enquo(expr)
        
        summarise(df, 
                  mean = mean(!!expr),
                  sum = sum(!!expr),
                  n = n()
        )
}
#                        dT, factor, groupo
superGrouper <- function(df, expr1, expr2 ){
        expr1 <- enquo(expr1)
        expr2 <- enquo(expr2)
        this <- filter(df, expr2)
        g <- ggplot( this, aes(expr1) )
}

makeGoodDF <- function(inputDF){
        # removes all columns with NA elements in them
        cNames <- names(inputDF)
        
        t1 <- apply( inputDF, 2, is.na)
        apply(t1, 2, sum) -> inputDF_NA
        
        cbind(cNames, inputDF_NA > 0) -> tblNA
        
        as.vector(which(inputDF_NA == 0)) -> indxGood
        inputDF[,indxGood] ->  outDF
        outDF
}



is.finite.data.frame <- function(obj){
        #http://stackoverflow.com/questions/8173094/how-to-check-a-data-frame-for-any-non-finite
            sapply(obj,FUN = function(x) all(is.finite(x)))
        }


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

willow.survival.cloud <- function(inputDF,
                                  survived = inputDF$status14,
                                  col = inputDF$Ranch ){
        g <- ggplot(inputDF, aes(x = Z, y = survived))
        g + geom_jitter(aes(color = col))
}
willow.survival.cloud.Stock <- function(inputDF,
                                  survived = inputDF$status14,
                                  col = inputDF$stock_anal ){
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

willow.testTrain <- function( inputDF, trainingProportion = 0.75 ){
        ## 75% of the sample size
        smp_size <- floor(trainingProportion * nrow(inputDF))
        
        ## set the seed to make your partition reproductible
        set.seed(123)
        train_ind <- sample(seq_len(nrow(inputDF)), size = smp_size)
        
        train <- inputDF[train_ind, ]
        test <- inputDF[-train_ind, ]
        return( list(train, test ) )
}

willow.logistic.regression1 <- function(inputDF){
        m1 <- glm(inputDF$status14 ~ inputDF$Z + inputDF$Ranch, 
                  family = binomial(link = "logit"),
                  data = inputDF)
        print(summary(m1))
        print(anova(m1, test = "Chisq"))
        return(m1)       
}

willow.logistic.regressionNS <- function(inputDF){
        m1 <- glm(inputDF$status14 ~ inputDF$Z + inputDF$NUTM, 
                  family = binomial(link = "logit"),
                  data = inputDF)
        print(summary(m1))
        print(anova(m1, test = "Chisq"))
        return(m1)       
}

willow.logistic.regression <- function(inputDF){
        m1 <- glm(inputDF$status14 ~ inputDF$Z, 
                  family = binomial(link = "logit"),
                  data = inputDF)
        print(summary(m1))
        print(anova(m1, test = "Chisq"))
        return(m1)       
}

willow.logistic.regressionOnlyNS <- function(inputDF){
        m1 <- glm(inputDF$status14 ~ inputDF$NUTM, 
                  family = binomial(link = "logit"),
                  data = inputDF)
        print(summary(m1))
        print(anova(m1, test = "Chisq"))
        return(m1)       
}

willow.logistic.regressionAll<- function(inputDF){
        m1 <- glm(inputDF$status14 ~ . , 
                  family = binomial(link = "logit"),
                  data = inputDF)
        print(summary(m1))
        print(anova(m1, test = "Chisq"))
        return(m1)       
}
willow.logistic.regression.Stock<- function(inputDF){
        m1 <- glm(inputDF$status14 ~ inputDF$stock_anal , 
                  family = binomial(link = "logit"),
                  data = inputDF)
        print(summary(m1))
        print(anova(m1, test = "Chisq"))
        return(m1)       
}

willow.violin <- function(inputDF, x , y ){
        g4 <- ggplot(dRWz, aes(x, y))
        g4 + geom_dotplot(binaxis = "y", binwidth = 0.1)
        g4 + geom_violin(scale = "area")
        #return(g4)
        
}