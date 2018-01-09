# --------------------------------------------
# INFORMATION
# --------------------------------------------
# This is a modification of the code on the NoiseFiltersR package of functions
# related to INFFC. The motivation behind this refactorization is to enable
# INFFC function to be used on a dataset with a very large number of variables.
# To achieve this, all 'formulas' should be removed, and algorithms that do not
# support a 'non-formula' usage should be traded by a equivalent from another
# package or, at last resource, reimplemented. The results are not necessarily 
# the same, but are quite similar.
# --------------------------------------------

personal.getKNN <- function(train, test, k = 7, classColumn = which(colnames(train) == 'class'), SCALE = TRUE) {
    train <- if (SCALE) scale(train[-classColumn]) else train[-classColumn]
    test <- if (SCALE) scale(test[-classColumn]) else test[-classColumn]

    knn <- matrix(nrow = nrow(test), ncol = k)

    for (i in 1:nrow(test)) {
        dist <- vector()
        for (j in 1:nrow(train)) {
            dist[j] <- sum((test[i, ] - train[j, ])^2.0)^0.5 
        }
        knn[i,] <- sort.list(dist)[2:(k + 1)]
    }

    res <- list()
    res$C <- knn

    return (res)
}

modified.Clean <- function (data, NoisyIndexes, k, index) 
{
    neighborsIndexes <- personal.getKNN(train = data[-index, 
        ], test = data[index, ], k = k)$C
    n <- sum(sapply(neighborsIndexes, function(i) {
        i %in% NoisyIndexes
    }))
    (k + ifelse(index %in% NoisyIndexes, 1, -1) * (n - k))/(2 * 
        k)
}


modified.Confidence <- function (data, NoisyIndexes, k, index) 
{
    t <- sum(sapply(NoisyIndexes, function(i) {
        index %in% personal.getKNN(train = data[-i, ], 
            test = data[i, ], k = k)$C
    }))
    1/sqrt(1 + t^2)
}


modified.NoiseScore <- function (data, NoisyIndexes, k, indexToScore) 
{
    neighborsIndexes <- personal.getKNN(train = data[-indexToScore, 
        ], test = data[indexToScore, ], k = k)$C
    sum(sapply(neighborsIndexes, function(i) {
        modified.Confidence(data, NoisyIndexes, k, i) * modified.Clean(data, NoisyIndexes, 
            k, i) * ifelse(data[i, ]$class == data[indexToScore, 
            ]$class, -1, 1)
    }))/k
}


modified.FusionClassifiers <- function (data, trainingIndexes, majThreshold, returnNoisy = FALSE) 
{
    classColumn <- which(colnames(data) == 'class')

    predC50 <- predict(C50::C5.0(
        x = data[trainingIndexes, -classColumn],
        y = data[trainingIndexes, classColumn]), data)

    pred3NN <- sapply(1:nrow(data), function(i) {
        class::knn(
            train = data[setdiff(trainingIndexes, i), -classColumn], 
            test = data[i, -classColumn], 
            cl = data[setdiff(trainingIndexes, i), classColumn], 
            k = 3)
    })
    
    # invisible(utils::capture.output(predLOG <- predict(nnet::multinom(
    #     class ~ ., 
    #     data[trainingIndexes, ]), 
    #     data)))

	predSVM <- predict(e1071::svm(
        x = data[trainingIndexes, -classColumn], 
        y = data[trainingIndexes, classColumn], scale = TRUE), data[-classColumn])
    
    # votes <- (predC50 != data$class) + (pred3NN != data$class) + (predLOG != data$class)
    votes <- (predC50 != data$class) + (pred3NN != data$class) + (predSVM != data$class)
    
    if (returnNoisy) {
        return(which(votes >= majThreshold))
    } else {
        return(which(votes < majThreshold))
    }
}

modified.INFFC <- function (x, consensus = FALSE, p = 0.01, s = 3, k = 5, threshold = 0, 
    classColumn = ncol(x), ...) 
{
    if (!is.data.frame(x)) {
        stop("data argument must be a data.frame")
    }
    if (!classColumn %in% (1:ncol(x))) {
        stop("class column out of range")
    }
    if (!is.factor(x[, classColumn])) {
        stop("class column of data must be a factor")
    }
    origSize <- nrow(x)
    namesOrig <- names(x)
    rownamesOrig <- attr(x, "row.names")
    names(x)[classColumn] <- "class"
    if (any(names(x)[-classColumn] == "class")) {
        v <- names(x)[-classColumn]
        v[v == "class"] <- paste("classss", 1:sum(v == "class"), 
            sep = "")
        names(x)[-classColumn] <- v
    }
    row.names(x) <- 1:nrow(x)
    if (consensus) 
        majThreshold <- 3
    else majThreshold <- 2
    stopThreshold <- floor(origSize * p)
    KeepOn <- TRUE
    counter <- 0
    countIter <- 0
    while (KeepOn) {
        countIter <- countIter + 1
        PreFiltIndexes <- modified.FusionClassifiers(x, trainingIndexes = 1:nrow(x), 
            majThreshold)
        NoisyIndexes <- modified.FusionClassifiers(x, trainingIndexes = PreFiltIndexes, 
            majThreshold, returnNoisy = TRUE)
        scores <- sapply(NoisyIndexes, function(i) {
            modified.NoiseScore(x, NoisyIndexes, k, i)
        })
        IndexesToRemove <- NoisyIndexes[which(scores > threshold)]
        if (length(IndexesToRemove) > 0) {
            x <- x[-IndexesToRemove, ]
        }
        if (length(IndexesToRemove) <= stopThreshold & counter + 
            1 == s) 
            KeepOn <- FALSE
        if (length(IndexesToRemove) <= stopThreshold & counter + 
            1 < s) 
            counter <- counter + 1
        if (length(IndexesToRemove) > stopThreshold) 
            counter <- 0
        message("Iteration ", countIter, ": ", length(IndexesToRemove), 
            " noisy instances removed \n")
    }
    remIdx <- setdiff(1:origSize, as.integer(row.names(x)))
    names(x) <- namesOrig
    row.names(x) <- rownamesOrig[as.integer(row.names(x))]
    cleanData <- x
    repIdx <- NULL
    repLab <- NULL
    parameters <- list(consensus = consensus, p = p, s = s, k = k, 
        threshold = threshold)
    call <- match.call()
    call[[1]] <- as.name("INFFC")
    ret <- list(cleanData = cleanData, remIdx = remIdx, repIdx = repIdx, 
        repLab = repLab, parameters = parameters, call = call, 
        extraInf = NULL)
    class(ret) <- "filter"
    return(ret)
}