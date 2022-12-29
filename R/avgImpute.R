#### **** Helper functions - getNearestObs **** ####

#' @keywords internal
#' @author Daniel Hoop
.lastOfFirst <- function(x) {
  last <- lapply(x, function(x) x[1])
  if (length(last) > 1) {
    last <- lapply(last, function(x) x + 1)
    last[[length(last)]] <- last[[length(last)]] - 1
  }
  return(last)
}

#' @keywords internal
#' @author Daniel Hoop
.lastOfLast <- function(x) {
  last <- x[[length(x)]]
  last <- last[-length(last)] + length(last)
  return(list(last))
}

#' @keywords internal
#' @author Daniel Hoop
.distIndexOfElement_nothingGiven <- function(i, n) {
  j <- 1
  a <- list(1:(n-1))
  if (i == 1)
    return(a)
  for (j in 2:i)
    a <- c(.lastOfFirst(a), .lastOfLast(a))
  return(a)
}

#' @keywords internal
#' @author Daniel Hoop
.distIndexOfElement_indexGiven <- function(i, givenIndex) {
  a <- givenIndex
  if (length(givenIndex) == i)
    return(givenIndex)
  for (j in (length(givenIndex)+1):i)
    a <- c(.lastOfFirst(a), .lastOfLast(a))
  return(a)
}
# if (FALSE) {
#   # Test helper functions...
#   indx <- .distIndexOfElement_nothingGiven(1, 7); indx
#   indx <- .distIndexOfElement_indexGiven(2, indx); indx
#   indx <- .distIndexOfElement_indexGiven(3, indx); indx
#   indx <- .distIndexOfElement_indexGiven(4, indx); indx
# }

#' A function to convert ind to ids. Helper used in `getNearestObs`.
#' @keywords internal
#' @author Daniel Hoop
#' @examples
#' ind <- matrix(sample(1:10, 20, replace = TRUE), ncol = 2)
#' id <- letters[1:10]
#' .getIdFromInd(ind, id)
.getIdFromInd <- function(ind, id) {
  apply(ind, 2, function(x) {
    if (all(is.na(x)))
      return(x)
    return(id[x])
  })
}

#' @export
#' @title Get the nearest observations in n-dimensional vector space
#' @description Finds the nearest observations in \code{dataGet} that match most closely the observations in \code{data}.
#' @author Daniel Hoop
#'
#' @param data The data.frame containing the observations for which other observations should be found.
#' @param dataGet The data.frame containing the observations from which the clostest matching observations should be drawn.
#' @param cols Optional: The columns in \code{data} and \code{dataGet} to conduct the matching.
#' @param indexCols Optional: If given, then only observations will be matched from \code{dataGet} to \code{data}, when the value in \code{dataGet[, indexCols]} and \code{data[, indexCols]} are identical.
#' @param idCol Optional: The colname in `dataGet` that contains unique identifiers. If given, then an additional list place named "id" will be returned in the result containing the ids of the matched observations.
#' @param maxDist Optional (see details): The maximum distance between observations.
#' @param maxObs Optional (see details): The maximum number of observations that should be drawn in \code{dataGet} for each observation in \code{data}.
#' @param minObs Optional (see details): The minimum number of observations that should be drawn in \code{dataGet} for each observation in \code{data}.
#' @param distFunc The function to be applied in order to calculate the distance between all observations in \code{data}. Default is the function stats::dist.\cr
#' @param scale Logical value indicating if the data should be scaled before doing the calculations.
#' @param warnScaling Logical value indicating if a warning should be issued when the variables are on very different scale.
#' You can change the distance metric, e.g. to \code{'manhattan'} by setting \code{distFunc = function (x) \link[stats:dist]{stats::dist}(x, method='manhattan')}.
#' Or you could use the inverse of the cosine similarity as distance: \code{distFunc = function (x) 1/\link[coop:cosine]{coop::cosine}(t(x))}.
#'
#' @details If \code{maxDist} is not specified, then at least \code{minObs} must be given.\cr
#' If \code{minObs >= nrow(dataGet)}, then an error is thrown, because in that case each observation in dataGet would be chosen and the matching wouldn't have any effect at all.
#'
#' @return A list with two entries named 'ind' and 'dist'. Both list entries are a matrix of size \code{nrow(data) * nObs}.\cr
#' Each row in 'ind' contains the indices of the observations to be taken from \code{dataGet}, in ascending order, according to the distance.\cr
#' Each row in 'dist' contains the distances to the observations to be taken from \code{dataGet}, in ascending order.\cr
#' If not the same number of observations was chosen for each observation in \code{data}, then the rest of the columns in each for of 'ind' and 'dist' is filled with NA values.
#'
#' @examples data <- as.data.frame(matrix(rnorm(100), ncol=5))
#' dataGet <- as.data.frame(matrix(rnorm(1000), ncol=5))
#' getNearestObs(data = data, dataGet = dataGet, cols = c("V1", "V2"), maxDist = 3, minObs = 10, maxObs = 20, scale = TRUE, distFunc = function (x) stats::dist(x, method='manhattan'))
getNearestObs <- function (data, dataGet, cols = NULL, indexCols = NULL, idCol = NULL, maxDist = NULL, maxObs = NULL, minObs = NULL, distFunc = stats::dist, scale = FALSE, warnScaling = TRUE) {

  # Check that cols is also specified when either indexCols or idCol is.
  if (!is.null(indexCols) && is.null(cols))
    stop("If `indexCols` is specified, then also `cols` has to be specified.")
  if (!is.null(idCol) && is.null(cols))
    stop("If `idCol` is specified, then also `cols` has to be specified.")
  if (is.null(dim(data)) || is.null(dim(dataGet)))
    stop("Both, `data` and `dataGet` must be a data.frame or a matrix.")

  # Recursion if indexCols were given.
  if (!is.null(indexCols)) {
    missing <- indexCols[!indexCols %in% colnames(data)]
    if (length(missing) > 0)
      stop("The following columns specified in `indexCols` are missing in `data`: ", paste0(missing, collapse = ", "))
    missing <- indexCols[!indexCols %in% colnames(dataGet)]
    if (length(missing) > 0)
      stop("The following columns specified in `indexCols` are missing in `dataGet`: ", paste0(missing, collapse = ", "))
    lapply(indexCols, function(col) {
      if (any(is.na(data[, col])))
        stop("There must be no NA values in the `data[, indexCol]`.")
      if (any(is.na(dataGet[, col])))
        stop("There must be no NA values in the `dataGet[, indexCol]`.")
    })

    if (!is.data.frame(data))
      data <- data.frame(data, stringsAsFactors = FALSE)
    if (!is.data.frame(dataGet))
      dataGet <- data.frame(dataGet, stringsAsFactors = FALSE)

    rowCol <- "id_nr8YUPJwqrMd"
    idxCol <- "indx_nr8YUPJwqrMd"
    data   [, c(rowCol, idxCol)] <- data.frame(1:nrow(data),    paste.cols(data   [, indexCols, drop = FALSE]), stringsAsFactors = FALSE)
    dataGet[, c(rowCol, idxCol)] <- data.frame(1:nrow(dataGet), paste.cols(dataGet[, indexCols, drop = FALSE]), stringsAsFactors = FALSE)

    # Loop through all indexes
    resTmp <- lapply(sort(unique(data[, idxCol])), function(indx) {
      dataFilt <-    data   [, idxCol] == indx
      dataGetFilt <- dataGet[, idxCol] == indx
      res <-  getNearestObs(
        data    = data   [dataFilt, ],
        dataGet = dataGet[dataGetFilt, ],
        cols = cols, indexCols = NULL, idCol = idCol, maxDist = maxDist, maxObs = maxObs, minObs = minObs, distFunc = distFunc, scale = scale, warnScaling = warnScaling)
      row <-    data   [dataFilt,    rowCol]
      rowGet <- dataGet[dataGetFilt, rowCol]
      res[["ind"]] <- apply(res[["ind"]], 2, function(ind) rowGet[ind])
      res[["row"]] <- row
      return(res)
    })

    # Restore the order of matched observations, then return the result.
    row <- do.call("c", lapply(resTmp, function(x) x[["row"]]))
    ord <- order(row)

    result <- list(
      ind = do.call("rbind", lapply(resTmp, function(x) x[["ind"]]))[ord, , drop = FALSE],
      dist = do.call("rbind", lapply(resTmp, function(x) x[["dist"]]))[ord, , drop = FALSE]
    )
    if (!is.null(idCol))
      result[["id"]] <- do.call("rbind", lapply(resTmp, function(x) x[["id"]]))[ord, , drop = FALSE]

    return(result)
  }

  # Error checks
  if (!is.null(idCol)) {
    if (length(idCol) != 1)
      stop("`idCol` must be a character of length 1 naming the column name in `dataGet` that contains unique identifiers for the observations.")
    missing <- idCol[!idCol %in% colnames(dataGet)]
    if (length(missing) > 0)
      stop("The colname specified in `idCol` is missing in the colnames of `dataGet`: ", paste0(missing, collapse = ", "))
    idGet <- dataGet[, idCol]
  }
  if (!is.null(cols)) {
    missing <- cols[!cols %in% colnames(data)]
    if (length(missing) > 0)
      stop("The following columns specified in `cols` are missing in `data`: ", paste0(missing, collapse = ", "))
    missing <- cols[!cols %in% colnames(dataGet)]
    if (length(missing) > 0)
      stop("The following columns specified in `cols` are missing in `dataGet`: ", paste0(missing, collapse = ", "))
    data <- data[, cols, drop = FALSE]
    dataGet <- dataGet[, cols, drop = FALSE]
  }
  if (ncol(data) != ncol(dataGet) || any(colnames(data) != colnames(dataGet)))
    stop ("Unless you specify `cols`, `data` and `dataGet` must both be data.frames with the same number of columns and the same colnames.")
  if (!is.null(minObs) && minObs >= nrow(dataGet))
    warning ("`minObs` must be smaller than `nrow(dataGet)`. Otherwise all observations in dataGet would be chosen, and thus, this function would be useless. If `getNearstObs` was called from `imputeDomainDriven` with specified argument `lookupIndexNames`, then keep in mind that some of your subgroups may be smaller than `nrow(dataGet)` which may cause this warning.")
  if (is.null(maxDist) && is.null(minObs))
    stop ("Either `maxDist` or `minObs` must be specified.")
  if (is.null(maxDist) && !is.null(minObs) && !is.null(maxObs))
    stop ("When `maxDist` is NULL, then only `minObs` should be specified.")
  if (is.null(maxObs))
    maxObs <- Inf
  if (is.null(minObs))
    minObs <- -1
  if (is.na(maxObs))
    stop ("`maxObs` must not be NA.")
  if (is.na(minObs))
    stop ("`minObs` must not be NA.")
  if (minObs >= maxObs)
    stop ("`minObs` must be smaller than `maxObs`.")

  # Distance Calculations...

  # Merge the data and create indices to be used later.
  dataGet <- rbind(data, dataGet)
  if (scale)
    dataGet <- base::scale(dataGet)
  # Give hint when variables are on very different scales.
  if (!scale && warnScaling && ncol(dataGet) > 1) {
    means <- colMeans(dataGet[sample(1:nrow(dataGet), min(nrow(dataGet), 200)), , drop = FALSE], na.rm = TRUE)
    combs <- combn(means, m = 2)
    doWarn <- FALSE
    for (i in 1:ncol(combs)) {
      if (abs(combs[1, i]) > 0.05 &&
          abs(combs[1, i] - combs[2, i]) > 0.1 &&
          (abs(combs[1, i]) / abs(combs[2, i]) > 5 || abs(combs[1, i]) / abs(combs[2, i]) < 1/5))
        doWarn <- TRUE
    }
    if (doWarn)
      warning("Some variables are on different scales (more than factor 5 difference in the average values). Consider rescaling (e.g., using `scale`).")
  }

  # Calc distances, and for each obs in 'dataGet`...
  d <- distFunc(dataGet)
  # Check that the vector has the right length.
  requiredLengthOfD <- sum(1:(nrow(dataGet)-1))
  if (mode(d) != "numeric")
    stop("The value returned by `distFunc` must be of mode 'numeric'.")
  if (length(d) != requiredLengthOfD)
    stop("The value returned by `distFunc` must have the following length: sum(1:((nrow(data) + nrow(dataGet))-1))\n",
         "This is the standard that is returned by `stats::dist`. Please assure that the chosen `distFunc` returns the same number of distances in the same order as `stats::dist(..., diag = FALSE, upper = FALSE)`.\n",
         "With the given data, `distFunc` should return ", requiredLengthOfD, " distances. However, the chosen `distFunc` returns ", length(d), " distances.")

  # Set distances to observations in `data` to Inf.
  # Because we do not want to find similar observations from there...
  if (nrow(data) > 1) {
    setInf <- list()
    indx <- .distIndexOfElement_nothingGiven(1, nrow(dataGet))
    setInf[[1]] <- unlist(indx)[1:(nrow(data)-1)]
    if (nrow(data) > 2) {
      for(i in 2:(nrow(data)-1)) {
        indx <- .distIndexOfElement_indexGiven(i, indx)
        setInf[[i]] <- indx[[length(indx)]][1:(nrow(data)-i)]
      }
    }
    d[unlist(setInf)] <- Inf
  }

  # Get the observations with the smallest distance
  # In the very end, +1 must be added to the chosen indices because in the result of dist(), the first element is skipped.
  # Also, `nrow(data)` must be subtracted from the indices because we want the indices from `dataGet`.
  resultInd <- list()
  resultDist <- list()

  # Case when maxDist is the criterion.
  if (!is.null(maxDist)) {
    for (i in 1:nrow(data)) {
      if (i == 1) {
        indx <- .distIndexOfElement_nothingGiven(1, nrow(dataGet))
      } else {
        indx <- .distIndexOfElement_indexGiven(i, indx)
      }
      dTmp <- d[unlist(indx)]

      choose <- which(dTmp < maxDist)
      if (length(choose) > maxObs) {
        choose <- order(dTmp)[1:maxObs]
      } else if (length(choose) < minObs) {
        choose <- order(dTmp)[1:minObs]
      }
      choose <- choose[order(dTmp[choose])]

      resultInd[[i]] <- choose
      resultDist[[i]] <- dTmp[choose]
    }
    maxLength <- min(max(sapply(resultInd,  length)), maxObs)
    makeEqualLength <- function(x) if (length(x) == maxLength) x else c(x, rep(NA_real_, maxLength - length(x)))

    res <- list(
      ind = (1 - nrow(data)) + do.call("rbind", lapply(resultInd, makeEqualLength)),
      dist = do.call("rbind", lapply(resultDist, makeEqualLength))
    )

    # Make a matrix with 1 NA column, in case no observations where found,
    # in order to avoid problems in calling code.
    if (ncol(res$ind) == 0) {
      res <- list(
        ind = matrix(NA, nrow = length(resultInd)),
        dist = matrix(NA, nrow = length(resultInd)))
    }

    if (!is.null(idCol))
      res[["id"]] <- .getIdFromInd(ind = res$ind, id = idGet)

    return(res)

    # Case when minObs is the criterion.
  } else {

    for (i in 1:nrow(data)) {
      if (i == 1) {
        indx <- .distIndexOfElement_nothingGiven(1, nrow(dataGet))
      } else {
        indx <- .distIndexOfElement_indexGiven(i, indx)
      }
      dTmp <- d[unlist(indx)]
      choose <- order(dTmp)[1:minObs]
      resultInd[[i]] <- choose
      resultDist[[i]] <- dTmp[choose]
    }

    res <- list(
      ind = (1 - nrow(data)) + do.call("rbind", resultInd),
      dist = do.call("rbind", resultDist))

    if (!is.null(idCol))
      res[["id"]] <- .getIdFromInd(ind = res$ind, id = idGet)

    return(res)
  }
}

# # Test getNearestObs()
if (FALSE) {
  similarityCols <- c("ha_LN", "ha_OAF")
  spe <- zaUtils::load.spe(withGbVars = TRUE)
  data = spe[1:100, similarityCols]
  dataGet = spe[51:150, similarityCols]
  maxDist = 2
  maxObs = 5
  minObs = 2
  distFunc = function(x) stats::dist(x)

  #filtSpe <- spe[, "TypS3"] == 1512 & spe[, "Jahr"] == 2016
  #filtSpb <- spb[, "Jahr"] == 2016
  system.time({
    res <- getNearestObs(data = spe[1:100, similarityCols],
                         dataGet = spe[101:102, similarityCols],
                         maxDist = 0,
                         minObs = 0,
                         maxObs = 200,
                         distFunc = function(x) stats::dist(x))
  })
}


#### **** Helper functions - overlapping **** ####

#' @keywords internal
#' @author Daniel Hoop
.prepareExpression <- function (expr, argName) {
  if (is.expression(expr))
    stop (paste0("The argument '", argName, "' must not be an expression but quoted. Use something along the lines of:\n", argName, " = quote({\n  a <- 1\n  b <- 2\n})"))
  expr0 <- as.character(expr)
  expr0 <- if (length(expr0) > 0 && expr0[1] == "{") expr0[-1] else expr0
  expr0 <- if (length(expr0) > 0 && expr0[length(expr0)] == "}") expr0[-length(expr0)] else expr0
  # The next step is necessary, because inside if statements if(a == b) { a <- 1; b <- 2 }, "\n" characters are included, but these lines will not be in seperate vector places.
  expr0 <- unlist(strsplit(expr0, "\n"))
  expr0 <- expr0[expr0 != ""]
  return (expr0)
}

#' @keywords internal
#' @author Daniel Hoop
.checkIndexConsistency <- function (indices, indexOrder) {
  # Assure that indices is list.
  if (!is.list(indices))
    stop ("'indices' must be a list. Hint: If you call 1 column from a data.frame use drop=FALSE.", call.=FALSE)
  if (!is.list(indexOrder))
    indexOrder <- list(indexOrder)
  allIndices <- unique(unlist(indexOrder))

  # Check that all indices given in 'allIndices' are contained in 'indices'.
  notContained <- allIndices[!allIndices%in%names(indices)]
  if (length(notContained)>0)
    stop (paste0("All indices given in 'indexOrder' must exist in 'indices'. This indices are given in 'indexOrder' but missing in 'indices': ", paste0(notContained, collapse=", ")), call.=FALSE)
  rm(notContained)

  # Check that no index in 'indices' is NA.
  indexHasNA <- sapply(indices[allIndices], function(x)any(is.na(x)))
  indexHasNA <- indexHasNA[indexHasNA]
  if (length(indexHasNA) > 0)
    stop (paste0("There must be no NA values in the entries of indices. This indices contain NA values: ", paste0(names(indexHasNA), collapse=", ")), call.=FALSE)
}

#' @export
#' @title Convert distances to weights
#' @description Package default function to convert distances to weights. These weights are then used to calculate a weighted average of all similar observations, which is subsequently used for the imputation.
#' You may implement your own function and use it instead.
#' @author Daniel Hoop
#' @param dist A matrix containing the pairwise distances between the observations.
#' @param ... Not used but needed such that the same parameters can be passed into different functions such as \code{\link{createLookupWithMeanMethod}} and \code{\link{distToWeight}}.
#' @return A matrix with equal dimensions like \code{dist}, containing the weights.
distToWeight <- function (dist, ...) {
  # Converts distances to weights.
  #
  # Max weights to 1e9, then take logarithm. The weight range will shrink extremely due to log.
  # Try this: log(1 + c(1e308, .Machine$integer.max, 1e9, 10000, 300, 1, 0.1))
  weights <- 1 / dist
  maxWeight <- 1e9
  weights[weights == Inf] <- maxWeight
  weights <- log(1 + weights) # + 1 to avoid negative log values.
  return (weights)
}

# ref <- data.frame(geb_afa=c(1,1,1), geb_tot=c(2,2,2)); lookup(geb_afa/geb_tot, ref)
#' @export
#' @title Create the lookup function
#' @description Creates the lookup function to be used in the argument \code{expr} of the function \code{\link{imputeDomainDriven}}.
#' @author Daniel Hoop
#' @param ref The reference data set from which the average values should be derived (data.frame).
#' @param weights The weights for the observations given in the reference data set (numeric). If \code{is.null(weights)}, then all observations are weighted equally.
#' @param ... Not used but needed such that the same parameters can be passed into different functions such as \code{\link{createLookupWithMeanMethod}} and \code{\link{distToWeight}}.
#' @return The lookup function which will have the following arguments:\cr
#' \code{na.rm} Logical value indicating if na.rm should be applied when calculating the mean value of the reference group.\cr
#' \code{zeroToNaBeforeCalc} Logical value indicating if zero values should be turned to NA before calculating the mean of the reference group.\cr
#' \code{zeroToNaBeforeCalc} Logical value indicating if NA values should be turned to 0, when the result is returned.\cr
#' \code{getRefForDebug} Logical value indicating the whole reference dataset should be returned instead of a calculated average values.
#' This is useful if you are browsing the calculation expression and want to check how the reference dataset looks for a specific observation.
createLookupWithMeanMethod <- function(ref, weights=NULL, type = c("mean", "median"), ...) {
  # Creates a lookup funciton with given reference data.
  # This way, the reference data does not have to be passed as argument each function call.
  #
  type <- match.arg(type)
  if (is.null(weights))
    weights <- rep(1, nrow(ref))
  if (length(weights) != nrow(ref))
    stop ("length(weights) must be equal to nrow(ref)")

  return (function (expr, na.rm=TRUE, zeroToNaBeforeCalc=FALSE, naToZeroInOutput=FALSE, getRefForDebug=FALSE) {
    # The lookup function looks up values in the reference dataset.
    # Mean values from the reference dataset (e.g. most similar observations) will be calculated.
    # The given expression 'expr' will be evaluated, using the means, then the resulting value will be returned.
    # https://stackoverflow.com/questions/46834655/whats-the-difference-between-substitute-and-quote-in-r
    if (getRefForDebug)
      return (ref)
    subExpr <- substitute(expr)
    vars <- extract.I.vars(
      deparse(subExpr), keep.original = TRUE, original.single.vars.only = TRUE)
    missing <- vars[!vars%in%colnames(ref)]
    if (length(missing) > 0)
      stop (paste0("object(s) ", paste0(missing, collapse=", ")," not found when trying to lookup the value in `dataFill`."))
    if (zeroToNaBeforeCalc) {
      ref[vars] <- lapply(ref[vars], function (x) { x[x == 0] <- NA; return (x) })
    }
    if (type == "mean") {
      means <- mean.weight(ref, cols=vars, weights=weights, na.rm=na.rm)
    } else if (type == "median") {
      means <- median.weight(ref, cols=vars, weights=weights, na.rm=na.rm)
    }
    if (naToZeroInOutput) {
      means[is.na(means)] <- 0
    }
    res <- eval(subExpr, envir=as.list(means)) # eval(parse(text=depSubExpr), envir=as.list(means))
    return (res)
  })
}


#' Combine quoted expressions
#' @export
#' @author Daniel Hoop
#' @param ... Either all quoted expressions that should be combined or a list containing all of them.
#' @return A quoted expressions that contains all expressions given in \code{...}.
#' @examples # All given examples yield the same result.
#' combineQuotes(
#'   quote({a <- 1})
#'   quote({b <- 2})
#'   quote({c <- 3}))
#' combineQuotes(
#'   list(quote({a <- 1}),
#'        quote({b <- 2}),
#'        quote({c <- 3})))
#' combineQuotes(
#'   quote({a <- 1}),
#'   c("b <- 2"
#'     quote({c <- 3})))
combineQuotes <- function (...) {
  input <- unlist(list(...))
  return (eval(parse(text=paste0(c(
    "quote({",
    sapply(input, function (x) quoteToString(x, collapseTxt = TRUE)),
    "})"
  ), collapse="\n"))))
}

#' Converts a quoted expression to a string.
#' @author Daniel Hoop
#' @keywords internal
#' @param x The quoted expression to convert into a string.
#' @param collapseTxt Logical value indicating if the resulting character vector should be collapsed with newline characters \code{"\n"}.
quoteToString <- function (x, collapseTxt = TRUE) {
  # Check for length 0 has to be done also here, otherwise the resulting expression will contain the character string "character(0)".
  if (length(x) == 0)
    return ("")
  if (is.character(x))
    return (x)
  if (class(x) == "function")
    return (deparse(x))
  x <- as.character(x)
  if (x[1] == "{")
    x <- x[-1]
  if (length(x) == 0)
    return ("")
  if (x[length(x)] == "}")
    x <- x[-length(x)]
  if (collapseTxt)
    return (paste0(x, collapse="\n"))
  return (x)
}

#' Replace values given in \code{search} with values given in \code{replace} inside the vector \code{x}.
#' @keywords internal
replace.values <- function(search, replace, x, no.match.NA=FALSE, gsub=FALSE, fixed=FALSE){
  # This function replaces all elements in argument search with the corresponding elements in argument replace.
  # Replacement is done in x.
  # if no.match.NA=TRUE, values that could not be replaced will be NA instead of the old value.

  if(length(search)!=length(replace)) stop("length(search) must be equal length(replace)")
  if(any(duplicated(search))) stop("There must be no duplicated entries in search.")

  if(is.matrix(x)){
    return(apply(x,2,function(x)replace.values(search=search, replace=replace, x=x, no.match.NA=no.match.NA, gsub=gsub, fixed=fixed)))
  } else if(is.data.frame(x)){
    return(as.data.frame(lapply(x,function(x)replace.values(search=search, replace=replace, x=x, no.match.NA=no.match.NA, gsub=gsub, fixed=fixed)),stringsAsFactors=FALSE))
  }

  if (is.factor(search) || is.factor(replace))
    stop ("arguments 'search' and 'replace' must not contain factors.")
  isFac <- is.factor(x)
  if (isFac) {
    lvl <- levels(x)
    x <- as.character(x)
  }

  xnew <- x
  if(!gsub) {
    m1 <- match(x, search)
    xnew <- replace[ m1 ]
    if(!no.match.NA){
      isna <- is.na(m1)
      xnew[isna] <- x[isna]
    }
  } else {
    # Hier erst nach Laenge der Strings ordnen, damit lange Teilstrings vor kurzen ersetzt werden.
    ord <- order(nchar(search),decreasing=TRUE)
    search <- search[ord]
    replace <- replace[ord]
    for(i in 1:length(search)){
      xnew <- gsub(search[i],replace[i],xnew, fixed=fixed)
    }
  }
  if (isFac) {
    xnew <- factor(xnew, levels=lvl)
  }
  return(xnew)
}

#' Change character columns to numeric/integer, if they can be coerced as such.
#' @author Daniel Hoop
#' @keywords internal
#' @param x The data.frame/matrix to be converted.
#' @param stringsAsFactors Logical value indicating if character vectors should be converted to factors. Default is \code{FALSE}.
#' @return A data.frame with same dimensions as \code{x}. Colnames will be equal but rownames might have changed.
charColsToNum <- function(x, stringsAsFactors=FALSE){
  # This function checks in all cols of a data.frame if they can be coerced to numeric without producing NA values.
  # If it's possible the col is coerced to numeric with as.numeric()

  if (is.matrix(x)) {
    rownames(x) <- NULL
    x <- as.data.frame(x, stringsAsFactors=stringsAsFactors)
  }
  if (!is.data.frame(x))
    stop ("x must be a data.frame")
  if (ncol(x) == 0)
    return (x)

  rn <- rownames(x)
  cn <- colnames(x)
  x <- as.data.frame(lapply(x, function(x) {
    if (is.character(x))
      type.convert(x, as.is = !stringsAsFactors)
    else
      x
  }),
  stringsAsFactors=stringsAsFactors)
  rownames(x) <- rn
  colnames(x) <- cn
  return(x)
}


#' @title Multicore preparations
#' @description Installs required packages, registers a parallel cluster and returns it as value, if on Windows operating systems.
#' @author Daniel Hoop
#' @param nCores The number of cores to register.
#' @keywords internal
multiCorePrep <- function (nCores) {
  onWindows <- grepl("window", Sys.info()["sysname"], ignore.case=TRUE)
  nCores <- max(c(1, nCores[1]))
  if (nCores > 1) {
    if (onWindows) {
      installFromCRAN(c("doSNOW", "plyr"))
      parClust <- snow::makeCluster(nCores)
      doSNOW::registerDoSNOW(parClust)
      return (parClust)
    } else {
      installFromCRAN(c("doMC", "plyr"))
      doMC::registerDoMC(cores=nCores)
      return (NULL)
    }
  }
}
#' @title Multicore post operations
#' @description Stops the parallel cluster given in optional argument \code{parClust} (if on Windows operating systems).
#' @author Daniel Hoop
#' @keywords internal
multiCorePost <- function (parClust = NULL) {
  if (!is.null(parClust)) {
    snow::stopCluster(parClust)
  }
}

#### **** Fuction to find circular references **** ####

#' Escapes characters that have special meaning in regular expressions with backslashes.
#' @keywords internal
#' @author Daniel Hoop
#' @param x Character vector containing the strings that should be escaped.
#' @examples
#' escapeStr(c("asdf.asdf", ".asdf", "\\.asdf", "asdf\\.asdf") )
escapeStr <- function(x) {
  if (!is.character(x))
    stop("x must be a character vector.")
  regChars <- c(".","|","(",")","[","]","{","}","^","$","*","+","-","?")
  res <- apply(matrix(x), 1, function(y) {
    if (nchar(y)==1 && y%in%regChars)
      return (paste0("\\", y))
    for (i in nchar(y):2) {
      if ( substr(y, i-1, i-1) != "\\" && substr(y, i, i) %in% regChars )
        y <- paste0(substr(y, 1, i-1), "\\", substr(y, i, nchar(y)))
    }
    if (substring(y, 1, 1) %in% regChars)
      y <- paste0("\\", y)
    return (y)
  })
  return (res)
}

#' Splits an expression where operators occur, but not inside brackets.
#' @keywords internal
#' @author Daniel Hoop
#' @examples
#' .splitExceptInBracket("(a+b+c)+1++1+")
#' [1] "(a+b+c)" "1"       ""        "1"
#' .splitExceptInBracket("a+b", splitIfElseOnly=TRUE)
#' [1] "a+b"
#' .splitExceptInBracket("1 <- if (a == b) c else d", splitIfElseOnly=TRUE)
#' [1] "1 <- if (a == b)" "c"                "d"
#' .splitExceptInBracket(c("a = b+b", "a <-b+b"), operators = c("=", "<-"))
#' [[1]]
#' [1] "a"   "b+b"
#' [[2]]
#' [1] "a"   "b+b"
#' .splitExceptInBracket("a <= b", operators = c("<-", "="), notAroundOperator = c("<", ">", "!", "="))
.splitExceptInBracket <- function (txt,
                                   operators = NULL,
                                   notAroundOperator = NULL,
                                   ifIsAnOperator = TRUE,
                                   splitIfElseOnly = FALSE) {

  if (length(txt)>1)
    return (lapply(as.list(txt), .splitExceptInBracket,
                   operators = operators,
                   notAroundOperator = notAroundOperator,
                   ifIsAnOperator = ifIsAnOperator,
                   splitIfElseOnly = splitIfElseOnly))

  if (length(txt) == 0 || txt == "")
    return (txt)
  if (splitIfElseOnly) {
    op <- "else"
  } else {
    if (is.null(operators)) {
      op <- c("*", "-", "+", "/", "^", "=", "!", "<", ">", "%%", "} else {", "}else{", "} else ", " else ")
      opRegex <- "\\*|\\-|\\+|/|\\^|=|<|>|%%|(\n| |\\})+else(\\{| |\n)+"
    } else {
      op <- operators
      opRegex <- paste0(escapeStr(operators), collapse = "|")
    }
    if (is.null(notAroundOperator) && !grepl("\\(|\\)|\\[|\\]", txt))
      return (trimws(strsplit(txt, opRegex)[[1]]))
  }

  bBal <- 0 # Bracket balance
  parts <- character() # Splitted result
  lastSplit <- 1 # Index of last splitted operator
  hasIfOccured <- hasIfBracketOpened <- hasIfBracketClosed <- FALSE
  rest <- txt
  for(i in 1:nchar(txt)) {
    st <- substr(txt, i, i)
    restMin1 <- rest
    rest <- substring(txt, i)
    if (st == "(" || st == "[") bBal <- bBal + 1
    if (st == ")" || st == "]") bBal <- bBal - 1
    if (hasIfOccured && bBal == 1)
      hasIfBracketOpened <- TRUE
    if (bBal == 0) {
      hasIfOccured <- ifIsAnOperator && (hasIfOccured || grepl("if( |\n)*\\(", rest))
      if (hasIfBracketOpened)
        hasIfBracketClosed <- TRUE
      if (hasIfBracketClosed) {
        hasIfOccured <- hasIfBracketOpened <- hasIfBracketClosed <- FALSE
        parts <- c(parts, substr(txt, lastSplit, i))
        lastSplit <- i+1
      } else {
        # Normal case without if
        if (!is.null(notAroundOperator)) {
          opFound <- which(sapply(op, function (op) {
            foundTmp <- startsWith(rest, op)
            return (foundTmp
                    && !any(sapply(notAroundOperator,
                                   function(notOp) {
                                     startsWith(restMin1, notOp) ||
                                       startsWith(substring(rest, 2), notOp)
                                   })))
          }))
        } else {
          opFound <- which(sapply(op, function (op) startsWith(rest, op)))
        }
        if (length(opFound) > 0) {
          parts <- c(parts, substr(txt, lastSplit, i-1))
          lastSplit <- i + nchar(op[opFound]) # i+1
        }
      }
    }
  }
  if (lastSplit != i+1)
    parts <- c(parts, substr(txt, lastSplit, i))
  return (trimws(parts))
}

#' @keywords internal
#' @author Daniel Hoop
.splitAll <- function (txt, killFunctionCalls = TRUE, ignoreLhsAssignment = FALSE) {
  # Helper function for function 'findCircularReferences'
  if (length(txt) > 1)
    return (unlist(lapply(txt, .splitAll, ignoreLhsAssignment = ignoreLhsAssignment)))
  if (length(txt) == 0 || txt == "")
    return (txt)
  # Kill function calls
  if (killFunctionCalls)
    txt <- gsub("[a-zA-Z.][a-zA-Z0-9_]? *\\(", "", txt)
  if (ignoreLhsAssignment) {
    txt <- unlist(strsplit(txt, "\\*|\\+|/|\\^|%%|\\(|\\)|\\[|\\]|>|<=|==|=>|!=")) # Split everything except assignments
    txt <- unlist(strsplit(txt, "(?<!<)\\-", perl = TRUE)) # Negative lookbehind: "-" that is not preceeded by "<"
    txt <- unlist(strsplit(txt, "<(?!\\-)", perl = TRUE)) # Negative lookahead: "<" that is not followed by "-"
    # If there is an assignment in a verctor place, then take the right hand side.
    txt <- unlist(lapply(strsplit(txt, "<\\-|="), function (x) {
      if (length(x) == 2)
        return (x[2])
      return (x)
    }))
    return (trimws(txt))
  }
  opRegex <- "\\*|\\-|\\+|/|\\^|=|<|>|%%"
  return (trimws(unlist(strsplit(txt, opRegex))))
}

#' @export
#' @title Find circular references
#' @description Finds circular references in an expression.
#' @author Daniel Hoop
#' @param expr The expression that should be evaluated. Must be quoted like \code{expr = quote({ a <- 1; b <- 2 })}
#' @param assignOp The assignment operator which assigns the values of the right hand side (RHS) to the left hand side (LHS). A character vector of any length, e.g. c("<-", "=")
#' @param filterPatternFunction A function to filter the parts of a line. Consider a line \code{b <- a * foo(b/a)}.\cr
#' The function \code{foo} might take the value of \code{b} from somewhere else, thus you don't want this to be identified as a circular reference.\cr
#' In this case, specify \code{filterPatternFunction} as follows: \code{filterPatternFunction = function(x) return(x[!startsWith(x, 'foo(')]))}
#' @return \code{NULL}, if no circular references are found, otherwise the lines of the expression that contain circular references.
#' @examples expr <- quote({ a <- b; b <- c; c <- a })
#' findCircularReferences(expr)
#' # [1] "a <- b" "b <- c" "c <- a"
#'
#' findCircularReferences(quote({ a <- if (a == 1) a + 1 else b }))
#' # [1] "a <- if (a == 1) a + 1 else b"
#'
#' findCircularReferences(quote({ a <- if (a == 1) 2 else a + b }))
#' # [1] "a <- if (a == 1) 2 else a + b"
#'
#' expr <- quote({
#'   a <- 1
#'   z <- 9
#'   b <- 2
#'   if (a == 1) y <- z else if (b == 1) a <- 1 else x <- z
#'   d <- 3
#'   z <- y
#' })
#' findCircularReferences(expr)
#' [1] "z <- 9" "y <- z" "z <- y"
#'
#' findCircularReferences(quote({ a <- b; c <- d }))
#' # NULL
#'
#' findCircularReferences(quote({ a <- if (a == 1) 2 else b }))
#' # NULL
findCircularReferences <- function (expr, assignOp=c("<-", "="), filterPatternFunction=function(x) return(x[!grepl("^(lookup|if) *\\(", x)])) {

  expr <- .prepareExpression(expr, "expr")
  if (is.null(expr)) # This happens when an empty expression is given.
    return (NULL)

  # Split multiline if else statements like "if (a == b) c <- 1 else b <- 2"
  assignOpRegex <- paste0(escapeStr(assignOp), collapse = "|")
  expr <- unlist(apply(matrix(expr), 1, function(expr) {
    if (!(grepl("^if( |\\()", expr) && grepl(assignOpRegex, expr)))
      return (expr)
    expr <- .splitExceptInBracket(expr, splitIfElseOnly = TRUE)
    expr <- gsub("^if *\\(.*", "", expr)
    return(expr[expr != ""])
  }))

  # Convert multiple assignments per line to one assignment per line each.
  notAroundOperator <- if ("=" %in% assignOp) c("<", ">", "!", "=") else NULL
  expr0 <- unlist(mapply(
    expr = as.list(expr),
    splitted = lapply(as.list(expr), .splitExceptInBracket, operators = assignOp, ifIsAnOperator = FALSE, notAroundOperator = notAroundOperator),
    function (expr, splitted) {
      splitted <- trimws(splitted)
      if (length(splitted) > 2)
        return (rev(paste(c(splitted[-length(splitted)]), c(splitted[-1]), sep = paste0(" ", assignOp[1], " "))))
      return (expr)
    }))

  # Split again by assignment operator.
  expr1 <- lapply(expr0, .splitExceptInBracket, operators = assignOp, ifIsAnOperator = FALSE, notAroundOperator = notAroundOperator)

  # The next step splits each entry in expr1 accoding to the assignment operator. Then, right hand side will be split by operators
  # An entry in a list could look like this, where the original line in expression was z <- b + a
  #  list(list("z"), list(c("b","a")))
  errorMsg <- tryCatch({
    expr1 <- lapply(expr1, function (x) {
      if (length(x) == 1)
        return (NULL)
      if (length(x) > 2) {
        stop (paste0("Each row in the expression must contain maximally one assignment operator '", paste0(assignOp, collapse = "/"), "'. E.g.: a ", assignOp[1], " b.\n",
                     "An erroneouss row looks something like: ", paste0(x, collapse=paste0(" ", assignOp[1]," "))))
      }
      # Split outside of brackets
      res <- .splitExceptInBracket(as.list(x))
      if (length(res[[1]]) > 1)
        stop (paste0("Assigning to a LHS expression `a + b <- c` is not alloed.\n",
                     "An erroneouss row looks something like: ", paste0(x, collapse=paste0(" ", assignOp[1]," "))))
      if (!is.null(filterPatternFunction))
        res <- lapply(res, filterPatternFunction)
      # Now split completely all vars
      res[[2]] <- .splitAll(res[[2]], ignoreLhsAssignment = TRUE)
      if (!is.null(filterPatternFunction))
        res <- lapply(res, filterPatternFunction)
      res <- lapply(res, function (x) x[x!=""] )
      return (res)
    })
    "" # return
  }, error = function (e) return (e$message))
  if (errorMsg != "")
    return (errorMsg)

  expr0 <- expr0[!sapply(expr1, is.null)]
  expr1 <- expr1[!sapply(expr1, is.null)]

  # For each entry in left hand side (lhs), check if it appears in any right hand side (rhs).
  # If not, then delete from list, and carry on, as long as there are changes.
  # If there are no changes anymore, but the list is not empty, then there are circulars. If the list is empty in the end, everything is fine.
  while (TRUE) {
    lengthOld <- sum(!sapply(expr1, is.null))
    allx2 <- unlist(lapply(expr1, function(x)x[[2]]))
    expr1 <- lapply(expr1, function (x) {
      if (length(x) == 0)
        return (NULL)
      if (!x[[1]] %in% allx2)
        return (NULL)
      return (x)
    })
    lengthNew <- sum(!sapply(expr1, is.null))
    if (lengthNew == 0 | lengthNew == lengthOld)
      break
  }
  if (lengthNew > 0)
    return (expr0[!sapply(expr1, is.null)])
  return (NULL)
}

if (FALSE) {
  exprTest <- expression({
    a <- lookup(z)
    z <- b + a # z
    a <- b
    c <- b
    d <- c; b <- d
  })
  cat(paste0(findCircularReferences(exprTest), collapse="\n"))
}

#### **** Functions based on means **** ####

#' @keywords internal
#' @title Interpolation
#' @description Approximates also then, when xout is below/above the minimum/maximum value given in x.
#' @author Daniel Hoop
#' @inheritParams stats::approx
#' @param MARGIN If y has dimensions, i.e. is a matrix, over which of them should the function be applied? See also \code{\link[base]{apply}}.
approx.own <- function (x, y, xout, MARGIN=NULL) {
  if(length(dim(y)) > 0) {
    if (is.null(MARGIN))
      stop ("If y is a matrix, then along must not be NULL.")
    return (apply(y, MARGIN, function (y) approx.own(x=x, y=y, xout=xout)))
  }
  if(length(xout)>1)
    return(sapply(xout,function(xout1)approx.own(x=x, y=y, xout=xout1)))
  if(all(is.na(x)) || all(is.na(y)))
    return(rep(NA, length(xout)))

  isnaxy <- is.na(x) | is.na(y)
  if(any(isnaxy)){
    x <- x[!isnaxy]
    y <- y[!isnaxy]
  }

  if(length(x)==1){
    return(y)
  } else if(all(xout<x)) {
    return(y[which.min(x)])
  } else if(all(xout>x)) {
    return(y[which.max(x)])
  } else {
    return(approx(x=x,y=y,xout=xout)$y)
  }
}

# x <- array(NA, dim = c(3, 2, 3), dimnames = list(1:3, letters[1:2], NULL))
# y <- array(NA, dim = c(2, 2, 3), dimnames = list(6:7, letters[1:2], NULL))
# abind <- function (..., along = 1) {
#   x <- list(...)
#   dim1 <- dim(x[[1]])
#   dimn1 <- dimnames(x[[1]])
#   dims <- do.call("rbind", lapply(x, function (x) {
#     di <- dim(x)
#     if (length(di) != length(dim1) || any(di[-along] != dim1[-along]))
#       stop ("All arrays must have the same dimensions except for the one given in argument `along`.")
#     mapply(function (y, dimn1) {
#       if (length(y) != length(dimn1) || any(y != dimn1)) {
#         stop ("All dimnames must of all arrays must correspond, except those in dimension given in `along`.")
#       }
#     }, y = dimnames(x)[-along], dimn1=dimn1[-along])
#     return (di)
#   }))
#
#   dims <- colSums(dims)
#   dims[-along] <- dim1[-along]
#   dimn <- dimn1
#   dimn[[along]] <- do.call("c", lapply(x, function (x) dimnames(x)[[along]]))
#   res <- array(NA, dim = dims, dimnames=dimn)
#   rows <- 0
#   for (i in length(x)) {
#     stop("Not implemented.")
#   }
# }

#' @export
#' @title Calculate means for different index levels
#' @description Calculates the average values of each index level, the combination of which are specified.
#' @author Daniel Hoop
#' @param data The data.frame containing the columns to be calculated.
#' @param cols The columns in \code{data} for which the means should be caluclated. Character vector. If empty, then the means for all columns in \code{data} will be calculated.
#' @param weights The weights for each observation in \code{data}. Character vector. If empty, then unweighted means will be calculated.
#' @param indices The indices for which the means should be calculated. A data.frame (or list). See the function tapply as a rough reference.
#' @param indexOrder The index combinations for which the average values should be calculated. A list containing the combination of indices in each list place.
#' @param contIndex A continous index, e.g. the size of a farm. Numeric vector.
#' @param nObsInContIndexClass The desired number of observations in each class that will be build for the \code{contIndex}.
#' @param maxContIndexClasses The maximum number of classes that will be built for \code{contIndex}.
#' @param na.rm A logical value indicating whether NA values should be stripped before the average values are getting calculated.
#' @param countNonNa A logical value indicating whether only the number of observations in a strata should be counted (\code{countNonNa = FALSE}) or,
#' @param truncProbs The probability of quantiles within which the truncated mean should be calculated. Numeric vector of length two. Both numbers must lie between 0 and 1 (inclusive).
#' @param type Either \code{"mean"} or \code{"median"} indicating if the (weighted) means or the (weighted) medians should be calculated.
#' in addition, for each column in each strata the number of non NA values should be counted (\code{countNonNa = TRUE}), which is computationally more expensive.
#' @return A function to call in order to get the means for indices. See function \code{\link{getMean}} for more information.
#' @examples
#' data <- data.frame(
#'   a=11:41,
#'   b=61:91)
#' indices <- data.frame(
#'   ind1=c( 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,  4),
#'   ind2=c( 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 9, 7, 7, 7, 8, 8, 7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 9, 10),
#'   all =c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  1))
#' indexOrder <- list(
#'   c('ind1', 'ind2'),
#'   'ind1',
#'   'all')
#' getMean <- calcMean(data = data, indices = indices, indexOrder = indexOrder)
#' print(getMean(raw = TRUE)$mean)
#' print(getMean(raw = TRUE)$n)
calcMean <- function (data, cols=NULL, weights=NULL, indices, indexOrder,
                      contIndex = NULL, nObsInContIndexClass = 3, maxContIndexClasses = 10,
                      na.rm=TRUE, countNonNa=FALSE, truncProbs=NULL,
                      type=c("mean", "median")) {

  # Check errors
  type <- match.arg(type)
  if(is.list(data) && !is.data.frame(data))
    stop ("data must be matrix or data.frame but not a list.")
  .checkIndexConsistency(indices, indexOrder)
  if (is.null(weights))
    weights <- rep(1, nrow(data))
  if (nrow(data) != nrow(indices))
    stop ("nrow(data) must be equal to nrow(indices)")
  if (nrow(data) != length(weights))
    stop ("nrow(data) must be equal to length(weights)")

  # Prepare data for calculations. Is needed because of `countNonNa`.
  if (!is.null(cols)) {
    data <- create.cols(data, cols)
    data <- calc.I.cols(data)
  } else {
    cols <- colnames(data)
  }

  # Calculate means for all indexOrders
  sep <- "-X-"
  # First, calculate all means without contIndex.
  makeMeansWithoutContIndex <- function () {
    meansList <- nList <- nNonNaList <- indexList <- list()
    for (i in 1:length(indexOrder)) {
      indexPasted <- paste.cols(indices[indexOrder[[i]]], sep=sep)
      if (type == "mean") {
        meansList[[i]] <- mean.weight(data, cols=cols, weights=weights, index=indexPasted, trunc.probs=truncProbs)
      } else if (type == "median") {
        meansList[[i]] <- median.weight(data, cols=cols, weights=weights, index=indexPasted)
      }
      nList[[i]] <- tapply(indexPasted, indexPasted, length)
      if (countNonNa)
        nNonNaList[[i]] <- sapply(
          data[,cols,drop=FALSE], function (x)
            tapply(x, indexPasted, function (x)
              sum(!is.na(x))))
      indexList[[i]] <- .recoverIndexFromString(
        rownames(meansList[[i]]), origIndex=indices[indexOrder[[i]]], sep=sep)
    }
    # Create a matrix for means and nNonNa, but only a vector for n.
    meansDf <- do.call("rbind", meansList)
    nNonNa <- do.call("rbind", nNonNaList)
    n <- do.call("c", nList)
    return (list(meansDf=meansDf, nNonNa=nNonNa, n=n, indexList=indexList))
  }
  l <- makeMeansWithoutContIndex()
  meansDf <- l[["meansDf"]]
  nNonNa <- l[["nNonNa"]]
  n <- l[["n"]]
  indexList <- l[["indexList"]]

  # Create a matrix that contains all index combinations
  indexDf <- as.data.frame(
    matrix(NA, ncol=ncol(indices), nrow=sum(sapply(indexList,nrow))))
  colnames(indexDf) <- colnames(indices)
  rowCount <- 0
  for (i in 1:length(indexList)) {
    indexDf[ (1:nrow(indexList[[i]])) + rowCount ,
             names(indexList[[i]]) ] <- indexList[[i]]
    rowCount <- rowCount + nrow(indexList[[i]])
  }
  # Now make a string again (including NAs)
  indexChar <- paste.cols(indexDf, sep=sep)
  rownames(meansDf) <- indexChar
  if (countNonNa)
    rownames(nNonNa) <- indexChar
  names(n) <- indexChar


  # Once again, calculate the means, but this time with contIndex
  if (!is.null(contIndex)) {
    if (!is.numeric(contIndex))
      stop ("`contIndex` must be numeric.")
    if (length(contIndex) != nrow(indices))
      stop ("`length(contIndex)` must be equal to `nrow(indices)`.")
    makeMeansWithContIndex <- function () {
      meansListS <- nListS <- nNonNaListS <- contIndexListS <- list()
      # if (any(is.na(contIndex)))
      #   stop ("There must be no NA values in contIndex.")
      for (i in 1:length(indexOrder)) {
        indexPasted <- paste.cols(indices[indexOrder[[i]]], sep=sep)
        tab <- tapply(indexPasted[!is.na(contIndex)], indexPasted[!is.na(contIndex)], length)
        meansListS[[i]] <- array(NA, dim=c(length(tab), length(cols), maxContIndexClasses),
                                 dimnames = list(names(tab), cols, NULL))
        nListS[[i]] <- array(NA, dim=c(length(tab), maxContIndexClasses),
                             dimnames = list(names(tab), NULL))
        contIndexListS[[i]] <- nListS[[i]]
        if (countNonNa) {
          nNonNaListS[[i]] <- meansListS[[i]]
        }

        for (i2 in sort(unique(indexPasted))) { # i2 <- sort(unique(indexPasted))[1]
          numberOfQuantiles <- min(maxContIndexClasses+1, floor(tab[i2]/nObsInContIndexClass))
          if (numberOfQuantiles > 2) {
            q <- quantile(contIndex[indexPasted == i2],
                          probs=seq(0, 1, length.out = numberOfQuantiles), na.rm=TRUE)
            contIndexListS[[i]][i2, 1:length(q)] <- q
            for (i3 in 1:(length(q)-1)) { # i3 <- 1
              if (i3 < (length(q)-1)) {
                whichRows <- which(indexPasted == i2 & ((q[i3] <= contIndex & contIndex <  q[i3+1]) | q[i3] == contIndex))
              } else {
                whichRows <- which(indexPasted == i2 &   q[i3] <= contIndex & contIndex <= q[i3+1])
              }
              if (length(whichRows) == 0)
                break
              if (type == "mean") {
                meansListS[[i]][i2,,i3] <- mean.weight(
                  data[whichRows,], cols = cols,
                  weights = weights[whichRows], index = indexPasted[whichRows],
                  trunc.probs = truncProbs)
              } else if (type == "median") {
                meansListS[[i]][i2,,i3] <- median.weight(
                  data[whichRows,], cols = cols,
                  weights = weights[whichRows], index = indexPasted[whichRows])
              }
              nListS[[i]][i2,i3] <- length(whichRows)
              if (countNonNa) {
                nNonNaListS[[i]][i2,,i3] <- sapply(
                  data[whichRows,cols,drop=FALSE], function (x) tapply(
                    x, indexPasted[whichRows], function (x) sum(!is.na(x))))
              }
            }
          }
        }
      }

      # Create a matrix for means and nNonNa, but only a vector for n.
      installFromCRAN("abind")
      abindFunc <- function (...) abind::abind(..., along = 1); is.null(abindFunc)
      meansDfS <- do.call("abindFunc", meansListS)
      if (countNonNa) {
        nNonNaS <- do.call("abindFunc", nNonNaListS)
      } else {
        nNonNaS <- NULL
      }
      nS <- do.call("rbind", nListS)
      contIndexS <- do.call("rbind", contIndexListS)

      # Give names
      rownames(meansDfS) <- indexChar
      if (countNonNa)
        rownames(nNonNaS) <- indexChar
      rownames(nS) <- indexChar
      rownames(contIndexS) <- indexChar
      return (list(meansDfS=meansDfS, nNonNaS=nNonNaS, nS=nS, contIndexS=contIndexS))
    }
    l <- makeMeansWithContIndex()
    meansDfS <- l[["meansDfS"]]
    nNonNaS <- l[["nNonNaS"]]
    nS <- l[["nS"]]
    contIndexS <- l[["contIndexS"]]
  }
  # Prepare result to return
  means <- list(
    mean=meansDf, n=n, nNonNa=nNonNa,
    s = if (is.null(contIndex)) NULL else list(mean=meansDfS, n=nS, nNonNa=nNonNaS, contIndex=contIndexS),
    index=indexChar, indexOrder=indexOrder, indices=indices)
  class(means) <- "meanCalcResult"
  # Clean up the environment, i.e. only keep some objects
  killObj <- c(ls(all.names = TRUE), "killObj")
  killObj <- killObj[!killObj %in% c("sep", "means")]
  rm(list = killObj)

  # Build a closure that contains the object 'means', 'sep' and return this closure.
  # Documentation of this closure, see below.
  return (
    function (index, cols=NULL, contIndex=NULL, nMin, count=c("all", "nonNa"),
              forceValue=FALSE, raw=FALSE) {
      # The following variables are given by upper scope: means, sep

      # Assure that means is of the right class.
      if (class(means) != "meanCalcResult")
        stop ("'means' must be an object of class 'meanCalcResult'.",
              " Hint: Calculate with the function calcMean.")
      # Don't calculate anything if raw is TRUE.
      if (raw)
        return (means)
      # Assure that nNonNa was calculated if count = "nonNa" is chosen
      count <- match.arg(count)
      if (count == "nonNa" && is.null(means$nNonNa))
        stop ("If `count=\"nonNa\"` is chosen, then you must already set",
              " `countNonNa=TRUE` when calling the function `calcMean`.")
      # Assure that index is list.
      if (!is.list(index))
        stop ("'index' must be a list. Hint: If you call 1 column from a",
              " data.frame use drop=FALSE (e.g. data[,\"col\",drop=FALSE]).")
      # Check that all indices given in 'indices' are contained in 'index'.
      indicesNames <- names(means$indices)
      notContained <- names(index)[!names(index) %in% indicesNames]
      if (length(notContained)>0)
        stop ("All indices given in 'indexOrder' must exist in 'index'. ",
              "This indices are given in 'indexOrder' but missing in 'index': ",
              paste0(notContained, collapse=", "))
      # Check the opposite
      notContained <- indicesNames[!indicesNames %in% names(index)]
      if (length(notContained)>0)
        stop ("All indices given in 'means$indices' must be given in 'index'. ",
              "This indices are given in 'means$indices' but missing in 'index': ",
              paste0(notContained, collapse=", "))
      # Check that all cols are contained in the means
      if (is.null(cols))
        cols <- colnames(means$mean)
      notContained <- cols[!cols %in% colnames(means$mean)]
      if (length(notContained)>0)
        stop ("All columns given in 'cols' must exist in 'means'. ",
              "This columns are given in 'cols' but missing in 'means': ",
              paste0(notContained, collapse=", "))
      rm(notContained)
      # Reorder index! This is very important!
      getIndexNames <- indicesNames[ indicesNames%in%names(index) ]
      index <- as.data.frame(index[getIndexNames], stringsAsFactors=FALSE)

      # Create the pasted index and result
      indexChar <- paste.cols(index, sep=sep)
      indexRes <- index
      indexRes[] <- NA
      meanRes <- as.data.frame(
        matrix(NA_real_, ncol=length(cols), nrow=length(indexChar)))
      colnames(meanRes) <- cols
      contIndexRes <- rep(NA_real_, length(indexChar))
      # Create dummy index to use in while loop
      dummyIndex <- rep(NA_character_, length(indicesNames))
      names(dummyIndex) <- indicesNames
      # Create a function to get the right contIndex class
      getContIndexClass <- function (indx, contIndex) {
        class <- -1 + which(
          c(NA, means$s$contIndex[indx,]) <= contIndex & contIndex < c(means$s$contIndex[indx,], NA)
          | c(NA, means$s$contIndex[indx,]) == contIndex)
        if (length(class) == 1)
          return (class)
        contIndexBorders <- means$s$contIndex[indx, !is.na(means$s$contIndex[indx,])]
        if (length(contIndexBorders) == 0)
          return (NULL)
        if (contIndex >= max(contIndexBorders))
          return (-1 + which.max(contIndexBorders))
        if (contIndex <= min(contIndexBorders))
          return (-1 + which.min(contIndexBorders))
        warning ("Error in getContIndexClass. This should not happen. Please report this to the developer with sample data and code.", immediate. = FALSE)
      }
      # Create a function that will get N, no matter if count=c("all", "nonNa")
      getN <- function (indx, col, contIndexClass=NULL) {
        if (!is.null(contIndexClass)) {
          return (means$s$n[indx, contIndexClass])
        }
        if (count == "all")
          return (means$n[indx])
        if (count == "nonNa")
          return (means$nNonNa[indx, col[1]])
      }

      # Create results in case contIndex was given
      if (!is.null(contIndex)) {
        if (count == "nonNa")
          stop ("If `contIndex` is given, then only count = 'all' is a valid option.")
        if (is.null(means$s))
          stop ("If `contIndex` is given, then `contIndex` must also be given when the function",
                " `calcMean` was called earlier.")
        # Make the matrix to loop over.
        # The first two colnames must be other than those given in index!
        cn1 <- letters[sample(1:length(letters), 2)]
        while (any(cn1 %in% colnames(index)))
          cn1 <- paste0(cn1, letters[sample(1:length(letters), 2)])
        suIS <- cbind(contIndex, # 1 -> Will be grabbed with 1 below
                      indexChar) # 2 -> Currently not used
        colnames(suIS) <- cn1
        suIS <- cbind(suIS,
                      apply(index,2,trimws))
        # Now loop over each row in suIS.
        tmpRes <- apply(suIS, 1, function (row) {
          # Loop over all indexOrders and try if the value can be filled.
          # But only, if contIndex was given.
          if (!is.na(row[1])) {
            i1 <- 1
            while (i1 <= length(means$indexOrder)) {
              # Create dummy index
              di <- dummyIndex
              di[ means$indexOrder[[i1]] ] <- row[means$indexOrder[[i1]]]
              diVec <- di
              diChar <- paste(di, collapse=sep)
              # If dummy index is contained in means, then...
              if (diChar %in% means$index) {
                # Get the right contIndex class for row and check if enough observations are available for that contIndex class.
                contIndex <- as.numeric(row[1])
                contIndexClass <- getContIndexClass(indx=diChar, contIndex=contIndex)
                if (!is.null(contIndexClass)
                    && getN(diChar, cols, contIndexClass=contIndexClass) >= nMin) {
                  # Give back the approximated value, the index, and the contIndex.
                  rng <- range(means$s$contIndex[diChar,], na.rm = TRUE)
                  return (list(approx.own(x = means$s$contIndex[diChar,],
                                          y = means$s$mean[diChar, cols,],
                                          xout = contIndex,
                                          MARGIN = 1),
                               diVec,
                               max(rng[1], min(rng[2], contIndex))))
                } else {
                  # If the index is available, but contIndex classes are too small,
                  # don't go look for other indices, but rather fill the value with this index wihtout contIndex classes (further down below).
                  break
                }
              }
              i1 <- i1 + 1
            }
          }
          # If contIndex was NA or no contIndex class offered enough observations, return NA for value, index, and contIndex.
          return (list(rep(NA_real_, length(cols)),
                       rep(NA_real_, ncol(means$indices)),
                       NA_real_))
        })
        # Extract the results from the list
        contIndexRes[] <- sapply(tmpRes, function (x) x[[3]])
        meanRes[] <- t(sapply(tmpRes, function (x) x[[1]]))
        # It is necessary to calculate colums like I(a/b) again in order to get ratio of means instead of mean of ratios.
        meanRes <- calc.I.cols(meanRes)
        indexTmp <- as.data.frame(t(sapply(tmpRes, function (x) x[[2]])),
                                  stringsAsFactors = FALSE)
        for (i in 1:ncol(index))
          class(indexTmp[,i]) <- class(index[,i])
        indexRes[] <- indexTmp
        rm(i ,indexTmp, tmpRes)
        # Alternative 2
        # # sortUniqueIndexContIndex, sortUniqueIndexContIndexDataFrame
        # suIS <- data.frame(i = indexChar, s = contIndex, stringsAsFactors = FALSE)
        # suISDf <- as.matrix(index)
        # # suISDf <- as.matrix(
        # #   .recoverIndexFromString(indexChar, origIndex=index, sep=sep))
        # # nonDupl <- !duplicated(suIS)
        # # suIS <- suIS[nonDupl,,drop=FALSE]
        # # suISDf <- suISDf[nonDupl,,drop=FALSE]
        # # Loop over all unique rows.
        # for (i0 in 1:nrow(suIS)) { # i0 <- 1
        #   # if (suIS[i0,"i"] %in% means$index) {}
        #   i1 <- 1
        #   while (i1 <= length(means$indexOrder)) {
        #     di <- dummyIndex
        #     di[ means$indexOrder[[i1]] ] <- suISDf[i0, means$indexOrder[[i1]]]
        #     di <- list(char = paste(di, collapse=sep), vec = di)
        #
        #     contIndexClass <- getContIndexClass(indx=diChar, contIndex=suIS[i1,"s"])
        #     if ((diChar %in% means$index && getN(diChar, cols, contIndexClass=contIndexClass) >= nMin)) {
        #       browser()
        #     }
        #   }
        # }

        # Prepare the indices that still need to be filled because they only contain NA values
        remainingRows <- apply(meanRes, 1, function (x) all(is.na(x)))
      } else {
        remainingRows <- rep(TRUE, nrow(meanRes))
      }

      suIndexChar <- indexChar[remainingRows]
      suIndexDf <- index[remainingRows, , drop=FALSE]
      nonDupl <- !duplicated(suIndexChar)
      suIndexChar <- suIndexChar[nonDupl]
      suIndexDf <- suIndexDf[nonDupl, , drop=FALSE]

      # Create results without considering contIndex
      for (i0 in 1:length(suIndexChar)) { # i0 <- 1
        fillRows <- which(indexChar == suIndexChar[i0])
        for (i01 in 1:length(cols)) { # i01 <- 1
          # Always fill the remaining cols with the values that are right for
          # the actual col. This way the loop can be aborted after the first run
          # if count = "all".
          fillCols <- cols[i01:length(cols)]
          if (suIndexChar[i0] %in% means$index
              && getN(suIndexChar[i0], fillCols) >= nMin) {
            meanRes[fillRows,fillCols] <-
              as.list(means$mean[suIndexChar[i0],fillCols,drop=FALSE])
            indexRes[fillRows,] <-
              as.list(suIndexDf[i0,])
          } else {
            i1 <- 1
            while (i1 <= length(means$indexOrder)) {
              di <- dummyIndex
              di[ means$indexOrder[[i1]] ] <- suIndexDf[i0, means$indexOrder[[i1]]]
              diVec <- di
              diChar <- paste(di, collapse=sep)
              #di <- createDummyIndex()
              if ((diChar %in% means$index && getN(diChar, fillCols) >= nMin)
                  || (forceValue && i1 == length(means$indexOrder))) {
                meanRes[fillRows,fillCols] <-
                  as.list(means$mean[diChar,fillCols,drop=FALSE])
                indexRes[fillRows,] <-
                  as.list(diVec)
                break
              }
              i1 <- i1+1
            }
          }
          # No need to run over all cols if count == "all" because the first
          # run has already filled all cols.
          if (count == "all")
            break
        }
      }
      for (i in 1:length(indexRes))
        class(indexRes[[i]]) <- class(index[[i]])
      rownames(indexRes) <- NULL
      return (list(mean=meanRes, index=indexRes, contIndex=contIndexRes))
    })
}
# calcMean <- function (data, cols=NULL, weights=NULL, indices, indexOrder,
#                       na.rm=TRUE, countNonNa=FALSE, truncProbs=NULL,
#                       type=c("mean", "median")) {
#
#   # Check errors
#   type <- match.arg(type)
#   if(is.list(data) && !is.data.frame(data))
#     stop ("data must be matrix or data.frame but not a list.")
#   .checkIndexConsistency(indices, indexOrder)
#   if (is.null(weights))
#     weights <- rep(1, nrow(data))
#   if (nrow(data) != nrow(indices))
#     stop ("nrow(data) must be equal to nrow(indices)")
#   if (nrow(data) != length(weights))
#     stop ("nrow(data) must be equal to length(weights)")
#
#   # Prepare data for calculations. Is needed because of `countNonNa`.
#   if (!is.null(cols)) {
#     data <- create.cols(data, cols)
#     data <- calc.I.cols(data)
#   } else {
#     cols <- colnames(data)
#   }
#
#   # Calculate means for all indexOrders
#   meansList <- nList <- nNonNaList <- indexList <- list()
#   sep <- "-X-"
#   for (i in 1:length(indexOrder)) {
#     indexPasted <- paste.cols(indices[indexOrder[[i]]], sep=sep)
#     if (type == "mean") {
#       meansList[[i]] <- mean.weight(data, cols=cols, weights=weights, index=indexPasted, trunc.probs=truncProbs)
#     } else if (type == "median") {
#       meansList[[i]] <- median.weight(data, cols=cols, weights=weights, index=indexPasted)
#     }
#     nList[[i]] <- tapply(indexPasted, indexPasted, length)
#     if (countNonNa)
#       nNonNaList[[i]] <- sapply(
#         data[,cols,drop=FALSE], function (x) tapply(
#           x, indexPasted, function (x) sum(!is.na(x))))
#     indexList[[i]] <- .recoverIndexFromString(
#       rownames(meansList[[i]]), origIndex=indices[indexOrder[[i]]], sep=sep)
#   }
#   # Create a matrix for means an nNonNa, but only a vector for n.
#   meansDf <- do.call("rbind", meansList)
#   nNonNa <- do.call("rbind", nNonNaList)
#   n <- do.call("c", nList)
#   # Create a matrix that contains all index combinations
#   indexDf <- as.data.frame(
#     matrix(NA, ncol=ncol(indices), nrow=sum(sapply(indexList,nrow))))
#   colnames(indexDf) <- colnames(indices)
#   rowCount <- 0
#   for (i in 1:length(indexList)) {
#     indexDf[ (1:nrow(indexList[[i]])) + rowCount ,
#              names(indexList[[i]]) ] <- indexList[[i]]
#     rowCount <- rowCount + nrow(indexList[[i]])
#   }
#   # Now make a string again (including NAs)
#   indexChar <- paste.cols(indexDf, sep=sep)
#   rownames(meansDf) <- indexChar
#   if (countNonNa)
#     rownames(nNonNa) <- indexChar
#   names(n) <- indexChar
#   # Prepare result to return
#   means <- list(mean=meansDf, n=n, nNonNa=nNonNa,
#                 index=indexChar, indexOrder=indexOrder, indices=indices)
#   class(means) <- "meanCalcResult"
#   # Clean up the environment, i.e. only keep some objects
#   killObj <- c(ls(all.names = TRUE), "killObj")
#   killObj <- killObj[!killObj %in% c("sep", "means")]
#   rm(list = killObj)
#
#   # Build a closure that contains the object 'means' and return this closure it.
#   # Documentation of this closure, see below.
#   return (
#     function (index, cols=NULL, nMin, count=c("all", "nonNa"),
#               forceValue=FALSE, raw=FALSE) {
#
#       # Assure that means is of the right class.
#       if (class(means) != "meanCalcResult")
#         stop ("'means' must be an object of class 'meanCalcResult'.",
#               " Hint: Calculate with the function calcMean.")
#       # Don't calculate anything if raw is TRUE.
#       if (raw)
#         return (means)
#       # Assure that nNonNa was calculated if count = "nonNa" is chosen
#       count <- match.arg(count)
#       if (count == "nonNa" && is.null(means$nNonNa))
#         stop ("If `count=\"nonNa\"` is chosen, then you must already set",
#               " `countNonNa=TRUE` when calling the function `calcMean`.")
#       # Assure that index is list.
#       if (!is.list(index))
#         stop ("'index' must be a list. Hint: If you call 1 column from a",
#               " data.frame use drop=FALSE (e.g. data[,\"col\",drop=FALSE]).")
#       # Check that all indices given in 'indices' are contained in 'index'.
#       indicesNames <- names(means$indices)
#       notContained <- names(index)[!names(index) %in% indicesNames]
#       if (length(notContained)>0)
#         stop ("All indices given in 'indexOrder' must exist in 'index'. ",
#               "This indices are given in 'indexOrder' but missing in 'index': ",
#               paste0(notContained, collapse=", "))
#       # Check the opposite
#       notContained <- indicesNames[!indicesNames %in% names(index)]
#       if (length(notContained)>0)
#         stop ("All indices given in 'means$indices' must be given in 'index'. ",
#               "This indices are given in 'means$indices' but missing in 'index': ",
#               paste0(notContained, collapse=", "))
#       # Check that all cols are contained in the means
#       if (is.null(cols))
#         cols <- colnames(means$mean)
#       notContained <- cols[!cols %in% colnames(means$mean)]
#       if (length(notContained)>0)
#         stop ("All columns given in 'cols' must exist in 'means'. ",
#               "This columns are given in 'cols' but missing in 'means': ",
#               paste0(notContained, collapse=", "))
#       rm(notContained)
#       # Reorder index! This is very important!
#       getIndexNames <- indicesNames[ indicesNames%in%names(index) ]
#       index <- as.data.frame(index[getIndexNames], stringsAsFactors=FALSE)
#
#       # Create a function that will get N, no matter if count=c("all", "nonNa")
#       getN <- function (indx, col) {
#         if (count == "all")
#           return (means$n[indx])
#         if (count == "nonNa")
#           return (means$nNonNa[indx,col[1]])
#       }
#       # Create the pasted index and result
#       indexChar <- paste.cols(index, sep=sep)
#       suIndexChar <- sort(unique(indexChar))
#       suIndexDf <- as.matrix(
#         .recoverIndexFromString(suIndexChar, origIndex=index, sep=sep))
#       #indexRes <- character()
#       indexRes <- index
#       indexRes[] <- NA
#       meanRes <- as.data.frame(
#         matrix(NA, ncol=length(cols), nrow=length(indexChar)))
#       colnames(meanRes) <- cols
#       # Create dummy index to use in while loop
#       dummyIndex <- rep(NA_character_, length(indicesNames))
#       names(dummyIndex) <- indicesNames
#       # Fill the result with values
#       for (i0 in 1:length(suIndexChar)) { # i0 <- 1
#         fillRows <- which(indexChar == suIndexChar[i0])
#         for (i01 in 1:length(cols)) { # i01 <- 1
#           # Always fill the remaining cols with the values that are right for
#           # the actual col. This way the loop can be aborted after the first run
#           # if count = "all".
#           fillCols <- cols[i01:length(cols)]
#           if (suIndexChar[i0] %in% means$index
#               && getN(suIndexChar[i0], fillCols) >= nMin) {
#             meanRes[fillRows,fillCols] <-
#               as.list(means$mean[suIndexChar[i0],fillCols,drop=FALSE])
#             indexRes[fillRows,] <-
#               as.list(suIndexDf[i0,])
#           } else {
#             i1 <- 1
#             while(i1 <= length(means$indexOrder)) {
#               di <- dummyIndex
#               di[ means$indexOrder[[i1]] ] <-
#                 suIndexDf[i0, means$indexOrder[[i1]]]
#               diVec <- di
#               di <- paste(di, collapse=sep)
#               if ((di %in% means$index && getN(di, fillCols) >= nMin) ||
#                   (forceValue && i1 == length(means$indexOrder))) {
#                 meanRes[fillRows,fillCols] <-
#                   as.list(means$mean[di,fillCols,drop=FALSE])
#                 indexRes[fillRows,] <- as.list(diVec)
#                 break
#               }
#               i1 <- i1+1
#             }
#           }
#           # No need to run over all cols if count == "all" because the first
#           # run has already filled all cols.
#           if (count == "all")
#             break
#         }
#       }
#       for (i in 1:length(indexRes))
#         class(indexRes[[i]]) <- class(index[[i]])
#       return (list(mean=meanRes, index=indexRes))
#     }
#   )
# }

#' @title Imputute by means of strata averages
#' @description Draws from pre-calculated average values but only if the number of observations in the strata is large enough.
#' @author Daniel Hoop
#' @param index The index for each observation of which the mean value should be looked up.
#' @param contIndex The continuous index for each observation of which the mean value should be looked up.
#' @param cols The colums of which the average values should be looked up.
#' @param nMin The minimum number of observations that must be 'behind' a mean value in \code{means}, in order for it to be valuable.\cr
#' The indices will be 'traversed' in the indexOrder that was specified to create \code{means} by using the function \code{\link{calcMean}}.\cr
#' If in the first index combination specified in 'indexOrder' not enough observations are found, then the next higher index combination will be chosen, etc.\cr
#' @param count Either \code{"all"} or \code{"nonNa"}, indicating if, for the minimum number of observations given in \code{nMin}, all observations should be counted, or only those with non NA values.
#' @param forceValue Logical value indicating if the value of the last index according to the \code{indexOrder} given in \code{\link{calcMean}} should be taken, even if it does not provide the minimum required observations according to \code{nMin}.
#' @param raw Logical value indicating if the raw data should be returned instead of matching means to according index values.
#' @details If no index combination was found that offers enough observations, and \code{forceValue = FALSE} (default), then NA values will be returned for the respective rows in \code{index}.
#' @return A list containing two list places 'mean' and 'index'.\cr
#' The list place 'mean' contains the means that were looked up.\cr
#' The list place 'index' shows which index combination was chosen to look up the mean.
#' @examples # First, pre-calculate the means
#' data <- data.frame(
#'   a=11:41,
#'   b=61:91)
#' indices <- data.frame(
#'   ind1=c( 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,  4),
#'   ind2=c( 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 9, 7, 7, 7, 8, 8, 7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 9, 10),
#'   all =c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  1))
#' indexOrder <- list(
#'   c('ind1', 'ind2'),
#'   'ind1',
#'   'all')
#' contIndex <-
#'        c( 1, 2, 3, 4, 5, 6, 7, 8, 9,10, 1, 2, 3, 1,11,12,13, 1, 1, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3,  1)
#' getMean <- calcMean(data = data, cols=c("a","b","I(a/b)"), indices = indices, indexOrder = indexOrder, contIndex = contIndex)
#' print(getMean(raw = TRUE)$mean)
#' print(getMean(raw = TRUE)$n)
#'
#' # Define the index of some observations that should be looked up.
#' index <- data.frame(
#'   ind1=c(1, 2, 3, 4),
#'   ind2=c(7, 8, 9, 10),
#'   all=c(0, 0, 0, 1))
#'
#' # Now look up the means for some observations.
#' # Notice that the fourth observation did not get a value because it does not satisfy nMin.
#' getMean(index, nMin=10)
#' # The following line will get an imputed value for all observations because forceValue=TRUE.
#' getMean(index, nMin=10, forceValue=TRUE)
#' # If you give a continuous index, the results will be caluculated along that continuous index.
#' getMean(index, contIndex=c(1,2,NA,9), nMin=0)
getMean <- function (index, cols=NULL, nMin, count=c("all", "nonNa"),
                     forceValue=FALSE, raw=FALSE) {
  stop ("This function only serves a documentation porpuse and is not to be used standalone.",
        " You have to define it yourself by typing something along the lines of:",
        "\ngetMean <- calcMean(...). Consult the documentation for more information and examples.")
}

#.recoverIndexFromString(c("a_1","b_2"), origIndex=list(z=c("a","b"), y=c(1,2)))
#' @keywords internal
#' @author Daniel Hoop
.recoverIndexFromString <- function (string, origIndex=NULL, indexNames=NULL, indexClasses=NULL, sep="_") {
  tryCatch({
    index <- as.data.frame(do.call("rbind", strsplit(string, sep)), stringsAsFactors=FALSE)
  }, warning=function (w) {
    stop ("Not all index entries have the same number of separators. Consider choosing another separator that will surely not be part of the index value itself. E.g. index_1_index_2 (spparator is '_') -> index_1-X-index2 (the separator is '-X-')")
  })
  if (!is.null(origIndex)) {
    if (length(origIndex) != length(index))
      stop ("if !is.null(origIndex), then length(origIndex) must be equal to ncol(index)")
    if (is.matrix(origIndex))
      origIndex <- as.data.frame(origIndex, stringsAsFactors=FALSE)
    indexNames <- names(origIndex)
    indexClasses <- sapply(origIndex, class)
  }
  if (!is.null(indexNames)) {
    if (length(indexNames) != length(index))
      stop ("if !is.null(indexNames), then length(indexNames) must be equal to ncol(indexNames)")
    names(index) <- indexNames
  }
  if (!is.null(indexClasses)) {
    if (length(indexClasses) != length(index))
      stop ("if !is.null(indexClasses), then length(indexClasses) must be equal to ncol(index)")
    for (i in 1:length(indexClasses))
      class(index[[i]]) <- indexClasses[i]
  }
  return (index)
}

#### **** Functions based on calibration approach **** ####

#' @title calibration Method from package 'sampling'.
#' @description Slightly adapted to work with variables of which the population \code{total} is 0.
#' @author Alina Matei, adapted by Daniel Hoop
#' @inheritParams sampling::calib
#' @seealso \code{\link[sampling:calib]{sampling::calib}}
#' @keywords internal
#'
.calibAdapted <- function (Xs, d, total, q = rep(1, length(d)), method = c("linear", "raking", "truncated", "logit"), bounds = c(low = 0, upp = 10),  description = FALSE, max_iter = 500) {
  # This adapted version of sampling::calib is necessary because sampling::calib cannot handle variables of which the sum on the population level is zero.
  # This error message occurs:
  # Error in if (max(abs(tr - total)/total) < EPS1 & all(g >= bounds[1] &  :
  #   missing value where TRUE/FALSE needed
  # Thus, the condition is slightly changed. For information on arguments, refer to help(sampling::calib)

  if (any(is.na(Xs)) | any(is.na(d)) | any(is.na(total)) |
      any(is.na(q)))
    stop("the input should not contain NAs")
  if (!(is.matrix(Xs) | is.array(Xs)))
    Xs = as.matrix(Xs)
  if (is.matrix(Xs))
    if (length(total) != ncol(Xs))
      stop("Xs and total have different dimensions")
  if (is.vector(Xs) & length(total) != 1)
    stop("Xs and total have different dimensions")
  if (any(is.infinite(q)))
    stop("there are Inf values in the q vector")
  if (missing(method))
    stop("specify a method")
  if (!(method %in% c("linear", "raking", "logit", "truncated")))
    stop("the specified method is not in the list")
  if (method %in% c("linear", "raking") & !missing(bounds))
    stop("for the linear and raking the bounds are not allowed")
  EPS = .Machine$double.eps
  EPS1 = 1e-06
  n = length(d)
  lambda = as.matrix(rep(0, n))
  lambda1 = MASS::ginv(t(Xs * d * q) %*% Xs, tol = EPS) %*% (total -
                                                               as.vector(t(d) %*% Xs))
  if (method == "linear" | max(abs(lambda1)) < EPS)
    g = 1 + q * as.vector(Xs %*% lambda1)
  else if (method == "truncated") {
    if (!missing(bounds)) {
      if (bounds[2] <= 1 || bounds[1] >= 1 || bounds[1] >
          bounds[2])
        warning("The conditions low<1<upp are not fulfilled")
    }
    else stop("Specify the bounds")
    g = 1 + q * as.vector(Xs %*% lambda1)
    list = 1:length(g)
    l = 0
    g1 = g
    Xs1 = Xs
    d1 = d
    t2 = total
    list1 = 1:length(g)
    q1 = q
    while (l < max_iter) {
      l = l + 1
      if (any(g < bounds[1]) | any(g > bounds[2])) {
        g[g < bounds[1]] = bounds[1]
        g[g > bounds[2]] = bounds[2]
        list = (1:length(g))[g > bounds[1] & g < bounds[2]]
        if (length(list) != 0) {
          g1 = g[list]
          t2 = total - as.vector(t(g[-list] * d[-list]) %*%
                                   Xs[-list, ])
          Xs1 = Xs[list, ]
          d1 = d[list]
          q1 = q[list]
          list1 = list
        }
      }
      t1 = as.vector(t(d1) %*% Xs1)
      lambda1 = MASS::ginv(t(Xs1 * d1 * q1) %*% Xs1, tol = EPS) %*%
        (t2 - t1)
      if (length(list1) > 1)
        g1 = 1 + q1 * as.vector(Xs1 %*% lambda1)
      else if (length(list1) == 1) {
        g1 = 1 + q1 * as.vector(as.vector(Xs1) %*% as.vector(lambda1))
      }
      g[list1] = g1
      tr = crossprod(Xs, g * d)
      #total1 <- total[total != 0]
      #if (length(total))
      if (max(abs(tr - total)) < EPS1 & all(g >= bounds[1] & g <= bounds[2]))
        break
    }
    if (l == max_iter) {
      cat("No convergence in", max_iter, "iterations with the given bounds. \n")
      cat("The bounds for the g-weights are:", min(g),
          " and ", max(g), "\n")
      cat(" and the g-weights are given by g\n")
    }
  }
  else if (method == "raking") {
    lambda = as.matrix(rep(0, ncol(Xs)))
    w1 = as.vector(d * exp(Xs %*% lambda * q))
    for (l in 1:max_iter) {
      phi = t(Xs) %*% w1 - total
      T1 = t(Xs * w1)
      phiprim = T1 %*% Xs
      lambda = lambda - MASS::ginv(phiprim, tol = EPS) %*% phi
      w1 = as.vector(d * exp(Xs %*% lambda * q))
      if (any(is.na(w1)) | any(is.infinite(w1))) {
        warning("No convergence")
        g = NULL
        break
      }
      tr = crossprod(Xs, w1)
      if (max(abs(tr - total)/total) < EPS1)
        break
    }
    if (l == max_iter) {
      warning("No convergence")
      g = NULL
    }
    else g = w1/d
  }
  else if (method == "logit") {
    if (bounds[2] <= 1 || bounds[1] >= 1 || bounds[1] > bounds[2])
      stop("The conditions low<1<upp are not fulfilled")
    A = (bounds[2] - bounds[1])/((1 - bounds[1]) * (bounds[2] -
                                                      1))
    u = rep(1, length(d))
    F = (bounds[1] * (bounds[2] - 1) + bounds[2] * (1 - bounds[1]) *
           u)/(bounds[2] - 1 + (1 - bounds[1]) * u)
    w1 = as.vector(d * F)
    T = t(Xs * w1)
    phiprim = MASS::ginv(T %*% Xs, tol = EPS)
    g = F
    tr = crossprod(Xs, w1)
    if (max(abs(tr - total)/total) > EPS1 | any(g < bounds[1]) |
        any(g > bounds[2])) {
      lambda1 = rep(0, ncol(Xs))
      list = 1:length(g)
      t2 = total
      Xs1 = Xs
      d1 = d
      g1 = g
      q1 = q
      list1 = 1:length(g)
      for (l in 1:max_iter) {
        if (any(g < bounds[1]) | any(g > bounds[2])) {
          g[g < bounds[1]] = bounds[1]
          g[g > bounds[2]] = bounds[2]
          list = (1:length(g))[g > bounds[1] & g < bounds[2]]
          if (length(list) != 0) {
            g1 = g[list]
            t2 = total - as.vector(t(g[-list] * d[-list]) %*%
                                     Xs[-list, ])
            Xs1 = Xs[list, ]
            d1 = d[list]
            q1 = q[list]
            list1 = list
          }
        }
        t1 = as.vector(t(d1) %*% Xs1)
        phi = t(Xs1) %*% as.vector(d1 * g1) - t1
        T = t(Xs1 * as.vector(d1 * g1))
        phiprime = T %*% Xs1
        lambda1 = lambda1 - MASS::ginv(phiprime, tol = EPS) %*%
          (as.vector(phi) - t2 + t1)
        u = exp(A * (Xs1 %*% lambda1 * q1))
        F = g1 = (bounds[1] * (bounds[2] - 1) + bounds[2] *
                    (1 - bounds[1]) * u)/(bounds[2] - 1 + (1 -
                                                             bounds[1]) * u)
        if (any(is.na(g1))) {
          warning("no convergence")
          g1 = g = NULL
          break
        }
        g[list1] = g1
        tr = crossprod(Xs, g * d)
        if (max(abs(tr - total)/total) < EPS1 & all(g >=
                                                    bounds[1] & g <= bounds[2]))
          break
      }
      if (l == max_iter) {
        cat("no convergence in", max_iter, "iterations with the given bounds. \n")
        cat("the bounds for the g-weights are:", min(g),
            " and ", max(g), "\n")
        cat(" and the g-weights are given by g\n")
        g = NULL
      }
    }
  }
  if (description && !is.null(g)) {
    par(mfrow = c(3, 2), pty = "s")
    hist(g)
    boxplot(g, main = "Boxplot of g")
    hist(d)
    boxplot(d, main = "Boxplot of d")
    hist(g * d)
    boxplot(g * d, main = "Boxplot of w=g*d")
    if (method %in% c("truncated", "raking", "logit"))
      cat("number of iterations ", l, "\n")
    cat("summary - initial weigths d\n")
    print(summary(d))
    cat("summary - final weigths w=g*d\n")
    print(summary(as.vector(g * d)))
  }
  g
}

#' @keywords internal
#' @title Imputation by means of calibrated averages.
#' @description Calibrates certain columns of the reference dataset \code{dataGet} to fit to the sum in \code{dataFill}. Yeat no detailed description available.
#' @author Daniel Hoop
getMeanCalib <- function (dataFill, dataGet, colsCalib, colsCalc,
                          weightsFill=NULL, initialWeightsGet=NULL, na.rm=TRUE) {
  # To fill 1 farm only, just select 1 row. It will work as well.
  # In "dataGet": Get the 100-200 farms, most similar to the farms that should be "filled".
  #   As similarity columns, use FAT99 ratios (like [ruminants]/[total husbandry])
  #   Obsviously, the figures to determine similarity must be available in predecessors of dataGet and dataFill.
  # Alternative implemenation
  #   Give all available observations in dataGet.
  #   Give all observations to be filled in dataFill.
  #   Iterate over each observation (=row) in dataFill and run the calibration.
  #   Kind of an index might be needed to assure that only obseravations in the same year are calibrated.

  # Assuming you want to split costs for buildings:
  # In colsCalib, give the sum of building costs, as well as some structural information, such as animals, cultures.
  #    Needs to be available in dataGet only.
  # In colsCalc, give building depreciation, building reparation, etc.
  #    Needs to be available in dataGet only.
  # In initialWeightsGet, give the initial weights of the observations that shall be calibrated. Can also be NULL, will then be 1 for each observation.

  # Sanitize arguments
  if (is.null(initialWeightsGet))
    initialWeightsGet <- rep(1, nrow(dataGet))
  if (length(initialWeightsGet) != nrow(dataGet))
    stop ("length(initialWeightsGet) must be equal to nrow(dataGet)")
  if (is.null(weightsFill))
    weightsFill <- rep(1, nrow(dataFill))
  if (length(weightsFill) != nrow(dataFill))
    stop ("length(weightsFill) must be equal to nrow(dataGet)")

  # Install pcakage only if it is not yet installed.
  .install.package <- function (x) {
    x <- x[!x%in%rownames(installed.packages())];
    if (length(x>0)) install.packages(x)
  }
  .install.package("sampling")

  # Calibrate. But first adapt the target sum to be roughly in the order of the 'get sum'.
  targetSumFactor <- sum(initialWeightsGet) / sum(weightsFill)
  targetSum <- targetSumFactor * colSums(weightsFill * dataFill[ , colsCalib, drop=FALSE])
  calibAdjustmentFactor <- .calibAdapted(Xs=dataGet[ , colsCalib, drop=FALSE], d=initialWeightsGet, q=rep(1,length(initialWeightsGet)),
                                         total=targetSum, method="truncated", bounds=c("low"=0.1, "upp"=10), description=FALSE, max_iter=50)
  # Calculate the weights to later calculate weighted means with.
  weightsGet <- calibAdjustmentFactor * initialWeightsGet
  # Calc means, format as data.frame and return.
  means <- mean.weight(dataGet, cols=colsCalc, weights=weightsGet)
  means <- as.data.frame(t(as.matrix(means)), stringsAsFactors=FALSE)
  return (means)
}
#getMeanCalib(dataFill=spe[1,,drop=FALSE], dataGet=spb[ getNearestObs(data=spe[1,similarityCols[,"name"],drop=FALSE], dataGet=spb[,similarityCols[,"name"]], nObs=200)[1,], , drop=FALSE],
#             colsCalib=similarityCols[,"name"], colsCalc=c("I(Arbeitsverdienst/JAE_FamAK)","LE"))

#' @export
#' @title Checks expressions for errors
#' @description Checks for circular references, erroneouss +/- calculations over several lines, and more (see arguments).
#' @author Daniel Hoop
#' @param expr The expression to check for errors.
#' @param dataFill Optional data.frame in which the expression should be evaluated later on. If not NULL, then the function will check if variables that are being assigned in the expression do not already exist in dataFill. If they already exist, an error will be shown.
#' @param assignmentForbidden Optional character vector that contains variables that should not be assigned.
#' @param outputFile Optional: Character or connection to a file. If circular references were found, all problematic lines will be written into that file.
#' @inheritParams findCircularReferences
#' @return invisible(NULL) if everything is okay. Will cause an error if problems were detected in the expression.
#' @seealso \code{\link{findCircularReferences}}
#' @examples
#' checkExprValidity(quote({a <- 1})) # Nothing happens. Everything is ok.
#' checkExprValidity(quote({
#'   a <- 1
#'   + 2
#' }))
#' # An error will be shown because a new line must not start with a '+' or '-' sign.
#' # For more examples, see also the function 'findCircularReferences'.
checkExprValidity <- function (expr, dataFill = NULL, assignOp = c("<-", "="), assignmentForbidden = NULL, outputFile = NULL) {
  hintTurnOff <- "If you want to suppress this error, set `checkExprForErrors = FALSE`."
  # Assure that a valid expression was given.
  if (class(expr) != "{") # !"srcref"%in%names(attributes(expr)))
    stop ("A quoted expression must be given as argument like `quote({ a <- 1 })`. *NOT*: `expression({ a <- 1 })`. ")
  # Find circular calculations
  ci <- findCircularReferences(expr, assignOp = assignOp)
  if (!is.null(ci)) {
    cat(paste0(ci, collapse="\n"), "\n")
    if (length(outputFile) > 0)
      writeLines(ci, outputFile)
    stop ("Circular references were found in the calculation expression `expr`. Above this error message they should appear in the R console.", " ", hintTurnOff)
  }
  # Check the expression for validity. Preparations.
  expr0 <- .prepareExpression(expr, "expr")
  if (is.null(expr0)) # This happens when an empty expression is given.
    return (NULL)
  lhsVars <- trimws(sapply(strsplit(expr0, "<\\-|="),
                           function (x) if (length(x) > 1) x[1] else NULL))
  # Assure that no line begins with + or -. This is an error in the formula specification.
  plusMinusLines <- expr0[grep("^ *(\\+|\\-)", expr0)]
  if (length(plusMinusLines) > 0)
    stop ("Some lines start with `+` or `-`. This is most probably a misspecification. The +/- signs always have to be at the end of the line! ", hintTurnOff, " Below you see the erroneouss line(s):\n", paste0(plusMinusLines, collapse="\n"))
  # Assure that no forbidden assignments were performed.
  if (any(lhsVars %in% assignmentForbidden))
    stop ("Assignment of values to a variable called '", assignmentForbidden, "' are not allowed! Hence, this is forbidden: ", paste0(assignmentForbidden, " <- a/b", collapse="\n"), "\n", hintTurnOff)
  # Assure that no assignment on the LHS already exists in dataFill.
  if (!is.null(dataFill)) {
    doubleCreation <- lhsVars[lhsVars%in%colnames(dataFill)]
    if (length(doubleCreation) > 0)
      stop (paste0("Some variables are already existent in `dataFill`. They must not be reassigned. ", hintTurnOff, " If you're willing to reassign the variables and keep error checks, first remove these variables from `dataFill` and run the function again: ", paste0(doubleCreation, collapse=", ")))
  }
}

#### **** Most important function **** ####
#' @export
#' @title Imputing data using information from other sources, and domain knowledge
#' @description Fills lacking data in \code{dataFill} using information from \code{dataGet} and calculation rules contained in \code{expr}.
#' @author Daniel Hoop
#' @param dataFill The data.frame that should be filled with addtional information, i.e. the data.frame inside of which \code{expr} should be evaluated.
#' @param dataGet The data.frame from which additional information should be drawn if colums are referenced within the function named \code{lookupFuncName}.
#' @param expr The expression that should be evaluated inside \code{dataFill}. This must be quoted, and not be an actual expression.
#' @param lookupIndexNames For each row, the lookup function will search for the most similar observations in \code{dataGet}. However, you may want to restrict the
#' lookup to certain criteria, e.g. that similar observations must be placed in the same region. In that case, you can specify the colnames for the must have criteria.\cr
#' If NULL (default value), then all observations in dataGet are eglible for matching.
#' @param checkExprForErrors Logical value indicating if the expression should be checked for errors, e.g. for circular references.
#' @param errorOutputFile Optional: If \code{checkExprForErrors = TRUE}, then an output file can be specified, into which the lines that contain circular references will be written.
#' @param distFunc The distance function to be applied to find the most similar observations. See function \code{\link{getNearestObs}} or, e.g., \code{\link[stats:dist]{stats::dist}} for more details.
#' @param similarityCols The columns in \code{dataFill} and \code{dataGet} which will be used to find the most similar observations in \code{dataGet} for each observation in \code{dataFill}.
#' @param maxDist Optional: The maximum distance between observations. See also \code{\link{getNearestObs}}.
#' @param maxObs Optional: The maximum number of observations that should be drawn in \code{dataGet} for each observation in \code{data}. See also \code{\link{getNearestObs}}.
#' @param minObs Optional: The minimum number of observations that should be drawn in \code{dataGet} for each observation in \code{data}. See also \code{\link{getNearestObs}}.
#' @param warnIfToofewObs Logical value indicating if a warning should be given in case there are not enough observations available to satisfy the number given in \code{minObs}.
#' @param shareOfToofewObs The share of observations that should be taken from the remaining observations, in case the number given in \code{minObs} cannot be satisfied.
#' @param distToWeightFunc The function to convert distances into weights. These weights are then used to calculate a weighted average of all similar observations, which is subsequently used for the imputation. See also \code{\link{distToWeight}}.
#' Could be e.g. the inverse value of the distance, or simply 1 for every similar observation.\cr
#' The function must take a distance matrix and return a weight matrix with equal dimensions.
#' @param createLookupFunc The function to create the lookup function. See \code{\link{createLookupWithMeanMethod}} for an example.
#' @param showProgressBar Logical value indicating if a progress bar should be shown during the calculations.
#' @param nCores The number of cores that should be used for the calculations.
#' @param ... Further arguments passed either into \code{distToWeightFunc} or into \code{createLookupFunc}.
#' @inheritParams findCircularReferences
#' @inheritParams getNearestObs
#' @details
#' For performance reasons it is not possible to name the lookup function other than 'lookup'. For an example on how to use the lookup function, see the examples below.\cr
#' If the lookup function is not used inside \code{expr}, then there is no need to include information from \code{dataGet} and thus the computationally intensive procedure of finding the most similar observations will be skipped.\cr
#' If a figure is already available in \code{dataFill} and it will be newly created while evaluation \code{expr}, then an error is thrown.\cr
#' If, for some values in the lookup index, the number of observations in \code{dataGet} is smaller than \code{minObs}, then the maximum amount of available observations is taken from \code{dataGet}, but reduced by the factor 0.5.
#' @return A data.frame containing the same observations as given in \code{dataFill}, but with additional columns as defined in \code{expr}.
#' @examples dataFill <- data.frame(a=1:10, b=11:20, sim1=51:60, ind1=c(1,1,1,2,2,2,3,3,3,3))
#' dataGet <- data.frame(a=21:30, c=31:40, sim1=60:51, ind1=c(3,3,3,3,2,2,2,1,1,1))
#' expr <- quote({
#'   e <- a * b
#'   c <- a * lookup(a + c)
#' })
#' imputeDomainDriven(dataFill = dataFill,
#'                    dataGet = dataGet,
#'                    lookupIndexNames = 'ind1',
#'                    similarityCols = 'sim1',
#'                    minObs = 2,
#'                    expr = expr)
imputeDomainDriven <- function (
  dataFill,
  dataGet,
  expr,
  lookupIndexNames = NULL,
  useIndexIgnoreSimilarity = FALSE,
  checkExprForErrors = TRUE,
  assignOp = c("<-", "="),
  errorOutputFile = NULL,
  distFunc = stats::dist,
  similarityCols,
  maxDist = NULL,
  minObs = NULL,
  maxObs = NULL,
  warnIfToofewObs = FALSE,
  shareOfToofewObs = 0.5,
  distToWeightFunc = distToWeight,
  createLookupFunc = createLookupWithMeanMethod,
  warnScaling = FALSE,
  showProgressBar = nCores == 1,
  nCores = 1,
  ...) {

  # The name of the lookup function that will search for the most similar observations in \code{dataGet} and lookup their values.
  lookupFuncName = "lookup"

  if (showProgressBar && nCores > 1) {
    message("If `nCores > 1`, then the progress bar cannot be shown.")
    showProgressBar <- FALSE
  }
  # Create lookupIndexName artificially, if none was given.
  if (is.null(lookupIndexNames)) {
    kickIndexCol <- TRUE
    lookupIndexNames <- "lookupIndex_asd9fj0293xja01203jnfqop"
    dataFill[,lookupIndexNames] <- as.integer(1)
    dataGet[,lookupIndexNames] <- as.integer(1)
  } else {
    kickIndexCol <- FALSE
  }


  ### Error checks on settings
  if (checkExprForErrors)
    checkExprValidity(expr = expr, dataFill = dataFill, assignOp = assignOp, assignmentForbidden = lookupFuncName, outputFile = errorOutputFile)
  # Assure that all columns named in lookupIndexNames are available in dataFill and dataGet
  notAvail <- lookupIndexNames[!lookupIndexNames%in%c(colnames(dataFill), colnames(dataGet))]
  if (length(notAvail) > 0)
    stop (paste0("The following columns were given in `lookupIndexNames`, but are not available in `dataFill` or `dataGet`: ", paste0(notAvail, collapse=", ")))
  # Assure that similarityCols are available in dataFill and dataGet.
  notAvail <- similarityCols[!similarityCols%in%c(colnames(dataFill), colnames(dataGet))]
  if (length(notAvail) > 0)
    stop (paste0("The following columns were given in `similarityCols`, but are not available in `dataFill` or `dataGet`: ", paste0(notAvail, collapse=", ")))
  rm(notAvail)

  if (!useIndexIgnoreSimilarity && is.null(maxDist) && is.null(minObs))
    stop ("Either `maxDist` or `minObs` must be specified.")

  ### Get most similar observations
  # Only needs to be done, if the lookup function is used in the expression

  if (!any(grepl(paste0(lookupFuncName,"( |\n)*\\("), expr))) {
    # Case, no lookup is used.
    res <- getCol(dataFill, expr=within(dataFill, eval(expr)))

  } else {

    # Create an index that includes all other indices
    lookupIndexNames_cn <- paste(lookupIndexNames, collapse="_")
    lookupIndexNames_cn <- paste0(lookupIndexNames_cn, "_xCI32KnJ6hkc")
    dataFill[,lookupIndexNames_cn] <- paste.cols(dataFill[,lookupIndexNames,drop=FALSE])
    dataGet[,lookupIndexNames_cn] <- paste.cols(dataGet[,lookupIndexNames,drop=FALSE])
    suLookupInd <- sort(unique(c(dataFill[,lookupIndexNames_cn])))

    # First find the most similar farms for each observation in each index level.
    similarList <- lapply(1:length(suLookupInd), function (i) {
      selectedRows <- which(dataFill[,lookupIndexNames_cn] == suLookupInd[i])
      rowsGet <-  which(dataGet[,lookupIndexNames_cn] == suLookupInd[i])
      if (useIndexIgnoreSimilarity) {
        distRes <- list(
          ind = matrix(rep(rowsGet, length(selectedRows)), nrow = length(selectedRows), byrow = TRUE),
          dist = matrix(1, nrow = length(selectedRows), ncol = length(rowsGet))
        )

      } else {
        minObsLoop <- minObs
        if (!is.null(minObs)) {
          if (length(rowsGet) <= minObs) {
            minObsLoop <- floor(shareOfToofewObs * length(rowsGet))
            if (warnIfToofewObs)
              warning (paste0("For '", suLookupInd[i], "' in `lookupIndexNames`, there are not sufficient observations available",
                              " to fulfill the requirement given in `minObs` (=", minObs, ").",
                              " `minObs` was temporarily reduced to ", 100*round(shareOfToofewObs,1), " % of available observations according to the restrictions given by `lookupIndexNames`, thus it becomes: ", minObsLoop, "."))
          }
        }
        distRes <- getNearestObs(
          data = dataFill[selectedRows, similarityCols, drop=FALSE],
          dataGet = dataGet[rowsGet, similarityCols, drop=FALSE],
          maxDist = maxDist,
          minObs = minObsLoop,
          maxObs = maxObs,
          distFunc = distFunc,
          warnScaling = warnScaling)

        distRes[["ind"]] <- replace.values(1:length(rowsGet), rowsGet, distRes[["ind"]])
      }
      return(list(distRes = distRes, selectedRows = selectedRows))
    })

    # All distance matrices have to be harmonized to the same number of columns.
    # Also, the selected rows are added as first column to `dist` and `ind`.
    maxNcol = max(sapply(similarList, function (x) ncol(x[["distRes"]][["dist"]])))
    similarList <- lapply(similarList, function (x) {
      y <- x[["distRes"]]
      colDiff <- maxNcol - ncol(y[["dist"]])
      return(list(
        ind = cbind(x[["selectedRows"]],
                    y[["ind"]],
                    matrix(NA, nrow = nrow(y[["ind"]]), ncol = colDiff)),
        dist = cbind(x[["selectedRows"]],
                     y[["dist"]],
                     matrix(NA, nrow = nrow(y[["dist"]]), ncol = colDiff))))
    })
    # Extract 'ind' from the list, order according to the selected rows and remove the column that marks the selected rows.
    similarInd <- do.call("rbind", lapply(similarList, function(x) x[["ind"]]))
    similarInd <- similarInd[order(similarInd[, 1]), -1, drop = FALSE]
    # Same for 'dist'...
    similarWeights <- do.call("rbind", lapply(similarList, function(x) x[["dist"]]))
    similarWeights <- similarWeights[order(similarWeights[, 1]), -1, drop = FALSE]

    # Create weights from distances.
    oldDim <- dim(similarWeights)
    similarWeights <- distToWeightFunc(similarWeights, ...)
    if (length(dim(similarWeights)) != length(oldDim) || any(dim(similarWeights) != oldDim))
      stop ("The function provided in 'distToWeightFunc' must return an object with the same dimensions as its input.")

    # Add colum that keeps track of row order.
    # With primitive list operation, dataFill will not be copied in RAM. -> Faster.
    dataFill[["rowOder_xiuahwe9f8"]] <- 1:nrow(dataFill)

    # Keep the colum classes in order to restore them later.
    dataFillClasses <- sapply(dataFill, class)

    ### Execute the allocation for each observation in data
    # Prepare progress bar
    if (showProgressBar) {
      progressBar <- utils::txtProgressBar(max=nrow(dataFill), style = 3)
    }

    # Prepare the function
    fillDataFunc <- function (data) {
      # Create the lookup function for the observation
      rowsDataGet <- similarInd[data[,"rowOder_xiuahwe9f8"],]
      weightDataGet <- similarWeights[data[,"rowOder_xiuahwe9f8"],]

      keep <- !is.na(rowsDataGet)
      rowsDataGet <- rowsDataGet[keep]
      weightDataGet <- weightDataGet[keep]

      lookup <- createLookupFunc(dataGet[rowsDataGet,,drop=FALSE], weights=weightDataGet, ...)

      # Show progress bar
      if (showProgressBar) {
        utils::setTxtProgressBar(progressBar, progressBar$getVal()+1)
      }
      # Evaluate and return
      res <- getCol(data, expr=within(data, eval(expr)))
      return (res)
    }

    # To check integrity of data later.
    nrowDataFill <- nrow(dataFill)

    # Execute per observation
    nCores <- max(1, nCores[1])
    if (nCores == 1) {
      # Non paralell version
      # Using data.table is slower, see commit 43c8e925 [search term "data.table"]
      res <- by(dataFill, INDICES = dataFill[["rowOder_xiuahwe9f8"]], FUN = fillDataFunc)

      # Parallel version
    } else {
      # Using dlply is faster than using adply.
      # using foreach is also slower (see commits 43c8e925 [data.table] for last version with commented lines using foreach)
      parClust <- multiCorePrep(nCores)
      res <- plyr::dlply(.data = dataFill, .variables = "rowOder_xiuahwe9f8", .parallel = TRUE, .fun = fillDataFunc)
      multiCorePost(parClust)
    }

    rm(dataFill, dataGet); invisible(gc())

    if (showProgressBar) {
      close(progressBar)
    }

    if (!is.data.frame(res)) {
      # Throw error if observations were lost.
      if (length(res) != nrowDataFill)
        stop ("Internal error. Evaluating `expr` for all observations resulted in a loss of observations. Please contact the package maintainer and give a reproducible example how to get to this error.")
      # Make data.frame
      res <- as.data.frame(do.call("rbind",res), stringsAsFactors=FALSE)
      #res[] <- lapply(res, function(x)unlist(x))
    } else {
      # Throw error if observations were lost.
      if (nrow(res) != nrowDataFill)
        stop ("Internal error. Evaluating `expr` for all observations resulted in a loss of observations. Please contact the package maintainer and give a reproducible example how to get to this error.")
    }
    # Restore original row order
    res <- res[order(res[,"rowOder_xiuahwe9f8"]), !colnames(res)%in%c("rowOder_xiuahwe9f8", lookupIndexNames_cn)]
    if (kickIndexCol) {
      res <- res[, !colnames(res) %in% lookupIndexNames]
    }
  }
  return (res)
}
