# We are dependent on the pysch package
library(psych)

readFetchSisoData <- function() {
    results <- read.csv("results.csv", header = TRUE)
    results
}

# Return a list of psych alpha results, one for each movement thing
listOfAlphasForSisoData <- function(resultsDataFrame) {
    # Pull these out to make life a little easier
    anthroFrame <- resultsDataFrame[6:10]
    animacyFrame <- resultsDataFrame[12:16]
    likeabilityFrame <- resultsDataFrame[17:21]
    intelligenceFrame <- resultsDataFrame[22:27]
    safetyFrame <- resultsDataFrame[28:30]

    alphaAnthro <- alpha(anthroFrame)
    alphaAnimacy <- alpha(animacyFrame)
    alphaLikeability <- alpha(likeabilityFrame)
    alphaInt <- alpha(intelligenceFrame)
    alphaSafety <- alpha(safetyFrame, keys = c("GSS1.1"))
    listOfAlphas <- list(anthro=alphaAnthro, animacy=alphaAnimacy, likeability=alphaLikeability, int=alphaInt, safety=alphaSafety)
    listOfAlphas
}
