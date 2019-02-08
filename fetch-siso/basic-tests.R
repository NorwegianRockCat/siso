# We are dependent on the pysch package
library(psych)
library(dplyr)

readFetchSisoData <- function() {
    results <- read.csv("results.csv", header = TRUE)
    rename(results,  Experience.Robots = Experience.with.Robots.Unedited)
}


tws.Movement0VariableNames = list(
    GSAnthro1 = quo(GSAnthro1),
    GSAnthro2 = quo(GSAnthro2),
    GSAnthro3 = quo(GSAnthro3),
    GSAnthro4 = quo(GSAnthro4),
    GSAnthro5 = quo(GSAnthro5),

    GSAnimacy1 = quo(GSAnimacy1),
    GSAnimacy2 = quo(GSAnimacy2),
    GSAnimacy3 = quo(GSAnimacy3),
    GSAnimacy4 = quo(GSAnimacy4),
    GSAnimacy5 = quo(GSAnimacy5),

    PM1 = quo(PM1),

    GSL1 = quo(GSL1),
    GSL2 = quo(GSL2),
    GSL3 = quo(GSL3),
    GSL4 = quo(GSL4),
    GSL5 = quo(GSL5),

    GSI1 = quo(GSI1),
    GSI2 = quo(GSI2),
    GSI3 = quo(GSI3),
    GSI4 = quo(GSI4),
    GSI5 = quo(GSI5),
    GSI6 = quo(GSI6),

    GSS1 = quo(GSS1),
    GSS2 = quo(GSS2),
    GSS3 = quo(GSS3))

tws.Movement1VariableNames = list(
    GSAnthro1 = quo(GSAnthro1.1),
    GSAnthro2 = quo(GSAnthro1.2),
    GSAnthro3 = quo(GSAnthro1.3),
    GSAnthro4 = quo(GSAnthro1.4),
    GSAnthro5 = quo(GSAnthro1.5),

    GSAnimacy1 = quo(GSAnimacy1.1),
    GSAnimacy2 = quo(GSAnimacy1.2),
    GSAnimacy3 = quo(GSAnimacy1.3),
    GSAnimacy4 = quo(GSAnimacy1.4),
    GSAnimacy5 = quo(GSAnimacy1.5),

    PM1 = quo(PM1.1),

    GSL1 = quo(GSL1.1),
    GSL2 = quo(GSL1.2),
    GSL3 = quo(GSL1.3),
    GSL4 = quo(GSL1.4),
    GSL5 = quo(GSL1.5),

    GSI1 = quo(GSI1.1),
    GSI2 = quo(GSI1.2),
    GSI3 = quo(GSI1.3),
    GSI4 = quo(GSI1.4),
    GSI5 = quo(GSI1.5),
    GSI6 = quo(GSI1.6),

    GSS1 = quo(GSS1.1),
    GSS2 = quo(GSS1.2),
    GSS3 = quo(GSS1.3))

tws.Movement2VariableNames = list(
            GSAnthro1 = quo(GSAnthro2.1),
            GSAnthro2 = quo(GSAnthro2.2),
            GSAnthro3 = quo(GSAnthro2.3),
            GSAnthro4 = quo(GSAnthro2.4),
            GSAnthro5 = quo(GSAnthro2.5),

            GSAnimacy1 = quo(GSAnimacy2.1),
            GSAnimacy2 = quo(GSAnimacy2.2),
            GSAnimacy3 = quo(GSAnimacy2.3),
            GSAnimacy4 = quo(GSAnimacy2.4),
            GSAnimacy5 = quo(GSAnimacy2.5),

            PM1 = quo(PM2.1),

            GSL1 = quo(GSL2.1),
            GSL2 = quo(GSL2.2),
            GSL3 = quo(GSL2.3),
            GSL4 = quo(GSL2.4),
            GSL5 = quo(GSL2.5),

            GSI1 = quo(GSI2.1),
            GSI2 = quo(GSI2.2),
            GSI3 = quo(GSI2.3),
            GSI4 = quo(GSI2.4),
            GSI5 = quo(GSI2.5),
            GSI6 = quo(GSI2.6),

            GSS1 = quo(GSS2.1),
            GSS2 = quo(GSS2.2),
            GSS3 = quo(GSS2.3))

tws.Movement3VariableNames = list(
            GSAnthro1 = quo(GSAnthro3.1),
            GSAnthro2 = quo(GSAnthro3.2),
            GSAnthro3 = quo(GSAnthro3.3),
            GSAnthro4 = quo(GSAnthro3.4),
            GSAnthro5 = quo(GSAnthro3.5),

            GSAnimacy1 = quo(GSAnimacy3.1),
            GSAnimacy2 = quo(GSAnimacy3.2),
            GSAnimacy3 = quo(GSAnimacy3.3),
            GSAnimacy4 = quo(GSAnimacy3.4),
            GSAnimacy5 = quo(GSAnimacy3.5),

            PM1 = quo(PM3.1),

            GSL1 = quo(GSL3.1),
            GSL2 = quo(GSL3.2),
            GSL3 = quo(GSL3.3),
            GSL4 = quo(GSL3.4),
            GSL5 = quo(GSL3.5),

            GSI1 = quo(GSI3.1),
            GSI2 = quo(GSI3.2),
            GSI3 = quo(GSI3.3),
            GSI4 = quo(GSI3.4),
            GSI5 = quo(GSI3.5),
            GSI6 = quo(GSI3.6),

            GSS1 = quo(GSS3.1),
            GSS2 = quo(GSS3.2),
            GSS3 = quo(GSS3.3))


tws.Movement4VariableNames = list(
            GSAnthro1 = quo(GSAnthro4.1),
            GSAnthro2 = quo(GSAnthro4.2),
            GSAnthro3 = quo(GSAnthro4.3),
            GSAnthro4 = quo(GSAnthro4.4),
            GSAnthro5 = quo(GSAnthro4.5),

            GSAnimacy1 = quo(GSAnimacy4.1),
            GSAnimacy2 = quo(GSAnimacy4.2),
            GSAnimacy3 = quo(GSAnimacy4.3),
            GSAnimacy4 = quo(GSAnimacy4.4),
            GSAnimacy5 = quo(GSAnimacy4.5),

            PM1 = quo(PM4.1),

            GSL1 = quo(GSL4.1),
            GSL2 = quo(GSL4.2),
            GSL3 = quo(GSL4.3),
            GSL4 = quo(GSL4.4),
            GSL5 = quo(GSL4.5),

            GSI1 = quo(GSI4.1),
            GSI2 = quo(GSI4.2),
            GSI3 = quo(GSI4.3),
            GSI4 = quo(GSI4.4),
            GSI5 = quo(GSI4.5),
            GSI6 = quo(GSI4.6),

            GSS1 = quo(GSS4.1),
            GSS2 = quo(GSS4.2),
            GSS3 = quo(GSS4.3))

tws.MovementVariableNames = list(tws.Movement1VariableNames, tws.Movement2VariableNames, tws.Movement3VariableNames, tws.Movement4VariableNames, tws.Movement0VariableNames)


# Return a list of psych alpha results, one for each movement thing
listOfAlphasForSisoData <- function(resultsDataFrame, iteration = 1) {

    # Pull the frames out, put calculate the index
    anthroIndex <- 6 + (iteration - 1) * 26

    # The numbers on the side are calculated based on the first iteration
    anthroEndIndex <- anthroIndex + 4 # 10

    animacyIndex <- anthroEndIndex + 2 # 12
    animacyEndIndex <- animacyIndex + 4 # 16

    likeabilityIndex <- animacyEndIndex + 1 # 17
    likeabilityEndIndex <- likeabilityIndex + 4 # 21

    intelligenceIndex <- likeabilityEndIndex + 1 # 22
    intelligenceEndIndex <- intelligenceIndex + 5 # 27

    safetyIndex <- intelligenceEndIndex + 1 # 28
    safetyEndIndex <- safetyIndex + 2 # 30

    anthroFrame <- resultsDataFrame[anthroIndex:anthroEndIndex]
    animacyFrame <- resultsDataFrame[animacyIndex:animacyEndIndex]
    likeabilityFrame <- resultsDataFrame[likeabilityIndex:likeabilityEndIndex]
    intelligenceFrame <- resultsDataFrame[intelligenceIndex:intelligenceEndIndex]
    safetyFrame <- resultsDataFrame[safetyIndex:safetyEndIndex]

    alphaAnthro <- alpha(anthroFrame)
    alphaAnimacy <- alpha(animacyFrame)
    alphaLikeability <- alpha(likeabilityFrame)
    alphaInt <- alpha(intelligenceFrame)
    alphaSafety <- alpha(safetyFrame, keys = c(paste("GSS", iteration, ".1", sep = '')))
    list(anthro=alphaAnthro, animacy=alphaAnimacy, likeability=alphaLikeability, int=alphaInt, safety=alphaSafety)
}

filter_movement_for_instance <- function(resultsDataFrame, instance, movementType = 'Siso') {
    quo_instance_var <- enquo(instance)
    filter(resultsDataFrame, !!quo_instance_var == movementType)
}

tws.is_movement <- function(x) { grepl("Movement.", x, fixed = TRUE) }

tws.find_variable_names_for_movement <- function(group_var) {
    movement_list <- Filter(f = tws.is_movement, Map(f = quo_name, group_var))
    variable_list <- list()

    if (length(movement_list) > 0) {
        movement <- movement_list[[1]]
        if (movement == "Movement.1") {
            variable_list <- tws.MovementVariableNames[[1]]
        } else if (movement == "Movement.2") {
            variable_list <- tws.MovementVariableNames[[2]]
        } else if (movement == "Movement.3") {
            variable_list <- tws.MovementVariableNames[[3]]
        } else if (movement == "Movement.2") {
            variable_list <- tws.MovementVariableNames[[4]]
        }

    } else {
        variable_list <- tws.MovementVariableNames[[5]]
    }
    variable_list
}

summary_averages_for_movement <- function(resultsDataFrame, ...) {
    # All the variables are hard coded for the moment, but this will change.
    group_var <- quos(...)

    variable_list <- tws.find_variable_names_for_movement(group_var)
    
    resultsDataFrame %>%
        group_by(!!!group_var) %>%
        tws.summary(mean, variable_list)
}

summary_medians_for_movement <- function(resultsDataFrame, ...) {
    # All the variables are hard coded for the moment, but this will change.
    group_var <- quos(...)

    movement_list <- Filter(f = is_movement, Map(f = quo_name, group_var))

    variable_list <- tws.find_variable_names_for_movement(group_var)

    resultsDataFrame %>%
        group_by(!!!group_var) %>%
        tws.summary(median, variable_list)
}

tws.summary <- function(resultsDataFrame, func, variable_list) {
    resultsDataFrame %>%
        summarize(
            count = n(),
            anthro1 = func(!!variable_list$GSAnthro1, na.rm = TRUE),
            anthro2 = func(!!variable_list$GSAnthro2, na.rm = TRUE),
            anthro3 = func(!!variable_list$GSAnthro3, na.rm = TRUE),
            anthro4 = func(!!variable_list$GSAnthro4, na.rm = TRUE),
            anthro5 = func(!!variable_list$GSAnthro5, na.rm = TRUE),
            pm = func(!!variable_list$PM1, na.rm = TRUE),
            animacy1 = func(!!variable_list$GSAnimacy1, na.rm = TRUE),
            animacy2 = func(!!variable_list$GSAnimacy2, na.rm = TRUE),
            animacy3 = func(!!variable_list$GSAnimacy3, na.rm = TRUE),
            animacy4 = func(!!variable_list$GSAnimacy4, na.rm = TRUE),
            animacy5 = func(!!variable_list$GSAnimacy5, na.rm = TRUE),
            like1 = func(!!variable_list$GSL1, na.rm = TRUE),
            like2 = func(!!variable_list$GSL2, na.rm = TRUE),
            like3 = func(!!variable_list$GSL3, na.rm = TRUE),
            like4 = func(!!variable_list$GSL4, na.rm = TRUE),
            like5 = func(!!variable_list$GSL5, na.rm = TRUE),
            intel1 = func(!!variable_list$GSI1, na.rm = TRUE),
            intel2 = func(!!variable_list$GSI2, na.rm = TRUE),
            intel3 = func(!!variable_list$GSI3, na.rm = TRUE),
            intel4 = func(!!variable_list$GSI4, na.rm = TRUE),
            intel5 = func(!!variable_list$GSI5, na.rm = TRUE),
            intel6 = func(!!variable_list$GSI6, na.rm = TRUE),
            safe1 = func(!!variable_list$GSS1, na.rm = TRUE),
            safe2 = func(!!variable_list$GSS2, na.rm = TRUE),
            safe3 = func(!!variable_list$GSS3, na.rm = TRUE))
}

# Select each variables and put them together.
allEncounters <- function(resultsDataFrame) {
   move1 <- select(resultsDataFrame, movement = Movement.1, !!!tws.Movement1VariableNames)
   move2 <- select(resultsDataFrame, movement = Movement.2, !!!tws.Movement2VariableNames)
   move3 <- select(resultsDataFrame, movement = Movement.3, !!!tws.Movement3VariableNames)
   move4 <- select(resultsDataFrame, movement = Movement.4, !!!tws.Movement4VariableNames)

   bind_rows(move1, move2, move3, move4)
}

allSisoEncounters <- function(resultsDataFrame) {
    allmoves <- allEncounters(resultsDataFrame)
    filter(allmoves, movement == "Siso")
}

allLinearEncounters <- function(resultsDataFrame) {
    allmoves <- allEncounters(resultsDataFrame)
    filter(allmoves, movement == "Linear")
}
