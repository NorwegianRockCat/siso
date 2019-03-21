require(psych)
require(tidyverse)

tidyFetchSisoResults <- function() {
    movement <- c("Siso", "Linear")
    results <- read_csv("results.csv", col_types = cols(
                                           ID = col_integer(),
                                           Age = col_integer(),
                                           Gender = col_factor(levels = c("M", "F")),
                                           Experience.Robots = col_factor(levels = c("Yes", "No")),

                                           Movement.1 = col_factor(movement),
                                           Movement.2 = col_factor(movement),
                                           Movement.3 = col_factor(movement),
                                           Movement.4 = col_factor(movement),

                                           GSAnthro1.1 = col_integer(),
                                           GSAnthro1.2 = col_integer(),
                                           GSAnthro1.3 = col_integer(),
                                           GSAnthro1.4 = col_integer(),
                                           GSAnthro1.5 = col_integer(),
                                           PM1.1 = col_integer(),
                                           GSAnimacy1.1 = col_integer(),
                                           GSAnimacy1.2 = col_integer(),
                                           GSAnimacy1.3 = col_integer(),
                                           GSAnimacy1.4 = col_integer(),
                                           GSAnimacy1.5 = col_integer(),
                                           GSL1.1 = col_integer(),
                                           GSL1.2 = col_integer(),
                                           GSL1.3 = col_integer(),
                                           GSL1.4 = col_integer(),
                                           GSL1.5 = col_integer(),
                                           GSI1.1 = col_integer(),
                                           GSI1.2 = col_integer(),
                                           GSI1.3 = col_integer(),
                                           GSI1.4 = col_integer(),
                                           GSI1.5 = col_integer(),
                                           GSI1.6 = col_integer(),
                                           GSS1.1 = col_integer(),
                                           GSS1.2 = col_integer(),
                                           GSS1.3 = col_integer(),

                                           GSAnthro2.1 = col_integer(),
                                           GSAnthro2.2 = col_integer(),
                                           GSAnthro2.3 = col_integer(),
                                           GSAnthro2.4 = col_integer(),
                                           GSAnthro2.5 = col_integer(),
                                           PM2.1 = col_integer(),
                                           GSAnimacy2.1 = col_integer(),
                                           GSAnimacy2.2 = col_integer(),
                                           GSAnimacy2.3 = col_integer(),
                                           GSAnimacy2.4 = col_integer(),
                                           GSAnimacy2.5 = col_integer(),
                                           GSL2.1 = col_integer(),
                                           GSL2.2 = col_integer(),
                                           GSL2.3 = col_integer(),
                                           GSL2.4 = col_integer(),
                                           GSL2.5 = col_integer(),
                                           GSI2.1 = col_integer(),
                                           GSI2.2 = col_integer(),
                                           GSI2.3 = col_integer(),
                                           GSI2.4 = col_integer(),
                                           GSI2.5 = col_integer(),
                                           GSI2.6 = col_integer(),
                                           GSS2.1 = col_integer(),
                                           GSS2.2 = col_integer(),
                                           GSS2.3 = col_integer(),

                                           GSAnthro3.1 = col_integer(),
                                           GSAnthro3.2 = col_integer(),
                                           GSAnthro3.3 = col_integer(),
                                           GSAnthro3.4 = col_integer(),
                                           GSAnthro3.5 = col_integer(),
                                           PM3.1 = col_integer(),
                                           GSAnimacy3.1 = col_integer(),
                                           GSAnimacy3.2 = col_integer(),
                                           GSAnimacy3.3 = col_integer(),
                                           GSAnimacy3.4 = col_integer(),
                                           GSAnimacy3.5 = col_integer(),
                                           GSL3.1 = col_integer(),
                                           GSL3.2 = col_integer(),
                                           GSL3.3 = col_integer(),
                                           GSL3.4 = col_integer(),
                                           GSL3.5 = col_integer(),
                                           GSI3.1 = col_integer(),
                                           GSI3.2 = col_integer(),
                                           GSI3.3 = col_integer(),
                                           GSI3.4 = col_integer(),
                                           GSI3.5 = col_integer(),
                                           GSI3.6 = col_integer(),
                                           GSS3.1 = col_integer(),
                                           GSS3.2 = col_integer(),
                                           GSS3.3 = col_integer(),

                                           GSAnthro4.1 = col_integer(),
                                           GSAnthro4.2 = col_integer(),
                                           GSAnthro4.3 = col_integer(),
                                           GSAnthro4.4 = col_integer(),
                                           GSAnthro4.5 = col_integer(),
                                           PM4.1 = col_integer(),
                                           GSAnimacy4.1 = col_integer(),
                                           GSAnimacy4.2 = col_integer(),
                                           GSAnimacy4.3 = col_integer(),
                                           GSAnimacy4.4 = col_integer(),
                                           GSAnimacy4.5 = col_integer(),
                                           GSL4.1 = col_integer(),
                                           GSL4.2 = col_integer(),
                                           GSL4.3 = col_integer(),
                                           GSL4.4 = col_integer(),
                                           GSL4.5 = col_integer(),
                                           GSI4.1 = col_integer(),
                                           GSI4.2 = col_integer(),
                                           GSI4.3 = col_integer(),
                                           GSI4.4 = col_integer(),
                                           GSI4.5 = col_integer(),
                                           GSI4.6 = col_integer(),
                                           GSS4.1 = col_integer(),
                                           GSS4.2 = col_integer(),
                                           GSS4.3 = col_integer()))
                                        # Now gather the different movements together
    results <- reshape(as.data.frame(results), idvar="ID", direction = "long",
                       varying = list(Movement = c(5,31,57,83),
                                      GSAnthro1 = c(6,32,58,84),
                                      GSAnthro2 = c(7,33,59,85),
                                      GSAnthro3 = c(8,34,60,86),
                                      GSAnthro4 = c(9,35,61,87),
                                      GSAnthro5 = c(10,36,62,88),
                                      PM1 = c(11,37,63,89),
                                      GSAnimacy1 = c(12,38,64,90),
                                      GSAnimacy2 = c(13,39,65,91),
                                      GSAnimacy3 = c(14,40,66,92),
                                      GSAnimacy4 = c(15,41,67,93),
                                      GSAnimacy5 = c(16,42,68,94),
                                      GSL1 = c(17,43,69,95),
                                      GSL2 = c(18,44,70,96),
                                      GSL3 = c(19,45,71,97),
                                      GSL4 = c(20,46,72,98),
                                      GSL5 = c(21,47,73,99),
                                      GSI1 = c(22,48,74,100),
                                      GSI2 = c(23,49,75,101),
                                      GSI3 = c(24,50,76,102),
                                      GSI4 = c(25,51,77,103),
                                      GSI5 = c(26,52,78,104),
                                      GSI6 = c(27,53,79,105),
                                      GSS1 = c(28,54,80,106),
                                      GSS2 = c(29,55,81,107),
                                      GSS3 = c(30,56,82,108)),
                       v.names = c("Movement",
                                   "GSAnthro1",
                                   "GSAnthro2",
                                   "GSAnthro3",
                                   "GSAnthro4",
                                   "GSAnthro5",
                                   "PM1",
                                   "GSAnimacy1",
                                   "GSAnimacy2",
                                   "GSAnimacy3",
                                   "GSAnimacy4",
                                   "GSAnimacy5",
                                   "GSL1",
                                   "GSL2",
                                   "GSL3",
                                   "GSL4",
                                   "GSL5",
                                   "GSI1",
                                   "GSI2",
                                   "GSI3",
                                   "GSI4",
                                   "GSI5",
                                   "GSI6",
                                   "GSS1",
                                   "GSS2",
                                   "GSS3"))
    results <- as_tibble(results)
    dplyr::mutate(results, GSAnthro.avg = rowMeans(data.frame(GSAnthro1, GSAnthro2, GSAnthro3, GSAnthro4, GSAnthro5)),
                              GSAnimacy.avg = rowMeans(data.frame(GSAnimacy1, GSAnimacy2, GSAnimacy3, GSAnimacy4, GSAnimacy5)),
                              GSL.avg = rowMeans(data.frame(GSL1, GSL2, GSL3, GSL4, GSL5)),
                              GSI.avg = rowMeans(data.frame(GSI1, GSI2, GSI3, GSI4, GSI5, GSI6)),
                              GSS.avg = rowMeans(data.frame(GSS1, GSS2, GSS3)))
}

rawFetchSisoResults <- function() {
    read.csv("results.csv", header = TRUE)
}

MovementVariableNames <- function() {
    tws.AverageVariableNames <- list(
        Siso.GSAnthro1 = quo(Siso.GSAnthro1),
        Siso.GSAnthro2 = quo(Siso.GSAnthro2),
        Siso.GSAnthro3 = quo(Siso.GSAnthro3),
        Siso.GSAnthro4 = quo(Siso.GSAnthro4),
        Siso.GSAnthro5 = quo(Siso.GSAnthro5),
        Siso.PM1 = quo(Siso.PM1),
        Siso.GSAnimacy1 = quo(Siso.GSAnimacy1),
        Siso.GSAnimacy2 = quo(Siso.GSAnimacy2),
        Siso.GSAnimacy3 = quo(Siso.GSAnimacy3),
        Siso.GSAnimacy4 = quo(Siso.GSAnimacy4),
        Siso.GSAnimacy5 = quo(Siso.GSAnimacy5),
        Siso.GSL1 = quo(Siso.GSL1),
        Siso.GSL2 = quo(Siso.GSL2),
        Siso.GSL3 = quo(Siso.GSL3),
        Siso.GSL4 = quo(Siso.GSL4),
        Siso.GSL5 = quo(Siso.GSL5),
        Siso.GSI1 = quo(Siso.GSI1),
        Siso.GSI2 = quo(Siso.GSI2),
        Siso.GSI3 = quo(Siso.GSI3),
        Siso.GSI4 = quo(Siso.GSI4),
        Siso.GSI5 = quo(Siso.GSI5),
        Siso.GSI6 = quo(Siso.GSI6),
        Siso.GSS1 = quo(Siso.GSS1),
        Siso.GSS2 = quo(Siso.GSS2),
        Siso.GSS3 = quo(Siso.GSS3),
        Linear.GSAnthro1 = quo(Linear.GSAnthro1),
        Linear.GSAnthro2 = quo(Linear.GSAnthro2),
        Linear.GSAnthro3 = quo(Linear.GSAnthro3),
        Linear.GSAnthro4 = quo(Linear.GSAnthro4),
        Linear.GSAnthro5 = quo(Linear.GSAnthro5),
        Linear.PM1 = quo(Linear.PM1),
        Linear.GSAnimacy1 = quo(Linear.GSAnimacy1),
        Linear.GSAnimacy2 = quo(Linear.GSAnimacy2),
        Linear.GSAnimacy3 = quo(Linear.GSAnimacy3),
        Linear.GSAnimacy4 = quo(Linear.GSAnimacy4),
        Linear.GSAnimacy5 = quo(Linear.GSAnimacy5),
        Linear.GSL1 = quo(Linear.GSL1),
        Linear.GSL2 = quo(Linear.GSL2),
        Linear.GSL3 = quo(Linear.GSL3),
        Linear.GSL4 = quo(Linear.GSL4),
        Linear.GSL5 = quo(Linear.GSL5),
        Linear.GSI1 = quo(Linear.GSI1),
        Linear.GSI2 = quo(Linear.GSI2),
        Linear.GSI3 = quo(Linear.GSI3),
        Linear.GSI4 = quo(Linear.GSI4),
        Linear.GSI5 = quo(Linear.GSI5),
        Linear.GSI6 = quo(Linear.GSI6),
        Linear.GSS1 = quo(Linear.GSS1),
        Linear.GSS2 = quo(Linear.GSS2),
        Linear.GSS3 = quo(Linear.GSS3))

    tws.Movement0VariableNames <- list(
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

        PM1 = quo(PM1),

        GSS1 = quo(GSS1),
        GSS2 = quo(GSS2),
        GSS3 = quo(GSS3))

    tws.Movement1VariableNames <- list(
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

        PM1 = quo(PM1.1),

        GSS1 = quo(GSS1.1),
        GSS2 = quo(GSS1.2),
        GSS3 = quo(GSS1.3))

    tws.Movement2VariableNames <- list(
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

        PM1 = quo(PM2.1),

        GSS1 = quo(GSS2.1),
        GSS2 = quo(GSS2.2),
        GSS3 = quo(GSS2.3))

    tws.Movement3VariableNames <- list(
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

        PM1 = quo(PM3.1),

        GSS1 = quo(GSS3.1),
        GSS2 = quo(GSS3.2),
        GSS3 = quo(GSS3.3))

    tws.Movement4VariableNames <- list(
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

        PM1 = quo(PM4.1),

        GSS1 = quo(GSS4.1),
        GSS2 = quo(GSS4.2),
        GSS3 = quo(GSS4.3))
    list(movement.1 = tws.Movement1VariableNames, movement.2 = tws.Movement2VariableNames, movement.3 = tws.Movement3VariableNames, movement.4 = tws.Movement4VariableNames, movement.none = tws.Movement0VariableNames, movement.average = tws.AverageVariableNames)
}

tws.alphaForFrames <- function(anthroFrame, animacyFrame, likeabilityFrame, intelligenceFrame, safetyFrame, safetyFrame.plus.prediction) {
    alphaAnthro <- psych::alpha(anthroFrame)
    alphaAnimacy <- psych::alpha(animacyFrame)
    alphaLikeability <- psych::alpha(likeabilityFrame)
    alphaInt <- psych::alpha(intelligenceFrame)
    alphaSafety <- psych::alpha(safetyFrame, keys = c("GSS1"))
    alphaSafety.plus.prediction <- psych::alpha(safetyFrame.plus.prediction, keys = c("GSS1", "PM1"))
    list(anthro=alphaAnthro, animacy=alphaAnimacy, likeability=alphaLikeability, int=alphaInt, safety=alphaSafety, safetyPlus=alphaSafety.plus.prediction)
}

alphaAllEncounters <- function(df = tidyFetchSisoResults()) {
    anthroFrame <- df %>% dplyr::select(GSAnthro1:GSAnthro5)
    animacyFrame <- df %>% dplyr::select(GSAnimacy1:GSAnimacy5)
    likeabilityFrame <- df %>% dplyr::select(GSL1:GSL5)
    intelligenceFrame <- df %>% dplyr::select(GSI1:GSI6)
    safetyFrame <- df %>% dplyr::select(GSS1:GSS3)
    safetyFrame.plus.prediction <- df %>% dplyr::select(PM1, GSS1:GSS3)

    tws.alphaForFrames(anthroFrame, animacyFrame, likeabilityFrame, intelligenceFrame, safetyFrame, safetyFrame.plus.prediction)
}

filter_movement_for_instance <- function(resultsDataFrame, instance, movementType = 'Siso') {
    quo_instance_var <- enquo(instance)
    dplyr::filter(resultsDataFrame, !!quo_instance_var == movementType)
}

tws.is_movement <- function(x) { grepl("Movement.", x, fixed = TRUE) }

tws.find_variable_names_for_movement <- function(group_var) {
    movement_list <- Filter(f = tws.is_movement, Map(f = quo_name, group_var))
    variable_names = MovementVariableNames()

    if (length(movement_list) > 0) {
         variable_names[[tolower(movement_list[[1]])]]
    } else {
         variable_names[[5]]
    }
}

summary_averages_for_movement <- function(resultsDataFrame, ...) {
    # All the variables are hard coded for the moment, but this will change.
    group_var <- quos(...)

    variable_list <- tws.find_variable_names_for_movement(group_var)
    
    resultsDataFrame %>%
        dplyr::group_by(!!!group_var) %>%
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
        dpylr::summarize(
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
allEncounters <- function(resultsDataFrame = rawFetchSisoResults()) {
    variable.names <- MovementVariableNames()
    move1 <- dplyr::select(resultsDataFrame, movement = Movement.1, !!!variable.names$movement.1)
    move2 <- dplyr::select(resultsDataFrame, movement = Movement.2, !!!variable.names$movement.2)
    move3 <- dplyr::select(resultsDataFrame, movement = Movement.3, !!!variable.names$movement.3)
    move4 <- dplyr::select(resultsDataFrame, movement = Movement.4, !!!variable.names$movement.4)

    bind_rows(move1, move2, move3, move4)
}

allSisoEncounters <- function(df = allEncounters()) {
    df %>% dplyr::filter(movement == "Siso")
}

allLinearEncounters <- function(df = allEncounters()) {
    df %>% dplyr::filter(movement == "Linear")
}

siso.and.linear.godspeed.compenent.averages <- function(df = rawFetchSisoResults()) {
    df %>% dplyr::mutate(Siso.GSAnthro1 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSAnthro1.1, GSAnthro2.1), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnthro1.1, GSAnthro3.1), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro1.1, GSAnthro4.1), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnthro2.1, GSAnthro3.1), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro2.1, GSAnthro4.1), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro3.1, GSAnthro4.1), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSAnthro2 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSAnthro1.2, GSAnthro2.2), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnthro1.2, GSAnthro3.2), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro1.2, GSAnthro4.2), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnthro2.2, GSAnthro3.2), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro2.2, GSAnthro4.2), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro3.2, GSAnthro4.2), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSAnthro3 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSAnthro1.3, GSAnthro2.3), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnthro1.3, GSAnthro3.3), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro1.3, GSAnthro4.3), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnthro2.3, GSAnthro3.3), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro2.3, GSAnthro4.3), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro3.3, GSAnthro4.3), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSAnthro4 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSAnthro1.4, GSAnthro2.4), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnthro1.4, GSAnthro3.4), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro1.4, GSAnthro4.4), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnthro2.4, GSAnthro3.4), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro2.4, GSAnthro4.4), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro3.4, GSAnthro4.4), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSAnthro5 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSAnthro1.5, GSAnthro2.5), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnthro1.5, GSAnthro3.5), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro1.5, GSAnthro4.5), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnthro2.5, GSAnthro3.5), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro2.5, GSAnthro4.5), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro3.5, GSAnthro4.5), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.PM1 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(PM1.1, PM2.1), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(PM1.1, PM3.1), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(PM1.1, PM4.1), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(PM2.1, PM3.1), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(PM2.1, PM4.1), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(PM3.1, PM4.1), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSAnimacy1 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.1, GSAnimacy2.1), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.1, GSAnimacy3.1), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.1, GSAnimacy4.1), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnimacy2.1, GSAnimacy3.1), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy2.1, GSAnimacy4.1), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy3.1, GSAnimacy4.1), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSAnimacy2 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.2, GSAnimacy2.2), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.2, GSAnimacy3.2), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.2, GSAnimacy4.2), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnimacy2.2, GSAnimacy3.2), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy2.2, GSAnimacy4.2), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy3.2, GSAnimacy4.2), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSAnimacy3 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.3, GSAnimacy2.3), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.3, GSAnimacy3.3), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.3, GSAnimacy4.3), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnimacy2.3, GSAnimacy3.3), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy2.3, GSAnimacy4.3), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy3.3, GSAnimacy4.3), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSAnimacy4 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.4, GSAnimacy2.4), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.4, GSAnimacy3.4), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.4, GSAnimacy4.4), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnimacy2.4, GSAnimacy3.4), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy2.4, GSAnimacy4.4), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy3.4, GSAnimacy4.4), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSAnimacy5 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.5, GSAnimacy2.5), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.5, GSAnimacy3.5), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.5, GSAnimacy4.5), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnimacy2.5, GSAnimacy3.5), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy2.5, GSAnimacy4.5), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy3.5, GSAnimacy4.5), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSL1 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSL1.1, GSL2.1), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSL1.1, GSL3.1), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL1.1, GSL4.1), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSL2.1, GSL3.1), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL2.1, GSL4.1), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL3.1, GSL4.1), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSL2 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSL1.2, GSL2.2), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSL1.2, GSL3.2), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL1.2, GSL4.2), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSL2.2, GSL3.2), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL2.2, GSL4.2), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL3.2, GSL4.2), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSL3 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSL1.2, GSL2.2), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSL1.2, GSL3.2), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL1.2, GSL4.2), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSL2.2, GSL3.2), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL2.2, GSL4.2), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL3.2, GSL4.2), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSL4 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSL1.4, GSL2.4), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSL1.4, GSL3.4), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL1.4, GSL4.4), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSL2.4, GSL3.4), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL2.4, GSL4.4), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL3.4, GSL4.4), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSL5 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSL1.5, GSL2.5), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSL1.5, GSL3.5), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL1.5, GSL4.5), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSL2.5, GSL3.5), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL2.5, GSL4.5), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL3.5, GSL4.5), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSI1 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSI1.1, GSI2.1), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSI1.1, GSI3.1), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI1.1, GSI4.1), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSI2.1, GSI3.1), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI2.1, GSI4.1), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI3.1, GSI4.1), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSI2 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSI1.2, GSI2.2), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSI1.2, GSI3.2), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI1.2, GSI4.2), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSI2.2, GSI3.2), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI2.2, GSI4.2), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI3.2, GSI4.2), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSI3 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSI1.3, GSI2.3), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSI1.3, GSI3.3), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI1.3, GSI4.3), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSI2.3, GSI3.3), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI2.3, GSI4.3), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI3.3, GSI4.3), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSI4 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSI1.4, GSI2.4), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSI1.4, GSI3.4), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI1.4, GSI4.4), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSI2.4, GSI3.4), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI2.4, GSI4.4), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI3.4, GSI4.4), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSI5 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSI1.5, GSI2.5), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSI1.5, GSI3.5), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI1.5, GSI4.5), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSI2.5, GSI3.5), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI2.5, GSI4.5), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI3.5, GSI4.5), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSI6 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSI1.6, GSI2.6), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSI1.6, GSI3.6), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI1.6, GSI4.6), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSI2.6, GSI3.6), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI2.6, GSI4.6), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI3.6, GSI4.6), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSS1 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSS1.1, GSS2.1), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSS1.1, GSS3.1), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSS1.1, GSS4.1), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSS2.1, GSS3.1), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSS2.1, GSS4.1), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSS3.1, GSS4.1), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSS2 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSS1.2, GSS2.2), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSS1.2, GSS3.2), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSS1.2, GSS4.2), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSS2.2, GSS3.2), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSS2.2, GSS4.2), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSS3.2, GSS4.2), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSS3 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSS1.3, GSS2.3), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSS1.3, GSS3.3), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSS1.3, GSS4.3), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSS2.3, GSS3.3), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSS2.3, GSS4.3), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSS3.3, GSS4.3), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSAnthro1 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSAnthro1.1, GSAnthro2.1), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnthro1.1, GSAnthro3.1), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro1.1, GSAnthro4.1), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnthro2.1, GSAnthro3.1), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro2.1, GSAnthro4.1), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro3.1, GSAnthro4.1), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSAnthro2 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSAnthro1.2, GSAnthro2.2), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnthro1.2, GSAnthro3.2), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro1.2, GSAnthro4.2), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnthro2.2, GSAnthro3.2), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro2.2, GSAnthro4.2), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro3.2, GSAnthro4.2), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSAnthro3 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSAnthro1.3, GSAnthro2.3), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnthro1.3, GSAnthro3.3), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro1.3, GSAnthro4.3), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnthro2.3, GSAnthro3.3), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro2.3, GSAnthro4.3), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro3.3, GSAnthro4.3), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSAnthro4 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSAnthro1.4, GSAnthro2.4), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnthro1.4, GSAnthro3.4), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro1.4, GSAnthro4.4), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnthro2.4, GSAnthro3.4), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro2.4, GSAnthro4.4), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro3.4, GSAnthro4.4), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSAnthro5 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSAnthro1.5, GSAnthro2.5), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnthro1.5, GSAnthro3.5), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro1.5, GSAnthro4.5), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnthro2.5, GSAnthro3.5), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro2.5, GSAnthro4.5), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro3.5, GSAnthro4.5), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.PM1 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(PM1.1, PM2.1), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(PM1.1, PM3.1), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(PM1.1, PM4.1), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(PM2.1, PM3.1), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(PM2.1, PM4.1), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(PM3.1, PM4.1), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSAnimacy1 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.1, GSAnimacy2.1), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.1, GSAnimacy3.1), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.1, GSAnimacy4.1), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnimacy2.1, GSAnimacy3.1), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy2.1, GSAnimacy4.1), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy3.1, GSAnimacy4.1), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSAnimacy2 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.2, GSAnimacy2.2), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.2, GSAnimacy3.2), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.2, GSAnimacy4.2), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnimacy2.2, GSAnimacy3.2), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy2.2, GSAnimacy4.2), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy3.2, GSAnimacy4.2), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSAnimacy3 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.3, GSAnimacy2.3), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.3, GSAnimacy3.3), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.3, GSAnimacy4.3), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnimacy2.3, GSAnimacy3.3), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy2.3, GSAnimacy4.3), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy3.3, GSAnimacy4.3), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSAnimacy4 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.4, GSAnimacy2.4), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.4, GSAnimacy3.4), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.4, GSAnimacy4.4), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnimacy2.4, GSAnimacy3.4), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy2.4, GSAnimacy4.4), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy3.4, GSAnimacy4.4), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSAnimacy5 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.5, GSAnimacy2.5), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.5, GSAnimacy3.5), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.5, GSAnimacy4.5), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnimacy2.5, GSAnimacy3.5), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy2.5, GSAnimacy4.5), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy3.5, GSAnimacy4.5), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSL1 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSL1.1, GSL2.1), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSL1.1, GSL3.1), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL1.1, GSL4.1), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSL2.1, GSL3.1), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL2.1, GSL4.1), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL3.1, GSL4.1), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSL2 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSL1.2, GSL2.2), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSL1.2, GSL3.2), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL1.2, GSL4.2), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSL2.2, GSL3.2), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL2.2, GSL4.2), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL3.2, GSL4.2), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSL3 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSL1.2, GSL2.2), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSL1.2, GSL3.2), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL1.2, GSL4.2), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSL2.2, GSL3.2), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL2.2, GSL4.2), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL3.2, GSL4.2), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSL4 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSL1.4, GSL2.4), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSL1.4, GSL3.4), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL1.4, GSL4.4), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSL2.4, GSL3.4), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL2.4, GSL4.4), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL3.4, GSL4.4), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSL5 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSL1.5, GSL2.5), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSL1.5, GSL3.5), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL1.5, GSL4.5), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSL2.5, GSL3.5), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL2.5, GSL4.5), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL3.5, GSL4.5), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSI1 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSI1.1, GSI2.1), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSI1.1, GSI3.1), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI1.1, GSI4.1), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSI2.1, GSI3.1), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI2.1, GSI4.1), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI3.1, GSI4.1), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSI2 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSI1.2, GSI2.2), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSI1.2, GSI3.2), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI1.2, GSI4.2), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSI2.2, GSI3.2), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI2.2, GSI4.2), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI3.2, GSI4.2), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSI3 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSI1.3, GSI2.3), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSI1.3, GSI3.3), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI1.3, GSI4.3), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSI2.3, GSI3.3), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI2.3, GSI4.3), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI3.3, GSI4.3), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSI4 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSI1.4, GSI2.4), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSI1.4, GSI3.4), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI1.4, GSI4.4), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSI2.4, GSI3.4), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI2.4, GSI4.4), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI3.4, GSI4.4), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSI5 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSI1.5, GSI2.5), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSI1.5, GSI3.5), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI1.5, GSI4.5), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSI2.5, GSI3.5), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI2.5, GSI4.5), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI3.5, GSI4.5), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSI6 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSI1.6, GSI2.6), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSI1.6, GSI3.6), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI1.6, GSI4.6), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSI2.6, GSI3.6), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI2.6, GSI4.6), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI3.6, GSI4.6), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSS1 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSS1.1, GSS2.1), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSS1.1, GSS3.1), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSS1.1, GSS4.1), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSS2.1, GSS3.1), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSS2.1, GSS4.1), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSS3.1, GSS4.1), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSS2 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSS1.2, GSS2.2), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSS1.2, GSS3.2), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSS1.2, GSS4.2), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSS2.2, GSS3.2), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSS2.2, GSS4.2), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSS3.2, GSS4.2), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSS3 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSS1.3, GSS2.3), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSS1.3, GSS3.3), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSS1.3, GSS4.3), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSS2.3, GSS3.3), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSS2.3, GSS4.3), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSS3.3, GSS4.3), na.rm = TRUE),
                                       TRUE ~ NA_real_)
                         ) %>% dplyr::select(ID, Age, Gender, Experience.Robots, !!!MovementVariableNames()$movement.average)
}

godspeed.nonparamTestByName <- function(name, df, func = c(t.test, wilcox.test), alternative = c("two.sided", "less", "greater")) {
    var.name <- quo_name(enquo(name))
    result <- func(df[[paste("Siso.", var.name, sep = '')]], df[[paste("Linear.", var.name, sep = '')]], paired = TRUE, alternative)
    # Build a dataframe out of this.
    data.frame(Name = var.name,
               method = result$method,
               alternative = result$alternative,
               null.value = result$null.value,
               statistic = result$statistic,
               pvalue = round(result$p.value, digits = 4))
}

godspeed.wilcox.tests.for.components <- function(df = siso.and.linear.godspeed.compenent.averages()) {
    movement.0.variable_names <- MovementVariableNames()$movement.none
    twosided <- lapply(movement.0.variable_names, godspeed.nonparamTestByName, df = df, func = wilcox.test, alternative = "two.sided")
    greater <- lapply(movement.0.variable_names, godspeed.nonparamTestByName, df = df, func = wilcox.test, alternative = "greater")
    less <- lapply(movement.0.variable_names, godspeed.nonparamTestByName, df = df, func = wilcox.test, alternative = "less")
    list(two.sided = twosided, greater = greater, less = less)
}

shapiro.for.name <- function(df, name) {
    var.name <- quo_name(enquo(name))
    shapiro.test(df[[var.name]])
}

                                        # results.shapiro(siso.and.linear.godspeed.compenent.averages, MovementVariableNames()$movement.none)
                                        # results.shapiro(rawFetchSisoResults, unlist(MovementVariableNames()[1:4]))

results.shapiro <- function(df, variable.names) {
    lapply(X=variable.names, FUN=function(x) shapiro.for.name(df, x))
}

godspeed.average.for.series <- function(df = siso.and.linear.godspeed.compenent.averages()) {
    df %>% dplyr::mutate(Siso.GSAnthro.avg =
                             rowMeans(data.frame(Siso.GSAnthro1, Siso.GSAnthro2, Siso.GSAnthro3,
                                                 Siso.GSAnthro4, Siso.GSAnthro5), na.rm = TRUE),
                         Siso.GSAnimacy.avg = 
                             rowMeans(data.frame(Siso.GSAnimacy1, Siso.GSAnimacy2, Siso.GSAnimacy3,
                                                 Siso.GSAnimacy4, Siso.GSAnimacy5), na.rm = TRUE),
                         Siso.GSL.avg = 
                             rowMeans(data.frame(Siso.GSL1, Siso.GSL2, Siso.GSL3,
                                                 Siso.GSL4, Siso.GSL5), na.rm = TRUE),
                         Siso.GSI.avg = 
                             rowMeans(data.frame(Siso.GSI1, Siso.GSI2, Siso.GSI3,
                                                 Siso.GSI4, Siso.GSI5, Siso.GSI6), na.rm = TRUE),
                         Siso.GSS.avg = 
                             rowMeans(data.frame(Siso.GSS1, Siso.GSS2, Siso.GSS3), na.rm = TRUE),
                         Linear.GSAnthro.avg =
                             rowMeans(data.frame(Linear.GSAnthro1, Linear.GSAnthro2, Linear.GSAnthro3,
                                                 Linear.GSAnthro4, Linear.GSAnthro5), na.rm = TRUE),
                         Linear.GSAnimacy.avg = 
                             rowMeans(data.frame(Linear.GSAnimacy1, Linear.GSAnimacy2, Linear.GSAnimacy3,
                                                 Linear.GSAnimacy4, Linear.GSAnimacy5), na.rm = TRUE),
                         Linear.GSL.avg = 
                             rowMeans(data.frame(Linear.GSL1, Linear.GSL2, Linear.GSL3,
                                                 Linear.GSL4, Linear.GSL5), na.rm = TRUE),
                         Linear.GSI.avg = 
                             rowMeans(data.frame(Linear.GSI1, Linear.GSI2, Linear.GSI3,
                                                 Linear.GSI4, Linear.GSI5, Linear.GSI6), na.rm = TRUE),
                         Linear.GSS.avg = 
                             rowMeans(data.frame(Linear.GSS1, Linear.GSS2, Linear.GSS3), na.rm = TRUE))
}

# Calculate the godspeed average for iteration 
godspeed.average.for.iteration <- function(df = rawFetchSisoResults(), iteration = c("Movement.1", "Movement.2", "Movement.3", "Movement.4")) {
    match.arg(iteration)
    movement_var <- iteration
    variable_names <- tws.find_variable_names_for_movement(syms(movement_var))
    anthro.avg.name <- paste("GSAnthro", quo_name(movement_var), "avg", sep=".")
    animacy.avg.name <- paste("GSAnimacy", quo_name(movement_var), "avg", sep=".")
    likeability.avg.name <- paste("GSL", quo_name(movement_var), "avg", sep=".")
    intelligence.avg.name <- paste("GSI", quo_name(movement_var), "avg", sep=".")
    safety.avg.name <- paste("GSS", quo_name(movement_var), "avg", sep=".")

    df %>% dplyr::transmute(!!anthro.avg.name :=
                             rowMeans(data.frame(!!variable_names$GSAnthro1, !!variable_names$GSAnthro2, !!variable_names$GSAnthro3,
                                                 !!variable_names$GSAnthro4, !!variable_names$GSAnthro5), na.rm = TRUE),
                         !!animacy.avg.name :=
                             rowMeans(data.frame(!!variable_names$GSAnimacy1, !!variable_names$GSAnimacy2, !!variable_names$GSAnimacy3,
                                                 !!variable_names$GSAnimacy4, !!variable_names$GSAnimacy5), na.rm = TRUE),
                         !!likeability.avg.name :=
                             rowMeans(data.frame(!!variable_names$GSL1, !!variable_names$GSL2, !!variable_names$GSL3,
                                                 !!variable_names$GSL4, !!variable_names$GSL5), na.rm = TRUE),
                         !!intelligence.avg.name := 
                             rowMeans(data.frame(!!variable_names$GSI1, !!variable_names$GSI2, !!variable_names$GSI3,
                                                 !!variable_names$GSI4, !!variable_names$GSI5, !!variable_names$GSI6), na.rm = TRUE),
                         !!safety.avg.name := 
                             rowMeans(data.frame(!!variable_names$GSS1, !!variable_names$GSS2, !!variable_names$GSS3), na.rm = TRUE))

}


godspeed.wilcox.for.iterations <- function(df = rawFetchSisoResults(), alternative = c("two.sided", "less", "greater")) {
    averages.for.iterations <- lapply(X=c("Movement.1", "Movement.2", "Movement.3", "Movement.4"),
                                      FUN=function(x) godspeed.average.for.iteration(df, x))
    lapply(X=seq(1, length(averages.for.iterations)),
           FUN=function(x) wilcox.test(averages.for.iterations[[1]][[x]], averages.for.iterations[[4]][[x]], paired = TRUE, alternative))
}
