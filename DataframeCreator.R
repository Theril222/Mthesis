library(dplyr)
library(arett)
library(lubridate)
library(rpart)
library(rpart.plot)
library(caret)
library(tidyverse)		 # for data manipulation
library(party)
library(C50)
library(randomForest)
library(gbm)
library("xgboost")  # the main algorithm
library("archdata") # for the sample dataset
library("Ckmeans.1d.dp") # for xgb.ggplot.importance

options(digits.secs=6)
# Pfad zum Verzeichnis, in dem sich Ihre CSV-Dateien befinden
pfad <- "C:/Users/Kevin/OneDrive/Dokumente/R/Eye-Tracking-Daten/Alle"

# Liste aller .csv-Dateien im Verzeichnis erhalten
dateiliste <- list.files(path = pfad, pattern = "*.csv", full.names = TRUE)


# Jede Datei in der Liste importieren
daten_liste <- lapply(dateiliste, function(x) read.csv(x, stringsAsFactors = FALSE))

erweiterung <- read.csv2("C:/Users/Kevin/OneDrive/Dokumente/R/Eye-Tracking-Daten/Erweiterung_Tabelle.csv", dec=";")

# Copyright (c) Sebastian Kapp.
# Licensed under the MIT License.

#' Calculate Velocity
#'
#' @description
#' Calculate the velocity of each gaze point (results required for I-VT fixation filter)
#'
#' @details
#' This is an implementation of the velocity calculation as documented in the \href{https://www.tobiipro.com/siteassets/tobii-pro/learn-and-support/analyze/how-do-we-classify-eye-movements/tobii-pro-i-vt-fixation-filter.pdf}{Tobii Pro I-VT fixation filter}.
#' It first calculates the window size (number of data points) over which the velocity should be calculated based on the specified window length (in ms)
#' and the average time between data points of the first 100 samples. This accommodates data logs with non-uniform time between data points.
#' Then it calculates the velocity for each gaze point by calculating the angle between the first and last gaze point in the current window
#' with the origin of the gaze point in the middle of the window. This angle divided by the window length is saved as the velocity of the middle gaze point.
#'
#' The default window length of 20ms and the eye tracker rate of the Microsoft HoloLens 2 of 30Hz results in a window size of two and therefore a
#' calculation of the velocity between the previous and current gaze point.
#'
#' @format Input data frame columns
#' \describe{
#'   \item{gazeHasValue}{Logical (boolean) if there is valid gaze data}
#'   \item{eyeDataRelativeTimestamp}{Timestamp of the data}
#'   \item{gazeorigin_x}{X coordinates of the gaze origin}
#'   \item{gazeorigin_y}{Y coordinates of the gaze origin}
#'   \item{gazeorigin_z}{Z coordinates of the gaze origin}
#'   \item{gazePoint_x}{X coordinates of the gaze point}
#'   \item{gazePoint_y}{Y coordinates of the gaze point}
#'   \item{gazePoint_z}{Z coordinates of the gaze point}
#' }
#'
#'
#' @param data Data frame of the eye tracking data we want to process
#' @param window_length Length of the window over which we want to calculate the velocity
#' @return The input data frame with the additional column \emph{velocity}
#'
#' @export
calculate_velocity2 <- function(data, window_length = 20) {
  # Calculate how many samples are to be analyzed using the window length in ms ----
  
  # Make sure we have 100 rows which we can analyze, if not analyze all available rows
  maxRow <- 100
  if (nrow(data) < 100) maxRow <- nrow(data)
  
  # Go through the first 100 samples (or less) to find the average time between samples
  sampleTime <- 0
  for (row in 2:maxRow) {
    sampleTime <- sampleTime + (data$eyeDataRelativeTimestamp[row] - data$eyeDataRelativeTimestamp[row-1])
  }
  sampleTime <- sampleTime / maxRow
  
  # Divide the velocity window by this sample time to get the number of samples we will analyze
  windowCount <- sampleTime / window_length
  # Add 1 and then round down to get final window size
  windowCount <- floor(windowCount + 1)
  # Small cleanup
  rm(maxRow, row, sampleTime)
  
  
  # calculate window borders ----
  
  windowLower <- 0
  windowUpper <- 0
  
  if (windowCount %% 2 != 0) {
    # If the window count is odd, we can simply subtract one and divide by two to get the offset for the lower and upper limit for the window
    windowUpper <- (windowCount - 1) / 2
    windowLower <- windowUpper
  } else {
    # Otherwise the lower limit is half of the window and the upper limit is one less than the lower limit
    windowLower <- windowCount / 2
    windowUpper <- windowLower - 1
  }
  
  # Calculate velocities ----
  
  # How many valid samples did we get so far?
  nValidSamplesWindow <- 0
  data['velocity'] = NaN
  
  # We now go through all gaze points and calculate the velocities.
  for (row in 1:nrow(data)) {
    # If the data is valid and we haven't reached enough valid data to fill the window size yet, increase the number of valid samples we have seen
    # Note: We also have to check for an existing gaze position as we can't guarantee that one exists!
    if (data$gazeHasValue[row] && nValidSamplesWindow < windowCount) {
 
      nValidSamplesWindow <- nValidSamplesWindow + 1
      
    } else if (!data$gazeHasValue[row]) {
      # If we don't have a value reset our counter
 
      nValidSamplesWindow <- 0
    }

    # If we have enough valid samples up to this point calculate the velocity in the middle of the window
    if (nValidSamplesWindow >= windowCount) {
      # Limits of the current window
      upperRow <- row
      sampleRow <- upperRow - windowUpper
      lowerRow <- sampleRow - windowLower

      # Get the head (eye) position from the sample row
      origin_x <- data$gazeOrigin_x[sampleRow]
      origin_y <- data$gazeOrigin_y[sampleRow]
      origin_z <- data$gazeOrigin_z[sampleRow]
      
      # Get the gaze position at the lower end of the window
      lowerGaze_x <- data$gazePoint_x[lowerRow]
      lowerGaze_y <- data$gazePoint_y[lowerRow]
      lowerGaze_z <- data$gazePoint_z[lowerRow]
      
      # Get the gaze position at the upper end of the window
      upperGaze_x <- data$gazePoint_x[upperRow]
      upperGaze_y <- data$gazePoint_y[upperRow]
      upperGaze_z <- data$gazePoint_z[upperRow]
      
      # Calculate the direction of the gaze in form of the vector between the head and gaze position
      lowerDirection_x <- data$gazeOrigin_x[sampleRow] - data$gazePoint_x[lowerRow]
      lowerDirection_y <- data$gazeOrigin_y[sampleRow] - data$gazePoint_y[lowerRow]
      lowerDirection_z <- data$gazeOrigin_z[sampleRow] - data$gazePoint_z[lowerRow]
      upperDirection_x <- data$gazeOrigin_x[sampleRow] - data$gazePoint_x[upperRow]
      upperDirection_y <- data$gazeOrigin_y[sampleRow] - data$gazePoint_y[upperRow]
      upperDirection_z <- data$gazeOrigin_z[sampleRow] - data$gazePoint_z[upperRow]
      
      
      # Calculate the scalar product of the two vectors
      scalar <- lowerDirection_x * upperDirection_x + lowerDirection_y * upperDirection_y + lowerDirection_z * upperDirection_z
      # Calculate the length of the two vectors
      lowerDirection_length <- sqrt(lowerDirection_x * lowerDirection_x + lowerDirection_y * lowerDirection_y + lowerDirection_z * lowerDirection_z)
      upperDirection_length <- sqrt(upperDirection_x * upperDirection_x + upperDirection_y * upperDirection_y + upperDirection_z * upperDirection_z)
      
      # Calculate the angle between the two vectors using the scalar product and their lengths (result in rad)
      angleRad <- acos(scalar / (lowerDirection_length * upperDirection_length))
      # The final angle is supposed to be in degrees instead of rad, therefore we translate the value
      angleDeg <- (angleRad * 180) / pi
      
      # The time delta is the time between the upper and lower window
      timeDeltaMs <- data$eyeDataRelativeTimestamp[upperRow] - data$eyeDataRelativeTimestamp[lowerRow]
      
      # We need the time in seconds instead of ms to get the correct velocity unit
      timeDeltaS <- timeDeltaMs / 1000
      # The velocity is now the angle over time

      data$velocity[sampleRow] <- angleDeg / timeDeltaS

    }
  }
  # Return the result
  return(data)
}


calculate_AOI_fixation <- function(data) {
  nAoiAf <- 0
  nAoialf <- 0
  nAoiModell <- 0
  for (row in 1:nrow(data)){
    if(data$gazeHasValue[row] && data$gazePointAOIHit[row] == TRUE && data$classification[row] == 'fixation'){
      if(substr(data$gazePointAOI_name[row],1,5) == 'Round'){
        nAoiModell <- nAoiModell + 1
      }else if(substr(data$gazePointAOI_name[row],1,7) =='Arbeits'){
        nAoiAf <- nAoiAf + 1
      }else if(substr(data$gazePointAOI_name[row],1,6) == 'Anleit' && (data$task[1] == 'Steamboat' || data$task[1] == 'Dog' )){
        nAoiModell <- nAoiModell +1
      }
      data$AOIFCAF[row] <- nAoiAf
      data$AOIFCModell[row] <- nAoiModell
      data$AOIFCTotal[row] <- nAOItotal <- nAoiAf + nAoiModell

      
    }else{
      data$AOIFCAF[row] <- nAoiAf
      data$AOIFCModell[row] <- nAoiModell
      data$AOIFCTotal[row] <- nAOItotal <- nAoiAf + nAoiModell
    }
  }
  return(data)
}

calculate_classifications <- function(data) {
  fixations <- 0
  saccades <- 0
  gaps <- 0
  
  for (row in 1:nrow(data)){
    
      if(data$classification[row] == 'fixation'){
        fixations <- fixations + 1
      }else if(data$classification[row] == 'gap'){
        gaps <- gaps + 1
      }else if(data$classification[row] == 'saccade'){
        saccades <- saccades +1
      }
      data$fixcount[row] <- fixations
      data$gapcount[row] <- gaps
      data$saccadecount[row] <- saccades
      
    }
  return(data)
}

merge_fixations_ivt2 <- function(data, max_time = 75, max_angle = 0.5) {
  # Get all event ids and remove all missing ids
  allEventIds <- unique(data$eventIndex)
  allEventIds <- allEventIds[!is.na(allEventIds)]
  
  # Remember the original event types
  originalEventTypes <- data$eventType
  
  # We go through all events using the event ids and if the event isn't a fixation check for the duration of the event
  for (eventId in allEventIds) {
    # Make sure we aren't in the first or last event as they can't be a gap between two fixations
    if (eventId == 1 || eventId == allEventIds[length(allEventIds)]) {
      next
    }
    # As the info about the event is in every line with this id we can simply use the first line of the event
    eventInfo <- data[data$eventIndex == eventId, ]
    eventInfo <- eventInfo[1, ]

    # Check if the event isn't a fixation and the duration is smaller than the limit
    if (!is.na(eventInfo$eventType) && (eventInfo$classification != "fixation") && (eventInfo$eventDuration < max_time)) {
      # If this is the case we now need to check if we are actually between to fixations
      
      # The previous and next event ids
      # Note: we can not simply use -1 and +1 of the id as we might have merged before this event which creates gaps in the event ids
      allEventIdsCurrently <- unique(data$eventIndex)
      allEventIdsCurrently <- allEventIdsCurrently[!is.na(allEventIdsCurrently)]
      currentEventIdRow <- match(eventId, allEventIdsCurrently)
      previosEventId <- allEventIdsCurrently[currentEventIdRow - 1]
      nextEventId <- allEventIdsCurrently[currentEventIdRow + 1]
      
      # Get the last line from the previous event
      eventInfoPrevious <- data[data$eventIndex == previosEventId, ]
      eventInfoPrevious <- eventInfoPrevious[nrow(eventInfoPrevious), ]
      
      # And the first line of the next event
      eventInfoNext <- data[data$eventIndex == nextEventId, ]
      eventInfoNext <- eventInfoNext[1, ]
      # Check if we are between two fixations
      # Note: As we only merge fixations we ignore all other cases
      if (eventInfoPrevious$classification == "fixation" && eventInfoNext$classification == "fixation") {
        # If this is the case, calculate the angle between the fixations
        
        # First get the head position as average between the last point in the previous fixation and the first point in the next fixation
        origin_x <- (eventInfoPrevious$gazeOrigin_x + eventInfoNext$gazeOrigin_x) / 2
        origin_y <- (eventInfoPrevious$gazeOrigin_y + eventInfoNext$gazeOrigin_y) / 2
        origin_z <- (eventInfoPrevious$gazeOrigin_z + eventInfoNext$gazeOrigin_z) / 2
        
        # Calculate the direction of the gaze in the last point in the previous fixation and the first point in the next fixation
        previousDirection_x <- origin_x - eventInfoPrevious$gazePoint_x
        previousDirection_y <- origin_y - eventInfoPrevious$gazePoint_y
        previousDirection_z <- origin_z - eventInfoPrevious$gazePoint_z
        nextDirection_x <- origin_x - eventInfoNext$gazePoint_x
        nextDirection_y <- origin_y - eventInfoNext$gazePoint_y
        nextDirection_z <- origin_z - eventInfoNext$gazePoint_z
        
        # Calculate the scalar product of the two vectors
        scalar <- previousDirection_x * nextDirection_x + previousDirection_y * nextDirection_y + previousDirection_z * nextDirection_z
        
        # Calculate the length of the two vectors
        previousDirection_length <- sqrt(previousDirection_x * previousDirection_x + previousDirection_y * previousDirection_y + previousDirection_z * previousDirection_z)
        nextDirection_length <- sqrt(nextDirection_x * nextDirection_x + nextDirection_y * nextDirection_y + nextDirection_z * nextDirection_z)
        # Calculate the angle between the two vectors using the scalar product and their lengths (result in rad)
        angleRad <- acos(scalar / (previousDirection_length * nextDirection_length))
        #rounding error
        if((scalar / (previousDirection_length * nextDirection_length)) > 1 && (scalar / (previousDirection_length * nextDirection_length)) < 1.001 ){
          angleRad <- 0
        }
        # The final angle is supposed to be in degrees instead of rad, therefore we translate the value
        angleDeg <- (angleRad * 180) / pi
        # Now check if this angle is smaller or equal than the specified limit
        if (angleDeg <= max_angle) {
          # If all these conditions are met, merge the fixations.
          # For this we extend the previous event and merge the gap as well as the following fixation into one
          
          # The rows that form the new fixation event
          newEventRows <- data$eventIndex >= previosEventId & data$eventIndex <= nextEventId
          # The new fixation position is the average position of the two fixations we are merging (not including the gap)
          data$fixation_x[newEventRows] <- mean(data$gazePoint_x[newEventRows][originalEventTypes[newEventRows] != "saccade"])
          data$fixation_y[newEventRows] <- mean(data$gazePoint_y[newEventRows][originalEventTypes[newEventRows] != "saccade"])
          data$fixation_z[newEventRows] <- mean(data$gazePoint_z[newEventRows][originalEventTypes[newEventRows] != "saccade"])
          
          # The duration is simply the sum of the three events we are merging
          data$eventDuration[newEventRows] <- sum(unique(data$eventDuration[newEventRows]))
          
          # The event type is a fixation
          data$eventType[newEventRows] <- "fixation"
          
          # The new event index is the one of the first event
          data$eventIndex[newEventRows] <- previosEventId
          # Mark all modified rows
          data$modified[newEventRows] <- TRUE
        }
      }
      
    }
  }
  
  # Return the result
  return(data)
}

classify_idt2 <- function(data, dispersion_threshold = 1.6, time_window = 250) {
  # Calculate how many samples are to be analyzed using the window length in ms
  
  # Make sure we have 100 rows which we can analyze ----
  idt_maxRow <- 100
  if (nrow(data) < 100) idt_maxRow <- nrow(data)
  
  # Go through the first 100 samples (or less) to find the average time between samples
  idt_sampleTime <- 0
  for (idt_row in 2:idt_maxRow) {
    idt_sampleTime <- idt_sampleTime + (data$eyeDataRelativeTimestamp[idt_row] - data$eyeDataRelativeTimestamp[idt_row-1])
  }
  idt_sampleTime <- idt_sampleTime / idt_maxRow
  
  # Divide the window by this sample time to get the number of samples we will analyze
  idt_minWindowCount <- time_window / idt_sampleTime
  # Add 1 and then round down to get final window size
  idt_minWindowCount <- floor(idt_minWindowCount + 1)
  
  # Small cleanup
  rm(idt_maxRow, idt_row, idt_sampleTime)
  
  
  # Initialize the calculation with an initial window ----
  
  # We start at the beginning of the file
  idt_lowerWindow <- 1
  # And add the min window size to get our initial window
  idt_upperWindow <- idt_lowerWindow + (idt_minWindowCount - 1)
  
  
  # Initialize the classification variables ----
  
  # Index of the current fixation
  idt_fixationIndex <- 1
  idt_lastFixationRow <- NA
  
  # classification columns
  data$eventIndex <- NA # Duration of the current event
  data$eventType <- NA # Duration of the current event
  data$eventDuration <- NA # Duration of the current event
  data$fixation_x <- NA # x coordinate of the fixation event identified
  data$fixation_y <- NA # y coordinate of the fixation event identified
  data$fixation_z <- NA # z coordinate of the fixation event identified
  
  
  # Main calculation loop of the classification ----
  
  # Loop until we have exceeded our data
  while (idt_upperWindow <= nrow(data)) {
    
    # Check if we have a gaze point for all rows in the current window
    idt_allGazePointsValid <- TRUE
    
    # For this we start at the end of the window and go forward to catch the last invalid point
    for (row in c(idt_upperWindow:idt_lowerWindow)) {
      if (is.na(data$gazePoint_x[row])) {
        # If we have an invalid row, we can't identify a fixation and we have to move our window
        idt_allGazePointsValid <- FALSE
        
        # For this we start directly after the last invalid gaze point with the min window size and check again
        idt_lowerWindow <- row + 1
        idt_upperWindow <- idt_lowerWindow + (idt_minWindowCount - 1)
        
        # We can stop the check here
        break
      }
    }
    rm(row)
    
    # If we have invalid gaze points restart the check
    # Note: We have already set the new window!
    if (!idt_allGazePointsValid) {
      next
    }
    
    # We now know that we have valid gaze data for all points in the current window
    
    # At first we assume our window covers a fixation
    idt_maxDispersionExceeded <- FALSE
    
    # Get the mean gaze origin for the angle calculations
    idt_meanGazeOrigin_x <- mean(data$gazeOrigin_x[c(idt_lowerWindow:idt_upperWindow)])
    idt_meanGazeOrigin_y <- mean(data$gazeOrigin_y[c(idt_lowerWindow:idt_upperWindow)])
    idt_meanGazeOrigin_z <- mean(data$gazeOrigin_z[c(idt_lowerWindow:idt_upperWindow)])
    
    ###
    # Check the angle between every point in the current window against the limit
    for (i in idt_lowerWindow:(idt_upperWindow-1)) {
      # If we already exceeded our max dispersion angle we can stop our search
      if (idt_maxDispersionExceeded) {
        break
      }
      
      
      # Direction vector for the first point
      idt_iVector_x <- idt_meanGazeOrigin_x - data$gazePoint_x[i]
      idt_iVector_y <- idt_meanGazeOrigin_y - data$gazePoint_y[i]
      idt_iVector_z <- idt_meanGazeOrigin_z - data$gazePoint_z[i]
      
      for (j in (i+1):idt_upperWindow) {
        # Direction vector for the second point
        idt_jVector_x <- idt_meanGazeOrigin_x - data$gazePoint_x[j]
        idt_jVector_y <- idt_meanGazeOrigin_y - data$gazePoint_y[j]
        idt_jVector_z <- idt_meanGazeOrigin_z - data$gazePoint_z[j]
        
        # Calculate the scalar product of the two vectors
        idt_scalar <- idt_iVector_x * idt_jVector_x + idt_iVector_y * idt_jVector_y + idt_iVector_z * idt_jVector_z
        
        # Calculate the length of the two vectors
        idt_iVector_length <- sqrt(idt_iVector_x * idt_iVector_x + idt_iVector_y * idt_iVector_y + idt_iVector_z * idt_iVector_z)
        idt_jVector_length <- sqrt(idt_jVector_x * idt_jVector_x + idt_jVector_y * idt_jVector_y + idt_jVector_z * idt_jVector_z)
        
        # Calculate the angle between the two vectors using the scalar product and their lengths (result in rad)
        idt_angleRad <- acos(idt_scalar / (idt_iVector_length * idt_jVector_length))
        
        if((idt_scalar / (idt_iVector_length * idt_jVector_length)) > 1 && (idt_scalar / (idt_iVector_length * idt_jVector_length)) < 1.001 ){
          idt_angleRad <- 0
        }
        
        # The final angle is supposed to be in degrees instead of rad, therefore we translate the value
        idt_angleDeg <- (idt_angleRad * 180) / pi
        
        # Check the angle against the limit
        if (idt_angleDeg > dispersion_threshold) {
          # If we exceeded the limit we can stop checking for this window
          idt_maxDispersionExceeded <- TRUE
          
          # We now know that the first point is too far apart from the second vector.
          # As we are in the initial window we wouldn't have the required minimum fixation duration if we would discard everything after the second point
          # Therefore we start a new initial window one point after the "current first point" and check again
          idt_lowerWindow <- i + 1
          idt_upperWindow <- idt_lowerWindow + (idt_minWindowCount - 1)
          
          # Break the comparison of the current first point with second points as we exceeded the angle
          # As we set the corresponding flag this also breaks the search for all other points
          break
        }
      }
    }
    
    # If we exceeded the max dispersion angle in our search we have to restart the search
    # Note: The new lower and upper window limits have already been set so we don't need to do anything here!
    if (idt_maxDispersionExceeded) {
      next
    }
    
    ###
    # We now have a window that is exactly the minimum size and does not exceed the max dispersion angle
    # Now we can add additional points to our window until the new point exceeds our dispersion angle
    
    # Reset the exceeded flag (should already be false but to make sure)
    idt_maxDispersionExceeded <- FALSE
    
    while (!idt_maxDispersionExceeded) {
      # Add a new point to our window
      idt_upperWindow <- idt_upperWindow + 1
      
      # Make sure we are still inside our data
      if (idt_upperWindow > nrow(data)) {
        # Treat exceeding the number of data rows the same as exceeding the max dispersion
        # This triggers the final calculation and assignment of the fixation
        # After this calculation the window is moved which again results in exceeding of the number of rows
        #    which is caught in the main loop and causes it to end.
        # Note: This leaves the window with one point too much which we will correct later
        idt_maxDispersionExceeded <- TRUE
        break
      }
      
      # Make sure the new line actually contains data
      if (is.na(data$gazePoint_x[idt_upperWindow])) {
        # Treat reaching invalid data the same as exceeding the max dispersion
        # This triggers the final calculation and assignment of the fixation
        # After this calculation the window is moved which then triggers the invalid data check again moving the window until it reaches valid data again
        # Note: This leaves the window with one point too much which we will correct later
        idt_maxDispersionExceeded <- TRUE
        break
      }
      
      # Check the angle of the new point to all previous points
      # Note: Now the new point is point i while the old, "lower" points are the points j
      
      # Direction vector for the first point
      idt_iVector_x <- idt_meanGazeOrigin_x - data$gazePoint_x[idt_upperWindow]
      idt_iVector_y <- idt_meanGazeOrigin_y - data$gazePoint_y[idt_upperWindow]
      idt_iVector_z <- idt_meanGazeOrigin_z - data$gazePoint_z[idt_upperWindow]
      
      for (j in idt_lowerWindow:(idt_upperWindow-1)) {
        # Direction vector for the second point
        idt_jVector_x <- idt_meanGazeOrigin_x - data$gazePoint_x[j]
        idt_jVector_y <- idt_meanGazeOrigin_y - data$gazePoint_y[j]
        idt_jVector_z <- idt_meanGazeOrigin_z - data$gazePoint_z[j]
        
        # Calculate the scalar product of the two vectors
        idt_scalar <- idt_iVector_x * idt_jVector_x + idt_iVector_y * idt_jVector_y + idt_iVector_z * idt_jVector_z
        
        # Calculate the length of the two vectors
        idt_iVector_length <- sqrt(idt_iVector_x * idt_iVector_x + idt_iVector_y * idt_iVector_y + idt_iVector_z * idt_iVector_z)
        idt_jVector_length <- sqrt(idt_jVector_x * idt_jVector_x + idt_jVector_y * idt_jVector_y + idt_jVector_z * idt_jVector_z)
        
        # Calculate the angle between the two vectors using the scalar product and their lengths (result in rad)
        idt_angleRad <- acos(idt_scalar / (idt_iVector_length * idt_jVector_length))
        
        if((idt_scalar / (idt_iVector_length * idt_jVector_length)) > 1 && (idt_scalar / (idt_iVector_length * idt_jVector_length)) < 1.001 ){
          idt_angleRad <- 0
        }
        
        # The final angle is supposed to be in degrees instead of rad, therefore we translate the value
        idt_angleDeg <- (idt_angleRad * 180) / pi
        
        # Check the angle against the limit
        if (idt_angleDeg > dispersion_threshold) {
          # We now know that adding the new point will exceed the max dispersion angle, therefore we stop checking.
          # By setting the exceeded flag we also stop adding new points to the window.
          # Note: This leaves the window with one point too much which we will correct later
          idt_maxDispersionExceeded <- TRUE
          break
        }
      }
      
      # If we did not exceed the max dispersion angle with this point we can simply continue our loop and add more points to the window
    }
    
    # When we reach this point our loop of adding points to the current window stopped. This can be of three reasons:
    #  a) We reached the end of our data after adding a new point
    #  b) We reached missing data
    #  c) We exceeded the max dispersion angle after adding a new point
    
    # As we always break the loop after adding a new point, our last valid window is one point smaller than the current state
    idt_upperWindow <- idt_upperWindow - 1
    
    ###
    # If we previously identified a fixation and have a gap, write an event index into this gap
    if (!is.na(idt_lastFixationRow) && idt_lowerWindow > 1 && idt_lastFixationRow+1 != idt_lowerWindow) {
      # Update the event index for all rows from the last fixation to this one
      data$eventIndex[c((idt_lastFixationRow+1):(idt_lowerWindow-1))] <- idt_fixationIndex
      data$eventType[c((idt_lastFixationRow+1):(idt_lowerWindow-1))] <- NA
      # Increase the index as we just wrote the index for the gap between fixations
      idt_fixationIndex <- idt_fixationIndex + 1
    }
    
    ###
    # Now we can calculate the identified fixation
    
    # The rows we are currently looking at are all rows inside our window
    idt_currentFixationRows <- c(idt_lowerWindow:idt_upperWindow)
    
    # Write the fixation classification into the data
    data$eventIndex[idt_currentFixationRows] <- idt_fixationIndex
    data$eventType[idt_currentFixationRows] <- "fixation"
    
    # The duration is the time from the last sample before the window and the last sample in this window
    # (Note: Starting before this window to prevent gaps)
    if (idt_lowerWindow == 1) {
      # If this is our first sample we can of course not use the sample before it
      data$eventDuration[idt_currentFixationRows] <- data$eyeDataRelativeTimestamp[idt_upperWindow] - data$eyeDataRelativeTimestamp[idt_lowerWindow]
    }
    else {
      # Otherwise calculate as described
      data$eventDuration[idt_currentFixationRows] <- data$eyeDataRelativeTimestamp[idt_upperWindow] - data$eyeDataRelativeTimestamp[idt_lowerWindow-1]
    }
    
    
    # The fixation position is the average position of all gaze points in this fixation
    data$fixation_x[idt_currentFixationRows] <- mean(data$gazePoint_x[idt_currentFixationRows])
    data$fixation_y[idt_currentFixationRows] <- mean(data$gazePoint_y[idt_currentFixationRows])
    data$fixation_z[idt_currentFixationRows] <- mean(data$gazePoint_z[idt_currentFixationRows])
    
    # Increase the fixation index as we just classified a fixation and note the last row of this fixation
    idt_fixationIndex <- idt_fixationIndex + 1
    idt_lastFixationRow <- idt_upperWindow
    
    ###
    # Start a new window with the minimum window size directly after the current window
    idt_lowerWindow <- idt_upperWindow + 1
    idt_upperWindow <- idt_lowerWindow + (idt_minWindowCount - 1)
    
    ###
    # Restart the loop evaluating first the minimal window size and then adding new points again
    # Note: If we exceeded our data with the new window the loop will stop automatically
  }
  
  return(data)
}

for (x in 1:length(dateiliste)){
  daten_liste[[x]] <- daten_liste[[x]] %>% mutate( task = substr(dateiliste[x], 87, nchar(dateiliste[x]) -4 ))
  daten_liste[[x]] <- daten_liste[[x]] %>% mutate(probant = substr(dateiliste[x], 81, 85))
  daten_liste[[x]] <- daten_liste[[x]] %>% mutate(gazePointAOI_name  = ifelse(is.na(gazePointAOI_name), 'No_Gaze', gazePointAOI_name))
  daten_liste[[x]]$gazeHasValue <- as.logical(tolower(daten_liste[[x]]$gazeHasValue))
  daten_liste[[x]]$gazePointAOIHit <- as.logical(tolower(daten_liste[[x]]$gazePointAOIHit))
  daten_liste[[x]]$Duration <- daten_liste[[x]]$eyeDataTimestamp - daten_liste[[x]]$eyeDataTimestamp[1]
  daten_liste[[x]]$eyeDataTimestamp <- as.POSIXct(daten_liste[[x]]$eyeDataTimestamp / 1000, origin="1970-01-01")
}

#df
df <- daten_liste

for(x in 1:length(dateiliste)){
  while(df[[x]]$gazeHasValue[1] == FALSE) {
    df[[x]] <- df[[x]] %>%  filter(!row_number() %in% c(1))
  } 
}


flush.console()
for (x in 1:length(df)){
  print(x)
  df[[x]] <- gap_fill(df[[x]], max_gap_length = 75)
  df[[x]] <- noise_reduction(df[[x]], method = median, window_size = 3)
  df[[x]] <- calculate_velocity2(df[[x]], window_length = 20)
  df[[x]] <- classify_iaoi(df[[x]], min_fixation_duration = 100)
  df[[x]] <- classify_ivt(df[[x]], velocity_threshold = 100)
  df[[x]] <- classify_idt2(df[[x]], dispersion_threshold = 1.6, time_window = 250)
  df[[x]] <- merge_fixations_iaoi(df[[x]], max_time = 75)
  df[[x]] <- merge_fixations_idt( df[[x]], max_time = 75, dispersion_threshold = 1.6)
  df[[x]] <- merge_fixations_ivt2(df[[x]], max_time = 75, max_angle = 0.5)
  df[[x]] <- calculate_AOI_fixation(df[[x]])
  df[[x]] <- calculate_classifications(df[[x]])
}
df2 <- data.frame()
for (x in 1:length(df)){
  # Get the row data
  probant <- df[[x]][nrow(df[[x]]), 'probant']
  task  <- df[[x]][nrow(df[[x]]), 'task']
  duration <- df[[x]][nrow(df[[x]]), 'Duration']
  AOIFCAF <- df[[x]][nrow(df[[x]]), 'AOIFCAF']
  AOIFCModell <- df[[x]][nrow(df[[x]]), 'AOIFCModell']
  AOIFCTotal <- df[[x]][nrow(df[[x]]), 'AOIFCTotal']
  fixcount <- df[[x]][nrow(df[[x]]), 'fixcount']
  gapcount <- df[[x]][nrow(df[[x]]), 'gapcount']
  saccadecount <-df[[x]][nrow(df[[x]]), 'saccadecount']
  # Populate the row
  new.row <- data.frame(probant = probant, task = task, duration = duration, AOIFCAF = AOIFCAF, AOIFCModell = AOIFCModell, AOIFCTotal = AOIFCTotal, fixcount = fixcount, gapcount = gapcount, saccadecount = saccadecount )
  
  # Add the row
  df2 <- rbind(df2, new.row)
}

df3 <- left_join(df2, erweiterung, by = c('probant' = 'Teilnehmer', 'task' = 'Aufgabe'))
df4 <- df3[,c('duration','AOIFCAF', 'AOIFCModell', 'AOIFCTotal', 'fixcount', 'Abgeschlossen', 'outcome')  ]


summary(df4)
dim(df4)
df4$outcome = as.factor(df4$outcome)
parts = createDataPartition(df4$duration, p = 0.7, list = F)
train = df4[parts, ]
test = df4[-parts, ]
t <- as.vector(test$outcome)
t2 <- as.factor(t)

Classes <- unique(test$outcome)

model2 <- rpart(outcome ~., data = train)
prp(model2, extra = 1)

# specifying the CV technique which will be passed into the train() function later and number parameter is the "k" in K-fold cross validation
train_control = trainControl(method = "cv", number = 5, search = "grid")

## Customsing the tuning grid (ridge regression has alpha = 0)
multi_classification_Tree_Grid =  expand.grid(maxdepth = c(1,3,5,7,9))


set.seed(50)

# training a Regression model while tuning parameters (Method = "rpart")
model = train(outcome~., data = train, method = "rpart2", trControl = train_control, tuneGrid = multi_classification_Tree_Grid)

# summarising the results
print(model)


#use model to make predictions on test data
pred_y = predict(model, test)
pred_x = predict(model2, test, type = 'class')

mallo <- data.frame(value = test$outcome, pred_x)
confusionMatrix(pred_x, t2)


# confusion Matrix
confusionMatrix(data = pred_y, reference =t2)

target = outcome ~ duration + AOIFCAF  + AOIFCModell  + AOIFCTotal + fixcount  + Abgeschlossen 

tree = rpart(target, data = train, method = "class")
rpart.plot(tree)

predictions = predict(tree, test, type = 'class')

confusionMatrix(factor(predictions), t2)


tree = ctree(outcome ~ ., data = train)
plot(tree, main="Conditional Inference Tree for Cognitive Load")

# build model
tree = C5.0(outcome ~ ., data = train, trials=10)
plot(tree)
# make predictions
confusionMatrix(predict(tree, newdata=test, type = 'class'), t2)



tree_ms3 = rpart(target, train, control = rpart.control(minsplit = 3))
tree_ms10 = rpart(target, train, control = rpart.control(minsplit = 10))
tree_ms7 = rpart(target, train, control = rpart.control(minsplit = 7))




predict_test = predict(tree_ms3, test, type = "class")
predict_test2 = predict(tree_ms7, test, type = "class")
predict_test3 = predict(tree_ms10, test, type = "class")



confusionMatrix(factor(predict_test), t2)
confusionMatrix(factor(predict_test2), t2)
confusionMatrix(factor(predict_test3), t2)





fit <- randomForest(outcome ~ ., train,ntree=500)
summary(fit)
predictedrf = predict(fit,test)



confusionMatrix(factor(predictedrf), t2)


# train a model using our training data
model_gbm = gbm(outcome ~.,
                data = train,
                distribution = "multinomial",
                cv.folds = 9,
                shrinkage = .01,
                n.minobsinnode = 10,
                n.trees = 500)       # 500 tress to be built


#use model to make predictions on test data
pred_test = predict.gbm(object = model_gbm,
                        newdata = test,
                        n.trees = 500,           # 500 tress to be built
                        type = "response")



# Give class names to the highest prediction value.
class_names = colnames(pred_test)[apply(pred_test, 1, which.max)]
result = data.frame(test$outcome, class_names)



conf_mat = confusionMatrix(test$outcome, as.factor(class_names))
print(conf_mat)



testing <- df4[,c('duration','AOIFCAF', 'AOIFCModell', 'AOIFCTotal', 'fixcount', 'Abgeschlossen', 'outcome')  ]

testnum <- df4[,c('duration','AOIFCAF', 'AOIFCModell', 'AOIFCTotal', 'fixcount', 'Abgeschlossen', 'outcome')  ] 
testnum$outcome <- as.numeric(testnum$outcome)
testnum <- testnum %>% mutate(outcome = outcome - 1)
summary(testnum)

train_index <- sample(1:nrow(testnum), nrow(testnum)*0.7)
# Full data set
data_variables <- as.matrix(testnum[,-9])
data_label <- testnum[,"outcome"]
data_matrix <- xgb.DMatrix(data = as.matrix(testnum), label = data_label)
# split train data and make xgb.DMatrix
train_data   <- data_variables[train_index,]
train_label  <- data_label[train_index]
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
# split test data and make xgb.DMatrix
test_data  <- data_variables[-train_index,]
test_label <- data_label[-train_index]
test_matrix <- xgb.DMatrix(data = test_data, label = test_label)

numberOfClasses <- length(unique(testnum$outcome))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses)
nround    <- 100 # number of XGBoost rounds
cv.nfold  <- 9

# Fit cv.nfold * cv.nround XGB models and save OOF predictions
cv_model <- xgb.cv(params = xgb_params,
                   data = train_matrix, 
                   nrounds = nround,
                   nfold = cv.nfold,
                   verbose = FALSE,
                   prediction = TRUE)


OOF_prediction <- data.frame(cv_model$pred) %>%
  mutate(max_prob = max.col(., ties.method = "last"),
         label = train_label + 1)
head(OOF_prediction)

# confusion matrix
confusionMatrix(factor(OOF_prediction$max_prob),
                factor(OOF_prediction$label),
                mode = "everything")


bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = nround)



# Predict hold-out test set
test_pred <- predict(bst_model, newdata = test_matrix)
test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                          ncol=length(test_pred)/numberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label + 1,
         max_prob = max.col(., "last"))
# confusion matrix of test set
confusionMatrix(factor(test_prediction$max_prob),
                factor(test_prediction$label),
                mode = "everything")

