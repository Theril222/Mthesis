library(dplyr)
library(arett)
library(lubridate)
options(digits.secs=6)
# Pfad zum Verzeichnis, in dem sich Ihre CSV-Dateien befinden
pfad <- "C:/Users/Kevin/OneDrive/Dokumente/R/Eye-Tracking-Daten/Alle"

# Liste aller .csv-Dateien im Verzeichnis erhalten
dateiliste <- list.files(path = pfad, pattern = "*.csv", full.names = TRUE)


# Jede Datei in der Liste importieren
daten_liste <- lapply(dateiliste, function(x) read.csv(x, stringsAsFactors = FALSE))

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
    if(data$gazeHasValue[row] && data$gazePointAOIHit[row] == TRUE){
      if(substr(data$gazePointAOI_name[row],1,5) == 'Round'){
        nAoiModell <- nAoiModell + 1
      }else if(substr(data$gazePointAOI_name[row],1,7) =='Arbeits'){
        nAoiAf <- nAoiAf + 1
      }else if(substr(data$gazePointAOI_name[row],1,6) == 'Anleit' && (data$task[1] == 'Steamboat' || data$task[1] == 'Dog' )){
        nAoialf <- nAoialf +1
      }
      data$AOIFCAF[row] <- nAoiAf
      data$AOIFCAlF[row] <- nAoialf
      data$AOIFCModell[row] <- nAoiModell
      data$AOIFCTotal[row] <- nAOItotal <- nAoiAf + nAoialf + nAoiModell

      
    }else{
      data$AOIFCAF[row] <- nAoiAf
      data$AOIFCAlF[row] <- nAoialf
      data$AOIFCModell[row] <- nAoiModell
      data$AOIFCTotal[row] <- nAOItotal <- nAoiAf + nAoialf + nAoiModell
    }
  }
  return(data)
}

merge_fixations_iaoi2 <- function(data, max_time = 75) {
  # Get all event ids and remove all missing ids
  allEventIds <- unique(data$eventIndex)
  allEventIds <- allEventIds[!is.na(allEventIds)]
  
  # Duplicate the list of all event ids so we can update the list without influencing the loop
  currentEventIds <- allEventIds
  
  # Remember the original event types
  originalEventTypes <- data$eventType
  
  # We go through all events using the event ids and if the event isn't a fixation check for the duration of the event
  for (eventId in allEventIds) {
    # Make sure we aren't in the first or last event as they can't be a gap between two fixations
    if (eventId == 1 || eventId == allEventIds[length(allEventIds)]) {
      next
    }
    
    # As the info about the event is in every line with this id we can simply use the first line of the event
    eventInfo <- data[match(eventId, data$eventIndex), c("eventType", "eventDuration")]
    
    # Check if the event isn't a fixation and the duration is smaller than the limit
    if (!is.na(eventInfo$eventType) && (eventInfo$eventType != "fixation") && (eventInfo$eventDuration < max_time)) {
      # If this is the case we now need to check if we are actually between to fixations
      
      # The previous and next event ids
      # Note: We can not simply use -1 and +1 of the id as we might have merged before this event which creates gaps in the event ids
      currentEventIdRow <- match(eventId, currentEventIds)
      previosEventId <- currentEventIds[currentEventIdRow - 1]
      nextEventId <- currentEventIds[currentEventIdRow + 1]
      
      # Get the last line from the previous event
      eventInfoPrevious <- data[max(which(data$eventIndex == nextEventId)), c("eventType", "gazePointAOI_name")]
      
      # And the first line of the next event
      eventInfoNext <- data[match(nextEventId, data$eventIndex), c("eventType", "gazePointAOI_name")]
      print(eventInfoPrevious$gazePointAOI_name)
      print(eventInfoNext$gazePointAOI_name)
      # Check if we are between two fixations
      # Note: As we only merge fixations we ignore all other cases
      if (eventInfoPrevious$eventType == "fixation" && eventInfoNext$eventType == "fixation") {
        # If this is the case, check if the fixations are of the same AOI (due to the I-AOI filter this is the differentiation between fixations)
        if (eventInfoPrevious$gazePointAOI_name == eventInfoNext$gazePointAOI_name) {
          # If all these conditions are met, merge the fixations.
          # For this we extend the previous event and merge the gap as well as the following fixation into one
          
          # The rows that form the new fixation event
          newEventRows <- data$eventIndex >= previosEventId & data$eventIndex <= nextEventId
          
          # The new fixation position is the average position of the two fixations we are merging (but not the positions during a saccade)
          fixationRows <- originalEventTypes[newEventRows] == "fixation"
          data$fixation_x[newEventRows] <- mean(data$gazePoint_x[newEventRows][fixationRows])
          data$fixation_y[newEventRows] <- mean(data$gazePoint_y[newEventRows][fixationRows])
          data$fixation_z[newEventRows] <- mean(data$gazePoint_z[newEventRows][fixationRows])
          
          # The duration is simply the sum of the three events we are merging
          data$eventDuration[newEventRows] <- sum(unique(data$eventDuration[newEventRows]))
          
          # The event type is a fixation
          data$eventType[newEventRows] <- "fixation"
          
          # The new event index is the one of the first event
          data$eventIndex[newEventRows] <- previosEventId
          
          # With this we have overwritten the current and next event id and therefore removed them from existence
          # Update the list of ids accordingly
          currentEventIds <- currentEventIds[currentEventIds != eventId & currentEventIds != nextEventId]
        }
      }
    }
  }
  
  return(data)
}

for (x in 1:length(dateiliste)){
  daten_liste[[x]] <- daten_liste[[x]] %>% mutate( task = substr(dateiliste[x], 87, nchar(dateiliste[x]) -4 ))
  daten_liste[[x]] <- daten_liste[[x]] %>% mutate(probant = substr(dateiliste[x], 81, 85))
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
  df[[x]] <- gap_fill(df[[x]], max_gap_length = 75)
  df[[x]] <- noise_reduction(df[[x]], method = median, window_size = 3)
  df[[x]] <- classify_iaoi(df[[x]], min_fixation_duration = 100)
  df[[x]] <- merge_fixations_iaoi(df[[x]], max_time = 75)
  df[[x]] <- calculate_velocity2(df[[x]], window_length = 20)
  df[[x]] <- calculate_AOI_fixation(df[[x]])
  df[[x]] <- classify_ivt(df[[x]], velocity_threshold = 100)
  df[[x]] <- merge_fixations_iaoi2(df[[x]], max_time = 75)
  df[[x]] <- merge_fixations_ivt(df[[x]], max_time = 75, max_angle = 0.5)
  
}


