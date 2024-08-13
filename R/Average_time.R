library(dplyr)


# Show Tasks with
result <- df3 %>%
  filter(Abgeschlossen == 1) %>%
  # milliseconds to minutes
  mutate(duration_min = duration / 60000) %>%  
  group_by(task) %>%
  summarise(
    total_duration_min = sum(duration_min),
    # Number of records for each task
    count = n(),  
    # Average Duration of Task
    duration_min_avg = total_duration_min / count  
  ) %>%
  mutate(
    # Minutes
    minutes = floor(duration_min_avg),  
    # Seconds
    seconds = round((duration_min_avg - minutes) * 60),  
    # Format as Min:Sec
    duration_min_sec = sprintf("%02d:%02d", minutes, seconds) 
  )


print(result %>% select(task, duration_min_sec))



# Show Tasks with
result2 <- df3 %>%
  filter(Abgeschlossen == 0) %>%
  # milliseconds to minutes
  mutate(duration_min = duration / 60000) %>%  
  group_by(task) %>%
  summarise(
    total_duration_min = sum(duration_min),
    # Number of records for each task
    count = n(),  
    # Average Duration of Task
    duration_min_avg = total_duration_min / count  
  ) %>%
  mutate(
    # Minutes
    minutes = floor(duration_min_avg),  
    # Seconds
    seconds = round((duration_min_avg - minutes) * 60),  
    # Format as Min:Sec
    duration_min_sec = sprintf("%02d:%02d", minutes, seconds) 
  )


print(result2 %>% select(task, duration_min_sec))


# Show Tasks with
result3 <- df3 %>%
  # milliseconds to minutes
  mutate(duration_min = duration / 60000) %>%  
  group_by(task) %>%
  summarise(
    total_duration_min = sum(duration_min),
    # Number of records for each task
    count = n(),  
    # Average Duration of Task
    duration_min_avg = total_duration_min / count  
  ) %>%
  mutate(
    # Minutes
    minutes = floor(duration_min_avg),  
    # Seconds
    seconds = round((duration_min_avg - minutes) * 60),  
    # Format as Min:Sec
    duration_min_sec = sprintf("%02d:%02d", minutes, seconds) 
  )


print(result3 %>% select(task, duration_min_sec))

