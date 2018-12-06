# Advent of Code 2018
# Day 4 Puzzle - https://adventofcode.com/2018/day/4
#
rm(list=ls())
startTime = Sys.time() # time the speed of the code
# Set Directory & Options
setwd('~/GitHub/adventofcode2018') 
options(stringsAsFactors = F)

# Input Data
duty = read.delim('data/day-4-input.txt', header = F, 
                  col.names = 'log', encoding = 'UTF-8')

# Get the mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Part 1: Find the guard that has the most minutes asleep. 
# What minute does that guard spend asleep the most?

# Clean up the duty log
duty$date = substr(duty$log,2,17) # Split time stamps from file
duty$info = gsub('\\[[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}\\]',
                 '',duty$log) # get info separate from time stamp
duty$guardID = gsub('[A-z# ]','',duty$info) # find Guard ID from shift starts

# Sort by date
duty$date = as.POSIXct(duty$date)
duty = duty[order(duty$date), ]

# Determine guard on duty
n = 1; glist = which(!duty$guardID%in%'')
while (n<nrow(duty)){
  # What's the current guard's ID?
  gID = duty$guardID[n]
  # When is the next shift?
  if(which(glist%in%n)<length(glist)){
    nxt.shift = glist[which(glist%in%n)+1] 
    # Assign all current time stamps to current guard
    duty$guardID[n:(nxt.shift-1)] = gID
    
    n = nxt.shift
  } else {
    duty$guardID[n:nrow(duty)] = gID
    break
  }
}

# Which guard spent the most time asleep?
duty.summary = data.frame(guardID = unique(duty$guardID))
for(g in 1:nrow(duty.summary)){
  # Isolate that guard's schedule
  sch = duty[duty$guardID%in%duty.summary$guardID[g], ]
  sch$min = as.numeric(substr(sch$date, 15,16))
  
  sleep = mins = vector()
  for(time in 1:nrow(sch)){
    if(grepl('sleep', sch$info[time])){
      # Amount of time sleeping
      sleep = c(sleep, difftime(sch$date[time+1],sch$date[time], units = 'mins'))
      mins = c(mins, sch$min[time]:((sch$min[time+1])-1))
    }
  }
  
  duty.summary$totalMinutesAsleep[g] = sum(sleep)
  duty.summary$bestMin[g] = paste(getmode(mins), collapse = '|')
  
  # Use for Part 2
  if(!is.na(duty.summary$bestMin[g])){
    duty.summary$numInMode[g] = length(mins[mins%in%duty.summary$bestMin[g]])
  } else {
    duty.summary$numInMode[g] = NA
  }
  
}

# What is the ID of the guard you chose multiplied by the minute you chose?
as.numeric(duty.summary$guardID[duty.summary$totalMinutesAsleep%in%max(duty.summary$totalMinutesAsleep)])*
  as.numeric(duty.summary$bestMin[duty.summary$totalMinutesAsleep%in%max(duty.summary$totalMinutesAsleep)])

# Part 2: Of all guards, which guard is most frequently asleep on the same minute?
# What is the ID of the guard you chose multiplied by the minute you chose?
as.numeric(duty.summary$guardID[duty.summary$numInMode%in%max(duty.summary$numInMode)])*
  as.numeric(duty.summary$bestMin[duty.summary$numInMode%in%max(duty.summary$numInMode)])

print(difftime(Sys.time(), startTime))
