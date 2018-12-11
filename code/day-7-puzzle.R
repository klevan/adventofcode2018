# Advent of Code 2018
# Day 7 Puzzle - https://adventofcode.com/2018/day/7
#
rm(list=ls())
startTime = Sys.time() # time the speed of the code
# Set Directory & Options
setwd('~/GitHub/adventofcode2018') 
options(stringsAsFactors = F)

# Input Data
path = read.delim('data/day-7-input.txt', 
                  col.names = 'instruction',
                  header = F,  encoding = 'UTF-8')
path$prereq = substr(path[,1],6,6)
path$step = substr(path[,1],37,37)

# Part 1: Determine ordering of steps ---------
# Are there any steps without prereqs?
# Take the first one available, that is highest in the alphabet
strt = sort(unique(path$prereq[!path$prereq%in%path$step]))
dir = strt[1]
while (length(dir)!=length(unique(c(path$step,path$prereq)))){
  # What's available now?
  # Get a sorted list of step that are
  avail = unique(sort(c(path$step[path$prereq%in%dir& # now just available
                                    !path$step%in%dir], # and were not already selected
                        strt[!strt%in%dir]))) # or had no prereqs
  # In the availability list are there any 'multiple' prereqs that aren't fulfilled?
  avail = avail[!avail%in%path$step[!path$prereq%in%dir]] # remove those
  
  dir = c(dir, avail[1])
  print(dir)
}
dir = paste0(dir, collapse = '')
print(dir)

# Part 2: Each step takes time, but there are 5 people executing the steps -------
# Are there any steps without prereqs?
# Take the first one available, that is highest in the alphabet
strt = sort(unique(path$prereq[!path$prereq%in%path$step]))
dir = vector() # create container for the answer
time = 0; n = 2; wrk = lag = vector()
while (length(dir)!=length(unique(c(path$step,path$prereq)))){
  # What's available now?
  # Get a sorted list of step that are
  avail = unique(sort(c(path$step[path$prereq%in%dir& # now just available
                                    !path$step%in%dir], # and were not already selected
                        strt[!strt%in%dir]))) # or had no prereqs
  # In the availability list are there any 'multiple' prereqs that aren't fulfilled?
  avail = avail[!avail%in%path$step[!path$prereq%in%dir]] # remove those
  
  # How long will each timestep take?
  if(length(wrk)>0){
    new = avail[!avail%in%names(wrk)]
    lag = c(wrk, sapply(new, function(n) which(LETTERS%in%n)+0))# +60  
  } else {
    lag = sort(sapply(avail, function(n) which(LETTERS%in%n)+0))# +60
  }
  
  
  # Are there people available to take a task?
  if(length(wrk)<n){
    if((n-length(wrk))>length(lag)){
      wrk = c(wrk,lag)  
    } else {
      wrk = c(wrk,lag[1:(n-length(wrk))])
    }
  }
  
  time = time + min(wrk) # add seconds to timer
  # remove time from workers
  wrk = wrk - min(wrk)
  
  dir = c(dir, names(wrk[wrk%in%0]))
  wrk = wrk[!wrk%in%0]
  print(dir)
}
dir = paste0(dir, collapse = '')
print(dir)


print(difftime(Sys.time(), startTime))