# Advent of Code 2018
# Day 1 Puzzle - https://adventofcode.com/2018/day/1
#
rm(list=ls())
startTime = Sys.time() # time the speed of the code
# Set Directory & Options
setwd('~/GitHub/adventofcode2018') 
options(stringsAsFactors = F)

# Input Data
chg = read.delim('data/day-1-input.txt', header = F, 
                 col.names = 'freq', encoding = 'UTF-8')
# Part 1: Sum all
sum(chg$freq)

# Part 2: Determine first duplicate number
# when duplicate is in existing list, can use 'duplicated' function to find dup
# when many iterations required, can use the fact that the sum() of the list 
# is the new starting number whereby all list items are incremented
# 
# Determine the list of increments
t = 0; add = vector();
for(n in 1:nrow(chg)){
  # Current time step
  t = (t + chg$freq[n])
  # Log the time step
  add = c(add, t)
}
rm(n,t)
# If there is a duplicate in the first try
if(any(duplicated(add))){
  # print that duplicate
  print(add[duplicated(add)][1])
} else {
  # Increment entire list by the sum 
  # until duplicate is found
  final = add;
  while(!any(duplicated(final))){
    # Log the time step
    # If a duplicate is found
    # then the while loop ends
    add = (sum(chg$freq) + add)
    final = c(final, add)

  }
  print(final[duplicated(final)][1])
}
print(difftime(Sys.time(), startTime))