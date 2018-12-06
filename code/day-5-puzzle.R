# Advent of Code 2018
# Day 5 Puzzle - https://adventofcode.com/2018/day/5
#
rm(list=ls())
startTime = Sys.time() # time the speed of the code
# Set Directory & Options
setwd('~/GitHub/adventofcode2018') 
options(stringsAsFactors = F)

# Input Data
polymer = as.character(read.delim('data/day-5-input.txt', 
                                  header = F,  encoding = 'UTF-8'))

# Part 1: How many units remain after fully reacting the polymer you scanned?
# Setup; so we can track progress
x = t = 1; start = nchar(polymer)
while(x<3){
  L = LETTERS[t]
  l = letters[t]
  # Remove Aa & aA, and repeat for all letters
  polymer = gsub(paste0(L,l), '', polymer)
  polymer = gsub(paste0(l,L), '', polymer)
  
  n = nchar(polymer) # is this string getting shorter?
  print(n)
  if(t<26){
    t = t+1
  } else {
    t = 1
    if(start>n){
      start = n
    } else{
      x = x + 1
    }
  }
}

print(n) # this is the most compact under Part 1 conditions

# Part 2: If one letter (both cases) were removed, 
# which one should be removed to make the most compact polymer?
poly = vector()
for(alpha in LETTERS){
  polymer = as.character(read.delim('data/day-5-input.txt', 
                                    header = F,  encoding = 'UTF-8'))
  polymer = gsub(paste0('[',alpha,tolower(alpha),']{1}'), '', polymer)
  x = t = 1; start = nchar(polymer)
  while(x<3){
    L = LETTERS[t]
    l = letters[t]
    # Remove Aa & aA, and repeat for all letters
    polymer = gsub(paste0(L,l), '', polymer)
    polymer = gsub(paste0(l,L), '', polymer)
    
    n = nchar(polymer) # is this string getting shorter?
    print(n)
    if(t<26){
      t = t+1
    } else {
      t = 1
      if(start>n){
        start = n
      } else{
        x = x + 1
      }
    }
  }
  poly = c(poly, n)
}

# removing all instances of this letter would make the polymer more compact
LETTERS[which(poly%in%min(poly))]
# and the length is
min(poly)

print(difftime(Sys.time(), startTime))