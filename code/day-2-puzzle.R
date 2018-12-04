# Advent of Code 2018
# Day 2 Puzzle - https://adventofcode.com/2018/day/2
#
rm(list=ls())
startTime = Sys.time() # time the speed of the code
# Set Directory & Options
setwd('~/GitHub/adventofcode2018') 
options(stringsAsFactors = F)

# Input Data
boxes = read.delim('data/day-2-input.txt', header = F,
                   col.names = 'IDs', encoding = 'UTF-8')

# Part 1: Checksum; look for doubles & triples
# Which are doubled?
boxes.split = strsplit(boxes$IDs, split = '')
boxes[, c('dbl', 'tpl')] = NA
for(n in 1:nrow(boxes)){
  b = boxes.split[[n]]
  dups = b[duplicated(b)]
  
  if(length(b)==0){
    boxes$dbl[n] = boxes$tpl[n] = 0  
  } else {
    for(m in dups){
      if(length(b[b%in%m])%in%2){
        boxes$dbl[n] = 1
      }
      if(length(b[b%in%m])%in%3){
        boxes$tpl[n] = 1
      }
    }
  }
}
# Determine Checksum
print(sum(boxes$dbl, na.rm = T) * sum(boxes$tpl, na.rm = T))

# Part 2: Figure out which box IDs are only one letter apart
# Use the distance function to assess differences
boxes = boxes$IDs
mat = as.data.frame(adist(boxes))
rownames(mat) = boxes # assign names
box.list = (rownames(which(mat == 1, arr.ind = TRUE)))
# remove differing characters

print(difftime(Sys.time(), startTime))