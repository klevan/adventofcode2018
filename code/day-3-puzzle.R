# Advent of Code 2018
# Day 3 Puzzle - https://adventofcode.com/2018/day/3
#
rm(list=ls())
startTime = Sys.time() # time the speed of the code
# Set Directory & Options
setwd('~/GitHub/adventofcode2018') 
options(stringsAsFactors = F)

library(sp)
library(raster)

# Input Data
suit = read.delim('data/day-3-input.txt', header = F, 
                  col.names = 'loc', encoding = 'UTF-8')

# Part 1: How many squares are in two or more claims?
# Example - #123 @ 3,2: 5x4 
# where 1,1 is the start of the suit; this example specifies 
# one corner = x + 3, y + 2; 
# opposite corner = x + 3 + 5, y + 2 + 3
# 
# Format:
#ID @ x,y: x*y
# 1 @ 1,3: 4x4
# 2 @ 3,1: 4x4
# 3 @ 5,5: 2x2

# Solution:
# suit = read.delim('clipboard', col.names = 'loc', header = F)
# For each row & each column, are there overlaps?
# Make a Polygon for each claim
makePoly <- function(input){
  # Get the values
  vals = unlist(strsplit(input, split = ' '))
  # Set the beginning point
  beg = as.numeric(gsub(':','',unlist(strsplit(vals[3],split = ',')))) # x, y
  # What is the furthest point?
  inc = as.numeric(unlist(strsplit(vals[4],split = 'x'))) # x, y
  # Make a matrix
  m <- matrix(c(beg, # start corner
                beg[1]+inc[1],beg[2], # X corner
                beg[1]+inc[1],beg[2]+inc[2], # far corner
                beg[1],beg[2]+inc[2]), # Y corner
              ncol = 2, byrow = TRUE)
  
  out <- Polygons(list(Polygon(m)), gsub('#','',vals[1]))
  return(out)
}
locs = sapply(suit$loc, makePoly)
polys = SpatialPolygons((locs))

plot(polys, border = rainbow(n = 3), 
     xlab = "X", ylab = "Y", 
     main = "Claims on the Santa Suit material",
     xlim=c(0,1000),ylim=c(0,1000))
axis(1);axis(2); box()

# Convert each item into it's list of addresses, 
# then look for ones with > 1 instance
makeList <- function(input){
  # Get the values
  vals = unlist(strsplit(input, split = ' '))
  # Set the beginning point
  beg = as.numeric(gsub(':','',unlist(strsplit(vals[3],split = ',')))) # x, y
  # What is the furthest point?
  inc = as.numeric(unlist(strsplit(vals[4],split = 'x'))) # x, y
  
  mini = (beg + 1) # x, y start corner
  maxi = (beg + inc) # x, y max corner
  coord = vector()
  for(x in mini[1]:maxi[1]){
    for (y in mini[2]:maxi[2]){
      coord = c(coord, paste(x,y,sep=','))
    }
  }
  return(coord)
}

coords = (sapply(suit$loc, makeList))
cor = unlist(coords)
dup = unique(cor[duplicated(cor)])

length(dup) # area of duplicate claims

# Part 2: Find the one square that doesn't overlap
# unduplicated coordinates
for (i in 1:nrow(suit)){
  if(length(coords[[i]][coords[[i]]%in%dup])==0){
    print(suit[i,])
  }
}

print(difftime(Sys.time(), startTime))