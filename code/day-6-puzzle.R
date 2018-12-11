# Advent of Code 2018
# Day 6 Puzzle - https://adventofcode.com/2018/day/6
#
rm(list=ls())
startTime = Sys.time() # time the speed of the code
# Set Directory & Options
setwd('~/GitHub/adventofcode2018') 
options(stringsAsFactors = F)

# Input Data
map = read.delim('data/day-6-input.txt', sep = ',',
                 header = F,  encoding = 'UTF-8', 
                 col.names = c('X','Y'))

map = read.delim('clipboard', sep = ',', col.names = c('X','Y'))

# Part 1: Calc largest manhattan distance that isn't infinite
dist(map, method = 'manhattan')


mat = matrix(data = 0, nrow = 50, 
             ncol = 50)
for(i in 1:nrow(data)){
  mat[data$Y[i], data$X[i]] = 1
}
d = dist(x = mat,method = 'manhattan')

# Part 2:

print(difftime(Sys.time(), startTime))