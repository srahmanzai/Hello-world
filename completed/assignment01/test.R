theURL <- "http://www.jaredlander.com/data/tomatoFirst.csv"
tomato <- read.table (file=theURL, header=TRUE, sep=",")
head(tomato)