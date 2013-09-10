source("R/Rpath.R")

lst = list(first = TRUE, second = FALSE, third = list(fourth = 5, fifth = 6), sixth = "SO FUNNY!!!")

rpath(lst, "/.third")

rpath(lst, "/.third.fourth")
