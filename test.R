source("R/Rpath.R")

lst = list(first = TRUE, second = FALSE, third = list(fourth = 5, fifth = "hi"), sixth = "SO FUNNY!!!")

rpath(lst, "/.third")

rpath(lst, "/.third.fourth")

rpath(lst, "/.*[.fourth]")
rpath(lst, "/.third.*")
rpath(lst, "/.third[.fourth]")
