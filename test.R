source("R/Rpath.R")

lst = list(first = TRUE, second = FALSE, third = list(fourth = 5, fifth = "hi"), sixth = "SO FUNNY!!!")

rpath(lst, "/.third")

rpath(lst, "/.third.fourth")

rpath(lst, "/.*[.fourth]") #doesn't work!!!
rpath(lst, "/.third.*")
rpath(lst, "/.third[.fourth]")

rpath(lst, "/.logical", use_classes = TRUE)
rpath(lst, "/.list", use_classes = TRUE)
rpath(lst, "/.list.character", use_classes = TRUE)
