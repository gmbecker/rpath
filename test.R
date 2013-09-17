#source("R/Rpath.R")
#source("R/no_return.R")
library(rpath)
options(error=recover)
lst = list(first = TRUE, second = FALSE, third = list(fourth = 5, fifth = "hi"), sixth = "SO FUNNY!!!")

rpath(lst, "/third")

rpath(lst, "/third/fourth")

rpath(lst, "/*[fourth]")
rpath(lst, "/*")
rpath(lst, "/third/*")
rpath(lst, "/third/*/*")
rpath(lst, "/third[fourth]")

rpath(lst, "/logical", use_classes = TRUE)
rpath(lst, "/list", use_classes = TRUE)
rpath(lst, "/list/character", use_classes = TRUE)

rpath(lst, "//fourth")

lst2 = c(lst, lst)

rpath(lst2, "/third")
rpath(lst2, "/third/fourth")
