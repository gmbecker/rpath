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
rpath(lst, "/*/fourth")
rpath(lst, "//third")

rpath(lst, "//logical", use_classes=TRUE)

lst2 = c(lst, lst)

rpath(lst2, "/third")
rpath(lst2, "/third/fourth")

rpath(lst2, "/third[1]/fourth")

rpath(lst2, "/third[1]")


lst3 = list(first = TRUE, second = FALSE, third = list(fourth = 5, fifth = "hi"), sixth = "SO FUNNY!!!", third = list(fourth = 6, fifth = "yay!"), third = list(fourth=3, fifth="not6"))
rpath(lst3, "/third[fourth=='6']")
rpath(lst3, "/third")
rpath(lst3, "/third[fourth!='6']")


attrfun = function(obj) c(sapply(names(obj), function(x) obj[[x]], simplify=FALSE), class = class(obj))

rpath(lst3, "/third/@class", attr_fun = attrfun)
rpath(lst3, "/third/@fourth", attr_fun = attrfun)
rpath(lst3, "/third/*[@class=='character']", attr_fun = attrfun)
rpath(lst3, "/third[@fourth=='6']", attr_fun = attrfun)

rpath(lst3, "/third[!fourth]")
rpath(lst3, "/third[!zzz]")
rpath(lst3, "/third[zzz]")
rpath(lst3, "/third[zzz||fourth=='6']") #not working
