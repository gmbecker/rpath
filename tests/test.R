library(rpath)
lst = list(first = TRUE, second = FALSE, third = list(fourth = 5, fifth = "hi"), sixth = "SO FUNNY!!!")

stopifnot(identical(rpath(lst, "/third"),
                    list(lst$third)))

stopifnot(identical(rpath(lst, "/third/fourth"),
                    list(5)))

stopifnot(identical(rpath(lst, "/*[fourth]"),
                    rpath(lst, "/third")))

stopifnot(identical(rpath(lst, "/*"),
                    unname(lst)))

stopifnot(identical(rpath(lst, "/third/*"),
                    unname(lst$third)))

stopifnot(identical(rpath(lst, "/third[fourth]"),
                    rpath(lst, "/third")))

stopifnot(identical(rpath(lst, "/cl:logical"),
                    unname(lst[1:2])))

stopifnot(identical(rpath(lst, "/cl:list"),
                    rpath(lst, "third")))

stopifnot(identical(rpath(lst, "/cl:list/cl:character"),
                    rpath(lst, "/third/fifth")))

stopifnot(identical(rpath(lst, "//fourth"),
                    rpath(lst, "/third/fourth")))

stopifnot(identical(rpath(lst, "/*/fourth"),
                    rpath(lst, "//fourth")))

stopifnot(identical(rpath(lst, "//third"),
                    rpath(lst, "/third")))

stopifnot(identical(rpath(lst, "//cl:logical"),
                    rpath(lst, "/cl:logical")))

## note the order because its width-first traversal, not depth first
stopifnot(identical(rpath(lst, "//cl:character"),
                    list("SO FUNNY!!!", "hi")))

rpath(lst, "/third/*/*") ##XXX WRONG!!!!



lst2 = c(lst, lst)

stopifnot(identical(rpath(lst2, "/third"),
                    c(rpath(lst, "/third"),
                      rpath(lst, "/third"))))

stopifnot(identical(rpath(lst2, "/third/fourth"),
                    list(5, 5)))

stopifnot(identical(rpath(lst2, "/third[1]/fourth"),
                    rpath(lst, "/third/fourth")))

stopifnot(identical(rpath(lst2, "/third[1]"),
                    rpath(lst, "/third")))


lst3 = list(first = TRUE, second = FALSE, third = list(fourth = 5, fifth = "hi"), sixth = "SO FUNNY!!!", third = list(fourth = 6, fifth = "yay!"), third = list(fourth=3, fifth="not6"))



res5only <- rpath(lst3, "/third[fourth=='5']")
res6only <- rpath(lst3, "/third[fourth=='6']")
res3only <- rpath(lst3, "/third[fourth=='3']")

stopifnot(identical(res6only,
                    list(list(fourth = 6, fifth = "yay!"))))


stopifnot(identical(rpath(lst3, "/third"),
                    c(res5only, res6only, res3only)))

stopifnot(identical(rpath(lst3, "/third[fourth!='6']"),
                    c(res5only, res3only)))

stopifnot(identical(rpath(lst3, "/third[fourth<='5']"),
                    c(res5only, res3only)))




attrfun = function(obj) {
 inds = which(nchar(names(obj)) > 0)
 ret = sapply(names(obj)[inds], function(x) obj[[x]], simplify=FALSE)
 c(ret, class = class(obj))
}

#rpath(lst3, "/third/@class", attr_fun = attrfun)
#rpath(lst3, "/third/@fourth", attr_fun = attrfun)
#rpath(lst3, "/third/*[@class=='character']", attr_fun = attrfun)
#rpath(lst3, "/third[@fourth=='6']", attr_fun = attrfun)

stopifnot(identical(rpath(lst3, "/third/@class", as_funcs = attrfun),
                    rep(list("list"), 3)))

stopifnot(identical(rpath(lst3, "/third/@class", as_funcs = list(a=attrfun)),
                    rpath(lst3, "/third/@class", as_funcs = attrfun)))

stopifnot(identical(rpath(lst3, "/third/@fourth", as_funcs = list(a=attrfun)),
                    list(5, 6, 3)))

stopifnot(identical(rpath(lst3, "/third/*[@class=='character']", as_funcs = list(a=attrfun)),
                    list("hi", "yay!", "not6")))

stopifnot(identical(rpath(lst3, "/third[@fourth!='6']", as_funcs = list(a=attrfun)),
                    rpath(lst3, "/third[fourth!='6']")))

## doesn't work
## stopifnot(identical(rpath(lst3, "/third[@fourth=='6']", as_funcs = list(as=attrfun)),
##                     rpath(lst3, "/third[fourth=='6']")))


stopifnot(identical(rpath(lst3, "/third[!fourth]"),
                    list()))

stopifnot(identical(rpath(lst3, "/third[!zzz]"),
                    rpath(lst3, "/third")))
stopifnot(identical(rpath(lst3, "/third[zzz]"),
                    list()))

stopifnot(identical(rpath(lst3, "/third[zzz|fourth=='6']"),
                    rpath(lst3, "/third[fourth=='6']")))

lst4 = list(first = TRUE, second = FALSE, third = list(fourth = 5, fifth = "hi"), sixth = "SO FUNNY!!!", third = list(fourth = 6, fifth = "yay!"), third = list(fourth=3, fifth=c("not6", special="heehee")))

stopifnot(identical(rpath(lst4, "//fifth[@special]", as_funcs =list(a= attrfun)),
                    list(c("not6", special = "heehee"))))
