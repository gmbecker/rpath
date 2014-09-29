##' @export
getClassesVec = function(obj)
    {
        if(is(obj, "rpath_matchList"))
            obj = obj@matches
        sapply(obj, function(x) class(x)[1])
    }

##' List termination condition
##' Termination condition which traverses to the first non-list (e.g. a vector) element
##' @export
list_termination= function(x)
{
    ret = FALSE
    if(is.null(x) || !is(x, "list") || !length(x))
        ret = TRUE
    ret
}


##' Namespace resolution functions
##' A list of node-name resolution functions by namespace
##' Provided namespaces are 'cl' for element class and nm for element name (elements with  no name are given name "", which will never match a non-wildcard (*) rpath directive.
##' @export
nsFuncs = list(cl = getClassesVec,
     nm = function(x) {
         nm = names(x)
         if(length(nm))
             nm
         else
             rep("", length(x))
     })

##' Attributespace resolution functions
##' A list of attribute resolution fuctions by attributespace
##' Provided attributespaces are 'a' for object attributes
##' @export
asFuncs = list("a" = attributes)

