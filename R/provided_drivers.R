getClassesVec = function(obj)
    {
        if(is(obj, "rpath_matchList"))
            obj = obj@matches
        sapply(obj, function(x) class(x)[1])
    }


list_termination= function(x)
{
    ret = FALSE
    if(is.null(x) || !is.list(x) || !length(x))
        ret = TRUE
    ret
}
