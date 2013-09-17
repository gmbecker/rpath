
setClass("rpath_match", representation(value = "ANY"))

rpath_match = function(value, term_condition = NULL)
{
    if(is(value, "rpath_match"))
        value
    else
    {
        if(!is.null(term_condition) && term_condition(value))
            new("terminal_node", value = value)
        else
            new("rpath_match", value = value)
    }
}

setClass("rpath_matchList", representation(matches = "list"), validity = function(object) all(sapply(object@matches, function(x) is(x, "rpath_match"))))

setAs("list", "rpath_matchList", function(from) new("rpath_matchList", matches = from))

setAs("rpath_matchList", "list", function(from) from@matches)

#this should be entirely unnecessary but it isn't....for "reasons" (lapply calls as.list directly)
as.list.rpath_matchList = function(from) as(from, "list")



setClass("terminal_node", contains = "rpath_match")

terminal_node = function(value)
{
    if(is(value, "terminal_node"))
        value
    else
        new("terminal_node", value = value)
}

is_terminal_node = function(value) is(value, "terminal_node")

setClass("no_match", contains = "rpath_match")

no_match_found= function()
{
    new("no_match")
}

no_match = function(obj)
    {
        if(is(obj, "rpath_matchList"))
        {
            
            ( !length(obj) || all(sapply(obj@matches, no_match) ) ) 
        } else {
            is(obj, "no_match")
        }
    }

trim_matchList = function(mlist)
{
    realmatch = sapply(mlist@matches, function(x) !no_match(x))
    mlist[realmatch]
}