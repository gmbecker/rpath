
setClass("rpath_match", representation(value = "ANY",
                                       indices = "integer"))

rpath_match = function(value, term_condition = NULL, indices = integer())
{
    if(is(value, "rpath_match"))
        value
    else
    {
        if(!is.null(term_condition) && term_condition(value))
            new("terminal_node", value = value, indices = indices)
        else
            new("rpath_match", value = value, indices = indices)
    }
}

setClass("rpath_constant", representation = list(type="character"), contains = "rpath_match")

rpath_const = function(type, value)
{
    if(!is(value, type))
        stop("constant value ", value, "does not match declared type ", type)
    new("rpath_constant", type = type, value = value)
}

setClass("rpath_matchList", representation(matches = "list", indices = "integer"), validity = function(object) all(sapply(object@matches, function(x) is(x, "rpath_match"))))

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


setClass("rpath_step", representation = list(type = "character", payload = "list", index = "numeric", namespace = "character"))

rpath_step = function(type, payload, index = numeric())
{
    namespace = ""
    if(type == "node" && is(payload, "character")) {
        if(grepl("^@", payload)) {
            type = "attribute"
        } else if (grepl(":", payload, fixed=TRUE)) {
            tmp = strsplit(payload, ":")[[1]] # list of length 1, [[1]] gives us the answers
            namespace = tmp[1]
            payload = tmp[2]
        }
    }

    if(type == "attribute" && is(payload, "character")) {
        payload = gsub("@", "", payload)
        if(grepl("~", payload, fixed=TRUE)) {
            tmp = strsplit(payload, "~")[[1]]# list of length 1, [[1]] gives us the answers
            namespace = tmp[1]
            payload = tmp[2]
        }
    }

    if(!is(payload, "list"))
        payload = list(payload)
    new("rpath_step", type = type, payload = payload, index = index, namespace = namespace)
}
