
makeParsers = function(state, ...)
{
    
    
    parsers <- list(
        '^//$' = function(match, index, state)
    {
        list("allnodes", "")
    },
        '"([^"]*?)"' = function(match, index = length(state$result) + 1, state)
    {
#        state$result[[index]] <- list("string", match)
        list("string", match)
    },
        '(/[^=[:space:]!]*)' = function(match, index = length(state$result) + 1, state)
    {
        if (match == ".")
            match = ""
        else
        {
#            match = rpath_split(match, state = state)
            match = rpath_split(match, state = new.env())
            if(length(match) == 2)
                match = match[[2]]
        }
        #state$result[[index]] <- c("node", match);
        list("node", match)
    },
        '(==|!=|!|\\|\\||&&)' = function(match, index = length(result) + 1, state)
    {
#        state$result[[index]] <- list("operator", match)
        list("operator", match)
    }
        )
    args = list(...)
    parsers[names(args)] = args
    parsers
}

rpath_parse = function(predicate, parsers = makeParsers(state = state), state)
{
#    state$result <- list()
    for( i in seq(1, length(parsers)))
    {
        #find out what the js replace method on arrays does
        #predicate = predicate.replace(parsers[[i]], parsers[[i+1]])
        expr = names(parsers)[i]
        tmpmatch = gregexpr(expr, predicate, perl=TRUE)[[1]]
        if(any(tmpmatch > 0))
        {
            matches = regmatches(predicate, tmpmatch)
            res = sapply(matches, parsers[[i]], state = state)
            gsub(expr, "", predicate, perl=TRUE)
#            regmatches(predicate, matches) <- ""
        }
    }

    #get rid of annoying
#    predicate = gsub("^\\.([^\\[\\(/]+)$", "\\1", predicate) 
    predicate = rpath_split(predicate, parsers, new.env())
    state$lastSteps <- c(state$lastSteps, predicate)

   # return(state$result)
    predicate
}
