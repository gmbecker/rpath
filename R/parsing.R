op_regex = '([^\\[!=&\\|]+|\\([^\\)]+\\))(==|!=|\\|\\||&&|<={0,1}|>={0,1})([^\\[!=&<>]+|\\([^\\)]+\\))'
makeParsers = function(state, ...)
{
    
    parsers <- list(
        '^//$' = function(match, index, state)
    {
        rpath_step("allnodes", "")
#        list("allnodes", "")
    },
        '^("|\')([^"]*?)\\1$' = function(match, index = length(state$result) + 1, state)
    {
#        state$result[[index]] <- list("string", match)
#        list("string", match)
        match = gsub("(^('|\")|('|\")$)", "", match)
        rpath_step("string", match)
    },
        '^@[^=[:space:]!]*$' = function(match, index, state)
    {
        match = gsub("@", "", match, fixed= TRUE)
        rpath_step("attribute", match)
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
       # list("node", match)
        rpath_step("node", match)
    },
        #XXX I think these are going to cause problems with complicated paths like /a[b[c==5]/d == 6]
        #'^([^\\[!=&\\(\\|]+|\\([^\\)]+\\))(==|!=|\\|\\||&&)([^\\[!=&\\|\\(]+|\\([^\\)]+\\))$' = function(match, index, state)
        op_regex = function(match, index, state)
    {
        op = gsub( op_regex, "\\2", match)
        tmp = gsub(op, paste0("_=-_", op, "_=-_"), match, fixed=TRUE)
        sides = strsplit(tmp, split = paste0("_=-_", op, "_=-_"), fixed = TRUE)[[1]]
        #op = gsub(".*(==|!=|\\|\\||&&).*", "\\1", match)
       # sides = strsplit(match, split=op, fixed=TRUE)[[1]]
#        list(list("operator", op), rpath_parse(sides[1], state = new.env()), rpath_parse(sides[2], state = new.env()))
        list(rpath_step("operator", op), rpath_parse(sides[1], state = new.env()), rpath_parse(sides[2], state = new.env()))
    },
        '![^\\]]+' = function(match, index, state)
    {
        op = "!"
        node = gsub(op, "", match,fixed = TRUE)
        list(rpath_step("operator", op), rpath_parse(node, state = new.env()))
 #       list(list("operator", op), rpath_parse(node, state = new.env()))
    })
    #annoying but need this to get the op parser to have the right name in the list
    names(parsers)[names(parsers)=="op_regex"] = op_regex
    args = list(...)
    parsers[names(args)] = args
    parsers
}

rpath_parse = function(predicate, parsers = makeParsers(state = state), state)
{
    if(!nchar(predicate))
        return(NULL)
    ret = list()
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
            res = lapply(matches, parsers[[i]], state = state)
            if(length(res) == 1)
                ret[length(ret) + 1] = res
            else if (length(res) > 1)
                ret = c(ret, res)
                
            predicate = gsub(expr, "", predicate, perl=TRUE)
#            regmatches(predicate, matches) <- ""
        }
    }

    #get rid of annoying
#    predicate = gsub("^\\.([^\\[\\(/]+)$", "\\1", predicate) 
    if(nchar(predicate))
    {
        predicate = rpath_split(predicate, parsers, new.env())
        state$lastSteps <- c(state$lastSteps, predicate)
        ret = c(ret, predicate)
    }
   # return(state$result)
#    predicate
#    unlist(ret, recursive = FALSE)
    ret
}
