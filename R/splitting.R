

split_regex = "/(?!(/|[^\\[]+\\]))"
predicate_regex =  "^([^\\[]+)?\\[( *[^\\]]+) *\\]$";
                                        #predicate_regex =  "^([^\\[]+)"#(\\[( *[^\\]]+) *\\])*$";
index_regex = "^([[:digit:]]+)$"




rpath_split = function(path, parsers = makeParsers(state = state), state = new.env())
{
    steps = doSplit(path)

    if(steps[1] == "" || steps[1] == "/") {

        steps = steps[-1]
    }

    for(st in steps)
    {
        if(st == "//")
            state$result <- c(state$result, list(list("allnodes", "")))
        else
        {

            predmatch = matchPredicate(st)
            
            if(!no_match(predmatch)) {
                                        #   predicate = matchRes[[2]]
                if(grepl(index_regex, predmatch[3]))
                    tokens = list("index", as.numeric(predmatch[[3]]))
                else {
                    tokens = rpath_parse(predmatch[[3]], parsers = parsers, state = state)
                    tokens = flatten(compact(tokens))
                tokens = regroup(tokens)
                }
                
                if(nchar(predmatch[2])) {
                    state$result <- c(state$result, list(list("node", predmatch[2])))
                }
                state$result <- c(state$result, list(list("predicate", tokens)))
            } else {
                state$result <- c(state$result, list(list("node", st)))
            }
        }
    }
    
    state$result
}



#can't get predicate_regex to match all 3 parts (ie "c[.e]"-> 'c[.e]', c, .e) so this is a hack that seems to work

matchPredicate = function(path)
{
    match = grepl(predicate_regex, path, perl=TRUE)
    
    if(!match)
        return(no_match_found())

    res = strsplit(path, "(\\[|\\])")[[1]]

    c(path, res)
}


doPredicate = function(path)
{
    predmatch = matchPredicate(path)

    if(no_match(predmatch))
        return(no_match_found())

    if(grepl(index_regex, predmatch[3]))
        parsedPred = list("index", as.numeric(predmatch[3]))
    else
       # parsedPred = rpath_parse(path)
        parsedPred = rpath_split(path, state = new.env())

    list(list("node", predmatch[2]), parsedPred)
}

#Argh! why can't I figure out a regular expression to do this :(
doSplit = function(path)
{

    res = strsplit(path, split_regex, perl=TRUE)[[1]]
    slashes = grepl("/$", res)
    res2 <- mapply(function(x, doit)
               {
                   res  = x
                   if(doit)
                   {
                       x = gsub("/$", "", x)
                       if(nchar(x))
                           res = c(x, "//")
                       else
                           res = "//"
                   }
                   res
               }, res, slashes, USE.NAMES=FALSE)
    unlist(res2)
}
