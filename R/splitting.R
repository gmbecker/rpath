

#split_regex = "/(?!(/|[^\\[@]+\\]))"
split_regex = "/(?!(/|&[^\\[]+\\]))"
predicate_regex =  "^([^\\[]+)?\\[( *[^\\]]+) *\\]$";
                                        #predicate_regex =  "^([^\\[]+)"#(\\[( *[^\\]]+) *\\])*$";
index_regex = "^([[:digit:]]+(\\:[[:digit:]]+)?)$"




rpath_split = function(path, parsers = makeParsers(state = state), state = new.env())
{
    if(is.null(state$result))
        state$result = list()
    steps = doSplit(path)

    if(steps[1] == "" || steps[1] == "/") {

        steps = steps[-1]
    }

    i = 1
    while(i <= length(steps))
#    for(st in steps)
    {
        st = steps[i]
        if(st == "//")
        {
            if(i == length(steps))
                stop("path seems to have terminated with '//'. This is not valid, was '//*' intented?")
            state$result <- c(state$result, rpath_step("allnodes", rpath_split(steps[i+1], state = new.env() )))
            steps = steps[-i]
        } else {
            predmatch = matchPredicate(st)

            if(!no_match(predmatch)) {
                nodelst = NULL
                tokens = NULL
                #rpath_step function handles detecting when the "node" is an attribute. Clunky/gross to have it there but lots of code duplication otherwise
                if(nchar(predmatch[2]))
                    nodelst = rpath_step("node", predmatch[2])
                if(grepl(index_regex, predmatch[3])){
                    if(!is.null(nodelst))
                        nodelst@index = eval(parse(text=predmatch[[3]])) #support numbers and x:y syntax
                } else {
                    tokens = rpath_parse(predmatch[[3]], parsers = parsers, state = state)
                    tokens = regroup(unlist(tokens, recursive = FALSE))
                }

                if(!is.null(nodelst))
                    state$result[[length(state$result) + 1]] <- nodelst
                if(!is.null(tokens))
                    state$result[[length(state$result) + 1]] <- rpath_step("predicate", tokens)
            } else {
                
                state$result[[length(state$result) + 1]] <- rpath_step("node", st)
            }
        }
        i = i + 1
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
