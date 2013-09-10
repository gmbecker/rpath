node_exec = function(robj, node, exist, executors, state)
{
    node = unlist(node, recursive = FALSE)
    if(is.character(node) && identical(node, ""))
        return(robj)

    len = length(robj)
    if(len > 1)
    {
        arr = list()
        for(i in seq(1, len))
        {
#            res = rpath_exec(robj[[i]], c("node", node), exist, executors = executors, state = state);
                        res = rpath_exec(robj[i], c("node", node), exist, executors = executors, state = state);
            if(!is.null(res))
                arr = c(arr, res)
        }
        if(exist)
            ret = length(arr)
        else
            ret = arr
    } else if (is.list(node) && length(node) > 1) {
        res = robj
        for(i in seq(1, length(node), by=2)) {
            res = rpath_exec(res, node[i + 0:1], exist, executors = executors, state = state)
        }
        ret = res
    }  else  {
        if(identical(node, "*"))
        {
            arr = list()
            for(key in names(robj))
                arr = c(arr, robj[[key]])
            if(exist)
                ret = length(arr)
            else
                ret = arr
        } else if (node %in% names(robj)) {
            val = robj[[node]]
            if(exist)
                ret = if(is.list(val)) length(val) else val
            else
                ret = robj[[node]]
        } else {
            ret = NULL
        }
    }
    ret
}

index_exec = function(robj, index, executors, state)
{
    if(index <= length(robj))
        robj[[index]]
    else
        NULL
}
       
    
executors <- list( 
    node = node_exec,
    index =  index_exec,
    not = function(robj, operand, executors, state) rpath_exec(robj, operand, TRUE, executors = executors, state = state),
    eq = function(robj, operands,executors,  state) rpath_compare(robj, operands[[1]], operands[[2]]),
    noteq = function(robj, operands, executors, state) !rpath_compare(robj, operands[[1]], operands[[2]]),
    or = function(robj, operands, executors, state) rpath_exec(robj, operands[[1]], TRUE, executors = executors, state = state) || rpath_exec(robj, operands[[2]], TRUE, executors = executors, state = state),
    and = function(robj, operands, executors, state) rpath_exec(robj, operands[[1]], TRUE, executors = executors, state = state) && rpath_exec(robj, operands[[2]], TRUE, executors = executors, state = state),
    string = function(robj, string, executors, state) string
    )


rpath_compare = function(lNode, rNode)
{
    llst = is.vector(lNode)
    rlst = is.vector(rNode)

    ret = FALSE
    
    if(rlst && !llst)
    {
        llst = TRUE
        rrst = FALSE
        tmp = rNode
        rNode = lNode
        lNode = tmp
    }

    if(rlst && llst)
    {
        for(li in seq(length(lNode), 1, by=-1)){
            for(ri in seq(length(rNode), 1, by=-1)){
                if(lNode[[li]] == rNode[[ri]])
                    ret = TRUE
            }
        }
    } else if (llst && !rlst) {
        for(li in seq(length(lNode), 1, by=-1)) {
            if(lNode[[li]] == rNode)
                ret = TRUE
        }
    } else if (lNode == rNode) {
        ret = TRUE
    }

    ret
}

    


rpath_exec <- function(robj, step, exist=FALSE, executors = executors, state)
{
    if(step[[1]] == "predicate") {
        if (is.vector(robj) && length(robj) > 1 && step[[2]][[1]] != 'index') {
            arr = list();
            
            for (i in seq(along = robj))
            {
                #res = rpath_exec(robj[[i]], exist = exist, step = step ,executors = executors, state = state);
                #trying to get individual sections to retain names for indexing ith [] instead of [[]]
                res = rpath_exec(robj[i], exist = exist, step = step ,executors = executors, state = state);
                if (!is.null(res))
                    arr = c(arr, res)
            }
            return(arr);
        } else {
            res = rpath_exec(robj, step = step[[2]], exist = TRUE, executors= executors, state = state)
            if(is.logical(res))
            {
                if(!res)
                    res = NULL
                else
                    res = robj
            } 
            return(res)
        }
    } else {
#        return(executors[[ step[[ 1 ]] ]](robj, step[[1]], exist, executors = executors, state = state))
                return(executors[[ step[[ 1 ]] ]](robj, step[-1], exist, executors = executors, state = state))
    }
    NULL
}



split_regex = "\\.(?![^\\[]+\\])"
predicate_regex =  "^([^\\[]+)?\\[( *[^\\]]+) *\\]$";
                                        #predicate_regex =  "^([^\\[]+)"#(\\[( *[^\\]]+) *\\])*$";
index_regex = "^([[:digit:]]+)$"

makeParsers = function(state, ...)
{
    
    
    parsers <- list(
        '"([^"]*?)"' = function(match, index, state)
    {
        state$result[[index]] <- list("string", match)
    },
        '(\\.[^=[:space:]!]*)' = function(match, index, state)
    {
        if (match == ".")
            match = ""
        else
        {
            match = rpath_split(match, state = state)
            if(length(match) == 2)
                match = match[[1]]
        }
        state$result[[index]] <- c("node", match);
    },
        '(==|!=|!|\\|\\||&&)' = function(match, index = length(result) + 1, state)
    {
        state$result[[index]] <- list("operator", match)
    }
        )
    args = list(...)
    parsers[names(args)] = args
    parsers
}

parse = function(predicate, parsers = makeParsers(state = state), state)
{
    state$result <- list()
    for( i in seq(1, length(parsers)))
    {
        #find out what the js replace method on arrays does
        #predicate = predicate.replace(parsers[[i]], parsers[[i+1]])
        expr = names(parsers)[i]
        tmpmatch = gregexpr(expr, predicate, perl=TRUE)[[1]]
        if(any(tmpmatch > 0))
        {
            matches = regmatches(tmpmatch, predicate)
            res = sapply(matches, parsers[[i]])
            regmatches(matches, predicate) <- res
        }
    }
        
    state$lastSteps <- c(state$lastSteps, predicate)

   # return(state$result)
    predicate
}


regroup = function(tokens)
{
    tokens = unlist(tokens)
    for(key in names(operators))
    {
        operator = operators[[key]]
        for(i in seq(1, length(tokens), by=2))
        {
            if(tokens[[i]] == "operator" && tokens[[i+1]]  == key)
            {
                if(operator$operand == 1){
                    tokens[[i]] = operator$name
                    tmp = tokens[i+2:3]
                    tokens[[i+1]] = tmp
                    tokens = tokens[-seq(i+2:3)]
                } else if (operator.operand == 2) {
                    #slice i-2, i and i+2, i+4
                    operands = unlist(c(tokens[i-2:0], tokens[i+2:4]))
                    tokens[[i]] = operator$name
                    tokens[[i+1]] = operands
                                        #splice i+2, 2 and i-2,2
                    tokens = tokens[-seq(i+2:3)]
                    tokens = tokens[-seq(i-2:1)]
                }
            }
        }
        
    }
    tokens
}

operators <- list(
    "!" = list(name = "not", operand = 1),
    "==" = list(name = "eq", operand =2),
    "!=" = list(name = "noteq", operand = 2),
    "&&" = list(name = "and", operand = 2),
    "||" = list(name = "or", operand = 2)
 )

flatten = unlist

compact  = function(lst)
{
    lst[!sapply(lst, is.null)]
}

rpath_split = function(path, parsers = makeParsers(state = state), state)
{
    
    
    step = tokens =  NULL

    #<string>.split
    steps = strsplit(path, split = split_regex, perl=TRUE)[[1]]

    if(steps[1] == "" || steps[1] == "/") {

        steps = steps[-1]
    }

    for(st in steps)
    {
        tmpmatchRes = gregexpr(predicate_regex, st)[[1]]
        matchRes = regmatches(st, tmpmatchRes)
        
        if(length(matchRes)) {
         #   predicate = matchRes[[2]]
            predicate = matchRes[[1]]
            mIndex = regmatches(predicate, gregexpr(index_regex, predicate))[[1]]
            if(length(mIndex))
                tokens = list("index", mIndex[[1]])
            else {
                tokens = parse(predicate, parsers = parsers, state = state)
                tokens = flatten(compact(tokens))
                tokens = regroup(tokens)
            }

            if(length(matchRes) > 1 && nchar(matchRes[[2]])) {
                state$result <- c(state$result, "node", matchRes[[1]])
            }
            state$result <- c(state$result, "predicate", tokens)
        } else {
            state$result <- c(state$result, "node", st)
        }
    }
    
    state$result
}



rpath = function(robj, path, state = new.env())
{
    state$lastSteps = list()
    state$result = list()
    if(is.null(robj) || !is(path, "character") || !nchar(path))
        return(list())

    if(identical(path, "."))
        return(robj)

    #steps = unlist(rpath_split(path, state =  state))
    steps = rpath_split(path, state =  state)
         
    res = rpath_exec(robj, steps[1:2], executors = executors, state = state)

    i = 3
    while(!is.null(res) && i < length(steps))
    {
        res = rpath_exec(res, steps[i + 0:1], executors = executors, state = state)
        i = i+2
    }
    if(is.null(res))
        list()
 #   else if (!is.list(res))
 #       list(res)
    else
        res
}
