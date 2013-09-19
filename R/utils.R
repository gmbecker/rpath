

#wrapper that accepts a rapth_match object and calls the user-supplied terminal condition function on its value
checkTermCondition = function(x, term_fun)
{
    if(is_terminal_node(x) || no_match(x))
        return(TRUE)

    term_fun(x@value)
}


regroup = function(tokens)
{
  #  tokens = unlist(tokens)
    for(key in names(operators))
    {
        operator = operators[[key]]
        if(FALSE)
        {
            
            for(i in seq(1, length(tokens)))
            {
                if(tokens[[i]][[1]] == "operator" && tokens[[i]][[2]]  == key)
                {
                    if(operator$operand == 1){
                    
                        tokens[[i]][[1]] = operator$name
                        tmp = tokens[[i+2]]
                        tokens[[i+1]] = tmp
                        tokens = tokens[-(i+2)]
                    } else if (operator$operand == 2) {
                                        #slice i-2, i and i+2, i+4
                                        # operands = unlist(c(tokens[i-2:0], tokens[i+2:4]))
                        operands = list(tokens[[i+1]], tokens[[i+2]])
                        tokens[[i]][[1]] = operator$name
                        tokens[[i]][[2]] = operands
                                        #splice i+2, 2 and i-2,2
                        tokens = tokens[-seq(i+2:5)]
                                        #                    tokens = tokens[-seq(i-2:1)]
                    }
                }
            }
        }
        i = 1
        while(i < length(tokens))
#        for(i in seq(1, length(tokens)))
        {
            if(tokens[[i]][[1]] == "operator" && tokens[[i]][[2]]  == key)
            {
                if(operator$operand == 1){
                    tokens[[i]][[1]] = operator$name
                    tokens[[i]][[2]] = tokens[[i+1]]
                    tokens = tokens[-(i+1)]
                } else if (operator$operand == 2) {
                                        #slice i-2, i and i+2, i+4
                                        # operands = unlist(c(tokens[i-2:0], tokens[i+2:4]))
                         operands = list(tokens[[i+1]], tokens[[i+2]])
                        tokens[[i]][[1]] = operator$name
                        tokens[[i]][[2]] = operands
                                        #splice i+2, 2 and i-2,2
                        tokens = tokens[-(i+1:2)]
                                        #                    tokens = tokens[-seq(i-2:1)]
                }
            }
            i = i + 1
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
    res = lst[!sapply(lst, no_match)]
}

combineMatchLists = function(..., lst, trim = TRUE)
{
    if(missing(lst))
        lst = list(...)

    ret = list()
    for(tmp in lst)
    {
        if(!no_match(tmp))
        {
            if(is(tmp, "rpath_matchList"))
                ret = c(ret, tmp@matches)
            else if (is(tmp, "rpath_match"))
                ret = c(ret, tmp)
        }
    }
    
    res = new("rpath_matchList", matches = ret)
    if(trim)
        res = trimMatchList(res)
    if(length(res) == 1)
        res = res[[1]]
    res

}

trimMatchList = function(mlist)
{
    drop = sapply(mlist, no_match)
    mlist[!drop]
}
