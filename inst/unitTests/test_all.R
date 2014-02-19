
test_basic = function()
{
    lst = list(first = TRUE, second = FALSE, third = list(fourth = 5, fifth = list("hi", "lo")), sixth = "SO FUNNY!!!")
    resthird = rpath(lst, "/third")
    checkTrue(identical(resthird, list(list(fourth=5, fifth=list("hi", "lo")))), "Checking single level path /third")
    resfourth = rpath(lst, "/third/fourth")
    checkTrue(identical(resfourth, list(5)), "Checking two-level path /third/fourth")
}

test_star = function()
{
      lst = list(first = TRUE, second = FALSE, third = list(fourth = 5, fifth = list("hi", "lo")), sixth = "SO FUNNY!!!")
      res1 = rpath(lst, "/*")
      checkTrue(identical(res1, list(TRUE, FALSE, list(fourth=5, fifth=list("hi", "lo")), "SO FUNNY!!!")), "Testing path with wildcard after root /*")
      res2 = rpath(lst, "/third/*")
      checkTrue(identical(res2, list(5, list("hi", "lo"))), "Testing path ending with wildcard /third/*")
      res3 = rpath(lst, "/third/*/*")
      checkTrue(identical(res3, list("hi", "lo")), "Testing path with two wildcards /third/*/*")
  }

test_bracket = function()
{
      lst = list(first = TRUE, second = FALSE, third = list(fourth = 5, fifth = list("hi", "lo")), sixth = "SO FUNNY!!!")
      res1 = rpath(lst, "/third[fourth]")
      checkTrue(identical(res1, list(list(fourth=5, fifth=list("hi", "lo")))), "Testing path with presence condition /third[fourth]")
      res2 = rpath(lst, "/*[fourth]")
      checkTrue(identical(res2, list(list(fourth=5, fifth=list("hi", "lo")))), "Testing path with presence condition on wildcard /*[fourth]")
  }

test_doubleslash = function()
{
      lst = list(first = TRUE, second = FALSE, third = list(fourth = 5, fifth = list("hi", seventh="lo")), sixth = "SO FUNNY!!!")
      res1 = rpath(lst, "//fourth")
      checkTrue(identical(res1, list(5)), "Testing path with any-descendent criterion from root //fourth")
      res2 = rpath(lst, "/third//seventh")
      checkTrue(identical(res2, list("lo")), "Testing path with any-descendent criterion not from root /third//seventh")
      lst2 = list(stuff = lst)
      res3 = rpath(lst2, "//seventh")
      checkTrue(identical(res3, list("lo")))
      res4 = rpath(lst2, "//stuff")
      checkTrue(identical(res4, list(lst)), "Testing if a path beginning with // can find direct children of root")
  }

test_dupnames = function()
{
    lst = rep(list(first = TRUE, second = FALSE, third = list(fourth = 5, fifth = list("hi", seventh="lo")), sixth = "SO FUNNY!!!"), times = 2)
    res1 = rpath(lst, "/third")
    checkTrue(identical(res1, rep(list(fourth=5, fifth=list("hi", seventh = "lo")), times =2)), "Testing basic one-step path in presence of duplicate names /third")
    res2 = rpath(lst, "/third/fourth")
    checkTrue(identical(res2, list(5, 5)), "Testing basic two-step path in presence of duplicate names /third/fourth")
    res3 = rpath(lst, "/third//seventh")
    checkTrue(identical(res3, list("lo", "lo")), "Testing path with intermediate // in presence of duplicate names /third//seventh")
    res4 = rpath(lst, "/third/*/seventh")
    checkTrue(identical(res4, list("lo", "lo")), "Testing path with intermediate wildcard in presence of dupilicate names /third/*/seventh")
    res5 =rpath(lst, "/third/*[seventh]")
    checkTrue(identical(res5, list(list("hi", seventh="lo"), list("hi", seventh="lo"))), "Testing path with wildcard and presence condition in presence of duplicate names /third/*[seventh]")
}

test_logicalconds = function()
{
    lst3 = list(first = TRUE, second = FALSE, third = list(fourth = 5, fifth = "hi"), sixth = "SO FUNNY!!!", third = list(fourth = 6, fifth = "yay!"), third = list(fourth=3, fifth="not6"))
    res1 = rpath(lst3, "/third[fourth=='6']")
    checkTrue(identical(res1, list(list(fourth = 6, fifth="yay!"))), "Testing path with equality condition /third[fourth=='6']")
    res2 = rpath(lst3, "/third[fourth!='6']")
    checkTrue(identical(res2, list(list(fourth=5, fifth="hi"), list(fourth=3, fifth="not6"))), "Testing path with non-equality condition /third[fourth!='6']")    
    res3 = rpath(lst3, "/third[fourth<='5']") 
    checkTrue(identical(res3, list(list(fourth=5, fifth="hi"), list(fourth=3, fifth="not6"))), "Testing path with less-than-or-equal condition /third[fourth<='5']")    
}

test_attrfun = function()
{
    lst3 = list(first = TRUE, second = FALSE, third = list(fourth = 5, fifth = "hi"), sixth = "SO FUNNY!!!", third = list(fourth = 6, fifth = "yay!"), third = list(fourth=3, fifth="not6"))
    attrfun = function(obj) c(sapply(names(obj), function(x) obj[[x]], simplify=FALSE), class = class(obj))
    
    res1 = rpath(lst3, "/third/@class", attr_fun = attrfun)
    checkTrue(identical(res1, list("list", "list", "list")), "Testing path with attribute selection with custom attr_fun /third/@class")
    res2 = rpath(lst3, "/third/@fourth", attr_fun = attrfun)
    checkTrue(identical(res2, list(5, 6, 3)), "Testing path with attribute selection with custom attr_fun(2) /third/@fourth")
    res3 = rpath(lst3, "/third/*[@class=='character']", attr_fun = attrfun)
    checkTrue(identical(res3, list("hi", "yay!", "not6")), "Testing path with attribute in logical condition using custom attr_fun /third/*[@class=='character']")
    res4= rpath(lst3, "/third[@fourth=='6']", attr_fun = attrfun)
    checkTrue(identical(res4, list(list(fourth=6, fifth="yay!"))), "Testing path with attribute in logical condition using custom attr_fun(2) /third[@fourth=='6']")
}

test_notpresent = function()
{
    lst3 = list(first = TRUE, second = FALSE, third = list(fourth = 5, fifth = "hi"), sixth = "SO FUNNY!!!", third = list(fourth = 6, fifth = "yay!"), third = list( fifth="not6"))
    res1 = rpath(lst3, "/third[!fourth]")
    checkTrue(identical(res1, list(list(fifth="not6"))), "Testing path with negative presence condition /third[!fourth]")
    res2 = rpath(lst3, "/third[!zzz]")
    checkTrue(identical(rpath(lst3, "/third"), res2), "Testing path with negative presence condition (2) /third[!zzz]")
    res3 = rpath(lst3, "/third[zzz]")
    checkTrue(length(res3) == 0, "Testing path with presence condition that is never met /third[zzz]")
}

test_compound_or= function()
{
    lst3 = list(first = TRUE, second = FALSE, third = list(fourth = 5, fifth = "hi"), sixth = "SO FUNNY!!!", third = list(fourth = 6, fifth = "yay!"), third = list(fourth=3, fifth="not6"))
    res1 = rpath(lst3, "/third[zzz|fourth=='6']")
    checkTrue(identical(res1, rpath(lst3, "/third[fourth=='6']")), "Testing path with compound-or of presence and logical conditions /third[zzz|fourth=='6']")
    res2 = rpath(lst3, "/third[fourth=='6'|fifth=='hi']")
    checkTrue(identical(res2, list(list(fourth=5, fifth="hi"), list(fourth = 6, fifth="yay!"))), "Testing path with compound-or of two logical conditions /third[fourth=='6'|fifth=='hi']")



test_compound_and= function()
{
    lst3 = list(first = TRUE, second = FALSE, third = list(fourth = 5, fifth = "hi"), sixth = "SO FUNNY!!!", third = list(fourth = 6, fifth = "yay!"), third = list(fourth=3, fifth="not6"))
    res1 = rpath(lst3, "/third[zzz&&fourth=='6']")
    checkTrue(length(res1) == 0, "Testing compound-and condition where one side is met and the other is not /third[zzz&&fourth=='6']")
    res2 = rpath(lst3, "/third[fourth=='6'&&zzz]")
    checkTrue(length(res2) == 0, "Testing compound-and condition where one side is met and the other is not /third[fourth=='6'&&zzz]")
     res3 = rpath(lst3, "/third[fourth<='6'&&fifth=='hi']")
    checkTrue(identical(res2, list(list(fourth=5, fifth="hi"))), "Testing path with compound-or of two logical conditions /third[fourth=='6'|fifth=='hi']")
}
