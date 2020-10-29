################################################################
# evaluator.iol
#
# this file contains the different objects that the "evaluator" uses.
# These are:
#   atom         word
#   variable     .word
#   predicate    word'
#   boolean      True | False
#   expression   (...)
#   list         <...>
#   set          {...}
#
# every object has at least these methods:
#   Eval,
#   Image,
#   Equal,
#   Type, and
#   element.
# Eval evaluates an object and return the result.
# Image returns a string of what the object looks like.
# Equal assumes that it's parameter is the same type and checks
# to make sure it is equal to itself.  Equal does NOT evaluate its
# argument!  There is an expression function "_equal" which does.
# Type returns a string with the type of the object.
# element generates all the atoms within the object.
#
# every type has an associated "new_TYPE" procedure which creates
# a new object of that type.  In some cases it checks to make
# sure it isn't making a look-alike.
#

global err_flag                                                 # err_flag is used to halt evaluation when an error occurs.  when
                                                                # the value is &null then it is not set.  if its value is non-null
                                                                # then it is set.  

procedure evaluate( x )                                         # procedure evaluate() is the interface procedure to the outside.
                                                                # if it can convert the parameter to some evaluator object then it
                                                                # resets the error flag and evaluates the object.  When the
                                                                # evaluation is done it checks the error flag to make sure there
                                                                # were no errors.  if so then the value returned by the evaluation
                                                                # is shown.
  if t := convert(x) then                                       # if we can convert the parameter to a evaluatable object
  {
    err_flag := &null                                           # reset the error flag.
    r := t$Eval()                                               # evaluate the object.
    if /err_flag then                                           # if the error flag is not set 
      fout$Write( r$Image() )                                   # then display the result.
    return
  }                                                             
  fail                                                          # if the input can't be converted then the procedure fails.
end

procedure convert( x )                                          # convert() takes a string or a list of strings and makes an object
                                                                # out of it.  This is basically the top level of a hand-written
                                                                # recursive descent parser.
  if type(x) == "string" then                                   # if it's a string
  {
    x := toke_list(x, '(){}<>')                                 # make it into a list of strings.  brackets are punctuation marks.
    if (*x > 1) & (not balanced(x))                             # check to make sure all the brackets are balanced.
      then fail                                                 # if they aint then fail.
  }
  case x[1] of
  {                                                             # first check for compound types:
    "(": return new_Expression(x)                               # if the first thing is a "(" then make an expression;
    "{": return new_Set(x)                                      # if "{" then make a set; or
    "<": return new_List(x)                                     # if "<" then make a list.
    default:                                                    # otherwise figure out what kind of simple object it is:
    {
      x := x[1]
      if x == Nil$Image() then return Nil                       # if it looks like nil it is;
      if x == True$Image() then return True                     # if it looks like true it is;
      if x == False$Image() then return False                   # if it looks like false it is;
      if x[1] == "." then return new_Variable(x)                # if it starts with a period it's a variable;
      if x[-1] == "'" then return new_Predicate(x)              # if it ends with prime it's a predicate;
      return new_Atom(x)                                        # otherwise it's an atom.
    }
  }
end

procedure next_Var( mark )                                      # procedure next_Var() is called by the translator to get variables
  static letter, number, alphabet                               # for quantifiers.  it starts at ".a" and then goes to ".z", then
                                                                # ".a1" to ".z1", etc.  If somebody wants a special looking variable
                                                                # they can call this procedure with a mark.  It will go between the
                                                                # period and the letter.
  initial {                                                     # this gets done the first time:
    letter := "z"                                               # set the letter to "z" so that we start with "a".
    number := -1                                                # set the number to -1 so that we start with 0.
    alphabet := string(&lcase)
  }
  /mark := ""
  if letter == "z"                                              # if we're at the end of the alphabet
    then letter := "a" & number +:= 1                           # then go back to "a" and use the next number,
    else letter := alphabet[ find(letter, alphabet) + 1 ]       # otherwise get the next letter
  if number > 0                                                 # if the number is larger than 0
    then return "." || mark || letter || number                 # then use it in the variable name
    else return "." || mark || letter                           # otherwise don't
end

procedure Equal( x, y )                                         # procedure Equal() checks to see that two objects are the same.
  if x$Type() ~== y$Type() then fail                            # if they aren't the same type then fail.
  return x$Equal(y)                                             # otherwise pass the job on to one of them.
end


################################################
# class Atom
#
# the string that the atom is is kept in the "a" field.
#
class Atom( a )
  method Eval()
    if \err_flag then return Nil
    return self                                                 # the value of an atom is itself.
  end

  method Image()
    return self.a
  end

  method Type()
    return "Atom"
  end

  method element()
    return self
  end

  method Equal(y)
    if self.a == y$Image()                                      # if they have the same image then 
      then return                                               # they're equal,
      else fail                                                 # otherwise not.
  end
end



procedure new_Atom(x)                                           # new_Atom keeps a table of all the atoms created.  This way
  static Atom_t                                                 # we don't get multiple atoms that look the same. 
  initial Atom_t := table()                                     # initialize the table the first time.
  /Atom_t[x] := Atom(x)                                         # if the table doesn't have one, put a new one in.
  return Atom_t[x]                                              # return the one in the table.
end


################################################
# class Predicate
#
# the difference between a predicate and an atom is that the value of
# a predicate is a set kept in the current model.
#
class Predicate( a )
  method Eval()
    local r
    if \err_flag then return Nil;   r := cur_m$Eval( self$Image() )   # get the associated object from the current model,
    if \err_flag then return Nil;   r := r$Eval()                     # evaluate it,
    if \err_flag then return Nil;   return r                          # and return it.
  end

  method Image()
    return self.a
  end

  method Type()
    return "Predicate"
  end

  method element()
    fail                                                        # if a predicate is in the model, so are all of its atoms.
  end

  method Equal(y)
    if self.a == y$Image()                                      # again, if it looks the same it is.
      then return
      else fail
  end
end

procedure new_Predicate(x)
  static Predicate_t
  initial Predicate_t := table()                                # initialize the table the first time.
  /Predicate_t[x] := Predicate(x)                               # create a new predicate if necessary, and
  return Predicate_t[x]                                         # return the predicate by that name.
end

################################################
# class Boolean
#
# "tof" hold the string of the boolean: "true", "false", or "nil".
#
class Boolean( tof )
  method Eval()
    if \err_flag then return Nil
    return self
  end

  method Image()
    return self.tof
  end

  method Type()
    return "Boolean"
  end

  method element()
    fail                                                        # no atoms in a Boolean!
  end

  method Equal( x )
    if x$Image() == self.tof
      then return
      else fail
  end
end

# And nobody should be asking for new booleans.

################################################
# class Variable
#
# v is the form of the variable, vals is the value stack.
#
class Variable( v, vals )                                       
  method Eval()
    if \err_flag then return Nil
    if *self.vals = 0                                           # if the variable has no value
      then {
        err_flag := 1                                           # then set the error flag,
        fout$Write("unbound variable: " || self$Image())        # display an error message,
        return Nil                                              # and return nil.
      }
      else return (self.vals[1])$Eval()                         # otherwise return the value of the thing on the top of the stack.
  end

  method Image()
    return self.v                                               # return the form of the variable.
  end

  method Type()
    return "Variable"
  end

  method Set( x )
    push( self.vals, x )                                        # put a new object on the stack,
    return x                                                    # and return it.
  end

  method Reset()
    if *self.vals = 0                                           # if there is no value on the stack
      then return Nil                                           # then return nil,
      else return pop( self.vals )                              # otherwise pop a value from the stack and return it.
  end

  method Equal(x)
    if self.v == x$Image()
      then return
      else fail
  end

  method element()
    fail
  end

initially
  self.vals := []                                               # initialize the stack.
end

procedure new_Variable(x)
  static var_table
  initial var_table := table()                                  # the first time this is called set up the table.
  /var_table[x] := Variable(x)                                  # if the table hasn't got one put in a new one.
  return var_table[x]                                           # return the variable.
end

################################################
# class Expression
#
# e holds the list of items in the expression.
# var holds the variable part of a quantified expression.
# restriction holds the restriction part.
# scope holds the scope part.
# log1 and log2 hold the value of arguments to logical functions.
#
class Expression( e, var, restriction, scope, log1, log2 )      
  method Type()
    return "Expression"
  end

  method Eval()
    if \err_flag then return Nil
    if (self.e[1])$Type() == "Atom" then                        # if the first thing in the expression is an atom
      case (self.e[1])$Image() of                               # check to see if it is a predefined function.
      {
        "set":   if (self.e[2])$Type() == "Variable"            # set a variable if the type of the first argument is "Variable"
                   then return (self.e[2])$Set( self.e[3] )      
        "reset": if (self.e[2])$Type() == "Variable"            # reset a "Variable"
                   then return (self.e[2])$Reset()

        "A":           return self$_for_all()                   # universal 
        "E":           return self$_there_exists()              # existential 
        "EU":          return self$_there_exists_unique()       # unique existential
        "M":           return self$_most()                      # "most" quantifier

        "&"|"and"|"^": return self$_and()                       # conjunction
        "v"|"or":      return self$_or()                        # discjunction
        "xv"|"xor":    return self$_exclusive_or()              # exclusive discjunction
        "impl":        return self$_implies()                   # implication
        "~"|"!"|"not": return self$_not()                       # negation
        "bimpl":       return self$_bi_implies()                # bi-implication

        "equal"|"=":   return self$_equal()                     # equality
      }                                                         # otherwise
    return self$Predicate()                                     # assume it's a predicate.
  end

  method Image()
    local s
    if *self.e = 0 then return "()"                             # if it's an empty expression show it.
    s := "("                                                    # start with a left paren,
    every s ||:= (!self.e)$Image() || " "                       # get the images of all the things in the expression,
    return s[1:-1] || ")"                                       # put on a right paren and return it.
  end

  method element()
    suspend (!self.e)$element()                                 # generate all the atoms in the expression.
  end

  method fetch_e()
    return self.e                                               # return the expression for the Equal method.
  end

  method Equal(x)
    local other_e
    other_e := x$fetch_e()                                      # get the list of things in the other expression.
    if *other_e ~= *self.e then fail                            # if they're not the same length then they're not equal.
    every i := 1 to *self.e do                                  # check all the items in the expressions
      if not (Equal( self.e[i], other_e[i] ))                   # to see if they're equal.
        then fail                                               # if one aint, then fail.
    return
  end

################################################
# quantifiers
#
  method quantify()                                             # quantify() breaks apart the separate elements of a 
                                                                # quantifier expression, and checks all the types.
    (self.var := \(self.e[2]) &                                 # the variable is the second thing in the expression
     (self.var)$Type() == "Variable" &                          # and must be "Variable" type.
     self.restriction := \(self.e[3]) &                         # the restriction is the third thing in the expression.
     self.scope := \(self.e[4])                                 # the scope is the fourth, and last thing in the expression.
    ) | (msg$Write( "Expression error: not of form (quant var " ||  # error message if something didn't work out.
                    "restriction scope): " || self$Image()) & 
         err_flag := 1 & fail)                                  # set the error flag and fail
    return                                                      # if every thing is alright, return.
  end

  method _for_all()                                             # UNIVERSAL
    local x, r_flag, nil_count, flag
    self$quantify() | fail                                      # set up the quantifier stuff or fail.
    flag := True                                                # set the flag to true.
    nil_count := 0                                              # initialize a counter for unknowns.
    every val := cur_m$domain() do                              # iterate with all the atoms in the current model.
    {
      if \err_flag then return Nil
      (self.var)$Set( val )                                     # set the variable.

      r_flag := (self.restriction)$Eval()                       # evaluate the restriction.
      if Equal(r_flag, Nil)                                     # if it's nil
        then nil_count +:= 1                                    # then increment the counter.
      if Equal(r_flag, True ) then                              # (otherwise) if it's true
      {
        flag := (self.scope)$Eval()                             # then evaluate the scope.
        if Equal( flag, Nil )                                   # if that's nil
          then nil_count +:= 1                                  # then increment the counter.
      }
      (self.var)$Reset()                                        # reset the variable.
      if Equal( flag, False )                                   # if we found a true restriction and false scope
        then return False                                       # then it's false
    }                                                           # after iterating with all the atoms
    if nil_count > 0                                            # check to see if we found any nils.
      then return Nil                                           # if so return nil,
    return True                                                 # otherwise return true. (found no contrary evidence.)
  end

  method _there_exists()                                        # EXISTENTIAL
    local nil_count, flag, r_flag
    self$quantify() | fail                                      # do the quantifier setup stuff or fail.
    flag := False                                               # set the flag to false.
    nil_count := 0                                              # initialize the nil counter.
    every val := cur_m$domain() do                              # iterate with all the atoms in the model.
    {
      if \err_flag then return Nil
      (self.var)$Set( val )                                     # set the variable to the atom.
      r_flag := (self.restriction)$Eval()                       # evaluate the restriction.
      if Equal(r_flag, Nil)                                     # if it is nil
        then nil_count +:= 1                                    # then increment the nil counter.
      if Equal(r_flag, True ) then                              # (otherwise) if it is true
      {
        flag := (self.scope)$Eval()                             # then evaluate the scope,
        if Equal(flag, Nil)                                     # and if that is nil
          then nil_count +:= 1                                  # increment the nil counter.
      }
      (self.var)$Reset()                                        # reset the variable.
      if Equal( flag, True ) then return True                   # if the restriction and scope were true the return true.
    }                                                           # after the atomic iteration
    if nil_count > 0                                            # check the nil counter.  if we found a nil item
      then return Nil                                           # then return nil,
    return False                                                # otherwise return false.
  end

  method _there_exists_unique()                                 # UNIQUE EXISTENTIAL
    local flag, counter_t, counter_n, r_flag
    self$quantify() | fail                                      # do the quantifier setup or fail.
    counter_t := counter_n := 0                                 # initialize the true and nil counters.
    every val := cur_m$domain() do                              # iterate with the atoms in the model.
    {
      if \err_flag then return Nil
      (self.var)$Set(val)                                       # set the variable.
      r_flag := (self.restriction)$Eval()                       # evaluate the restriction.
      if Equal(r_flag,Nil)                                      # if the restriction is nil
        then counter_n +:= 1                                    # then increment the nil counter.
      if Equal(r_flag, True ) then                              # if the restriction is true
      {
        flag := (self.scope)$Eval()                             # evaluate the scope.
        if Equal(flag, True) then counter_t +:= 1               # if the scope is true increment the true counter.
        if Equal(flag, Nil) then counter_n +:= 1                # if the scope is nil increment the nil counter.
      }
      (self.var)$Reset()                                        # reset the variable
      if counter_t > 1 then return False                        # if we've found more than one existence return false.
    }                                                           # after done iterating:
    if counter_n > 0                                            # if we found some unknown ones
      then return Nil                                           # then return nil.
    if counter_t = 1                                            # if we found exactly one true restriction and true scope
      then return True                                          # then return true.
    return False                                                # otherwise return false.
  end

  method _most()                                                # MOST
    local flag, r_flag, true_count, false_count, nil_count
    self$quantify() | fail                                      # do the quantifier setup or fail.
    true_count := false_count := nil_count := 0                 # initialize all the counters.
    every val := cur_m$domain() do                              # iterate with all the atoms in the model.
    {
      if \err_flag then return Nil
      (self.var)$Set(val)                                       # set the variable.
      r_flag := (self.restriction)$Eval()                       # evaluate the restriction.
      if Equal(r_flag, Nil)                                     # if the restriction is nil
        then nil_count +:= 1                                    # increment the nil counter.
      if Equal(r_flag, True ) then                              # if the restriction is true
      {
        flag := (self.scope)$Eval()                             # evaluate the scope.
        if Equal(flag, True)                                    # if the scope is true
          then true_count +:= 1                                 # increment the true counter.
        if Equal(flag, False)                                   # if the scope is false
          then false_count +:= 1                                # increment the false counter.
        if Equal(flag, Nil)                                     # it it's nil
          then nil_count +:= 1                                  # increment the nil counter.
      }
      (self.var)$Reset()                                        # reset the variable.
    }                                                           # after the atomic iteration:
    if true_count + false_count + nil_count = 0                 # if the restriction was never true
      then return True                                          # then return true.
    if true_count > false_count + nil_count                     # if we found more true than false and nil
      then return True                                          # return true.
    if false_count >= true_count + nil_count                    # if we found more false than (true and nil) - 1
      then return False                                         # then return false.
    return Nil                                                  # otherwise return nil.
  end

################################################
# logical functions
#
  method _not()                                                               # NOT
    local x
    if *self.e ~= 2 then                                                      # check that we have one argument.
    {
      msg$Write( "function takes one argument: " || self$Image() )            # if we don't give an error,
      err_flag := 1                                                           # set the error flag, 
      return Nil                                                              # and bomb out.
    }
    x := (self.e[2])$Eval()                                                   # evaluate the argument.
    if x$Type() ~== "Boolean" then                                            # if it's not boolean
    {
      msg$Write( "value of argument must be boolean: " || self$Image() )      # give an error.
      err_flag := 1                                                           # set the error flag.
      return Nil
    }
    if Equal( x, Nil )   then return Nil                                      # (not nil) is nil.
    if Equal( x, False ) then return True                                     # (not false) is true.
                              return False                                    # (not true) is false.
  end

  method pre_logical_function()                                               # pre_logical_function does the evaluating and 
                                                                              # type checks for binary logical functions.
    if *self.e ~= 3 then                                                      # check that we have two arguements
    {
      msg$Write( "function takes two arguments: " || self$Image() )           # or give an error
      err_flag := 1                                                           # and set the error flag.
      fail
    }
    self.log1 := (self.e[2])$Eval()                                           # evaluate the first argument,
    self.log2 := (self.e[3])$Eval()                                           # and the second.
    if self.log1$Type() ~== "Boolean" | self.log2$Type() ~== "Boolean" then   # if either isn't boolean
    {
      msg$Write( "value of arguments to must be boolean: " || self$Image() )  # write and error message,
      err_flag := 1                                                           # and set the error flag.
      fail
    }
    return                                                                    # finished ok.  
  end

  method _and()                                                               # AND
    if not self$pre_logical_function()                   then return Nil      # setup or bomb.
    if Equal(self.log1, False) | Equal(self.log2, False) then return False    # if either is false then return false,
    if Equal(self.log1, Nil) | Equal(self.log2, Nil)     then return Nil      # (otherwise) if either is nil then return nil,
                                                              return True     # (otherwise) return true.
  end

  method _or()                                                                # OR
    if not self$pre_logical_function()                   then return Nil      # setup or bomb.
    if Equal(self.log1, True) | Equal(self.log2, True)   then return True     # if either is true then return true,
    if Equal(self.log1, Nil) | Equal(self.log2, Nil)     then return Nil      # (otherwise) if either is nil then return nil,
                                                              return False    # (otherwise) return false.
  end

  method _exclusive_or()                                                      # XOR
    if not self$pre_logical_function()                   then return Nil      # setup or bomb.
    if Equal(self.log1, Nil) | Equal(self.log2, Nil)     then return Nil      # if either is nil then return nil,
    if Equal(self.log1, self.log2)                       then return False    # (otherwise) if they're equal then return false,
                                                              return True     # (otherwise) return true.
  end

  method _implies()                                                           # IMPL
    if not self$pre_logical_function()                   then return Nil      # setup or bomb.
    if Equal(self.log1, False)                           then return True     # if the first argument is false then return true.
    if Equal(self.log1, Nil)                             then return Nil      # if the first argument is nil then return nil.
                                                              return log2     # otherwise return the second argument.
  end

  method _bi_implies()                                                        # BIMPL
    if not self$pre_logical_function()                   then return Nil      # setup or bomb.
    if Equal( self.log1, Nil ) | Equal( self.log2, Nil ) then return Nil      # if either argument is nil then return nil.
    if Equal( self.log1, self.log2 )                     then return True     # if they're equal then return true.
                                                              return False    # otherwise return false.
  end

  method _equal()                                                             # EQUAL
    if *self.e ~= 3 then                                                      # if we don't have exactly two arguments
    {
      msg$Write( "function \"equal\" takes two arguments: " || self$Image() ) # give and error message,
      err_flag := 1                                                           # set the error flag,
      return Nil                                                              # and bomb out.
    }
    if Equal( (self.e[2])$Eval(), (self.e[3])$Eval() )                        # if the values of the arguments are equal
      then return True                                                        # then return true
      else return False                                                       # otherwise return false.
  end

################################################
# predicates
#
  method Predicate()
    local memb, arg, arg2, func, i, v, new_x
    if *self.e > 1 then                                         # check that we have at least one argument.
    {
      every i := 2 to *self.e do                                # iterate over the arguments
      {
        if \err_flag then return Nil
        if Equal( IndInd, self.e[i] ) then                      # looking for an individual indeterminate.
        {                                                       # if we find one
          v := new_Variable( next_Var("?") )                    # get a new variable
          new_l := self.e[1:i] ||| [v] ||| self.e[i+1:0]        # and build a ...
          new_x := new_Expression( new_l, 1 )                   # new expression;
          self.e := [new_Atom("E"), v, True, new_x]             # and make this expression an existential,
          return self$Eval()                                    # and evaluate it.
        }
      }                                                         # if there are no individual indeterminates
      func := (self.e[1])$Eval()                                # evaluate the function (predicate).
      if func$Type() ~== "Set"                                  # if the result is not a "Set"
        then {
          err_flag := 1                                         # then set the error flag,
          fout$Write("not a function: " || func$Image())        # write an error message,
          return Nil                                            # and bomb out.
        }                                                       # if everything is ok
      arg := (new_List( self.e[2:0], 1 ))$Eval()                # make a "List" from the arguments and evaluate it.
      every memb := func$Bang() do                              # search through the "Set"
      {
        if \err_flag then return Nil
        if (memb$Type() ~== "List") |                           # looking for a "List" 
           (memb$Splat() ~= arg$Splat() + 1) then next          # that is the one longer than the one we made from the arguments.
        arg2 := (memb$aCdr())$Eval()                            # if the value of the anti-cdr of the list from the set
        if Equal( arg, arg2 )                                   # is equal to the list we made from the arguments
          then return (memb$aCons())$Eval()                     # then return the anti-cons of the list from the set.
      }                                                         # if we can't find a mapping
      return Nil                                                # return nil
    }
  end
end

procedure new_Expression( x, immediate )
  local e, cnt, right_cnt
  if \immediate                                                 # if we don't need to conver the pieces in the list
    then return Expression( x )                                 # just make a new expression
  e := []                                                       # start a new list
  cnt := 2                                                      
  while cnt < *x do                                             # while there are more things in the list
  {
    right_cnt := lbal(x[cnt:0]) + cnt                           # find the next bracketed thing,
    put(e, convert(x[cnt:right_cnt]))                           # convert it, and put it in the list;
    cnt := right_cnt                                            # move on to the next thing.
  }
  return Expression( e )                                        # make a new expression out of the new list.
end

################################################
# class List
#
# the "l" field hold the list of things in the List.
#
class List( l )
  method Type()
    return "List"
  end

  method Eval()
    local newl
    if \err_flag then return Nil
    newl := []                                                  # make a new list
    every put( newl, (!(self.l))$Eval() )                       # evaluate everything in the List and put it in the list.
    return new_List(newl, 1)                                    # return the new List.
  end

  method Splat()
    return *self.l                                              # return how many things are in the list.
  end

  method aCdr()                                                 # anti-cdr. (everything but the last in the list)
    return new_List( self.l[1:-1], 1 )                          # return a List of everything but the last.
  end

  method aCons()                                                # anti-cons. (the last thing in the list)
    return self.l[-1]                                           # return the last thing in the list.
  end

  method Image()                                                # to see what a list looks like
    local s
    if *self.l = 0 then return "<>"                             # if there is nothing in the list just do "<>".
    s := "<"                                                    # otherwise: start with a "<",
    every s ||:= (!(self.l))$Image() || " "                     # get the image of everything in the list,
    return s[1:-1]||">"                                         # and finally put on a ">".
  end

  method element()
    suspend (!self.l)$element()                                 # return all the elements in all the things in the list.
  end

  method fetch_l()
    return self.l                                               # return the list of the List.
  end

  method Equal( x )                                             # to see if two lists are equal
    local other_l
    other_l := x$fetch_l()                                      # get the list of the other List,
    if *self.l ~= *other_l                                      # check the length of both,
      then fail
    every i := 1 to *self.l do                                  # and then compare one to one down the line.
      if not Equal( self.l[i], other_l[i] )
        then fail
    return
  end
end

procedure new_List( x, immediate )
  local l, cnt, right_cnt
  if \immediate                                                 # if we don't need to convert
    then return List(x)                                         # just make a new List.
  l := []                                                       # otherwise: start a new list
  cnt := 2
  while cnt < *x do                                             # while there are still things in the list
  {
    r_cnt := lbal( x[cnt:0] ) + cnt                             # get the next bracketed thing,
    put( l, convert( x[cnt:r_cnt] ))                            # convert it, and put it in the List;
    cnt := r_cnt                                                # go on to the next thing.
  }
  return List( l )                                              # return a new List of the list.
end

################################################
# class Set
#
# the "s" field holds the set of things in the Set.
#
class Set( s )
    method Image()                                              # to see what a set looks like
        local str
        if *self.s = 0 then return "{}"                         # if there is nothing in it then "{}"
        str := "{"                                              # otherwise: start with a "{",
        every str ||:= (!self.s)$Image() || " "                 # get the image of everything in the set,
        return str[1:-1] || "}"                                 # and then put on a "}"
    end

    method Eval()
        local news
        if \err_flag then return Nil
        news := set()                                           # start a new set
        every insert( news, (!self.s)$Eval() )                  # insert the values of all the things in the Set
        return new_Set(news, 1)                                 # and return the new Set. (new_Set will remove duplicates.)
    end

    method Type()
        return "Set"
    end

  method Bang()
    suspend !self.s                                             # generate all the things in the set.
  end

  method Member( y )                                            # to see if something is member
    local x
    every x := !self.s do                                       # iterate thru all the things in the Set
      if Equal( x, y )                                          # looking for something equal.
        then return
    fail
  end

  method Splat()
    return *self.s                                              # return how many things are in the Set.
  end

  method element()
    suspend (!self.s)$element()                                 # return all the atoms in everything in the Set.
  end

  method Equal( x )                                             # to see if two Sets are equal
    if *self.s ~= x$Splat()                                     # check the size of them;
      then fail
    every p := !self.s do                                       # and then check that every thing in one 
      if not x$Member( p ) then fail                            # is in the other.
    return
  end
end

procedure new_Set( x, immediate )
  local s, cnt, r_cnt, new
  s := set()                                                    # start a new set.
  if \immediate then                                            # if immediate, don't need to convert everything.
  {
    every new := !x do                                          # get every thing and
      if not Equal( new, !s ) then insert( s, new )             # if it's not in the Set, then insert it.
  }
  else                                                          # not immediate:
  {
    cnt := 2
    while cnt < *x do                                           # while theres things in the list (x)
    {
      r_cnt := lbal( x[cnt:0] )+cnt                             # get the next bracketed thing,
      new := convert(x[cnt:r_cnt])                              # convert it,
      cnt := r_cnt                                              # and move on to the next thing;
      if not Equal( new, !s ) then insert( s, new )             # if the new one isn't in the Set then insert it.
    }
  }
  return Set( s )                                               # return the new Set.
end    
