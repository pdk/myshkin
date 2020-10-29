################################################################
# misc.iol
#
# this file contains some miscellaneous procedures.
#
procedure balanced( x )                                         # balanced() tells if the list is a completely balanced list
  if lbal(x) = *x                                               # bracket-wise.
    then return
    else fail
end

procedure lbal( l )                                             # lbal() finds the position of the balanced bracket from the
    local angle, paren, squar, curly, pos                       # leftmost thing.
    paren := squar := curly := angle := 0                       # initialize all the counters.
    every pos := 1 to *l do                                     # start at the left position
    {
        case l[pos] of                                          # check what kind of bracket it might be
        {
            "<": angle +:= 1  ;  ">": angle -:= 1               # and increment the appropriate counter.
            "(": paren +:= 1  ;  ")": paren -:= 1
            "[": squar +:= 1  ;  "]": squar -:= 1
            "{": curly +:= 1  ;  "}": curly -:= 1
        }
        if (angle = paren = squar = curly = 0)                  # when all the counter are zero then we've found the position.
            then return pos
    }
    fail                                                        # not enough right-side things were in the string.
end

procedure toke( line, punctuation )                             # procedure toke() takes a string and generates all the 
                                                                # words and punctuation marks.
    local w, wchar
    /punctuation := ''
    wchar := (&ascii -- ' \t') -- punctuation                   # figure out what characters are left to be in words.

    line ?                                                      # with the string
      while tab(upto(wchar++punctuation)) do {                  # move upto a word character or a puntuation mark;
       w := ( tab(any(punctuation)) |                           # get the string containing either one punctuation mark or
              tab(many(wchar))                                  # a lot of word characters.
            )
       suspend w                                                # suspend with the string.
    }
end

procedure toke_list( s, punctuation )                           # procedure toke_list() make a list of all the words and
    local tl                                                    # punctuation marks in an input string.
    tl := []                                                    # start a list.
    every put( tl, toke(s, punctuation) )                       # get every toke and put it in the list.
    return tl                                                   # return the list.
end
