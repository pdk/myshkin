################################################################
# file.iol
#
# this file does all the input/output directing.
# there are three file directors: fin, fout, and msg.
# each one of these keeps a stack of files.  when one closes
# the one on the top of the stack automatically becomes current.
#
procedure file_status()                                         # display the names of the current files.
   msg$Write( "input is          " || fin$Name() )
   msg$Write( "output is         " || fout$Name() )
end

# class director
#
# an object of this type holds a stack of files.
# the direction field says which way the stuff is flowing.  the
# name field contains the name of the current file.  the file 
# has the current file.  and the stack field contains the 
# stack of pending files.
class director( direction, name, file, stack )
    method Read()
        local s
        if self.name == "stdin"                                 # if we're reading from the console, 
          then writes( "? " )                                   # show a "?" prompt.
        s := ""                                                 # start with an empty string.
        while s ||:= read(self.file) do                         # read the current file while possible.
        {
          s ?:= trim(tab(upto('#')|0), ' \t')                   # strip every thing after the "#" (comment) mark.
          if (*s > 0) & (s[-1] ~== "\\")                        # if we got some good stuff and the last character on the 
                                                                # line is not a backslash
            then return s                                       # then return what we've got.
          s := s[1:-1] || " "                                   # otherwise put on a space " ".
          if self.name == "stdin" then                          # if we're reading from the console
          {
            if *s > 0 then writes("-")                          # if we're continuing an input line then show a dash.
            writes("? ")                                        # give the next prompt
          }
        }
        write("-- eof --")                                      # let the user know that a file ended.
        self$Close() | fail                                     # pop to the next file in the stack, or fail.
        return self$Read()                                      # read from the new file.
    end

    method Write(s)
        return write( self.file, s )                            # write a line to the current file.
    end

    method Push()
        push( self.stack, self.name )                           # push a name
        push( self.stack, self.file )                           # and file on the stack.
    end

    method Pop()
        self.file := pop( self.stack )                          # pop a file
        self.name := pop( self.stack )                          # and a name from the stack.
    end

    method Open( n )                                            # Open() opens a new file.
        local q
        if type(n) == "list" then                               # if we get a list of names, 
        {
            while q := pull(n) do self$Open(q)                  # open them all
            return                                              # and return.
        }
        if \self.file                                           # if we have a file currently
            then self$Push()                                    # put it on the stack.
        if n == "-" then                                          # if we're opening the standard file
        {
            if self.direction == "in"                             # if this is an input director
                then self.name := "stdin" & self.file := &input   # then set the name to "stdin" and the file to stdin
                else self.name := "stdout" & self.file := &output # otherwise do stdout;
            return                                                # and return.
        }
        q := ((self.direction == "in" & "r") | "c")             # do "r" if input, "c" if output.
        if self.file := open( n, q ) then {                     # if we open the file
            self.name := n                                      # set the name to the name of the file we just opened
            return                                              # and return.
        } else {                                                # if we can't open the file
            self$Pop()                                          # go back to the one on the stack,
            self$Error("couldn't open "||self.name)             # give and error message,
            fail                                                # and fail.
        }
    end

    method Close()                                              # to close a file
        if *self.stack > 0 then {                               # if there's anything on the stack
            close(self.file)                                    # close the current one,
            self$Pop()                                          # pop to the one on the top of the stack,
            return                                              # and return.
        }
        else                                                    # otherwise
        {
            self$Error("nothing in stack")                      # send a message
            fail                                                # and fail.
        }
    end

    method Error(msg)                                           # write an error message to stderr.
        write(&errout,"Director ("||\self.name||"): " || msg )  
    end

    method Name()                      
        return self.name                                        # return the name of the current file.
    end

initially                                                       # when the object is created
    if /self.direction then                                     # if the direction is unspecified
        self$Error("direction unspecified") & fail              # say so and fail.
    self.stack := list()                                        # otherwise, make a stack
    self$Open(\self.name|"-")                                   # and open the specified file name, or the default file.
end
