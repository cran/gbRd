# version 0.02

# Vizh help-a na Rd_db i primerite tam!
#  ""  sasto tools/R/Rd.R for other internal functions!
#  ""  i RdConv2.R za utility functions.

Rdo_empty <- function(){                                                # Create an empty Rdo
    res <- list()
    class(res) <- "Rd"    # attr( res, "Rd_tag") <- "Rd"
    res
}
                      # Create a minimal Rdo.
                      # todo: this is very basic, among other things, needs also empty lines.
Rdo_create <- function(arguments, title="Dummy title", name="dummy name"){
    res <- Rdo_empty()
    res[[1]] <- Rd_name(name)
    res[[2]] <- Rd_title(title)
    res[[3]] <- arguments
    res
}


Rdo_section <- function(rdo, sec){
    sectag <- sec # redundant now but may facilitate future improvements.
    type <- attr( rdo, "Rd_tag")
    if(type == "Rd"){
        tags <- tools:::RdTags(rdo)
        wrk <- rdo[[which(tags==sec)]] ## use of [[]] assumes only one element here !!! ???
        type <- sec
    }else if (type == sec){
        wrk <- rdo
    }else{ # assume elements of rdo are elements of sec and wrap accordingly.
        wrk <- rdo
        attr( wrk, "Rd_tag") <- sectag

    }
    wrk
}


Rdo_set_sectag <- function(s,sectag,eltag){
    attr( s, "Rd_tag") <- eltag
    res <- list(s)
    attr( res, "Rd_tag") <- sectag

    res
}

Rd_title <- function(s) Rdo_set_sectag(s, sectag="\\title"    , eltag="TEXT")
Rd_name <- function(s)  Rdo_set_sectag(s, sectag="\\name"     , eltag="VERB")
Rd_args <- function(s)  Rdo_set_sectag(s, sectag="\\arguments", eltag="VERB")

Rdo_get_args <- function(rd,args,...){     # tools:::RdTags(rd[[which(tags=="\\arguments")]])

   rdo <- Rd_fun(rd,
                 , keep_section  = "\\arguments"
                 )

    tags <- tools:::RdTags(rdo)
    rdargs <- rdo[[which(tags=="\\arguments")]] ## use of [[]] assumes only one element here

    # to do:
    # Ako iskam da vklyucha obrabotka na prazni redove i drug text mezhdu item-ite,
    #  obrabotkata ste se uslozhni. Tryabva i dopalnitelen argument!
    if(missing(args))
        return(rdargs)

   # uslozhnyavam f, za da obrabotva i sluchai, kogato
   # nyakolko argumenta sa opisani v edin item.
   # f <- function(x){x[[1]] %in% args}
   f <- function(x){
       # tozi code tryabva da se ischisti, dokato otkriya tazi rabota
       # (t.e. che e neobchodimo as.character se omotach.
       wrk0 <- as.character(x[[1]])    # x[[1]] is tagged with Rd_tag or similar!
       if(wrk0 %in% args)
           return(TRUE) # else return(FALSE)
       # print(wrk0)
       wrk <- strsplit(wrk0,",[ ]*")
       if(!is.character(wrk[[1]])){
           warning("wrk[[1]] is not a character vector! ", wrk)
           return(FALSE)
       }
       wrk <- any( wrk[[1]] %in% args )   # ima li nuzhda ot trim na blanks?
       # if(isTRUE(wrk)) TRUE else FALSE   # krapka, inache ne raboti korektno!
       wrk
   }
    sel <- !sapply(rdargs, f)

    # deal with "..." arg
    if("..." %in% args || "\\dots" %in% args){  # since formals() represents ... by "..."
        f2 <- function(x){
            if(is.list(x[[1]]) && length(x[[1]])>0 &&
               attr(x[[1]][[1]],"Rd_tag") == "\\dots")
                TRUE
            else
                FALSE
        }
        i2 <- sapply(rdargs, f2)
        # print(i2)
        sel[i2] <- FALSE
    }

    rdargs[sel] <- NULL   # keeps attributes (even if 0 or 1 elem remain).
    rdargs
}

Rdo_get_arg <- function(rd,arg){
    wrk <- Rdo_get_args(rd,arg)
    wrk[[1]]
}


Rdo_args2txt_list <- function(x,arg,...){
    rdo <- Rd_fun(x)
    if(missing(arg)){
        tmparg <- tools:::.Rd_get_argument_names(rdo)
        # now correct for merged descriptions...
        arg <- character(0)
        for(s in tmparg){
            arg <- c(arg, strsplit( as.character(s), ",[ ]*")[[1]])
        }
    }
    # print(arg)
    res <- list()
    for(a in arg)
        res[[a]] <- Rdo_args2txt(rdo,a,...)
    res
}

Rdo_args2txt <- function(rdo,arg,title="Hhh",name="Aa",type="text"){
    wrk <- Rdo_get_args(rdo,arg)
    wrk2 <- Rdo_create(arguments=wrk,title=title,name=name)

    res <- Rd_help2txt(wrk2
                       , keep_section = "\\arguments"
                       , omit_sec_header = TRUE
                       )

    ## nay-dobre e da ima programka, koyato da macha izlishni poleta!

    res <- paste(res,collapse="\n")
    res
}



# based on print.help_files_with_topic() in the sources of R-2.10.0.
Rd_help2txt <- function(x, topic, pkgname=""
                        , help_type="text"
                        , verbose=FALSE
                        , try.all.packages=FALSE
                        , keep_section = TRUE
                        , omit_sec_header = FALSE
                        ){
    rdo <- Rd_fun(x, topic=topic, pkgname=pkgname
                  , help_type        = help_type
                  , verbose          = verbose
                  , try.all.packages = try.all.packages
                  , keep_section     = keep_section
                  )

    temp <- tools::Rd2txt(rdo, out=tempfile("Rtxt"), package=pkgname)

    res <- readLines(temp) # note: temp is a (temporary) file name.
    unlink(temp)


    ## krapka, iztrii title i/ili name ako ne sa poiskani.
    iomit <- numeric(0)
              # the code below assumes thateach item is on one line followed by  a blank line.
    if(!("\\title" %in% keep_section))
        iomit <- c(iomit,1:2)

    # !!! Poleto "name" ne vliza v teksta, zatoca nyama nuzhda ot tova!
    # if(!("\\name" %in% keep_section))
    #     iomit <- c(iomit,3:4)

    if(isTRUE(omit_sec_header))
        iomit <- c(iomit,3:4)

    ## file.show(temp,
    ##           title = gettextf("R Help on '%s'", topic),
    ##           delete.file = TRUE)

    if(length(iomit)>0)
        res <- res[-iomit]          # !!! ??? MNOGO GRUBA KRAPKA za omit-vane na title/name

    res
}


# based on print.help_files_with_topic() in the sources of R-2.10.0.
Rd_fun <- function(x, topic, pkgname=""
                    , help_type="text"
                    , verbose=FALSE
                    , try.all.packages=FALSE
                    , keep_section = TRUE
                   ){
                                                       # is it better to check with "inherit"?
    rdo <- NULL
    ## prepare the "Rd" object rdo
    if(class(x) == "Rd"){  # if(inherits(file, "Rd")) ...
        rdo <- x
    }else{
        if(class(x) != "help_files_with_topic" ){
            ## help returns an object of class "help_files_with_topic"
            ##  the  eval(substitute()) wrapper
            ## (saw it in tkGUI, vzh sasto help.R, sasto: .tryHelp in question.R)
            ## is needed to cover
            ## the case when x is a function. Without this wrapper the result is not correct.
            ## Izglezhda, che bez substitute() argumentat se evvaluate-va
            ## na nepodochodyasto myasto.
            ## If x is a name of a function, then the wrapper is not needed.

            # wrk <- eval(substitute(help(x, help_type=help_type
            #            , verbose=verbose
            #            , try.all.packages=try.all.packages)))


            # wrk <- eval(substitute(help(x, help_type=help_type
            #            , verbose=verbose
            #            , try.all.packages=try.all.packages)))


            # cat("KUKUKUUUU: ", substitute(x), "   class(x): ", class(x), "\n\n" )
            wrk <- do.call("help",list(x, help_type=help_type
                       , verbose=verbose
                       , try.all.packages=try.all.packages))


            # cat("kiki!\n")
            x <- wrk
        }

        ## Check for errors! ???
        # print(unclass(x))
        # cat("\n\nx is: "    ,unclass(x)                      ,"\n\n\n")

        if(class(x) == "help_files_with_topic"){

            # cat("kuku!\n")

            ## from print.help_files_with_topic in help.R
            ##
            ## browser <- getOption("browser")
            topic <- attr(x, "topic")
            type <- attr(x, "type")
            paths <- as.character(x) # removes attributes of x.

            file <- paths

                                                     # !!! check for lenght(paths)==0  !!!! ??
            # the following commands are probably copied from utils::: .getHelpFile
            # but no error is raized, rdo simply remain NULL.
            path <- dirname(file)
            dirpath <- dirname(path)
            pkgname <- basename(dirpath)
            RdDB <- file.path(path, pkgname)

            ## cat("\n\nx is: "    ,unclass(x)                      ,"\n\n\n")
            ## cat("paths is: ",paths                      ,"\n")
            ## cat("file is: ", file                       ,"\n")
            ## cat("path is: ", path                       ,"\n")
            ## cat("RdDB is: ", paste(RdDB, "rdx", sep="."),"\n")

            if(file.exists(paste(RdDB, "rdx", sep="."))) {
            # cat("kiwi!\n")
                rdo <- tools:::fetchRdDB(RdDB, basename(file))
                # a debugging message, remove later!
                # cat("Class of object returned by \"tools:::fetchRdDB: ", class(rdo),"\n")
                # naistina return-va "Rd".
            }
        }
    }
    if(is.null(rdo))                       # todo: should someting less radical be done?
        stop("rdo object is NULL!")

    if(is.character(keep_section) && length(keep_section)>0){
        tags <- tools:::RdTags(rdo)
        keep_tags <- unique(c("\\title","\\name",keep_section))
        rdo[which(!(tags %in% keep_tags))] <-  NULL
    }

    rdo
}


# wrk[[1]] <- Rd_name("Random name")
# wrk[[2]] <- Rd_title("Kukurigu")
# Rd2txt(wrk)
# Rd2HTML(wrk)
