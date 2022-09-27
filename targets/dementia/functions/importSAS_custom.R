


# Copy of extra working version of importSAS named isas made by Christian, originally located here:
#source('Z:/Workdata/706582/christiantorp-pedersen/importSAS_til_TAG/isas_CTP.R')

importSAS_custom<- function(filename, wd = NULL, keep = NULL, drop = NULL, where = NULL,
         obs = NULL, filter = NULL, filter.by = NULL, filter.cond = c(1, 1),
         set.hook = NULL, step.hook = NULL, pre.hook = NULL,
         post.hook = NULL, savefile = NULL, overwrite = TRUE, show.sas.code = TRUE,
         save.tmp = FALSE, content = FALSE, na.strings = c("\\."), date.vars = NULL,datetime.vars=NULL,
         character.vars = "pnr", numeric.vars = NULL, sas.program,
         sas.switches, sas.runner, skip.date.conversion = FALSE,force.numeric=TRUE,
         sas.data.extension="sas7bdat",verbose = TRUE,
         ...)
{
  if (!file.exists(filename)){
    stop(paste0("A file with name ",filename," does not exist."))
  }else{
    if (tolower(tools::file_ext(filename))!=sas.data.extension)
      stop("The filename exists, but file extension does not match sas.data.extension: ",sas.data.extension)
  }
  ## DD <- dirname(filename)
  ## FF <- basename(filename)
  ## fullname <- list.files(path=DD,pattern=paste0("^",filename,"$"),full.names=TRUE)
  .SD = NULL
  keep <- tolower(keep)
  drop <- tolower(drop)
  date.vars <- tolower(date.vars)
  datetime.vars <- tolower(datetime.vars)
  olddir <- getwd()
  if (length(wd) == 0){
    wd <- getwd()
  }else{
    setwd(wd)
  }
  # cleaning up old temporary directories
  olddirectories <- list.files(wd,pattern="heaven_tempSASfiles[a-z0-9]+")
  for (old in olddirectories){
    message("Cleaning up temporary directories from previous calls.")
    unlink(old,recursive=TRUE,force=TRUE)
  }
  tmpname <- basename(tempfile(pattern="heaven_tempSASfiles"))
  tmpdir = paste0(wd,"/",tmpname)
  if (verbose) message("Writing temporary SAS files (log, lst, input data, output data) to directory\n",tmpdir)
  if (file.exists(tmpdir)) {
    stop(paste("file.exists:",tmpdir))
  }else{
    try.val <- try(dir.create(tmpdir))
    if (class(try.val)[[1]]=="try-error")
      stop("Cannot create temporary directory.\nYou probably do not have permission to write to the directory: \n ",
           tmpdir, "\nTry to specify another directory with the argument \"wd\" or change the working directory.",
           sep = "")
  }
  if (length(savefile) > 0) {
    outfile <- paste(wd, "/", savefile, sep = "")
    if (file.exists(outfile)) {
      if (interactive()){
        maybestop <- utils::askYesNo(paste0("Overwrite existing file: ",outfile,"? "))
        if (is.na(maybestop)||maybestop==FALSE){
          stop("File exists:",outfile)
        }
      }else{
        stop("File exists:",outfile)
      }
    }
    message("The output of SAS will be saved to file:\n",outfile)
  }
  else {
    outfile <- paste(tmpdir, "/", "sasimport_internal_tmpout.csv",
                     sep = "")
  }
  setwd(tmpdir)
  on.exit({
    setwd(olddir)
    if (!save.tmp) {
      unlink(tmpdir,recursive=TRUE,force=TRUE)
    }else{
      cat("\nTemporary directory:\n ",tmpdir,"\ncan now be inspected -- and should be removed manually afterwards!\n")
    }
  })
  if (.Platform$OS.type == "unix") {
    if (missing(sas.program)) {
      sas.program <- "sas"
    }
    if (missing(sas.switches)) {
      sas.switches <- ""
    }
    if (missing(sas.runner)) {
      sas.runner <- "system"
    }
  }
  else {
    if (missing(sas.program)) {
      sas.program <- "C:/Program Files/SASHome/SASFoundation/9.4/sas.exe"
    }
    if (missing(sas.switches)) {
      sas.switches <- "-batch -nosplash -noenhancededitor -sysin "
    }
    if (missing(sas.runner)) {
      sas.runner <- "shell"
    }
  }
  existing.files <- NULL
  tmp.SASfile <- paste(tmpdir, "/", "sasimport_internal_tmpfile.sas",
                       sep = "")
  tmp.log <- paste(tmpdir, "/", "sasimport_internal_tmpfile.log",
                   sep = "")
  tmp.filterfile <- paste(tmpdir, "/", "sasimport_internal_tmpfilterfile.csv",
                          sep = "")
  tmp.SASproccont <- paste(tmpdir, "/", "sasimport_internal_tmpproccont.sas",
                           sep = "")
  tmp.proccontout <- paste(tmpdir, "/", "sasimport_internal_tmpproccontout.csv",
                           sep = "")
  tmp.proccontlog <- paste(tmpdir, "/", "sasimport_internal_tmpproccont.log",
                           sep = "")
  try.val <- try(file.create(tmp.SASfile))
  
  files <- c(tmp.SASfile, tmp.log, tmp.filterfile, outfile,
             tmp.SASproccont, tmp.proccontout, tmp.proccontlog)
  for (file in files) {
    if (file.exists(file))
      existing.files <- c(existing.files, basename(file))
  }
  if (length(existing.files) > 0 & overwrite == FALSE) {
    stop(paste("Aborted to not overwrite the file(s):", paste(" ",
                                                              existing.files, collapse = "\n"), "in the directory:",
               paste(" ", tmpdir), "Set the argument \"overwrite\" equal to \"TRUE\" to allow overwriting.",
               sep = "\n"))
  }
  for (file in files) {
    if (file.exists(file))
      file.remove(file)
  }
  cond <- ""
  
  ## ----------------------------- start proc contents -------------------------
  file.create(tmp.SASproccont)
  cat("ods listing close;\nODS OUTPUT variables=dcontent; \n proc contents data='",
      filename, "';\nrun;\nproc sort data=dcontent;\nby num;\nrun; \ndata _NULL_; \nset dcontent; \n file '",
      tmp.proccontout, "' dsd; \nif _n_ eq 1 then link names; \nput (_all_)(~); return; \nnames:\nlength _name_ $32; \ndo while(1); \ncall vnext(_name_); \nif upcase(_name_) eq '_NAME_' then leave; \nput _name_ ~ @; \nend; \nput; \nreturn; \nrun;\n",
      sep = "", file = tmp.SASproccont, append = TRUE)
  if (.Platform$OS.type == "unix")
    fprog <- paste0(sas.program, " ", sas.switches, " ",
                    tmp.SASproccont)
  else fprog <- paste0("\"\"", sas.program, "\" ", sas.switches,
                       "\"", tmp.SASproccont, "\"\"")
  runcontents <- try(do.call(sas.runner, list(fprog)), silent = FALSE)
  if (class(runcontents)[1] == "try-error") {
    warning(paste("Running sas on", fprog, "yielded the error shown above."))
  }
  if (file.exists(tmp.proccontout)) {
    suppressWarnings(dt.content <- data.table::fread(file = tmp.proccontout,
                                                     header = TRUE)[, c("Variable","Type","Format","Informat"),with=FALSE])
  }
  else {
    stop(paste("Running sas on", fprog, "did not produce the expected output file."))
  }
  
  ## ----------------------------- end proc contents -------------------------
  if (length(keep) > 0) {
    ## make life easier for the user: if pnr is not in var.names remove it
    if (!("pnr" %in% tolower(dt.content$Variable))) {
      character.vars <- character.vars[character.vars!="pnr"]
    }
    keep <- unique(c(keep,date.vars,datetime.vars,numeric.vars,character.vars))
    if ("pnr" %in% tolower(dt.content$Variable)) {
      if (!("pnr" %in% keep)) keep <- c("pnr",keep)
    }
    cond <- paste(cond, "keep=", paste(keep, collapse = " "),
                  " ", sep = "")
  }else{
    # keep all variables
    keep <- tolower(dt.content$Variable)
  }
  if (length(drop) > 0) {
    cond <- paste(cond, "drop=", paste(drop, collapse = " "),
                  " ", sep = "")
  }
  if (length(where) > 0) {
    cond <- paste(cond, "where=(", where, ") ", sep = "")
  }
  if (length(obs) > 0 && !is.infinite(obs)) {
    cond <- paste(cond, "obs=", format(obs, scientific = FALSE),
                  " ", sep = "")
  }
  if (length(cond) > 0) {
    if (length(set.hook) > 0 & is.character(set.hook))
      cond <- paste("(", cond, set.hook, ")", sep = " ")
    else cond <- paste("(", cond, ")", sep = " ")
  }
  if (length(filter)>0){
    ## convert filter variables to lower
    orig.filter.names <- copy(names(filter))
    setnames(filter,tolower(names(filter)))
    if (length(filter.by) == 0) {
      filter.names <- names(filter)
    }
    else {
      filter.names <- tolower(filter.by)
    }
  }
  keep.check <- drop.check <- filter.check <- TRUE
  if (length(keep) > 0) {
    keep.check <- tolower(keep) %in% tolower(dt.content$Variable)
  }
  if (length(drop) > 0) {
    drop.check <- tolower(drop) %in% tolower(dt.content$Variable)
  }
  if (length(filter) > 0) {
    filter.check <- filter.names %in% tolower(dt.content$Variable)
  }
  # autodetect variable types and formats
  # ----------------------------------------------------------------
  # restrict var.format and var.type to interesting variables
  
  dt.content <- dt.content[tolower(Variable)%in%keep]
  # assess formats according to SAS proc contents
  dt.content[,target.type:="character"]
  dt.content[grepl("num", Type, ignore.case = TRUE),target.type:="numeric"]
  dt.content[grepl("date|dato|DDMM|MMDD", Format, ignore.case = TRUE) |
               grepl("date|dato|DDMM|MMDD", Informat, ignore.case = TRUE),target.type:="date"]
  dt.content[grepl("datetime", Format, ignore.case = TRUE) |
               grepl("datetime", Informat, ignore.case = TRUE),target.type:="datetime"]
  # user may force different types
  if (!is.null(date.vars)){
    for (name in tolower(date.vars)) {
      if (!(name %in% tolower(dt.content$Variable))){
        warning(paste(name, "not found in dataset."))
      } else {
        dt.content[name == Variable,target.type:="date"]
      }
    }
  }
  if (!is.null(datetime.vars)){
    for (name in tolower(datetime.vars)) {
      if (!(name %in% tolower(dt.content$Variable))){
        warning(paste(name, "not found in dataset."))
      } else {
        dt.content[name == Variable,target.type:="datetime"]
      }
    }
  }
  if (!is.null(numeric.vars)){
    for (name in tolower(numeric.vars)) {
      if (!(name %in% tolower(dt.content$Variable))){
        warning(paste(name, "not found in dataset."))
      } else {
        dt.content[name == Variable,target.type:="numeric"]
      }
    }
  }
  if (!is.null(character.vars)){
    for (name in tolower(character.vars)) {
      if (!(name %in% tolower(dt.content$Variable))){
        warning(paste(name, "not found in dataset."))
      } else {
        dt.content[name == Variable,target.type:="character"]
      }
    }
  }
  dt.content[tolower(Variable)%in%numeric.vars,target.type:="numeric"]
  dt.content[tolower(Variable)%in%character.vars,target.type:="character"]
  #
  datetime.vars <- dt.content[target.type=="datetime"]$Variable
  date.vars <- dt.content[target.type=="date"]$Variable
  numeric.vars <- dt.content[target.type=="numeric"]$Variable
  character.vars <- dt.content[target.type=="character"]$Variable
  #
  # could consider a format statement for variables where user specifies
  # numeric.vars or character.vars. for now only formatting date
  # and datetime variables
  format.statement <- if (length(date.vars)==0||skip.date.conversion[[1]] == TRUE)
    ""
  else paste("format ", paste(date.vars, collapse = " "), " yymmdd10.;")
  if (length(datetime.vars)>0 && skip.date.conversion[[1]] == FALSE)
    format.statement <- paste(format.statement,paste("\nformat ", paste(datetime.vars, collapse = " "), " datetime.;"))
  if (length(pre.hook) > 0 & is.character(pre.hook)) {
    cat(pre.hook, file = tmp.SASfile, append = TRUE)
  }
  if (length(filter) > 0) {
    data.table::fwrite(filter, quote = TRUE, file = tmp.filterfile)
    cat("proc import datafile='", tmp.filterfile, "' \n",
        "out = csv_import \ndbms =csv; \nrun; \n", sep = "",
        file = tmp.SASfile, append = TRUE)
  }
  cat("data df; \nset '",filename,"'",cond,";\n",format.statement,sep = "",file = tmp.SASfile,append = TRUE)
  if (length(step.hook) > 0 & is.character(step.hook)) {
    cat(step.hook, sep = "", file = tmp.SASfile, append = TRUE)
  }
  cat("\nrun;\n", sep = "", file = tmp.SASfile, append = TRUE)
  if (length(filter) > 0) {
    cat("proc sort data=csv_import; \nby ", paste(filter.names,
                                                  collapse = " "), "; \nrun; \nproc sort data=df; \nby ",
        paste(filter.names, collapse = " "), "; \nrun;\n ",
        sep = "", file = tmp.SASfile, append = TRUE)
    tmp.merge.statement <- matrix(paste(c("( NOT", "( "),
                                        rep(letters[1:2], each = 2), ")"), ncol = 2)
    tmp.merge.statement <- rbind(tmp.merge.statement[1, ],
                                 rep("", 2), tmp.merge.statement[2, ])
    if (any(filter.cond == 0)) {
      if (all(filter.cond == 0)) {
        merge.cond.statement <- ""
      }
      else {
        merge.cond.statement <- paste("if", tmp.merge.statement[(filter.cond[filter.cond !=
                                                                               0] + 2), which(filter.cond != 0)], ";\n")
      }
    }
    else {
      merge.cond.statement <- paste("if", tmp.merge.statement[(filter.cond[1] +
                                                                 2), 1], "AND", tmp.merge.statement[(filter.cond[2] +
                                                                                                       2), 2], ";\n")
    }
    cat("data df; \nmerge csv_import(IN=a) df(IN=b); \nby ",
        paste(filter.names, collapse = " "), ";\n", merge.cond.statement,
        "run;\n", sep = "", file = tmp.SASfile, append = TRUE)
  }
  if (length(post.hook) > 0 & is.character(post.hook)) {
    cat(post.hook, file = tmp.SASfile, append = TRUE)
  }
  if (show.sas.code == TRUE) {
    cat("\nRunning the following sas code in the background.\nYou can cancel SAS at any time.\n")
    cat("\n-------------------------SAS-code-------------------------\n")
    cat(readChar(tmp.SASfile, file.info(tmp.SASfile)$size))
    cat("\n----------------------------------------------------------\n")
  }
  else {
    if (verbose)
      cat("\nRunning sas code in the background. You can cancel SAS at any time.\n")
  }
  tmp.lines <- paste("data _NULL_; \nset df; \n file '", outfile,
                     "' dsd; \nif _n_ eq 1 then link names; \nput (_all_)(~); return; \nnames:\nlength _name_ $32; \ndo while(1); \ncall vnext(_name_); \nif upcase(_name_) eq '_NAME_' then leave; \nput _name_ ~ @; \nend; \nput; \nreturn; \nrun;\n")
  cat(tmp.lines, file = tmp.SASfile, append = TRUE)
  if (!(prod(keep.check) * prod(drop.check) * prod(filter.check))) {
    error.mes <- "Some of the variables specified in the keep or drop statement or in the filter file is not found in the import file.\n"
    if (length(keep) > 0)
      error.mes <- paste(error.mes, "The KEEP argument(s): \n  ",
                         paste(tolower(keep[!keep.check]), collapse = "\n"),
                         "\nare not found in the import file.\n", sep = "")
    if (length(drop) > 0)
      error.mes <- paste(error.mes, "The DROP argument(s): \n  ",
                         paste(tolower(drop[!drop.check]), collapse = "\n"),
                         "\nare not found in the import file.\n", sep = "")
    if (length(filter) > 0)
      error.mes <- paste(error.mes, "The FILTER argument(s): \n  ",
                         paste(filter.names[!filter.check], collapse = "\n"),
                         "\nare not found in the import file.\n", sep = "")
    error.mes <- paste(error.mes, "\nThe content of the import file is:\n",
                       sep = "")
  }
  if (content == TRUE) {
    df <- dt.content
    if (!(prod(keep.check) * prod(drop.check) * prod(filter.check))) {
      cat(paste("Warning:\n", error.mes, sep = ""))
      print(df)
      cat("\nThis will give an error when content=FALSE.\n")
    }
  }
  else {
    if (!(prod(keep.check) * prod(drop.check) * prod(filter.check))) {
      cat(paste("Error:\n", error.mes, sep = ""))
      print(dt.content)
      stop("Aborted.")
    }
    else {
      if (.Platform$OS.type == "unix") {
        fprog <- paste0(sas.program, " ", sas.switches,
                        " ", tmp.SASfile)
      }
      else {
        fprog <- paste0("\"\"", sas.program, "\" ", sas.switches,
                        "\"", tmp.SASfile, "\"\"")
      }
      runSAS <- try(do.call(sas.runner, list(fprog)), silent = FALSE)
      if (class(runSAS)[1] == "try-error") {
        warning(paste("Running sas on", fprog, "yielded the error shown above."))
      }
      if (!file.exists(outfile)) {
        stop(paste0("SAS did not produce output file. Maybe you have misspecified a SAS statement?",
                    ifelse(save.tmp == FALSE, "\nRun with save.tmp=TRUE and then",
                           "\nPlease"), " check the log file:\n", tmp.log))
      }
      info <- file.info(outfile)
      ia <- c(list(file = outfile, header = TRUE),
              list(...))
      # types of variables
      if (length(ia$colClasses) == 0){
        ia$colClasses <- list(character = NULL, numeric = NULL)
      }else{
        ia$colClasses <- lapply(ia$colClasses,tolower)
      }
      # for some reason the variable name of pnr is lower case
      # in the file that SAS writes to disc
      uu <- data.table::fread(outfile,nrows=1)
      print(character.vars)
      print(uu[,1:3,with=0L])
      #browser()
      if ("pnr"%in%tolower(character.vars))
        character.vars[tolower(character.vars)=="pnr"] <- "pnr"
      ia$colClasses[["character"]] <- c(character.vars,date.vars,datetime.vars)
      ia$colClasses[["numeric"]] <- numeric.vars
      
      # reset filternames
      if (length(filter) > 0){
        setnames(filter,orig.filter.names)
      }
      if (info$size == 0) {
        warning("The dataset produced by SAS appears to be empty.")
        # empty data set
        return(df)
      }
      else {
        if (verbose)
          tryread <- try(df <- do.call(data.table::fread,ia))
        else
          suppressWarnings(tryread <- try(df <- do.call(data.table::fread,ia)))
        if ("try-error" %in% class(tryread)) {
          warning("Could not read the constructed dataset into R. \nSomething probably went wrong during SAS program execution. ")
          df <- NULL
        }
      }
      # change case of variable names
      names(df) <- tolower(names(df))
      # force numeric format
      if (force.numeric[[1]] == TRUE && length(numeric.vars) > 0) {
        numeric.vars <- tolower(numeric.vars)
        if (verbose)
          try(df[, `:=`((numeric.vars), lapply(.SD, as.numeric)),
                 .SDcols = numeric.vars], silent = FALSE)
        else
          suppressWarnings(try(df[, `:=`((numeric.vars), lapply(.SD, as.numeric)),
                                  .SDcols = numeric.vars], silent = FALSE))
      }
      # deal with na.strings in character variables
      if (length(na.strings)>0 && length(character.vars) > 0) {
        character.vars <- tolower(character.vars)
        if (verbose)
          try(df[, `:=`((character.vars), lapply(.SD, function(x){x[grepl(paste0(na.strings,collapse="|"),x)] <- NA;x})),
                 .SDcols = character.vars], silent = FALSE)
        else
          suppressWarnings(
            try(df[, `:=`((character.vars), lapply(.SD, function(x){x[grepl(paste0(na.strings,collapse="|"),x)] <- NA;x})),
                   .SDcols = character.vars], silent = FALSE)
          )
      }
      # date format
      if (skip.date.conversion[[1]] == FALSE && length(date.vars)>0) {
        date.vars <- tolower(date.vars)
        # respect if user wants character or numeric format instead of date format
        if (any(c(character.vars,numeric.vars)%in%date.vars))
          date.vars <- setdiff(date.vars,c(character.vars,numeric.vars))
        if (verbose)
          try(df[, `:=`((date.vars), lapply(.SD, lubridate::ymd)),
                 .SDcols = date.vars], silent = FALSE)
        else
          suppressWarnings(try(df[, `:=`((date.vars), lapply(.SD, lubridate::ymd)),
                                  .SDcols = date.vars], silent = FALSE))
      }
      # datetime format
      if (skip.date.conversion[[1]] == FALSE && length(datetime.vars)>0) {
        datetime.vars <- tolower(datetime.vars)
        # respect if user wants character or numeric format instead of date format
        if (any(c(character.vars,numeric.vars)%in%date.vars))
          datetime.vars <- setdiff(datetime.vars,c(character.vars,numeric.vars))
        if (verbose)
          try(df[, `:=`((datetime.vars), lapply(.SD, lubridate::dmy_hms)),
                 .SDcols = datetime.vars], silent = FALSE)
        else{
          suppressWarnings(try(df[, `:=`((datetime.vars), lapply(.SD, lubridate::dmy_hms)),
                                  .SDcols = datetime.vars], silent = FALSE))
        }
      }
    }
  }
  return(try(df[], silent = TRUE))
}