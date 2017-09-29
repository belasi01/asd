#' generate a vector of ASD file names ending by *.asd.txt
#'
#' @param basename is a character string corresponding to the base of the file names
#' produced by the RS3 software  (e.g. "StationX_00010.asd.txt"; the basename is "StationX").
#' @param ASD_start is a number corresponding to the first file of a set of ASD measurement
#' (e.g. "StationX_00010.asd.txt"; the number is 10).
#' @param ASD_end is a number corresponding to the last file of a set of ASD measurement.
#' @param ADD_UNDERSCORE is a logical parameter indicating whether or not the basename is
#' separated from the file number in the name. By default, the basename is separated from the file
#' number by an underscore (ADD_UNDERSCORE=TRUE).


generate.file.names.ASD <- function(basename, ASD_start, ASD_end, ADD_UNDERSCORE=TRUE) {

  files_num = as.integer(ASD_start):as.integer(ASD_end)

  # check the numbers to generate the right string lenght
  ix.0000 = which(files_num<10)
  if (length(ix.0000) > 0) {
    if (ADD_UNDERSCORE) list1=paste(basename,
          "_0000",
          as.character(files_num[ix.0000]),
          ".asd.txt",
          sep="") else  list1=paste(basename,
                                    "0000",
                                    as.character(files_num[ix.0000]),
                                    ".asd.txt",
                                    sep="")
  } else list1=NA

  ix.000 = which(files_num>9 & files_num<100)
  if (length(ix.000) > 0) {
    if (ADD_UNDERSCORE) list2=paste(basename,
                "_000",
                as.character(files_num[ix.000]),
                ".asd.txt",
                sep="") else   list2=paste(basename,
                                           "000",
                                           as.character(files_num[ix.000]),
                                           ".asd.txt",
                                           sep="")
  } else list2=NA

  ix.00 = which(files_num>99 & files_num<1000)
  if (length(ix.00) > 0) {
    if (ADD_UNDERSCORE)  list3=paste(basename,
                "_00",
                as.character(files_num[ix.00]),
                ".asd.txt",
                sep="") else list3=paste(basename,
                                         "00",
                                         as.character(files_num[ix.00]),
                                         ".asd.txt",
                                         sep="")
  } else list3=NA


  ix.0 = which(files_num>999 & files_num<10000)
  if (length(ix.0) > 0) {
    if (ADD_UNDERSCORE) list4=paste(basename,
                "_0",
                as.character(files_num[ix.0]),
                ".asd.txt",
                sep="") else list4=paste(basename,
                                         "0",
                                         as.character(files_num[ix.0]),
                                         ".asd.txt",
                                         sep="")
  } else list4=NA

  ix= which(files_num>9999)
  if (length(ix) > 0) {
    if (ADD_UNDERSCORE) list5=paste(basename,
                "_",
                as.character(files_num[ix]),
                ".asd.txt",
                sep="") else list5=paste(basename,
                                         as.character(files_num[ix]),
                                         ".asd.txt",
                                         sep="")
  } else list5=NA


  alllist=c(list1,list2,list3,list4,list5)
  return(alllist[!is.na(alllist)])
}

