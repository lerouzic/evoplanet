

# A couple of helper functions
is.letter <- function(x) grepl("[[:alpha:]]", x)
is.number <- function(x) grepl("[[:digit:]]", x)
rm.last.char <- function(s) substr(s, 1, nchar(s)-1)

read.genotypes <- function(file, ncol=12) {
	# Reads and formats the genotype table. 
	# Excel files are OK
	# Assumes a 12-column format
	if (grep(file, pattern="\\.xls")) {
		library("readxl")
		dd <- as.matrix(readxl::read_excel(file))
		
	} else {
		dd <- as.matrix(read.table(file)) # Not necessary? Genotypes are always provided as Excel files? 
	}
	stopifnot(ncol(dd) == ncol || ncol(dd) == ncol+1)
	if (ncol(dd) == ncol+1)
		dd <- dd <- dd[,2:ncol(dd)]
	rownames(dd) <- LETTERS[1:nrow(dd)]
	dd
}

read.ODtable <- function(sss) {
	# (this function should not be called directly by the user)
	# sss should be a vector of 10 character strings (10 lines of the file corresponding to a table
	s <- lapply(strsplit(sss, "\\s+"), function(st) st[nzchar(st)])
	
	# File structure:
	# Line #1: Plate: ...
	# Line #2: Temperature 1 2 3...
	# Line #3: 22.2 data1 data2 data3
	# Line #4 to 10: data1 data2 data3...
	
	stopifnot(length(sss) == 10)
	stopifnot(s[[1]][1] == "Plate:")
	m <- do.call(rbind, lapply(s[3:10], function(x) if(length(x) == 13) as.numeric(x[2:13]) else as.numeric(x)))

	attr(m, "name")  <- s[[1]][2]                 # Plate name
	attr(m, "wavel") <- as.numeric(s[[1]][11])    # Wave length (from the metadata)
	attr(m, "temp")  <- as.numeric(s[[3]][1])     # Real temperature
	m
}

read.ODdata <- function(file, encoding="UTF16LE") {
	# Reads the OD file and formats the results
	# The output is a list of 12 x 8 matrices
	# Warning: this function assumes that the file format follows exactly the template. In particular:
	# * Each meaningful block is expected to start with "Plate:"
	# * Data tables are expected to be 12 x 8
	# * The first data line of the table contains the temperature
	# The file encoding was unusual, so it has to be specified
	
	ss <- readLines(con <- file(file, encoding=encoding))
	close(con)
	
	plate.lines <- which(grepl(ss, pattern="Plate:"))
	stopifnot(length(plate.lines) > 0)
	
	ans <- lapply(plate.lines, function(pl) read.ODtable(ss[pl:(pl+9)]))
	names(ans) <- sapply(ans, attr, "name")
	
	ll <- unlist(strsplit(ss[length(ss)], "\\s+"))
	attr(ans, "date") <- ll[which(grepl(ll, pattern="Saved:"))+1]	
	attr(ans, "time") <- ll[which(grepl(ll, pattern="Saved:"))+2]
	attr(ans, "file") <- rm.last.char(ll[which(grepl(ll, pattern="Filename:"))+1])
	
	ans
}

ODdata.table <- function(ODfile, genfile) {
	gen <- read.genotypes(genfile)
	od  <- read.ODdata(ODfile)
	
	ans <- do.call(rbind, c(lapply(od, function(oo) {
			nm <- attr(oo, "name")
			nm <- unlist(strsplit(nm, split="_+"))
			nm1 <- unlist(strsplit(nm[1], ""))
			food      <- paste0(nm1[is.letter(nm1)], collapse="") # From the plate name
			temp      <- paste0(nm1[is.number(nm1)], collapse="") # From the plate name
			replicate <- paste0(attr(od, "file"), "_", c(letters,LETTERS)[which(names(od) %in% attr(oo, "name"))])
			# if (abs(as.numeric(temp) -attr(oo, "temp")) > 1) 
			#  	warning("Replicate", nm, ", Temperature mismatch (", temp, " vs ", attr(oo, "temp"))
			data.frame(
				Food       = rep(food, length(oo)),
				Temp       = rep(temp, length(oo)),
				Wavelength = rep(attr(oo, "wavel"), length(oo)),
				Genotype   = c(gen),
				BioReplicate= as.character(1+duplicated(c(gen))),
				TechReplicate=rep(replicate, length(oo)),
				DataFile   = rep(attr(od, "file"), length(oo)),
				Date       = rep(attr(od, "date"), length(oo)),
				Time       = rep(attr(od, "time"), length(oo)),
				OD         = c(oo)
			)
		}), list(make.row.names=FALSE)))
	# Probably unnecessary if we are going to save the file
	ans$Food <- factor(ans$Food)
	ans$Temp <- factor(ans$Temp)
	ans$Wavelength <- factor(ans$Wavelength)
	ans$Genotype <- factor(ans$Genotype)
	ans$BioReplicate <- factor(ans$BioReplicate)
	ans$TechReplicate <- factor(ans$TechReplicate)
	ans$DataFile <- factor(ans$DataFile)
	ans$Date <- as.Date(ans$Date,  format="%d/%m/%Y")
	ans$Time  <- strptime(paste(ans$Date, ans$Time), format="%Y-%m-%d %H:%M:%S", tz="CET")
	
	ans
}
