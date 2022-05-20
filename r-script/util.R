
# split by '.', and return last token
getClassName <- function(class){
	k = strsplit(class,'.',fixed=TRUE)
	k = k[[1]]
	return(k[length(k)])
}

# split by "_"
getProjectName <- function(proj){
	k = strsplit(proj,'_',fixed=TRUE)
	k = k[[1]]
	return(k[2])
}


# classDescription file should have two columns: project name and class name
sampleStratifiedSelection <- function(pathToClassDescription, n, outputFile){

	mp <-  read.table(pathToClassDescription,header=F)
	PROJ_COLUMN = 1
	CLASS_COLUMN = 2

	sampled = c()

	projects = sort(unique(mp[,PROJ_COLUMN]))
	totalProj = length(projects)

	final = c()

	perProj = floor(n / totalProj)
	if(perProj > 0){ # at least one per project
		for(p in projects){
			mask = mp[,PROJ_COLUMN] == p
			classes = mp[mask , CLASS_COLUMN]

			if(length(classes) <= perProj){
				selected = classes
			} else {
				selected = sample(classes,perProj,replace = FALSE)
			}

			for(sel in selected){
				sampled = c(sampled , paste(p," \t ",sel,sep=""))
			}
		}
	}

	## fill the remaining at random
	while(length(sampled) < n){

		p = sample(projects,1)
		mask = mp[,PROJ_COLUMN] == p
		classes = mp[mask , CLASS_COLUMN]
		sel = sample(classes,1)
		val = paste(p," \t ",sel,sep="")

		if(any(sampled == val)){
			next()
		}

		sampled = c(sampled , val)
	}

	sampled = sort(sampled)

	TABLE = outputFile
	unlink(TABLE)
	sink(TABLE, append=TRUE, split=TRUE)

	for(row in sampled){
		cat(row,"\n")
	}

	sink()
}


gatherAllTables <- function(directory,ignoreColumns=NULL,filePattern="^statistics(\\w|-|_)*\\.csv$"){
	allTables = NULL

	for(table in list.files(directory,recursive=TRUE,full.names=TRUE,pattern=filePattern) ){

		cat("Reading: ",table,"\n")

		tryCatch( {dt <- read.csv(table,header=T)} ,
				error = function(e){
					cat("Error in reading table ",table,"\n", paste(e), "\n")
				})

		if(! is.null(ignoreColumns)){
			for(name in ignoreColumns){
				dt[name] = NULL
			}
		}

		if(is.null(allTables)){
			allTables = dt
		} else {
			tryCatch( {allTables = rbind(allTables,dt)} ,
					error = function(e){
						cat("Error in concatenating table ",table,"\n", paste(e), "\n")
					})
		}
	}
	return(allTables)
}

gatherAndSaveData <- function(directory,zipFile,ignoreColumns=NULL,filePattern="^statistics(\\w|-|_)*\\.csv$"){
	cat("Loading data...",date(),"\n")

	dt = gatherAllTables(directory,ignoreColumns,filePattern)

	cat("Data is loaded. Starting compressing. ",date(),"\n")

	write.table(dt, file = gzfile(zipFile))

	cat("Data is compressed and saved. Starting reading back. ",date(),"\n")

	table <- read.table(gzfile(zipFile),header=T)

	cat("Data read back. Done! ",date(),"\n")

	return(table)
}

### return a boolean vector, where each position in respect to x is true if that element appear in y
areInTheSubset <- function(x,y){

	### first consider vector with all FALSE
	result = x!=x
	for(k in y){
		result = result | x==k
	}
	return(result)
}


measureA <- function(a,b){

	if(length(a)==0 & length(b)==0){
		return(0.5)
	} else if(length(a)==0){
		## motivation is that we have no data for "a" but we do for "b".
		## maybe the process generating "a" always fail (eg out of memory)
		return(0)
	} else if(length(b)==0){
		return(1)
	}

	r = rank(c(a,b))
	r1 = sum(r[seq_along(a)])

	m = length(a)
	n = length(b)
	A = (r1/m - (m+1)/2)/n

	return(A)
}