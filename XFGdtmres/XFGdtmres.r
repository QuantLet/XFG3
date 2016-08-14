source('analysisFunctions')

## RESULTS LOADER

## PARAMETERS

## xaxis labels
start 	= '2012-05-18'
end 	= '2016-07-22'
axisLab = seq(as.POSIXct(start), as.POSIXct(end), by = "2 weeks")

## Number of time slices and number of topics
ntimes 	= 110
ntopics = 50

## Input path and output path
filePath 	= 'bitcointalk'
outputPath 	= 'output'


## Creates vec of topic ids for loading results
topics 	= sapply(0:(ntopics - 1), function(x) {
			if (x < 100) {
				if (x < 10) {return(paste('00', toString(x), sep = ''))
				} else {return(paste('0', toString(x), sep = ''))}
			} else {return(toString(x))}
		})
	
## Load vocab
vocab 	= read.delim(paste(filePath, "/vocabulary.dat", sep = ''), sep = '\n', header = FALSE)[,1]

## Load the results into list of length ntopics of word/time matrices
## tl[[topicNumber]] returns a matrix of size V x ntimes
tl 		= lapply(topics, function(topic) {
			d = scan(paste(filePath, "/", outputPath, "/lda-seq/topic-", toString(topic), "-var-e-log-prob.dat", sep=''))
			f = matrix(d, ncol=ntimes, byrow = TRUE)
			rownames(f) <- 1:length(vocab)
			return(f)
			}
		)
		

	
## Load the topic document proportions
## Gives a matrix of size Documents x topics, documents are ordered by time
a = scan(paste(filePath, "/", outputPath, "/lda-seq/gam.dat", sep = ""))
b = matrix(a, ncol = ntopics, byrow = TRUE)
rs = rowSums(b)
gamma = b / rs
rownames(gamma) = 1:dim(gamma)[1]

## Load the time slices
times = read.table(paste(filePath, "/-seq.dat", sep = ""), sep = "\n")
times = c(times)[[1]]
times = times[2:length(times)]
	

