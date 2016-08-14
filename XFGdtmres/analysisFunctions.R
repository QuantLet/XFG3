

## Jensen Shannon divergence
JS = function(p, q) {
	m 	= 0.5 * (p + q)
	Js 	= 0.5 * (sum(p * log(p / m)) + sum(q * log(q / m)))
	return(Js)
}

## TfIdf inspired term score
termScore = function(id, topic, time) {
	inv = 1
	for (t in 1:ntopics) {
		inv = inv * exp(tl[[t]][id,time])
	}
	b 	= exp(tl[[topic]][id,time])
	termw 	= b * log(b/(inv^(1/ntopics)))
	return(termw)
}

## Helper to convert time in string format to timeslice number
dateToSlice = function(strTime) {
	return(which(as.POSIXct(strTime) < axisLab)[1])
}

## Helper to convert timeslice number to time in string format
sliceToDate = function(slice) {
	return(axisLab[slice])
}
	

## Returns a vector of size ntimes of normalized time slice topic document proportions
topicProp = function(topic) {
		
	timeDivided = list()
	current = 1 

	for (x in 1:ntimes) {
		if (!is.na(times[x])) {
			next1 = current + times[x] - 1
			timeDivided[[x]] = gamma[current:next1,topi]
			current = current + times[x] + - 1
		} else {
			timeDivided[[x]] = 0
		}
	}
	timeAverage = c()
	for (x in 1:ntimes) {
		timeAverage = c(timeAverage, mean(timeDivided[[x]], trim = 0))
	}
	return(timeAverage)
}


## Returns vector of length ndocs of document ids which exhibit highest topic 
## probabilities
topDocTopic = function(topic, ndocs) {
	a = sort(gamma[,topic], decreasing = TRUE) 
	return(a[1:ndocs])
}

	
	
## Returns top nwords from topic topic in time time using termScore ranking
termTime = function(topic, time, nwords) {
	termsRanked = sapply(1:length(vocab), function(termId) {return(termScore(termId, topic, time))})
	t1 			= sort(termsRanked, decreasing = TRUE)
	return(c(vocab[as.integer(names(t1[1:nwords]))]))
}


## Returns evolution of top nwords for topic topic
termTimes = function(topic, nwords) {
	ws = list()
	for (time in 1:ntimes) {
		ws[[time]] = termTime(topic, time, nwords)
	}
	return(ws)
}


## Returns evolution of a given word in a given topic
## Choice of matching word exactly or with substring
termEvo = function(term, topic, substr = FALSE) {
	if (substr == TRUE) {
		termIds = which(grepl(term, vocab))
		evo 	= c()
		for (time in 1:ntimes) {
			evo = c(evo, sum(sapply(1:length(termIds), 
							function(x) {
								return(termScore(x, topic, time))
							})))
		}
		return(evo)
	}
	termId	= which(term == vocab)
	evo 	= c()
	for (time in 1:ntimes) {
		evo = c(evo, termScore(termId, topic, time))
	}
	return(evo)
}


## Returns the topic for which the term achieves maximum weight
## Choice of exact term or substring
maxWeight = function(term, substr = FALSE) {
	termId = which(term == vocab)
	top = c()
	for (topic in 1:ntopics) {
		top = c(top, sum(termEvo(term, topic, substr)))
	}
	a   = tail(sort(top), 1)
	return(which(a == top))
}



## Return vector timeslice to timeslice JS divergence for topic topic
seqJS = function(topic) {
	JsVec = c()
	for (t in 1:(ntimes - 1)) {
		distr1 = exp(tl[[topic]][,t])
		distr2 = exp(tl[[topic]][, t + 1])
		Js 	   = JS(distr1, distr2)
		JsVec  = c(JsVec, Js)
	}	
	return(JsVec)
}

## Return timeslices that go over a certain threshold on Jenson Shannon divergence
JSthreshold = function(threshold) {
	slices = c()
	for (topic in ntopics) {
		change = seqJS(topic)
		slices = c(slices, which(change > threshold))
	}
	return(slices)
}


