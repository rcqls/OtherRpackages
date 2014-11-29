## as.recursive.list: transform a formula as a list of 
formula.as.recursive.list <- function(l) if(length(l)>1) lapply(as.list(l),formula.as.recursive.list) else l

## 
recursive.list.as.call <- function(l,replace=TRUE) {
	if(length(l)==1 || class(l) == "call") 
		if(l == 1)
			#as.call(list(as.name("I"),1)) 
			as.name(".One.")
		else
			l 
	else  
		as.call(c(l[[1]],lapply(l[-1],recursive.list.as.call)))
}

## 
recursive.list.as.formula <- function(l) eval(recursive.list.as.call(l))

## 

cqls.formula <- function(form) {
	form <- recursive.list.as.formula(formula.as.recursive.list(form))
	form.terms <- attributes(terms(form))
	## build terms of new formula as character
	formCh <- c()
	if(form.terms$response) formCh <- c(formCh,as.character(form.terms$variables[[2]]))
	formCh <- c(formCh,"~")
	tmp <- colnames(form.terms$factors)
	tmp <- gsub("\\.One\\.\\:","",tmp)
	tmp <- gsub("\\:\\.One\\.","",tmp)
	formCh <- c(formCh,paste(unlist(lapply(tmp,function(e) if(e == ".One.") NULL else e)),collapse=" + "))
	formCh <- paste(formCh,collapse="")
	eval(parse(text=formCh))
}

