## as.recursive.list: transform a formula as a list of 
formula.as.recursive.list <- function(l) if(length(l)>1) lapply(as.list(l),formula.as.recursive.list) else l

## replace 1 to .One. #todo: more general action
recursive.list.as.call <- function(l) {
	if(length(l)==1 || class(l) == "call") 
		if(l == 1)
			#as.call(list(as.name("I"),1)) 
			as.name(".One.")
		else
			l 
	else  
		as.call(c(l[[1]],lapply(l[-1],recursive.list.as.call)))
}

## replace 1 to .One. #todo: more general action
recursive.list.as.formula <- function(l) eval(recursive.list.as.call(l))

## The goal is to onvert y~(1+x):z as y~z+x:z (when originally R do y~x:z)
## This is done by replacing 1 with ".One.", i.e. y~(.One. + x):z which is equivalent to
## y~.One.:z+x:z (expected) and then simplify .One. as y~z+x:z

formula.fix.one <- function(form) {
	form <- recursive.list.as.formula(formula.as.recursive.list(form))
	form.terms <- attributes(terms(form))
	## build terms of new formula as character
	formCh <- c()
	if(form.terms$response) formCh <- c(formCh,as.character(form.terms$variables[[2]]))
	formCh <- c(formCh,"~")
	tmp <- colnames(form.terms$factors)
	tmp <- gsub("\\.One\\.\\:","",tmp)
	tmp <- gsub("\\:\\.One\\.","",tmp)
	formCh <- c(formCh,paste(unique(unlist(lapply(tmp,function(e) if(e == ".One.") NULL else e))),collapse=" + "))
	formCh <- paste(formCh,collapse="")
	eval(parse(text=formCh))
}

