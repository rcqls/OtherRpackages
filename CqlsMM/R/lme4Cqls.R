## In the CqlsMM interface, test random effect
is.random.factor <- function(vn) {
    charToRaw("A") <= (charToRaw(substring(vn,1,1))->tmp) & tmp <= charToRaw("Z")
}

## data.frame with tolower variable names

data.frame.CqlsMM <- function(formula,data) {
    vars.form <- all.vars(formula)
    ## Maybe find recursively until globalenv()
    # if(missing(data)) {
    #     varnames <- c(ls(parent.frame()),globalenv())
    #     vars<- varnames[tolower(substring(varnames,1,1)) %in% letters] #has to start with letter
    # } else 
    vars <- names(data)
    # # dictionary
    # dict <- vars[match(tolower(vars.form),tolower(vars))]
    # names(dict) <- vars.form
    df <- data[match(tolower(vars.form),tolower(vars))]
    names(df) <- vars.form
    attr(df,"has.random.factor") <- any(sapply(vars.form,is.random.factor))
    df  
}

## Conversion of formula taking into account 
## lower (fixed effect) and upper (random effect) variables

formula.CqlsMM <- function(formula) {
    # terms
    terms <- terms(formula)
    
    # create terms random matrix (like attr(terms,"factors"))
    nc <- ncol(attr(terms,"factors"))
    attr(terms,"random") <- t(sapply(rownames(attr(terms,"factors")),function(fa) {
        is.random <- any(sapply(all.vars(parse(text=fa)),is.random.factor))
        rep(is.random,nc)
    }))

    dimnames(attr(terms,"random")) <- dimnames(attr(terms,"factors"))

    # create lmer formula
    # fixed effects
    fixed.factors.terms <-  (!attr(terms,"random"))*attr(terms,"factors")
    random.factors.terms <-  attr(terms,"random")*attr(terms,"factors")
    which.random.factors <- apply(random.factors.terms,2,sum)>0
    rownames <- rownames(attr(terms,"factors"))

    formula.terms <- sapply(seq(which.random.factors),function(i) {
        if(which.random.factors[i]) {
            fixed <- paste(rownames[fixed.factors.terms[,i]>0],sep=":")
            random <- paste(rownames[random.factors.terms[,i]>0],sep=":")
            if(length(fixed)==0) fixed<- 1 else fixed <- paste0(fixed,"+0")
            paste0("(" ,fixed,"|",random,")")
        } else {
            attr(terms,"term.labels")[i]
        }
    })

    formula.ch <- ""
    if(attr(terms,"response")) formula.ch <- paste0(formula.ch,rownames[1])

    formula.ch <- paste0(formula.ch,"~")
    
    if(!attr(terms,"intercept")) formula.ch <- paste0(formula.ch,"0 + ")

    formula.ch <- paste0(formula.ch,paste(formula.terms,collapse=" + ")) 

    formula.old <- formula

    formula <- as.formula(formula.ch)
    ## print(list(df=df,term=terms,formula=formula,formula.old=formula.old))
    
    formula
}

## lmm.cqls interface including both lm and lmer
lmm.cqls <- function(formula,data,...) {
    ## apply fix one: y ~ (1 + z1 + Z2) + (1 + z1 + Z2)*x
    form <- formula.fix.one(formula)
    df <- data.frame.CqlsMM(form,data)
    if(attr(df,"has.random.factor")) {
        require(lme4)
        form <- formula.CqlsMM(form)
        lmer(form,df,...)
    } else lm(form,df,...)
}

## lmer.cqls interface
lmer.cqls <- function(formula,data,...) {
    require(lme4)
    ## apply fix one: y ~ (1 + z1 + Z2) + (1 + z1 + Z2)*x
    form <- formula.fix.one(formula)
    df <- data.frame.CqlsMM(form,data)
    ## apply CqlsMM interface
    form <- formula.CqlsMM(form)
    lmer(form,df,...)
}

## lm.cqls interface only

lm.cqls <- function(formula,...) {
    lm(formula.fix.one(formula),...)
}

