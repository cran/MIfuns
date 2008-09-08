`resample.data.frame` <-
function(x, names, key=NULL, rekey=FALSE, out=NULL, stratify=NULL, seed=0, ...){
set.seed(seed)
dat <- x
if(!is.null(out))out <- sub("/$","",out)
if(is.null(key))dat$resample.rownames <- rownames(dat)
if(is.null(key))key <- "resample.rownames"
if(is.null(stratify))stratify <- rep(0, nrow(dat))
if(is.character(stratify) && all(stratify %in% names(dat)))stratify <- dat[stratify]
if(!is.list(stratify)) stratify <- list(stratify)
stratify <- as.data.frame(stratify)
"%nests%" <- function(a,b)length(unique(interaction(interaction(a),interaction(b))))==length(unique(interaction(b)))
if(!stratify %nests% dat[[key]])stop("key not nested within stratification levels")
ind.key <- dat[[key]][!duplicated(dat[[key]])]
ind.strat <- stratify[!duplicated(dat[[key]]),]
bins <- split(ind.key,f=ind.strat,drop=TRUE)
rowsets <- split(rownames(dat),dat[[key]]) 
doBin <- function(bin){
	  if(length(bin)==1)return(bin)
        return(sample(bin,replace=TRUE))
    }
    doName <- function(name) {
        sample.id <- unlist(sapply(bins,doBin))
        sample.rownames <- rowsets[as.character(sample.id)]
        sample.dataset <- dat[(unlist(sample.rownames)), ]
        if (rekey) 
            sample.dataset[[key]] <- rep(1:length(sample.rownames), 
                times = sapply(sample.rownames, length))
        sample.dataset$resample.rownames <- NULL
        if (is.null(out)) 
            return(sample.dataset)
        write.csv(sample.dataset, file = paste(out, "/", name, 
            ".csv", sep = ""), row.names = FALSE, quote = FALSE)
        return(nrow(sample.dataset))
    }
    invisible(lapply(as.list(as.character(names)), doName))
}

