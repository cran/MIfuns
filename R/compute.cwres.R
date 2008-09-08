`compute.cwres` <-
function (run.number=1, tab.prefix = "cwtab", sim.suffix = "", 
    est.tab.suffix = ".est", deriv.tab.suffix = ".deriv", old.file.convention = FALSE, 
    id = "ALL", printToOutfile = TRUE, onlyNonZero = TRUE, ...) 
{
    out.file = paste(tab.prefix, run.number, sim.suffix, sep = "")
    full.dataset <- read.cwres.data(out.file, old.file.convention = old.file.convention, 
        est.tab.suffix = est.tab.suffix, deriv.tab.suffix = deriv.tab.suffix, 
        ...)
    if (is.null(full.dataset)) {
        return()
    }
    num.reps <- length(full.dataset)
    tot.cwres <- c()
    for (rep in 1:num.reps) {
        dataset <- full.dataset[[rep]]
        first.only.data <- dataset@data[!duplicated(dataset@data$ID), 
            ]
        all.etas <- dataset@eta
        all.etas <- cbind(first.only.data["ID"], all.etas)
        OMEGA <- as.matrix(dataset@omega)
        SIGMA <- as.matrix(dataset@sigma)
        H.names = c()
        i = 1
        while (i < (length(dataset@sigma) + 1)) {
            H.names = c(H.names, paste("H", i, "1", sep = ""))
            i = i + 1
        }
        G.names = c()
        i = 1
        while (i < (length(dataset@omega) + 1)) {
            G.names = c(G.names, paste("G", i, "1", sep = ""))
            i = i + 1
        }
        if (id == "ALL") {
            id.vals <- unique(dataset@data$ID)
            CWRES <- c()
            for (i in id.vals) {
                ind.data <- subset(dataset@data, ID == i)
                ind.etas <- t(as.matrix(all.etas[all.etas$ID == 
                  i, colnames(all.etas) != "ID"]))
                CWRESI <- ind.cwres(ind.data, H.names, G.names, 
                  OMEGA, SIGMA, ind.etas, ...)
                CWRES <- c(CWRES, CWRESI)
            }
            CWRES <- as.matrix(CWRES)
            if (printToOutfile == TRUE) {
                if (old.file.convention) {
                  filename <- paste(out.file, ".cwres", sep = "")
                }
                else {
                  filename <- out.file
                }
                data.cwres <- data.frame("ID"=dataset@data$ID)
          if(!is.null(dataset@data$MDV)) data.cwres$MDV=dataset@data$MDV
          if(!is.null(dataset@data$DV)) data.cwres$DV=dataset@data$DV
          if(!is.null(dataset@data$IPRE)) data.cwres$IPRE=dataset@data$IPRE
          if(!is.null(dataset@data$WRES)) data.cwres$WRES=dataset@data$WRES
          if(!is.null(CWRES)) data.cwres$CWRES=CWRES
          #tmp <- installed.packages(priority = "NA")
          #      if (length(grep("xpose4", tmp)) > 0) {
          #        xpose.version <- tmp["xpose4", "Version"]
          #        xpose.text <- paste("from Xpose version", xpose.version, 
          #          sep = " ")
          #      }
          #      else {
                  xpose.text <- "from Xpose 4.0-6.1"
          #      }
                if (rep == 1) {
                  append.table.message <- FALSE
                }
                else {
                  append.table.message <- TRUE
                }
                cat(paste("TABLE for CWRES computed using compute.cwres.R", 
                  xpose.text, "on", format(Sys.time(), "%a %b %d, %X, %Y"), 
                  "\n"), file = filename, append = append.table.message)
                newdata <- format(data.cwres, sci = TRUE)
                suppressWarnings(write.table(newdata, filename, 
                  row.names = FALSE, sep = " ", quote = FALSE, 
                  append = TRUE))
            }
            if (onlyNonZero == TRUE) {
                if (is.null(dataset@data$MDV)) {
                }
                else {
                  data.cwres <- cbind(dataset@data, CWRES)
                  tmp <- subset(data.cwres, MDV == 0)
                  CWRES <- tmp$CWRES
                }
            }
        }
        else {
            data1 <- dataset@data[dataset@data$ID == id, ]
            ind.etas <- t(as.matrix(all.etas[all.etas$ID == id, 
                colnames(all.etas) != "ID"]))
            CWRES <- ind.cwres(data1, H.names, G.names, OMEGA, 
                SIGMA, ind.etas, ...)
            if (onlyNonZero == TRUE) {
                if (is.null(data1$MDV)) {
                }
                else {
                  data1.cwres <- cbind(data1, CWRES)
                  tmp <- subset(data1.cwres, MDV == 0)
                  CWRES <- tmp$CWRES
                }
            }
        }
        tot.cwres <- c(tot.cwres, CWRES)
    }
    return(tot.cwres)
}

