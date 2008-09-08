#User script to plot structural/statistical diagnostic plots and/or 
#plots to evaluate covariate-parameter relationships and/or
#any plots user would like to create with their own code


# Important variables that are available for plot script use include:

# ProjectDir - Main project directory that contains NONMEM table files (directory referenced in call to NONR)
# cont.cov - if defined in NONR
# cat.cov - if defined in NONR
# par.list - if defined in NONR
# eta.list - if defined in NONR
# i  - ctl stream number 


# Define Table File, Parameter File, and Data File name then
# import the files
TabFileName<-paste(ProjectDir,"/",i,".TAB",sep="") # *.TAB file defined in $TABLE record
ParFileName<-paste(ProjectDir,"/",i,"par.TAB",sep="") # *par.TAB file defined in $TABLE record
runno <- i
# block of code below gets data file name out of NONMNEM control stream.
ndir <- paste(ProjectDir, "/", i, sep = "")
ctl1 <- paste(ndir, "/", i, ".ctl", sep = "")
ab <- "^\\$DATA +([^ ]+).*$"
ab1 <- scan(file = ctl1, what = "", comment.char = "", allowEscapes = T,
       sep = "\n", quiet = T)
ab2 <- grep("\\$DATA", ab1, value = T)
DataFileNm <- sub(ab, "\\1", ab2)

# end of code block to get NONMEM data file name
setwd(ndir)
DataFileName<-paste(DataFileNm) # data file used for NONMEM execution

# Check if NONMEM run completed by checking for *.TAB file. If it did, then move on, otherwise stop
# and provide an error messgae
# Check for *.TAB run file then 
# read in if it exists else return error message.
if(file.exists(TabFileName)=="TRUE"){
data<-read.table(TabFileName,skip=1,header=TRUE,as.is=T)

dataObs<-data[data$EVID==0,] # limit to observations only.  Could substitute WRES==0 if no EVID in data set

# Read Dataset, remove commented rows, and create 
# additional file with one line/individual:
dataset<-read.table(file=DataFileName,header=TRUE, sep=",", as.is=T)
dataNM<-dataset[dataset$C != "C",]


numDV<-c("DV","LDV","AMT")  # vector of variable names that should be numeric in dataNM
# loop to change variables in numDV to numeric
for(k in numDV){
	dataNM[[k]]<-as.numeric(as.character(dataNM[[k]]))
}

DataNMi<-dataNM[!duplicated(dataNM$ID),]

#Read in *par.TAB file from NONMEM run
dataPar<-read.table(ParFileName,skip=1,header=TRUE,as.is=T)
Parind <- dataPar[!duplicated(dataPar$ID),]

# header row in NONMEM data file indicates what dataset variables are available for plotting from dataNM
# covariates can be taken from dataNMi assumming one covariate measure per individual (not time related)
# $TABLE record in control stream indicates what variables are available in dataObs (*.TAB) and Parind (*par.TAB) for plotting

# add plotting code below before "}else" to generate relevant graphs


pdfname<-paste(ProjectDir,"/TestPlots_",i,".pdf",sep=""); # user supplied name of output pdf file
pdf(file=pdfname);

# The plotting code below can be as complicated or as simple as is required. The first few examples below are relatiovely simple and
# the last example is more complex.

# plot of observed PK vs Time based on Nonmem data set
plot(dataNM$TIME, dataNM$DV, main="PK obs", xlab="Time",
     ylab="PK measure")
     
# histogram of ETA1 based on Nonmem table file (XXpar.TAB)       
hist(Parind$ETA1, main="Histogram of ETA1")


dev.off()

}else

{cat (paste("NO *.TAB FILE EXISTS FOR RUN ",runno,"\n",sep=""))} 


