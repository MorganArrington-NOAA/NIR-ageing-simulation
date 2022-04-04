#install.packages("devtools")
#devtools::install_github("nwfsc-assess/nwfscAgeingError", force = TRUE)
# Load package
library(nwfscAgeingError)

##### Run examples
# File where the Punt et al. (2008) model (pre-compiled in ADMB) resides
SourceFile <- file.path(system.file("executables",
                                    package = "nwfscAgeingError"), .Platform$file.sep)

# This is where all runs will be located
dir <- getwd()
DateFile <- file.path(dir, Sys.Date())
dir.create(DateFile)

##### Generate and run with pcod sample
AgeReads <- read.csv("pcod_doublereads.csv")
AgeReads <- round(AgeReads)
##### Format data
Nreaders <-dim(AgeReads)[2]
# make table formatted as required by ageing error software
Reads2 = data.frame(count=1, AgeReads[1,])
# loop over rows of original data
for(RowI in 2:nrow(AgeReads)){
  DupRow <- NA
  # loop over all previous rows looking for duplicates of the current row
  for(PreviousRowJ in 1:nrow(Reads2)){
    # if all values match, take note of previous row number
    if(all(AgeReads[RowI,1:Nreaders]==
           Reads2[PreviousRowJ,1:Nreaders+1])){
      DupRow = PreviousRowJ
    }
  }
  # if no duplicate found, add new row
  if(is.na(DupRow)){
    # Add new row to ChinaReads2
    Reads2 <- rbind(Reads2, data.frame(count=1, AgeReads[RowI,]))
  }
  # if duplicate found, increment count
  if(!is.na(DupRow)){
    # Increment number of samples for the previous duplicate
    Reads2[DupRow,1] <- Reads2[DupRow,1] + 1
  }
}

Reads2 <- as.matrix(Reads2)

#Reads2[,4] <-Reads2[,2]-Reads2[,3] #might be depricated

### ADMB settings
# Necessary Inputs:
#
# Format data: Data should be formatted with unique reading records as rows and readers/labs as columns (exampling in Table 1).  Specifically, each column corresponds to a reader, readers, lab or labs with a unique reading error and bias; the Punt (2008) model allows for approximately 15 unique columns, so the number of 'readers' must be less than this.  Additionally, an additional column inserted on the left-hand side of the data matrix indicates the number of otoliths with that unique read record; this cell is generally '1', but any instances where two or more otoliths have identical reads for all readers are combined and this cell is incremented.  Any missing entries (i.e., where a reader has not read anything for a given otolith) are indicated with a '-999' in that cell. The model can be configured such that a given column (i.e. reader) has parameter values that 'mirror' the parameter values for a reader to it's left.  This can allow estimation of a model where readers within the same lab are estimated to have the same reading error and bias.  Any instance where a particular reader (or lab) provides multiple reads for a single otolith can be dealt with by creating a 2nd column for that reader, and configuring the model so that parameters for that 2nd column mirror the parameters for the 1st column for that reader. 	Select inputs: The call-function 'FnRun()' in R writes data in the necessary format and then calls the Punt (2008) model.  This model requires several inputs, which are listed and explained below:
#
# Data: This is the data set as previously formatted.  If the data has multiple rows with identical reads, this will cause an error and the 'XXX.rep' file will have a properly formatted data matrix which can be cut-pasted into a 'XXX.dat' file for use. 

# SigOpt: This a vector with one entry for each reader (i.e. Ncol-1 entries).  Each entry specifies the functional form of reading error as a function of true age.  Possible entries include:
#'   '-1', '-2', '-3', etc: This will make this reader mirror the estimated SD from another reader to it's left.  '-1' causes it to mirror the estimated SD for the first reader, etc.  This number has to be lower than the current entry number.
 
# '1' : Constant CV, i.e., a 1 parameter linear relationship of SD with true age.
# '2': Curvilinear SD, i.e., a 3 parameter Hollings-form relationship of SD with true age

# '3': Curvilinear with CV, i.e., a 3-parameter Hollings-form relationship of CV with true age

# '4': No error (but potentially bias)

SigOpt <- c(1,-1)

# BiasOpt: This is a vector with one entry for each reader:
#
# '-1', '-2', '-3': See SigOpt
  
# '0': Unbiased

# '1': Constant CV, i.e., a 1-parameter linear relationship of bias with true age
 
# '2': Curvilinear, i.e., a 2-parameter Hollings-form relationship of bias with true age

BiasOpt <- c(0,0)

# NDataSets: This is generally '1' and other values are not implemented in the current R-code. 
 
# MinAge: The minimum possible 'True' age
MinAge <- 0
 
#  MaxAge: The maximum possible 'True' age
MaxAge <- ceiling(max(Reads2[,-1])/10)*10

#  RefAge: An arbitrarily chosen age from which 'true' age-composition fixed-effects are calculated as an offset.  This has no effect on the answer, but could potentially effect estimation speed

#  MinusAge: The minimum age for which an age-specific age-composition is estimated.  Ages below this MinusAge have 'true' proportion-at-age (Pa) estimated as P_a=P_MinusAge?e^(?(MinusAge-a)), where ? is an estimated log-linear trend in the 'true' proportion-at-age.  If MinusAge = MinAge, ? is not estimated.
 
#  PlusAge: Identical to MinusAge except defining the age above with age-specific age-composition is not estimated.

#  MaxSd: An upper bound on possible values for the standard deviation of reading error
 
#  MaxExpectedAge: Set to MaxAge

#  SaveFile: Directory where 'agemat.exe' is located and where all ADMB intermediate and output files should be located.

#  EffSampleSize: Indicating whether effective sample size should be calculated.  Missing values in the data matrix will cause this to be ineffective, in which case this should be set to '0'

#  Intern: 'TRUE' indicates that ADMB output should be displayed in R; 'FALSE' does not.

KnotAges <- c(NA,NA)
RefAge <- 10
MinusAge <- 0


##### Run the model (MAY TAKE 5-10 MINUTES)
RunFn(Data = Reads2, SigOpt = SigOpt, KnotAges = KnotAges,
      BiasOpt = BiasOpt,
      NDataSets = 1, 
      MinAge = MinAge, 
      MaxAge = MaxAge, 
      RefAge = RefAge,
      MinusAge = MinusAge, 
      PlusAge = 18, 
      SaveFile = DateFile, 
      AdmbFile = SourceFile,
      EffSampleSize = 0, 
      Intern = FALSE, 
      JustWrite = FALSE, 
      CallType = "shell"
)
# Plot output
PlotOutputFn(Data = Reads2, MaxAge = MaxAge,
             SaveFile = DateFile, PlotType = "PDF"
)

example("StepwiseFn")
# Run model selection
# This outputs a series of files
# 1. "Stepwise - Model loop X.txt" --
#   Shows the AIC/BIC/AICc value for all different combinations
#   of parameters arising from changing one parameter at a time
#   according to SearchMat during loop X
# 2. "Stepwise - Record.txt" --
#   The Xth row of IcRecord shows the record of the
#   Information Criterion for all trials in loop X,
#   while the Xth row of StateRecord shows the current selected values
#   for all parameters at the end of loop X
# 3. Standard plots for each loop
# WARNING: One run of this stepwise model building example can take
# 8+ hours, and should be run overnight

##### Stepwise selection

# Define matrix explaining stepwise model selection options
# One row for each reader + 2 rows for
# PlusAge (age where the proportion-at-age begins to
# decrease exponentially with increasing age) and
# MinusAge (the age where the proportion-at-age begins to
# decrease exponentially with decreasing age)
# Each element of a given row is a possible value to search
# across for that reader
SearchMat <- array(NA,
                   dim = c(Nreaders * 2 + 2, 7),
                   dimnames = list(c(paste("Error_Reader", 1:Nreaders),
                                     paste("Bias_Reader", 1:Nreaders), "MinusAge", "PlusAge"),
                                   paste("Option", 1:7))
)
# Readers 1 and 3 search across options 1-3 for ERROR
SearchMat[c(1, 2), 1:3] <- rep(1, 2) %o% c(1, 2, 3)
# Reader 2 mirrors reader 1
SearchMat[2, 1] <- -1
# Reader 4 mirrors reader 3
SearchMat[4, 1] <- -3
# Reader 1 has no BIAS
SearchMat[5, 1] <- 0
# Reader 2 mirrors reader 1
SearchMat[6, 1] <- -1
# Reader 3 search across options 0-2 for BIAS
SearchMat[7, 1:3] <- c(1, 2, 0)
# Reader 4 mirrors reader 3
SearchMat[8, 1] <- -3
# MinusAge searches with a search kernal of -10,-4,-1,+0,+1,+4,+10
SearchMat[9, 1:7] <- c(
  StartMinusAge,
  StartMinusAge - 10,
  StartMinusAge - 4,
  StartMinusAge - 1,
  StartMinusAge + 1,
  StartMinusAge + 4,
  StartMinusAge + 10
)
SearchMat[9, 1:7] <- ifelse(SearchMat[9,1:7] < MinAge,
                            NA, SearchMat[9, 1:7]
)
# PlusAge searches with a search kernal of -10,-4,-1,+0,+1,+4,+10
SearchMat[10, 1:7] <- c(
  StartPlusAge,
  StartPlusAge - 10,
  StartPlusAge - 4,
  StartPlusAge - 1,
  StartPlusAge + 1,
  StartPlusAge + 4,
  StartPlusAge + 10
)
SearchMat[10,1:7] <- ifelse(SearchMat[10, 1:7] > MaxAge,
                            NA, SearchMat[10, 1:7])


# Make SearchMat
SearchMat <- matrix(data = c(1,2,3,4,NA,NA,NA,
               1,2,3,4,NA,NA,NA,
               1,2,0,NA,NA,NA,NA,
               1,2,0,NA,NA,NA,NA,
               20,10,16,19,21,24,30,
               20,10,16,19,21,24,30), nrow = 6, byrow = T)


setwd("C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Puntilizer/")

StepwiseFn(SearchMat = SearchMat, 
           Data = Reads2,
           NDataSets = 1, 
           KnotAges = KnotAges,
           MinAge = MinAge, 
           MaxAge = MaxAge,
           RefAge = RefAge, 
           MaxSd = 40, 
           MaxExpectedAge = MaxAge,
           EffSampleSize = 0,
           SaveFile = DateFile,
           InformationCriterion = c("AIC", "AICc", "BIC")[3],
)
