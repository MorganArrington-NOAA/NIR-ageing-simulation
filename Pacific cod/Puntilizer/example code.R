install.packages("devtools")
devtools::install_github("nwfsc-assess/nwfscAgeingError", force = TRUE)
# Load package
library(nwfscAgeingError)

##### Run examples
# File where the Punt et al. (2008) model (pre-compiled in ADMB) resides
SourceFile <- file.path(system.file("executables",
                                    package = "nwfscAgeingError"), .Platform$file.sep)

SourceFile <- paste("C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Puntilizer/",sep="")

# This is where all runs will be located
dir <- getwd()
DateFile <- file.path(dir, Sys.Date())
dir.create(DateFile)

##### Generate and run with an artificial dataset
example(SimulatorFn)
utils::write.csv(AgeReads,
                 file = file.path(DateFile, "Simulated_data_example.csv"))

##### Format data
example(RunFn)
##### Run the model (MAY TAKE 5-10 MINUTES)
RunFn(Data = AgeReads2, SigOpt = SigOpt, KnotAges = KnotAges,
      BiasOpt = BiasOpt,
      NDataSets = 1, MinAge = MinAge, MaxAge = MaxAge, RefAge = 10,
      MinusAge = 1, PlusAge = 30, SaveFile = DateFile, AdmbFile = SourceFile,
      EffSampleSize = 0, Intern = FALSE, JustWrite = FALSE, CallType = "shell"
)
# Plot output
PlotOutputFn(Data = AgeReads2, MaxAge = MaxAge,
             SaveFile = DateFile, PlotType = "PDF"
)

example(StepwiseFn)
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
StepwiseFn(SearchMat = SearchMat, Data = AgeReads2,
           NDataSets = 1, MinAge = MinAge, MaxAge = MaxAge,
           RefAge = 10, MaxSd = 40, MaxExpectedAge = MaxAge+10,
           SaveFile = DateFile,
           InformationCriterion = c("AIC", "AICc", "BIC")[3]
)