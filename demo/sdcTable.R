install.packages("sdcTable")
library(sdcTable)

wd <- "../data"
setwd(wd)
file_name <- "data.csv"

microData <- read.csv(file_name, strip.white = TRUE)
microData<-microData[,2:4]
View(microData)
addmargins(xtabs(C~T+E,microData))
addmargins(xtabs(~T+E,microData))


dimT <- sdcHierarchies::hier_create(root = "Tot", nodes = unique(microData[,"T"]))
sdcHierarchies::hier_display(dimT)
dimE <- sdcHierarchies::hier_create(root = "Tot", nodes = unique(microData[,"E"]))
sdcHierarchies::hier_display(dimE)


problem <- makeProblem(microData,
                       dimList = list(T=dimT, E=dimE),
                       dimVarInd = 1:2,
                       numVarInd = 3)

problem

# Minimum frequency 3
frequency <- primarySuppression(problem, type = "freq", maxN = 2)
frequency

# 20%-rule
p20 <- primarySuppression(frequency, type = "p", p = 20, numVarName = "C")
p20

# Secondary suppressions
resGAUSS <- protectTable(p20, method = "GAUSS")
resGAUSS
resHITAS <- protectTable(p20, method = "HITAS")
resHITAS
resOPT <- protectTable(p20, method = "OPT")
resOPT
resHYPERCUBE <- protectTable(p20, method = "HYPERCUBE")
resHYPERCUBE
resSIMPLEHEURISTIC <- protectTable(p20, method = "SIMPLEHEURISTIC")
resSIMPLEHEURISTIC

finalData <- getInfo(resHYPERCUBE, type = "finalData")
finalData

suppressed <- finalData
suppressed[suppressed[,sdcStatus]=="u", "C"] <- NA 
suppressed[suppressed[,sdcStatus]=="x", "C"] <- NA 

xtabs(C~T+E, suppressed)
