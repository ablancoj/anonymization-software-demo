library(sdcTable)

wd <- "../data"
setwd(wd)
file_name <- "synthdata.csv"


microData <- read.csv(file_name, strip.white = TRUE)
residence.tax <- read.csv("ca_prov.csv", strip.white = TRUE)
cob.tax <- read.csv("country_tax.csv", strip.white = TRUE, sep=";")

addmargins(xtabs(income ~ residence + country.birth, microData))
addmargins(xtabs( ~ residence + country.birth, microData))

addmargins(xtabs(income ~ residence + sex, microData))
addmargins(xtabs( ~ residence + sex, microData))

dim.sex <- sdcHierarchies::hier_create(root = "Sum", nodes = unique(microData$sex))
sdcHierarchies::hier_display(dim.sex)

dim.residence <- sdcHierarchies::hier_create(root = "Sum", nodes = unique(microData$residence))
dim.residence <- sdcHierarchies::hier_create(root = "Sum", nodes = unique(residence.tax$CA))
for (ca in unique(residence.tax$CA)) {
  dim.residence <- sdcHierarchies::hier_add(dim.residence, root = ca, nodes = residence.tax[residence.tax[,"CA"]==ca,"Prov"])
}
sdcHierarchies::hier_display(dim.residence)

dim.country.birth <- sdcHierarchies::hier_create(root = "Sum", nodes = unique(microData$country.birth))
dim.country.birth <- sdcHierarchies::hier_create(root = "Sum", nodes = unique(cob.tax$L0))
for (l0 in unique(cob.tax$L0)) {
  dim.country.birth <- sdcHierarchies::hier_add(dim.country.birth, root = l0, nodes = unique(cob.tax[cob.tax[,"L0"]==l0,"L1"]))
  for (l1 in unique(cob.tax[cob.tax[,"L0"]==l0,"L1"])) {
    dim.country.birth <- sdcHierarchies::hier_add(dim.country.birth, root = l1, nodes = unique(cob.tax[cob.tax[,"L1"]==l1,"L2"]))
    for (l2 in unique(cob.tax[cob.tax[,"L1"]==l1,"L2"])) {
      dim.country.birth <- sdcHierarchies::hier_add(dim.country.birth, root = l2, nodes = unique(cob.tax[cob.tax[,"L2"]==l2,"L3"]))
    }
  }
}
sdcHierarchies::hier_display(dim.country.birth)

problem <- makeProblem(microData,
                       dimList = list(residence = dim.residence, country.birth = dim.country.birth),
                       dimVarInd = c(3,4),
                       numVarInd = 5)

createArgusInput(problem,
                 method="OPT",
                 primSuppRules = list(list(type="freq", n=3, rg=10), list(type="p", n=5, p=20)))

# Minimum frequency 3
frequency <- primarySuppression(problem, type = "freq", maxN = 2)
frequency

# 20%-rule
p20 <- primarySuppression(frequency, type = "p", p = 20, numVarName = "income")
p20

# (2,75)-dominance rule
dominance <- primarySuppression(p20, type = "nk", n = 2, k = 75, numVarName = "income")
dominance

resOPT <- protectTable(p20, method = "OPT")
resOPT

finalData <- getInfo(resOPT, type = "finalData")
finalData

suppressed <- finalData
suppressed[suppressed[,sdcStatus]=="u", "income"] <- NA 
suppressed[suppressed[,sdcStatus]=="x", "income"] <- NA 

View(xtabs(income~sex+country.birth, suppressed))
