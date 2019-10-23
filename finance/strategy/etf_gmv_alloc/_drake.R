
## system("Rscript ./R/data.R")
source("R/packages.R")
source("R/functions.R")
source("R/plan.R")

drake_config(
  plan
  ## parallelism = "clustermq",
  ## jobs = 16
)
