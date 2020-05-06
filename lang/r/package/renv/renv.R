
library(renv)

setwd("~/Dropbox/repos/github/five-dots/notes/lang/r/package/renv")

## Start renv
renv::init()
renv::status()

renv::load()

## Check libPaths
.libPaths()
renv::paths$root()
renv::paths$cache()
renv::paths$library()

## Update renv.lock
renv::snapshot(confirm = FALSE)

## Delete unused link
renv::clean(confirm = FALSE)

renv::restore()
renv::modify()
renv::history()
renv::revert()
?renv::config

## Update renv package itself
renv::upgrade(version = "master")


renv::install("dplyr@0.8.4")
library(dplyr)
library(tidyverse)
library(ccgarch)

packageVersion("dplyr")
renv::remove("dplyr@0.8.3")
renv::purge()

Sys.getenv("RENV_PATHS_ROOT")
