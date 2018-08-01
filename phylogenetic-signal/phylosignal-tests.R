library(purrr)
library(ape)
library(dplyr)
library(caper)
library(beepr)

source('phylogenetic-signal/helper.r')

# get data
signal_datafiles = list.files('phylogenetic-signal/data/', pattern = "*.btdata", full.names = TRUE)
signal_treefiles = list.files('phylogenetic-signal/data/', pattern = "*.bttrees", full.names = TRUE)
signal_locationfiles = list.files('phylogenetic-signal/data/', 
                                  pattern = "*locations.csv", full.names = TRUE)
length(signal_datafiles) == length(signal_datafiles) 
length(signal_datafiles) == length(signal_locationfiles)
length(signal_datafiles)

# read data
data = map(signal_datafiles, read.table, col.names = c("taxa", "terminology"))
names(data) = basename(signal_datafiles)

trees = map(signal_treefiles, read.nexus)
trees = map(trees, function(t) t[is.rooted.multiPhylo(t)])
trees = map(trees, function(t) 
  t[sample(seq_along(t), size = 1000, replace = FALSE)])
trees = map(trees, function(t) map(t, makeditree))
names(trees) = basename(signal_treefiles)
map(trees, length)

locations = map(signal_locationfiles, read.csv)

# merge data with real labels
kincodes = read.table('phylogenetic-signal/data/kincodes', 
                      sep = ",", col.names = c("code", "name"))
data = map(data, function(x) left_join(x, kincodes, by = c("terminology" = "code")))

# merge with latitude & longitude 
data = map2(data, locations, function(x, y) left_join(x, y, 
                                                      by = c("taxa" = "Name_on_tree_tip")))

# run d-statistic tests
## This sections takes ~1.5 hours to run
# These are permutation tests, so the results will not 
# always be exactly the same

# austronesian
AN = map(trees$austronesian.bttrees, function(tr)
  phylo.d2(data$austronesian.btdata, tr))
AN_mean = apply(simplify2array(AN), 1:2, mean)
write.csv(AN_mean, 'AN.csv')
# bantu
BT = map(trees$bantu.bttrees, function(tr)
  phylo.d2(data$bantu.btdata, tr))
BT_mean = apply(simplify2array(BT), 1:2, mean)
write.csv(BT_mean, 'BT.csv')
# uto-aztecan
UA = map(trees$utoaztecan.bttrees, function(tr)
  phylo.d2(data$utoaztecan.btdata, tr))
UA_mean = apply(simplify2array(UA), 1:2, mean)
write.csv(UA_mean, 'UA.csv')
beep(3)