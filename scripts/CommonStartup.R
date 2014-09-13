library(nat)
library(flycircuit)
library(nat.flybrains)
library(brew)
library(xtable)
library(dendroextras)

# Define a function for a frontal view of the brain
frontalView<-function(zoom=0.6){
  um=structure(c(1, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1), .Dim = c(4L, 4L))
  rgl.viewpoint(userMatrix=um,zoom=zoom)
}

# Define a function to find dendrogram cutting height
height_for_ngroups <- function(hc, k) {
  s <- sort(hc$height, decreasing=TRUE)
  s[k]+1e-6
}

# Overwrite RGL's inRows function to reduce the number of digits from 7 to 4, reducing file size considerably
inRows <- function(values, perrow, leadin="\t", digits=4) {
  if (is.matrix(values)) values <- t(values)
  values <- c(values)
  if (is.numeric(values)) values <- formatC(values, digits = digits, width = 1)
  len <- length(values)
  if (len%%perrow != 0) values <- c(values, rep("PADDING", perrow - len%%perrow))
  values <- matrix(values, ncol = perrow, byrow = TRUE)
  lines <- paste(leadin, apply(values, 1, function(row) paste(row, collapse = ", ")))
  lines[length(lines)] <- gsub(", PADDING", "", lines[length(lines)])
  paste(lines, collapse = ",\n")
}

# Load affinity propagation results
apres16k.p0 <- load_si_data("apres16k.p0.rds")
apresdf <- as.data.frame(apres16k.p0)
exemplars <- levels(apresdf$exemplar)

# Load neuron database for converting FlyCircuit identifiers
load_fcdb('neuron')

# Load spatial distribution information
load_si_data('spatdist_jfrc.rda')

# Load dps object for plotting neurons
dps <- read.neuronlistfh("http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/flycircuit/dpscanon_f9dc90ce5b2ffb74af37db1e3a2cb35b.rds", localdir=getOption('flycircuit.datadir'))
dps <- read.neurons(dps)

# Attach the all-by-all score matrix and load into memory
allbyall <- fc_attach_bigmat("allbyallblastcanon_f9dc90ce5b2ffb74af37db1e3a2cb35b")
allbyallmem <- allbyall[, ]

# Cluster all the exemplars
exemhc <- hclustfc(exemplars, scoremat=allbyallmem)
exemhc$height <- sqrt(exemhc$height)

# Select a few neurons from central brain group that were OL.
olneuron <- c("FruMARCM-F000057_seg001", "TPHMARCM-F001806_seg001", "THMARCM-466F_seg2", "DvGlutMARCM-F003807_seg001", "DvGlutMARCM-F002964_seg001", "FruMARCM-F001388_seg001", "TPHMARCM-1126M_seg1", "TPHMARCM-1098F_seg1", "DvGlutMARCM-F1088_seg1", "DvGlutMARCM-F004312_seg001", "FruMARCM-M002003_seg001")

# Collect central brain neurons: groups 2 and 3
excb <- subset(exemhc, k=3, groups=c(2, 3))
excb <- setdiff(excb, olneuron)

# Select a few neurons from central brain group that were OL
exol=subset(exemhc, k=3, groups=1)
olneuron <- c("FruMARCM-F000057_seg001", "TPHMARCM-F001806_seg001", "THMARCM-466F_seg2", "DvGlutMARCM-F003807_seg001", "DvGlutMARCM-F002964_seg001","FruMARCM-F001388_seg001", "TPHMARCM-1126M_seg1", "TPHMARCM-1098F_seg1", "DvGlutMARCM-F1088_seg1", "DvGlutMARCM-F004312_seg001", "FruMARCM-M002003_seg001")
exol <- c(exol, olneuron)
oln <- subset(apresdf, exemplar%in%exol)$item
# Divide optic lobe neurons into VPNs and intrinsic.
# Define optic lobe neuropils
oll <- c("LO_L", "LOP_L", "ME_L", "AME_L")
olr <- c("LO_R", "LOP_R", "ME_R", "AME_R")
ol <- c(oll,olr)
allneuropils <- getRegionsFromSurf(FCWBNP.surf)
centralbrain <- setdiff(allneuropils, c(oll, olr))
x <- subset(spatdist_jfrc, rownames(spatdist_jfrc)%in%oln)
vpns <- rownames(x)[rowSums(x[,centralbrain]) > 2]

# Add VPN exemplars
exvpns <- unique(subset(apresdf, item %in% vpns)$exemplar)
exnotol <- union(exvpns, excb)

# Cluster again
exnotolhc <- hclustfc(exnotol, unsquare=T, scoremat=allbyallmem)
exemplars <- exnotol


# Define 19 superclusters
num_superclusters <- 19
exemplars_by_supercluster <- sapply(1:num_superclusters, function(x) subset(exnotolhc, k=num_superclusters, groups=x))

# Open an RGL window ready for plotting
open3d()
par3d(windowRect=c(150,150,150+1024,150+768))

# Set clustering version
cluster_version <- 4
