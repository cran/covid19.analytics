# script for converting CSV and ascii files into RDS


### REQUIRES to download
#
# 1) phylogenetic tree from
#	https://www.ncbi.nlm.nih.gov/labs/virus/vssi/#/precomptree
#
# 2.i) nucleotides CSV file
# 2.ii) proteins CSV file
# 2.iii) nucleotides FASTA file
# 2.iv) proteins FASTA file
#	https://www.ncbi.nlm.nih.gov/labs/virus/vssi/#/virus?SeqType_s=Protein&VirusLineage_ss=SARS-CoV-2,%20taxid:2697049&utm_source=nuccore&utm_medium=referral&utm_campaign=COVID-19



library(ape)

dirSRC='origs'
treeFile=paste0(dirSRC,'/','tree.nwk')
cv19tree <- read.tree(treeFile)
save(cv19tree,file="cv19tree.rds")

###

dirSRC='origs'
tgtFile=paste0(dirSRC,'/','sequences-nucleotides.fasta')
cv19.nucleotides <- read.FASTA(tgtFile)
save(cv19.nucleotides, file='sequences-nucleotides.rds')

###

dirSRC='origs'
tgtFile=paste0(dirSRC,'/','sequences-proteins.fasta')
cv19.proteinss <- read.FASTA(tgtFile)
save(cv19.proteins, file='sequences-proteins.rds')

###

