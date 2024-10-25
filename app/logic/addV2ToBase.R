box::use(
  IlluminaHumanMethylation450kanno.ilmn12.hg19[],
  IlluminaHumanMethylation450kmanifest[],
  minfi[],
  modelTsne[],
  ComplexHeatmap[],
  IlluminaHumanMethylationEPICmanifest[],
  dplyr[],
  sesame[],
  ExperimentHub[],
)

#' @export
addV12ToBase <- function(annotation, rawdata) {


  # library(IlluminaHumanMethylation450kanno.ilmn12.hg19)
  # library(IlluminaHumanMethylation450kmanifest)
  # library(minfi)
  # library(ComplexHeatmap)
  # library(IlluminaHumanMethylationEPICmanifest)
  # library(dplyr)

  betas <- openSesame(rawdata, platform = "EPICv2")
  special_samples <- colnames(betas)
  puce_samples <- colnames(betas)

  # Transform EPICV2 into a 850K dataframe
  betas <- mLiftOver(x = betas, target_platform = "EPIC")
  betas <- as.data.frame(betas)

  # Ensure both datasets have the same CpGs
  # Necessary when different CHIP types are mixed
  betas <- betas %>%
    filter(row.names(betas) %in% rownames(BValsC))

  # Combine both dataframes: the one with EPIC and 450K, and the EPICV2 transformed into 450K
  #betas <- cbind(BValsC, betas)
  # fixing bug : seems to work with merge, I think that mLiftOver mess with the order of rownames somehow
  betas <- merge(BValsC ,betas, by=0, all=TRUE)


}












