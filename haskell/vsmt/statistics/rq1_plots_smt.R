library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(scales)

finResultsFile <- "../munged_data/financial_smt.csv"

finData <- read.csv(file=finResultsFile) %>%
  mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>%
  mutate(Algorithm = gsub("-->", "\U27f6", Algorithm), Mean = Time) %>% select(-Time)

finDF <- finData %>% mutate(data = "Fin")

data <- finDF

## VCoreSize > 1 as proxy for non-singleton cases
rq1DF <- data %>% filter(VCoreSize > 1) %>% group_by(Algorithm) %>%
  arrange(Variants)

rq1DFFin <- rq1DF %>% filter(data == "Fin")

breaksRq1 <- function(x) {
  if (max(x) < 17) {
    2^(1:4)
  } else {
    2^(7:10)}
  }

rq1Fin <- ggplot(rq1DFFin) +
  geom_line(aes(x=Variants, y=Mean, color=Algorithm)) +
  geom_point(aes(x=Variants, y=Mean, shape=Algorithm, color=Algorithm),size=3) +
  scale_shape_manual(values = c(1,6,5,17)) +
  theme_classic() +
  scale_x_continuous(breaks=breaksRq1, limits=c(2,NA)) +
  ylab("Time [Sec.] to solve all Variants") +
  theme(legend.position = c(0.08,0.75), axis.text.x = element_text(angle = 90,vjust=0.5,hjust=1))

ggsave("../plots/RQ1_Fin_Smt.png", plot = rq1Fin, height = 4, width = 7, device = "png")
