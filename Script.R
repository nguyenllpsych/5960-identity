#######################################################
##    PSY 5960: Identity - Personality - Wellbeing   ##
##                Linh Nguyen                        ##
##            Created: 13-Nov-2020                   ##
##         Last updated: 04-Dec-2020                 ##
##                                                   ##
## UPDATES: analyses on full data                    ##
##                                                   ##
## FILES NEEDED IN WORKING DIRECTORY:                ##
##   (1) SCORE Data PSY 5960.csv                     ##
##   (2) SCORE Dictionary PSY 5960.csv               ##
##   (3) renv.lock                                   ##
##                                                   ##
## TO NAVIGATE: Edit - Folding - Collapse All        ##
#######################################################

# META ====
# > Libraries ----
library(tidyverse)
library(codebook)
library(apaTables)
library(profileR)
library(pwr)
library(renv)
renv::restore() #ensuring packages are at the same version.
#if there's an error in running renv::restore(), just don't run this line and let me know

# > Data ----
# >> simulated data for pre-reg ----
data <- read.csv(file = "./Data/SCORE Data PSY 5960.csv") #remove ./Data/ if file in current directory
dict <- read.csv(file = "./Data/SCORE Dictionary PSY 5960.csv") #remove ./Data/ if file in current directory

# CLEANING ====
# > basic cleaning ----
## Recode no/yes -> 1/2 
data$liveus <- as.factor(data$liveus)
data$usborn <- as.factor(data$usborn)
data$liveus <- plyr::revalue(data$liveus, c("1" = "0","2" = "1"))
data$usborn <- plyr::revalue(data$usborn, c("1" = "0","2" = "1"))
data$liveus <- as.numeric(as.character(data$liveus))
data$usborn <- as.numeric(as.character(data$usborn))

## Make sure variable types are correct 
names <- dict %>% 
  filter(type == "factor") %>% 
  pull(variable)
data[,names] <- 
  lapply(data[,names], as.numeric) #factor variables are coded as numeric for codebook purposes

rm(names)

## Variable labels
var_label(data) <- dict %>% 
  dplyr::select(variable, label) %>% 
  dict_to_list()
data$age <- as.numeric(data$age)
data[135,]$age <- 22 #said 0.22 but should be 22

## Add ID 
data$ID <- rep(1:1260)
data$ID <- as.character(data$ID)

## Value labels 0 no 1 yes 
likert <- dict %>% 
  filter (value_labels == "0 no 1 yes") %>%
  pull(variable)
add_likert <- function(x) {
  val_labels(x) <- c("No" = 0,
                     "Yes" = 1)
  x
}
data <- data %>%
  mutate_at(likert, 
            add_likert)

## Value labels 0-5 none of the time - all of the time 
likert <- dict %>% 
  filter (value_labels == "1 none of the time 2 a little of the time 3 some of the time 4 most of the time 5 all of the time") %>%
  pull(variable)
add_likert <- function(x) {
  val_labels(x) <- c("none of the time" = 1,
                     "a little of the time" = 2,
                     "some of the time" = 3,
                     "most of the time" = 4,
                     "all of the time" = 5)
  x
}
data <- data %>%
  mutate_at(likert, 
            add_likert)

## Value labels 1-4 strongly disagree - strongly agree 
likert <- dict %>% 
  filter (value_labels == "1 strongly disagree 2 disagree 3 agree 4 strongly agree") %>%
  pull(variable)
add_likert <- function(x) {
  val_labels(x) <- c("strongly disagree" = 1,
                     "disagree" = 2,
                     "agree" = 3,
                     "strongly agree" = 4)
  x
}
data <- data %>%
  mutate_at(likert, 
            add_likert)

## Value labels 1-5 strongly disagree - neither - strongly agree 
likert <- dict %>% 
  filter (value_labels == "1 strongly disagree 2 disagree 3 neither agree nor disagree 4 agree 5 strongly agree") %>%
  pull(variable)
add_likert <- function(x) {
  val_labels(x) <- c("strongly disagree" = 1,
                     "disagree" = 2,
                     "neither agree nor disagree" = 3,
                     "agree" = 4,
                     "strongly agree" = 5)
  x
}
data <- data %>%
  mutate_at(likert, 
            add_likert)  

## Value labels 1-5 strongly disagree - neutral - strongly agree 
likert <- dict %>% 
  filter (value_labels == "1 strongly disagree 2 disagree 3 neutral 4 agree 5 strongly agree") %>%
  pull(variable)
add_likert <- function(x) {
  val_labels(x) <- c("strongly disagree" = 1,
                     "disagree" = 2,
                     "neutral" = 3,
                     "agree" = 4,
                     "strongly agree" = 5)
  x
}
data <- data %>%
  mutate_at(likert, 
            add_likert)  

## Value labels 1-5 strongly disagree - neutral - strongly agree 
likert <- dict %>% 
  filter (value_labels == "1 strongly disagree 2 disagree 3 neutral 4 agree 5 strongly agree") %>%
  pull(variable)
add_likert <- function(x) {
  val_labels(x) <- c("strongly disagree" = 1,
                     "disagree" = 2,
                     "neutral" = 3,
                     "agree" = 4,
                     "strongly agree" = 5)
  x
}
data <- data %>%
  mutate_at(likert, 
            add_likert)  

rm(likert, add_likert)

## Value labels miscellaneous demographics 
val_labels(data$schooling) <- c("did not complete high school" = 1,
                                "high school graduate" = 2,
                                "currently attending two-yr college" = 3,
                                "currently attending four-yr college" = 4,
                                "two-year college grad" = 5,
                                "4-yr college grad" = 6,
                                "grad/professional degree" = 7) 
                               
val_labels(data$politics) <- c("extremely liberal" = 1,
                               "liberal" = 2,
                               "slightly liberal" = 3,
                               "moderate, middle of the road" = 4,
                               "slightly conservative" = 5,
                               "conservative" = 6,
                               "extremely conservative" = 7) 

val_labels(data$socialclass) <- c("low-income/poor" = 1,
                                  "working class/lower middle class" = 2,
                                  "middle class" = 3,
                                  "upper middle class" = 4,
                                  "upper class" = 5) 

val_labels(data$usbornp) <- c("neither parent U.S. born" = 1,
                              "one parent U.S. born" = 2,
                              "both parents U.S. born" = 3) 

val_labels(data$religion) <- c("not at all religious" = 1,
                                  "a little religious" = 2,
                                  "fairly religious" = 3,
                                  "very religious" = 4) 

val_labels(data$ethnic_cat) <- c("white" = 1,
                                  "black" = 2,
                                  "other" = 6,
                                  "latino" = 7) 

## Reverse-scoring 
reversed_items <- dict %>%  #make a list of reversed items
  filter (keying == -1) %>% 
  pull(variable)
data <- data %>% #rename reversed items to end with R
  rename_at(reversed_items, add_R)

variable <- c(dict$variable) #create a list of variables
y <- data.frame(matrix(ncol = length(variable), nrow = 0)) #make empty dataframe
colnames(y) <- variable #name empty dataframe with variable list
y <- y %>% #rename variable list so reversed items end with R
  rename_at(reversed_items, add_R)
dict$variable <- colnames(y) #merge back renamed reverse items into dictionary
rm(list = c("y", "reversed_items", "variable"))

data <- data %>%  #reverse values in data
  mutate_at(vars(matches("\\dR$")),
            reverse_labelled_values)

## Gender 
female <- data %>% 
  filter(str_detect(gender_o, fixed("Fem", ignore_case=TRUE))|
           str_detect(gender_o, fixed("Wom", ignore_case=TRUE))|
           str_detect(gender_o, fixed("she", ignore_case=TRUE))|
           str_detect(gender_o, fixed("girl", ignore_case=TRUE))|
           gender_o == "f" |
           gender_o == "F"|
           gender_o == "Feamle"|
           gender_o == "Fenale")%>% 
  pull(ID)

male <- data %>% 
  filter(gender_o == "A male. Man. Penis."|
           gender_o == "A man"|
           gender_o == "A normal guy"|
           gender_o == "Kale"|
           gender_o == "Make"|
           gender_o == "Male"|
           gender_o == "male"|
           gender_o == "male"|
           gender_o == "MALE"|
           gender_o == "Malw"|
           gender_o == "Man"|
           gender_o == "man"|
           gender_o == "masculino"|
           gender_o == "Men"|
           gender_o == "Transgender Male") %>% 
  pull(ID)

nonbinary <- data %>% 
  filter(str_detect(gender_o, fixed("Non-Binary", ignore_case=TRUE))|
           str_detect(gender_o, fixed("Nonbinary", ignore_case=TRUE))|
           gender_o == "Queer"|
           gender_o == "Binary"|
           gender_o == "Agender/No gender/Gender isn't real"|
           gender_o == "Gender Fluid"|
           gender_o == "No gender"|
           gender_o == "Non Binary"|
           gender_o == "Non Binary Questioning"|
           gender_o == "agender"|
           gender_o == "Bigender") %>% 
  pull(ID)

data <- data %>% 
  mutate(gender_f = ifelse(
    ID %in% female, # select when ID corresponds to "female"
    1,
    ifelse(
      ID %in% male, # select when ID corresponds to "male"
      2,
      ifelse(
        ID %in% nonbinary, #select when ID corresponds to "nonbinary"
        3,
        NA))))
data$gender_f <- as.numeric(data$gender_f)

rm("female","male","nonbinary")

val_labels(data$gender_f) <- c("female" = 1,
                               "male" = 2,
                               "nonbinary" = 3) 

var_label(data$gender_f) <- "Gender Coded as Factor"

# > scale scoring ----
# >> Rosenberg Self-Esteem Scale (Rosenberg, 1965) ----
## create list of items for each variable 
rse_selfesteem <- dict %>% 
  filter (scale == "RSE Self-Esteem") %>% 
  pull(variable)
rse_positive <- dict %>% 
  filter (subscale == "RSE Positive Self-Esteem") %>% 
  pull(variable)
rse_negative <- dict %>% 
  filter (subscale == "RSE Negative Self-Esteem") %>% 
  pull(variable)

## create aggregated variables 
data$rse_selfesteem <- data %>% 
  dplyr::select(all_of(rse_selfesteem)) %>% 
  aggregate_and_document_scale()
data$rse_positive <- data %>% 
  dplyr::select(all_of(rse_positive)) %>% 
  aggregate_and_document_scale()
data$rse_negative <- data %>% 
  dplyr::select(all_of(rse_negative)) %>% 
  aggregate_and_document_scale()

## add variable label for aggregated variables 
var_label(data$rse_selfesteem) <- "General Self-Esteem 10 RSE items aggregated by rowMeans"
var_label(data$rse_positive) <- "Positive Self-Esteem 5 RSE items aggregated by rowMeans"
var_label(data$rse_negative) <- "Negative Self-Esteem 5 RSE items aggregated by rowMeans"

rm(rse_negative, rse_positive, rse_selfesteem)

# >> Eriksonian Psychosocial Stage Inventory (Rosenthal et al., 1981) ----
## create list of items for each variable 
epsi_confusion <- dict %>% 
  filter (scale == "EPSI Confusion") %>% 
  pull(variable)
epsi_coherence <- dict %>% 
  filter (scale == "EPSI Coherence") %>% 
  pull(variable)

## create aggregated variables 
data$epsi_confusion <- data %>% 
  dplyr::select(all_of(epsi_confusion)) %>% 
  aggregate_and_document_scale()
data$epsi_coherence <- data %>% 
  dplyr::select(all_of(epsi_coherence)) %>% 
  aggregate_and_document_scale()

## add variable label for aggregated variables
var_label(data$epsi_confusion) <- "Identity Confusion 6 EPSI items aggregated by rowMeans"
var_label(data$epsi_coherence) <- "Identity Coherence 6 EPSI items aggregated by rowMeans"

rm(epsi_confusion, epsi_coherence)

# >> Big Five Aspect Scale (DeYoung et al., 2007) ----
## create list of items for each variable 
bfas_agreeableness <- dict %>% 
  filter (scale == "BFAS Agreeableness") %>% 
  pull(variable)
bfas_conscientiousness <- dict %>% 
  filter (scale == "BFAS Conscientiousness") %>% 
  pull(variable)
bfas_extraversion <- dict %>% 
  filter (scale == "BFAS Extraversion") %>% 
  pull(variable)
bfas_neuroticism <- dict %>% 
  filter (scale == "BFAS Neuroticism") %>% 
  pull(variable)
bfas_opennessdomain <- dict %>% 
  filter (scale == "BFAS Openness Domain") %>% 
  pull(variable)
bfas_assertiveness <- dict %>% 
  filter (subscale == "BFAS Assertiveness") %>% 
  pull(variable)
bfas_compassion <- dict %>% 
  filter (subscale == "BFAS Compassion") %>% 
  pull(variable)
bfas_enthusiasm <- dict %>% 
  filter (subscale == "BFAS Enthusiasm") %>% 
  pull(variable)
bfas_industriousness <- dict %>% 
  filter (subscale == "BFAS Industriousness") %>% 
  pull(variable)
bfas_intellect <- dict %>% 
  filter (subscale == "BFAS Intellect") %>% 
  pull(variable)
bfas_opennessaspect <- dict %>% 
  filter (subscale == "BFAS Openness Aspect") %>% 
  pull(variable)
bfas_orderliness <- dict %>% 
  filter (subscale == "BFAS Orderliness") %>% 
  pull(variable)
bfas_politeness <- dict %>% 
  filter (subscale == "BFAS Politeness") %>% 
  pull(variable)
bfas_volatility <- dict %>% 
  filter (subscale == "BFAS Volatility") %>% 
  pull(variable)
bfas_withdrawal <- dict %>% 
  filter (subscale == "BFAS Withdrawal") %>% 
  pull(variable)

## reorder items within list so reversed items are not first 
bfas_agreeableness <- bfas_agreeableness[c(2,1,3:20)]
bfas_compassion <- bfas_compassion[c(2,1,3:10)]
bfas_neuroticism <- bfas_neuroticism[c(2,1,3:20)]
bfas_orderliness <- bfas_orderliness[c(2,1,3:10)]
bfas_withdrawal <- bfas_withdrawal[c(2,1,3:10)]

## create aggregated variables 
data$bfas_agreeableness <- data %>% 
  dplyr::select(all_of(bfas_agreeableness)) %>% 
  aggregate_and_document_scale()
data$bfas_conscientiousness <- data %>% 
  dplyr::select(all_of(bfas_conscientiousness)) %>% 
  aggregate_and_document_scale()
data$bfas_extraversion <- data %>% 
  dplyr::select(all_of(bfas_extraversion)) %>% 
  aggregate_and_document_scale()
data$bfas_neuroticism <- data %>% 
  dplyr::select(all_of(bfas_neuroticism)) %>% 
  aggregate_and_document_scale()
data$bfas_opennessdomain <- data %>% 
  dplyr::select(all_of(bfas_opennessdomain)) %>% 
  aggregate_and_document_scale()
data$bfas_assertiveness <- data %>% 
  dplyr::select(all_of(bfas_assertiveness)) %>% 
  aggregate_and_document_scale()
data$bfas_compassion <- data %>% 
  dplyr::select(all_of(bfas_compassion)) %>% 
  aggregate_and_document_scale()
data$bfas_enthusiasm <- data %>% 
  dplyr::select(all_of(bfas_enthusiasm)) %>% 
  aggregate_and_document_scale()
data$bfas_industriousness <- data %>% 
  dplyr::select(all_of(bfas_industriousness)) %>% 
  aggregate_and_document_scale()
data$bfas_intellect <- data %>% 
  dplyr::select(all_of(bfas_intellect)) %>% 
  aggregate_and_document_scale()
data$bfas_opennessaspect <- data %>% 
  dplyr::select(all_of(bfas_opennessaspect)) %>% 
  aggregate_and_document_scale()
data$bfas_orderliness <- data %>% 
  dplyr::select(all_of(bfas_orderliness)) %>% 
  aggregate_and_document_scale()
data$bfas_politeness <- data %>% 
  dplyr::select(all_of(bfas_politeness)) %>% 
  aggregate_and_document_scale()
data$bfas_volatility <- data %>% 
  dplyr::select(all_of(bfas_volatility)) %>% 
  aggregate_and_document_scale()
data$bfas_withdrawal <- data %>% 
  dplyr::select(all_of(bfas_withdrawal)) %>% 
  aggregate_and_document_scale()

## add variable label for aggregated variables 
var_label(data$bfas_agreeableness) <- "Agreeableness Domain 20 BFAS items aggregated by rowMeans"
var_label(data$bfas_conscientiousness) <- "Conscientiousness Domain 20 BFAS items aggregated by rowMeans"
var_label(data$bfas_extraversion) <- "Extraversion Domain 20 BFAS items aggregated by rowMeans"
var_label(data$bfas_neuroticism) <- "Neuroticism Domain 20 BFAS items aggregated by rowMeans"
var_label(data$bfas_opennessdomain) <- "Openness Domain 20 BFAS items aggregated by rowMeans"
var_label(data$bfas_assertiveness) <- "Assertiveness Aspect 10 BFAS items aggregated by rowMeans"
var_label(data$bfas_compassion) <- "Compassion Aspect 10 BFAS items aggregated by rowMeans"
var_label(data$bfas_enthusiasm) <- "Enthusiasm Aspect 10 BFAS items aggregated by rowMeans"
var_label(data$bfas_industriousness) <- "Industriousness Aspect 10 BFAS items aggregated by rowMeans"
var_label(data$bfas_intellect) <- "Intellect Aspect 10 BFAS items aggregated by rowMeans"
var_label(data$bfas_opennessaspect) <- "Openness Aspect 10 BFAS items aggregated by rowMeans"
var_label(data$bfas_orderliness) <- "Orderliness Aspect 10 BFAS items aggregated by rowMeans"
var_label(data$bfas_politeness) <- "Politeness Aspect 10 BFAS items aggregated by rowMeans"
var_label(data$bfas_volatility) <- "Volatility Aspect 10 BFAS items aggregated by rowMeans"
var_label(data$bfas_withdrawal) <- "Withdrawal Aspect 10 BFAS items aggregated by rowMeans"
rm(list = c("bfas_agreeableness", "bfas_assertiveness", "bfas_compassion",
            "bfas_conscientiousness", "bfas_enthusiasm", "bfas_extraversion",
            "bfas_industriousness", "bfas_intellect" ,"bfas_neuroticism",
            "bfas_opennessaspect", "bfas_opennessdomain", "bfas_orderliness",
            "bfas_politeness", "bfas_volatility", "bfas_withdrawal"))

# >> Dimensions of Identity Development Scale (Luyckx et al., 2008) ----
## create list of items for each variable 
dids_commitmaking <- dict %>% 
  filter (scale == "DIDS Commitment Making") %>% 
  pull(variable)
dids_commitid <- dict %>% 
  filter (scale == "DIDS Identification with Commitment") %>% 
  pull(variable)
dids_explorebreadth <- dict %>% 
  filter (scale == "DIDS Exploration in Breadth") %>% 
  pull(variable)
dids_exploredepth <- dict %>% 
  filter (scale == "DIDS Exploration in Depth") %>% 
  pull(variable)
dids_explorerum <- dict %>% 
  filter (scale == "DIDS Ruminative Exploration") %>% 
  pull(variable)

## create aggregated variables 
data$dids_commitmaking <- data %>% 
  dplyr::select(all_of(dids_commitmaking)) %>% 
  aggregate_and_document_scale()
data$dids_commitid <- data %>% 
  dplyr::select(all_of(dids_commitid)) %>% 
  aggregate_and_document_scale()
data$dids_explorebreadth <- data %>% 
  dplyr::select(all_of(dids_explorebreadth)) %>% 
  aggregate_and_document_scale()
data$dids_exploredepth <- data %>% 
  dplyr::select(all_of(dids_exploredepth)) %>% 
  aggregate_and_document_scale()
data$dids_explorerum <- data %>% 
  dplyr::select(all_of(dids_explorerum)) %>% 
  aggregate_and_document_scale()

## add variable label for aggregated variables 
var_label(data$dids_commitmaking) <- "Commitment Making 5 DIDS items aggregated by rowMeans"
var_label(data$dids_commitid) <- "Identification with Commitment 5 DIDS items aggregated by rowMeans"
var_label(data$dids_explorebreadth) <- "Exploration in Breadth 5 DIDS items aggregated by rowMeans"
var_label(data$dids_exploredepth) <- "Exploration in Depth 5 DIDS items aggregated by rowMeans"
var_label(data$dids_explorerum) <- "Ruminative Exploration 5 DIDS items aggregated by rowMeans"

rm(dids_commitmaking, dids_commitid, dids_explorebreadth, dids_exploredepth, dids_explorerum)

# >> Kessler Psychological Distress Scale (Kessler et al., 2002) ----
## create list of items for each variable 
k10_distress <- dict %>% 
  filter (scale == "K10 Psychological Distress") %>% 
  pull(variable)

## create aggregated variables 
data$k10_distress <- data %>% 
  dplyr::select(all_of(k10_distress)) %>% 
  aggregate_and_document_scale()

## add variable label for aggregated variables 
var_label(data$k10_distress) <- "Psychological Distress 10 K10 items aggregated by rowMeans"

rm(k10_distress)

# > clean-up ----
data <- data %>% 
  dplyr::select(ID, age, ethnic_cat, liveus, gender_f, gender_o, sexualo_o, schooling, socialclass, 
         usborn, usbornp, politics, religion, covidstress_1,
         rse_selfesteem:k10_distress)

attach(data)
# ANALYSIS ====

# > Power ----
pwr.r.test(n = 1260, sig.level = 0.05, power = 0.80)
pwr.r.test(n = 1260, sig.level = 0.05, power = 0.99)
pwr.r.test(n = 1260, r = .2, sig.level = 0.05)

# > Bivariate correlations ----
data %>% select(dids_commitmaking:dids_explorerum) %>% 
  apa.cor.table()

data %>% select(dids_explorebreadth, k10_distress, rse_selfesteem, rse_positive, rse_negative) %>% 
  apa.cor.table()

data %>% select(dids_exploredepth, k10_distress, rse_selfesteem, rse_positive, rse_negative) %>% 
  apa.cor.table()

# > (H1) - Exploration in depth ----
# >> Self-esteem
modH1a <- lm(rse_selfesteem ~ dids_exploredepth)
summary(modH1a)

modH1b <- lm(rse_selfesteem ~ dids_exploredepth + dids_explorerum)
summary(modH1b)

# >> Distress
modH1c <- lm(k10_distress ~ dids_exploredepth)
summary(modH1c)

modH1d <- lm(k10_distress ~ dids_exploredepth + dids_explorerum)
summary(modH1d)

# > (H2) - Exploration in breadth ----
# >> Self-esteem
modH2a <- lm(rse_selfesteem ~ dids_explorebreadth)
summary(modH2a)

modH2b <- lm(rse_selfesteem ~ dids_explorebreadth + dids_explorerum)
summary(modH2b)

# >> Distress
modH2c <- lm(k10_distress ~ dids_explorebreadth)
summary(modH2c)

modH2d <- lm(k10_distress ~ dids_explorebreadth + dids_explorerum)
summary(modH2d)

# > (H3) - Identity status MDS ----
# >> Dimensions ----
## standardize data
identity <- as.data.frame(scale(data[35:39]))

## correlation matrix
ridentity <- as.data.frame(cor(identity))

## transpose data to have variable on rows, subjects on columns
identity.t <- as.data.frame(t(identity))

#euclidean distance 
dist <- dist(identity.t)

#mds analysis
mds <- cmdscale(dist, eig = TRUE, k = 4)

#plot of eigenvalues
#elbow at 1 or 4
eig <- mds$eig
plot(eig, ylab = "eigenvalue")

#plot of dimension 1 and 2
plot(x = -mds$points[,1], 
     y = -mds$points[,2],
     xlab = "Coordinate 1",
     ylab = "Coordinate 2",
     main = "Metric MDS")
text(x = -mds$points[,1], 
     y = -mds$points[,2],
     labels=rownames(identity.t),
     cex=0.7, font=2)

#plot of dimension 2 and 3
plot(x = -mds$points[,2], 
     y = -mds$points[,3],
     xlab = "Coordinate 2",
     ylab = "Coordinate 3",
     main = "Metric MDS")
text(x = -mds$points[,2], 
     y = -mds$points[,3],
     labels=rownames(identity.t),
     cex=0.7, font=2)

#plot of dimension 3 and 4
plot(x = -mds$points[,3], 
     y = -mds$points[,4],
     xlab = "Coordinate 3",
     ylab = "Coordinate 4",
     main = "Metric MDS")
text(x = -mds$points[,3], 
     y = -mds$points[,4],
     labels=rownames(identity.t),
     cex=0.7, font=2)

#normalize dimension weights and flip sign(change depending on number of dimensions)
score.mds <- as.data.frame(-mds$points[,1:4]/5) %>% round(digits = 3)
names(score.mds) <- paste0("Dimension ", c(1:4))
score.mds

# >> Profile analysis ----
#pams function, using 4 dimensions as obtained through screeplot 
#pams dimensional configurations are normalized
#divided by number of objects
pams <- pams(data = identity, dim = 4)
pams.weight <- as.data.frame(pams$weights.matrix)

#example of the first 6 participants
head(pams.weight)

#summary of R-squared for all participants
summary(pams.weight$R.sq)

#merge person-profile match indices to data
pams.weight <- as.data.frame(pams.weight)
pams.weight$ID <- c(1:1260)

data <- merge(data, pams.weight)

# > (H4) - Correlation with external variables ----
# >> Personality traits ----
# >>> identity processes ----
data %>% select(dids_commitmaking:dids_explorerum,
                bfas_agreeableness, bfas_compassion, bfas_politeness) %>% 
  apa.cor.table()
  
data %>% select(dids_commitmaking:dids_explorerum,
                bfas_conscientiousness, bfas_industriousness, bfas_orderliness) %>% 
  apa.cor.table()

data %>% select(dids_commitmaking:dids_explorerum,
                bfas_extraversion, bfas_assertiveness, bfas_enthusiasm) %>% 
  apa.cor.table()

data %>% select(dids_commitmaking:dids_explorerum,
                bfas_neuroticism, bfas_volatility, bfas_withdrawal) %>% 
  apa.cor.table()
  
data %>% select(dids_commitmaking:dids_explorerum,
                bfas_opennessdomain, bfas_opennessaspect, bfas_intellect) %>% 
  apa.cor.table()  
  
# >>> identity MDS profiles ----
data %>% select(weight1:weight4,
                bfas_opennessdomain, bfas_opennessaspect, bfas_intellect) %>% 
  apa.cor.table()  
  
data %>% select(weight1:weight4,
                bfas_conscientiousness, bfas_industriousness, bfas_orderliness) %>% 
  apa.cor.table()  
  
data %>% select(weight1:weight4,
                bfas_extraversion, bfas_assertiveness, bfas_enthusiasm) %>% 
  apa.cor.table()  
  
data %>% select(weight1:weight4,
                bfas_neuroticism, bfas_volatility, bfas_withdrawal) %>% 
  apa.cor.table()  
  
data %>% select(weight1:weight4,
                bfas_opennessdomain, bfas_opennessaspect, bfas_intellect) %>% 
  apa.cor.table()   
  
# >> EPSI identity stages ----
# >>> identity processes ----
data %>% select(dids_commitmaking:dids_explorerum,
                epsi_coherence, epsi_confusion) %>% 
  apa.cor.table()
  
data %>% select(dids_commitmaking:dids_explorerum,
                epsi_coherence, epsi_confusion) %>% 
  apa.cor.table()

data %>% select(dids_commitmaking:dids_explorerum,
                epsi_coherence, epsi_confusion) %>% 
  apa.cor.table()

data %>% select(dids_commitmaking:dids_explorerum,
                epsi_coherence, epsi_confusion) %>% 
  apa.cor.table()
  
data %>% select(dids_commitmaking:dids_explorerum,
                epsi_coherence, epsi_confusion) %>% 
  apa.cor.table()  
  
# >>> identity MDS profiles ----
data %>% select(weight1:weight4,
                epsi_coherence, epsi_confusion) %>% 
  apa.cor.table()
  
data %>% select(weight1:weight4,
                epsi_coherence, epsi_confusion) %>% 
  apa.cor.table()

data %>% select(weight1:weight4,
                epsi_coherence, epsi_confusion) %>% 
  apa.cor.table()

data %>% select(weight1:weight4,
                epsi_coherence, epsi_confusion) %>% 
  apa.cor.table()
  
data %>% select(weight1:weight4,
                epsi_coherence, epsi_confusion) %>% 
  apa.cor.table()
