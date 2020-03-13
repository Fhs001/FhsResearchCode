# Statistical Analyses 

# Install packages -----

#install.packages("Amelia")
library(Amelia)

#install.packages("dplyr")
library(dplyr)

#install.packages("plyr")
library(plyr)

# install.packages("formattable")
library(formattable)



# Read files into objects ----

Final_Merge_Analysis <- read.csv("/Users/laurahudson/Desktop/FHS_Data01/Merge/Final_Merge_Analysis.csv", row.names =1, stringsAsFactors = FALSE, na.strings = c("", "NULL", "NA", " "))   



# Select for variables with frequency >5% -----

# Check whether variables are binary, creating dataframe

Is.binary <- function(v) {
  x <- unique(v)
  length(x) - sum(is.na(x)) == 2L
}

Is_binary <- vapply(Final_Merge_Analysis, Is.binary, logical(1))

Is_binary <- as.data.frame(cbind(names(Final_Merge_Analysis), Is_binary))

# Split dataframe into one for binary and one for non-binary variables

Binary_variables <- Is_binary[Is_binary$Is_binary == TRUE,]
Non_Binary_variables <- Is_binary[Is_binary$Is_binary == FALSE,]

# List whether binary (and non-binary) variables as characters, and then tranform into DFs

Binary_variables <- as.character(Binary_variables[,-2])
Non_Binary_variables <- as.character(Non_Binary_variables[,-2])

Binary_variables <- dplyr::select(Final_Merge_Analysis, one_of(Binary_variables))
Non_Binary_variables <- dplyr::select(Final_Merge_Analysis, one_of(Non_Binary_variables))


# Remove variables that aren't mutation data (i.e. clinical data)

Binary_variables <- dplyr::select(Binary_variables, -one_of("Gender", "Sidedness", "OS_Status",
                                                            "PFS_Status", "DFS_Status", 
                                                            "RELAPSE_FREE_INTERVAL_STATUS", "OS_FROM_PRIMARY_DIAGNOSIS_STATUS", "pEMVI", 
                                                            "Liver_Met", "Node_Met", "Lung_Met", "Peritoneum_Met", 
                                                            "sanger_panel", "Spinal", "Focus", "Victor", "Quasar2", 
                                                            "BRAF", "PIK3CA_ex9", "KRAS_c1213", "CN_APC_isLoss", 
                                                            "BCOR", "ERBB4", "FAM123B", "MTOR", "SETD2", "TBX3", "CLSPN", "MLL2", 
                                                            "T_Stage_Binary", "N_Stage_Binary", "M_Stage_Binary", 
                                                            "Sidedness_Binary", "RB1", "QC_Set", "Brain_Met", 
                                                            "Focus_4C", "Focus_Irinotecan", "ATRX", "IHC_cd56_pos_call"))

# Calculate frequencies of variables

Binary_variables <- Binary_variables %>% mutate_if(is.character, as.numeric) 

Total_Observations <- apply(Binary_variables, 2, function(x) { length(which(!is.na(x)))})

Model_Binary <- apply(Binary_variables, 2, sum, na.rm = TRUE) #Total number of mutations

FreqMutOutofNonMissingData <- Model_Binary/Total_Observations*100 

Model_Binary <- data.frame(names(Binary_variables), Model_Binary, Total_Observations, FreqMutOutofNonMissingData)

#Total_Observations <- data.frame(names(Binary_variables), Total_Observations)


Model_Binary$FreqMutOutofAllData <- Model_Binary$Model_Binary/nrow(Binary_variables)*100
names(Model_Binary)[1:2] <- c("Binary_variable", "Mutation_Count")
Model_Binary$Binary_variable <- as.character(Model_Binary$Binary_variable)


# No mutation (ie. Wt)
Model_Binary$NoMutationCount <- colSums(Binary_variables == 0, na.rm = TRUE)
Model_Binary$NoMutation_Frequency <- (Model_Binary$NoMutationCount/nrow(Binary_variables))*100

# No data (ie. NA)
Model_Binary$Missing <- colSums(apply(Binary_variables, 2, is.na))
Model_Binary$Missing_Frequency <- (Model_Binary$Missing/nrow(Final_Merge_Analysis))*100

# Arrange by frequency of mutation, and keep only those with mut frequency >5%

Model_Binary <- dplyr::arrange(Model_Binary, FreqMutOutofNonMissingData)
Model_Binary_Freq_Over_5 <- subset(Model_Binary, FreqMutOutofNonMissingData>5)


# Create list for variables of frequency >5%

List_var_f_over5 <- Model_Binary_Freq_Over_5$Binary_variable
print(List_var_f_over5 )





# Select samples for final analysis: exclude samples with >1 specific site of metastasis ----

#Final_Merge_Analysis_1met <- Final_Merge_Analysis


# Final_Merge_Analysis_1met$Total_Mets <- Final_Merge_Analysis_1met$Liver_Met + Final_Merge_Analysis_1met$Lung_Met + Final_Merge_Analysis_1met$Node_Met + Final_Merge_Analysis_1met$Peritoneum_Met
# table(Final_Merge_Analysis_1met$Total_Mets)

# Final_Merge_Analysis_1met$Include[Final_Merge_Analysis_1met$Total_Mets == "1"] <- "Yes" #Should = 433
# table(Final_Merge_Analysis_1met$Include)

# Final_Merge_Analysis_1met$Include[is.na(Final_Merge_Analysis_1met$Include)] <- "No"

#Final_Merge_Analysis_1met <- Final_Merge_Analysis_1met[Final_Merge_Analysis_1met$Include == "Yes",]
#Final_Merge_Analysis_1met$Include <- NULL

# UNIVARIATE ANALYSES ----
# For Liver ----

# Select variables

Final_Merge_Liver <- Final_Merge_Analysis[!is.na(Final_Merge_Analysis$Liver_Met),]


Final_Merge_Selected_Liver <- dplyr::select(Final_Merge_Liver, "Scort_ID", "Liver_Met", 
                                            "T_Stage_Binary", "N_Stage_Binary", "M_Stage_Binary",
                                            "Focus",  "NewEpoc", "Quasar2", "Spinal", "Victor", 
                                            "Sidedness_Binary", "Age", 
                                            List_var_f_over5, "Hypoxia_Buffa", "TGFb_Fibroblast", 
                                            "ESTIMATE_TumorPurity", "MCP_CD8_T_cells", "MCP_Cytotoxic_lymphocytes", 
                                            "Methyl_Phen_Index",  "Total_Mutations")

colnames(Final_Merge_Selected_Liver)

# Check missing data 

sapply(Final_Merge_Selected_Liver,function(x) sum(is.na(x)))

# Make data numeric

Final_Merge_Selected_Liver[, -1] <- lapply(Final_Merge_Selected_Liver[, -1], function(x) as.numeric(as.character(x)))


# Run regression

Dependent_Liver <- dplyr::select(Final_Merge_Selected_Liver, one_of("Scort_ID", "Liver_Met"))
Independent_Liver <- dplyr::select(Final_Merge_Selected_Liver, -one_of("Scort_ID", "Liver_Met"))

Independent_cols_Liver <- ncol(Independent_Liver)
Test_Regression_Liver <- lapply(1:Independent_cols_Liver, function(x) glm(Dependent_Liver[,2] ~ Independent_Liver[,x], family = binomial))

View(Test_Regression_Liver)
Summaries_Liver <- lapply(Test_Regression_Liver, summary)

#Check samples used

CheckSamplesLiverUV <- lapply(1:Independent_cols_Liver, function(x) length(resid(Test_Regression_Liver[[x]])))
CheckSamplesLiverUV
CheckSamplesLiverUV <- unlist(CheckSamplesLiverUV)
CheckSamplesLiverUV <- data.frame(CheckSamplesLiverUV[11:45])
CheckSamplesLiverUV

IndepedentVarUsed <- colnames(Independent_Liver[11:45])

CheckSamplesLiverUV$Variable <- c(IndepedentVarUsed)
CheckSamplesLiverUV <- CheckSamplesLiverUV[,c(2,1)]

print(CheckSamplesLiverPmv$Variable)

VarOrder <- c("AMER1", "APC",   "ARID1A", "ATM", "BRAF_V600E", "FBXW7", 
              "KRAS", "MSI_Binary", "PIK3CA", "SMAD4", "SOX9", "TP53", 
              "ZFP36L2", "Total_Mutations", 
              "BRAF_Signature_Binary" , "CSM1", "CMS2", "CMS3", "CMS4", 
              "CRIS_A", "CRIS_B", "CRIS_C", "CRIS_D", "CRIS_E", 
              "ESTIMATE_TumorPurity" , "Hypoxia_Buffa",  "MCP_CD8_T_cells"  , 
              "MCP_Cytotoxic_lymphocytes", "TGFb_Fibroblast"  , 
              "CIN_DNN_Binary"   , "CN_08_p_isLoss"  ,  "CN_08_q_isGain"   , 
              "CN_SMAD4_isLoss" , "CN_TP53_isLoss"  , "Methyl_Phen_Index")

CheckSamplesLiverUV <- CheckSamplesLiverUV[c(2,28,3,1,8,7,22,11,16,4,
                                             10,26,5,35,17,13,21,6,15,18,
                                             14,20,9,12,31,29,32,33,30,27,
                                             25,23,24,19,34),]

write.csv(CheckSamplesLiverUV, "/Users/laurahudson/Documents/UV_Liver_Samples1.csv")

# Extract co-efficients and p-values

Regression_Results_Liver <- lapply(Summaries_Liver, function(x) x$coefficients[,])
Regression_Results_Liver2 <- do.call(rbind.data.frame, Regression_Results_Liver)

# Remove odd rows, leaving even rows (i.e. filter out intercept and keep independent variable)

Regression_Results_Liver2 <- Regression_Results_Liver2[ c(FALSE,TRUE),]

# Create dataframe with OR, 95%CI and p-value for each independent variable

Regression_Results_Liver3 <- as.data.frame(cbind(colnames(Independent_Liver), exp(Regression_Results_Liver2$Estimate), exp(Regression_Results_Liver2$Estimate-(1.96*Regression_Results_Liver2$`Std. Error`)), exp(Regression_Results_Liver2$Estimate+(1.96*Regression_Results_Liver2$`Std. Error`)), Regression_Results_Liver2$`Pr(>|z|)`))
colnames(Regression_Results_Liver3) <- c("Variables", "OR", "95%CI_Lower", "95%CI_Upper", "P_value")
Regression_Results_Liver3[,-1] <- lapply(Regression_Results_Liver3[,-1], function(x) as.numeric(as.character(x)))

# Adjust p-values for FDR

Regression_Results_Liver3$Adjusted_PVal <- p.adjust(Regression_Results_Liver3$P_value, method = "fdr")

# Format regression results

as.datatable(
  formattable(Regression_Results_Liver3, list(
    Variables = formatter("span",
                          style = ~ style(color = ifelse(Adjusted_PVal <= 0.05, "green", "red")))))
)

# For Lung ----

# Select variables

Final_Merge_Lung <- Final_Merge_Analysis[!is.na(Final_Merge_Analysis$Lung_Met),]


Final_Merge_Selected_Lung <- dplyr::select(Final_Merge_Lung, "Scort_ID", "Lung_Met", 
                                           "T_Stage_Binary", "N_Stage_Binary", "M_Stage_Binary",
                                           "Focus",  "NewEpoc", "Quasar2", "Spinal", "Victor", 
                                           "Sidedness_Binary", "Age", 
                                           List_var_f_over5, "Hypoxia_Buffa", "TGFb_Fibroblast", 
                                           "ESTIMATE_TumorPurity", "MCP_CD8_T_cells", "MCP_Cytotoxic_lymphocytes", 
                                           "Methyl_Phen_Index",  "Total_Mutations")

# Check missing data 

sapply(Final_Merge_Selected_Lung,function(x) sum(is.na(x)))

# Make data numeric

Final_Merge_Selected_Lung[, -1] <- lapply(Final_Merge_Selected_Lung[, -1], function(x) as.numeric(as.character(x)))


# Run regression

Dependent_Lung <- dplyr::select(Final_Merge_Selected_Lung, one_of("Scort_ID", "Lung_Met"))
Independent_Lung <- dplyr::select(Final_Merge_Selected_Lung, -one_of("Scort_ID", "Lung_Met"))

Independent_cols_Lung <- ncol(Independent_Lung)
Test_Regression_Lung <- lapply(1:Independent_cols_Lung, function(x) glm(Dependent_Lung[,2] ~ Independent_Lung[,x], family = binomial))

View(Test_Regression_Lung)
Summaries_Lung <- lapply(Test_Regression_Lung, summary)

#Check samples used

CheckSamplesLungUV <- lapply(1:Independent_cols_Lung, function(x) length(resid(Test_Regression_Lung[[x]])))
CheckSamplesLungUV
CheckSamplesLungUV <- unlist(CheckSamplesLungUV)
CheckSamplesLungUV <- data.frame(CheckSamplesLungUV[11:45])
CheckSamplesLungUV

IndepedentVarUsed <- colnames(Independent_Lung[11:45])

CheckSamplesLungUV$Variable <- c(IndepedentVarUsed)
CheckSamplesLungUV <- CheckSamplesLungUV[,c(2,1)]

print(CheckSamplesLungPmv$Variable)

VarOrder <- c("AMER1", "APC",   "ARID1A", "ATM", "BRAF_V600E", "FBXW7", 
              "KRAS", "MSI_Binary", "PIK3CA", "SMAD4", "SOX9", "TP53", 
              "ZFP36L2", "Total_Mutations", 
              "BRAF_Signature_Binary" , "CSM1", "CMS2", "CMS3", "CMS4", 
              "CRIS_A", "CRIS_B", "CRIS_C", "CRIS_D", "CRIS_E", 
              "ESTIMATE_TumorPurity" , "Hypoxia_Buffa",  "MCP_CD8_T_cells"  , 
              "MCP_Cytotoxic_lymphocytes", "TGFb_Fibroblast"  , 
              "CIN_DNN_Binary"   , "CN_08_p_isLoss"  ,  "CN_08_q_isGain"   , 
              "CN_SMAD4_isLoss" , "CN_TP53_isLoss"  , "Methyl_Phen_Index")

CheckSamplesLungUV <- CheckSamplesLungUV[c(2,28,3,1,8,7,22,11,16,4,
                                           10,26,5,35,17,13,21,6,15,18,
                                           14,20,9,12,31,29,32,33,30,27,
                                           25,23,24,19,34),]

write.csv(CheckSamplesLungUV, "/Users/laurahudson/Documents/UV_Lung_Samples1.csv")



# Extract co-efficients and p-values

Regression_Results_Lung <- lapply(Summaries_Lung, function(x) x$coefficients[,])
Regression_Results_Lung2 <- do.call(rbind.data.frame, Regression_Results_Lung)

# Remove odd rows, leaving even rows (i.e. filter out intercept and keep independent variable)

Regression_Results_Lung2 <- Regression_Results_Lung2[ c(FALSE,TRUE),]

# Create dataframe with OR, 95%CI and p-value for each independent variable

Regression_Results_Lung3 <- as.data.frame(cbind(colnames(Independent_Lung), exp(Regression_Results_Lung2$Estimate), exp(Regression_Results_Lung2$Estimate-(1.96*Regression_Results_Lung2$`Std. Error`)), exp(Regression_Results_Lung2$Estimate+(1.96*Regression_Results_Lung2$`Std. Error`)), Regression_Results_Lung2$`Pr(>|z|)`))
colnames(Regression_Results_Lung3) <- c("Variables", "OR", "95%CI_Lower", "95%CI_Upper", "P_value")
Regression_Results_Lung3[,-1] <- lapply(Regression_Results_Lung3[,-1], function(x) as.numeric(as.character(x)))

# Adjust p-values for FDR

Regression_Results_Lung3$Adjusted_PVal <- p.adjust(Regression_Results_Lung3$P_value, method = "fdr")

# Format regression results

as.datatable(
  formattable(Regression_Results_Lung3, list(
    Variables = formatter("span",
                          style = ~ style(color = ifelse(Adjusted_PVal <= 0.05, "green", "red")))))
)


# 




# For Node ----

# Select variables

Final_Merge_Node <- Final_Merge_Analysis[!is.na(Final_Merge_Analysis$Node_Met),]


Final_Merge_Selected_Node <- dplyr::select(Final_Merge_Node, "Scort_ID", "Node_Met", 
                                           "T_Stage_Binary", "N_Stage_Binary", "M_Stage_Binary",
                                           "Focus",  "NewEpoc", "Quasar2", "Spinal", "Victor", 
                                           "Sidedness_Binary", "Age", 
                                           List_var_f_over5, "Hypoxia_Buffa", "TGFb_Fibroblast", 
                                           "ESTIMATE_TumorPurity", "MCP_CD8_T_cells", "MCP_Cytotoxic_lymphocytes", 
                                           "Methyl_Phen_Index",  "Total_Mutations")

# Check missing data 

sapply(Final_Merge_Selected_Node,function(x) sum(is.na(x)))

# Make data numeric

Final_Merge_Selected_Node[, -1] <- lapply(Final_Merge_Selected_Node[, -1], function(x) as.numeric(as.character(x)))


# Run regression

Dependent_Node <- dplyr::select(Final_Merge_Selected_Node, one_of("Scort_ID", "Node_Met"))
Independent_Node <- dplyr::select(Final_Merge_Selected_Node, -one_of("Scort_ID", "Node_Met"))

Independent_cols_Node <- ncol(Independent_Node)
Test_Regression_Node <- lapply(1:Independent_cols_Node, function(x) glm(Dependent_Node[,2] ~ Independent_Node[,x], family = binomial))

View(Test_Regression_Node)
Summaries_Node <- lapply(Test_Regression_Node, summary)

#Check samples used

CheckSamplesNodeUV <- lapply(1:Independent_cols_Node, function(x) length(resid(Test_Regression_Node[[x]])))
CheckSamplesNodeUV
CheckSamplesNodeUV <- unlist(CheckSamplesNodeUV)
CheckSamplesNodeUV <- data.frame(CheckSamplesNodeUV[11:45])
CheckSamplesNodeUV

IndepedentVarUsed <- colnames(Independent_Node[11:45])

CheckSamplesNodeUV$Variable <- c(IndepedentVarUsed)
CheckSamplesNodeUV <- CheckSamplesNodeUV[,c(2,1)]

print(CheckSamplesNodePmv$Variable)

VarOrder <- c("AMER1", "APC",   "ARID1A", "ATM", "BRAF_V600E", "FBXW7", 
              "KRAS", "MSI_Binary", "PIK3CA", "SMAD4", "SOX9", "TP53", 
              "ZFP36L2", "Total_Mutations", 
              "BRAF_Signature_Binary" , "CSM1", "CMS2", "CMS3", "CMS4", 
              "CRIS_A", "CRIS_B", "CRIS_C", "CRIS_D", "CRIS_E", 
              "ESTIMATE_TumorPurity" , "Hypoxia_Buffa",  "MCP_CD8_T_cells"  , 
              "MCP_Cytotoxic_lymphocytes", "TGFb_Fibroblast"  , 
              "CIN_DNN_Binary"   , "CN_08_p_isLoss"  ,  "CN_08_q_isGain"   , 
              "CN_SMAD4_isLoss" , "CN_TP53_isLoss"  , "Methyl_Phen_Index")

CheckSamplesNodeUV <- CheckSamplesNodeUV[c(2,28,3,1,8,7,22,11,16,4,
                                           10,26,5,35,17,13,21,6,15,18,
                                           14,20,9,12,31,29,32,33,30,27,
                                           25,23,24,19,34),]

write.csv(CheckSamplesNodeUV, "/Users/laurahudson/Documents/UV_Node_Samples1.csv")



# Extract co-efficients and p-values

Regression_Results_Node <- lapply(Summaries_Node, function(x) x$coefficients[,])
Regression_Results_Node2 <- do.call(rbind.data.frame, Regression_Results_Node)

# Remove odd rows, leaving even rows (i.e. filter out intercept and keep independent variable)

Regression_Results_Node2 <- Regression_Results_Node2[ c(FALSE,TRUE),]

# Create dataframe with OR, 95%CI and p-value for each independent variable

Regression_Results_Node3 <- as.data.frame(cbind(colnames(Independent_Node), exp(Regression_Results_Node2$Estimate), exp(Regression_Results_Node2$Estimate-(1.96*Regression_Results_Node2$`Std. Error`)), exp(Regression_Results_Node2$Estimate+(1.96*Regression_Results_Node2$`Std. Error`)), Regression_Results_Node2$`Pr(>|z|)`))
colnames(Regression_Results_Node3) <- c("Variables", "OR", "95%CI_Lower", "95%CI_Upper", "P_value")
Regression_Results_Node3[,-1] <- lapply(Regression_Results_Node3[,-1], function(x) as.numeric(as.character(x)))

# Adjust p-values for FDR

Regression_Results_Node3$Adjusted_PVal <- p.adjust(Regression_Results_Node3$P_value, method = "fdr")

# Format regression results

as.datatable(
  formattable(Regression_Results_Node3, list(
    Variables = formatter("span",
                          style = ~ style(color = ifelse(Adjusted_PVal <= 0.05, "green", "red")))))
)


# Peritoneum w/o clinical confounders  (use this one!) ----

# Select variables

Final_Merge_Peritoneum <- Final_Merge_Analysis[!is.na(Final_Merge_Analysis$Peritoneum_Met),]


Final_Merge_Selected_Peritoneum <- dplyr::select(Final_Merge_Peritoneum, "Scort_ID", "Peritoneum_Met", 
                                                 
                                                 List_var_f_over5, "Hypoxia_Buffa", "TGFb_Fibroblast", 
                                                 "ESTIMATE_TumorPurity", "MCP_CD8_T_cells", "MCP_Cytotoxic_lymphocytes", 
                                                 "Methyl_Phen_Index",  "Total_Mutations")


# Check missing data 

sapply(Final_Merge_Selected_Peritoneum,function(x) sum(is.na(x)))

# Make data numeric

Final_Merge_Selected_Peritoneum[, -1] <- lapply(Final_Merge_Selected_Peritoneum[, -1], function(x) as.numeric(as.character(x)))


# Run regression

Dependent_Peritoneum <- dplyr::select(Final_Merge_Selected_Peritoneum, one_of("Scort_ID", "Peritoneum_Met"))
Independent_Peritoneum <- dplyr::select(Final_Merge_Selected_Peritoneum, -one_of("Scort_ID", "Peritoneum_Met"))

Independent_cols_Peritoneum <- ncol(Independent_Peritoneum)
Test_Regression_Peritoneum <- lapply(1:Independent_cols_Peritoneum, function(x) glm(Dependent_Peritoneum[,2] ~ Independent_Peritoneum[,x], family = binomial))

View(Test_Regression_Peritoneum)
Summaries_Peritoneum <- lapply(Test_Regression_Peritoneum, summary)


#Check samples used

CheckSamplesPeritoneumUV <- lapply(1:Independent_cols_Peritoneum, function(x) length(resid(Test_Regression_Peritoneum[[x]])))
CheckSamplesPeritoneumUV
CheckSamplesPeritoneumUV <- unlist(CheckSamplesPeritoneumUV)
CheckSamplesPeritoneumUV <- data.frame(CheckSamplesPeritoneumUV[1:35])
CheckSamplesPeritoneumUV

IndepedentVarUsed <- colnames(Independent_Peritoneum[1:35])

CheckSamplesPeritoneumUV$Variable <- c(IndepedentVarUsed)
CheckSamplesPeritoneumUV <- CheckSamplesPeritoneumUV[,c(2,1)]

print(CheckSamplesPeritoneumUV$Variable)

VarOrder <- c("AMER1", "APC",   "ARID1A", "ATM", "BRAF_V600E", "FBXW7", 
              "KRAS", "MSI_Binary", "PIK3CA", "SMAD4", "SOX9", "TP53", 
              "ZFP36L2", "Total_Mutations", 
              "BRAF_Signature_Binary" , "CSM1", "CMS2", "CMS3", "CMS4", 
              "CRIS_A", "CRIS_B", "CRIS_C", "CRIS_D", "CRIS_E", 
              "ESTIMATE_TumorPurity" , "Hypoxia_Buffa",  "MCP_CD8_T_cells"  , 
              "MCP_Cytotoxic_lymphocytes", "TGFb_Fibroblast"  , 
              "CIN_DNN_Binary"   , "CN_08_p_isLoss"  ,  "CN_08_q_isGain"   , 
              "CN_SMAD4_isLoss" , "CN_TP53_isLoss"  , "Methyl_Phen_Index")

CheckSamplesPeritoneumUV <- CheckSamplesPeritoneumUV[c(2,28,3,1,8,7,22,11,16,4,
                                                       10,26,5,35,17,13,21,6,15,18,
                                                       14,20,9,12,31,29,32,33,30,27,
                                                       25,23,24,19,34),]

write.csv(CheckSamplesPeritoneumUV, "/Users/laurahudson/Documents/UV_Peritoneum_Samples1.csv")



# Extract co-efficients and p-values

Regression_Results_Peritoneum <- lapply(Summaries_Peritoneum, function(x) x$coefficients[,])
Regression_Results_Peritoneum2 <- do.call(rbind.data.frame, Regression_Results_Peritoneum)

# Remove odd rows, leaving even rows (i.e. filter out intercept and keep independent variable)

Regression_Results_Peritoneum2 <- Regression_Results_Peritoneum2[ c(FALSE,TRUE),]

# Create dataframe with OR, 95%CI and p-value for each independent variable

Regression_Results_Peritoneum3 <- as.data.frame(cbind(colnames(Independent_Peritoneum), exp(Regression_Results_Peritoneum2$Estimate), exp(Regression_Results_Peritoneum2$Estimate-(1.96*Regression_Results_Peritoneum2$`Std. Error`)), exp(Regression_Results_Peritoneum2$Estimate+(1.96*Regression_Results_Peritoneum2$`Std. Error`)), Regression_Results_Peritoneum2$`Pr(>|z|)`))
colnames(Regression_Results_Peritoneum3) <- c("Variables", "OR", "95%CI_Lower", "95%CI_Upper", "P_value")
Regression_Results_Peritoneum3[,-1] <- lapply(Regression_Results_Peritoneum3[,-1], function(x) as.numeric(as.character(x)))

# Adjust p-values for FDR

Regression_Results_Peritoneum3$Adjusted_PVal <- p.adjust(Regression_Results_Peritoneum3$P_value, method = "fdr")

# Format regression results

as.datatable(
  formattable(Regression_Results_Peritoneum3, list(
    Variables = formatter("span",
                          style = ~ style(color = ifelse(Adjusted_PVal <= 0.05, "green", "red")))))
)



# Edit table

RR_Peritoneum <- Regression_Results_Peritoneum3

RR_Peritoneum[,2:4] <- as.numeric(unlist(RR_Peritoneum[,2:4]))
RR_Peritoneum[,2:4] <- round((RR_Peritoneum[,2:4]), digits = 2)

colnames(RR_Peritoneum)  <- c("Variables", "UV_OR", "UV_CI_Lower", "UV_CI_Upper", "P_value", "Adjusted_PVal")

RR_Peritoneum$Brac <- c(")")

RR_Peritoneum$UVORCI <- paste(RR_Peritoneum$UV_OR, RR_Peritoneum$UV_CI_Lower, sep=" (")
RR_Peritoneum$UVORCI <- paste(RR_Peritoneum$UVORCI, RR_Peritoneum$UV_CI_Upper, sep="-")

RR_Peritoneum$UVORCI <- paste(RR_Peritoneum$UVORCI, RR_Peritoneum$Brac, sep="")

RR_Peritoneum$UV_OR <- NULL
RR_Peritoneum$UV_CI_Lower <- NULL
RR_Peritoneum$UV_CI_Upper <- NULL
RR_Peritoneum$Brac <- NULL

RR_Peritoneum <- RR_Peritoneum[, c(1, 4, 2, 3)] # column order
rownames(RR_Peritoneum)
list(RR_Peritoneum$Variables)

RR_Peritoneum <- RR_Peritoneum[c(2,28,3,1,8,
                                 7,22,11,16,4,
                                 10,26,5,35,17,
                                 13,21,6,15,18,
                                 14,20,9,12,31,
                                 29,32,33,30,27,
                                 25,23,24,19,34),] # row order


write.csv(RR_Peritoneum, "/Users/laurahudson/Documents/UV_Peritoneum02.csv")


# Check missingness of data -----

Selected_Variables <- subset(Regression_Results_Liver3, Adjusted_PVal<=1000)
Selected_Variables <- subset(Selected_Variables, Variables != "Focus")
Selected_Variables <- subset(Selected_Variables, Variables != "Spinal")
Selected_Variables <- subset(Selected_Variables, Variables != "NewEpoc")
Selected_Variables <- subset(Selected_Variables, Variables != "Quasar2")
Selected_Variables <- subset(Selected_Variables, Variables != "Victor")

Selected_Variables <- as.character(Selected_Variables$Variables)
Selected_Variables2 <- c("Liver_Met", "Lung_Met", "Peritoneum_Met", "Node_Met", 
                         "Cohort","Gender", Selected_Variables)

Test_subset <- Final_Merge_Analysis[Selected_Variables2] 
Test_subset_noQ <- subset(Test_subset, Cohort != "QUASAR2")
Test_subset_noQorV <- subset(Test_subset_noQ, Cohort != "VICTOR")

colnames(Test_subset) <- c("Liver Met","Lung Met","Peritoneum Met","Lymph Node Met",
                           "Cohort","Gender","T Stage"           
                           ,"N Stage","M Stage","Tumour Side"        
                           , "Age","ATM","AMER1"                    
                           ,"ARID1A","SMAD4","ZFP36L2"                  
                           ,"CMS3","FBXW7","BRAF-V600E"               
                           ,"CRIS-D", "SOX9","MSI"               
                           ,"CRIS-E" ,"CMS1","CRIS-B"                   
                           ,"CMS4","PIK3CA","BRAF Signature"    
                           ,"CRIS-A","TP53 CN Loss","CRIS-C"                   
                           ,"CMS2" ,"KRAS" ,"Chr08q CN Gain"           
                           ,"SMAD4 CN Loss","Chr08p CN Loss","TP53"                     
                           ,"CIN (Genome-wide)","APC","Hypoxia Signature"            
                           ,"TGFb Fibroblast","Tumour Purity", "MCP CD8 T-cells"          
                           ,"Cytotoxic Lymphocytes", "Methylation Burden" , "Mutation Burden"     )

MissMapTest <- missmap(Test_subset, main = "Missingness Map", legend=T,
                       col = c("#fee08b","#2166ac"), rank.order =F, 
                       x.cex = 0.7, y.cex = 0.5 ,margins = c(8,5))

MissMapTest <- missmap(Test_subset, main = "Missingness Map", legend=T,
                       col = c("#045a8d","#3288bd"), rank.order =F, 
                       x.cex = 0.7, y.cex = 0.5 ,margins = c(8,5))

GraphCols <- c("#053061", "#3288bd","#fee08b","#f46d43")


# PRELININARY MULTIVARIATE ANALYSES ----

#Final_Merge_Analysis <- Final_Merge_Analysis_1met


# For Liver ----

## Subset significant variables from univariate analysis 

Liver_Sig_Variables <- subset(Regression_Results_Liver3, Adjusted_PVal<=1000)

# Remove clinical variables for adjustment or as not needed

Liver_Sig_Variables <- subset(Liver_Sig_Variables, Variables != "T_Stage_Binary")
Liver_Sig_Variables <- subset(Liver_Sig_Variables, Variables != "N_Stage_Binary")
Liver_Sig_Variables <- subset(Liver_Sig_Variables, Variables != "M_Stage_Binary")
Liver_Sig_Variables <- subset(Liver_Sig_Variables, Variables != "Age")
Liver_Sig_Variables <- subset(Liver_Sig_Variables, Variables != "Sidedness_Binary")
Liver_Sig_Variables <- subset(Liver_Sig_Variables, Variables != "Focus")
Liver_Sig_Variables <- subset(Liver_Sig_Variables, Variables != "Spinal")
Liver_Sig_Variables <- subset(Liver_Sig_Variables, Variables != "NewEpoc")
Liver_Sig_Variables <- subset(Liver_Sig_Variables, Variables != "Quasar2")
Liver_Sig_Variables <- subset(Liver_Sig_Variables, Variables != "Victor")

# Remove mut variables with frequency count < 30

Liver_Sig_Variables <- subset(Liver_Sig_Variables, Variables != "SETD2")
Liver_Sig_Variables <- subset(Liver_Sig_Variables, Variables != "MTOR")
Liver_Sig_Variables <- subset(Liver_Sig_Variables, Variables != "FAM123B")
Liver_Sig_Variables <- subset(Liver_Sig_Variables, Variables != "BCOR")
Liver_Sig_Variables <- subset(Liver_Sig_Variables, Variables != "TBX3")
Liver_Sig_Variables <- subset(Liver_Sig_Variables, Variables != "MLL2")
Liver_Sig_Variables <- subset(Liver_Sig_Variables, Variables != "CLSPN")
Liver_Sig_Variables <- subset(Liver_Sig_Variables, Variables != "ERBB4")

Liver_Sig_Variables <- as.character(Liver_Sig_Variables$Variables)


## Select these variables from Binary Merge 

Final_Merge_Selected_Liver_MV <- dplyr::select(Final_Merge_Analysis, "Scort_ID", "Liver_Met", 
                                               Sig_Variables, 
                                               "T_Stage_Binary", "N_Stage_Binary", "M_Stage_Binary",
                                               "Age", "Sidedness_Binary", "Gender",
                                               "Cohort")

### Run regression for each dependent variable    

Dependent_Liver_MV <- dplyr::select(Final_Merge_Selected_Liver_MV, -one_of("Scort_ID", "Liver_Met", "T_Stage_Binary", "N_Stage_Binary", "M_Stage_Binary",
                                                                           "Age", "Sidedness_Binary","Gender",
                                                                           "Cohort"))

All_dependent_cols_Liver_MV <- ncol(Dependent_Liver_MV)






Model_Liver_MV <- lapply(1:All_dependent_cols_Liver_MV, function(x) glm(Liver_Met ~ Dependent_Liver_MV[,x]
                                                                        + T_Stage_Binary + N_Stage_Binary + M_Stage_Binary 
                                                                        + Age + Sidedness_Binary + Gender
                                                                        + Cohort,
                                                                        data = Final_Merge_Selected_Liver_MV, family = binomial))


lapply(1:All_dependent_cols_Liver_MV, function(x) print(Model_Liver_MV[[x]]))

Summary_Model_Liver_MV <- lapply(1:All_dependent_cols_Liver_MV, function(x) summary(Model_Liver_MV[[x]]))

#Check samples used
CheckSamplesLiverPmv <- lapply(1:All_dependent_cols_Liver_MV, function(x) length(resid(Model_Liver_MV[[x]])))
CheckSamplesLiverPmv <- unlist(CheckSamplesLiverPmv)
CheckSamplesLiverPmv <- data.frame(CheckSamplesLiverPmv)
DependentNames <- colnames(Dependent_Liver_MV)
CheckSamplesLiverPmv$Variable <- c(DependentNames)
CheckSamplesLiverPmv <- CheckSamplesLiverPmv[,c(2,1)]

print(CheckSamplesLiverPmv$Variable)

VarOrder <- c("AMER1", "APC",   "ARID1A", "ATM", "BRAF_V600E", "FBXW7", 
              "KRAS", "MSI_Binary", "PIK3CA", "SMAD4", "SOX9", "TP53", 
              "ZFP36L2", "Total_Mutations", 
              "BRAF_Signature_Binary" , "CSM1", "CMS2", "CMS3", "CMS4", 
              "CRIS_A", "CRIS_B", "CRIS_C", "CRIS_D", "CRIS_E", 
              "ESTIMATE_TumorPurity" , "Hypoxia_Buffa",  "MCP_CD8_T_cells"  , 
              "MCP_Cytotoxic_lymphocytes", "TGFb_Fibroblast"  , 
              "CIN_DNN_Binary"   , "CN_08_p_isLoss"  ,  "CN_08_q_isGain"   , 
              "CN_SMAD4_isLoss" , "CN_TP53_isLoss"  , "Methyl_Phen_Index")


CheckSamplesLiverPmv <- CheckSamplesLiverPmv[c(2,28,3,1,8,7,22,11,16,4,
                                               10,26,5,35,17,13,21,6,15,18,
                                               14,20,9,12,31,29,32,33,30,27,
                                               25,23,24,19,34),]


write.csv(CheckSamplesLiverPmv, "/Users/laurahudson/Documents/PMV_Liver_Samples1.csv")



## Extract coefficients 

Regression_Results_Liver_MV <- lapply(1:All_dependent_cols_Liver_MV, function(x) Summary_Model_Liver_MV[[x]]$coefficients)

Regression_Results_Liver2_MV <- lapply(1:All_dependent_cols_Liver_MV, function(x) as.data.frame(Regression_Results_Liver_MV[[x]]))

n <- lapply(1:All_dependent_cols_Liver_MV, function(x) nrow(Regression_Results_Liver2_MV[[x]]))

## Get P-values

P_values_MV_Liver <- lapply(1:All_dependent_cols_Liver_MV, function(x) unname(coef(Summary_Model_Liver_MV[[x]])[,'Pr(>|z|)'])[2:n[[x]]])

Adjusted_PVal_MV_Liver <- lapply(1:All_dependent_cols_Liver_MV, function(x) p.adjust(P_values_MV_Liver[[x]], method = "fdr"))


### Get names of coefficients

CoefNames_MV_Liver <- lapply(1:All_dependent_cols_Liver_MV, function(x) names(coef(summary(Model_Liver_MV[[x]]))[,'Pr(>|z|)'])[-1])

#### Create dataframe with OR, 95%CI and p-value for each dependent variable


OR_MV_Liver <- lapply(1:All_dependent_cols_Liver_MV, function(x) exp(Summary_Model_Liver_MV[[x]]$coefficients[-1,1]))

CI_MV_Low_Liver <- lapply(1:All_dependent_cols_Liver_MV, function(x) exp(Summary_Model_Liver_MV[[x]]$coefficients[-1,1] + (qnorm(0.025) * Summary_Model_Liver_MV[[x]]$coefficients[-1,2])))


CI_MV_High_Liver <- lapply(1:All_dependent_cols_Liver_MV, function(x) exp(Summary_Model_Liver_MV[[x]]$coefficients[-1,1] + (qnorm(0.975) * Summary_Model_Liver_MV[[x]]$coefficients[-1,2])))


Liver_MV_Regression_Results <- lapply(1:All_dependent_cols_Liver_MV, function(x) cbind(CoefNames_MV_Liver[[x]], OR_MV_Liver[[x]], CI_MV_Low_Liver[[x]], CI_MV_High_Liver[[x]], P_values_MV_Liver[[x]], Adjusted_PVal_MV_Liver[[x]]))


Liver_MV_Regression_Results <- lapply(1:All_dependent_cols_Liver_MV, function(x) as.data.frame(Liver_MV_Regression_Results[[x]]))


## Rename dependent variables

Dependent_Liver_MV_Names <- colnames(Dependent_Liver_MV)

### Subset dependent variables

Liver_MV_Regression_Results2 <- lapply(1:All_dependent_cols_Liver_MV, function(x) subset(Liver_MV_Regression_Results[[x]], V1 == "Dependent_Liver_MV[, x]"))


### Create dataframe of dependent variables

Liver_MV_Regression_Results3 <- do.call('rbind',Liver_MV_Regression_Results2) 

### Change column names

colnames(Liver_MV_Regression_Results3) <- c("Variables", "OR", "95%CI_Low", "95%CI_High", "P_value", "Adjusted_PVal")


### Fill variable column with names of actual variables

Dependent_Liver_MV_Names <- colnames(Dependent_Liver_MV)
print(Dependent_Liver_MV_Names)

Liver_MV_Regression_Results3$Variables <- c(Dependent_Liver_MV_Names)

Liver_MV_Regression_Results3$Adjusted_PVal <- as.numeric(levels(Liver_MV_Regression_Results3$Adjusted_PVal))[Liver_MV_Regression_Results3$Adjusted_PVal]

### Format output table

library(formattable)


as.datatable(
  formattable(Liver_MV_Regression_Results3, list(
    Variables = formatter("span",
                          style = ~ style(color = ifelse(Adjusted_PVal <= 0.05, "green", "red")))))
)


# Check no. samples used in analysis

Model_Liver_MV <- lapply(1:All_dependent_cols_Liver_MV, function(x) glm(Liver_Met ~ Dependent_Liver_MV[,x]
                                                                        + T_Stage_Binary + N_Stage_Binary
                                                                        + M_Stage_Binary + Age + Sidedness_Binary
                                                                        + Cohort,
                                                                        data = Final_Merge_Selected_Liver_MV, family = binomial))


# For Lung ----

## Subset significant variables from univariate analysis 

Lung_Sig_Variables <- subset(Regression_Results_Lung3, Adjusted_PVal<=1000)

# Remove clinical variables for adjustment or as not needed

Lung_Sig_Variables <- subset(Lung_Sig_Variables, Variables != "T_Stage_Binary")
Lung_Sig_Variables <- subset(Lung_Sig_Variables, Variables != "N_Stage_Binary")
Lung_Sig_Variables <- subset(Lung_Sig_Variables, Variables != "M_Stage_Binary")
Lung_Sig_Variables <- subset(Lung_Sig_Variables, Variables != "Age")
Lung_Sig_Variables <- subset(Lung_Sig_Variables, Variables != "Sidedness_Binary")
Lung_Sig_Variables <- subset(Lung_Sig_Variables, Variables != "Focus")
Lung_Sig_Variables <- subset(Lung_Sig_Variables, Variables != "Spinal")
Lung_Sig_Variables <- subset(Lung_Sig_Variables, Variables != "NewEpoc")
Lung_Sig_Variables <- subset(Lung_Sig_Variables, Variables != "Quasar2")
Lung_Sig_Variables <- subset(Lung_Sig_Variables, Variables != "Victor")

# Remove mut variables with frequency count < 30

Lung_Sig_Variables <- subset(Lung_Sig_Variables, Variables != "SETD2")
Lung_Sig_Variables <- subset(Lung_Sig_Variables, Variables != "MTOR")
Lung_Sig_Variables <- subset(Lung_Sig_Variables, Variables != "FAM123B")
Lung_Sig_Variables <- subset(Lung_Sig_Variables, Variables != "BCOR")
Lung_Sig_Variables <- subset(Lung_Sig_Variables, Variables != "TBX3")
Lung_Sig_Variables <- subset(Lung_Sig_Variables, Variables != "MLL2")
Lung_Sig_Variables <- subset(Lung_Sig_Variables, Variables != "CLSPN")
Lung_Sig_Variables <- subset(Lung_Sig_Variables, Variables != "ERBB4")

Lung_Sig_Variables <- as.character(Lung_Sig_Variables$Variables)


## Select these variables from Binary Merge 

Final_Merge_Selected_Lung_MV <- dplyr::select(Final_Merge_Analysis, "Scort_ID", "Lung_Met", Lung_Sig_Variables, 
                                              "T_Stage_Binary", "N_Stage_Binary", "M_Stage_Binary",
                                              "Age", "Sidedness_Binary", "Gender",
                                              "Cohort")

### Run regression for each dependent variable    

Dependent_Lung_MV <- dplyr::select(Final_Merge_Selected_Lung_MV, -one_of("Scort_ID", "Lung_Met", "T_Stage_Binary", 
                                                                         "N_Stage_Binary",
                                                                         "M_Stage_Binary",
                                                                         "Age", "Sidedness_Binary",
                                                                         "Gender",
                                                                         "Cohort"))

All_dependent_cols_Lung_MV <- ncol(Dependent_Lung_MV)


Model_Lung_MV <- lapply(1:All_dependent_cols_Lung_MV, function(x) glm(Lung_Met ~ Dependent_Lung_MV[,x]
                                                                      + T_Stage_Binary 
                                                                      + N_Stage_Binary
                                                                      + M_Stage_Binary
                                                                      + Age + Sidedness_Binary 
                                                                      + Gender
                                                                      + Cohort
                                                                      ,data = Final_Merge_Selected_Lung_MV, family = binomial))


Summary_Model_Lung_MV <- lapply(1:All_dependent_cols_Lung_MV, function(x) summary(Model_Lung_MV[[x]]))

#Check samples used
CheckSamplesLungPmv <- lapply(1:All_dependent_cols_Lung_MV, function(x) length(resid(Model_Lung_MV[[x]])))
CheckSamplesLungPmv <- unlist(CheckSamplesLungPmv)
CheckSamplesLungPmv <- data.frame(CheckSamplesLungPmv)
DependentNames <- colnames(Dependent_Lung_MV)
CheckSamplesLungPmv$Variable <- c(DependentNames)

CheckSamplesLungPmv <- CheckSamplesLungPmv[,c(2,1)]
CheckSamplesLungPmv <- CheckSamplesLungPmv[c(2,28,3,1,8,7,22,11,16,4,
                                             10,26,5,35,17,13,21,6,15,18,
                                             14,20,9,12,31,29,32,33,30,27,
                                             25,23,24,19,34),]

write.csv(CheckSamplesLungPmv, "/Users/laurahudson/Documents/PMV_Lung_Samples1.csv")

## Extract coefficients 

Regression_Results_Lung_MV <- lapply(1:All_dependent_cols_Lung_MV, function(x) Summary_Model_Lung_MV[[x]]$coefficients)

Regression_Results_Lung2_MV <- lapply(1:All_dependent_cols_Lung_MV, function(x) as.data.frame(Regression_Results_Lung_MV[[x]]))

n <- lapply(1:All_dependent_cols_Lung_MV, function(x) nrow(Regression_Results_Lung2_MV[[x]]))

## Get P-values

P_values_MV_Lung <- lapply(1:All_dependent_cols_Lung_MV, function(x) unname(coef(Summary_Model_Lung_MV[[x]])[,'Pr(>|z|)'])[2:n[[x]]])

Adjusted_PVal_MV_Lung <- lapply(1:All_dependent_cols_Lung_MV, function(x) p.adjust(P_values_MV_Lung[[x]], method = "fdr"))


### Get names of coefficients

CoefNames_MV_Lung <- lapply(1:All_dependent_cols_Lung_MV, function(x) names(coef(summary(Model_Lung_MV[[x]]))[,'Pr(>|z|)'])[-1])

#### Create dataframe with OR, 95%CI and p-value for each dependent variable


OR_MV_Lung <- lapply(1:All_dependent_cols_Lung_MV, function(x) exp(Summary_Model_Lung_MV[[x]]$coefficients[-1,1]))

CI_MV_Low_Lung <- lapply(1:All_dependent_cols_Lung_MV, function(x) exp(Summary_Model_Lung_MV[[x]]$coefficients[-1,1] + (qnorm(0.025) * Summary_Model_Lung_MV[[x]]$coefficients[-1,2])))


CI_MV_High_Lung <- lapply(1:All_dependent_cols_Lung_MV, function(x) exp(Summary_Model_Lung_MV[[x]]$coefficients[-1,1] + (qnorm(0.975) * Summary_Model_Lung_MV[[x]]$coefficients[-1,2])))


Lung_MV_Regression_Results <- lapply(1:All_dependent_cols_Lung_MV, function(x) cbind(CoefNames_MV_Lung[[x]], OR_MV_Lung[[x]], CI_MV_Low_Lung[[x]], CI_MV_High_Lung[[x]], P_values_MV_Lung[[x]], Adjusted_PVal_MV_Lung[[x]]))


Lung_MV_Regression_Results <- lapply(1:All_dependent_cols_Lung_MV, function(x) as.data.frame(Lung_MV_Regression_Results[[x]]))


## Rename dependent variables

Dependent_Lung_MV_Names <- colnames(Dependent_Lung_MV)

### Subset dependent variables

Lung_MV_Regression_Results2 <- lapply(1:All_dependent_cols_Lung_MV, function(x) subset(Lung_MV_Regression_Results[[x]], V1 == "Dependent_Lung_MV[, x]"))


### Create dataframe of dependent variables

Lung_MV_Regression_Results3 <- do.call('rbind',Lung_MV_Regression_Results2) 

### Change column names

colnames(Lung_MV_Regression_Results3) <- c("Variables", "OR", "95%CI_Low", "95%CI_High", "P_value", "Adjusted_PVal")


### Fill variable column with names of actual variables

Dependent_Lung_MV_Names <- colnames(Dependent_Lung_MV)
print(Dependent_Lung_MV_Names)

Lung_MV_Regression_Results3$Variables <- c(Dependent_Lung_MV_Names)

Lung_MV_Regression_Results3$Adjusted_PVal <- as.numeric(levels(Lung_MV_Regression_Results3$Adjusted_PVal))[Lung_MV_Regression_Results3$Adjusted_PVal]

### Format output table

library(formattable)


as.datatable(
  formattable(Lung_MV_Regression_Results3, list(
    Variables = formatter("span",
                          style = ~ style(color = ifelse(Adjusted_PVal <= 0.05, "green", "red")))))
)




# For Node ----

## Subset significant variables from univariate analysis 

Nodal_Sig_Variables <- subset(Regression_Results_Node3, Adjusted_PVal<= 1000)

# Remove clinical variables for adjustment or as not needed

Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "T_Stage_Binary")
Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "N_Stage_Binary")
Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "M_Stage_Binary")
Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "Age")
Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "Sidedness_Binary")
Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "Cohort")
Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "Focus")
Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "Spinal")
Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "NewEpoc")
Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "Quasar2")
Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "Victor")

# Remove mut variables with frequency count < 30

Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "SETD2")
Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "MTOR")
Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "FAM123B")
Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "BCOR")
Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "TBX3")
Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "MLL2")
Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "CLSPN")
Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "ERBB4")

Nodal_Sig_Variables <- as.character(Nodal_Sig_Variables$Variables)


## Select these variables from Binary Merge 

Final_Merge_Selected_Nodal_MV <- dplyr::select(Final_Merge_Analysis, "Scort_ID", "Node_Met", Nodal_Sig_Variables, 
                                               "T_Stage_Binary", "N_Stage_Binary", "M_Stage_Binary",
                                               "Age", "Sidedness_Binary", "Gender", 
                                               "Cohort")

### Run regression for each dependent variable    

Dependent_Nodal_MV <- dplyr::select(Final_Merge_Selected_Nodal_MV, -one_of("Scort_ID", "Node_Met", "T_Stage_Binary", "N_Stage_Binary", "M_Stage_Binary",
                                                                           "Age", "Sidedness_Binary", "Gender",
                                                                           "Cohort"))

All_dependent_cols_Nodal_MV <- ncol(Dependent_Nodal_MV)






Model_Nodal_MV <- lapply(1:All_dependent_cols_Nodal_MV, function(x) glm(Node_Met ~ Dependent_Nodal_MV[,x]
                                                                        + T_Stage_Binary + N_Stage_Binary
                                                                        + M_Stage_Binary + Age + Sidedness_Binary + Gender
                                                                        + Cohort
                                                                        ,
                                                                        data = Final_Merge_Selected_Nodal_MV, family = binomial))



Summary_Model_Nodal_MV <- lapply(1:All_dependent_cols_Nodal_MV, function(x) summary(Model_Nodal_MV[[x]]))

#Check samples used
CheckSamplesNodalPmv <- lapply(1:All_dependent_cols_Nodal_MV, function(x) length(resid(Model_Nodal_MV[[x]])))
CheckSamplesNodalPmv <- unlist(CheckSamplesNodalPmv)
CheckSamplesNodalPmv <- data.frame(CheckSamplesNodalPmv)
DependentNames <- colnames(Dependent_Nodal_MV)
CheckSamplesNodalPmv$Variable <- c(DependentNames)

CheckSamplesNodalPmv <- CheckSamplesNodalPmv[,c(2,1)]
CheckSamplesNodalPmv <- CheckSamplesNodalPmv[c(2,28,3,1,8,7,22,11,16,4,
                                               10,26,5,35,17,13,21,6,15,18,
                                               14,20,9,12,31,29,32,33,30,27,
                                               25,23,24,19,34),]

write.csv(CheckSamplesNodalPmv, "/Users/laurahudson/Documents/PMV_Nodal_Samples1.csv")

## Extract coefficients 

Regression_Results_Nodal_MV <- lapply(1:All_dependent_cols_Nodal_MV, function(x) Summary_Model_Nodal_MV[[x]]$coefficients)

Regression_Results_Nodal2_MV <- lapply(1:All_dependent_cols_Nodal_MV, function(x) as.data.frame(Regression_Results_Nodal_MV[[x]]))

n <- lapply(1:All_dependent_cols_Nodal_MV, function(x) nrow(Regression_Results_Nodal2_MV[[x]]))

## Get P-values

P_values_MV_Nodal <- lapply(1:All_dependent_cols_Nodal_MV, function(x) unname(coef(Summary_Model_Nodal_MV[[x]])[,'Pr(>|z|)'])[2:n[[x]]])

Adjusted_PVal_MV_Nodal <- lapply(1:All_dependent_cols_Nodal_MV, function(x) p.adjust(P_values_MV_Nodal[[x]], method = "fdr"))


### Get names of coefficients

CoefNames_MV_Nodal <- lapply(1:All_dependent_cols_Nodal_MV, function(x) names(coef(summary(Model_Nodal_MV[[x]]))[,'Pr(>|z|)'])[-1])

#### Create dataframe with OR, 95%CI and p-value for each dependent variable


OR_MV_Nodal <- lapply(1:All_dependent_cols_Nodal_MV, function(x) exp(Summary_Model_Nodal_MV[[x]]$coefficients[-1,1]))

CI_MV_Low_Nodal <- lapply(1:All_dependent_cols_Nodal_MV, function(x) exp(Summary_Model_Nodal_MV[[x]]$coefficients[-1,1] + (qnorm(0.025) * Summary_Model_Nodal_MV[[x]]$coefficients[-1,2])))


CI_MV_High_Nodal <- lapply(1:All_dependent_cols_Nodal_MV, function(x) exp(Summary_Model_Nodal_MV[[x]]$coefficients[-1,1] + (qnorm(0.975) * Summary_Model_Nodal_MV[[x]]$coefficients[-1,2])))


Nodal_MV_Regression_Results <- lapply(1:All_dependent_cols_Nodal_MV, function(x) cbind(CoefNames_MV_Nodal[[x]], OR_MV_Nodal[[x]], CI_MV_Low_Nodal[[x]], CI_MV_High_Nodal[[x]], P_values_MV_Nodal[[x]], Adjusted_PVal_MV_Nodal[[x]]))


Nodal_MV_Regression_Results <- lapply(1:All_dependent_cols_Nodal_MV, function(x) as.data.frame(Nodal_MV_Regression_Results[[x]]))



## Rename dependent variables

Dependent_Nodal_MV_Names <- colnames(Dependent_Nodal_MV)



### Subset dependent variables

Nodal_MV_Regression_Results2 <- lapply(1:All_dependent_cols_Nodal_MV, function(x) subset(Nodal_MV_Regression_Results[[x]], V1 == "Dependent_Nodal_MV[, x]"))


### Create dataframe of dependent variables

Nodal_MV_Regression_Results3 <- do.call('rbind',Nodal_MV_Regression_Results2) 

### Change column names

colnames(Nodal_MV_Regression_Results3) <- c("Variables", "OR", "95%CI_Low", "95%CI_High", "P_value", "Adjusted_PVal")


### Fill variable column with names of actual variables

Dependent_Nodal_MV_Names <- colnames(Dependent_Nodal_MV)
print(Dependent_Nodal_MV_Names)

Nodal_MV_Regression_Results3$Variables <- c(Dependent_Nodal_MV_Names)

Nodal_MV_Regression_Results3$Adjusted_PVal <- as.numeric(levels(Nodal_MV_Regression_Results3$Adjusted_PVal))[Nodal_MV_Regression_Results3$Adjusted_PVal]

### Format output table

library(formattable)


as.datatable(
  formattable(Nodal_MV_Regression_Results3, list(
    Variables = formatter("span",
                          style = ~ style(color = ifelse(Adjusted_PVal <= 0.05, "green", "red")))))
)



# For Peritoneum ----

## Subset significant variables from univariate analysis 

Peritoneal_Sig_Variables <- subset(Regression_Results_Peritoneum3, Adjusted_PVal<= 1000)

# Remove clinical variables for adjustment or as not needed

Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "T_Stage_Binary")
Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "N_Stage_Binary")
Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "M_Stage_Binary")
Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "Age")
Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "Sidedness_Binary")
Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "Cohort")
Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "Focus")
Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "Spinal")
Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "NewEpoc")
Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "Quasar2")
Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "Victor")

# Remove mut variables with frequency count < 30

Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "SETD2")
Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "MTOR")
Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "FAM123B")
Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "BCOR")
Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "TBX3")
Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "MLL2")
Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "CLSPN")
Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "ERBB4")

Peritoneal_Sig_Variables <- as.character(Peritoneal_Sig_Variables$Variables)


## Select these variables from Binary Merge 

Final_Merge_Selected_Peritoneal_MV <- dplyr::select(Final_Merge_Analysis, "Scort_ID", "Peritoneum_Met", Peritoneal_Sig_Variables, 
                                                    "T_Stage_Binary", "N_Stage_Binary", "M_Stage_Binary",
                                                    "Age", "Sidedness_Binary", "Gender",
                                                    "Cohort")




### Run regression for each dependent variable    

Dependent_Peritoneal_MV <- dplyr::select(Final_Merge_Selected_Peritoneal_MV, -one_of("Scort_ID", "Peritoneum_Met", "T_Stage_Binary", "N_Stage_Binary", "M_Stage_Binary",
                                                                                     "Age", "Sidedness_Binary", "Gender",
                                                                                     "Cohort"))

All_dependent_cols_Peritoneal_MV <- ncol(Dependent_Peritoneal_MV)






Model_Peritoneal_MV <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) glm(Peritoneum_Met ~ Dependent_Peritoneal_MV[,x]
                                                                                  + T_Stage_Binary + N_Stage_Binary
                                                                                  + M_Stage_Binary + Age + Sidedness_Binary
                                                                                  +Gender
                                                                                  + Cohort,
                                                                                  data = Final_Merge_Selected_Peritoneal_MV, family = binomial))



Summary_Model_Peritoneal_MV <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) summary(Model_Peritoneal_MV[[x]]))

#Check samples used
CheckSamplesPeritonealPmv <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) length(resid(Model_Peritoneal_MV[[x]])))
CheckSamplesPeritonealPmv <- unlist(CheckSamplesPeritonealPmv)
CheckSamplesPeritonealPmv <- data.frame(CheckSamplesPeritonealPmv)
DependentNames <- colnames(Dependent_Peritoneal_MV)
CheckSamplesPeritonealPmv$Variable <- c(DependentNames)

CheckSamplesPeritonealPmv <- CheckSamplesPeritonealPmv[,c(2,1)]
CheckSamplesPeritonealPmv <- CheckSamplesPeritonealPmv[c(2,28,3,1,8,7,22,11,16,4,
                                                         10,26,5,35,17,13,21,6,15,18,
                                                         14,20,9,12,31,29,32,33,30,27,
                                                         25,23,24,19,34),]

write.csv(CheckSamplesPeritonealPmv, "/Users/laurahudson/Documents/PMV_Peritoneal_Samples1.csv")


## Extract coefficients 

Regression_Results_Peritoneal_MV <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) Summary_Model_Peritoneal_MV[[x]]$coefficients)

Regression_Results_Peritoneal2_MV <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) as.data.frame(Regression_Results_Peritoneal_MV[[x]]))

n <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) nrow(Regression_Results_Peritoneal2_MV[[x]]))

## Get P-values

P_values_MV_Peritoneal <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) unname(coef(Summary_Model_Peritoneal_MV[[x]])[,'Pr(>|z|)'])[2:n[[x]]])

Adjusted_PVal_MV_Peritoneal <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) p.adjust(P_values_MV_Peritoneal[[x]], method = "fdr"))


### Get names of coefficients

CoefNames_MV_Peritoneal <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) names(coef(summary(Model_Peritoneal_MV[[x]]))[,'Pr(>|z|)'])[-1])

#### Create dataframe with OR, 95%CI and p-value for each dependent variable


OR_MV_Peritoneal <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) exp(Summary_Model_Peritoneal_MV[[x]]$coefficients[-1,1]))

CI_MV_Low_Peritoneal <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) exp(Summary_Model_Peritoneal_MV[[x]]$coefficients[-1,1] + (qnorm(0.025) * Summary_Model_Peritoneal_MV[[x]]$coefficients[-1,2])))


CI_MV_High_Peritoneal <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) exp(Summary_Model_Peritoneal_MV[[x]]$coefficients[-1,1] + (qnorm(0.975) * Summary_Model_Peritoneal_MV[[x]]$coefficients[-1,2])))


Peritoneal_MV_Regression_Results <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) cbind(CoefNames_MV_Peritoneal[[x]], OR_MV_Peritoneal[[x]], CI_MV_Low_Peritoneal[[x]], CI_MV_High_Peritoneal[[x]], P_values_MV_Peritoneal[[x]], Adjusted_PVal_MV_Peritoneal[[x]]))


Peritoneal_MV_Regression_Results <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) as.data.frame(Peritoneal_MV_Regression_Results[[x]]))


## Rename dependent variables

Dependent_Peritoneal_MV_Names <- colnames(Dependent_Peritoneal_MV)

### Subset dependent variables

Peritoneal_MV_Regression_Results2 <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) subset(Peritoneal_MV_Regression_Results[[x]], V1 == "Dependent_Peritoneal_MV[, x]"))


### Create dataframe of dependent variables

Peritoneal_MV_Regression_Results3 <- do.call('rbind',Peritoneal_MV_Regression_Results2) 

### Change column names

colnames(Peritoneal_MV_Regression_Results3) <- c("Variables", "OR", "95%CI_Low", "95%CI_High", "P_value", "Adjusted_PVal")


### Fill variable column with names of actual variables

Dependent_Peritoneal_MV_Names <- colnames(Dependent_Peritoneal_MV)
print(Dependent_Peritoneal_MV_Names)

Peritoneal_MV_Regression_Results3$Variables <- c(Dependent_Peritoneal_MV_Names)

Peritoneal_MV_Regression_Results3$Adjusted_PVal <- as.numeric(levels(Peritoneal_MV_Regression_Results3$Adjusted_PVal))[Peritoneal_MV_Regression_Results3$Adjusted_PVal]

rownames(Peritoneal_MV_Regression_Results3) <-NULL

### Format output table

library(formattable)


as.datatable(
  formattable(Peritoneal_MV_Regression_Results3, list(
    Variables = formatter("span",
                          style = ~ style(color = ifelse(Adjusted_PVal <= 0.05, "green", "red")))))
)




