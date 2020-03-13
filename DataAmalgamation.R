
# Install packages ----

#install.packages("dplyr")
library(dplyr)

#install.packages("plyr")
library(plyr)

#install.packages("Amelia")
library(Amelia)

# Read files into objects ----

Updated_Scort_Data <- read.csv("~/Downloads/panSCORT_271119.csv",  row.names =1,)
Old_Final_Merge <- read.csv("~/Documents/University/FHS/Data/Final_Merge_Clinical_Data.csv", row.names=1)

table(Updated_Scort_Data$CN_TP53)
table(Old_Final_Merge$CN_TP53)

# Select non-updated cohorts from old merge ----

Old_Merge_QnV <- Old_Final_Merge[Old_Final_Merge$Cohort == "VICTOR"
                                 | Old_Final_Merge$Cohort == "QUASAR2",]

table(Old_Merge_QnV$DDRD)

# Updated data - make column headings and data contents consistent ----

# Clinical data

colnames(Updated_Scort_Data) <- gsub("scort_id", "Scort_ID", colnames(Updated_Scort_Data))
Updated_Scort_Data$Scort_ID <- as.character(Updated_Scort_Data$Scort_ID)

colnames(Updated_Scort_Data) <- gsub("patient_id", "Patient_ID", colnames(Updated_Scort_Data))
Updated_Scort_Data$Patient_ID <- as.character(Updated_Scort_Data$Patient_ID)

colnames(Updated_Scort_Data) <- gsub("sample_type", "Sample_Type", colnames(Updated_Scort_Data))
Updated_Scort_Data$Sample_Type <- as.character(Updated_Scort_Data$Sample_Type)

colnames(Updated_Scort_Data) <- gsub("trial", "Block_Source", colnames(Updated_Scort_Data))
Updated_Scort_Data$Block_Source <- as.character(Updated_Scort_Data$Block_Source)

colnames(Updated_Scort_Data) <- gsub("work_strand", "Work_Strand", colnames(Updated_Scort_Data))
Updated_Scort_Data$Work_Strand <- as.character(Updated_Scort_Data$Work_Strand)

colnames(Updated_Scort_Data) <- gsub("Location", "Primary_Location", colnames(Updated_Scort_Data))
Updated_Scort_Data$Primary_Location <- as.character(Updated_Scort_Data$Primary_Location)

colnames(Updated_Scort_Data) <- gsub("OS_STATUS", "OS_Status", colnames(Updated_Scort_Data))
Updated_Scort_Data$OS_Status <- as.character(Updated_Scort_Data$OS_Status)

colnames(Updated_Scort_Data) <- gsub("OS_MONTHS", "OS_Time_Years", colnames(Updated_Scort_Data))
Updated_Scort_Data$OS_Time_Years <- Updated_Scort_Data$OS_Time_Years/12

colnames(Updated_Scort_Data) <- gsub("DFS_STATUS", "DFS_Status", colnames(Updated_Scort_Data))
Updated_Scort_Data$DFS_Status <- as.character(Updated_Scort_Data$DFS_Status)

colnames(Updated_Scort_Data) <- gsub("DFS_MONTHS", "DFS_Time_Years", colnames(Updated_Scort_Data))
Updated_Scort_Data$DFS_Time_Years <- Updated_Scort_Data$DFS_Time_Years/12

colnames(Updated_Scort_Data) <- gsub("PROGRESSION_FREE_SURVIVAL_STATUS", "PFS_Status", colnames(Updated_Scort_Data))
Updated_Scort_Data$PFS_Status <- as.character(Updated_Scort_Data$PFS_Status)

colnames(Updated_Scort_Data) <- gsub("PROGRESSION_FREE_SURVIVAL_MONTHS", "PFS_Time_Years", colnames(Updated_Scort_Data))
Updated_Scort_Data$PFS_Time_Years <- Updated_Scort_Data$PFS_Time_Years/12

### Primary tumour staging columns

colnames(Updated_Scort_Data) <- gsub("pT", "T_Stage", colnames(Updated_Scort_Data))
Updated_Scort_Data$T_Stage <- as.character(Updated_Scort_Data$T_Stage)

colnames(Updated_Scort_Data) <- gsub("pN", "N_Stage", colnames(Updated_Scort_Data))
Updated_Scort_Data$N_Stage <- as.character(Updated_Scort_Data$N_Stage)

colnames(Updated_Scort_Data) <- gsub("pM", "M_Stage", colnames(Updated_Scort_Data))
Updated_Scort_Data$M_Stage <- as.character(Updated_Scort_Data$M_Stage)

### Metastasis columns

colnames(Updated_Scort_Data) <- gsub("LiverMet", "Liver_Met", colnames(Updated_Scort_Data))
Updated_Scort_Data$Liver_Met <- as.character(Updated_Scort_Data$Liver_Met)

colnames(Updated_Scort_Data) <- gsub("NodeMet", "Node_Met", colnames(Updated_Scort_Data))
Updated_Scort_Data$Node_Met <- as.character(Updated_Scort_Data$Node_Met)

colnames(Updated_Scort_Data) <- gsub("LungMet", "Lung_Met", colnames(Updated_Scort_Data))
Updated_Scort_Data$Lung_Met <- as.character(Updated_Scort_Data$Lung_Met)

colnames(Updated_Scort_Data) <- gsub("PeritoneumMet", "Peritoneum_Met", colnames(Updated_Scort_Data))
Updated_Scort_Data$Peritoneum_Met <- as.character(Updated_Scort_Data$Peritoneum_Met)

Updated_Scort_Data$Brain_Met <- ""

colnames(Updated_Scort_Data) <- gsub("OtherMet", "Specify_Met", colnames(Updated_Scort_Data))
Updated_Scort_Data$Other_Met <- as.character(Updated_Scort_Data$Other_Met)

### Other 

colnames(Updated_Scort_Data) <- gsub("MSI_SCORT_Call", "MSI", colnames(Updated_Scort_Data))
Updated_Scort_Data$MSI <- as.character(Updated_Scort_Data$MSI)

colnames(Updated_Scort_Data) <- gsub("MSI_Call", "MSI", colnames(Updated_Scort_Data))
Updated_Scort_Data$MSI <- as.character(Updated_Scort_Data$MSI)

colnames(Updated_Scort_Data) <- gsub("Total_coding_muts", "Total_Mutations", colnames(Updated_Scort_Data))
Updated_Scort_Data$Total_Mutations <- as.character(Updated_Scort_Data$Total_Mutations)

colnames(Updated_Scort_Data) <- gsub("Metachronous_Synchronous", "Distant_Metastasis", colnames(Updated_Scort_Data))
Updated_Scort_Data$Distant_Metastasis <- as.character(Updated_Scort_Data$Distant_Metastasis)

colnames(Updated_Scort_Data) <- gsub("BRAF_other", "BRAF_others", colnames(Updated_Scort_Data))
Updated_Scort_Data$BRAF_others <- as.character(Updated_Scort_Data$BRAF_others)

colnames(Updated_Scort_Data) <- gsub("KRAS_other", "KRAS_others", colnames(Updated_Scort_Data))
Updated_Scort_Data$KRAS_others <- as.character(Updated_Scort_Data$KRAS_others)

colnames(Updated_Scort_Data) <- gsub("PIK3CA_other", "PIK3CA_others", colnames(Updated_Scort_Data))
Updated_Scort_Data$PIK3CA_others <- as.character(Updated_Scort_Data$PIK3CA_others)


colnames(Updated_Scort_Data) <- gsub("ddrd_score", "DDRD_Scores", colnames(Updated_Scort_Data))
Updated_Scort_Data$DDRD_Scores <- as.character(Updated_Scort_Data$DDRD_Scores)

colnames(Updated_Scort_Data) <- gsub("ddrd_call", "DDRD", colnames(Updated_Scort_Data))
Updated_Scort_Data$DDRD <- as.character(Updated_Scort_Data$DDRD)

### Organise cohort, block source, and workstrand columns (for updated Scort data)

table(Updated_Scort_Data$Cohort)

## Cohorts

Updated_Scort_Data$Cohort <- gsub("QC", "QC_Set", Updated_Scort_Data$Cohort)
Updated_Scort_Data$Cohort <- gsub("FT2", "COPERNICUS", Updated_Scort_Data$Cohort)
Updated_Scort_Data$Cohort <- gsub("WS2 FOCUS", "FOCUS", Updated_Scort_Data$Cohort)
Updated_Scort_Data$Cohort <- gsub("WS2 FOXTROT", "FOXTROT", Updated_Scort_Data$Cohort)
Updated_Scort_Data$Cohort <- gsub("WS3 GRAMPIAN", "GRAMPIAN", Updated_Scort_Data$Cohort)
Updated_Scort_Data$Cohort <- gsub("WS3 TREC", "TREC", Updated_Scort_Data$Cohort)
Updated_Scort_Data$Cohort <- gsub("WS4 POLYPS", "POLYPS", Updated_Scort_Data$Cohort)
Updated_Scort_Data$Cohort <- gsub("WS4 PT1s", "PT1_BELFAST", Updated_Scort_Data$Cohort)
Updated_Scort_Data$Cohort <- gsub("WS4 SPINAL", "SPINAL", Updated_Scort_Data$Cohort)
Updated_Scort_Data$Cohort <- gsub("WS5 FOCUS4C", "FOCUS_4C", Updated_Scort_Data$Cohort)
Updated_Scort_Data$Cohort <- gsub("WS5 NEW EPOC", "NEW EPOC", Updated_Scort_Data$Cohort)
Updated_Scort_Data$Cohort <- gsub("FOCUS IRI", "FOCUS_IRINOTECAN", Updated_Scort_Data$Cohort)

## Workstrands

Updated_Scort_Data$Work_Strand <- gsub("QC", "QC_Set", Updated_Scort_Data$Work_Strand)
Updated_Scort_Data$Work_Strand <- gsub("2", "WS2", Updated_Scort_Data$Work_Strand)
Updated_Scort_Data$Work_Strand <- gsub("3", "WS3", Updated_Scort_Data$Work_Strand)
Updated_Scort_Data$Work_Strand <- gsub("4", "WS4", Updated_Scort_Data$Work_Strand)
Updated_Scort_Data$Work_Strand <- gsub("5", "WS5", Updated_Scort_Data$Work_Strand)
Updated_Scort_Data$Work_Strand <- gsub("7", "WS7", Updated_Scort_Data$Work_Strand)
Updated_Scort_Data$Work_Strand <- gsub("FTWS2", "FT2", Updated_Scort_Data$Work_Strand)

colnames(Updated_Scort_Data)
colnames(Old_Final_Merge_QnV)


# Old data merge - make consistent ----

colnames(Updated_Scort_Data)
colnames(Old_Merge_QnV)

colnames(Old_Merge_QnV) <- gsub("OS_Time", "OS_Time_Years", colnames(Old_Merge_QnV))
Old_Merge_QnV$OS_Time_Years <- as.character(Old_Merge_QnV$OS_Time_Years)

colnames(Old_Merge_QnV) <- gsub("PFS_Time", "PFS_Time_Years", colnames(Old_Merge_QnV))
Old_Merge_QnV$PFS_Time_Years <- as.character(Old_Merge_QnV$PFS_Time_Years)

colnames(Old_Merge_QnV) <- gsub("Tumour_location", "Primary_Location", colnames(Old_Merge_QnV))
Old_Merge_QnV$Primary_Location <- as.character(Old_Merge_QnV$Primary_Location)

colnames(Old_Merge_QnV) <- gsub("Liver", "Liver_Met", colnames(Old_Merge_QnV))
Old_Merge_QnV$Liver_Met <- as.character(Old_Merge_QnV$Liver_Met)

colnames(Old_Merge_QnV) <- gsub("Lung", "Lung_Met", colnames(Old_Merge_QnV))
Old_Merge_QnV$Lung_Met <- as.character(Old_Merge_QnV$Lung_Met)

colnames(Old_Merge_QnV) <- gsub("Nodal", "Node_Met", colnames(Old_Merge_QnV))
Old_Merge_QnV$Node_Met <- as.character(Old_Merge_QnV$Node_Met)

colnames(Old_Merge_QnV) <- gsub("Peritoneal", "Peritoneum_Met", colnames(Old_Merge_QnV))
Old_Merge_QnV$Peritoneum_Met <- as.character(Old_Merge_QnV$Peritoneum_Met)

colnames(Old_Merge_QnV) <- gsub("Brain", "Brain_Met", colnames(Old_Merge_QnV))
Old_Merge_QnV$Brain_Met <- as.character(Old_Merge_QnV$Brain_Met)


# Join dataframes ----

Final_Merge_Updated <- rbind.fill(Updated_Scort_Data,  Old_Merge_QnV)
table(Final_Merge_Updated$CN_TP53)

# Further edits ----

table(Final_Merge_Updated$Gender)

# APC

Final_Merge_Updated$APC <- gsub("5", "", Final_Merge_Updated$APC)

# TNM Staging

Final_Merge_Updated$T_Stage <- gsub("T0", "0", Final_Merge_Updated$T_Stage)
Final_Merge_Updated$T_Stage <- gsub("T1", "1", Final_Merge_Updated$T_Stage)
Final_Merge_Updated$T_Stage <- gsub("T2", "2", Final_Merge_Updated$T_Stage)
Final_Merge_Updated$T_Stage <- gsub("T3", "3", Final_Merge_Updated$T_Stage)
Final_Merge_Updated$T_Stage <- gsub("T4", "4", Final_Merge_Updated$T_Stage)
Final_Merge_Updated$T_Stage <- gsub("x", "X", Final_Merge_Updated$T_Stage)
Final_Merge_Updated$T_Stage <- gsub("1,2", "1 or 2", Final_Merge_Updated$T_Stage)
Final_Merge_Updated$T_Stage <- gsub("3,4", "3 or 4", Final_Merge_Updated$T_Stage)
Final_Merge_Updated$T_Stage <- gsub("3a", "3", Final_Merge_Updated$T_Stage)
Final_Merge_Updated$T_Stage <- gsub("3b", "3", Final_Merge_Updated$T_Stage)
Final_Merge_Updated$T_Stage <- gsub("3c", "3", Final_Merge_Updated$T_Stage)

Final_Merge_Updated$N_Stage <- gsub("N0", "0", Final_Merge_Updated$N_Stage)
Final_Merge_Updated$N_Stage <- gsub("N1", "1", Final_Merge_Updated$N_Stage)
Final_Merge_Updated$N_Stage <- gsub("N2", "2", Final_Merge_Updated$N_Stage)
Final_Merge_Updated$N_Stage <- gsub("N3", "3", Final_Merge_Updated$N_Stage)
Final_Merge_Updated$N_Stage <- gsub("x", "X", Final_Merge_Updated$N_Stage)
Final_Merge_Updated$N_Stage <- gsub("1,2", "1 or 2", Final_Merge_Updated$N_Stage)

Final_Merge_Updated$M_Stage <- gsub("M0", "0", Final_Merge_Updated$M_Stage)
Final_Merge_Updated$M_Stage <- gsub("M1", "1", Final_Merge_Updated$M_Stage)
Final_Merge_Updated$M_Stage <- gsub("MX", "X", Final_Merge_Updated$M_Stage)
Final_Merge_Updated$M_Stage <- gsub("Mx", "X", Final_Merge_Updated$M_Stage)
Final_Merge_Updated$M_Stage <- gsub("x", "X", Final_Merge_Updated$M_Stage)
Final_Merge_Updated$M_Stage <- gsub("Not Stated", "", Final_Merge_Updated$M_Stage)
Final_Merge_Updated$M_Stage <- gsub("no data", "", Final_Merge_Updated$M_Stage)

table(Final_Merge_Updated$M_Stage)

# M_Stage data for NEW EPOC, QUASAR2, VICTOR, AND GRAMPIAN cohorts

Final_Merge_Updated$M_Stage[Final_Merge_Updated$Cohort == "NEW EPOC"] <- "1"

Final_Merge_Updated$M_Stage[Final_Merge_Updated$Cohort == "QUASAR2"
                            |Final_Merge_Updated$Cohort == "VICTOR"
                            |Final_Merge_Updated$Cohort == "GRAMPIAN"] <- "0"


# Primary_Location

table(Final_Merge_Updated$Primary_Location)

Final_Merge_Updated$Primary_Location <- gsub("colon", "Colon", Final_Merge_Updated$Primary_Location)
Final_Merge_Updated$Primary_Location <- gsub("ascending colon", "Ascending colon", Final_Merge_Updated$Primary_Location)
Final_Merge_Updated$Primary_Location <- gsub("ascending Colon", "Ascending colon", Final_Merge_Updated$Primary_Location)
Final_Merge_Updated$Primary_Location <- gsub("Ascending Colon", "Ascending colon", Final_Merge_Updated$Primary_Location)
Final_Merge_Updated$Primary_Location <- gsub("caecum", "Caecum", Final_Merge_Updated$Primary_Location)
Final_Merge_Updated$Primary_Location <- gsub("caecum_appendix", "Caecum appendix", Final_Merge_Updated$Primary_Location)
Final_Merge_Updated$Primary_Location <- gsub("Caecum_appendix", "Caecum appendix", Final_Merge_Updated$Primary_Location)
Final_Merge_Updated$Primary_Location <- gsub("descending Colon", "Descending colon", Final_Merge_Updated$Primary_Location)
Final_Merge_Updated$Primary_Location <- gsub("Descending Colon", "Descending colon", Final_Merge_Updated$Primary_Location)
Final_Merge_Updated$Primary_Location <- gsub("hepatic flexure", "Hepatic flexure", Final_Merge_Updated$Primary_Location)
Final_Merge_Updated$Primary_Location <- gsub("junction of descending and sigmoid", "Junction of descending and sigmoid", Final_Merge_Updated$Primary_Location)
Final_Merge_Updated$Primary_Location <- gsub("left Colon", "Left colon", Final_Merge_Updated$Primary_Location)
Final_Merge_Updated$Primary_Location <- gsub("Left Colon n_s", "Left colon", Final_Merge_Updated$Primary_Location)
Final_Merge_Updated$Primary_Location <- gsub("other please give details", "Other", Final_Merge_Updated$Primary_Location)
Final_Merge_Updated$Primary_Location <- gsub("rectosigmoid", "Rectosigmoid", Final_Merge_Updated$Primary_Location)
Final_Merge_Updated$Primary_Location <- gsub("rectosigmoid junction", "Rectosigmoid junction", Final_Merge_Updated$Primary_Location)
Final_Merge_Updated$Primary_Location <- gsub("rectum", "Rectum", Final_Merge_Updated$Primary_Location)
Final_Merge_Updated$Primary_Location <- gsub("right Colon", "Right colon", Final_Merge_Updated$Primary_Location)
Final_Merge_Updated$Primary_Location <- gsub("Right Colon n_s", "Right colon", Final_Merge_Updated$Primary_Location)
Final_Merge_Updated$Primary_Location <- gsub("sigmoid Colon", "Sigmoid colon", Final_Merge_Updated$Primary_Location)
Final_Merge_Updated$Primary_Location <- gsub("Sigmoid Colon", "Sigmoid colon", Final_Merge_Updated$Primary_Location)
Final_Merge_Updated$Primary_Location <- gsub("splenic flexure", "Splenic flexure", Final_Merge_Updated$Primary_Location)
Final_Merge_Updated$Primary_Location <- gsub("transverse Colon", "Transverse colon", Final_Merge_Updated$Primary_Location)
Final_Merge_Updated$Primary_Location <- gsub("Transverse Colon", "Transverse colon", Final_Merge_Updated$Primary_Location)


table(Final_Merge_Updated$Primary_Location)


# Sidedness (Left = 1; Right = 0)

Final_Merge_Updated$Sidedness[Final_Merge_Updated$Primary_Location == "Ascending colon"
                              |Final_Merge_Updated$Primary_Location == "Transverse colon"
                              |Final_Merge_Updated$Primary_Location == "Right colon"
                              |Final_Merge_Updated$Primary_Location == "Right Colon"
                              |Final_Merge_Updated$Primary_Location == "Caecum"
                              |Final_Merge_Updated$Primary_Location == "Caecum appendix"
                              |Final_Merge_Updated$Primary_Location == "Hepatic flexure"] <- "Right"

Final_Merge_Updated$Sidedness[Final_Merge_Updated$Primary_Location == "Rectum"
                              |Final_Merge_Updated$Primary_Location == "Left colon"
                              |Final_Merge_Updated$Primary_Location == "Left Colon"
                              |Final_Merge_Updated$Primary_Location == "Rectosigmoid junction"
                              |Final_Merge_Updated$Primary_Location == "Rectosigmoid"
                              |Final_Merge_Updated$Primary_Location == "Splenic flexure"
                              |Final_Merge_Updated$Primary_Location == "Sigmoid colon"
                              |Final_Merge_Updated$Primary_Location == "Junction of descending and sigmoid"
                              |Final_Merge_Updated$Primary_Location == "Descending colon"
                              |Final_Merge_Updated$Left_Colon == "1"
                              |Final_Merge_Updated$Rectum == "1"] <- "Left" 


Final_Merge_Updated$Sidedness <- gsub("Left", "1", Final_Merge_Updated$Sidedness)
Final_Merge_Updated$Sidedness <- gsub("Right", "0", Final_Merge_Updated$Sidedness)


# Sidedness - Coopernicus and Trec are rectal cancers  (so, "Right" = "0")

Final_Merge_Updated$Sidedness[Final_Merge_Updated$Cohort == "COPERNICUS"
                              |Final_Merge_Updated$Cohort == "TREC"] <- "0"


# Sites of metastasis

Final_Merge_Updated$Peritoneum_Met[Final_Merge_Updated$Specify_Met == "peritoneal"
                                   |Final_Merge_Updated$Primary_Location == "Peritoneal"
                                   |Final_Merge_Updated$Primary_Location == "Peritoneal deposits"
                                   |Final_Merge_Updated$Primary_Location == "peritoneal deposits"] <- "Yes"

Final_Merge_Updated$Liver_Met[Final_Merge_Updated$Specify_Met == "multiple liver metastases"
                              |Final_Merge_Updated$Specify_Met == "Liver"
                              |Final_Merge_Updated$Specify_Met == "FURTHER DISEASE PROGRESSION - LIVER NS LUNG METASTASES"
                              |Final_Merge_Updated$Specify_Met == "CT Multiple Pulmonary Mets + Liver Mets"] <- "Yes"

Final_Merge_Updated$Lung_Met[Final_Merge_Updated$Specify_Met == "CT Multiple Pulmonary Mets + Liver Mets"
                             |Final_Merge_Updated$Specify_Met == "histology confirms lung lesion as metastatic colorectal adenocarcinoma"
                             |Final_Merge_Updated$Specify_Met == "Lung"] <- "Yes"


# OS and PFS Status

Final_Merge_Updated$OS_Status <- gsub("DECEASED", "1", Final_Merge_Updated$OS_Status)
Final_Merge_Updated$OS_Status <- gsub("LIVING", "0", Final_Merge_Updated$OS_Status)

Final_Merge_Updated$PFS_Status <- gsub("Progressed", "1", Final_Merge_Updated$PFS_Status)
Final_Merge_Updated$PFS_Status <- gsub("Progression free", "0", Final_Merge_Updated$PFS_Status)
Final_Merge_Updated$PFS_Status <- gsub("Progression Free", "0", Final_Merge_Updated$PFS_Status)



## Remove undesired columns

Final_Merge_Updated$Liver_Met_Bin <- NULL
Final_Merge_Updated$Lung_Met_Bin <- NULL
Final_Merge_Updated$Peritoneum_Met_Bin <- NULL
Final_Merge_Updated$Node_Met_Bin <- NULL
Final_Merge_Updated$Other_Met_Bin <- NULL
Final_Merge_Updated$Other_Met <- NULL
Final_Merge_Updated$Response <- NULL
Final_Merge_Updated$Response_Binary <- NULL
Final_Merge_Updated$Met_Surgery <- NULL
Final_Merge_Updated$Met_Chemo <- NULL
Final_Merge_Updated$Rectum <- NULL
Final_Merge_Updated$Left_Colon <- NULL


Final_Merge_Updated$Peritoneum_Met[Final_Merge_Updated$Specify_Met == "peritoneal"
                                   | Final_Merge_Updated$Specify_Met == "Peritoneal"
                                   | Final_Merge_Updated$Specify_Met == "Peritoneal Deposits"
                                   | Final_Merge_Updated$Specify_Met == "PERITONEAL DISEASE"
                                   | Final_Merge_Updated$Specify_Met == "peritoneal deposits"] <- "Yes"

# Exclude samples in QC_Set from Focus

table(Final_Merge_Updated$Cohort)

Final_Merge_Updated$ForExclusion[Final_Merge_Updated$Cohort == "QC_Set"
                                 & Final_Merge_Updated$Block_Source == "FOCUS"] <- "Yes"

Final_Merge_Updated$ForExclusion[is.na(Final_Merge_Updated$ForExclusion)] <- "No"

Final_Merge_Updated <- Final_Merge_Updated[Final_Merge_Updated$ForExclusion == "No",]
Final_Merge_Updated$ForExclusion <- NULL

# Exclude metastatic samples from New Epoc (only has liver, lung, node metastases)

table(Final_Merge_Updated$Cohort, Final_Merge_Updated$Sample_Type)

Final_Merge_Updated$ForExclusion[Final_Merge_Updated$Cohort == "NEW EPOC"
                                 & Final_Merge_Updated$Sample_Type == "Liver metastasis"] <- "Yes"

table(Final_Merge_Updated$ForExclusion)

Final_Merge_Updated$ForExclusion[is.na(Final_Merge_Updated$ForExclusion)] <- "No"

Final_Merge_Updated <- Final_Merge_Updated[Final_Merge_Updated$ForExclusion == "No",]
Final_Merge_Updated$ForExclusion <- NULL

table(Final_Merge_Updated$Cohort)

Final_Merge_Updated <- subset(Final_Merge_Updated, Cohort == "FOCUS"
                              | Cohort == "FOCUS_4C"
                              | Cohort == "FOCUS_IRINOTECAN"
                              | Cohort == "NEW EPOC"
                              | Cohort == "QC_Set"
                              | Cohort == "SPINAL"
                              | Cohort == "VICTOR"
                              | Cohort == "QUASAR2")

# Save updated non-binarised merge -----

write.csv(Final_Merge_Updated, "/Users/laurahudson/Desktop/FHS_Data01/Merge/Final_Merge_Updated.csv")      

Final_Merge_Updated <- read.csv("~/Desktop/FHS_Data01/Merge/Final_Merge_Updated.csv", row.names =1, stringsAsFactors = FALSE, na.strings = c("", "NULL", "NA", " "))


# Check for missing values -----

sapply(Final_Merge_Updated,function(x) sum(is.na(x)))
sapply(Final_Merge_Updated, function(x) length(unique(x)))


# Numerally binarise variables for analysis ----

# Gender

Final_Merge_Updated$Gender <- gsub("Male", "1", Final_Merge_Updated$Gender)
Final_Merge_Updated$Gender <- gsub("Female", "0", Final_Merge_Updated$Gender)

# Metastases

Final_Merge_Updated$Liver_Met <- gsub("Yes", "1", Final_Merge_Updated$Liver_Met)
Final_Merge_Updated$Lung_Met <- gsub("Yes", "1", Final_Merge_Updated$Lung_Met)
Final_Merge_Updated$Node_Met <- gsub("Yes", "1", Final_Merge_Updated$Node_Met)
Final_Merge_Updated$Peritoneum_Met <- gsub("Yes", "1", Final_Merge_Updated$Peritoneum_Met)
Final_Merge_Updated$Brain_Met <- gsub("Yes", "1", Final_Merge_Updated$Brain_Met)

Final_Merge_Updated$Liver_Met <- gsub("No", "0", Final_Merge_Updated$Liver_Met)
Final_Merge_Updated$Lung_Met <- gsub("No", "0", Final_Merge_Updated$Lung_Met)
Final_Merge_Updated$Node_Met <- gsub("No", "0", Final_Merge_Updated$Node_Met)
Final_Merge_Updated$Peritoneum_Met <- gsub("No", "0", Final_Merge_Updated$Peritoneum_Met)
Final_Merge_Updated$Brain_Met <- gsub("No", "0", Final_Merge_Updated$Brain_Met)

# MSI (Create new column "MSI_Binary"; MSS = "0" and MSI = "1")

Final_Merge_Updated$MSI_Binary[Final_Merge_Updated$MSI=="MSS"] <- "0"
Final_Merge_Updated$MSI_Binary[Final_Merge_Updated$MSI=="MSI"] <- "1"

# Wt/Mut data (Wt = "0"; Mut = "1")

Final_Merge_Updated[] <- lapply(Final_Merge_Updated, gsub, pattern = "Wt", replacement = "0", fixed = T)
Final_Merge_Updated[] <- lapply(Final_Merge_Updated, gsub, pattern = "Mut", replacement = "1", fixed = T)

# Sidedness (Create new column "Sidedness_Binary"; Left = "1", Right = "0")

Final_Merge_Updated$Sidedness_Binary[Final_Merge_Updated$Sidedness=="0"] <- "0"
Final_Merge_Updated$Sidedness_Binary[Final_Merge_Updated$Sidedness=="1"] <- "1"

# BRAF Signature (Create new column; "BRAFm-like" = "1"; other = "0")

Final_Merge_Updated$BRAF_Signature_Binary[Final_Merge_Updated$BRAF_Signature=="pred-BRAFwt"] <- "0"
Final_Merge_Updated$BRAF_Signature_Binary[Final_Merge_Updated$BRAF_Signature=="BRAFm-like"] <- "1"



# T Stage (Create new column; T0/1/2 = "0", T3/4 = "1")

Final_Merge_Updated$T_Stage_Binary[Final_Merge_Updated$T_Stage == "0"
                                   |Final_Merge_Updated$T_Stage == "1"
                                   |Final_Merge_Updated$T_Stage == "2"
                                   |Final_Merge_Updated$T_Stage == "1 or 2"] <- "0"

Final_Merge_Updated$T_Stage_Binary[Final_Merge_Updated$T_Stage == "3"
                                   |Final_Merge_Updated$T_Stage == "4"
                                   |Final_Merge_Updated$T_Stage == "3 or 4"] <- "1"

# N Stage (Create new column; N0 = "0", N1/2 = "1")

Final_Merge_Updated$N_Stage_Binary[Final_Merge_Updated$N_Stage == "0"] <- "0"

Final_Merge_Updated$N_Stage_Binary[Final_Merge_Updated$N_Stage == "1"
                                   |Final_Merge_Updated$N_Stage == "2"
                                   |Final_Merge_Updated$N_Stage == "1 or 2"] <- "1"

# M Stage (Create new column; M0/X = "0", M1 = "1")

Final_Merge_Updated$M_Stage_Binary[Final_Merge_Updated$M_Stage == "0"
                                   |Final_Merge_Updated$M_Stage == "X"] <- "0"

Final_Merge_Updated$M_Stage_Binary[Final_Merge_Updated$M_Stage == "1"] <- "1"

# CIN_DNN (Create new column; + = "1")

Final_Merge_Updated$CIN_DNN_Binary[Final_Merge_Updated$CIN_DNN == "CIN-"] <- "0"
Final_Merge_Updated$CIN_DNN_Binary[Final_Merge_Updated$CIN_DNN == "CIN+"] <- "1"


# Binarise categorical variables for analysis ----

## Cohorts

table(Final_Merge_Updated$Cohort)

#Focus
Final_Merge_Updated$Focus[Final_Merge_Updated$Cohort == "FOCUS"] <- "1"
Final_Merge_Updated$Focus[Final_Merge_Updated$Cohort != "FOCUS"] <- "0"

# New Epoc
Final_Merge_Updated$NewEpoc[Final_Merge_Updated$Cohort == "NEW EPOC"] <- "1"
Final_Merge_Updated$NewEpoc[Final_Merge_Updated$Cohort != "NEW EPOC"] <- "0"

# Quasar 2
Final_Merge_Updated$Quasar2[Final_Merge_Updated$Cohort == "QUASAR2"] <- "1"
Final_Merge_Updated$Quasar2[Final_Merge_Updated$Cohort != "QUASAR2"] <- "0"

# Spinal
Final_Merge_Updated$Spinal[Final_Merge_Updated$Cohort == "SPINAL"] <- "1"
Final_Merge_Updated$Spinal[Final_Merge_Updated$Cohort != "SPINAL"] <- "0"

# Victor
Final_Merge_Updated$Victor[Final_Merge_Updated$Cohort == "VICTOR"] <- "1"
Final_Merge_Updated$Victor[Final_Merge_Updated$Cohort != "VICTOR"] <- "0"

# Focus Irinotecan
Final_Merge_Updated$Focus_Irinotecan[Final_Merge_Updated$Cohort == "FOCUS_IRINOTECAN"] <- "1"
Final_Merge_Updated$Focus_Irinotecan[Final_Merge_Updated$Cohort != "FOCUS_IRINOTECAN"] <- "0"

# Focus4C
Final_Merge_Updated$Focus_4C[Final_Merge_Updated$Cohort == "FOCUS_4C"] <- "1"
Final_Merge_Updated$Focus_4C[Final_Merge_Updated$Cohort != "FOCUS_4C"] <- "0"

# QC_Set
Final_Merge_Updated$QC_Set[Final_Merge_Updated$Cohort == "QC_Set"] <- "1"
Final_Merge_Updated$QC_Set[Final_Merge_Updated$Cohort != "QC_Set"] <- "0"



## CRIS (A,B,C,D,E)

# A
Final_Merge_Updated$CRIS_A[Final_Merge_Updated$CRIS == "CRIS-A"] <- "1"
Final_Merge_Updated$CRIS_A[Final_Merge_Updated$CRIS == "CRIS-B"
                           |Final_Merge_Updated$CRIS == "CRIS-C"
                           |Final_Merge_Updated$CRIS == "CRIS-D"
                           |Final_Merge_Updated$CRIS == "Unclassified"
                           |Final_Merge_Updated$CRIS == "CRIS-E" ] <- "0"

# B
Final_Merge_Updated$CRIS_B[Final_Merge_Updated$CRIS == "CRIS-B"] <- "1"
Final_Merge_Updated$CRIS_B[Final_Merge_Updated$CRIS == "CRIS-A"
                           |Final_Merge_Updated$CRIS == "CRIS-C"
                           |Final_Merge_Updated$CRIS == "CRIS-D"
                           |Final_Merge_Updated$CRIS == "Unclassified"
                           |Final_Merge_Updated$CRIS == "CRIS-E"] <- "0"

# C
Final_Merge_Updated$CRIS_C[Final_Merge_Updated$CRIS == "CRIS-C"] <- "1"
Final_Merge_Updated$CRIS_C[Final_Merge_Updated$CRIS == "CRIS-B"
                           |Final_Merge_Updated$CRIS == "CRIS-A"
                           |Final_Merge_Updated$CRIS == "CRIS-D"
                           |Final_Merge_Updated$CRIS == "Unclassified"
                           |Final_Merge_Updated$CRIS == "CRIS-E"] <- "0"

# D
Final_Merge_Updated$CRIS_D[Final_Merge_Updated$CRIS == "CRIS-D"] <- "1"
Final_Merge_Updated$CRIS_D[Final_Merge_Updated$CRIS == "CRIS-B"
                           |Final_Merge_Updated$CRIS == "CRIS-C"
                           |Final_Merge_Updated$CRIS == "CRIS-A"
                           |Final_Merge_Updated$CRIS == "Unclassified"
                           |Final_Merge_Updated$CRIS == "CRIS-E"] <- "0"


# E
Final_Merge_Updated$CRIS_E[Final_Merge_Updated$CRIS == "CRIS-E"] <- "1"
Final_Merge_Updated$CRIS_E[Final_Merge_Updated$CRIS == "CRIS-B"
                           |Final_Merge_Updated$CRIS == "CRIS-C"
                           |Final_Merge_Updated$CRIS == "CRIS-D"
                           |Final_Merge_Updated$CRIS == "Unclassified"
                           |Final_Merge_Updated$CRIS == "CRIS-A"] <- "0"


#CMS (1,2,3,4)


# 1
Final_Merge_Updated$CMS1[Final_Merge_Updated$CMS == "CMS1"] <- "1"
Final_Merge_Updated$CMS1[Final_Merge_Updated$CMS == "CMS2"
                         |Final_Merge_Updated$CMS == "Unclassified"
                         |Final_Merge_Updated$CMS == "CMS3"
                         |Final_Merge_Updated$CMS == "CMS4"] <- "0"

# 2
Final_Merge_Updated$CMS2[Final_Merge_Updated$CMS == "CMS2"] <- "1"
Final_Merge_Updated$CMS2[Final_Merge_Updated$CMS == "CMS1"
                         |Final_Merge_Updated$CMS == "Unclassified"
                         |Final_Merge_Updated$CMS == "CMS3"
                         |Final_Merge_Updated$CMS == "CMS4"] <- "0"

# 3
Final_Merge_Updated$CMS3[Final_Merge_Updated$CMS == "CMS3"] <- "1"
Final_Merge_Updated$CMS3[Final_Merge_Updated$CMS == "CMS2"
                         |Final_Merge_Updated$CMS == "Unclassified"
                         |Final_Merge_Updated$CMS == "CMS1"
                         |Final_Merge_Updated$CMS == "CMS4"] <- "0"

# 4
Final_Merge_Updated$CMS4[Final_Merge_Updated$CMS == "CMS4"] <- "1"
Final_Merge_Updated$CMS4[Final_Merge_Updated$CMS == "CMS2"
                         |Final_Merge_Updated$CMS == "Unclassified"
                         |Final_Merge_Updated$CMS == "CMS3"
                         |Final_Merge_Updated$CMS == "CMS1"] <- "0"


## Copy number data

#CN_SMAD4_isLoss (Loss (1) = -2, -1; No_Loss (0) = 0, 1, 2)


Final_Merge_Updated$CN_SMAD4_isLoss[Final_Merge_Updated$CN_SMAD4 == "-2" |
                                      Final_Merge_Updated$CN_SMAD4 == "-1"] <- "1"

Final_Merge_Updated$CN_SMAD4_isLoss[Final_Merge_Updated$CN_SMAD4 == "2" |
                                      Final_Merge_Updated$CN_SMAD4 == "1" |
                                      Final_Merge_Updated$CN_SMAD4 == "0"] <- "0"


#CN_TP53_isLoss (Loss (1) = -2, -1; No_Loss (0) = 0, 1, 2)

prop.table(table(Final_Merge_Updated$CN_TP53))* 100
table(Final_Merge_Updated$CN_TP53)

Final_Merge_Updated$CN_TP53_isLoss[Final_Merge_Updated$CN_TP53 == "-2" |
                                     Final_Merge_Updated$CN_TP53 == "-1"] <- "1"

Final_Merge_Updated$CN_TP53_isLoss[Final_Merge_Updated$CN_TP53 == "2" |
                                     Final_Merge_Updated$CN_TP53 == "1" |
                                     Final_Merge_Updated$CN_TP53 == "0"] <- "0"



## CN_08_p_isLoss (Loss (1) = "Loss"; No loss (0) = "Neutral", "Gain")


Final_Merge_Updated$CN_08_p_isLoss[Final_Merge_Updated$CN_08_p == "Loss"] <- "1"

Final_Merge_Updated$CN_08_p_isLoss[Final_Merge_Updated$CN_08_p == "Neutral" |
                                     Final_Merge_Updated$CN_08_p == "Gain"] <- "0"


## CN_08_q_isGain (Gain (1) = "Gain"; No loss (0) = "Neutral", "Loss")

Final_Merge_Updated$CN_08_q_isGain[Final_Merge_Updated$CN_08_q == "Gain"] <- "1"

Final_Merge_Updated$CN_08_q_isGain[Final_Merge_Updated$CN_08_q == "Neutral" |
                                     Final_Merge_Updated$CN_08_q == "Loss"] <- "0"


## CN_APC


Final_Merge_Updated$CN_APC_isLoss[Final_Merge_Updated$CN_APC == "-2"
                                  | Final_Merge_Updated$CN_APC == "-1"] <- "1"

Final_Merge_Updated$CN_APC_isLoss[Final_Merge_Updated$CN_APC == "0" |
                                    Final_Merge_Updated$CN_APC == "1"] <- "0"

table(Final_Merge_Updated$CN_APC_isLoss)

Final_Merge_Updated$CN_APC_isGain[Final_Merge_Updated$CN_APC == "1"] <- "1"

Final_Merge_Updated$CN_APC_isGain[Final_Merge_Updated$CN_APC == "-1" |
                                    Final_Merge_Updated$CN_APC == "-2"
                                  | Final_Merge_Updated$CN_APC == "0"] <- "0"

# Remove rows w/o metastasis to specified sites  ----

# Summarise metastases

table(Final_Merge_Updated$Cohort)

table(Final_Merge_Updated$Liver_Met)
table(Final_Merge_Updated$Lung_Met)
table(Final_Merge_Updated$Node_Met)
table(Final_Merge_Updated$Peritoneum_Met)

# Create addition column for met data at specified site

Final_Merge_Updated$Specific_Met_Data[Final_Merge_Updated$Liver_Met == "1"
                                      | Final_Merge_Updated$Liver_Met == "0" ] <- "Y"

Final_Merge_Updated$Specific_Met_Data[Final_Merge_Updated$Lung_Met == "1"
                                      | Final_Merge_Updated$Lung_Met == "0" ] <- "Y"

Final_Merge_Updated$Specific_Met_Data[Final_Merge_Updated$Node_Met == "1"
                                      | Final_Merge_Updated$Node_Met == "0" ] <- "Y"

Final_Merge_Updated$Specific_Met_Data[Final_Merge_Updated$Peritoneum_Met == "1"
                                      | Final_Merge_Updated$Peritoneum_Met == "0" ] <- "Y"

table(Final_Merge_Updated$Specific_Met_Data)

# Remove rows without data on metastatis to lungs, liver, node, or peritoneum

Final_Merge_Updated$Specific_Met_Data[is.na(Final_Merge_Updated$Specific_Met_Data)] <- "N"
table(Final_Merge_Updated$Specific_Met_Data)

Final_Merge_Analysis <- subset(Final_Merge_Updated, subset = (Specific_Met_Data == "Y"))
table(Final_Merge_Analysis$Specific_Met_Data)
Final_Merge_Analysis$Specific_Met_Data <- NULL

# Re-check mets values

table(Final_Merge_Analysis$Liver_Met)
table(Final_Merge_Analysis$Lung_Met)
table(Final_Merge_Analysis$Node_Met)
table(Final_Merge_Analysis$Peritoneum_Met)

table(Final_Merge_Analysis$Cohort)

prop.table(table(Final_Merge_Analysis$CN_TP53_isLoss))

# Save dataframe for analysis -----

write.csv(Final_Merge_Analysis, "/Users/laurahudson/Desktop/FHS_Data01/Merge/Final_Merge_Analysis.csv")      

# Check missing data ----