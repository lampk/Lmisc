Process for an interim analysis:
- Create a new folder with the same structure as InterimAnalysisPreparation (subfolders Analysis, Data, Output)
- Copy VAD's based on the interim analysis snapshot from folder Data\VAD_csv (note: when the final analysis is finish, this folder only stores the final VAD) to Data
- Copy Dummy-randomization list from folder Data\Randomization_list to Data folder
- analysis.R in Analysis folder should then create all the outputs

DSMB statistician needs to:
- Put the true randomization list in the Data folder (and remove the dummy list)
- change lines 5,6 and 14 of the code in analysis.R