Guide and Overview of GitHub hendrikknoche/LRanalysis

The Github have been split up in four different folders with subfolders:


LR_Analysis_Code
	-In this folder all the code used to analysis the LR studies is located
	-The code we are working on right now is called �Study11 LR6b Analysis�
	-The subfolder called Functions contains r scripts for specific functions the main scripts can call. This is mean to make the main scripts a bit more manageable.
	-The subfolder called Results is meant for future graphs resulting from the analysis.
WAM_Analysis_Code
	-This folder contains the scripts used to analysis the WAM studies
	-We are not focusing on these at the moment and should be disregarded
Tables for database and raw data
	-Since all this data is already uploaded to the database there is no need to look in this file any further.
	-Contains four subfolders, one for each of the tables in the database (Studies, Handedness, touchEvents, and touchEventsTemporal). 
	-Each folder contains a merge file (which is the one uploaded to the database), and the raw data in separate files.
	-There is also an example of the script used to merge the files together. 
Copies of old code (just in case I F up)
	- This folder can be disregarded if I never make a mistake and is only there as a safety

	
.gitignore tells github what files not to push. Speceficlly the config.R file which contains four varbles: 
	ODBCUID = "USERNAME"
	pass = "PASSWORD"
Each user needs to create there own config.R file

