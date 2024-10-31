####################################
# Praat script to create textgrids #
# Created by                       #
# Joseph V. Casillas 10/11/2022    #
####################################


#### Enter the path to where the files are kept

form Enter information
	comment Folders where files are kept:
	sentence dirFiles ../../exp/empathy_intonation_perc/stim/wavs/
	sentence newDir ../../exp/empathy_intonation_perc/stim/wavs/
	positive number 1
endform


#### Prepare the loop
Create Strings as file list: "allFiles", dirFiles$ + "/*.wav"
select Strings allFiles
numberOfFiles = Get number of strings
clearinfo
#### Begin loop

for i from number to numberOfFiles
	select Strings allFiles
	fileName$ = Get string... i
	prefix$ = fileName$ - ".wav"
	Read from file... 'dirFiles$'/'fileName$'
	nameSound$ = selected$("Sound")
        To TextGrid: "utterance", ""
	select TextGrid 'nameSound$'
        select Sound 'nameSound$'
	plus TextGrid 'nameSound$'
	Edit
	pause Continue?
	select TextGrid 'nameSound$'
	Write to binary file... 'newDir$'/'nameSound$'.TextGrid
	select all
	minus Strings allFiles
	Remove
	printline 'nameSound$'	'i'
endfor