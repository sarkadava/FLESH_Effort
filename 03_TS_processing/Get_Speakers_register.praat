# script Get_Speakers_register.praat
# Author: Celine De Looze
# email: celine.delooze@lpl.univ-aix.fr
# purpose: Get f0 min, max, sd, key (median) and span (max-min) for each sound file (ex: each speaker)
# requires: none


clearinfo

form calculate_register
	#indicate where your sound files and TextGrid are
	sentence input_folder E:\FLESH_ContinuousBodilyEffort\TS_processing\Audio\
	#indicate where you want your output to be saved
	sentence output_folder E:\FLESH_ContinuousBodilyEffort\TS_processing\
endform

nocheck Remove

myList = Create Strings as file list... liste 'input_folder$'\*.wav

ns = Get number of strings

line$="FILE'tab$'f0min'tab$'f0max'tab$'f0sd'tab$'key'tab$'span'newline$'"
line$>'output_folder$'\SpeakerRegister.txt
#Indicate instead of "SpeakerRegister" (if you want to change it) your output file name


for i from 1 to ns
	select Strings liste
	name$ = Get string... 'i'
	Read from file... 'input_folder$'\'name$'
	mySound=selected("Sound")
	mySound$=selected$("Sound")
	
	pitch_step = 0.01
	To Pitch... 'pitch_step' 30 800
	myPitch=selected("Pitch")
	myPitch$=selected$("Pitch")
	minimum_f0= Get minimum... 0 0 Hertz Parabolic
	maximum_f0= Get maximum... 0 0 Hertz Parabolic
	q65 = Get quantile... 0.0 0.0 0.65 Hertz
	q15 = Get quantile... 0.0 0.0 0.15 Hertz
	
	max_f0 = 10*ceiling((1.92*q65)/10)
	min_f0 = 10*floor((0.83*q15)/10)

	if max_f0 = undefined
		max_f0 = 800
	endif

	if min_f0 = undefined
		min_f0 = 30
	endif

	select mySound
	To Pitch... 'pitch_step' 'min_f0' 'max_f0'
	myPitch2=selected("Pitch")
	myPitch2$=selected$("Pitch")

	min= Get minimum... 0 0 Hertz Parabolic
	max= Get maximum... 0 0 Hertz Parabolic
	key = Get quantile... 0 0 0.5 Hertz
	span = log2(max/min)
	sd= Get standard deviation... 0 0 hertz

	line$="'mySound$''tab$''min:0''tab$''max:0''tab$''sd:3''tab$''key:0''tab$''span:3''newline$'"
	line$>>'output_folder$'\SpeakerRegister.txt
	

endfor

select all
minus Strings liste
Remove


	
