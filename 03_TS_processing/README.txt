SETUP

If you continue from 01_XDF_processing and 02_MotionTracking_processing, you can skip step 1) 

1) create virtual environment FLESH_TSPROCESS (we will use this environment for most of the processing steps)

conda create --name FLESH_TSPROCESS

2) install necessary packages

conda activate FLESH_TSPROCESS

cd '\xxx\02_MotionTracking_processing'

pip install requirements.txt

3) You will also need Praat for extracting formants (download link here: https://www.fon.hum.uva.nl/praat/)

4) Extract the formants using Chris Carignan's Praat script directly in Praat (see here: https://github.com/ChristopherCarignan/formant-optimization).

Note that the script needs a lot of manual clicking for saving formants for each audio file.

5) Extract register of speakers using script Get_Speakers_register.praat directly in Praat

6) You are ready to start with the scripts as ordered by the initial numbers


