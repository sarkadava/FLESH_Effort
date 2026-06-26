SETUP

If you continue from 01_XDF_processing and 02_MotionTracking_processing, you can skip step 1-2

1) create virtual environment FLESH_TSPROCESS (we will use this environment for most of the processing steps)

cd FLESH_ContinuousBodilyEffort
conda env create -f environment.yml
conda activate FLESH_TSPROCESS

2) - Add Conda Environment to Jupyter Notebook
python -m ipykernel install --user --name=FLESH_TSPROCESS --display-name "Python (FLESH_TSPROCESS)"

3) You will also need Praat for extracting formants (download link here: https://www.fon.hum.uva.nl/praat/)

4) Extract the formants using Chris Carignan's Praat script directly in Praat (see here: https://github.com/ChristopherCarignan/formant-optimization).

5) Extract register of speakers using script Get_Speakers_register.praat directly in Praat

6) You are ready to start with the scripts as ordered by the initial numbers. Always make sure you are in the folder of the script you are running and that you selected kernel FLESH_TSPROCESS


