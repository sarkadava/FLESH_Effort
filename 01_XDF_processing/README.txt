Note that raw data are not available in this repository. They will be published in an upcoming work. 
If you are interested in viewing the data, contact kadava[et]leibniz[dot]de

SETUP

1) create virtual environment FLESH_TSPROCESS (we will use this environment for most of the processing steps)

cd FLESH_ContinuousBodilyEffort
conda env create -f environment.yml
conda activate FLESH_TSPROCESS

2) - Add Conda Environment to Jupyter Notebook
python -m ipykernel install --user --name=FLESH_TSPROCESS --display-name "Python (FLESH_TSPROCESS)"

3) We need to also install package `shign`. The source is already in the repository so we can just go to the folder and install it from there (see the Github for more info: https://github.com/KnurpsBram/shign)

cd shign
pip install .

4) You are ready to start with the scripts as ordered by the initial numbers. Always make sure you are in the folder of the script you are running and that you selected kernel FLESH_TSPROCESS







