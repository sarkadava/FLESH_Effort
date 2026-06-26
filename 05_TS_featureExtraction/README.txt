SETUP

If you continue from previous folders, you don't need any additional setup

1) create virtual environment FLESH_TSPROCESS (we will use this environment for most of the processing steps)

cd FLESH_ContinuousBodilyEffort
conda env create -f environment.yml
conda activate FLESH_TSPROCESS

2) Add Conda Environment to Jupyter Notebook

python -m ipykernel install --user --name=FLESH_TSPROCESS --display-name "Python (FLESH_TSPROCESS)"

You can now run all scripts in this folder. Always make sure you are in the folder of the script you are running and that you selected kernel FLESH_TSPROCESS


