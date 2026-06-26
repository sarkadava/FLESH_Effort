SETUP

If you continue from previous folders, you can skip steps 1-2

1) create virtual environment FLESH_TSPROCESS (we will use this environment for most of the processing steps)

cd FLESH_ContinuousBodilyEffort
conda env create -f environment.yml
conda activate FLESH_TSPROCESS

2) Add Conda Environment to Jupyter Notebook

python -m ipykernel install --user --name=FLESH_TSPROCESS --display-name "Python (FLESH_TSPROCESS)"

3) Download ConceptNet numberbatch 

In folder \numberbatch follow the url link to download the multilingual numberbatch (version 19.08) with word embeddings,
unzip the file

You can now run the notebook ConceptNet_similarity. Always make sure you are in the folder of the script you are running and that you selected kernel FLESH_TSPROCESS



