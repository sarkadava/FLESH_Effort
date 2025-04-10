SETUP

If you continue from previous folders, you can skip steps 1-2

1) create virtual environment FLESH_TSPROCESS (we will use this environment for most of the processing steps)

conda create --name FLESH_TSPROCESS python=3.12.2

conda activate FLESH_TSPROCESS

2) Add Conda Environment to Jupyter Notebook

pip install ipykernel
python -m ipykernel install --user --name=FLESH_TSPROCESS --display-name "Python (FLESH_TSPROCESS)"


3) install necessary packages

cd '\xxx\06_ConceptSimilarity'

pip install -r requirements_general.txt

4) Download ConceptNet numberbatch 

In folder \numberbatch follow the url link to download the multilingual numberbatch (version 19.08) with word embeddings,
unzip the file

5) open ConceptNetsimilarity.ipynb


