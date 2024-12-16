SETUP

If you continue from previous folders, you can skip steps 1) a

1) create virtual environment FLESH_TSPROCESS (we will use this environment for most of the processing steps)

conda create --name FLESH_TSPROCESS python=3.12.2

conda activate FLESH_TSPROCESS

2) install necessary packages

cd '\xxx\06_ConceptSimilarity'

pip install -r requirements_general.txt

3) Download ConceptNet numberbatch 

In folder \numberbatch follow the url link to download the multilingual numberbatch (version 19.08) with word embeddings,
unzip the file

4) open ConceptNetsimilarity.ipynb


