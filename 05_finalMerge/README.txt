SETUP

If you continue from folders 1-4, you don't need to install anything and can directly proceed to the script TS_mergeAnnotations.ipynb


1) create virtual environment FLESH_TSPROCESS (we will use this environment for most of the processing steps)

conda create --name FLESH_TSPROCESS python=3.12.2

conda activate FLESH_TSPROCESS

2) Add Conda Environment to Jupyter Notebook

pip install ipykernel
python -m ipykernel install --user --name=FLESH_TSPROCESS --display-name "Python (FLESH_TSPROCESS)"


3) install necessary packages

pip install -r requirements_general.txt