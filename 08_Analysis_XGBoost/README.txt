PYTHON SETUP

If you continue from previous folders, you can skip steps 1-3

1) create virtual environment FLESH_TSPROCESS (we will use this environment for most of the processing steps)

conda create --name FLESH_TSPROCESS python=3.12.2

conda activate FLESH_TSPROCESS

2) Add Conda Environment to Jupyter Notebook

pip install ipykernel
python -m ipykernel install --user --name=FLESH_TSPROCESS --display-name "Python (FLESH_TSPROCESS)"


3) install necessary packages

cd '\xxx\08_Analysis_XGBoost'
pip install -r requirements_general.txt

4) open 01_PCA_featureDimensions.ipynb


R SETUP

To run 02_XGBoost_effortIndicators.qmd, you will either need RStudio, or you can run it in Visual Studio Code with R extension.

In both cases, you will need to install all packages listed in the first chunk of the code.

