SETUP

1) create virtual environment FLESH_TSPROCESS (we will use this environment for most of the processing steps)

conda create --name FLESH_TSPROCESS python=3.12.2

conda activate FLESH_TSPROCESS

2) - Add Conda Environment to Jupyter Notebook
pip install ipykernel
python -m ipykernel install --user --name=FLESH_TSPROCESS --display-name "Python (FLESH_TSPROCESS)"

3) install necessary packages

cd '\xxx\01_XDF_PROCESSING'

pip install -r requirements_general.txt


4) We need to also install package `shign`. The source is already in the repository so we can just go to the folder and install it from there (see the Github for more info: https://github.com/KnurpsBram/shign)

cd shign
pip install .

5) open xdf_workflow.ipynb






