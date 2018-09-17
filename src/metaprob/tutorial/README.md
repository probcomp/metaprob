# Metaprob tutorial

This directory contains a work-in-progress tutorial for Metaprob, in the file `Tutorial.ipynb`.

To run it, you will need to:

1. Ensure you have Jupyter installed. Can you run `jupyter notebook` and start a Jupyter server?
2. Ensure lein is installed.
3. Install the Lein Jupyter kernel: `lein jupyter install-kernel`.
4. Inside the `metaprob-clojure` directory, run `lein jupyter notebook`. (If you prefer (and have installed) Jupyter Lab, you may use it (`lein jupyter lab`), but you will need to run `jupyter labextension install @jupyterlab/javascript-extension` for the visualization code to work.)
5. Running step 4 should open a web browser to the Jupyter Notebook (or Lab) application. Navigate to the `Tutorial.ipynb` file.

If you run into an error that your IO rate has been exceeded, try starting the server with this option:

`lein jupyter notebook --NotebookApp.iopub_data_rate_limit=10000000`

