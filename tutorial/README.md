# Metaprob tutorial

This directory contains a work-in-progress tutorial for Metaprob, in the file `Tutorial.ipynb`.

To run it you will need to run the following commands from the root of this repository (in the same directory as `Makefile`):

1. Ensure `docker` and `docker-compose` are installed. Can you run `docker version` and `docker-compose version`?
2. Build the Docker image with `make docker-build`.
3. Run the Docker image with `make docker-notebook`. A URL like `http://(<container-id> or 127.0.0.1):8888/?token=<token>` will be printed.
4. Copy the URL from your terminal into the address bar of your browser.
5. Replace `(<container-id> or 127.0.0.1)` with `127.0.0.1`. This should leave you with a URL like `http://127.0.0.1:8888/?token=<token>`.
6. Hit enter to navigate to the provided URL.
7. Click on `Tutorial.ipynb`.
