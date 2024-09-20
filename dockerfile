# Use the official Jupyter base notebook image with Python 3.11
FROM quay.io/jupyter/scipy-notebook:python-3.11

LABEL maintainer="Hu Chuan-Peng <hcp4715@hotmail.com>"

# Set environment variables to minimize Docker image size
ENV CONDA_AUTO_UPDATE_CONDA=false \
    PATH="/opt/conda/bin:$PATH"

USER root
RUN apt-get update && apt-get install -y graphviz
# Create a new conda environment and install packages
RUN conda install -y \
    graphviz \
    bambi=0.13.0 \
    pymc=5.16.2 \
    PreliZ=0.9.0 \
    ipympl=0.9.4 \
    pingouin=0.5.4 && \
    conda clean --all --yes

# Remove cache and unused packages to reduce image size
RUN rm -rf /home/jovyan/.cache && \
    conda clean --all --yes && \
    fix-permissions /home/jovyan

# Set the working directory
USER $NB_UID
WORKDIR $HOME

# Expose the default Jupyter notebook port
EXPOSE 8888

# Command to start Jupyter Notebook
CMD ["start-notebook.sh"]