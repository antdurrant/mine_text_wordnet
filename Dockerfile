FROM rocker/shiny-verse:latest

RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    xtail \
    wget \
    libpoppler-cpp-dev \
    libtesseract-dev \
    tesseract-ocr-eng 

# install Python 3.7 (Miniconda) and set path variable
RUN wget --quiet https://repo.anaconda.com/miniconda/Miniconda3-py37_4.8.2-Linux-x86_64.sh -O ~/miniconda.sh && \
    /bin/bash ~/miniconda.sh -b -p /opt/conda && \
    rm ~/miniconda.sh && \
    /opt/conda/bin/conda clean -tipsy && \
    ln -s /opt/conda/etc/profile.d/conda.sh /etc/profile.d/conda.sh && \
    echo ". /opt/conda/etc/profile.d/conda.sh" >> ~/.bashrc && \
    echo "conda activate base" >> ~/.bashrc
ENV PATH /opt/conda/bin:$PATH

# install nltk & download multilingual wordnet
RUN /opt/conda/bin/conda install nltk

RUN [ "python", "-c", "import nltk; nltk.download('omw')" ]

# let R know the right version of python to use
ENV RETICULATE_PYTHON "/opt/conda/bin/python3.7"

  
  # copy the setup script, run it, then delete it
COPY src/setup.R /
RUN Rscript setup.R && rm setup.R
  
COPY apps /srv/shiny-server/

EXPOSE 3838

