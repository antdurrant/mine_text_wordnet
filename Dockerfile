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


# install miniconda & add to path
RUN apt-get -qq update && apt-get -qq -y install curl bzip2 \
    && curl -sSL https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh -o /tmp/miniconda.sh \
    && bash /tmp/miniconda.sh -bfp /usr/local \
    && rm -rf /tmp/miniconda.sh \
    && conda install -y python=3 \
    && conda update conda \
    && apt-get -qq -y remove curl bzip2 \
    && apt-get -qq -y autoremove \
    && apt-get autoclean \
    && rm -rf /var/lib/apt/lists/* /var/log/dpkg.log \
    && conda clean --all --yes

ENV PATH /opt/conda/bin:$PATH

# install nltk & download multilingual wordnet
RUN conda install nltk 

RUN python -m nltk.downloader -d /usr/lib/nltk_data  wordnet wordnet_ic omw

ENV NLTK_DATA /usr/lib/nltk_data

  
  # copy the setup script, run it, then delete it
COPY src/setup.R /
RUN Rscript setup.R && rm setup.R
  
COPY apps/mine_text_wordnet /srv/shiny-server/

RUN rm /srv/shiny-server/index.html

RUN rm -rf /srv/shiny-server/sample-apps

EXPOSE 3838

