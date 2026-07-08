FROM inwt/r-shiny:4.4.3

RUN echo "options(repos = c(getOption('repos'), PANDORA = 'https://Pandora-IsoMemo.github.io/drat/'))" >> /usr/local/lib/R/etc/Rprofile.site

RUN Rscript -e "remotes::install_github('tidyverse/ellmer@v0.4.1')"

ADD . .

RUN installPackage

CMD ["Rscript", "inst/R_Code/startApplication.R"]
