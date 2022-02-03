FROM bmsc-shiny-base:latest

ADD . .

RUN installPackage

CMD ["Rscript", "inst/R_Code/startApplication.R"]
