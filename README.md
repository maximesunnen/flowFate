# flowFate

FlowFate is a free, interactive [Shiny](https://shiny.rstudio.com/) web application developed to automate the analysis of cell differentiation data obtained by flow cytometry. We developed flowFate by combining existing R packages ([flowCore](https://www.bioconductor.org/packages/devel/bioc/vignettes/flowCore/inst/doc/HowTo-flowCore.pdf) and [flowWorkspace](https://www.bioconductor.org/packages/release/bioc/html/flowWorkspace.html) for FCS file import, data manipulation and gating; [ggcyto](https://www.bioconductor.org/packages/release/bioc/html/ggcyto.html) for visualization) into a customized workflow tailored to the analysis of differentiation data. We were able to automate a crucial and time-consuming process of data analysis using [openCyto's](https://www.bioconductor.org/packages/release/bioc/html/openCyto.html) data-driven gating functions.

Built with [{golem}](https://github.com/ThinkR-open/golem).

## Background
FlowFate is tailored to analyze cell differentiation data obtained according to [Chippalkatti et al](). Briefly, C2C12 are a special type of cells that can turn into muscle cells at some point but are not fully mature yet. Scientists use the term "undifferentiated" for these cells and once they've turned into muscle cells, they're called "differentiated".  Muscle cells can be characterised by the expression of a specific muscle protein called myosin. Therefore


## Dockerized the app

0. Makes sure that no folder `deploy` exists at the project root.

1. Follow steps in `dev/03_deploy.R`, namely:

Of note, {dockerfiler} is needed (it uses {pak}).

``` r
devtools::check()
devtools::build()
golem::add_dockerfile_with_renv_shinyproxy(output_dir = "deploy", lockfile = "renv.lock")
# if renv.lock.prod is missing
attachment::create_renv_for_prod()
file.copy("renv.lock.prod", "deploy/renv.lock.prod")
```

2. Should obtain a README in `deploy` that is:

```
docker build -f Dockerfile_base --progress=plain -t flowfate_base .
docker build -f Dockerfile --progress=plain -t flowfate:latest .
docker run -p 3838:3838 flowfate:latest
# then go to 127.0.0.1:3838
```

`deploy` folder contains:

```
Dockerfile
Dockerfile_base
flowFate_0.0.0.9000.tar.gz
README
renv.lock
renv.lock.prod
```


3.  `scp` the `deploy` folder to the \`shiny-sever\` in the **shinyproxy** `container_apps/` and execute **as root** the commands in the README

```
docker build -f Dockerfile_base --progress=plain -t flowfate_base .
docker build -f Dockerfile --progress=plain -t flowfate:latest .
```

4. edit `/home/aurelien.ginolhac/deploiement/container_system/shinyproxy-docker/application.yml`

adding something like:

```yaml
  - id: mdm200504
    display-name: MDM200504
    container-network: sp-network
    description: "Companion app for the MDM project 200504"
    container-cmd: ["R", "-e", "library(mdm200504);options('shiny.port'=3838,shiny.host='0.0.0.0');mdm200504::run_app()"]
    container-image: mdm200504
```

5. In `/home/aurelien.ginolhac/deploiement/`, run `./relance_machinerie` so the new app is linked.

## Installation

To run the app in R, install R and R Studio [here](https://posit.co/download/rstudio-desktop/), then run the commands below in your R Studio console.

This package has many dependencies, expect an installation time of roughly 30 minutes if you have none of those dependencies.

The app can be accessed inside the University of Luxembourg network at this URL: https://shiny-server.uni.lu/app/flowfate

``` r 
# install the flowFate package

install.packages(c("remotes", "BiocManager"))

# install dependencies from Bioconductor
BiocManager::install(c("openCyto", "ggcyto", "flowWorkspace", "flowCore"))

install.packages("flowFate")
remotes::install_github("maximesunnen/flowFate")
# open the app
flowFate::run_app()
```

