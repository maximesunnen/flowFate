# flowFate

FlowFate is a free, interactive [Shiny](https://shiny.rstudio.com/) web application developed to automate the analysis of cell differentiation data obtained by flow cytometry. 
We developed flowFate by combining existing R packages

- [flowCore]([https://www.bioconductor.org/packages/devel/bioc/vignettes/flowCore/inst/doc/HowTo-flowCore.pdf](https://bioconductor.org/packages/release/bioc/html/flowCore.html)) and [flowWorkspace](https://www.bioconductor.org/packages/release/bioc/html/flowWorkspace.html) for FCS file import data manipulation and gating
- [ggcyto](https://www.bioconductor.org/packages/release/bioc/html/ggcyto.html) for visualization

into a customized workflow tailored to the analysis of differentiation data. We were able to automate a crucial and time-consuming process of data analysis using [openCyto's](https://www.bioconductor.org/packages/release/bioc/html/openCyto.html) data-driven gating functions.

Built with [{golem}](https://github.com/ThinkR-open/golem).

## Background
<p align="justify"> FlowFate is tailored to analyze cell differentiation data obtained according to [Chippalkatti et al](). Briefly, C2C12 are a type of cell with the ability to turn into (they "differentiate") into muscle cells. Until they do this, they're in an immature state and scientists call them "undifferentiated".Technically, one could analyze a pool of C2C12 cells under the microscope visually determine which cells are differentiated and which are not. Practically this would be way too tedious. Luckily, differentiated muscle cells produce a specific protein called myosin that undifferentiated cells do not produce (or in low amounts). One can stain this protein with an antibody attached to a fluorescent molecule and analyze the pool of cells using a flow cytometer. A flow cytometer is a device that channels cells in a small capillary so that they can pass in front of a laser one-by-one. This laser light excites the fluorescent molecule attached to our antibody (itself attached to myosin), which in response also emits light (of a different color than the laser light) that can ultimately be captured by a camera. Depending on how much of this light is captured, one can determine if a cell is differentiated or not (remember that even undifferentiatde cells can produce a small amount of myosin and we're only interested in those cells that produce a lot of it). </p>

## Installation

To run the app in R, install R and R Studio [here](https://posit.co/download/rstudio-desktop/), then run the commands below in your R Studio console, in the defined order.

Because of the large amount of dependencies, it can take ~ 15 minutes to install the app. Users having access to the network of the University of Luxembourg can open the app by clicking on the following link: https://shiny-server.uni.lu/app/flowfate. 

``` r 
# install the remotes and BiocManager package
install.packages(c("remotes", "BiocManager"))

# install dependencies from Bioconductor
BiocManager::install(c("ggcyto", "flowWorkspace", "flowCore"))
remotes::install_github("openCyto")

# install the flowFate package
remotes::install_github("maximesunnen/flowFate")

# open the app
flowFate::run_app()
```

## Dockerization

0. Makes sure that no folder `deploy` exists at the project root.

1. Follow steps in `dev/03_deploy.R`, namely:

Note: {dockerfiler} is needed ( uses {pak}).

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

Make sure to use the binary versions for Linux from [PPPM](https://packagemanager.posit.co)

`RUN R -e "renv::restore(repos = 'https://packagemanager.posit.co/cran/__linux__/jammy/latest/')"`

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
