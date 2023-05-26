# flowFate

FlowFate is a free, interactive [Shiny](https://shiny.rstudio.com/) web application developed to automate the analysis of cell differentiation data obtained by flow cytometry. 
We developed flowFate by combining existing R packages

- [flowCore](https://bioconductor.org/packages/release/bioc/html/flowCore.html) and [flowWorkspace](https://www.bioconductor.org/packages/release/bioc/html/flowWorkspace.html) for FCS file import data manipulation and gating
- [ggcyto](https://www.bioconductor.org/packages/release/bioc/html/ggcyto.html) for visualization

into a customized workflow tailored to the analysis of differentiation data. We were able to automate a crucial and time-consuming process of data analysis using [openCyto's](https://www.bioconductor.org/packages/release/bioc/html/openCyto.html) data-driven gating functions.

Built with [{golem}](https://github.com/ThinkR-open/golem).

## Background
<p align="justify"> FlowFate has been designed for the analysis of cell differentiation data obtained according to [Chippalkatti et al's]() protocol. Briefly, C2C12 cells that can turn into (or "differentiate") into muscle cells at some point in their lifecycle. Until they differentiate, these cells are in an immature state (scientists call them "undifferentiated"). Technically, one could analyze a pool of C2C12 cells under the microscope and visually determine the percentage of differentiated cells in a specific condition (temperature, nutrients, ...). Practically however, this too tedious. Luckily, differentiated muscle cells produce a protein called myosin not (or only in low amounts) produced by undifferentiated cells. Using an antibody attached to a fluorescent molecule (i.e a molecule that can emit light of a certain wavelength when excited by a laser), it is possible to stain myosin and analyze the pool of cells using a flow cytometer. A flow cytometer is a device used in biological research channels cells through a small capillary so that they pass in front of a laser one-by-one. This laser light excites the fluorescent molecule attached to our antibody, the antibody itself being attached to myosin if this protein is present in the cell. The light emitted by the fluorescent molecule in repsonse to this excitation can be captured by a camera. The amount of light captured reflects the amount of myosin present in the cell, allowing us to determine if a cell is differentiated or not. Keep in mind that even undifferentiatde cells can produce small amounts of myosin and therefore the differentiated cells we're interested in are those that produce larger amounts.</p>

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
