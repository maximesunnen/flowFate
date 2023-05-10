# flowFate

Project to build a [`shiny`](https://shiny.rstudio.com/) app for the analysis of flow cytometry data using the flowCore and flowWorkspace framework.

Build with [{golem}](https://github.com/ThinkR-open/golem).



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

``` r 
# install the flowFate package
install.packages("flowFate")

# open the app
XXXXXXXX
```

Alternatively, if you don't want to install R, go to [link_to_server]() to use the app.
