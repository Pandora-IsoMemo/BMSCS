# BMSCS (constraint-estimation-app)

## Access to online version

- MAIN version: <https://isomemoapp.com/app/bmscs>
- BETA version: <https://isomemoapp.com/app/bmscs-beta>

## Documenation

- <https://pandora-isomemo.github.io/BMSCS/>

## Installation instructions

- <https://pandora-isomemo.github.io/docs/apps.html#bmscs>

## Release notes (Changelog)

- see `NEWS.md`

## Folder for online models

- [`inst/app/predefinedModels`](https://github.com/Pandora-IsoMemo/bmsc-app/tree/main/inst/app/predefinedModels)

## Notes for developers

### Documentation Updates

When adding information to help pages, docstrings, or vignettes, please
update documentation locally as follows. The documentation of the main
branch is built automatically via GitHub Actions. Run these commands
before opening a PR with doc or vignette changes.

``` r

devtools::document() # or CTRL + SHIFT + D in RStudio
devtools::build_site()
```

### Local Docker Container

When testing with a local docker container, please make sure to rebuild
the docker image after changes in the R code or dependencies. You can do
this from the root of the repository via:

``` bash
docker build -t bmscs-app:latest .
```

or for a full rebuild without cache:

``` bash
docker build --no-cache -t bmscs-app:latest .
```

After that, start the container as usual via:

``` bash
docker run -p 3838:3838 bmscs-app:latest
```

and access the app in your browser at `http://localhost:3838/`. Stop the
container with `CTRL + C` in the terminal.

**Optional:**

Add `-it` for interactive mode, or `--rm` to remove the container after
stopping.
