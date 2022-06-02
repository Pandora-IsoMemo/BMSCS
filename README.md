# BMSC App (constraint-estimation-app)

- Access beta version: https://isomemoapp.com/app/osteo-bio-r-beta

# DOWNLOAD INSTALLATION ON LOCAL MACHINE
- First download Docker (see instructions: https://docs.docker.com/desktop/windows/install/) and see video instructions (https://www.youtube.com/watch?v=_9AWYlt86B8)
- Next, open your windows CMD line and run these following commands:
  > - download and install BMSC app: `docker pull ghcr.io/pandora-isomemo/bmsc-app:main`
  > - start the app run: `docker run -p 3838:3838 ghcr.io/pandora-isomemo/bmsc-app:main`
  > - this outputs `Listening on http://0.0.0.0:3838`, then copy & paste the `http://0.0.0.0:3838` to your browser to access local version. 
  > - quit the app run: `docker run -p 3838:3838 --rm ghcr.io/pandora-isomemo/bmsc-app:main` or just close the browser window


## Naming Conventions in R

- Numbers as Prefix. Grouped into files with same type of functionality
- Higher number indicate that the functions are closer to the functionality of the app / higher abstraction level
- 00: Namespace, Settings
- 01: Helper functions
- 02: Helper shiny modules (files include ui + server component)
- 03: Main shiny modules (basically tabs in the app)
- 04: Start application / main functionality
