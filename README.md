# Horace
A repository used for parsing digital Latin corpora from Horace, with functionality for multiple machine-operators from Docker.

## Using `tabulae` with This Repository

### Prerequisites
- Download or clone this repository to your desktop
- Download or clone [tabulae](https://github.com/neelsmith/tabulae) and place it in this repository
- Replace `stemtypes.fst` in `tabulae/fst/symbols` with the version located in `tabulae-fix`
- Download [Docker Desktop](https://www.docker.com/products/docker-desktop) for Windows 10 Pro
- Download [Docker Toolbox](https://docs.docker.com/toolbox/) for Mac or Windows 10

### Starting, Stopping, and Resuming Chris Blackwell's Docker App
`sbt-sfst-docker` provides the `sfst` toolkit that `tabulae` uses and starts an `sbt` console in your current directory. Follow the instructions on the docker app's README or run these commands by copying and pasting them into a terminal in this directory:
- Initial installation: `docker run --name citeWork -ti -v $(pwd):/workspace eumaeus/sbt-sfst-docker:v3`
  - You may need to manually write the path to this repository instead of using `$(pwd)`
  - Ex: `docker run --name citeWork -ti -v C:\Users\NAME\Desktop\horace:/workspace eumaeus/sbt-sf
  st-docker:v3`
- To end a session: `:quit` in the Docker sbt console
  - If you have properly entered the sbt console, you will see `scala >` on your command line
- To resume a session:
  - `docker restart citeWork`, then
  - `docker exec -ti citeWork sbt console`

### Using `tabulae` Within the Docker Session
- Put the corpus you want to parse into a file (preferably .txt) in the `editions` repository
- Run `:load scripts/win_parse.sc` for Windows
- Run `:load scripts/mac_parse.sc` for Mac
- Run `compile("horace-morphology")`
- Follow the guidelines in the script for more information about how to work with the parser
