# Post-processing tool v1.0

 This tool currently supports:

- processing results from fault injection campaign on Haddoc2 streaming architecture
- abstraction of Haddoc2 into flow based formalization
- coverage computation of a fault injection strategy

> **Quick Setup**
> Please follow the steps in [docker installation](#docker)
> and then follow the [run example guide](#examples).

## Index

- [Post-processing tool v1.0](#post-processing-tool-v10)
  - [Index](#index)
  - [Installation](#installation)
    - [Docker](#docker)
      - [Requirements](#requirements)
      - [Run a docker container](#run-a-docker-container)
        - [Get the docker image](#get-the-docker-image)
        - [Or Build the docker image from sources](#or-build-the-docker-image-from-sources)
        - [Run a container](#run-a-container)
      - [Provided makefile](#provided-makefile)
    - [Full installation](#full-installation)
      - [External tools requirements](#external-tools-requirements)
  - [Usage](#usage)
    - [Mandatory campaign files](#mandatory-campaign-files)
    - [Measure strategy and indicators](#measure-strategy-and-indicators)
      - [Example of indicators](#example-of-indicators)
        - [Example 1](#example-1)
  - [Examples](#examples)
    - [Minimal example](#minimal-example)

## Installation

### Docker

> A Docker image is provided to quickly start using the tool.

#### Requirements

On Linux, install the package **docker.io**

On Windows, install [Docker desktop](https://docs.docker.com/desktop/windows/install/)

#### Run a docker container

##### Get the docker image

You can download the docker image tar [here](https://nxp1.sharepoint.com/:u:/r/sites/FuSa_for_NN_ML-Onera-NXPexchange/Shared%20Documents/Onera-NXP%20exchange/postprocessing/posprocessing-v1.0.tar?csf=1&web=1&e=eEgqtm).
Then run in a terminal:

```bash
docker load -i postprocess-v1.0.tar
```

##### Or Build the docker image from sources

Alternatively, you can build the image by running in a terminal (with elevated privileges in linux based OS):

```bash
cd postprocessing
docker build -t i_postprocessing-v1.0 .
```

##### Run a container

Once you have the image loaded, start a new container with:

```bash
docker run --name c_postprocessing-v1.0 \
-v ${PWD}/output:/home/output \
-v ${PWD}/data:/home/data -t -i i_postprocessing-v1.0
```

> The folders **output** and **data**
> shall be shared between the container and the host in order to be able
> to modify the configurations and keep analysis results accessible.

---

#### Provided makefile

Alternatively, you can build a docker image directly from sources with the **Makefile** provided.

All the commands to build the docker image
and run the container are contained in a Makefile
> WARNING: in linux, the docker commands require super-user privilege

To launch a command in the container just enter:

```bash
make docker_run CMD=<CMD>
```

This will build the image, create a container and start **\<CMD\>**(default launch the sbt shell) in the container.

To remove the generated container, run:

```bash
make docker_rm
```

To remove the generated image, run:

```bash
make docker_rmi
```

### Full installation

#### External tools requirements

Install **gnuplot**.

Install **Scala Building Tool** (SBT).
See [SBT setup](https://www.scala-sbt.org/1.x/docs/Setup.html) for instructions.

---

## Usage

```bash
postprocess 1.x
Usage: postprocess [analysis] [options] <args>...

  -v, --verbose
some notes.

Command: analysis [default] [options] CampaignDirectory

Command: analysis default

  CampaignDirectory        Directory containing all campaign files and results, ex: data/leNet5
  -M, --measures <MeasureStrategyFile>
                           Measure strategy file contains several indicators to be applied to campaign results
  -i, --indicator <JSON1 JSON2 ...>
                           Indicators to add to measure strategy
  -I, --indicatorDir <indicatorDirectory>
                           Directory of indicators to add to measure strategy
  -df, --datasetFilter <minIndex,maxIndex>
                           Limit the number of files processed
  -p, --plot               Enable plot generation
```

The main application is launched by executing the command:

```bash
sbt run # in bash
```

```bash
run # in SBT shell
```

### Mandatory campaign files

The postprocessing tool requires several files in
the data directory describing the campaign to analyse.
> Info: By default, the program looks into the **data/leNet5** directory.

A typical campaign folder is structured as such:

```text
modelName/
  ├─ results/
  │     ├─ campaign_1/
  │     │      ├─ campaign.conf
  │     │      ├─ datalog_0
  │     │      ├─ ...
  │     ├─ campaign_2/
  │            ├─ campaign.conf
  │            ├─ datalog_0
  │            ├─ ...
  ├─ architecture.json
  ├─ labelsDictionary.txt
```

Where:

- **architecture.json** described the CNN model (keras model in JSON format)
- **labelsDictionary.txt** contains the sorted label names associated with the CNN model (a name by line)
- several **campaign_result** directories each containing:
  - a **campaign.conf** file with all the injection points in the injection strategy
  - **datalog_xxx** files (one per input data) containing the results from the injection campaign

### Measure strategy and indicators

An indicator or measure file is a Json file describing the indicators to compute on campaign results.
An indicator is defined in a JSON file as such:

```json
{
  "name": "NAME",
  "failureClass": "FAILURE_CLASS",
  "projection": "PROJECTION"
}

```

Where:

- **"FAILURE_CLASS"** is the type of impact on classification caused by an injection, it can take the following values:
  - **"SC-X"** where **X** is a positive integer (e.g. SC-3)
   >*an injection results in an SC-X if the image is
   misclassified and the correct label is not in the X higher scores.*
  - **"Observed"**
   >*an injection is observed if the classification is correct but the CNN score values are modified*
  - **"Masked"**
   >*an injection is masked if the output scores are identical to the golden scores*
  - **"Degraded-F"** where F is a float value (e.g. Degraded-0.03)
    >*the result of an injection is degraded-F if the classification is correct but the variation of the top label.
 score w.r.t the golden score is superior to +/- **F***

- **"PROJECTION"** is a logical expression that filters the results to be treated, it is a combination of conditions where a condition is a test on:  
  - **faultType**: the fault type (STUCK_AT_0,"STUCK_AT_1 or BITFLIP)
  - **bitIndex**: the targeted bit
  - **channelIndex**: the targeted channel
  - **layerId**: the name of the layer in the model (cf. json keras model file)
    >*For LeNet5: conv2d, max_pooling2d,conv2d_1, max_pooling2d_1, dense, dense_seq, dense_1, dense_1_seq, dense_2*
  - **inputIndex**: the index of the input image in the test dataset
  - **inputLabel**: the label of the input image

#### Example of indicators

##### Example 1

This indicator compute the rate of faults in the two convolution layers that result in a misclassification for images representing a 7.

```json
{
  "name": "My Indicator",
  "failureClass": "SC-1",
  "projection": "inputLabel==7 & (layerId==conv2d | layerId==conv2d_1)"
}
```

## Examples

### Minimal example

```bash
(sbt) run analysis -df (0,9) -v -i src/test/resources/indicators/correct1.json data/leNet5
```
