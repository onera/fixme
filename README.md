# Flow Inspector X Modelling Errors (FIXME) v1.0

FIXME is a tool coded in Scala used to analyse fault injection strategies targeting CNN hardware accelerators
to improve the fault injection experiments on such hardware architectures. It uses the formal modelling of RTL defined in
[Quality of fault injection strategies on hardware accelerator](https://link.springer.com/chapter/10.1007/978-3-031-14835-4_15)

This tool currently supports:

1. Analysis of streaming CNN accelerators using flow based modelling for improving fault injection:

- computation of alive intervals of registers for any input data
- computation of fault equivalence classes for any input flow
- coverage computation of any provided fault injection strategy (i.e. fault list)

2. Analysis of fault injection experiments for fault equivalence refinement

- processing results from fault injection campaign on CNN accelerator streaming architecture
- computation of equivalence classes based on the effect on CNN scores and/or classification
- support addition of custom metrics (indicators)

> **Quick Setup**
> Please follow the steps in [docker installation](#docker)
> and then follow the [run example guide](#examples).

## Installation

### Docker

> A Docker image is provided to quickly start using the tool.

#### Requirements

On Linux, install the package **docker.io**

On Windows, install [Docker desktop](https://docs.docker.com/desktop/windows/install/)

#### Instructions

##### Build the docker image from sources

You can build the image by running in a terminal (with elevated privileges in linux based OS):

```bash
cd fixme
docker build -t i_fixme-v1.0 .
```

##### Run a container

Once you have the image built, start a new container with:

```bash
docker run --name c_fixme-v1.0 \
-v ${PWD}/output:/home/output \
-v ${PWD}/data:/home/data -t -i i_fixme-v1.0
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

This will build the image, create a container and start **\<CMD\>** (by default it launches the sbt shell) in the container.

To remove the generated container, run:

```bash
make docker_rm
```

To remove the generated image, run:

```bash
make docker_rmi
```

### Manual installation

#### External tools requirements

Install **Scala Building Tool** (SBT).
See [SBT setup](https://www.scala-sbt.org/1.x/docs/Setup.html) for instructions.

Install **gnuplot** (optional for graph generation).

---

## Usage

```bash
fixme 1.x
Usage: fixme [analysis|flows|coverage] [options] <args>...

  -v, --verbose
some notes.

Command: analysis [options] CampaignDirectory
  -M, --measures <MeasureStrategyFile> // Measure strategy file contains several indicators to be applied to campaign results
  -cc, --consistency
  -msn, --overrideMsName <value>
  -fc, --failureClasses <JSON> // Failure Classes to use with the measure strategy
  -p, --plot               // Enable plot generation
  -cps, --checkpointStep <value> // the number of data to treat before saving a temporary csv measure file.
  -th, --threads <value>   // number of threads to use
  -gcf, --globalClassFilter <value> // global class log file path
  -df, --datasetFilter <minIndex,maxIndex> // Limit the number of files processed (according to Regex filter on file names) default everything
  -ex, --excludeResults <string,string,...> // Limit the number of files processed according to filter on path default everything
  CampaignDirectory        // Directory containing all campaign files and results, ex: data/leNet5

Command: flows cnnModelFile

  cnnModelFile             Json Keras file containing the CNN model topology
  
Command: coverage [options] cnnModelFile

  -is, --injectionStrategy <value> // Name of the injection strategy to use (inputBase,inputOnly,preRegisters,postRegisters
  cnnModelFile             Json Keras file containing the CNN model topology
```

The main application is launched by executing the command:

```bash
sbt run <command> <options> <args> # in bash
```

```bash
run # in SBT shell
```

### Identification of fault equivalence classes and coverage computation

FIXME is able to identify fault equivalence in a CNN streaming based accelerator using a flow modelling.

There are two modes available in the main interface:

1. computation of flows of a streaming accelerator and export of the flows to latex files
2. computation of fault equivalence classes and coverage of 3 injection strategies

These two modes can be used to reproduce the results presented in TODO.
In both cases the command takes as argument the path to a JSON file containing the description of the CNN model (in keras format) that is implemented by the target accelerator.

An example for the LeNet5 model is provided in the *src/test/resources/leNet5/architecture.json* file.
#### Examples and tests

 Compute all flows temporal classes, i.e. instants when a data contributing to th CNN computation is tramsitted on each location in the accelerator.
``` text
sbt:fixme> run flows src/test/resources/leNet5/architecture.json
```

Compute the coverage of a fault injection strategy among the encoded ones (i.e., inputBased, inputOnly, preRegister, postRegister).

``` text
sbt:fixme> run coverage -is inputBased src/test/resources/leNet5/architecture.json
```

`

### Analysis of fault injection results
#### Mandatory campaign files

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

- **architecture.json** describes the CNN model (keras model in JSON format)
- **labelsDictionary.txt** contains the sorted label names associated with the CNN model (a name by line)
- several **campaign_result** directories each containing:
  - a **campaign.conf** file with all the injection points in the injection strategy
  - **datalog_xxx** files (one per input data) containing the results from the injection campaign

#### Measure strategy and indicators

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

#### Examples

### Minimal example

```bash
(sbt) run analysis -df (0,9) -v -i src/test/resources/indicators/correct1.json src/test/resources/leNet5
```
