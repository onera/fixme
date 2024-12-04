# base image
FROM openjdk:8
ENV SBT_VERSION 1.6.2
ARG USER_ID
ARG GROUP_ID
# basic installs
RUN apt-get update
RUN apt-get install -y wget unzip gnuplot
# install sbt
RUN wget https://github.com/sbt/sbt/releases/download/v${SBT_VERSION}/sbt-${SBT_VERSION}.zip
RUN unzip sbt-${SBT_VERSION}.zip
# set environment variable
ENV PATH="${HOME}/sbt/bin:${PATH}"

# create an output directory
RUN mkdir /home/output
RUN mkdir /home/data
# copy source code into /home/my_code
COPY src /home/src
COPY build.sbt /home/build.sbt

# set the working directory when running the docker image
WORKDIR /home
RUN sbt compile\
    exit
CMD ["sbt"]