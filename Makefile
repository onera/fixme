PROJECT=postprocess
VERSION=v1.0
WORKDIR=$(shell pwd)
IMAGE=i_$(PROJECT)-$(VERSION)
CONTAINER=c_$(PROJECT)-$(VERSION)
RESDIRS=$(CAMPDIR)/stuck_at_0
CMD=sbt
PLOTDIR=$(WORKDIR)/output/plot
DATADIR=$(PLOTDIR)/data
GNUCOMMANDS=$(wildcard $(PLOTDIR)/*.gnu)
GNUDATA=$(wildcard $(PLOTDIR)/data/*.dat)
GNUPLOTS=$(patsubst %.gnu,%.svg,$(GNUCOMMANDS))

.PHONY: all docker_run docker_rm docker_rmi plot clean
all:
	@echo "all: print this message"
	@echo "plot: generate figures from .gnu and .dat files in output/plot directory"
	@echo "docker_run: launch the tool using docker (it creates the image and container if needed)"
	@echo "docker_rm: deletes the container only"
	@echo "docker_rmi: deletes the container and the image"


__docker_image__:
	docker build -t $(IMAGE) --build-arg USER_ID=$(id -u) --build-arg GROUP_ID=$(id -g) .
	@touch __docker_image__

__docker_container__: __docker_image__
	docker create --name $(CONTAINER) -v $(WORKDIR)/output:/home/output -v $(WORKDIR)/data:/home/data:ro -t -i $(IMAGE) $(CMD)
	@touch __docker_container__

docker_run: __docker_container__
	docker start -ai $(CONTAINER)

docker_rm:
	@echo "Suppressing $(CONTAINER) container..."
	docker container rm -f $(CONTAINER) -v
	@rm -f __docker_container__

docker_rmi:	docker_rm
	@echo "Deleting $(IMAGE) image..."
	docker rmi $(IMAGE) openjdk:8
	@rm -f __docker_image__

$(IMAGE).tar: __docker_image__
	docker save $(IMAGE) --output $(IMAGE).tar

docker_save: $(IMAGE).tar

%.svg: %.gnu data/%.dat
	`gnuplot -c $<`

plot: $(GNUPLOTS)

clean:
	rm -rf output

