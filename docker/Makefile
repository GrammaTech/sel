DOCKER_REGISTRY_HOST ?= docker.grammatech.com:14850

BUILD_TAG ?= latest
BUILD_FROM_TAG ?= $(BUILD_TAG)

FILEPP_OPTIONS = -D __GTD__BUILD_TAG=$(BUILD_TAG) \
                 -D __GTD__BUILD_FROM_TAG=$(BUILD_FROM_TAG)

IMAGE = sel
GTDOCKER_CONFIG = config/GTDockerConfig.h

.PHONY: FORCE
FORCE:

$(IMAGE): $(IMAGE)/Dockerfile
	if [ ! -z $(LOCAL_IMAGE_NAME) ]; then \
		docker build -f $< -t $(LOCAL_IMAGE_NAME) $@; \
	else \
		docker build -f $< -t $@ $@; \
	fi

## Preproccess Dockerfile.in to Dockerfile
$(IMAGE)/Dockerfile: $(IMAGE)/Dockerfile.in $(GTDOCKER_CONFIG)
	filepp -b -lc \\\\ $< -o $@ -I ./config $(FILEPP_OPTIONS)
	rm -f $@~

$(GTDOCKER_CONFIG): FORCE
	## wget main include
	curl https://git.grammatech.com/gt/docker-base/raw/master/docker/config/GTDockerConfig.h > $@

.PHONY: clean
clean:
	rm -f $(GTDOCKER_CONFIG)
	rm -f $(IMAGE)/Dockerfile
