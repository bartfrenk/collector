.PHONY: help database-create-image database-run-container

APPLICATION   := Collector
DATABASE_PORT := 15432

.DEFAULT_GOAL := help

define PYTHON_TOJSON=
import sys, yaml, json; json.dump(yaml.load(sys.stdin), sys.stdout, indent=4)
endef

help: ## Show this help
	@echo "Makefile for '${APPLICATION}'\n"
	@fgrep -h "##" $(MAKEFILE_LIST) | \
	fgrep -v fgrep | sed -e 's/## */##/' | column -t -s##

database-create-image: ## Build the database image
	docker build -t collector/database -f res/docker/Dockerfile.db .

database-run-container: ## Start the database container
	@docker create \
			-p ${DATABASE_PORT}:5432 --name database-collector-dev \
		   collector/database 2> /dev/null; \
	docker start database-collector-dev

target/schemas/%.json: res/schemas/%.yaml target/schemas
	@python -c '${PYTHON_TOJSON}' < $< > $@

target/schemas:
	@mkdir -p target/schemas


##

test-put-schema: ## Post the default/product schema
test-put-schema: target/schemas/product.json
	curl -d @target/schemas/product.json 'http://localhost:5000/schemas/default/product' \
		 -H 'Content-Type: application/json' -v -X 'PUT'

test-get-schema: ## Get the default/product schema
	curl 'http://localhost:5000/schemas/default/product'

test-post-source: ## Post a new source
test-post-source: target/schemas/source.json
	curl -d @target/schemas/source.json 'http://localhost:5000/sources' \
		 -H 'Content-Type: application/json' -v
