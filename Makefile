.PHONY: permfix
permfix:
	sudo chown -R $(shell whoami):$(shell whoami) ./volumes
