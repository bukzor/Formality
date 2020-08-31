export PATH := $(CURDIR)/venv/bin:$(CURDIR)/sbin:$(PATH)


all: venv blacken

sbin/venv-update:
	curl -sSL https://raw.githubusercontent.com/Yelp/venv-update/master/venv_update.py > $@.tmp
	chmod 755 $@.tmp
	mv $@.tmp $@

venv: Makefile sbin/venv-update requirements.txt
	venv-update venv= --python python3 --prompt='(venv:fm) ' venv

blacken:
	./venv/bin/black .
