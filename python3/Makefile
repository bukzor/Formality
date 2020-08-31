export PATH := $(PATH):$(CURDIR)/sbin
export VIRTUALENV_INTERPRETER_RUNNING := 1


all: venv

sbin/venv-update:
	curl -sSL https://raw.githubusercontent.com/Yelp/venv-update/master/venv_update.py > $@.tmp
	chmod 755 $@.tmp
	mv $@.tmp $@

venv: Makefile sbin/venv-update requirements.txt
	venv-update venv= --python python3 venv --prompt='fm:'
