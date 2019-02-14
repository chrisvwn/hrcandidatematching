#!/bin/bash
/bin/R --slave --no-restore -e "source('searchVagas.R'); searchVagas()"
