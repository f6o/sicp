#!/bin/bash
set -e

gosh -l ./ch4-leval.scm -e '(begin (define the-global-environment (setup-environment)) (driver-loop))'