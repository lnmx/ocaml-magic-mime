#!/bin/sh -ex
# Generate the mime_types.ml file

./_build/factory/main.native ./factory/freedesktop.org.xml ./factory/manifest.txt mime_types.ml
