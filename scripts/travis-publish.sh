#!/bin/bash

# Example setting to use at command line for testing:
# export TRAVIS_SCALA_VERSION=2.10.5;export TRAVIS_PULL_REQUEST="false";export TRAVIS_BRANCH="master"

export publish_cmd="publishLocal"

# if [[ $TRAVIS_PULL_REQUEST == "false" && $TRAVIS_BRANCH == "master" && $(cat version.sbt) =~ "-SNAPSHOT" ]]; then
#   export publish_cmd="publish gitSnapshots publish"
# fi

sbt_cmd="sbt ++$TRAVIS_SCALA_VERSION"

coverage="$sbt_cmd validateJVM"
scala_js="$sbt_cmd macrosJS/test && $sbt_cmd coreJS/test && $sbt_cmd extrasJS/test && $sbt_cmd && $sbt_cmd lawsJS/test && $sbt_cmd testsJS/test"
scala_jvm="$sbt_cmd validateJVM"

run_cmd="$coverage && $scala_js && $scala_jvm $publish_cmd"
eval $run_cmd
