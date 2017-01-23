#!/bin/bash

# Build the ultibo projects in this repo.
#
# Runs on linux/x64 with either ultibo installed or docker installed.
# Runs on windows git bash with ultibo installed.
# Runs on windows 10 pro git bash with docker installed.

function log {
    echo $* | tee -a $LOG
}

function ultibo-bash-quotation {
    if [ "$(which $1)" != "" ]
    then
        echo -n $*
    else
        local DOCKER_IMAGE=markfirmware/ultibo-bash
        echo -en "docker run --rm -i -v $(pwd):/workdir --entrypoint /bin/bash $DOCKER_IMAGE -c \"$*\""
    fi
}

function ultibo-bash {
    eval $(ultibo-bash-quotation $*)
}

function build-lpr {
    local LPR_FILE=$1
    local TARGET_COMPILER_OPTIONS=$2
    local CFG_NAME=$3
    local LPR_FOLDER=$(dirname $LPR_FILE)
    local INCLUDES=-Fi/root/ultibo/core/fpc/source/packages/fv/src
    log
    log
    log .... building $LPR_FILE
    rm -rf $LPR_FOLDER/obj && \
    mkdir -p $LPR_FOLDER/obj && \
    ultibo-bash fpc \
     -l- \
     -v0ewn \
     -B \
     -Tultibo \
     -O2 \
     -Parm \
     -Mdelphi \
     -FuSource \
     -FE$LPR_FOLDER/obj \
     $INCLUDES \
     $TARGET_COMPILER_OPTIONS \
     @/root/ultibo/core/fpc/bin/$CFG_NAME \
     $LPR_FILE |& tee -a $LOG && \
\
    mv kernel* $LPR_FOLDER/$OUTPUT
    if [[ $? -ne 0 ]]; then log fail: $?; fi
}

function build-as {
    local TARGET=$1
    local FOLDER=$2
    local REPO=$3
    if [[ -d $FOLDER ]]
    then
        ls $FOLDER/*.lpr > /dev/null 2>&1
        if [[ $? -eq 0 ]]
        then
            rm -rf $FOLDER/$OUTPUT
            mkdir -p $FOLDER/$OUTPUT
            local LPR_FILE=$FOLDER/*.lpr
            case $TARGET in
                QEMU)
                    build-lpr $LPR_FILE "-CpARMV7A -WpQEMUVPB" qemuvpb.cfg ;;
                RPi)
                    build-lpr $LPR_FILE "-CpARMV6 -WpRPIB" rpi.cfg ;;
                RPi2)
                    build-lpr $LPR_FILE "-CpARMV7A -WpRPI2B" rpi2.cfg ;;
                RPi3)
                    build-lpr $LPR_FILE "-CpARMV7A -WpRPI3B" rpi3.cfg ;;
            esac
            local THISOUT=$OUTPUT/kernels/$FOLDER
            rm -rf $THISOUT && \
            mkdir -p $THISOUT && \
            cp -a $FOLDER/$OUTPUT/* $THISOUT && \
            if [[ $? -ne 0 ]]; then log fail: $?; fi
        fi
    fi
}

function build-asphyre {
    local SAMPLES_PATH=Samples/FreePascal/Ultibo
    for SAMPLE_PATH in $SAMPLES_PATH/*
    do
        build-as RPi2 $SAMPLE_PATH ultibohub/Asphyre
    done
}

function create-build-summary {
    cat $LOG | egrep -i '(fail|error|warning|note):' | sort | uniq > $ERRORS
    log
    log Summary:
    log
    cat $ERRORS | tee -a $LOG
    log
    log $(wc $ERRORS)
    if [[ -s $ERRORS ]]
    then
        exit 1
    fi
}

ERRORS=build-errors.txt
OUTPUT=build-output
LOG=$OUTPUT/build.log
rm -rf $OUTPUT
mkdir -p $OUTPUT
rm -f $LOG

build-asphyre

create-build-summary
