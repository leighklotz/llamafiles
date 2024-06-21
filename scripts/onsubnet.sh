#!/usr/bin/env bash

# https://stackoverflow.com/questions/40746463/how-to-automatically-switch-ssh-config-based-on-local-subnet

if [[ "$1" == "--help" ]] || [[ "$1" == "-h" ]]  || [[ "$1" == "" ]] ; then
  printf "Usage:\n\tonsubnet [ --not ] partial-ip-address\n\n"
  printf "Example:\n\tonsubnet 10.10.\n\tonsubnet --not 192.168.0.\n\n"
  printf "Note:\n\tThe partial-ip-address must match starting at the first\n"
  printf "\tcharacter of the ip-address, therefore the first example\n"
  printf "\tabove will match 10.10.10.1 but not 110.10.10.1\n"
  exit 0
fi

on=0
off=1
if [[ "$1" == "--not" ]] ; then
  shift
  on=1
  off=0
fi

regexp="^$(sed 's/\./\\./g' <<<"$1")"

if [[ "$(uname)" == "Darwin" ]] ; then
  ifconfig | grep -F 'inet ' | grep -Fv 127.0.0. | cut -d ' ' -f 2 | grep -Eq "$regexp"
else
  hostname -I | tr -s " " "\012" | grep -Fv 127.0.0. | grep -Eq "$regexp"
fi

if [[ $? == 0 ]]; then 
  exit $on
else
  exit $off
fi
