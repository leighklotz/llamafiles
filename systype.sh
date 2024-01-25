#!/bin/bash

echo '# Description of this system:'
echo '```bash'
echo '$ uname -a'
uname -a
echo '$ cat /etc/os-release'
egrep -v '^[A-Z_]+_URL="http' /etc/os-release 
echo '```'
