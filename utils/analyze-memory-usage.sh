#!/bin/bash

# Script to analyze memory usage and pipe output to 'ask'

# Check if 'ask' script exists and is executable
if [ ! -x "$(command -v ask)" ]; then
  echo "Error: 'ask' script not found or not executable." >&2
  exit 1
fi

# Capture initial memory overview
free_output=$(free -h)

# Capture top 5 memory consuming processes
top_processes=$(ps -eo pid,ppid,%mem,%cpu,cmd --sort=-%mem | head -n 6) # Include header row

# Capture /proc/meminfo
meminfo_output=$(cat /proc/meminfo | head -n 10) # Show first 10 lines for brevity

# Combine the outputs into a narrative
narrative="
## Memory Usage Analysis ##

Here's a summary of the system's memory usage:

### Overall Memory Usage: ###
$free_output

### Top 5 Memory Consuming Processes: ###
$top_processes

### /proc/meminfo (First 10 lines): ###
$meminfo_output

Based on the information above, we can observe [add your analysis here - the LLM will do this part].  It appears [the LLM will fill this out]
"

# Pipe the narrative to the 'ask' script
echo "$narrative" | ask "Analyze the provided memory usage data and provide a concise report, including potential bottlenecks or areas of concern.  Also, summarize the overall health, if possible."

exit 0
