#!/bin/bash
#
# Simple test harness for the JavaScript Fibonacci number
# project.  $1 is the path to the instrumented project,
# while $2 is the test case to execute.

# Enter the instrumented project directory
cd $1
# Install the package dependencies (this should be done
# prior to testing, but having it here avoids clutting
# the SEL repository with no real harm)
npm install
# Execute the application against the given test case
node ./app.js $2
