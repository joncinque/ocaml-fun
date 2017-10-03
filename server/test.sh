#!/usr/bin/env bash

ERROR=1
test_curl () {
  echo "Testing query \"$1\" for result \"$2\""
  res=`curl localhost:4000/$1`
  if [[ $? -ne 0 ]]
  then
    echo "curl failed setting"
    exit $ERROR
  fi

  if [[ $res != $2 ]]
  then
    echo "Unexpected response: $res"
    exit $ERROR
  fi
}

testkey="somekey"
testval="someval"

# 1. Test set
test_curl "set?$testkey=$testval" "Set successful"

# 2. Test get
test_curl "get?key=$testkey" $testval
