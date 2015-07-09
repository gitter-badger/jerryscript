#!/bin/bash

# Copyright 2014-2015 Samsung Electronics Co., Ltd.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

REPOSITORY_DIR=$(dirname "$0")/../..
CPPCHECK=${REPOSITORY_DIR}/third-party/cppcheck/cppcheck
SUPPRESSIONS_LIST=$(dirname "$0")/suppressions-list

if [ ! -x "$CPPCHECK" ]
then
  exit 1;
fi

${CPPCHECK} "$@" "--exitcode-suppressions=$SUPPRESSIONS_LIST"
status_code=$?

exit $status_code
