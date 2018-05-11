#!/bin/bash

# Create a new API
API_ID=$(http --check-status POST $PROXY42_URL:4001/apis/ hostname="httpbin.org" servers:='["http://httpbin.org"]' frontend_prefix="/1500/" backend_prefix="/" strategy=random rate_limit:=43 additional_headers="" auth_config:='{"strategy": "auth_key"}' | jq .id | tr -d '""')

echo "API_ID = $API_ID"

# Create a new developer
DEVELOPER_ID=$(http --check-status POST $PROXY42_URL:4001/developers email=test@apinf.io | jq .id | tr -d '""')

echo "DEVELOPER_ID = $DEVELOPER_ID"

# Issue Key to a Developer
KEY=$(http --check-status POST $PROXY42_URL:4001/plugins/auth_key/issue_key developer_id=$DEVELOPER_ID | jq .key | tr -d '""')

echo "KEY = $KEY"

# Authorize the developer to an API
http POST $PROXY42_URL:4001/authorizations developer_id=$DEVELOPER_ID api_id=$API_ID

# Check authorization

# Make unauthorized call
if http --check-status GET $SERVER_URL:8080/1500/; then
    echo "[FAIL] Authorization Accepted!!!"
else
    case $? in
        3) echo "[FAIL] Unexpected Direction" ;;
        4) echo "[OK] Request Failed as Expected" ;;
        5) echo "[FAIL] Unexpected Server Error" ;;
    esac
fi

# Make Authorized call
if http --check-status GET $SERVER_URL:8080/1500/ Authorization:"Bearer $KEY" &> /dev/null; then
    echo "[OK] Authorization Passed!!!"
else
    case $? in
        3) echo "[FAIL] Unexpected Direction" ;;
        4) echo "[FAIL] Request Unexpectedly Forbidden" ;;
        5) echo "[FAIL] Unexpected Server Error" ;;
    esac
fi


# Cleanup
http --check-status DELETE "$PROXY42_URL:4001/apis/$API_ID"

#TODO : This fails.
http --check-status DELETE "$PROXY42_URL:4001/developers/$DEVELOPER_ID"
#rm api_create.json developer_create.json issue.json authorize.json

function fail () {
    echo  -e " \033[;31m ~ [FAIL] $1 \033[0m "
    exit 1
}

function ok () {
    echo  -e " \033[;32m ~  $1 \033[0m "
}

function msg () {
    echo "[INFO] $1"
}

function assert_status_code () {
    msg "http $1 $2"
    status_code="$(http -h --timeout=4.5 $1 $SERVER_URL/$2 BEARER:$token | grep HTTP/  | cut -d ' ' -f 2)"
    if [ "$status_code" != "$3" ]
    then
        fail "request $2 $1 respone status code get $? not $3"
    else
        ok "request $2 $1 got response status code $3"
    fi
}
