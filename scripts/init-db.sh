#!/bin/bash

if [[ -z "${DOCKER_RUNNING}" ]]; then
    ENV_FILE="../.env"
    export POSTGRES_PASSWORD=$(grep POSTGRES_PASSWORD $ENV_FILE | cut -d '=' -f2)
fi

psql -U postgres <<EOF
CREATE ROLE nmebious_admin WITH LOGIN PASSWORD '$POSTGRES_PASSWORD';

GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO new_user;

CREATE DATABASE nmebious OWNER nmebious_admin;
CREATE DATABASE nmebious_test OWNER nmebious_admin;

\c nmebious nmebious_admin
CREATE TABLE post(
       id SERIAL PRIMARY KEY,
       board VARCHAR(16),
       ip_hash CHAR(64),
       submission_date TIMESTAMP,
       type CHAR(4),
       checksum CHAR(32),
       data TEXT
);
CREATE TABLE ban(
       ip_hash CHAR(64)
);
CREATE TABLE api_key(
       key TEXT
);
\c nmebious_test nmebious_admin
CREATE TABLE post(
       id SERIAL PRIMARY KEY,
       board VARCHAR(16),
       ip_hash CHAR(64),
       submission_date TIMESTAMP,
       type CHAR(4),
       checksum CHAR(32),
       data TEXT
);
CREATE TABLE ban(
       ip_hash CHAR(64)
);
CREATE TABLE api_key(
       key TEXT
);
EOF
