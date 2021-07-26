#!/bin/bash

ENV_FILE="../.env"

if [[ -z "${POSTGRES_USER}" ]]; then
   USERNAME=$(grep POSTGRES_USER $ENV_FILE | cut -d '=' -f2)
   PASSWORD=$(grep POSTGRES_PASSWORD $ENV_FILE | cut -d '=' -f2)

   psql -U postgres -c "CREATE ROLE $USERNAME WITH LOGIN SUPERUSER PASSWORD '$PASSWORD';"
fi

psql -U ${POSTGRES_USER:-postgres} << EOF
CREATE DATABASE nmebious;
CREATE DATABASE test;

\c nmebious
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
\c test
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
